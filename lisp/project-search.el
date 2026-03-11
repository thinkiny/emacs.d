;;; project-search.el --- Unified project search  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function eglot-current-server "eglot")
(declare-function eglot-server-capable "eglot")
(declare-function eglot-uri-to-path "eglot")
(declare-function jsonrpc-request "jsonrpc")
(declare-function jsonrpc-async-request "jsonrpc")

(defvar eglot--cached-server)

(defgroup project-search nil
  "Unified project search via LSP and ripgrep."
  :group 'tools
  :prefix "project-search-")

(defcustom project-search-max-results 50
  "Maximum number of results for project search.
For the interactive ivy command this caps rg output; LSP results
are not truncated client-side (the server controls its own limit)."
  :type 'integer
  :group 'project-search)

(defcustom project-search-debounce-delay 0.3
  "Seconds to wait after last keystroke before sending the query.
Applies to both the LSP and rg paths in the interactive ivy command."
  :type 'number
  :group 'project-search)

;;; ---- Shared Backend ----

(defun project-search--project-root ()
  "Return the current project root directory."
  (or (and (fboundp 'projectile-project-root)
           (ignore-errors (projectile-project-root)))
      default-directory))

(defun project-search--lsp-kind-to-string (kind)
  "Convert LSP SymbolKind integer KIND to a human-readable string."
  (if (boundp 'eglot--symbol-kind-names)
      (or (alist-get kind eglot--symbol-kind-names) "Unknown")
    "Unknown"))

(defun project-search--find-eglot-server (&optional project-root)
  "Find an active Eglot server for PROJECT-ROOT that supports workspace/symbol.
Returns nil when no server exists or the server lacks
:workspaceSymbolProvider, causing callers to fall back to ripgrep."
  (when (fboundp 'eglot-current-server)
    (let* ((root (or project-root (project-search--project-root)))
           (bufs (if (fboundp 'projectile-project-buffers)
                     (ignore-errors (projectile-project-buffers root))
                   (buffer-list))))
      (cl-some (lambda (buf)
                 (when-let* ((file (buffer-file-name buf)))
                   (when (file-in-directory-p file root)
                     (with-current-buffer buf
                       (when-let* ((server (eglot-current-server)))
                         (let ((eglot--cached-server server))
                           (when (eglot-server-capable :workspaceSymbolProvider)
                             server)))))))
               bufs))))

(defun project-search--xref-to-result (item)
  "Convert xref ITEM to the normalized alist format for MCP."
  (let* ((summary (xref-item-summary item))
         (loc     (xref-item-location item))
         (file    (xref-location-group loc))
         (line    (xref-location-line loc))
         (col     (if (cl-typep loc 'xref-file-location)
                      (slot-value loc 'column) 0))
         (kind-and-name
          (if (string-match "\\`#\\([a-z]+\\) \\(.*\\)" summary)
              (cons (capitalize (match-string 1 summary))
                    (match-string 2 summary))
            (cons "Text" summary)))
         (uri  (concat "file://" (expand-file-name file)))
         (line-0 (1- line)))
    `((name . ,(cdr kind-and-name))
      (kind . ,(car kind-and-name))
      (location . ((uri . ,uri)
                   (range . ((start . ((line . ,line-0) (character . ,col)))
                             (end   . ((line . ,line-0) (character . ,col)))))))
      (containerName . ""))))

(defun project-search--xref-query (query)
  "Search for QUERY using the current xref backend."
  (when-let* ((backend (xref-find-backend)))
    (xref-backend-apropos backend query)))

(defun project-search--search (query &optional _kind max-results)
  "Search project for QUERY, returning normalized alist results.
Uses Eglot (sync) when available, then xref-backend-apropos, then ripgrep.
Results capped at MAX-RESULTS (default `project-search-max-results')."
  (let* ((project-root (project-search--project-root))
         (limit (or max-results project-search-max-results))
         (xrefs
          (or (condition-case err
                  (when-let* ((server (project-search--find-eglot-server project-root)))
                    (let* ((raw (jsonrpc-request server :workspace/symbol
                                                `(:query ,query)))
                           (sorted (seq-sort-by
                                    (lambda (r) (or (plist-get r :score) 0))
                                    #'> (append raw nil))))
                      (project-search--lsp-to-xrefs sorted)))
                (error (message "project-search LSP error: %S" err) nil))
              (ignore-errors (project-search--xref-query query))
              (ignore-errors
                (project-search--rg-to-xrefs query project-root limit)))))
    (seq-take (seq-map #'project-search--xref-to-result (or xrefs '()))
              limit)))

;;; ---- Ivy Interactive Command ----

(defvar project-search--ivy-default-directory nil
  "Saved `default-directory' from when the ivy session started.")

(defvar project-search--ivy-project-root nil
  "Project root for the current `ivy-project-search' session.")

(defvar project-search--ivy-server nil
  "Eglot server for the current `ivy-project-search' session.")

(defvar project-search--ivy-rg-extra-args nil
  "Extra rg flags for the current ivy session (e.g. \"-tgo -i\").")

(defvar project-search--request-id 0
  "Monotonic counter to discard stale async LSP responses.")

(defvar project-search--debounce-timer nil
  "Timer for debouncing workspace/symbol and rg requests.")

(defvar project-search--ivy-candidates nil
  "Current alist of (display-string . xref-location) for the ivy session.")

(defun project-search--lsp-to-xrefs (results)
  "Convert LSP SymbolInformation plists RESULTS to xref items."
  (seq-filter
   #'identity
   (seq-map
    (lambda (r)
      (ignore-errors
        (let* ((name (plist-get r :name))
               (kind (plist-get r :kind))
               (kind-name (project-search--lsp-kind-to-string kind))
               (loc (plist-get r :location))
               (uri (plist-get loc :uri))
               (range (plist-get loc :range))
               (line (1+ (plist-get (plist-get range :start) :line)))
               (col (plist-get (plist-get range :start) :character))
               (file-path (eglot-uri-to-path uri)))
          (xref-make (format "#%s %s" (downcase kind-name) name)
                     (xref-make-file-location file-path line (or col 0))))))
    results)))

(defun project-search--rg-to-xrefs (query project-root max-results &optional extra-args)
  "Run rg for QUERY in PROJECT-ROOT and return xref items.
Results are capped at MAX-RESULTS.  EXTRA-ARGS is an optional
string of additional rg flags (e.g. \"-tgo -i\")."
  (let ((default-directory project-root)
        (limit (or max-results project-search-max-results))
        (xrefs '()))
    (with-temp-buffer
      (apply #'call-process "rg" nil t nil
             (append (when extra-args (split-string-and-unquote extra-args))
                     (list "--no-heading" "--line-number" "--column"
                           "--smart-case" "--color" "never"
                           "--" query ".")))
      (goto-char (point-min))
      (while (and (not (eobp)) (< (length xrefs) limit))
        (when (looking-at "\\./\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)")
          (let ((file (expand-file-name (match-string 1) project-root))
                (line (string-to-number (match-string 2)))
                (col (string-to-number (match-string 3)))
                (text (match-string 4)))
            (push (xref-make (string-trim text)
                             (xref-make-file-location file line col))
                  xrefs)))
        (forward-line 1)))
    (nreverse xrefs)))

(defun project-search--update-ivy-candidates (xrefs)
  "Update ivy candidates from XREFS."
  (when xrefs
    (let ((collection (ivy-xref-make-collection xrefs)))
      (setq project-search--ivy-candidates collection)
      (ivy--set-candidates (mapcar #'car collection))
      (ivy--insert-minibuffer (ivy--format ivy--all-candidates)))))

(defun project-search--ivy-action (candidate)
  "Jump to the xref location associated with CANDIDATE."
  (when-let* ((entry (assoc candidate project-search--ivy-candidates)))
    (let ((marker (xref-location-marker (cdr entry))))
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker))))

(defun project-search--extract-kind-prefix (input)
  "Extract the kind filter prefix from INPUT.
A leading `#word' is treated as a kind prefix.  Returns the
prefix as a lowercase string, or nil if none found.
  e.g. \"#fu myFunc\" -> \"fu\", \"#file foo\" -> \"file\", \"bar\" -> nil."
  (when (string-match "\\`#\\([a-zA-Z]+\\)" input)
    (downcase (match-string 1 input))))

(defun project-search--extract-query (input)
  "Extract the search query from INPUT, stripping any `#prefix'.
  e.g. \"#fu myFunc\" -> \"myFunc\", \"#f foo\" -> \"foo\", \"bar\" -> \"bar\"."
  (string-trim (replace-regexp-in-string "\\`#[a-zA-Z]*\\s-*" "" input)))

(defun project-search--kind-matches-prefix-p (kind-int prefix)
  "Return non-nil if LSP SymbolKind KIND-INT matches PREFIX.
PREFIX is a lowercase string matched against the start of the
kind name from `eglot--symbol-kind-names'."
  (when-let* ((kind-name (project-search--lsp-kind-to-string kind-int)))
    (string-prefix-p prefix (downcase kind-name))))

(defun project-search--send-lsp-request (server query project-root req-id &optional kind-prefix max-results)
  "Send workspace/symbol request for QUERY to SERVER.
PROJECT-ROOT is used for candidate formatting.  REQ-ID is checked
against `project-search--request-id' to discard stale
responses.  When KIND-PREFIX is non-nil, only symbols whose kind
name starts with that prefix are kept.  When MAX-RESULTS is
non-nil, it caps the rg fallback output."
  (jsonrpc-async-request
   server :workspace/symbol `(:query ,query)
   :success-fn
   (lambda (resp)
     (when (= req-id project-search--request-id)
       (ignore-errors
         (let* ((results (append resp nil))
                (filtered (if kind-prefix
                              (seq-filter
                               (lambda (r)
                                 (project-search--kind-matches-prefix-p
                                  (plist-get r :kind) kind-prefix))
                               results)
                            results))
                (sorted (seq-sort-by (lambda (r) (or (plist-get r :score) 0)) #'> filtered))
                (xrefs (project-search--lsp-to-xrefs sorted)))
           (if xrefs
               (project-search--update-ivy-candidates xrefs)
             (project-search--update-ivy-candidates
              (project-search--rg-to-xrefs query project-root max-results)))))))
   :error-fn
   (lambda (&rest _)
     (when (= req-id project-search--request-id)
       (project-search--update-ivy-candidates
        (project-search--rg-to-xrefs query project-root max-results))))
   :timeout-fn
   (lambda ()
     (when (= req-id project-search--request-id)
       (project-search--update-ivy-candidates
        (project-search--rg-to-xrefs query project-root max-results))))))


(defun project-search--ivy-function (input)
  "Dynamic collection function for `ivy-project-search'.
INPUT is the current minibuffer text.  Both LSP and rg paths are
debounced.  A `#prefix' at the start of INPUT filters LSP results
by kind and is stripped before querying."
  (let ((server project-search--ivy-server)
        (project-root project-search--ivy-project-root)
        (max-results project-search-max-results)
        (extra-args project-search--ivy-rg-extra-args)
        (query (project-search--extract-query input))
        (kind-prefix (project-search--extract-kind-prefix input)))
    (or (let ((ivy-text query)) (ivy-more-chars))
        (if server
            (let ((req-id (cl-incf project-search--request-id)))
              (when project-search--debounce-timer
                (cancel-timer project-search--debounce-timer))
              (setq project-search--debounce-timer
                    (run-with-timer
                     project-search-debounce-delay nil
                     #'project-search--send-lsp-request
                     server query project-root req-id kind-prefix max-results))
              nil)
          (condition-case err
              (let ((xrefs (project-search--rg-to-xrefs query project-root max-results extra-args)))
                (when xrefs
                  (let ((collection (ivy-xref-make-collection xrefs)))
                    (setq project-search--ivy-candidates collection)
                    (mapcar #'car collection))))
            (error (message "project-search rg error: %S" err) nil))))))


;;;###autoload
(defun ivy-project-rg (&optional options)
  "Search project with ripgrep using xref-based ivy display.
OPTIONS is an optional string of extra rg flags (e.g. \"-tgo -i\")."
  (interactive)
  (let* ((project-root (project-search--project-root))
         (project-search--ivy-project-root project-root)
         (project-search--ivy-server nil)
         (project-search--ivy-rg-extra-args options)
         (project-search--ivy-default-directory default-directory)
         (default-directory project-root))
    (ivy-read (format "[%s] rg: " (projectile-project-name))
              #'project-search--ivy-function
              :dynamic-collection t
              :require-match t
              :action #'project-search--ivy-action
              :caller 'ivy-project-search)))

;;;###autoload
(defun ivy-project-search ()
  "Search project symbols via LSP or ripgrep with ivy completion.
When an active Eglot server exists for the project, queries
workspace/symbol asynchronously.  Otherwise falls back to async
ripgrep text search.  Results are limited to
`project-search-max-results'."
  (interactive)
  (let* ((project-root (project-search--project-root))
         (server (project-search--find-eglot-server project-root))
         (project-search--ivy-project-root project-root)
         (project-search--ivy-server server)
         (project-search--ivy-default-directory default-directory)
         (default-directory project-root))
    (ivy-read (format "[%s] sym: " (projectile-project-name))
              #'project-search--ivy-function
              :dynamic-collection t
              :require-match t
              :action #'project-search--ivy-action
              :caller 'ivy-project-search)))

(defun project-search--unwind ()
  "Clean up debounce timer on ivy exit."
  (when project-search--debounce-timer
    (cancel-timer project-search--debounce-timer)
    (setq project-search--debounce-timer nil)))

(with-eval-after-load 'ivy
  (ivy-configure 'ivy-project-search
    :unwind-fn #'project-search--unwind)
  (add-to-list 'ivy-more-chars-alist '(ivy-project-search . 2)))

(provide 'project-search)
;;; project-search.el ends here
