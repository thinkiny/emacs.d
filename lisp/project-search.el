;;; project-search.el --- Unified project search  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'eglot nil t)
(require 'projectile nil t)

(declare-function eglot-current-server "eglot")
(declare-function eglot-server-capable "eglot")
(declare-function eglot-uri-to-path "eglot")
(declare-function jsonrpc-request "jsonrpc")
(declare-function jsonrpc-async-request "jsonrpc")
(declare-function projectile-project-root "projectile")
(declare-function projectile-project-buffers "projectile")
(declare-function ivy-more-chars "ivy")
(declare-function ivy-read "ivy")
(declare-function ivy--set-candidates "ivy")
(declare-function ivy--insert-minibuffer "ivy")
(declare-function ivy--format "ivy")
(declare-function ivy-configure "ivy")
(defvar ivy-last)
(defvar ivy--all-candidates)
(declare-function counsel--async-command "counsel")
(declare-function counsel-git-grep-action "counsel")
(declare-function counsel-git-grep-transformer "counsel")
(declare-function counsel--grep-unwind "counsel")

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
Only affects the LSP path in the interactive ivy command;
the rg fallback uses `counsel-async-command-delay'."
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

(defun project-search--make-result (name kind uri start-line start-char end-line end-char &optional container)
  "Build a normalized symbol result alist with NAME, KIND, URI, range, and CONTAINER."
  `((name . ,name)
    (kind . ,kind)
    (location . ((uri . ,uri)
                 (range . ((start . ((line . ,start-line) (character . ,start-char)))
                           (end   . ((line . ,end-line)   (character . ,end-char)))))))
    (containerName . ,(or container ""))))

(defun project-search--parse-lsp-result (res)
  "Convert an LSP SymbolInformation plist RES to a result alist."
  (let* ((kind  (plist-get res :kind))
         (loc   (plist-get res :location))
         (range (plist-get loc :range))
         (start (plist-get range :start))
         (end   (plist-get range :end)))
    (project-search--make-result
     (plist-get res :name)
     (project-search--lsp-kind-to-string kind)
     (plist-get loc :uri)
     (plist-get start :line)
     (plist-get start :character)
     (plist-get end :line)
     (plist-get end :character)
     (plist-get res :containerName))))

(defun project-search--eglot-query-sync (query &optional kind-filter project-root)
  "Search workspace symbols matching QUERY via Eglot synchronously.
When KIND-FILTER is non-nil, keep only symbols of that kind."
  (when-let* ((server (project-search--find-eglot-server project-root)))
    (let* ((raw (jsonrpc-request server :workspace/symbol `(:query ,query)))
           (results (append raw nil))
           (filtered (if (and kind-filter (not (string-empty-p kind-filter)))
                         (seq-filter
                          (lambda (r)
                            (string-equal
                             (project-search--lsp-kind-to-string (plist-get r :kind))
                             kind-filter))
                          results)
                       results)))
      (seq-map #'project-search--parse-lsp-result filtered))))

(defun project-search--rg-parse-match (data project-root)
  "Parse a ripgrep JSON match DATA into a result alist.
PROJECT-ROOT expands relative paths into file URIs."
  (let* ((path (alist-get 'text (alist-get 'path data)))
         (line-num (alist-get 'line_number data))
         (text (alist-get 'text (alist-get 'lines data)))
         (submatches (alist-get 'submatches data))
         (has-sub (and submatches (> (length submatches) 0)))
         (start-col (if has-sub (alist-get 'start (aref submatches 0)) 0))
         (end-col   (if has-sub (alist-get 'end   (aref submatches 0)) 0)))
    (project-search--make-result
     (string-trim (or text ""))
     "Text"
     (concat "file://" (expand-file-name path project-root))
     (1- line-num) start-col
     (1- line-num) end-col)))

(defun project-search--rg-query (query project-root &optional max-results)
  "Search for QUERY in PROJECT-ROOT using ripgrep with JSON output.
Total results are capped at MAX-RESULTS."
  (let ((default-directory project-root)
        (limit (or max-results project-search-max-results))
        (results '()))
    (with-temp-buffer
      (call-process "rg" nil t nil
                    "--json" "--smart-case"
                    "--" query ".")
      (goto-char (point-min))
      (while (and (not (eobp)) (< (length results) limit))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (parsed (ignore-errors
                         (json-parse-string line :object-type 'alist))))
          (when (and parsed (string-equal (alist-get 'type parsed) "match"))
            (push (project-search--rg-parse-match
                   (alist-get 'data parsed) project-root)
                  results)))
        (forward-line 1)))
    (nreverse results)))

(defun project-search--search (query &optional kind max-results)
  "Search project for QUERY, optionally filtering by KIND.
Uses Eglot when available, falls back to ripgrep.
Results are limited to MAX-RESULTS (default `project-search-max-results')."
  (let* ((project-root (project-search--project-root))
         (limit (or max-results project-search-max-results))
         (eglot-results (ignore-errors
                          (project-search--eglot-query-sync
                           query kind project-root)))
         (all-results (or eglot-results
                          (ignore-errors
                            (project-search--rg-query
                             query project-root limit)))))
    (seq-take (or all-results '()) limit)))

;;; ---- Ivy Interactive Command ----

(defvar project-search--ivy-project-root nil
  "Project root for the current `ivy-project-search' session.")

(defvar project-search--ivy-server nil
  "Eglot server for the current `ivy-project-search' session.")

(defvar project-search--request-id 0
  "Monotonic counter to discard stale async LSP responses.")

(defvar project-search--debounce-timer nil
  "Timer for debouncing LSP workspace/symbol requests.")

(defun project-search--format-lsp-candidate (res project-root)
  "Format LSP SymbolInformation plist RES as an ivy candidate string.
PROJECT-ROOT is used for relative path display."
  (let* ((name (plist-get res :name))
         (kind (plist-get res :kind))
         (kind-name (project-search--lsp-kind-to-string kind))
         (loc (plist-get res :location))
         (uri (plist-get loc :uri))
         (range (plist-get loc :range))
         (line (1+ (plist-get (plist-get range :start) :line)))
         (col (plist-get (plist-get range :start) :character))
         (file-path (eglot-uri-to-path uri))
         (rel-path (file-relative-name file-path project-root)))
    (propertize
     (concat
      (propertize (format "%s:%d: " rel-path line) 'face 'compilation-info)
      (propertize (format "#%s " (downcase kind-name)) 'face 'font-lock-type-face)
      name)
     'project-search-file file-path
     'project-search-line line
     'project-search-col (or col 0))))

(defun project-search--ivy-transformer (candidate)
  "Display transformer for `ivy-project-search' candidates.
LSP candidates pass through (already formatted); rg candidates
are transformed via `counsel-git-grep-transformer'."
  (if (get-text-property 0 'project-search-file candidate)
      candidate
    (counsel-git-grep-transformer candidate)))

(defun project-search--ivy-action (candidate)
  "Jump to the location described by CANDIDATE.
Dispatches between LSP results (text-property based) and rg
results (file:line:text format)."
  (if (get-text-property 0 'project-search-file candidate)
      (let ((file (get-text-property 0 'project-search-file candidate))
            (line (get-text-property 0 'project-search-line candidate))
            (col  (get-text-property 0 'project-search-col candidate)))
        (find-file file)
        (goto-char (point-min))
        (forward-line (1- line))
        (forward-char (or col 0)))
    (counsel-git-grep-action candidate)))

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

(defun project-search--send-lsp-request (server query project-root req-id &optional kind-prefix)
  "Send workspace/symbol request for QUERY to SERVER.
PROJECT-ROOT is used for candidate formatting.  REQ-ID is checked
against `project-search--request-id' to discard stale
responses.  When KIND-PREFIX is non-nil, only symbols whose kind
name starts with that prefix are kept."
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
                (candidates
                 (seq-filter
                  #'identity
                  (seq-map
                   (lambda (r)
                     (ignore-errors
                       (project-search--format-lsp-candidate
                        r project-root)))
                   filtered))))
           (ivy--set-candidates candidates)
           (ivy--insert-minibuffer
            (ivy--format ivy--all-candidates))))))
   :error-fn
   (lambda (&rest _)
     (message "project-search: LSP request failed"))
   :timeout-fn
   (lambda ()
     (message "project-search: LSP request timed out"))))

(defun project-search--ivy-function (input)
  "Dynamic collection function for `ivy-project-search'.
INPUT is the current minibuffer text.  For LSP, debounces then
sends an async request; for rg, delegates to counsel's async
command infrastructure.  A `#prefix' at the start of INPUT filters
LSP results by kind and is stripped before querying."
  (let ((server project-search--ivy-server)
        (project-root project-search--ivy-project-root)
        (max-results project-search-max-results)
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
                     server query project-root req-id kind-prefix))
              nil)
          (let ((default-directory project-root))
            (counsel--async-command
             (format "rg -S --no-heading --line-number --color never -- %s . | head -n %d"
                     (shell-quote-argument query)
                     max-results))
            nil)))))

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
         (default-directory project-root))
    (ivy-read (if server "LSP Symbols: " "rg search: ")
              #'project-search--ivy-function
              :dynamic-collection t
              :require-match t
              :action #'project-search--ivy-action
              :caller 'ivy-project-search)))

(defun project-search--unwind ()
  "Clean up debounce timer and counsel process on ivy exit."
  (when project-search--debounce-timer
    (cancel-timer project-search--debounce-timer)
    (setq project-search--debounce-timer nil))
  (counsel--grep-unwind))

(with-eval-after-load 'ivy
  (with-eval-after-load 'counsel
    (ivy-configure 'ivy-project-search
      :display-transformer-fn #'project-search--ivy-transformer
      :unwind-fn #'project-search--unwind
      :exit-codes '(1 "No matches found")))
  (add-to-list 'ivy-more-chars-alist '(ivy-project-search . 2)))

(provide 'project-search)
;;; project-search.el ends here
