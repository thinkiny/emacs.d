;;; project-search.el --- Unified project search  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ivy-xref)
(require 'projectile)
(require 'eglot)

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

(defcustom project-search-sync-max-results 6
  "Maximum number of results for the synchronous search path."
  :type 'integer
  :group 'project-search)

(defcustom project-search-sync-truncate-width 80
  "Maximum width for truncating text in synchronous search results."
  :type 'integer
  :group 'project-search)

;;; ---- Shared Backend ----

(defun project-search--project-root ()
  "Return the current project root directory."
  (or (ignore-errors (projectile-project-root))
      default-directory))

(defun project-search--lsp-kind-to-string (kind)
  "Convert LSP SymbolKind integer KIND to a human-readable string."
  (if (boundp 'eglot--symbol-kind-names)
      (or (alist-get kind eglot--symbol-kind-names) "Unknown")
    "Unknown"))

(defun project-search--lsp-filter-and-sort-by-score (results)
  "Filter and order LSP symbol RESULTS using optional `:score`.

Rules:
- Drop entries where `:score` is present and numeric 0.
- If an entry lacks `:score`, treat it as score -1 (i.e., lower than any
  non-negative scored result).
- Preserve the original order of unscored entries.

Implementation detail: If there are no scored entries at all, we return the
filtered unscored list as-is (no sorting)."
  (let ((scored nil)
        (unscored nil))
    (dolist (r (append results nil))
      (let ((has-score (plist-member r :score))
            (score (plist-get r :score)))
        (unless (and has-score (numberp score) (zerop score))
          (if has-score
              (push r scored)
            (push r unscored)))))
    (setq unscored (nreverse unscored))
    (if (null scored)
        unscored
      (nconc (seq-sort-by (lambda (r) (or (plist-get r :score) -1)) #'> scored)
             unscored))))

(defun project-search--find-eglot-server (&optional project-root)
  "Find an active Eglot server for PROJECT-ROOT that supports workspace/symbol.

Returns nil when no suitable server exists, causing callers to fall back to
ripgrep."
  (let* ((root (or project-root (project-search--project-root)))
         (bufs (projectile-project-buffers root)))
    (cl-some (lambda (buf)
               (with-current-buffer buf
                 (when-let* ((server (eglot-current-server)))
                   (let ((eglot--cached-server server))
                     (and (eglot-server-capable :workspaceSymbolProvider)
                          server)))))
             bufs)))

(defun project-search--location-line-col (loc)
  "Return (LINE . COL) for xref location LOC.
Try `xref-location-line' first, then resolve via `xref-location-marker'.
Falls back to (1 . 0) when neither works."
  (let ((line (xref-location-line loc))
        (col  (ignore-errors (xref-file-location-column loc))))
    (if line
        (cons line (or col 0))
      (condition-case nil
          (let ((marker (xref-location-marker loc)))
            (with-current-buffer (marker-buffer marker)
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char marker)
                  (cons (line-number-at-pos) (current-column))))))
        (error '(1 . 0))))))

(defun project-search--read-line (file line)
  "Return the trimmed text of LINE in FILE, or nil on error."
  (condition-case nil
      (let ((buf (find-buffer-visiting file)))
        (if buf
            (with-current-buffer buf
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char (point-min))
                  (forward-line (1- line))
                  (string-trim (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position))))))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (forward-line (1- line))
            (string-trim (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))))
    (error nil)))

(defun project-search--sync-format-result (item)
  "Convert xref ITEM to the normalized alist format for MCP."
  (let* ((summary (xref-item-summary item))
         (loc     (xref-item-location item))
         (file    (xref-location-group loc))
         (lc      (project-search--location-line-col loc))
         (line    (car lc))
         (col     (cdr lc))
         (lsp-p   (get-text-property 0 'project-search-lsp summary))
         (kind-and-name
          (if (and lsp-p (string-match "\\`#\\([a-z]+\\) \\(.*\\)" summary))
              (cons (capitalize (match-string 1 summary))
                    (match-string 2 summary))
            (cons "Text" summary)))
         (source  (and lsp-p (project-search--read-line file line)))
         (text    (and source (project-search--truncate-text
                               source nil project-search-sync-truncate-width)))
         (uri     (concat "file://" (expand-file-name file))))
    `((name . ,(cdr kind-and-name))
      (kind . ,(car kind-and-name))
      ,@(when text `((text . ,text)))
      (location . ((uri . ,uri)
                   (line . ,line)
                   (col . ,col))))))

(defun project-search--xref-query (query)
  "Search for QUERY using the current xref backend."
  (when-let* ((backend (xref-find-backend)))
    (xref-backend-apropos backend query)))

(defun project-search--sync-lsp-xrefs (server query &optional max-width)
  "Query SERVER for workspace symbols matching QUERY."
  (condition-case err
      (let* ((raw (jsonrpc-request server :workspace/symbol `(:query ,query)))
             (ordered (project-search--lsp-filter-and-sort-by-score raw))
             (xrefs (project-search--lsp-to-xrefs ordered query max-width)))
        xrefs)
    (error (message "project-search LSP error: %S" err) nil)))

(defun project-search--sync-rg-fallback (query root limit width)
  "Run rg search, returning xref items."
  (ignore-errors (project-search--rg-to-xrefs query root limit nil width)))

(defun project-search-sync-query (query)
  "Search project for QUERY, returning normalized alist results.
Uses Eglot when available, falling back to xref then ripgrep."
  (let* ((root (project-search--project-root))
         (limit project-search-sync-max-results)
         (width project-search-sync-truncate-width)
         (server (project-search--find-eglot-server root))
         (xrefs (if server
                    (or (project-search--sync-lsp-xrefs server query width)
                        (project-search--sync-rg-fallback query root limit width))
                  ;; No LSP: try xref, fall back to rg
                  (or (ignore-errors (project-search--xref-query query))
                      (project-search--sync-rg-fallback query root limit width)))))
    (seq-take (seq-map #'project-search--sync-format-result (or xrefs '()))
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

(defvar project-search--rg-process nil
  "Current async rg process for ivy-project-rg.")

(defun project-search--truncate-text (text &optional query max-width)
  "Truncate TEXT based on MAX-WIDTH, keeping QUERY centered with context.
MAX-WIDTH defaults to window width for ivy, or can be explicitly provided."
  (let* ((max-length (or max-width
                         (max 50 (- (window-width (minibuffer-window)) 20)))))
    (if (<= (length text) max-length)
        text
      ;; Text needs truncation
      (if (and query (not (string-empty-p query))
               (string-match-p (regexp-quote query) text))
          ;; Center query with context on both sides
          (let* ((pos (string-match (regexp-quote query) text))
                 (query-len (length query))
                 (half-space (/ (- max-length query-len) 2))
                 (start (max 0 (- pos half-space)))
                 (end (min (length text) (+ start max-length))))
            (concat (if (> start 0) "..." "")
                    (substring text start end)
                    (if (< end (length text)) "..." "")))
        ;; No query or not found - truncate from start
        (concat (substring text 0 (- max-length 3)) "...")))))

(defun project-search--lsp-to-xrefs (results &optional query max-width)
  "Convert LSP SymbolInformation plists RESULTS to xref items.
QUERY is optionally used for smart truncation of long symbol names.
MAX-WIDTH controls text truncation (defaults to window width)."
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
          (xref-make (propertize
                      (project-search--truncate-text
                       (format "#%s %s" (downcase kind-name) name)
                       query
                       max-width)
                      'project-search-lsp t)
                     (xref-make-file-location file-path line (or col 0))))))
    results)))

(defconst project-search--rg-base-args
  '("--no-heading" "--line-number" "--column"
    "--ignore-case" "--color" "never" "--hidden")
  "Base ripgrep flags shared by sync and async search.")

(defun project-search--parse-rg-line (line project-root)
  "Parse a single ripgrep output LINE.

Return a list (FILE LINE COL TEXT) where FILE is an absolute path,
or nil when LINE doesn't match the expected `rg --line-number --column'
format.  PROJECT-ROOT is used to resolve relative paths."
  (when (string-match
         "\\(?:\\./\\)?\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)" line)
    (list (expand-file-name (match-string 1 line) project-root)
          (string-to-number (match-string 2 line))
          (string-to-number (match-string 3 line))
          (match-string 4 line))))

(defun project-search--rg-match-to-xref (match query &optional max-width)
  "Convert MATCH from `project-search--parse-rg-line' to an xref item.

MAX-WIDTH is forwarded to `project-search--truncate-text'."
  (pcase-let ((`(,file ,line ,col ,text) match))
    (xref-make (project-search--truncate-text (string-trim text) query max-width)
               (xref-make-file-location file line col))))

(defun project-search--rg-to-xrefs (query project-root max-results &optional extra-args max-width)
  "Run rg for QUERY in PROJECT-ROOT and return xref items.
Results are capped at MAX-RESULTS.  EXTRA-ARGS is an optional
string of additional rg flags (e.g. \"-tgo -i\").
MAX-WIDTH controls text truncation (defaults to window width)."
  (let ((default-directory project-root)
        (limit (or max-results project-search-max-results))
        (xrefs '()))
    (with-temp-buffer
      (apply #'call-process "rg" nil t nil
             (append (when extra-args (split-string-and-unquote extra-args))
                     (append project-search--rg-base-args
                             (list "--" query "."))))
      (goto-char (point-min))
      (while (and (not (eobp)) (< (length xrefs) limit))
        (when-let* ((line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position)))
                    (match (project-search--parse-rg-line line project-root)))
          (push (project-search--rg-match-to-xref match query max-width) xrefs))
        (forward-line 1)))
    (nreverse xrefs)))

(defun project-search--update-ivy-candidates (xrefs)
  "Update ivy candidates from XREFS."
  (let ((collection (if xrefs
                        (ivy-xref-make-collection xrefs)
                      '())))
    (setq project-search--ivy-candidates collection)
    (run-with-idle-timer
     0 nil
     (lambda ()
       (ignore-errors
         (ivy-update-candidates (mapcar #'car collection)))))))

(defun project-search--ivy-action (candidate)
  "Jump to the xref location associated with CANDIDATE."
  (when-let* ((entry (assoc candidate project-search--ivy-candidates)))
    (let ((marker (xref-location-marker (cdr entry))))
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker))))

(defun project-search--parse-input (input)
  "Parse INPUT into (QUERY . KIND-PREFIX).
A leading `#word ' is treated as a kind filter prefix.
e.g. \"#fu myFunc\" -> (\"myFunc\" . \"fu\"), \"foo\" -> (\"foo\" . nil)."
  (if (string-match "\\`#\\([a-zA-Z]+\\)\\s-+" input)
      (cons (string-trim (replace-regexp-in-string "\\`#[a-zA-Z]+\\s-+" "" input))
            (downcase (match-string 1 input)))
    (cons (string-trim input) nil)))

(defun project-search--kind-matches-prefix-p (kind-int prefix)
  "Return non-nil if LSP SymbolKind KIND-INT matches PREFIX.
PREFIX is a lowercase string matched against the start of the
kind name from `eglot--symbol-kind-names'."
  (when-let* ((kind-name (project-search--lsp-kind-to-string kind-int)))
    (string-prefix-p prefix (downcase kind-name))))

(defun project-search--rg-fallback-ivy (query project-root req-id max-results)
  "Run rg fallback and update ivy if REQ-ID is still current."
  (when (= req-id project-search--request-id)
    (project-search--update-ivy-candidates
     (project-search--rg-to-xrefs query project-root max-results))))

(defun project-search--send-lsp-request (req-id server query project-root &optional kind-prefix max-results)
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
                (ordered (project-search--lsp-filter-and-sort-by-score filtered))
                (xrefs (project-search--lsp-to-xrefs ordered query)))
           (if xrefs
               (project-search--update-ivy-candidates xrefs)
             (project-search--rg-fallback-ivy query project-root req-id max-results))))))
   :error-fn
   (lambda (&rest _)
     (project-search--rg-fallback-ivy query project-root req-id max-results))
   :timeout-fn
   (lambda ()
     (project-search--rg-fallback-ivy query project-root req-id max-results))))

(defun project-search--debounce-send (fn &rest args)
  "Cancel pending timer, schedule FN with ARGS after debounce delay.
Returns 0 for ivy dynamic collection.
The current `project-search--request-id' value is prepended to ARGS
so the scheduled function receives the ID that was current at call time."
  (when project-search--debounce-timer
    (cancel-timer project-search--debounce-timer))
  (cl-incf project-search--request-id)
  (setq project-search--debounce-timer
        (apply #'run-with-timer project-search-debounce-delay nil
               fn project-search--request-id args))
  0)

(defun project-search--ivy-lsp-query (server query project-root kind-prefix max-results)
  "Send a debounced async LSP workspace/symbol request."
  (project-search--debounce-send
   #'project-search--send-lsp-request
   server query project-root kind-prefix max-results))

(defun project-search--ivy-function (input &rest _)
  "Dynamic collection function for `ivy-project-search'."
  (pcase-let ((`(,query . ,kind-prefix) (project-search--parse-input input)))
    (or (let ((ivy-text query)) (ivy-more-chars))
        (if project-search--ivy-server
            (project-search--ivy-lsp-query
             project-search--ivy-server query
             project-search--ivy-project-root kind-prefix
             project-search-max-results)
          (project-search--ivy-rg-query
           query project-search--ivy-project-root
           project-search--ivy-rg-extra-args
           project-search-max-results)))))

(defun project-search--send-rg-request (req-id query project-root extra-args max-results)
  "Send async rg request for QUERY in PROJECT-ROOT.
REQ-ID is checked against `project-search--request-id' to discard stale
responses."
  (when (process-live-p project-search--rg-process)
    (kill-process project-search--rg-process))
  (let* ((default-directory project-root)
         (buf (generate-new-buffer " *project-search-rg*" t))
         (args (append (when extra-args (split-string-and-unquote extra-args))
                      (append project-search--rg-base-args
                              '("--line-buffered")
                              (list "--max-count" (number-to-string max-results)
                                    "--" query "."))))
         (pending "")
         (xrefs '())
         (count 0)
         (proc (apply #'start-process "project-search-rg" buf "rg" args)))
    (setq project-search--rg-process proc)
    (set-process-query-on-exit-flag proc nil)

    (set-process-filter
     proc
     (lambda (_process chunk)
       (when (= req-id project-search--request-id)
         (setq pending (concat pending chunk))
         (let* ((parts (split-string pending "\n"))
                (tail (car (last parts)))
                (lines (butlast parts)))
           (setq pending tail)
           (dolist (line lines)
             (when (< count max-results)
               (when-let* ((match (project-search--parse-rg-line line project-root)))
                 (push (project-search--rg-match-to-xref match query) xrefs)
                 (cl-incf count))))
           (project-search--update-ivy-candidates (nreverse (copy-sequence xrefs))))
         (when (and (>= count max-results) (process-live-p proc))
           (kill-process proc)))))

    (set-process-sentinel
     proc
     (lambda (_process _event)
       (when (= req-id project-search--request-id)
         (when (and (< count max-results)
                    (not (string-empty-p pending)))
           (when-let* ((match (project-search--parse-rg-line pending project-root)))
             (push (project-search--rg-match-to-xref match query) xrefs)
             (cl-incf count)))
         (ignore-errors
           (project-search--update-ivy-candidates (nreverse (copy-sequence xrefs)))))))))

(defun project-search--ivy-rg-query (query project-root extra-args max-results)
  "Send a debounced async rg request."
  (project-search--debounce-send
   #'project-search--send-rg-request
   query project-root extra-args max-results))

(defun project-search--ivy-rg-function (input &rest _)
  "Dynamic collection function for `ivy-project-rg' using only ripgrep."
  (let ((query (string-trim input)))
    (or (let ((ivy-text query)) (ivy-more-chars))
        (project-search--ivy-rg-query
         query project-search--ivy-project-root
         project-search--ivy-rg-extra-args
         project-search-max-results))))

(defun project-search--ivy-read (prompt collection-fn caller &optional initial-input)
  "Start an ivy session with PROMPT, COLLECTION-FN, and CALLER.
INITIAL-INPUT is inserted into the minibuffer after setup."
  (setq project-search--ivy-default-directory default-directory)
  (minibuffer-with-setup-hook
      (lambda () (when (and initial-input (not (string-empty-p initial-input)))
                   (insert initial-input)))
    (ivy-read (format "[%s] %s" (projectile-project-name) prompt)
              collection-fn
              :dynamic-collection t
              :require-match t
              :action #'project-search--ivy-action
              :caller caller)))

;;;###autoload
(defun ivy-project-rg (&optional options)
  "Search project with ripgrep using xref-based ivy display.
OPTIONS is an optional string of extra rg flags (e.g. \"-tgo -i\").
Session variables are set with `setq' so they survive `ivy-resume'."
  (interactive)
  (let* ((project-root (project-search--project-root))
         (default-directory project-root))
    (setq project-search--ivy-project-root project-root
          project-search--ivy-server nil
          project-search--ivy-rg-extra-args options)
    (project-search--ivy-read "rg: " #'project-search--ivy-rg-function 'ivy-project-rg)))

;;;###autoload
(defun ivy-project-search (&optional initial-query)
  "Search project symbols via LSP or ripgrep with ivy completion.
When an active Eglot server exists for the project, queries
workspace/symbol asynchronously.  Otherwise falls back to async
ripgrep text search.  Results are limited to
`project-search-max-results'.
Session variables are set with `setq' so they survive `ivy-resume'.
When called interactively with prefix arg, pre-fill with symbol at point."
  (interactive)
  (let* ((project-root (project-search--project-root))
         (server (project-search--find-eglot-server project-root))
         (default-directory project-root))
    (setq project-search--ivy-project-root project-root
          project-search--ivy-server server)
    (project-search--ivy-read "search: " #'project-search--ivy-function 'ivy-project-search initial-query)))

;;;###autoload
(defun ivy-project-search-at-point ()
  "Like `ivy-project-search' but pre-filled with symbol at point."
  (interactive)
  (ivy-project-search (thing-at-point 'symbol t)))

(defun project-search--unwind ()
  "Clean up debounce timer on ivy exit."
  (when project-search--debounce-timer
    (cancel-timer project-search--debounce-timer)
    (setq project-search--debounce-timer nil))
  (when (process-live-p project-search--rg-process)
    (kill-process project-search--rg-process)))

(defun project-search--ivy-occur (_cands)
  "Populate an ivy-occur buffer from the current project-search session.
The dynamic collection functions return 0 to signal async results,
which crashes `ivy--occur-default'.  This handler reads from
`ivy--all-candidates' instead."
  (let ((cands ivy--all-candidates))
    (ivy-occur-mode)
    (insert (format "%d candidates:\n" (length cands)))
    (ivy--occur-insert-lines cands)
    (read-only-mode)))

(with-eval-after-load 'ivy
  (dolist (cmd '(ivy-project-search ivy-project-rg))
    (ivy-configure cmd
      :occur #'project-search--ivy-occur
      :unwind-fn #'project-search--unwind)
    (add-to-list 'ivy-more-chars-alist `(,cmd . 2))))


(defun get-default-args-for-ripgrep()
  (let ((file-name (buffer-file-name)))
    (concat (if file-name
                (pcase (file-name-extension file-name)
                  ("go" "-tgo")
                  ("py" "-tpython")
                  ("js" "-tjs")
                  ("cc" "-tcpp")
                  (_ ""))
              "")
            " -i")))

(defun ivy-project-rg-type()
  (interactive)
  (ivy-project-rg (get-default-args-for-ripgrep)))

(use-package counsel-projectile
  :after (ivy-xref projectile)
  :config
  (counsel-projectile-mode)
  (define-key projectile-command-map (kbd "s t") #'ivy-project-rg-type)
  (define-key projectile-command-map (kbd "s g") #'ivy-project-rg)
  (define-key projectile-command-map (kbd "s s") #'ivy-project-search)
  (define-key projectile-command-map (kbd "s .") #'ivy-project-search-at-point))

(provide 'project-search)
;;; project-search.el ends here
