;;; claude-extra-mcp-tools.el --- MCP tools for xwidget buffers  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'files)
(require 'subr-x)
(require 'seq)
(require 'xref)
(require 'xwidget)
(require 'eglot nil t)
(require 'projectile nil t)


(declare-function claude-code-ide-mcp--send-notification "claude-code-ide-mcp")
(declare-function claude-code-ide-make-tool "claude-code-ide-mcp-server")

(defgroup claude-extra-mcp-tools nil
  "Extra MCP tools for Claude Code IDE."
  :group 'tools
  :prefix "claude-extra-")

(defcustom claude-extra-workspace-search-max-results 10
  "Maximum number of results returned by workspace search."
  :type 'integer
  :group 'claude-extra-mcp-tools)

(defcustom claude-xwidgets-mcp-timeout-seconds 0.8
  "Timeout in seconds while waiting for xwidget JavaScript callback results."
  :type 'number
  :group 'claude-extra-mcp-tools)

(defcustom claude-xwidgets-mcp-poll-interval 1.5
  "Interval in seconds for polling xwidget selection changes.
Xwidget web views handle mouse events via GTK/WebKit, bypassing
Emacs's command loop.  A polling timer is the only way to detect
selection changes in these buffers."
  :type 'number
  :group 'claude-extra-mcp-tools)

(defconst claude-xwidgets--selected-text-js
  "(function() {
  var s = window.getSelection().toString();
  if (s) return s;
  var iframes = document.getElementsByTagName('iframe');
  for (var i = 0; i < iframes.length; i++) {
    try {
      s = iframes[i].contentWindow.getSelection().toString();
      if (s) return s;
    } catch(e) {}
  }
  return '';
})();"
  "JavaScript that returns current page selection text, including iframe content.")

(defconst claude-xwidgets--pdf-visible-text-js
  "(function() {
  try {
    var app = PDFViewerApplication;
    if (!app || !app.pdfViewer) return '';
    var pageNum = app.pdfViewer.currentPageNumber;
    var pageEl = document.querySelector('.page[data-page-number=\"' + pageNum + '\"]');
    if (!pageEl) return '';
    var textLayer = pageEl.querySelector('.textLayer');
    if (!textLayer) return '';
    return textLayer.textContent || '';
  } catch(e) {
    return '';
  }
})();"
  "JavaScript that extracts text from the current PDF page via the rendered text layer.")

(defconst claude-xwidgets--viewport-visible-text-js
  "(function() {
  var elements = document.querySelectorAll('body *:not(script):not(style)');
  var visibleText = '';
  elements.forEach(function(el) {
    var rect = el.getBoundingClientRect();
    var isInViewport = (
      rect.top >= 0 && rect.left >= 0 &&
      rect.bottom <= (window.innerHeight || document.documentElement.clientHeight) &&
      rect.right <= (window.innerWidth || document.documentElement.clientWidth)
    );
    if (isInViewport && el.checkVisibility()) {
      Array.from(el.childNodes).forEach(function(node) {
        if (node.nodeType === Node.TEXT_NODE && node.textContent.trim() !== '') {
          visibleText += node.textContent.trim() + ' ';
        }
      });
    }
  });
  return visibleText.trim();
})();"
  "JavaScript that extracts visible text from the current viewport.")

;;; State

(defvar claude-xwidgets--poll-timer nil
  "Repeating timer for polling xwidget selection changes.")

(defvar-local claude-xwidgets--last-selection nil
  "Last xwidget selection text, cached from the poll timer.
Used both for change detection (notifications) and as a fast
return value for `claude-xwidgets--selected-text'.")

;;; Helpers

(defun claude-xwidgets--non-claude-code-buffer ()
  "Return the first visible buffer whose name does not start with \"*claude-code\".
Falls back to `current-buffer' if no such window is found."
  (or (cl-loop for w in (window-list)
               for b = (window-buffer w)
               unless (string-prefix-p "*claude-code" (buffer-name b))
               return b)
      (current-buffer)))

(defun claude-xwidgets--session ()
  "Return the current xwidget WebKit session or nil."
  (when (fboundp 'xwidget-webkit-current-session)
    (ignore-errors (xwidget-webkit-current-session))))

(defun claude-xwidgets--buffer-p ()
  "Return non-nil when current buffer is backed by an xwidget session."
  (claude-xwidgets--session))

(defun claude-xwidgets--pdf-buffer-p ()
  "Return non-nil when current buffer is a PDF xwidget buffer."
  (string-prefix-p "*PDF:" (buffer-name)))

(defun claude-xwidgets--resolve-buffer (file-path)
  "Resolve FILE-PATH to a buffer.
When FILE-PATH is nil or empty, return the first non-claude-code buffer.
Otherwise try exact match, then fuzzy name match, then `current-buffer'."
  (cond
   ((or (null file-path) (string-empty-p file-path))
    (claude-xwidgets--non-claude-code-buffer))
   (t
    (or (get-file-buffer file-path)
        (cl-find-if
         (lambda (b)
           (string-match-p (regexp-quote (file-name-nondirectory file-path))
                           (buffer-name b)))
         (buffer-list))
        (current-buffer)))))

(defun claude-xwidgets--call-in-buffer (file-path thunk)
  "Call THUNK in the buffer associated with FILE-PATH.
When FILE-PATH is nil or empty, call THUNK in the current buffer."
  (let* ((buf (claude-xwidgets--resolve-buffer file-path))
         (win (get-buffer-window buf t)))
    (if win
        (with-selected-window win
          (funcall thunk))
      (with-current-buffer buf
        (funcall thunk)))))

(defun claude-xwidgets--eval-sync (script &optional timeout-seconds)
  "Execute JavaScript SCRIPT in current xwidget and wait for callback result.
Return the callback string result, or nil on timeout."
  (let* ((session (claude-xwidgets--session))
         (timeout (or timeout-seconds claude-xwidgets-mcp-timeout-seconds))
         (deadline (+ (float-time) timeout))
         (done nil)
         (result nil))
    (unless session
      (error "Current buffer does not have an active xwidget session"))
    (xwidget-webkit-execute-script
     session
     script
     (lambda (value)
       (setq result value)
       (setq done t)))
    (while (and (not done) (< (float-time) deadline))
      (sit-for 0.01 t))
    result))

(defun claude-xwidgets--selected-text ()
  "Return selected text from the current xwidget page.
Prefer the cached value from the poll timer when available,
falling back to a synchronous JavaScript evaluation."
  (or claude-xwidgets--last-selection
      (claude-xwidgets--eval-sync claude-xwidgets--selected-text-js)
      ""))

(defun claude-xwidgets--xwidget-selection-alist (&optional text)
  "Build a selection alist for the current xwidget buffer.
TEXT defaults to the result of `claude-xwidgets--selected-text'."
  (let* ((text (or text (claude-xwidgets--selected-text)))
         (file-path (buffer-file-name))
         (empty (string-empty-p text)))
    `((text . ,text)
      (filePath . ,(or file-path ""))
      ,@(when file-path
          `((fileUrl . ,(concat "file://" file-path))))
      (selection . ((start . ((line . 1) (character . 1)))
                    (end . ((line . 1) (character . 1)))
                    (isEmpty . ,(if empty t :json-false)))))))

;;; getCurrentSelection :around advice

(defun claude-xwidgets--get-current-selection-around (orig-fn arguments)
  "Around advice: handle xwidget buffers, delegate everything else.
ORIG-FN is the original `getCurrentSelection' handler.
ARGUMENTS are the MCP tool arguments (unused)."
  (if (claude-xwidgets--buffer-p)
      (claude-xwidgets--xwidget-selection-alist)
    ;; Not an xwidget buffer: call the original handler
    (funcall orig-fn arguments)))

;;; get-selection (standalone tool)

(defun claude-xwidgets--handle-get-selection-text (&optional file-path)
  "Handle get-selection MCP tool call.
Returns the currently selected text and its context.
Handles both xwidget buffers (via JavaScript) and regular buffers
\(via Emacs region).
When FILE-PATH is given, operate on that buffer instead of current."
  (claude-xwidgets--call-in-buffer
   file-path
   (lambda ()
     (if (claude-xwidgets--buffer-p)
         (claude-xwidgets--xwidget-selection-alist)
       ;; Regular buffer: delegate to the original handler
       (claude-code-ide-mcp-handle-get-current-selection nil)))))

;;; getVisibleText

(defun claude-xwidgets--format-visible-text (text &optional file-path url)
  "Format TEXT with an optional FILE-PATH or URL header line."
  (format "%s%s"
          (cond (file-path (format "%s\n" file-path))
                (url (format "%s\n" url))
                (t ""))
          text))

(defun claude-xwidgets--get-pdf-visible-text ()
  "Extract text from the current PDF page via the rendered text layer."
  (let ((text (or (claude-xwidgets--eval-sync claude-xwidgets--pdf-visible-text-js) "")))
    (claude-xwidgets--format-visible-text text (buffer-file-name))))

(defun claude-xwidgets--get-xwidget-visible-text ()
  "Extract visible text from the current xwidget viewport."
  (let ((text (or (claude-xwidgets--eval-sync
                   claude-xwidgets--viewport-visible-text-js)
                  "")))
    (claude-xwidgets--format-visible-text
     text
     (buffer-file-name)
     (ignore-errors (xwidget-webkit-uri (claude-xwidgets--session))))))

(defun claude-xwidgets--get-buffer-visible-text ()
  "Extract the text currently visible in the Emacs window."
  (let ((text (buffer-substring-no-properties
               (window-start) (window-end nil t))))
    (claude-xwidgets--format-visible-text text (buffer-file-name))))

(defun claude-xwidgets--handle-get-visible-text (&optional file-path)
  "Handle getVisibleText MCP tool call.
Returns the text content currently visible to the user.
When FILE-PATH is given, operate on that buffer instead of current."
  (claude-xwidgets--call-in-buffer
   file-path
   (lambda ()
     (cond
      ((claude-xwidgets--pdf-buffer-p)
       (claude-xwidgets--get-pdf-visible-text))
      ((claude-xwidgets--buffer-p)
       (claude-xwidgets--get-xwidget-visible-text))
      (t
       (claude-xwidgets--get-buffer-visible-text))))))

;;; Xwidget selection polling
(defun claude-xwidgets--handle-selection-change (buf text)
  "Update selection state and notify if TEXT changed in BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((val (or text "")))
        (unless (equal val claude-xwidgets--last-selection)
          (setq claude-xwidgets--last-selection val)
          (claude-code-ide-mcp--send-notification
           "selection_changed"
           (claude-xwidgets--xwidget-selection-alist val)))))))

(defun claude-xwidgets--poll-selection ()
  "Poll xwidget selection and send notification if changed.
Uses async `xwidget-webkit-execute-script' with a callback so the
event loop is never blocked.  Returns immediately when the current
buffer is not an xwidget buffer."
  (ignore-errors
    (when-let* ((session (claude-xwidgets--session)))
      (let ((buf (current-buffer)))
        (xwidget-webkit-execute-script
         session
         claude-xwidgets--selected-text-js
         (lambda (text)
           (ignore-errors
             (claude-xwidgets--handle-selection-change buf text))))))))

(defun claude-xwidgets--start-poll-timer ()
  "Start the xwidget selection polling timer."
  (claude-xwidgets--stop-poll-timer)
  (setq claude-xwidgets--poll-timer
        (run-with-timer claude-xwidgets-mcp-poll-interval
                        claude-xwidgets-mcp-poll-interval
                        #'claude-xwidgets--poll-selection)))

(defun claude-xwidgets--stop-poll-timer ()
  "Stop the xwidget selection polling timer."
  (when claude-xwidgets--poll-timer
    (cancel-timer claude-xwidgets--poll-timer)
    (setq claude-xwidgets--poll-timer nil)))

;;; Workspace symbol search

(defun claude-extra--lsp-kind-to-string (kind)
  "Convert LSP SymbolKind integer KIND to a human-readable string."
  (if (boundp 'eglot--symbol-kind-names)
      (or (alist-get kind eglot--symbol-kind-names) "Unknown")
    "Unknown"))

(defun claude-extra--find-eglot-server (project-root)
  "Find an active Eglot server for PROJECT-ROOT.
Searches project buffers via Projectile when available,
otherwise falls back to iterating `buffer-list'."
  (when (fboundp 'eglot-current-server)
    (let ((bufs (if (fboundp 'projectile-project-buffers)
                    (ignore-errors (projectile-project-buffers project-root))
                  (buffer-list))))
      (cl-some (lambda (buf)
                 (when-let* ((file (buffer-file-name buf)))
                   (when (file-in-directory-p file project-root)
                     (with-current-buffer buf (eglot-current-server)))))
               bufs))))

(defun claude-extra--make-symbol-result (name kind uri start-line start-char end-line end-char &optional container)
  "Build a symbol result alist with NAME, KIND, location (URI + range), and CONTAINER."
  `((name . ,name)
    (kind . ,kind)
    (location . ((uri . ,uri)
                 (range . ((start . ((line . ,start-line) (character . ,start-char)))
                           (end   . ((line . ,end-line)   (character . ,end-char)))))))
    (containerName . ,(or container ""))))

(defun claude-extra--eglot-workspace-search (query kind-filter project-root)
  "Search workspace symbols matching QUERY via Eglot in PROJECT-ROOT.
When KIND-FILTER (a string like \"Function\") is non-nil, keep only
matching kinds.  Returns a list of result alists."
  (when-let* ((server (claude-extra--find-eglot-server project-root)))
    (let* ((raw (jsonrpc-request server :workspace/symbol `(:query ,query)))
           (results (append raw nil))
           (filtered (if (and kind-filter (not (string-empty-p kind-filter)))
                         (seq-filter
                          (lambda (r)
                            (string-equal
                             (claude-extra--lsp-kind-to-string (plist-get r :kind))
                             kind-filter))
                          results)
                       results)))
      (seq-map
       (lambda (res)
         (let* ((kind  (plist-get res :kind))
                (loc   (plist-get res :location))
                (range (plist-get loc :range))
                (start (plist-get range :start))
                (end   (plist-get range :end)))
           (claude-extra--make-symbol-result
            (plist-get res :name)
            (claude-extra--lsp-kind-to-string kind)
            (plist-get loc :uri)
            (plist-get start :line)
            (plist-get start :character)
            (plist-get end :line)
            (plist-get end :character)
            (plist-get res :containerName))))
       filtered))))

(defun claude-extra--rg-parse-match (data project-root)
  "Extract fields from a ripgrep match DATA alist and return a symbol result.
PROJECT-ROOT is used to expand relative paths into file URIs."
  (let* ((path (alist-get 'text (alist-get 'path data)))
         (line-num (alist-get 'line_number data))
         (text (alist-get 'text (alist-get 'lines data)))
         (submatches (alist-get 'submatches data))
         (has-sub (and submatches (> (length submatches) 0)))
         (start-col (if has-sub (alist-get 'start (aref submatches 0)) 0))
         (end-col   (if has-sub (alist-get 'end   (aref submatches 0)) 0)))
    (claude-extra--make-symbol-result
     (string-trim (or text ""))
     "Text"
     (concat "file://" (expand-file-name path project-root))
     (1- line-num) start-col
     (1- line-num) end-col)))

(defun claude-extra--rg-workspace-search (query project-root)
  "Search for QUERY in PROJECT-ROOT using Ripgrep JSON output.
Returns a list of result alists."
  (let ((default-directory project-root)
        (results '()))
    (with-temp-buffer
      (call-process "rg" nil t nil
                    "--json" "--smart-case" "--max-count"
                    (number-to-string claude-extra-workspace-search-max-results)
                    "--" query ".")
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (parsed (ignore-errors
                         (json-parse-string line :object-type 'alist))))
          (when (and parsed (string-equal (alist-get 'type parsed) "match"))
            (push (claude-extra--rg-parse-match (alist-get 'data parsed) project-root)
                  results)))
        (forward-line 1)))
    (nreverse results)))

(defun claude-extra--handle-workspace-search (query &optional kind)
  "Search workspace for QUERY, optionally filtering by KIND.
Uses Eglot (LSP workspace/symbol) when an active server exists
for the current project, otherwise falls back to Ripgrep text search.
Results are limited to `claude-extra-workspace-search-max-results'."
  (let* ((project-root (or (and (fboundp 'projectile-project-root)
                                (ignore-errors (projectile-project-root)))
                           default-directory))
         (eglot-results (ignore-errors
                          (claude-extra--eglot-workspace-search
                           query kind project-root)))
         (all-results (or eglot-results
                          (ignore-errors
                            (claude-extra--rg-workspace-search
                             query project-root)))))
    (seq-take (or all-results '()) claude-extra-workspace-search-max-results)))

;;; Registration

;;;###autoload
(defun claude-extra-mcp-tools-setup ()
  "Register extra MCP tools for claude-code-ide."
  (interactive)
  (advice-add 'claude-code-ide-mcp-handle-get-current-selection
              :around #'claude-xwidgets--get-current-selection-around)

  ;; Register get-visible-text tool
  (claude-code-ide-make-tool
   :function #'claude-xwidgets--handle-get-visible-text
   :name "claude-code-ide-mcp-get-visible-text"
   :description "Get user visible buffer text"
   :args '((:name "file_path"
            :type string
            :description "Current working file path."
            :optional t)))

  ;; Register get-selection tool
  (claude-code-ide-make-tool
   :function #'claude-xwidgets--handle-get-selection-text
   :name "claude-code-ide-mcp-get-selection-text"
   :description "Get selected text by user"
   :args '((:name "file_path"
            :type string
            :description "Current working file path."
            :optional t)))
  ;; Register workspace-symbols tool
  (claude-code-ide-make-tool
   :function #'claude-extra--handle-workspace-search
   :name "claude-code-ide-mcp-workspace-symbols"
   :description "Search for files, functions, variables, classes, etc., by name pattern across your project. This helps you discover code elements when you know part of the name."
   :args '((:name "query"
            :type string
            :description "The search query.")
           (:name "kind"
            :type string
            :description "Symbol kind filter (e.g. Function, Class, Variable). Only effective with LSP."
            :optional t)))

  (claude-xwidgets--start-poll-timer))

(provide 'claude-extra-mcp-tools)
;;; claude-extra-mcp-tools.el ends here
