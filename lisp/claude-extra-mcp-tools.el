;;; claude-extra-mcp-tools.el --- MCP tools for xwidget buffers  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'xwidget)
(require 'project-search)
(require 'claude-code-ide-mcp-server)


(declare-function claude-code-ide-mcp--send-notification "claude-code-ide-mcp")

(defgroup claude-extra-mcp-tools nil
  "Extra MCP tools for Claude Code IDE."
  :group 'tools
  :prefix "claude-extra-")

(defcustom claude-xwidgets-mcp-timeout-seconds 0.8
  "Timeout in seconds while waiting for xwidget JavaScript callback results."
  :type 'number
  :group 'claude-extra-mcp-tools)

(defcustom claude-extra-mcp-treesit-max-chars 4096
  "Maximum character size for `claude-code-ide-mcp-treesit-info' responses."
  :type 'integer
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
})();")

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
})();")


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

;;; Helpers

(defun claude-xwidgets--session ()
  "Return the current xwidget WebKit session or nil."
  (xwidget-at (point-min)))

(defun claude-xwidgets--buffer-p ()
  "Return non-nil when current buffer is backed by an xwidget session."
  (claude-xwidgets--session))

(defun claude-xwidgets--pdf-buffer-p ()
  "Return non-nil when current buffer is a PDF xwidget buffer."
  (string-prefix-p "*PDF:" (buffer-name)))

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
  "Return selected text from the current xwidget page."
  (or (ignore-errors (claude-xwidgets--eval-sync claude-xwidgets--selected-text-js))
      ""))

(defun claude-xwidgets--selection-alist (&optional text)
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

(defun claude-mcp--current-selection-advice (orig-fn arguments)
  "Around advice: handle xwidget buffers, delegate everything else.
ORIG-FN is the original `getCurrentSelection' handler.
ARGUMENTS are the MCP tool arguments (unused)."
  (if (claude-xwidgets--buffer-p)
      (claude-xwidgets--selection-alist)
    ;; Not an xwidget buffer: call the original handler
    (funcall orig-fn arguments)))

;;; getVisibleText

(defun claude-xwidgets--pdf-visible-text ()
  "Extract text from the current PDF page via the rendered text layer."
  (or (ignore-errors (claude-xwidgets--eval-sync claude-xwidgets--pdf-visible-text-js)) ""))

(defun claude-xwidgets--viewport-text ()
  "Extract visible text from the current xwidget viewport."
  (or (ignore-errors (claude-xwidgets--eval-sync
                      claude-xwidgets--viewport-visible-text-js))
      ""))

(defun claude-mcp--window-text ()
  "Extract the text currently visible in the Emacs window."
  (buffer-substring-no-properties (window-start) (window-end nil t)))

(defun claude-mcp--visible-text ()
  "Get the text content currently visible to the user."
  (cond
   ((claude-xwidgets--pdf-buffer-p)
    (claude-xwidgets--pdf-visible-text))
   ((claude-xwidgets--buffer-p)
    (claude-xwidgets--viewport-text))
   (t
    (let ((win (get-buffer-window (current-buffer) t)))
      (if win
          (with-selected-window win
            (claude-mcp--window-text))
        "")))))

(defun claude-mcp--selection-text ()
  "Return selected text if available, handling all buffer types.
Returns nil or empty string if no text is selected."
  (cond
   ((claude-xwidgets--buffer-p)
    ;; For xwidget buffers, use existing selection mechanism
    (claude-xwidgets--selected-text))
   (t
    ;; For regular buffers, check if region is active and get selected text
    (when (and (region-active-p) (use-region-p))
      (buffer-substring-no-properties (region-beginning) (region-end))))))

(defun claude-mcp--read-screen ()
  "Handle getVisibleText MCP tool call.
Returns an alist with text and location information."
  (claude-code-ide-mcp-server-with-session-context nil
    (let* ((selected-text (claude-mcp--selection-text))
           (text (if (and selected-text (not (string-empty-p selected-text)))
                     selected-text
                   (claude-mcp--visible-text)))
           (file-path (buffer-file-name))
           (uri (cond
                 (file-path (concat "file://" (expand-file-name file-path)))
                 ((claude-xwidgets--session) (xwidget-webkit-uri (claude-xwidgets--session)))
                 (t ""))))
      (if (claude-xwidgets--buffer-p)
          `((text . ,text)
            (location . ((uri . ,uri))))
        (let ((line (line-number-at-pos (window-start))))
          `((text . ,text)
            (location . ((uri . ,uri)
                         (line . ,line)))))))))

;;; Xwidget selection polling
(defun claude-xwidgets--on-selection-change (buf text)
  "Send notification when selection changes in BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (claude-code-ide-mcp--send-notification
       "selection_changed"
       (claude-xwidgets--selection-alist text)))))

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
             (claude-xwidgets--on-selection-change buf text))))))))

;;; Project search (delegates to project-search.el)
(defun claude-mcp--project-search (query)
  "Search project for QUERY."
  (claude-code-ide-mcp-server-with-session-context nil
    (project-search-sync-query query)))

(defun claude-mcp--selection-poll-advice (orig-fn arg)
  "Around advice for `claude-code-ide-mcp--track-selection'.
In xwidget buffers, poll the selection directly since the original
function requires `buffer-file-name'.  ORIG-FN is the original function."
  (if (claude-xwidgets--buffer-p)
      (claude-xwidgets--poll-selection)
    (funcall orig-fn arg)))

;;; treesit-info size guard

(defun claude-mcp--truncate-string (text max-chars)
  "Truncate TEXT to MAX-CHARS."
  (let ((limit (max 0 max-chars)))
    (if (<= (length text) limit)
        text
      (substring text 0 limit))))

(defun claude-mcp--treesit-truncation-notice (original-chars cap-chars)
  "Return a short truncation notice for tree-sitter output."
  (format "\n\n[truncated treesit output: %d chars, capped at %d chars]"
          original-chars cap-chars))

(defun claude-mcp--treesit-size-advice (orig-fn &rest args)
  "Limit `claude-code-ide-mcp-treesit-info' output size."
  (let ((result (apply orig-fn args)))
    (if (not (stringp result))
        result
      (let* ((cap-chars (max 0 claude-extra-mcp-treesit-max-chars))
             (result-chars (length result)))
        (if (<= result-chars cap-chars)
            result
          (let* ((notice (claude-mcp--treesit-truncation-notice
                          result-chars cap-chars))
                 (notice-chars (length notice)))
            (if (>= notice-chars cap-chars)
                (claude-mcp--truncate-string notice cap-chars)
              (let* ((body-budget (- cap-chars notice-chars))
                     (body (claude-mcp--truncate-string
                            result body-budget)))
                (concat body notice)))))))))

;;; Registration

;;;###autoload
(defun claude-extra-mcp-tools-setup ()
  "Register extra MCP tools for claude-code-ide."
  (advice-add 'claude-code-ide-mcp-handle-get-current-selection
              :around #'claude-mcp--current-selection-advice)
  (advice-add 'claude-code-ide-mcp--send-selection-for-project
              :around #'claude-mcp--selection-poll-advice)
  (advice-add 'claude-code-ide-mcp-treesit-info
              :around #'claude-mcp--treesit-size-advice)

  ;; Register get-visible-text tool
  (claude-code-ide-make-tool
   :function #'claude-mcp--read-screen
   :name "claude-code-ide-mcp-read-screen"
   :description "Use this tool when you lack context; It retrieves the text currently visible in the user's active window."
   :args nil)

  ;; Register project-search tool
  (claude-code-ide-make-tool
   :function #'claude-mcp--project-search
   :name "claude-code-ide-mcp-project-search"
   :description "Use this tool to search for functions, variables, classes, etc., by name pattern across the project."
   :args '((:name "pattern"
            :type string
            :description "The pattern to search for symbols.")))

  (setq claude-code-ide-mcp-server-tools
        (cl-remove-if (lambda (spec)
                        (equal (plist-get spec :name) "claude-code-ide-mcp-xref-find-apropos"))
                      claude-code-ide-mcp-server-tools))
  )

(provide 'claude-extra-mcp-tools)
;;; claude-extra-mcp-tools.el ends here
