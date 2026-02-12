;;; claude-xwidgets-mcp-tools.el --- MCP tools for xwidget buffers  -*- lexical-binding: t; -*-

;;; Code:

(require 'subr-x)
(require 'xwidget)


(declare-function claude-code-ide-mcp--send-notification "claude-code-ide-mcp")
(declare-function claude-code-ide-make-tool "claude-code-ide-mcp-server")

(defgroup claude-xwidgets-mcp-tools nil
  "Xwidget MCP tools for Claude Code IDE."
  :group 'tools
  :prefix "claude-xwidgets-")

(defcustom claude-xwidgets-mcp-timeout-seconds 0.8
  "Timeout in seconds while waiting for xwidget JavaScript callback results."
  :type 'number
  :group 'claude-xwidgets-mcp-tools)

(defcustom claude-xwidgets-mcp-poll-interval 2
  "Interval in seconds for polling xwidget selection changes.
Xwidget web views handle mouse events via GTK/WebKit, bypassing
Emacs's command loop.  A polling timer is the only way to detect
selection changes in these buffers."
  :type 'number
  :group 'claude-xwidgets-mcp-tools)

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

;;; getCurrentSelection :around advice

(defun claude-xwidgets--get-current-selection-around (orig-fn arguments)
  "Around advice: handle xwidget buffers, delegate everything else.
ORIG-FN is the original `getCurrentSelection' handler.
ARGUMENTS are the MCP tool arguments (unused)."
  (if (claude-xwidgets--buffer-p)
      ;; Xwidget buffer: get selection from JavaScript
      (let* ((text (claude-xwidgets--selected-text))
             (file-path (buffer-file-name))
             (empty (string-empty-p text)))
        `((text . ,text)
          (filePath . ,(or file-path ""))
          ,@(when file-path
              `((fileUrl . ,(concat "file://" file-path))))
          (selection . ((start . ((line . 1) (character . 1)))
                        (end . ((line . 1) (character . 1)))
                        (isEmpty . ,(if empty t :json-false))))))
    ;; Not an xwidget buffer: call the original handler
    (funcall orig-fn arguments)))

;;; getVisibleText

(defun claude-xwidgets--get-pdf-visible-text ()
  "Extract text from the current PDF page via the rendered text layer."
  (let* ((text (or (claude-xwidgets--eval-sync claude-xwidgets--pdf-visible-text-js) ""))
         (file-path (buffer-file-name)))
    (format "%s%s"
            (if file-path (format "%s\n" file-path) "")
            text)))

(defun claude-xwidgets--get-xwidget-visible-text ()
  "Extract visible text from the current xwidget viewport."
  (let* ((text (or (claude-xwidgets--eval-sync
                    claude-xwidgets--viewport-visible-text-js)
                   ""))
         (file-path (buffer-file-name))
         (url (ignore-errors
                (xwidget-webkit-uri (claude-xwidgets--session)))))
    (format "%s%s"
            (cond (file-path (format "%s\n" file-path))
                  (url (format "%s\n" url))
                  (t ""))
            text)))

(defun claude-xwidgets--get-buffer-visible-text ()
  "Extract the text currently visible in the Emacs window."
  (let* ((start (window-start))
         (end (window-end nil t))
         (text (buffer-substring-no-properties start end))
         (file-path (buffer-file-name)))
    (format "%s%s"
            (if file-path (format "%s\n" file-path) "")
            text)))

(defun test-func()
  (interactive)
  (prin1 (claude-xwidgets--handle-get-visible-text)))

(defun claude-xwidgets--handle-get-visible-text ()
  "Handle getVisibleText MCP tool call.
Returns the text content currently visible to the user."
  (cond
   ((claude-xwidgets--pdf-buffer-p)
    (claude-xwidgets--get-pdf-visible-text))
   ((claude-xwidgets--buffer-p)
    (claude-xwidgets--get-xwidget-visible-text))
   (t
    (claude-xwidgets--get-buffer-visible-text))))

;;; Xwidget selection polling
(defun claude-xwidgets--poll-selection ()
  "Poll xwidget selection and send notification if changed.
Uses async `xwidget-webkit-execute-script' with a callback so the
event loop is never blocked.  Returns immediately when the current
buffer is not an xwidget buffer."
  (condition-case nil
      (when-let* ((session (claude-xwidgets--session)))
        (let ((buf (current-buffer)))
          (xwidget-webkit-execute-script
           session
           claude-xwidgets--selected-text-js
           (lambda (text)
             (condition-case nil
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (let* ((text (or text ""))
                            (file-path (buffer-file-name))
                            (empty (string-empty-p text)))
                       (unless (equal text claude-xwidgets--last-selection)
                         (setq claude-xwidgets--last-selection text)
                         (claude-code-ide-mcp--send-notification
                          "selection_changed"
                          `((text . ,text)
                            (filePath . ,(or file-path ""))
                            ,@(when file-path
                                `((fileUrl . ,(concat "file://" file-path))))
                            (selection
                             . ((start . ((line . 1) (character . 1)))
                                (end . ((line . 1) (character . 1)))
                                (isEmpty . ,(if empty t :json-false))))))))))
               (error nil))))))
    ;; Swallow errors so the timer keeps running.
    (error nil)))

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

;;; Registration

;;;###autoload
(defun claude-xwidgets-mcp-tools-setup ()
  "Register xwidget MCP tools for claude-code-ide."
  (interactive)
  (advice-add 'claude-code-ide-mcp-handle-get-current-selection
              :around #'claude-xwidgets--get-current-selection-around)
  ;; Register getVisibleText tool
  (claude-code-ide-make-tool
   :function #'claude-xwidgets--handle-get-visible-text
   :name "claude-code-ide-mcp-get-visible-text"
   :description "Get the text content currently user looking at"
   :args nil)
  (claude-xwidgets--start-poll-timer))

(provide 'claude-xwidgets-mcp-tools)
;;; claude-xwidgets-mcp-tools.el ends here
