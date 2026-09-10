;; -*- lexical-binding: t; -*-

(require 'xwidget)

;;; Customization

(defgroup xwidget-webkit '() "xwidget webkit" :group 'tools)
(setq xwidget-webkit-buffer-name-format "*WEB: %T*")
(defun local-proxy-endpoint--sync-xwidget()
  "Mirror `local-proxy-endpoint' into `xwidget-webkit-proxy'."
  (setq xwidget-webkit-proxy (local-proxy-http-url)))

(add-hook 'local-proxy-endpoint-change-functions
          #'local-proxy-endpoint--sync-xwidget)

(local-proxy-endpoint--sync-xwidget)

;;; URL & Session Helpers
(defun xwidget-webkit-get-current-url ()
  "Return the URL of the current xwidget session, or nil if none exists."
  (and (derived-mode-p 'xwidget-webkit-mode)
       (if-let* ((session (xwidget-webkit-current-session)))
           (xwidget-webkit-uri session))))

(defun xwidget-webkit-get-file-url ()
  (if (s-ends-with? ".html" (buffer-file-name))
      (concat "file://" (buffer-file-name))))

(defun xwidget-webkit-create-or-goto-url (url)
  "Open or reload URL in an xwidget session."
  (if (derived-mode-p 'xwidget-webkit-mode)
      (xwidget-webkit-goto-uri (xwidget-at (point-min)) url)
    (xwidget-webkit-new-session url)))

(defun xwidget-webkit-set-local-file-mapping(local-file)
  (when-let* ((session (xwidget-at (point-min)))
              (buffer (xwidget-buffer session))
              (local-file (or local-file
                              (file-name-concat (or (projectile-project-root) "~/org") "*web*"))))
    (with-current-buffer buffer
      (map-buffer-to-local-file local-file))))

(defun xwidget-webkit-browse-open-url(url &optional new-session local-file)
  "Ask xwidget-webkit to browse URL.
When NEW-SESSION is non-nil, open URL in a new xwidget session instead of
reusing an existing one."
  (interactive (progn
                 (list
                  (read-string "open URL: "
                               (or (xwidget-webkit-get-current-url)
                                   (xwidget-webkit-get-file-url))))))
  (when (stringp url)
    (unless (string-match "\\`[A-Za-z]+:" url)
      (setq url (concat "https://" url)))
    (if (s-starts-with? "https://arxiv.org/pdf" url)
        (funcall 'pdf-xwidget-open url)
      (if new-session
          (xwidget-webkit-new-session url)
        (xwidget-webkit-create-or-goto-url url)))
    (xwidget-webkit-set-local-file-mapping local-file)))

(setq browse-url-browser-function 'xwidget-webkit-browse-open-url)

;;; Follow Link

(use-package xwwp-follow-link-ivy
  :custom
  (xwwp-follow-link-completion-system 'ivy)
  :bind (:map xwidget-webkit-mode-map
              ("C-c l" . xwwp-follow-link)))

;;; In-Page Search

(defvar-local xwidget-webkit-isearch-last-length 0)
(defvar-local xwidget-webkit-searching nil)

(defconst xwidget-webkit-search-js "
var xwSearchForward = %s;
var xwSearchRepeat = %s;
var xwSearchString = '%s';
if (window.getSelection() && !window.getSelection().isCollapsed) {
if (xwSearchRepeat) {
if (xwSearchForward)
window.getSelection().collapseToEnd();
else
window.getSelection().collapseToStart();
} else {
if (xwSearchForward)
window.getSelection().collapseToStart();
else {
var sel = window.getSelection();
window.getSelection().collapse(sel.focusNode, sel.focusOffset + 1);
}
}
}
window.find(xwSearchString, false, !xwSearchForward, true, false, true);
 ")

(defun xwidget-webkit-search-cb(end)
  (setq xwidget-webkit-searching nil))

(defun xwidget-webkit-search-fun-function ()
  "Return the function which perform the search in xwidget webkit."
  (lambda (string &optional bound noerror count)
    (unless xwidget-webkit-searching
      (setq xwidget-webkit-searching t)
      (or bound noerror count) ;; Kill warns
      (let ((current-length (length string))
            search-forward
            search-repeat)
        (if (eq isearch-forward nil)
            (setq search-forward "false")
          (setq search-forward "true"))
        (if (eq current-length xwidget-webkit-isearch-last-length)
            (setq search-repeat "true")
          (setq search-repeat "false"))
        (setq xwidget-webkit-isearch-last-length current-length)
        (xwidget-webkit-execute-script
         (xwidget-webkit-current-session)
         (format xwidget-webkit-search-js
                 search-forward
                 search-repeat
                 string)
         #'xwidget-webkit-search-cb)
        (let ((target-point (if isearch-forward (point-min) (point-max))))
          (goto-char target-point)
          (set-match-data (list target-point target-point))
          target-point)))))

;;; Buffer Quit

(defun xwidget-webkit-quit ()
  "Ask to kill the buffer; if no, stay in the buffer."
  (interactive)
  (when (y-or-n-p (format "Close %s? " (buffer-name)))
    (let ((kill-buffer-query-functions nil))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))

;;; Translate

(defvar xwidget-translate-timer nil)
(defun xwidget-translate-range()
  (interactive)
  (if xwidget-translate-timer
      (cancel-timer xwidget-translate-timer))
  (setq xwidget-translate-timer
        (run-with-timer 0.1 nil
                        (lambda ()
                          (setq xwidget-translate-timer nil)
                          (xwidget-webkit-get-selection
                           (lambda (text)
                             (translate-brief text)))))))

;;; Chrome Integration

(defun xwidget-webkit-open-url-in-chrome (url &optional background)
  "Open URL in Chrome."
  (let ((frame (selected-frame)))
    (browse-url-chrome url)
    (when background
      (run-with-timer 0.3 nil
                      (lambda ()
                        (select-frame-set-input-focus frame))))))

(defun xwidget-webkit-link-at-caret ()
  "Return the URL of the link under the caret, or nil."
  (when-let* ((result (xwidget-webkit-execute-script-sync
                       "window.__caretEmacs ? window.__caretEmacs.linkAtCaret() : ''")))
    (let ((url (string-trim result "\"" "\"")))
      (unless (string-empty-p url) url))))

(defun xwidget-webkit-open-in-chrome (&optional background)
  "Open link at caret in Chrome; fall back to current session URL.
With non-nil BACKGROUND (or prefix arg), refocus Emacs afterward."
  (interactive "P")
  (when-let* ((url (or (xwidget-webkit-link-at-caret)
                       (xwidget-webkit-uri (xwidget-webkit-current-session)))))
    (xwidget-webkit-open-url-in-chrome url background)))

(defun xwidget-webkit-open-in-chrome-background ()
  "Open link at caret (or session URL) in Chrome without leaving Emacs."
  (interactive)
  (xwidget-webkit-open-in-chrome t))


;;; HTML file opening

(defun html-xwidget-view (&optional file)
  "Open FILE as HTML in an xwidget-webkit session.
When FILE is nil, uses `buffer-file-name' (for `auto-mode-alist' use).
Creates a new xwidget session and kills the original file-visiting buffer."
  (interactive "fHTML file: ")
  (let ((html-file (expand-file-name (or file buffer-file-name)))
        (init-buf (current-buffer)))
    (xwidget-webkit-new-session (concat "file://" html-file))
    (when-let* ((session (xwidget-webkit-last-session))
                (buffer (xwidget-buffer session)))
      (with-current-buffer buffer
        (setq-local buffer-file-name html-file)
        (setq-local buffer-read-only t)
        (set-buffer-modified-p nil)
        (setq-local default-directory (file-name-directory html-file))))
    (kill-buffer init-buf)))

(add-auto-mode 'html-xwidget-view "\\.html?\\'")

;;; View Source

(defun xwidget-webkit--decode-js-string (result)
  "Decode the JSON-quoted string RESULT returned by script execution."
  (if (and (stringp result) (string-prefix-p "\"" result))
      (condition-case nil (json-read-from-string result) (error result))
    result))

(defun xwidget-webkit--show-source-buffer (source name)
  "Show SOURCE html in a `web-mode' buffer named *source: NAME*."
  (let ((source-buffer (get-buffer-create (format "*source: %s*" name))))
    (with-current-buffer source-buffer
      (erase-buffer)
      (insert source)
      (web-mode)
      (goto-char (point-min)))
    (switch-to-buffer source-buffer)))

(defconst xwidget-webkit--html-file-regexp "\\.x?html?\\'"
  "Files whose source `xwidget-webkit-view-source' opens directly.")

(defun xwidget-webkit-view-source ()
  "View the source of the page in the current xwidget-webkit session.
  For a local HTML page, open its file in `web-mode'; otherwise fetch the
  rendered HTML into a source buffer."
  (interactive)
  (if-let* ((file buffer-file-name)
            ((string-match-p xwidget-webkit--html-file-regexp file))
            ((file-exists-p file)))
      ;; html-xwidget-view stamps buffer-file-name without visiting FILE, so
      ;; hide it from get-file-buffer, which would hand back this buffer.
      (let ((auto-mode-alist (cons (cons xwidget-webkit--html-file-regexp #'web-mode)
                                   auto-mode-alist))
            (buffer-file-name nil))
        (switch-to-buffer (find-file-noselect file)))
    (when-let* ((result (xwidget-webkit-execute-script-sync
                         "(document.documentElement || document).outerHTML" 5))
                (source (xwidget-webkit--decode-js-string result))
                (session (xwidget-webkit-current-session)))
      (xwidget-webkit--show-source-buffer
       source (or (xwidget-webkit-title session) "xwidget")))))

;;; Caret.js
(require 'caret-xwidget)

;;; Keymap

(with-eval-after-load 'xwidget
  (easy-menu-define nil xwidget-webkit-mode-map "Xwidget WebKit menu."
    (list "Xwidget WebKit" :visible nil))
  (unbind-key (kbd "-") 'xwidget-webkit-mode-map)
  (unbind-key (kbd "+") 'xwidget-webkit-mode-map)
  (define-key xwidget-webkit-mode-map (kbd "g") #'xwidget-webkit-browse-open-url)
  (define-key xwidget-webkit-mode-map (kbd "F") 'xwidget-webkit-forward)
  (define-key xwidget-webkit-mode-map (kbd "B") 'xwidget-webkit-back)
  (define-key xwidget-webkit-mode-map (kbd "M-c") 'xwidget-webkit-copy-selection-as-kill)
  (define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
  (define-key xwidget-webkit-mode-map (kbd "O") 'xwidget-webkit-open-in-chrome)
  (define-key xwidget-webkit-mode-map (kbd "o") 'xwidget-webkit-open-in-chrome-background)
  ;;(define-key xwidget-webkit-mode-map (kbd "<drag-mouse-1>") #'xwidget-translate-range)
  (define-key xwidget-webkit-mode-map (kbd "C-s") #'isearch-forward)
  (define-key xwidget-webkit-mode-map (kbd "C-r") #'isearch-backward)
  (define-key xwidget-webkit-mode-map (kbd "q") #'xwidget-webkit-quit)
  (define-key xwidget-webkit-mode-map (kbd "V") #'xwidget-webkit-view-source)
  (define-key xwidget-webkit-mode-map (kbd "C-,") #'xwidget-translate-range)
  (define-key xwidget-webkit-mode-map (kbd "C-x 2") 'split-window-below-recent)
  (define-key xwidget-webkit-mode-map (kbd "C-x 3") 'split-window-right-recent))

;;; JS Execution

(defun xwidget-webkit-eval-scripts(scripts)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   (format "(function() { %s })();" (string-join scripts "\n"))))

(defun xwidget-webkit-eval-script(script)
  (xwidget-webkit-eval-scripts (list script)))

(defun xwidget-webkit-execute-script-sync (js &optional timeout)
  "Execute JS in current xwidget synchronously, returning the result.
Buffers user keystrokes during execution and replays them afterward.
TIMEOUT defaults to 2 seconds."
  (let ((done nil)
        (result nil)
        (timeout (or timeout 2))
        (start (float-time))
        (deferred-events nil))
    (when-let* ((xw (xwidget-webkit-current-session)))
      ;; Start the async WebKit script execution
      (xwidget-webkit-execute-script
       xw js
       (lambda (response)
         (setq result response
               done t)))

      ;; Stay in the loop until JS finishes or timeout is reached
      (while (and (not done)
                  (< (- (float-time) start) timeout))
        (let ((ev (read-event nil nil 0.001)))
          (when ev
            ;; Buffer the event locally so it doesn't cause an infinite busy-loop
            (push ev deferred-events))))

      ;; Execution finished: Replay the buffered keys back into Emacs
      (when deferred-events
        (setq unread-command-events
              (nconc (nreverse deferred-events) unread-command-events)))
      result)))

;;; Mode Hook

(defun my-xwidget-webkit-mode-hook()
  (eldoc-mode -1)
  (setq-local isearch-search-fun-function 'xwidget-webkit-search-fun-function)
  (setq-local isearch-lazy-highlight nil)
  (setq-local isearch-wrap-function 'ignore)
  (setq-local header-line-format nil))

(add-hook 'xwidget-webkit-mode-hook #'my-xwidget-webkit-mode-hook)

(global-set-key (kbd "C-x / /") #'xwidget-webkit-browse-open-url)


;;; Window Sizing

(defun xwidget-webkit-auto-adjust-size-derived (frame)
  "Adjust xwidget size to fit FRAME for any `xwidget-webkit-mode' derivative."
  (dolist (win (window-list frame))
    (with-selected-window win
      (when (derived-mode-p 'xwidget-webkit-mode)
        (when-let* ((xwidget (xwidget-at (point-min))))
          (xwidget-webkit-adjust-size-to-window xwidget win))))))

(setq window-size-change-functions
      (remove 'xwidget-webkit-adjust-size-in-frame
              window-size-change-functions))

(add-hook 'window-size-change-functions #'xwidget-webkit-auto-adjust-size-derived)

;;; Transparent Background

(defun xwidget-webkit-inject-transparent-bg ()
  "Inject CSS to make page background transparent and theme the caret color."
  (interactive)
  (unless (derived-mode-p 'nov-xwidget-webkit-mode 'pdf-xwidget-mode)
    (let* ((caret-color (current-theme-cursor-hex))
           (caret-rule (if caret-color
                           (format ":root{--caret-color:%s}" caret-color)
                         "")))
      (xwidget-webkit-eval-script
       (format "var s = document.createElement('style');
s.textContent = '%shtml, body > *:not(a, input, select, button, caret-cursor) {
  background-color: transparent !important;
}';
(document.head || document.documentElement).appendChild(s);"
               caret-rule)))))

(defun xwidget-webkit--transparent-bg-callback-advice (xwidget event-type)
  "Inject transparent background CSS on `load-committed'."
  (when (and (eq event-type 'load-changed)
             (string-equal (nth 3 last-input-event) "load-committed"))
    (xwidget-webkit-inject-transparent-bg)))

(advice-add 'xwidget-webkit-callback :after
            #'xwidget-webkit--transparent-bg-callback-advice)

(provide 'init-xwidget-webkit)
