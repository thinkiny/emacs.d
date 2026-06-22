;; -*- lexical-binding: t; -*-

(require 'xwidget)

;;; Customization

(defgroup xwidget-webkit '() "xwidget webkit" :group 'tools)
(setq xwidget-webkit-buffer-name-format "*WEB: %T*")
(setq xwidget-webkit-proxy (local-proxy-http-url))

;;; URL & Session Helpers

(defun xwidget-webkit-get-current-url()
  (if (derived-mode-p 'xwidget-webkit-mode)
      (when-let* ((session (xwidget-webkit-current-session))
                  (url (xwidget-webkit-uri session)))
        url)))

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
                 (regexp-quote string))
         #'xwidget-webkit-search-cb)
        (point-min)))))

;;; Buffer Quit

(defun xwidget-webkit-quit ()
  "Ask to kill the buffer; if no, quit the window."
  (interactive)
  (if (y-or-n-p (format "Close %s? " (buffer-name)))
      (let ((kill-buffer-query-functions nil))
        (set-buffer-modified-p nil)
        (kill-buffer (current-buffer)))
    (quit-window)))

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

(defun xwidget-webkit-open-in-chrome ()
  "Open current xwidget URL in Chrome."
  (interactive)
  (when-let* ((url (xwidget-webkit-uri (xwidget-webkit-current-session))))
    (xwidget-webkit-open-url-in-chrome url)))

(defun xwidget-webkit-open-in-chrome-background ()
  "Open current xwidget URL in Chrome."
  (interactive)
  (when-let* ((url (xwidget-webkit-uri (xwidget-webkit-current-session))))
    (xwidget-webkit-open-url-in-chrome url t)))

;;; Keymap

(with-eval-after-load 'xwidget
  (easy-menu-define nil xwidget-webkit-mode-map "Xwidget WebKit menu."
    (list "Xwidget WebKit" :visible nil))
  (unbind-key (kbd "-") xwidget-webkit-mode-map)
  (unbind-key (kbd "+") xwidget-webkit-mode-map)
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
  (define-key xwidget-webkit-mode-map (kbd "C-,") #'xwidget-translate-range))

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
  (setq-local isearch-search-fun-function #'xwidget-webkit-search-fun-function)
  (setq-local isearch-lazy-highlight nil)
  (setq-local header-line-format nil))

(add-hook 'xwidget-webkit-mode-hook #'my-xwidget-webkit-mode-hook)

(global-set-key (kbd "C-x / /") #'xwidget-webkit-browse-open-url)

;;; Caret.js

(require 'caret-xwidget)

;;; Window Sizing

(defun xwidget-webkit-auto-adjust-size-derived (window)
  "Adjust xwidget size to fit WINDOW for any `xwidget-webkit-mode' derivative."
  (with-current-buffer (window-buffer window)
    (when (derived-mode-p 'xwidget-webkit-mode)
      (when-let* ((xwidget (xwidget-webkit-current-session)))
        (xwidget-webkit-adjust-size-to-window xwidget window)))))

(defun xwidget-webkit-adjust-size-derived-in-frame (frame)
  "Adjust xwidget sizes for all xwidget-webkit derived-mode windows in FRAME."
  (walk-windows #'xwidget-webkit-auto-adjust-size-derived 'no-minibuf frame))

(add-to-list 'window-size-change-functions
             #'xwidget-webkit-adjust-size-derived-in-frame)

(setq window-size-change-functions
      (remove 'xwidget-webkit-adjust-size-in-frame
              window-size-change-functions))

;;; Transparent Background

(defun xwidget-webkit-inject-transparent-bg ()
  "Inject CSS to make page background transparent."
  (interactive)
  (when (not (derived-mode-p 'nov-xwidget-webkit-mode 'pdf-xwidget-mode))
    (xwidget-webkit-eval-script
     "var s = document.createElement('style');
s.textContent = 'html,body,:not(caret-cursor){background:transparent!important} ::selection{background:auto!important;}';
document.head.appendChild(s);")))

(defun xwidget-webkit--transparent-bg-callback-advice (orig-fn xwidget event-type)
  "Inject transparent background CSS on page load."
  (funcall orig-fn xwidget event-type)
  (when (and (eq event-type 'load-changed)
             (string-equal (nth 3 last-input-event) "load-finished"))
    (xwidget-webkit-inject-transparent-bg)))

(advice-add 'xwidget-webkit-callback :around
            #'xwidget-webkit--transparent-bg-callback-advice)

(provide 'init-xwidget-webkit)
