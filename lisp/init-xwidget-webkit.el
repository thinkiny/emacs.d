(defgroup xwidget-webkit '() "xwidget webkit" :group 'tools)
(defcustom xwidget-webkit-urls '()
  "Specify xwidgets webkit URLS."
  :type '(alist :key-type string :value-type string)
  :group 'xwidget-webkit)

(require 'xwidget)
(with-eval-after-load 'xwidget
  (defun xwidget-webkit--update-progress-timer-function (_)))

(defvar xwidget-webkit-browse-session nil)

(defun xwidget-webkit-get-browse-buffer()
  (let ((buffer (and xwidget-webkit-browse-session (xwidget-buffer xwidget-webkit-browse-session))))
    (and (buffer-live-p buffer) buffer)))

(defun xwidget-webkit-browse-open-url (url &optional rest)
  "Ask xwidget-webkit to browse URL.
NEW-SESSION specifies whether to create a new xwidget-webkit session.
Interactively, URL defaults to the string looking like a url around point."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: ")))
  (or (featurep 'xwidget-internal)
      (user-error "Your Emacs was not compiled with xwidgets support"))
  (when (stringp url)
    ;; If it's a "naked url", just try adding https: to it.
    (unless (string-match "\\`[A-Za-z]+:" url)
      (setq url (concat "https://" url)))
    (let ((buffer (xwidget-webkit-get-browse-buffer)))
      (if buffer
          (progn
            (xwidget-webkit-goto-uri xwidget-webkit-browse-session url)
            (switch-to-buffer buffer))
        (xwidget-webkit-new-session url)
        (setq xwidget-webkit-browse-session (xwidget-webkit-last-session))))))

(setq browse-url-browser-function 'xwidget-webkit-browse-open-url)

(defun advice/after-xwidget-plus-webkit-browse-url (&rest _)
  "Advice to add switch to window when calling `xwidget-webkit-browse-url'."
  (display-buffer (xwidget-buffer (xwidget-webkit-current-session))))
(advice-add #'xwidget-webkit-browse-url :after #'advice/after-xwidget-plus-webkit-browse-url)

(defun xwidget-webkit-browse (url)
  (interactive "sURL: ")
  (if (cl-search "://" url)
      (xwidget-webkit-browse-url url)
    (xwidget-webkit-browse-url (concat "http://" url))))

;; xwwp-follow-link
(use-package xwwp-follow-link-ivy
  :custom
  (xwwp-follow-link-completion-system 'ivy)
  :bind (:map xwidget-webkit-mode-map
              ("C-c l" . xwwp-follow-link)))


(global-set-key (kbd "C-x /") #'xwidget-webkit-browse)

;;; Search text in page
;; Initialize last search text length variable when isearch starts
(defvar xwidget-webkit-isearch-last-length 0)
(add-hook 'isearch-mode-hook
          (lambda ()
            (setq xwidget-webkit-isearch-last-length 0)))

;; This is minimal. Regex and incremental search will be nice
(defvar xwidget-webkit-search-js "
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

(defun xwidget-webkit-search-fun-function ()
  "Return the function which perform the search in xwidget webkit."
  (lambda (string &optional bound noerror count)
    (or bound noerror count) ;; Kill warns
    (let ((current-length (length string))
          search-forward
          search-repeat)
      ;; Forward or backward
      (if (eq isearch-forward nil)
          (setq search-forward "false")
        (setq search-forward "true"))
      ;; Repeat if search string length not changed
      (if (eq current-length xwidget-webkit-isearch-last-length)
          (setq search-repeat "true")
        (setq search-repeat "false"))
      (setq xwidget-webkit-isearch-last-length current-length)
      (xwidget-webkit-execute-script
       (xwidget-webkit-current-session)
       (format xwidget-webkit-search-js
               search-forward
               search-repeat
               (regexp-quote string)))
      (point-min))))


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
                             (bing-dict-brief text)))))))

(add-hook 'xwidget-webkit-mode-hook
          (lambda ()
            (setq-local auto-translate-mouse-selection t)
            ;;(setq-local isearch-search-fun-function #'xwidget-webkit-search-fun-function)
            (setq-local isearch-lazy-highlight nil)
            (setq-local header-line-format nil)
            (setq-local left-fringe-width 0)
            (define-key xwidget-webkit-mode-map (kbd "n") 'xwidget-webkit-scroll-up-line)
            (define-key xwidget-webkit-mode-map (kbd "p") 'xwidget-webkit-scroll-down-line)
            (define-key xwidget-webkit-mode-map (kbd "j") 'xwidget-webkit-scroll-up-line)
            (define-key xwidget-webkit-mode-map (kbd "k") 'xwidget-webkit-scroll-down-line)
            (define-key xwidget-webkit-mode-map (kbd "M-v") 'xwidget-webkit-scroll-down)
            (define-key xwidget-webkit-mode-map (kbd "M-c") 'xwidget-webkit-copy-selection-as-kill)
            (define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
            (define-key xwidget-webkit-mode-map (kbd "C-v") 'xwidget-webkit-scroll-up)
            (define-key xwidget-webkit-mode-map (kbd "<drag-mouse-1>") #'xwidget-translate-range)
            ;;(local-set-key (kbd "C-s") #'isearch-forward)
            (define-key xwidget-webkit-mode-map (kbd "<double-mouse-1>") #'xwidget-translate-range)
            (define-key xwidget-webkit-mode-map (kbd "C-,") #'xwidget-translate-range)))

(provide 'init-xwidget-webkit)
