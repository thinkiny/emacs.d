;; -*- lexical-binding: t; -*-

(defgroup xwidget-webkit '() "xwidget webkit" :group 'tools)
(defcustom xwidget-webkit-urls '()
  "Specify xwidgets webkit URLS."
  :type '(alist :key-type string :value-type string)
  :group 'xwidget-webkit)

(require 'xwidget)
(setq xwidget-webkit-buffer-name-format "*WEB: %T")
;; (with-eval-after-load 'xwidget
;;   (defun xwidget-webkit--update-progress-timer-function (_)))

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

(defun xwidget-get-file-url ()
  (if (s-ends-with? ".html" (buffer-file-name))
      (list (concat "file://" (buffer-file-name)))))

(defun xwidget-webkit-browse (url)
  (interactive (list (read-string "URL: " (xwidget-get-file-url))))
  (if (cl-search "://" url)
      (xwidget-webkit-browse-open-url url)
    (xwidget-webkit-browse-open-url (concat "http://" url))))

;; xwwp-follow-link
(use-package xwwp-follow-link-ivy
  :custom
  (xwwp-follow-link-completion-system 'ivy)
  :bind (:map xwidget-webkit-mode-map
              ("C-c l" . xwwp-follow-link)))


;;; Search text in page
;; Initialize last search text length variable when isearch starts
(defvar-local xwidget-webkit-isearch-last-length 0)
(defvar-local xwidget-webkit-searching nil)

;; This is minimal. Regex and incremental search will be nice
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
                 (regexp-quote string))
         #'xwidget-webkit-search-cb)
        (point-min)))))

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

(defun xwidget-webkit-open-in-chrome(&optional rest)
  (interactive)
  (let* ((url (xwidget-webkit-uri (xwidget-webkit-current-session))))
    (setq prev-frame (selected-frame))
    (browse-url-chrome url)
    (quit-window)
    (run-with-timer 0.15 nil
                    (lambda ()
                      (select-frame-set-input-focus prev-frame)))))

(defun xwidget-scroll-up-scan()
  (interactive)
  (xwidget-webkit-scroll-up percision-scroll-scan-height))

(defun xwidget-scroll-down-scan()
  (interactive)
  (xwidget-webkit-scroll-down percision-scroll-scan-height))

(defun xwidget-scroll-up-page()
  (interactive)
  (xwidget-webkit-scroll-up (get-precision-scroll-page-height)))

(defun xwidget-scroll-down-page()
  (interactive)
  (xwidget-webkit-scroll-down (get-precision-scroll-page-height)))

(with-eval-after-load 'xwidget
  (easy-menu-define nil xwidget-webkit-mode-map "Xwidget WebKit menu."
    (list "Xwidget WebKit"  :visible nil))

  (define-key xwidget-webkit-mode-map (kbd "g") #'xwidget-webkit-browse-url)
  (define-key xwidget-webkit-mode-map (kbd "n") 'xwidget-scroll-up-scan)
  (define-key xwidget-webkit-mode-map (kbd "p") 'xwidget-scroll-down-scan)
  (define-key xwidget-webkit-mode-map (kbd "j") 'xwidget-scroll-up-scan)
  (define-key xwidget-webkit-mode-map (kbd "k") 'xwidget-scroll-down-scan)
  (define-key xwidget-webkit-mode-map (kbd "s") 'xwidget-scroll-up-scan)
  (define-key xwidget-webkit-mode-map (kbd "w") 'xwidget-scroll-down-scan)
  (define-key xwidget-webkit-mode-map (kbd "v") 'xwidget-scroll-up-page)
  (define-key xwidget-webkit-mode-map (kbd "M-v") 'xwidget-scroll-down-page)
  (define-key xwidget-webkit-mode-map (kbd "M-c") 'xwidget-webkit-copy-selection-as-kill)
  (define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
  (define-key xwidget-webkit-mode-map (kbd "O") 'xwidget-webkit-open-in-chrome)
  (define-key xwidget-webkit-mode-map (kbd "C-v") 'xwidget-scroll-up-page)
  ;;(define-key xwidget-webkit-mode-map (kbd "<drag-mouse-1>") #'xwidget-translate-range)
  (define-key xwidget-webkit-mode-map (kbd "C-s") #'isearch-forward)
  (define-key xwidget-webkit-mode-map (kbd "C-r") #'isearch-backward)
  ;;(define-key xwidget-webkit-mode-map (kbd "<double-mouse-1>") #'xwidget-translate-range)
  (define-key xwidget-webkit-mode-map (kbd "C-,") #'xwidget-translate-range))

(defun xwidget-execute-scripts(scripts)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   (format "(function() { %s })();" (string-join scripts "\n"))))

(defun xwidget-execute-script(script)
  (xwidget-execute-scripts (list script)))

(defun my-xwidget-webkit-mode-hook()
  ;;(setq-local auto-translate-mouse-selection t)
  (setq-local isearch-search-fun-function #'xwidget-webkit-search-fun-function)
  (setq-local isearch-lazy-highlight nil)
  (setq-local header-line-format nil))

(add-hook 'xwidget-webkit-mode-hook #'my-xwidget-webkit-mode-hook)

(global-set-key (kbd "C-x /") #'xwidget-webkit-browse)

(provide 'init-xwidget-webkit)
