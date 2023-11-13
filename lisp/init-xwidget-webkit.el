(defgroup xwidget-webkit '() "xwidget webkit" :group 'tools)
(defcustom xwidget-webkit-urls '()
  "Specify xwidgets webkit URLS."
  :type '(alist :key-type string :value-type string)
  :group 'xwidget-webkit)

(require 'xwidget)
(with-eval-after-load 'xwidget
  (defun xwidget-webkit--update-progress-timer-function (_)))
(setq browse-url-browser-function 'xwidget-webkit-browse-url)

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


(defun xwdiget-translate-range()
  (interactive)
  (xwidget-webkit-get-selection (lambda (text)
                                  (bing-dict-brief text))))

(add-hook 'xwidget-webkit-mode-hook
          (lambda ()
            ;;(setq-local isearch-search-fun-function #'xwidget-webkit-search-fun-function)
            (setq-local isearch-lazy-highlight nil)
            (setq-local header-line-format nil)
            (define-key xwidget-webkit-mode-map (kbd "n") 'xwidget-webkit-scroll-up-line)
            (define-key xwidget-webkit-mode-map (kbd "p") 'xwidget-webkit-scroll-down-line)
            (define-key xwidget-webkit-mode-map (kbd "M-v") 'xwidget-webkit-scroll-down)
            (define-key xwidget-webkit-mode-map (kbd "C-v") 'xwidget-webkit-scroll-up)
            ;;(local-set-key (kbd "C-s") #'isearch-forward)
            (local-set-key (kbd "C-,") #'xwdiget-translate-range)))

(provide 'init-xwidget-webkit)
