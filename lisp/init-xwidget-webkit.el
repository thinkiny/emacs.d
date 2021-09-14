(defgroup xwidget-webkit '() "xwidget webkit" :group 'tools)
(defcustom xwidget-webkit-urls '()
  "Specify xwidgets webkit URLS."
  :type '(alist :key-type string :value-type string)
  :group 'xwidget-webkit)

(require 'xwidget)
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

(provide 'init-xwidget-webkit)
