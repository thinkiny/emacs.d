(defgroup xwidget-webkit '() "xwidget webkit" :group 'tools)
(defcustom xwidget-webkit-urls '()
  "Specify xwidgets webkit URLS."
  :type '(alist :key-type string :value-type string)
  :group 'xwidget-webkit)

(require 'xwidget)
(setq browse-url-browser-function 'xwidget-webkit-browse-url)

(defun xwidget-plus-webkit-browse-url-advise (&rest _)
  "Advice to add switch to window when calling `xwidget-webkit-browse-url'."
  (switch-to-buffer (xwidget-buffer (xwidget-webkit-current-session))))
(advice-add #'xwidget-webkit-browse-url :after #'xwidget-plus-webkit-browse-url-advise)

(defun xwidget-webkit-browse (url)
  (if (cl-search "://" url)
      (xwidget-webkit-browse-url url)
    (xwidget-webkit-browse-url (concat "http://" url))))

(defun switch-or-create-xwidget-webkit-buffer()
  (interactive)
  (let ((session (xwidget-webkit-current-session)))
    (if session
        (switch-to-buffer (xwidget-buffer session))
      (xwidget-webkit-browse (read-string "URL: ")))))

(global-set-key (kbd "C-x /") #'switch-or-create-xwidget-webkit-buffer)

(provide 'init-xwidget-webkit)
