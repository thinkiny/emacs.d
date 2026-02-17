;; -*- lexical-binding: t; -*-

(require 'pdf-xwidget-mode)
(add-auto-mode 'pdf-xwidget-view "\\.pdf$")
(global-set-key (kbd "C-x / p") #'pdf-xwidget-open)

;; (require 'init-pdf-tools)
(provide 'init-pdf)
