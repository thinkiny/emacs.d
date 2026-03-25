;; -*- lexical-binding: t; -*-

(defvar nov-use-xwidget t)

(require 'init-nov)
(require 'nov-xwidget-mode)

(if nov-use-xwidget
    (add-auto-mode 'nov-xwidget-mode "\\.epub$")
  (add-auto-mode 'nov-mode "\\.epub$"))

(provide 'init-epub)
