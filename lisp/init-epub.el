;; -*- lexical-binding: t; -*-

(defvar nov-use-xwidget t)

(require 'init-nov)
(require 'nov-xwidget)

(defun modeline-nov-document-index()
  (format " %d/%d" nov-documents-index (length nov-documents)))

(if nov-use-xwidget
    (add-auto-mode 'nov-xwidget-mode "\\.epub$")
  (add-auto-mode 'nov-mode "\\.epub$"))

(provide 'init-epub)
