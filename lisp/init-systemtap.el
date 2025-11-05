;; -*- lexical-binding: t; -*-

(maybe-require-package 'systemtap-mode)
(add-to-list 'auto-mode-alist '("\\.stp\\'" . systemtap-mode))
(provide 'init-systemtap)
