;; -*- lexical-binding: t; -*-

(use-package kotlin-mode
  :mode "\\.k\\(ts\\|t\\)$"
  :config
  (add-hook 'kotlin-mode-hook 'lsp-later))

(provide 'init-kotlin)
