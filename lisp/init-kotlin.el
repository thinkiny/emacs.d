(use-package kotlin-mode
  :mode "\\.k\\(ts\\|t\\)$"
  :config
  (add-hook 'kotlin-mode-hook 'eglot-ensure))

(provide 'init-kotlin)
