(use-package kotlin-mode
  :mode "\\.kt$"
  :config
  (add-hook 'kotlin-mode-hook 'lsp-later))

(provide 'init-kotlin)
