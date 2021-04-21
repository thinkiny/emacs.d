(use-package rust-mode)
(use-package cargo)

(add-hook 'rust-mode-hook #'lsp-later)

(provide 'init-rust)
