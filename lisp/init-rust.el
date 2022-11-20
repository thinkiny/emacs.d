(use-package rust-mode)
(use-package cargo)

(add-hook 'rust-mode-hook #'eglot-ensure)

(provide 'init-rust)
