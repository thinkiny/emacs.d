;; -*- lexical-binding: t; -*-

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

(use-package cargo)

(defun my-rust-mode-hook()
  (setq indent-tabs-mode nil)
  (eglot-ensure))

(add-hook 'rust-mode-hook #'my-rust-mode-hook)

(provide 'init-rust)
