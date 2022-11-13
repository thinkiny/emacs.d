(use-package eglot
  :hook (eglot-managed-mode . my-eglot-mode-hook))

(use-package consult-eglot)

(defun my-eglot-mode-hook()
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c i") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c e") 'flymake-show-buffer-diagnostics)
  (define-key eglot-mode-map (kbd "C-c h") 'eldoc-doc-buffer)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-code-action-quickfix))

(provide 'init-eglot)
