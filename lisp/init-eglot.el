(use-package eglot
  :hook (eglot-managed-mode . my-eglot-mode-hook)
  :config
  (setq eldoc-documentation-functions
        (cons #'flymake-eldoc-function
              (remove #'flymake-eldoc-function eldoc-documentation-functions))))

(use-package consult-eglot)

;; format
(defvar-local eglot-enable-format-at-save t)
(defun eglot-enable-format ()
  (interactive)
  (whitespace-cleanup-mode 1)
  (add-hook 'before-save-hook 'eglot-format-buffer nil 'eglot-format)
  (setq-local eglot-enable-format-at-save t))

(defun eglot-disable-format ()
  (interactive)
  (whitespace-cleanup-mode 0)
  (remove-hook 'before-save-hook 'eglot-format-buffer 'eglot-format)
  (setq-local eglot-enable-format-at-save nil))

(defun my-eglot-mode-hook()
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c i") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c e") 'flymake-show-buffer-diagnostics)
  (define-key eglot-mode-map (kbd "C-c h") 'eldoc-doc-buffer)
  (define-key eglot-mode-map (kbd "C-c v") 'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-code-action-quickfix)
  (eglot--setq-saving eldoc-documentation-functions
                      '(eglot-signature-eldoc-function))
  (if eglot-enable-format-at-save
      (eglot-enable-format)))

(provide 'init-eglot)
