(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

(use-package pyvenv-auto
  :custom
  (pyvenv-auto-mode t))

(defun my-python-mode-hook()
  (setq-local lsp-enable-format-at-save nil)
  (eglot-ensure))

(add-hook 'python-mode-hook #'my-python-mode-hook)
(add-hook 'python-ts-mode-hook #'my-python-mode-hook)

(provide 'init-python)
