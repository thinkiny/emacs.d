(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

(defun my-python-mode-hook()
  (setq-local lsp-enable-format-at-save nil)
  (eglot-ensure))

(add-hook 'python-mode-hook #'my-python-mode-hook)


(provide 'init-python)
