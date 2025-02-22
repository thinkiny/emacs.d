(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-ts-mode)
                ("SConscript\\'" . python-ts-mode))
              auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(require-package 'pip-requirements)

(use-package pyvenv-auto
  :custom
  (pyvenv-auto-mode t))

;; use https://github.com/garyo/lsp-multiplexer/tree/main
(defun my-python-mode-hook()
  (eglot-ensure))

(add-hook 'python-ts-mode-hook #'my-python-mode-hook)

(provide 'init-python)
