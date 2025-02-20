(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-ts-mode)
                ("SConscript\\'" . python-ts-mode))
              auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(require-package 'pip-requirements)

(use-package pyvenv-auto
  :custom
  (pyvenv-auto-mode t))

(use-package flymake-ruff)
(use-package ruff-format)

(defun my-python-mode-hook()
  (eglot-ensure)
  ;; FIXME
  (run-with-timer 1 nil (lambda ()
                          (flymake-ruff-load)
                          (if eglot-enable-format-at-save
                              (ruff-format-on-save-mode)))))

(add-hook 'python-ts-mode-hook #'my-python-mode-hook)

(provide 'init-python)
