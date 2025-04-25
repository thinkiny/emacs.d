(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-ts-mode)
                ("SConscript\\'" . python-ts-mode))
              auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(require-package 'pip-requirements)

;; use https://github.com/garyo/lsp-multiplexer/tree/main
;; (with-eval-after-load 'eglot
;;   (set-eglot-server-progam '(python-mode python-ts-mode) '("lsp-multiplexer-python")))

(use-package pyvenv-auto
  :custom
  (pyvenv-auto-mode t))

(defun my-python-mode-hook()
  (eglot-ensure)
  (format-all-mode))

(add-hook 'python-ts-mode-hook #'my-python-mode-hook)

(provide 'init-python)
