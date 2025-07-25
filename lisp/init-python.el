(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-ts-mode)
                ("SConscript\\'" . python-ts-mode))
              auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(require-package 'pip-requirements)

;; use https://github.com/garyo/lsp-multiplexer/tree/main
;;(with-eval-after-load 'eglot
;;  (set-eglot-server-progam '(python-mode python-ts-mode) '("lsp-multiplexer-python")))

(use-package pyvenv-auto
  :custom
  (pyvenv-auto-mode t))

(use-package sphinx-doc
  :config
  (setq sphinx-doc-include-types t)
  (define-key sphinx-doc-mode-map (kbd "C-c d i") #'sphinx-doc))


(defun set-python-eglot-workspace()
  (setq-local eglot-enable-format-at-save nil)
  (toggle-format-all-mode)
  (setq-default eglot-workspace-configuration
                '(:basedpyright.analysis (:typeCheckingMode "standard"
                                          :diagnosticSeverityOverrides (:reportOptionalMemberAccess "warning"
                                                                        :reportOptionalSubscript "warning"
                                                                        :reportReturnType "warning"
                                                                        :reportTypedDictNotRequiredAccess "warning")
                                          :useLibraryCodeForTypes t
                                          :diagnosticMode "workspace"
                                          :autoSearchPaths t))))


(defun my-python-mode-hook()
  (sphinx-doc-mode)
  (set-python-eglot-workspace)
  (eglot-ensure))

(add-hook 'python-ts-mode-hook #'my-python-mode-hook)

(provide 'init-python)
