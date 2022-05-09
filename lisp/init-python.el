(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

(defvar-local lsp-pylsp-extra-paths [])
(defvar-local lsp-pylsp-cache-for [])
(with-eval-after-load 'lsp-mode
  ;;(setq lsp-pylsp-plugins-yapf-enabled t)
  (lsp-register-custom-settings '(("pylsp.plugins.jedi.extra_paths" lsp-pylsp-extra-paths)
                                  ("pylsp.plugins.jedi_completion.cache_for" lsp-pylsp-cache-for)))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection-fast
                                     (lambda () lsp-pylsp-server-command))
                    :major-modes '(python-mode cython-mode)
                    :priority -1
                    :remote? t
                    :server-id 'pylsp-remote
                    :library-folders-fn (lambda (_workspace) lsp-clients-pylsp-library-directories)
                    :initialized-fn (lambda (workspace)
                                      (with-lsp-workspace workspace
                                        (lsp--set-configuration (lsp-configuration-section "pylsp")))))))

(defun my-python-mode-hook()
  (setq-local lsp-enable-format-at-save nil)
  (eglot-ensure))

(add-hook 'python-mode-hook #'my-python-mode-hook)


(provide 'init-python)
