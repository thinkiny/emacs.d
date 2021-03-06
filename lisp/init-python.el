(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(setq python-shell-interpreter "python2")

(require-package 'pip-requirements)
(require-package 'indent-guide)

(defvar-local lsp-python-extra-paths [])
(after-load 'lsp-pyls
  (lsp-register-custom-settings
   '(("pyls.plugins.jedi.extra_paths" lsp-python-extra-paths)))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection
                                     (lambda () lsp-clients-python-command))
                    :major-modes '(python-mode cython-mode)
                    :priority -1
                    :remote? t
                    :server-id 'pyls-remote
                    :library-folders-fn (lambda (_workspace) lsp-clients-python-library-directories)
                    :initialized-fn (lambda (workspace)
                                      (with-lsp-workspace workspace
                                                          (lsp--set-configuration (lsp-configuration-section "pyls")))))))

(add-hook 'python-mode-hook (lambda ()
                              (setq-local lsp-enable-save-format nil)
                              (lsp-later)
                              (indent-guide-mode)))
(provide 'init-python)
