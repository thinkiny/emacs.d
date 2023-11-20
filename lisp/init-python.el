(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

(defvar-local lsp-pylsp-extra-paths [])
(defvar-local lsp-pylsp-cache-for [])
(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection
                                     (lambda () lsp-pylsp-server-command))
                    :activation-fn (lsp-activate-on "python")
                    :priority -1
                    :remote? t
                    :server-id 'pylsp
                    :library-folders-fn (lambda (_workspace) lsp-clients-pylsp-library-directories)
                    :initialized-fn (lambda (workspace)
                                      (with-lsp-workspace workspace
                                        (lsp--set-configuration (lsp-configuration-section "pylsp")))))))

(add-hook 'python-mode-hook (lambda ()
                              (setq-local lsp-enable-format-at-save nil)
                              (lsp-later)))

(provide 'init-python)
