(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

(defvar-local lsp-pylsp-extra-paths [])
(after-load 'lsp-pylsp
  (setq lsp-pylsp-plugins-yapf-enabled t)
  (lsp-register-custom-settings '(("pylsp.plugins.jedi.extra_paths" lsp-pylsp-extra-paths)))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection-new
                                     (lambda () lsp-pylsp-server-command))
                    :major-modes '(python-mode cython-mode)
                    :priority -1
                    :remote? t
                    :server-id 'pylsp-remote
                    :library-folders-fn (lambda (_workspace) lsp-clients-pylsp-library-directories)
                    :initialized-fn (lambda (workspace)
                                      (with-lsp-workspace workspace
                                        (lsp--set-configuration (lsp-configuration-section "pylsp")))))))

(add-hook 'python-mode-hook #'lsp-later)
;; (use-package lsp-jedi
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-jedi)
;;                          (lsp-later))))

(provide 'init-python)
