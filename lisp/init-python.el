(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

(defvar-local lsp-pyls-extra-paths [])
(after-load 'lsp-pyls
  (setq lsp-pyls-plugins-yapf-enabled t)
  (lsp-register-custom-settings '(("pyls.plugins.jedi.extra_paths" lsp-pyls-extra-paths)))
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

(add-hook 'python-mode-hook #'lsp-later)

;; (use-package lsp-jedi
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-jedi)
;;                          (lsp-later))))

(provide 'init-python)
