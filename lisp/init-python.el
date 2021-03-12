(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

(defvar-local lsp-pyls-extra-paths [])
(after-load 'lsp-pyls
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

;; (use-package lsp-pyright
;;   :demand t
;;   :config
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-tramp-connection (cons "pyright-langserver"
;;                                                 lsp-pyright-langserver-command-args))
;;     :major-modes '(python-mode)
;;     :remote? t
;;     :server-id 'pyright-remote
;;     :multi-root lsp-pyright-multi-root
;;     :priority 1
;;     :initialized-fn (lambda (workspace)
;;                       (with-lsp-workspace workspace
;;                         ;; we send empty settings initially, LSP server will ask for the
;;                         ;; configuration of each workspace folder later separately
;;                         (lsp--set-configuration
;;                          (make-hash-table :test 'equal))))
;;     :notification-handlers (lsp-ht ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
;;                                    ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
;;                                    ("pyright/endProgress" 'lsp-pyright--end-progress-callback)))))

(add-hook 'python-mode-hook (lambda ()
                              (setq-local lsp-enable-save-format nil)
                              (lsp-later)))
(provide 'init-python)
