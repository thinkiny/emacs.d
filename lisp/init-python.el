(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)
(require-package 'lsp-pyright)

(defvar-local lsp-pylsp-extra-paths [])
(defvar-local lsp-pylsp-cache-for [])
(with-eval-after-load 'lsp-mode
  (require 'lsp-pyright)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection-fast (lambda ()
                                                 (cons "pyright-python-langserver"
                                                       lsp-pyright-langserver-command-args)))
    :major-modes '(python-mode cython-mode)
    :multi-root lsp-pyright-multi-root
    :priority -1
    :remote? t
    :server-id 'pyright-remote
    :priority -1
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        ;; we send empty settings initially, LSP server will ask for the
                        ;; configuration of each workspace folder later separately
                        (lsp--set-configuration
                         (make-hash-table :test 'equal))))
    :download-server-fn (lambda (_client callback error-callback _update?)
                          (lsp-package-ensure 'pyright callback error-callback))
    :notification-handlers (lsp-ht ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
                                   ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
                                   ("pyright/endProgress" 'lsp-pyright--end-progress-callback))))
)

(defun my-python-mode-hook()
  (setq-local lsp-enable-format-at-save nil)
  (eglot-ensure))

(add-hook 'python-mode-hook #'my-python-mode-hook)


(provide 'init-python)
