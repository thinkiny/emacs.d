(use-package go-mode
  :after lsp-mode
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)
     ("gopls.deepCompletion" t t))))

(after-load 'lsp-go
  (setq lsp-go-codelens nil)
  (setq-local lsp-go-env (make-hash-table))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection
                                     (lambda () (cons lsp-go-gopls-server-path lsp-go-gopls-server-args)))
                    :major-modes '(go-mode go-dot-mod-mode)
                    :remote? t
                    :priority 0
                    :server-id 'gopls-remote
                    :completion-in-comments? t
                    :library-folders-fn #'lsp-go--library-default-directories
                    :after-open-fn (lambda ()
                                     (setq-local lsp-completion-filter-on-incomplete nil)))))

(defun check-valid-go-mode()
  (unless (and (bound-and-true-p lsp-mode) (derived-mode-p 'go-mode))
    (message "current file is not in go-mode and lsp-mode")))

(defun lsp-go-module-on ()
  (interactive)
  (when (check-valid-go-mode)
    (clrhash lsp-go-env)
    (puthash "GO111MODULE" "on" lsp-go-env)
    (call-interactively #'lsp-workspace-restart)
    (message "lsp go module on")))

(defun lsp-go-module-off ()
  (interactive)
  (when (check-valid-go-mode)
      (clrhash lsp-go-env)
      (puthash "GO111MODULE" "off" lsp-go-env)
      (puthash "GOPATH" (get-tramp-local-name (projectile-project-root)) lsp-go-env)
      (call-interactively #'lsp-workspace-restart)
      (message "go module off")))

(add-hook 'go-mode-hook #'lsp)
(provide 'init-golang)
