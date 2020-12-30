(use-package go-mode
  :after lsp-mode
  :config
  (setq lsp-go-codelens nil)
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)
     ("gopls.deepCompletion" t t))))

(after-load 'lsp-mode
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

(defun update-go-module-on (enable)
  (if (derived-mode-p 'go-mode 'lsp-mode)
      (progn
        (clrhash lsp-go-env)
        (if enable
            (puthash "GO111MODULE" "on" lsp-go-env)
          (progn
            (puthash "GO111MODULE" "off" lsp-go-env)
            (puthash "GOPATH" (get-tramp-local-name (projectile-project-root)) lsp-go-env)))
        (call-interactively #'lsp-workspace-restart)
        (if enable
            (message "go module on")
          (message "go module off")))
    (message "current file is not in go-mode and lsp-mode")))

(defun enable-go-module ()
  (interactive)
  (update-go-module-on t))

(defun disable-go-module ()
  (interactive)
  (update-go-module-on nil))

(add-hook 'go-mode-hook (lambda ()
                          (lsp)
                          (set-local lsp-go-env (make-hash-table))))
(provide 'init-golang)
