(use-package go-mode
  :after lsp-mode
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t))))

(with-eval-after-load 'lsp-mode
  (setq lsp-go-codelens nil)
  (setq-local lsp-go-env (make-hash-table))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection-fast (lambda () (cons lsp-go-gopls-server-path lsp-go-gopls-server-args)))
                    :major-modes '(go-mode go-dot-mod-mode)
                    :remote? t
                    :server-id 'gopls-remote
                    :completion-in-comments? t
                    :library-folders-fn #'lsp-go--library-default-directories
                    :after-open-fn (lambda ()
                                     (setq-local lsp-completion-filter-on-incomplete nil)))))

(defun check-valid-lsp-go-mode()
  (let ((res (and (bound-and-true-p lsp-mode) (derived-mode-p 'go-mode))))
    (unless res
      (message "current file is not in go-mode and lsp-mode"))
    res))

(defun lsp-enable-module ()
  (interactive)
  (when (and (check-valid-lsp-go-mode) (lsp-session-get-metadata 'go-path))
    (clrhash lsp-go-env)
    (puthash "GO111MODULE" "on" lsp-go-env)
    (lsp-session-set-metadata 'go-path nil)
    (my-lsp-workspace-restart)))

(defun lsp-disable-go-module ()
  (interactive)
  (when (and (check-valid-lsp-go-mode) (not (lsp-session-get-metadata 'go-path))
             (clrhash lsp-go-env)
             (puthash "GO111MODULE" "off" lsp-go-env)
             (puthash "GOPATH" (file-local-name (projectile-project-root)) lsp-go-env)
             (lsp-session-set-metadata 'go-path t)
             (my-lsp-workspace-restart))))

(add-hook 'go-mode-hook #'lsp-later)
(provide 'init-golang)
