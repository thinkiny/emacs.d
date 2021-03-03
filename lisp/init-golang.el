(use-package go-mode
  :after lsp-mode
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)
     ("gopls.deepCompletion" t t))))

(after-load 'lsp-go
  (setq lsp-go-codelens nil)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "gopls")
                    :major-modes '(go-mode go-dot-mod-mode)
                    :remote? t
                    :priority 0
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

(defun lsp-go-module-on ()
  (interactive)
  (when (and (check-valid-lsp-go-mode) (lsp-workspace-get-metadata 'go-path))
    (clrhash lsp-go-env)
    (puthash "GO111MODULE" "on" lsp-go-env)
    (unless lsp-later-timer
      (call-interactively #'lsp-workspace-restart))
    (lsp-workspace-set-metadata 'go-path nil)))

(defun lsp-go-module-off ()
  (interactive)
  (when (and (check-valid-lsp-go-mode) (not (lsp-workspace-get-metadata 'go-path))
      (clrhash lsp-go-env)
      (puthash "GO111MODULE" "off" lsp-go-env)
      (puthash "GOPATH" (get-tramp-local-name (projectile-project-root)) lsp-go-env)
      (unless lsp-later-timer
        (call-interactively #'lsp-workspace-restart))
      (lsp-workspace-set-metadata 'go-path t))))

(add-hook 'go-mode-hook #'lsp-later)
(provide 'init-golang)
