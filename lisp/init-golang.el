(use-package go-mode
  :after lsp-mode
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t))))

(with-eval-after-load 'lsp-mode
  (setq lsp-go-codelens nil)
  ;;(setq lsp-go-env (make-hash-table))
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

(defun lsp-enable-go-module ()
  (interactive)
  (when (and (check-valid-lsp-go-mode) (lsp-session-get-metadata 'go-path))
    (setq-local lsp-go-env (make-hash-table))
    (puthash "GO111MODULE" "on" lsp-go-env)
    (lsp-session-set-metadata 'go-path nil)
    (my-lsp-workspace-restart)))

(defun lsp-disable-go-module ()
  (interactive)
  (when (and (check-valid-lsp-go-mode) (not (lsp-session-get-metadata 'go-path))
             (setq-local lsp-go-env (make-hash-table))
             (puthash "GO111MODULE" "off" lsp-go-env)
             (puthash "GOPATH" (file-local-name (projectile-project-root)) lsp-go-env)
             (lsp-session-set-metadata 'go-path t)
             (my-lsp-workspace-restart))))

(defun lsp-enable-go-bazel()
  (interactive)
  (setq-local lsp-go-env (make-hash-table))
  ;; (puthash "GOPACKAGESDRIVER_BAZEL_TARGETS" "//app/..." lsp-go-env)
  ;; (puthash "GOPACKAGESDRIVER_BAZEL_QUERY" "kind(go_library, //app/...)" lsp-go-env)
  (puthash "GOPACKAGESDRIVER" (concat (projectile-project-root) "/gopackagesdriver.sh") lsp-go-env)
  (my-lsp-workspace-restart))

(defun generate-json-tag-line ()
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (if (string-match "^\\s-+\\(\\S-+\\)\\s-+\\S-+$" line)
        (let ((json-tag (format "  `json:\"%s%s\"`"
                                (downcase (substring (match-string 1 line) 0 1))
                                (substring (match-string 1 line) 1))))
          (end-of-line)
          (insert json-tag)))))

(defun generate-json-tag ()
  (interactive)
  (save-excursion
    (let ((start (re-search-backward "{$" nil t)))
      (forward-line 1)
      (while (not (= (char-after (line-beginning-position)) ?}))
        (generate-json-tag-line)
        (forward-line 1)))))

(defun my-go-mode-hook()
  (subword-mode)
  (lsp-later)
  (define-key go-mode-map (kbd "C-c g j") #'generate-json-tag))
(add-hook 'go-mode-hook #'my-go-mode-hook)

(provide 'init-golang)
