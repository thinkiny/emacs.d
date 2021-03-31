;; lsp
(use-package lsp-mode
  :config
  (diminish 'lsp-mode)
  (setq lsp-enable-file-watchers nil
        lsp-inhibit-message t
        lsp-diagnostics-provider :flycheck
        lsp-modeline-diagnostics-enable nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-file-watch-threshold nil
        lsp-before-save-edits nil
        lsp-eldoc-render-all  nil
        lsp-lens-enable nil
        lsp-signature-render-documentation nil
        lsp-enable-folding nil
        lsp-enable-links nil
        lsp-keep-workspace-alive nil
        lsp-log-io nil
        lsp-idle-delay 0.500
        lsp-headerline-breadcrumb-enable nil
        lsp-diagnostic-clean-after-change t
        lsp-enable-dap-auto-configure nil)

  (define-key lsp-mode-map (kbd "C-c r") 'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c a") 'lsp-avy-lens)
  (define-key lsp-mode-map (kbd "C-c i") 'lsp-organize-imports)
  (define-key lsp-mode-map (kbd "C-c w r") 'lsp-workspace-restart-fix)
  (define-key lsp-mode-map (kbd "C-c w a") 'lsp-workspace-folders-add)
  (define-key lsp-mode-map (kbd "C-c w d") 'lsp-workspace-folders-remove)
  (define-key lsp-mode-map (kbd "C-c w c") 'lsp-workspace-shutdown)
  (define-key lsp-mode-map (kbd "C-c SPC") 'lsp-signature-activate)
  (define-key lsp-mode-map (kbd "C-c y") 'dap-hydra)
  ;;(define-key lsp-mode-map (kbd "C-c v") 'lsp-ui-peek-find-implementation)
  (define-key lsp-mode-map (kbd "C-c v") 'lsp-find-implementation)
  (define-key lsp-mode-map (kbd "C-c e") 'lsp-ui-flycheck-list)
  (define-key lsp-mode-map (kbd "C-c f") 'lsp-execute-code-action)
  (define-key lsp-signature-mode-map (kbd "M-j") #'lsp-signature-next)
  (define-key lsp-signature-mode-map (kbd "M-k") #'lsp-signature-previous))

(use-package lsp-ivy
  :demand t
  :after lsp-mode
  :config
  (define-key lsp-mode-map (kbd "C-c w s") 'lsp-ivy-workspace-symbol))
;;(use-package lsp-sonarlint :after lsp-mode)

;;dap-mode
(use-package hydra)
(use-package posframe)
(use-package dap-mode
  :after lsp-mode
  :config
  (setq dap-auto-configure-features '(locals tooltip))
  (setq dap-debug-restart-keep-session nil)
  (define-key dap-mode-map (kbd "C-c /") #'dap-lsp-debug))

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

(defun dap-lsp-debug ()
  (interactive)
  (dap-delete-all-sessions)
  (call-interactively 'dap-debug))

;; format
(defvar-local lsp-enable-format t)
(defun lsp-format-on ()
  (interactive)
  (when (bound-and-true-p lsp-mode)
    (whitespace-cleanup-mode 1)
    (add-hook 'before-save-hook 'lsp-format-buffer nil 'lsp-format)
    (setq-local lsp-enable-format t)))

(defun lsp-format-off ()
  (interactive)
  (when (bound-and-true-p lsp-mode)
    (whitespace-cleanup-mode 0)
    (remove-hook 'before-save-hook 'lsp-format-buffer 'lsp-format)
    (setq-local lsp-enable-format nil)))

(defun lsp-format-off-project()
  (interactive)
  (when-let ((project-root (projectile-project-root)))
    (write-region (format "((%s . ((eval . (lsp-format-off)))))" major-mode) nil (format "%s/.dir-locals.el" project-root))))

;; tramp

;; lsp-restart fix tramp stuck
(defun lsp-workspace-restart-fix()
  (interactive)
  (call-interactively #'lsp-workspace-shutdown)
  (lsp-deferred))

(advice-add 'lsp :before (lambda (&optional arg)
                           (when-let ((name (buffer-file-name)))
                             (if (file-remote-p name)
                                 (setq-local lsp-log-io t)))))

(ignore-tramp-ssh-control-master 'lsp--start-workspace)

(add-hook 'lsp-log-io-mode-hook (lambda ()
                                  (run-at-time 10 10 #'lsp--erase-log-buffer)))

;; hook
(add-hook 'lsp-mode-hook (lambda ()
                           (setq-local flycheck-idle-change-delay 1.0)
                           (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
                           (setq-local global-whitespace-cleanup-mode nil)
                           (if lsp-enable-format
                               (lsp-format-on))))

;; lsp-ui
(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-sideline-update-mode 'line
        lsp-ui-doc-enable nil
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-imenu-enable nil
        lsp-ui-peek-enable t
        lsp-ui-peek-fontify 'always
        lsp-ui-sideline-delay 0.2)
  (set-face-foreground 'lsp-ui-sideline-code-action "MediumPurple4"))
;;(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-treemacs)

;; lsp-later
(defvar-local lsp-later-timer nil)
(defun lsp-later-run ()
  (setq-local lsp-later-timer nil)
  (lsp))

(defun lsp-later()
  (setq-local lsp-later-timer (run-at-time 5 nil #'lsp-later-run)))

(add-hook 'hack-local-variables-hook
          (lambda ()
            (when lsp-later-timer
              (cancel-timer lsp-later-timer)
              (lsp-later-run))))

(provide 'init-lsp)
