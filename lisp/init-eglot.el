(use-package eglot
  :hook (eglot-managed-mode . my-eglot-mode-hook)
  :config
  (setq eglot-events-buffer-size 0)
  (setq eglot-extend-to-xref t)
  (setq eglot-autoshutdown t)
  (setq eglot-prefer-plaintext t)
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename-with-current)
  (define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-override)
  (define-key eglot-mode-map (kbd "C-c i") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c e") 'flymake-show-buffer-diagnostics)
  (define-key eglot-mode-map (kbd "C-c h") 'eldoc-box-help-at-point)
  (define-key eglot-mode-map (kbd "C-c w r") 'eglot-restart-workspace)
  (define-key eglot-mode-map (kbd "C-c v") 'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-code-actions-current-line))

(with-eval-after-load 'eglot
  (defun print-eglot-project-root ()
    (interactive)
    (if-let ((server (eglot-current-server)))
        (message (project-root (eglot--project server)))
      (message "eglot server is not alive")))

  (defun eglot-disable-format-project()
    (interactive)
    (when-let ((project-root (projectile-project-root))
               (file (format "%s.dir-locals.el" project-root)))
      (write-region (format "((%s . ((eval . (eglot-disable-format)))))" major-mode) nil file)
      (message (format "write %s" file))))

  (defun eglot-rename-with-current (newname)
    "Rename the current symbol to NEWNAME."
    (interactive
     (let ((curr (thing-at-point 'symbol t)))
       (list (read-from-minibuffer
              (format "Rename `%s' to: " (or curr
                                             "unknown symbol"))
              curr nil nil nil
              (symbol-name (symbol-at-point))))))

    (unless (eglot--server-capable :renameProvider)
      (eglot--error "Server can't rename!"))
    (eglot--apply-workspace-edit
     (jsonrpc-request (eglot--current-server-or-lose)
                      :textDocument/rename `(,@(eglot--TextDocumentPositionParams)
                                             :newName ,newname))
     current-prefix-arg))

  (eglot--code-action eglot-code-action-override "source.overrideMethods")
  (defun eglot-code-actions-current-line()
    (interactive)
    (eglot-code-actions (line-beginning-position) (line-end-position) nil t))

  (defun eglot-restart-workspace()
    (interactive)
    (when-let ((server (eglot-current-server)))
      (cancel-timer eldoc-timer)
      (setq-local eldoc-idle-delay 60)
      (eglot-shutdown server))
    (setq-local eldoc-idle-delay 0.5)
    (eglot-ensure))
  )

(use-package consult-eglot)

;; format
(defvar-local eglot-enable-format-at-save t)
(defun eglot-enable-format ()
  (interactive)
  (whitespace-cleanup-mode 1)
  (add-hook 'before-save-hook 'eglot-format-buffer nil 'eglot-format)
  (setq-local eglot-enable-format-at-save t))

(defun eglot-disable-format ()
  (interactive)
  (whitespace-cleanup-mode 0)
  (remove-hook 'before-save-hook 'eglot-format-buffer 'eglot-format)
  (setq-local eglot-enable-format-at-save nil))

(defun my-eglot-mode-hook()
;; (eglot--setq-saving eldoc-documentation-functions
;;                       '(eglot-signature-eldoc-function))
  (eglot--setq-saving completion-at-point-functions
                      (list
                       (cape-capf-buster
                        (cape-super-capf
                         (cape-company-to-capf #'company-files)
                         #'eglot-completion-at-point
                         )
                        'equal)))
  (if eglot-enable-format-at-save
      (eglot-enable-format))
  (flymake-mode))

(ignore-tramp-ssh-control-master #'eglot--connect)

(with-eval-after-load-theme
 'eglot
 (when (theme-dark-p)
   (set-face-foreground 'eglot-inlay-hint-face (face-attribute 'default :foreground))))

(provide 'init-eglot)
