(use-package eglot
  :hook (eglot-managed-mode . my-eglot-mode-hook)
  :config
  (setq eglot-events-buffer-size 0)
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

  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename-with-current)
  (define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-override)
  (define-key eglot-mode-map (kbd "C-c i") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c e") 'flymake-show-buffer-diagnostics)
  (define-key eglot-mode-map (kbd "C-c h") 'eldoc-box-eglot-help-at-point)
  (define-key eglot-mode-map (kbd "C-c v") 'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-code-actions-current-line))

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
  (eldoc-box-hover-at-point-mode)
  (if eglot-enable-format-at-save
      (eglot-enable-format)))

(provide 'init-eglot)
