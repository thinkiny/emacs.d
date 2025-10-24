;;; init-eglot

(with-eval-after-load 'jsonrpc
  (setq jsonrpc-inhibit-debug-on-error t)
  (setq jsonrpc-event-hook nil)
  (fset #'jsonrpc--log-event #'ignore))

(use-package eglot
  :hook (eglot-managed-mode . my-eglot-mode-hook)
  :config
  (setq eglot-events-buffer-size 0)
  (setq eglot-code-action-indications nil)
  (setq eglot-events-buffer-config '(:size 0 :format full))
  (setq eglot-extend-to-xref t)
  (setq eglot-autoshutdown t)
  (setq eglot-prefer-plaintext t)
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider
                                            :documentOnTypeFormattingProvider))
  (setq jsonrpc-default-request-timeout 15)
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename-with-current)
  (define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-override)
  (define-key eglot-mode-map (kbd "C-c i") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c h") 'eldoc-box-help-at-point)
  (define-key eglot-mode-map (kbd "C-c w r") 'eglot-restart-workspace)
  (define-key eglot-mode-map (kbd "C-c v") 'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-code-actions-current-line)
  (define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions))

(with-eval-after-load 'eglot
  (setq completion-category-defaults nil)
  (setq mode-line-misc-info
        (cl-remove-if (lambda (x) (eq (car x) 'eglot--managed-mode)) mode-line-misc-info))
  ;; (add-to-list 'mode-line-misc-info
  ;;              `(eglot--managed-mode ("[" eglot--mode-line-format "] ")))
  (defun eglot-rename-with-current (newname)
    "Rename the current symbol to NEWNAME."
    (interactive
     (let ((curr (thing-at-point 'symbol t)))
       (list (read-from-minibuffer
              (format "Rename `%s' to: " (or curr
                                             "unknown symbol"))
              curr nil nil nil curr))))
    (eglot-server-capable-or-lose :renameProvider)
    (eglot--apply-workspace-edit
     (eglot--request (eglot--current-server-or-lose)
                     :textDocument/rename `(,@(eglot--TextDocumentPositionParams)
                                            :newName ,newname))
     this-command))

  (defun eglot-disable-format-project()
    (interactive)
    (when-let* ((project-root (projectile-project-root))
                (file (format "%s.dir-locals.el" project-root)))
      (write-region (format "((%s . ((eglot-enable-format-at-save . nil))))" major-mode) nil file)
      (message (format "write %s" file))))

  (eglot--code-action eglot-code-action-override "source.overrideMethods")

  (defun eglot-first-flymake-current-line()
    (if-let* ((digs (flymake-diagnostics (line-beginning-position) (line-end-position))))
        (car (seq-sort-by
              (lambda (x)
                (pcase (flymake-diagnostic-type x)
                  ('eglot-error 0)
                  ('error 0)
                  (_ 1)))
              '<
              digs))))

  (defun eglot-code-actions-current-line()
    (interactive)
    (when-let* ((dig (eglot-first-flymake-current-line))
                (beg (flymake--diag-beg dig))
                (end (flymake--diag-end dig)))
      (eglot-code-actions beg end nil t)))

  (defun eglot-restart-workspace()
    "Reconnect to SERVER.
     INTERACTIVE is t if called interactively."
    (interactive)
    (when-let* ((server (eglot-current-server)))
      (when (jsonrpc-running-p server)
        (ignore-errors (eglot-shutdown server t nil nil))))
    (eglot-ensure))
  )

;; format
(defvar-local eglot-enable-format-at-save t)
(put 'eglot-enable-format-at-save 'safe-local-variable #'always)

(defun eglot-enable-format ()
  (interactive)
  (whitespace-cleanup-mode 1)
  (add-hook 'before-save-hook 'eglot-format-buffer nil t)
  (setq-local eglot-enable-format-at-save t))

(defun eglot-disable-format ()
  (interactive)
  (whitespace-cleanup-mode 0)
  (remove-hook 'before-save-hook 'eglot-format-buffer t)
  ;; (if (bound-and-true-p format-all-mode)
  ;;     (remove-hook 'before-save-hook
  ;;                  'format-all--buffer-from-hook
  ;;                  'local))
  (setq-local eglot-enable-format-at-save nil))

;; (advice-add #'eglot--sig-info :around #'advice/ignore-errors)
;; (advice-add #'jsonrpc--process-filter :around #'advice/ignore-errors)

(defun my-eglot-mode-hook()
  ;; (eglot--setq-saving eldoc-documentation-functions
  ;;                       '(eglot-signature-eldoc-function
  ;;                         eglot-hover-eldoc-function))
  (setq-local completion-at-point-functions
              (list
               #'cape-file
               #'eglot-completion-at-point))

  (auto-revert-mode)
  (if eglot-enable-format-at-save
      (eglot-enable-format)
    (eglot-disable-format)))

(ignore-tramp-ssh-control-master #'eglot--connect)
(advice-add #'eglot-completion-at-point :around #'cape-wrap-buster)

(with-eval-after-load-theme 'eglot
                            ;;(when (theme-dark-p)
                            (set-face-foreground 'eglot-inlay-hint-face (face-attribute 'default :foreground)))

(defun set-eglot-server-progam (mode cmd)
  (setq eglot-server-programs (assoc-delete-all mode eglot-server-programs))
  (if (listp cmd)
      (push `(,mode ,@cmd) eglot-server-programs)
    (push `(,mode ,cmd) eglot-server-programs)))


;; manual execute lsp command
(defun my/eglot-enable-command-provider (orig-fn server)
  "Unconditionally add :executeCommandProvider to Eglot client capabilities."
  (let ((original-capabilities (funcall orig-fn server)))
    ;; Add or update :executeCommandProvider at the top level
    (plist-put original-capabilities
               :executeCommandProvider '(:commands (:dynamicRegistration :json-false)))))

(advice-add 'eglot-client-capabilities :around #'my/eglot-enable-command-provider)

(defun my/eglot-execute-command (command)
  "Interactively execute a COMMAND supported by the current Eglot LSP server.
COMMAND is a string as advertised by the server. No arguments are passed."
  (interactive
   (let* ((server (eglot-current-server))
          (caps (eglot--capabilities server))
          (provider (plist-get caps :executeCommandProvider))
          (commands (and provider (plist-get provider :commands))))
     (list (completing-read "LSP Command: "
                            (or (cl-coerce commands 'list) '())
                            nil nil))))
  (eglot-execute (eglot-current-server) (list :command command)))


(provide 'init-eglot)
