;;; init-eglot  -*- lexical-binding: t; -*-

(defconst eglot-enable-debug-buffer nil)

;; only for debug
;; (advice-add 'eglot-execute :before
;;             (lambda (server action &rest _)
;;               (message "eglot-execute: server=%s command=%s"
;;                        server (plist-get action :command)))
;;             '((name . eglot-log-execute)))
;; (advice-remove 'eglot-execute 'eglot-log-execute)

(with-eval-after-load 'jsonrpc
  (unless eglot-enable-debug-buffer
    (setq jsonrpc-inhibit-debug-on-error t)
    (setq jsonrpc-event-hook nil)
    (fset #'jsonrpc--log-event #'ignore)))

(use-package eglot
  :hook (eglot-managed-mode . my-eglot-mode-hook)
  :config
  (setq eglot-code-action-indications nil)
  (unless eglot-enable-debug-buffer
    (setq eglot-events-buffer-config '(:size 0 :format full)))
  (setq eglot-extend-to-xref t)
  (setq eglot-autoshutdown t)
  (setq eglot-sync-connect nil)
  (setq eglot-send-changes-idle-time 1)
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider
                                            :documentOnTypeFormattingProvider
                                            :documentRangeFormattingProvider))

  (setq jsonrpc-default-request-timeout 15)
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename-current)
  (define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-override)
  (define-key eglot-mode-map (kbd "C-c i") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c h") 'eldoc-box-help-at-point)
  (define-key eglot-mode-map (kbd "C-c w r") 'eglot-restart-workspace)
  (define-key eglot-mode-map (kbd "C-c v") 'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-code-actions-current-line)
  (define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions)

  ;;(require 'eglot-hover)
  ;; vtsls requests section "" (whole config); the empty-name key `:' answers it.
  ;; rass selects the vtsls overlay for config responses and notifications.
  (setq-default eglot-workspace-configuration '(:gopls (:staticcheck  t
                                                        :usePlaceholders t
                                                        :analyses  (:ST1003 :json-false))
                                                : (:rass
                                                   (:vtsls
                                                    (:vtsls (:autoUseWorkspaceTsdk t)
                                                     :typescript (:preferGoToSourceDefinition t
                                                                  :suggest (:completeFunctionCalls t)
                                                                  :preferences (:importModuleSpecifier "relative"))
                                                     :javascript (:preferGoToSourceDefinition t
                                                                  :suggest (:completeFunctionCalls t)
                                                                  :preferences (:importModuleSpecifier "relative")))))
                                                ))

  ;;(setq completion-category-defaults nil)
  (setq mode-line-misc-info
        (cl-remove-if (lambda (x) (eq (car x) 'eglot--managed-mode)) mode-line-misc-info)))

;; interactive commands
(defun eglot-rename-current ()
  "Rename the symbol at point with its current name prefilled."
  (interactive)
  (let ((bounds (refactor-backend-rename-bounds 'eglot)))
    (unless bounds
      (user-error "Eglot cannot rename the symbol at point"))
    (let* ((symbol-name
            (buffer-substring-no-properties (car bounds) (cdr bounds)))
           (new-name
            (string-trim
             (read-from-minibuffer
              (format "Rename `%s' to: " symbol-name)
              symbol-name nil nil nil symbol-name))))
      (refactor-apply-changeset (refactor-backend-rename 'eglot new-name)
                                :origin this-command))))

(refactor--define-kind-command eglot-code-action-override "source.overrideMethods")

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
    (flymake-mode -1)
    (when (jsonrpc-running-p server)
      (ignore-errors (eglot-shutdown server t nil nil))))
  (eglot-ensure))

;; (advice-add #'eglot--sig-info :around #'advice/ignore-errors)
;; (advice-add #'jsonrpc--process-filter :around #'advice/ignore-errors)

;; disable document sync
(defun my/eglot-disable-document-sync (server)
  "Force disable sync for SERVER."
  (if-let* ((capabilities (eglot--capabilities server)))
      (if (plist-get capabilities :textDocumentSync)
          (plist-put (plist-get capabilities :textDocumentSync)
                     :change 0)))) ; 0 = None, 1 = Full, 2 = Incremental

;; hook
(defun my-eglot-mode-hook()
  ;; (eglot--setq-saving eldoc-documentation-functions
  ;;                       '(eglot-signature-eldoc-function
  ;;                         eglot-hover-eldoc-function))
  (when (file-remote-p default-directory)
    (remove-hook 'before-save-hook #'eglot--signal-textDocument/willSave t)
    (remove-hook 'after-save-hook #'eglot--signal-textDocument/didSave t)
    (add-hook 'eglot-server-initialized-hook #'my/eglot-disable-document-sync t))

  (setq-local completion-at-point-functions
              (list
               #'cape-file
               #'eglot-completion-at-point))

  ;; (eglot-hover-mode)
  )

(defun sync-eglot-inlay-hint-face ()
  (set-face-foreground 'eglot-inlay-hint-face
                       (face-attribute 'default :foreground)))

(with-eval-after-load 'eglot
  (sync-eglot-inlay-hint-face)
  (add-hook 'load-theme-hook #'sync-eglot-inlay-hint-face))

;; set-server-program
(defun set-eglot-server-program (modes cmd)
  "Set or update eglot server program for MODES with CMD.
Removes any existing entries for the specified modes and adds new configuration."
  (cl-labels ((ensure-list (value)
                (if (listp value) value (list value)))
              (mode-symbol (mode)
                (if (consp mode) (car mode) mode))
              (entry-symbols (entry-key)
                (mapcar #'mode-symbol (ensure-list entry-key))))
    (let* ((modes (ensure-list modes))
           (mode-symbols (mapcar #'mode-symbol modes))
           (cmd-list (ensure-list cmd)))
      (setq eglot-server-programs
            (cl-remove-if
             (lambda (entry)
               (seq-intersection mode-symbols (entry-symbols (car entry))))
             eglot-server-programs))
      (push (append (list modes) cmd-list) eglot-server-programs))))

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

;; tramp
(defvar eglot-uri-to-path-tramp-cache nil)
(defun cache-tramp-eglot-uri-to-path(orig &optional uri)
  (cache-tramp-from-kv uri
                       'eglot-uri-to-path-tramp-cache
                       orig
                       uri))

(advice-add 'eglot-uri-to-path :around #'cache-tramp-eglot-uri-to-path)

(provide 'init-eglot)
