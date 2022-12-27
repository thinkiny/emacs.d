;;; -*- lexical-binding: t; -*-
(use-package lsp-mode
  :config
  (diminish 'lsp-mode)
  (require 'lsp-diagnostics)
  (setq lsp-inhibit-message t
        lsp-diagnostics-provider :flycheck
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-indentation nil
        lsp-references-exclude-definition t
        ;;lsp-enable-xref t
        lsp-auto-configure t
        lsp-enable-on-type-formatting nil
        lsp-enable-text-document-color nil
        lsp-file-watch-threshold nil
        lsp-before-save-edits nil
        lsp-eldoc-render-all nil
        lsp-enable-file-watchers t
        lsp-lens-enable nil
        lsp-signature-render-documentation nil
        lsp-enable-folding nil
        lsp-enable-links nil
        lsp-keep-workspace-alive nil
        lsp-log-io nil
        lsp-idle-delay 1
        ;;lsp-auto-guess-root t
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-dap-auto-configure nil
        ;; lsp-signature-doc-lines 1
        ;; lsp-signature-function #'my-lsp-lv-message
        )

  (defun my-lsp-lv-message (message)
    (if message
        (let ((pos (min (or (string-search "\n" message) (length message))
                        (frame-width))))
          (message (substring message 0 pos)))))

  (defun my-lsp-format()
    (interactive)
    (if (region-active-p)
        (call-interactively #'lsp-format-region)
      (lsp-format-buffer)))

  (defun lsp--text-document-code-action-params-line (&optional kind)
    "Code action params."
    (list :textDocument (lsp--text-document-identifier)
          :range  (lsp--region-to-range (line-beginning-position) (line-end-position))
          :context `( :diagnostics ,(lsp-cur-line-diagnostics)
                      ,@(when kind (list :only (vector kind))))))

  (defun lsp-code-actions-this-line (&optional kind)
    "Retrieve the code actions for the active region or the current line.
It will filter by KIND if non nil."
    (lsp-request "textDocument/codeAction" (lsp--text-document-code-action-params-line kind)))

  (lsp-defun lsp-execute-code-action-this-line ((action &as &CodeAction :command? :edit?))
    "Execute code action ACTION.
If ACTION is not set it will be selected from `lsp-code-actions-at-point'.
Request codeAction/resolve for more info if server supports."
    (interactive (list (lsp--select-action (lsp-code-actions-this-line))))
    (if (and (lsp-feature? "codeAction/resolve")
             (not command?)
             (not edit?))
        (lsp--execute-code-action (lsp-request "codeAction/resolve" action))
      (lsp--execute-code-action action)))

  (define-key lsp-mode-map (kbd "C-c r") 'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c a") 'lsp-avy-lens)
  (define-key lsp-mode-map (kbd "C-c i") 'lsp-organize-imports)
  (define-key lsp-mode-map (kbd "C-c w r") 'my-lsp-workspace-restart)
  (define-key lsp-mode-map (kbd "C-c w a") 'lsp-workspace-folders-add)
  (define-key lsp-mode-map (kbd "C-c w d") 'lsp-workspace-folders-remove)
  (define-key lsp-mode-map (kbd "C-c w c") 'lsp-workspace-shutdown)
  (define-key lsp-mode-map (kbd "C-c y") 'dap-hydra)
  ;;(define-key lsp-mode-map (kbd "C-c v") 'lsp-ui-peek-find-implementation)
  (define-key lsp-mode-map (kbd "C-c v") 'lsp-find-implementation)
  (define-key lsp-mode-map (kbd "C-c h") 'lsp-ui-doc-glance)
  (define-key lsp-mode-map (kbd "C-c w e") 'lsp-ui-flycheck-list)
  (define-key lsp-mode-map (kbd "C-c e") 'flycheck-list-errors)
  (define-key lsp-mode-map (kbd "C-c f") 'lsp-execute-code-action-this-line)
  (define-key lsp-mode-map (kbd "C-c s a") 'lsp-signature-activate)
  (define-key lsp-mode-map (kbd "C-c C-f") 'my-lsp-format)
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
;;(use-package posframe)
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

;; multi-root
(advice-add 'lsp :before (lambda (&rest _args)
                           (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

;; format
(defvar-local lsp-enable-format-at-save t)
(defun lsp-enable-format ()
  (interactive)
  (whitespace-cleanup-mode 1)
  (add-hook 'before-save-hook 'lsp-format-buffer nil 'lsp-format)
  (setq-local lsp-enable-format-at-save t))

(defun lsp-disable-format ()
  (interactive)
  (whitespace-cleanup-mode 0)
  (remove-hook 'before-save-hook 'lsp-format-buffer 'lsp-format)
  (setq-local lsp-enable-format-at-save nil))

(defun lsp-disable-format-project()
  (interactive)
  (when-let ((project-root (projectile-project-root))
             (file (format "%s.dir-locals.el" project-root)))
    (write-region (format "((%s . ((eval . (lsp-disable-format)))))" major-mode) nil file)
    (message (format "write %s" file))))

;; tramp
;;(ignore-tramp-ssh-control-master 'lsp)
(with-eval-after-load 'lsp-mode
  (defun lsp-tramp-connection-fast (local-command)
    "Create LSP stdio connection named name.
LOCAL-COMMAND is either list of strings, string or function which
returns the command to execute."
    (defvar tramp-connection-properties)
    (if (tramp-tramp-file-p default-directory)
        (add-to-list 'tramp-connection-properties
                     (list (regexp-quote (file-remote-p default-directory))
                           "direct-async-process" t)))
    (list :connect (lambda (filter sentinel name environment-fn _workspace)
                     (let* ((final-command (lsp-resolve-final-function
                                            local-command))
                            (process-name (generate-new-buffer-name name))
                            (process-environment
                             (lsp--compute-process-environment environment-fn))
                            (proc (make-process
                                   :name process-name
                                   :buffer (format "*%s*" process-name)
                                   :command final-command
                                   :connection-type 'pipe
                                   :coding 'no-conversion
                                   :noquery t
                                   :filter filter
                                   :sentinel sentinel
                                   :stderr (get-buffer-create (format "*%s::stderr*" process-name))
                                   :file-handler t)))
                       (cons proc proc)))
          ;;:test? (lambda() t)
          :test? (lambda () (-> local-command lsp-resolve-final-function lsp-server-present?))
          )))


;; ;; update lsp-symbol every two seconds
(setq lsp-modeline-symbol-running nil)
(defun lsp-enable-modeline-symbol()
  (unless lsp-modeline-symbol-running
    (setq lsp-modeline-symbol-running t)
    (run-at-time 2 2 (lambda ()
                       (setq lsp-modeline-symbol (lsp-modeline-get-symbol-name))))))
;; hook
(defun my-lsp-mode-hook ()
  (lsp-enable-modeline-symbol)
  (make-local-variable 'markdown-header-face-2)
  (set-face-attribute 'markdown-header-face-2 nil :height 1.0)
  (if lsp-enable-format-at-save
      (lsp-enable-format)))

(defun my-lsp-completion-mode-hook()
  (setq-local completion-category-defaults nil)
  (setq-local completion-at-point-functions
              (list
               (cape-capf-buster
                (cape-super-capf
                 (cape-company-to-capf #'company-files)
                 #'lsp-completion-at-point
                 )
                'equal))))

(add-hook 'lsp-mode-hook #'my-lsp-mode-hook)
(add-hook 'lsp-completion-mode-hook 'my-lsp-completion-mode-hook)

;; lsp-ui
(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-sideline-update-mode 'line
        lsp-ui-doc-enable nil
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-use-webkit nil
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-imenu-enable nil
        lsp-ui-peek-enable t
        lsp-ui-sideline-delay 0.2)
  (if (theme-dark-p)
      (set-face-foreground 'lsp-ui-sideline-code-action "MediumPurple1")
    (set-face-foreground 'lsp-ui-sideline-code-action "MediumPurple4")))
;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-treemacs)

;; lsp-later
(defvar-local lsp-later-timer nil)
(defun my-lsp-workspace-restart()
  (interactive)
  (if lsp-later-timer
      (progn
        (cancel-timer lsp-later-timer)
        (lsp-later-run))
    (call-interactively 'lsp-workspace-restart)))

(defun lsp-later-run ()
  (setq lsp-later-timer nil)
  (lsp))

(defun lsp-later()
  (let ((buf (current-buffer)))
    (setq lsp-later-timer
          (run-at-time 3 nil
                       (lambda ()
                         (with-current-buffer buf
                           (lsp-later-run)))))))

;;(add-hook 'doom-switch-buffer-hook #'lsp-try-reconnect nil 'local)

(add-hook 'hack-local-variables-hook
          (lambda ()
            (when lsp-later-timer
              (cancel-timer lsp-later-timer)
              (lsp-later-run))))


(with-eval-after-load 'lsp-mode
  (defun lsp-modeline-get-symbol-name ()
    "Get the symbol under cursor ."
    (if (and (bound-and-true-p lsp-mode) (lsp-feature? "textDocument/documentSymbol"))
        (-if-let* ((lsp--document-symbols-request-async t)
                   (symbols (lsp--get-document-symbols))
                   (symbols-hierarchy (lsp--symbols->document-symbols-hierarchy symbols)))
            ;;(concat " => " (gethash "name" (car (last symbols-hierarchy))))
            (concat " => "
                    (mapconcat
                     (lambda (symbol)
                       (gethash "name" symbol))
                     symbols-hierarchy "/"))
          "")
      "")))

(defun lsp-enable-log-io()
  (interactive)
  (setq lsp-log-io t)
  (my-lsp-workspace-restart))

(defun lsp-disable-log-io()
  (interactive)
  (setq lsp-log-io nil)
  (my-lsp-workspace-restart))

(provide 'init-lsp)
