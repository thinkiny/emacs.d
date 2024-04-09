(add-to-list 'load-path "~/.emacs.d/lsp-bridge")

(setq lsp-bridge-enable-mode-line nil)

(require 'lsp-bridge)
(setq lsp-bridge-enable-hover-diagnostic t)
(setq lsp-bridge-python-lsp-server "pylsp")
(setq lsp-bridge-enable-auto-format-code t)

(setq lsp-bridge-enable-with-tramp t)
(setq lsp-bridge-remote-start-automatically nil)

;; (setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)

(setq acm-enable-doc nil)
(setq acm-enable-tabnine nil)
(setq acm-backend-yas-match-by-trigger-keyword t)

(defun lsp-bridge-code-action-override()
  (interactive)
  (lsp-bridge-code-action "source.overrideMethods"))

(defun lsp-bridge-code-action-organize-imports()
  (interactive)
  (lsp-bridge-code-action "source.organizeImports"))

(defun lsp-bridge-code-action-quickfix()
  (interactive)
  (lsp-bridge-code-action "quickfix"))

;; (advice-add #'lsp-bridge-find-def :before #'xref-push-marker-stack-once)


(defun lsp-bridge-find-def-xref ()
  (interactive)
  (cond
   ((lsp-bridge-has-lsp-server-p)
    (setq-local lsp-bridge-jump-to-def-in-other-window nil)
    (xref-push-marker-stack)
    (lsp-bridge-call-file-api "find_define" (lsp-bridge--position)))
   (t
    (call-interactively 'xref-find-definitions))))

(defun lsp-bridge-find-references-xref ()
  (interactive)
  (cond
   ((lsp-bridge-has-lsp-server-p)
    (xref-push-marker-stack)
    (lsp-bridge-call-file-api "find_references" (lsp-bridge--position)))
   (t
    (call-interactively #'xref-find-references))))

;; (setq lsp-bridge-completion-hide-characters nil)

(with-eval-after-load 'lsp-bridge
  (setq lsp-bridge-completion-popup-predicates (delete 'lsp-bridge-not-match-hide-characters lsp-bridge-completion-popup-predicates))
  (define-key lsp-bridge-mode-map (kbd "M-/") 'lsp-bridge-popup-complete-menu)
  (define-key lsp-bridge-mode-map (kbd "C-c r") 'lsp-bridge-rename)
  (define-key lsp-bridge-mode-map (kbd "C-c o") 'lsp-bridge-code-action-override)
  (define-key lsp-bridge-mode-map (kbd "C-c i") 'lsp-bridge-code-action-organize-imports)
  (define-key lsp-bridge-mode-map (kbd "C-c e") 'lsp-bridge-diagnostic-list)
  (define-key lsp-bridge-mode-map (kbd "C-c h") 'lsp-bridge-popup-documentation)
  (define-key lsp-bridge-mode-map (kbd "C-c w r") 'lsp-bridge-restart-process)
  (define-key lsp-bridge-mode-map (kbd "C-c w s") 'lsp-bridge-workspace-list-symbols)
  (define-key lsp-bridge-mode-map (kbd "C-c v") 'lsp-bridge-find-impl)
  (define-key lsp-bridge-mode-map (kbd "C-c f") 'lsp-bridge-code-action-quickfix)
  (define-key lsp-bridge-mode-map (kbd "C-c a") 'lsp-bridge-code-action)
  (define-key lsp-bridge-mode-map (kbd "M-.") 'lsp-bridge-find-def-xref)
  ;;(define-key lsp-bridge-mode-map (kbd "M-[") 'lsp-bridge-find-def-return-xref)
  (define-key lsp-bridge-mode-map (kbd "M-,") 'lsp-bridge-find-references-xref))

(add-hook 'after-init-hook #'global-lsp-bridge-mode)

(provide 'init-lsp-bridge)
