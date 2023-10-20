(add-to-list 'load-path "~/.emacs.d/lsp-bridge")

(require 'lsp-bridge)

(setq lsp-bridge-enable-with-tramp t)

(setq acm-enable-tabnine nil)

(defun lsp-bridge-code-action-override()
  (interactive)
  (lsp-bridge-code-action "source.overrideMethods"))

(defun lsp-bridge-code-action-organize-imports()
  (interactive)
  (lsp-bridge-code-action "source.organizeImports"))

(defun lsp-bridge-code-action-quickfix()
  (interactive)
  (lsp-bridge-code-action "quickfix"))

(setq lsp-bridge-completion-hide-characters nil)
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
  (define-key lsp-bridge-mode-map (kbd "M-.") 'lsp-bridge-find-def)
  (define-key lsp-bridge-mode-map (kbd "M-[") 'lsp-bridge-find-def-return)
  (define-key lsp-bridge-mode-map (kbd "M-,") 'lsp-bridge-find-references))

(add-hook 'after-init-hook #'global-lsp-bridge-mode)

(provide 'init-lsp-bridge)
