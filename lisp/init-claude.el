;;; init-claude.el --- init-claude -*- lexical-binding: t -*-

(use-package claude-code-ide :ensure t
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind (:map global-map
              ("C-c y" . claude-code-ide-menu))
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-diagnostics-backend 'flymake)
  (setq claude-code-ide-window-side 'bottom
        claude-code-ide-window-height 10)
  ;; (setenv "ANTHROPIC_BASE_URL" "https://api.moonshot.cn/anthropic/")
  ;; (setenv "ANTHROPIC_MODEL" "kimi-k2-turbo-preview")
  ;; (setenv "ANTHROPIC_SMALL_FAST_MODEL" "kimi-k2-turbo-preview")
  )

(provide 'init-claude)
