;;; init-ai.el --- init-ai -*- lexical-binding: t -*-

;; (use-package aidermacs
;;   :bind (:map global-map
;;               ("C-c y" . aidermacs-transient-menu))
;;   :config
;;   (aidermacs-setup-minor-mode)
;;   (setq aidermacs-default-chat-mode 'architect)
;;   (setq aidermacs-backend 'vterm))

(use-package claude-code-ide :ensure t
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind (:map global-map
              ("C-c y c" . claude-code-ide-menu))
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-diagnostics-backend 'flymake)
  (setq claude-code-ide-window-side 'bottom
        claude-code-ide-window-height 10))

(use-package agent-shell
  :bind (:map global-map
              ("C-c y a" . agent-shell-cursor-start-agent))
  :config
  (setq agent-shell-header-style nil)
  (setq agent-shell-show-welcome-message nil)
  (setq agent-shell-cursor-environment
        (agent-shell-make-environment-variables
         "HTTP_PROXY" "http://127.0.0.1:1087"
         "HTTPS_PROXY" "http://127.0.0.1:1087")))

(provide 'init-ai)
