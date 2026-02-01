;;; init-ai.el --- init-ai -*- lexical-binding: t -*-

;; (use-package aidermacs
;;   :bind (:map global-map
;;               ("C-c y" . aidermacs-transient-menu))
;;   :config
;;   (aidermacs-setup-minor-mode)
;;   (setq aidermacs-default-chat-mode 'architect)
;;   (setq aidermacs-backend 'vterm))

;; claude-remote
;; #!/bin/bash
;;
;; if [[ -n "$CLAUDE_CODE_SSE_PORT" ]]; then
;;   lock=~/.claude/ide/$CLAUDE_CODE_SSE_PORT.lock
;;   if [[ -f $lock ]]; then
;;     sed -ri 's/"pid":[0-9]+/"pid":'$$'/' $lock
;;     # Use exec so claude inherits the shell's PID
;;     exec claude-chill -a 0 -- claude "$@"
;;   else
;;     exec claude "$@"
;;   fi
;; else
;;   exec claude "$@"
;; fi

;; claude-local
;; #!/bin/zsh -l
;;
;; if [ -n "$ANTHROPIC_BASE_URL" ]; then
;;    exec claude-chill -a 0 -- claude "$@"
;; else
;;    eval "$(ccr activate)"
;;    exec claude-chill -a 0 -- claude "$@"
;; fi

(use-package claude-code-ide
  :ensure t
  :vc (:url "https://github.com/thinkiny/claude-code-ide.el" :branch "tramp")
  :bind (:map global-map
              ("C-c y" . claude-code-ide-menu))
  :config
  ;; (setq claude-code-ide-debug t)
  (setq claude-code-ide-terminal-initialization-delay 1)
  (setq claude-code-ide-terminal-backend 'vterm)
  (setq claude-code-ide-prevent-reflow-glitch nil) ;; use claude-chill
  (setq claude-code-ide-vterm-anti-flicker nil) ;; use vterm-anti-flicker-filter-enable
  (setq claude-code-ide-use-side-window nil)
  (add-to-list 'display-buffer-alist
               '("\\*claude-code*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.5)))
  (setq claude-code-ide-show-claude-window-in-ediff nil)
  (setq claude-code-ide-window-side 'right
        claude-code-ide-window-width 80)
  (setq claude-code-ide-diagnostics-backend 'flymake)
  (claude-code-ide-emacs-tools-setup))

(use-package agent-shell
  :bind (:map global-map
              ("C-c s a" . agent-shell-cursor-start-agent))
  :config
  (setq agent-shell-header-style 'text)
  (setq agent-shell-show-welcome-message nil)
  (setq agent-shell-cursor-environment
        (agent-shell-make-environment-variables
         "HTTP_PROXY" "http://127.0.0.1:1087"
         "HTTPS_PROXY" "http://127.0.0.1:1087"
         :inherit-env t
         )))

(provide 'init-ai)
