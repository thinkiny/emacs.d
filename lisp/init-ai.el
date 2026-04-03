;;; init-ai.el --- init-ai -*- lexical-binding: t -*-

(use-package claude-code-ide
  :ensure t
  :vc (:url "https://github.com/thinkiny/claude-code-ide.el" :branch "mine")
  :bind (:map global-map
              ("C-c y" . claude-code-ide-menu))
  :config
  (setq claude-code-ide-debug nil)
  (setq claude-code-ide-cli-debug nil)
  (setq claude-code-ide-emacs-prompt
"# IMPORTANT
- Review Emacs MCP tool descriptions thoroughly.
- Prioritize Emacs MCP tools over any other tools or agents.
- Avoid reading any binary files (e.g., PDFs or EPUBs).
- Line numbers are 1-based; column numbers are 0-based.")

  (setq claude-code-ide-terminal-initialization-delay 1)
  (setq claude-code-ide-terminal-backend 'vterm)
  (setq claude-code-ide-show-claude-window-in-ediff nil)
  (setq claude-code-ide-mcp-selection-delay 0.2)
  (setq claude-code-ide-mcp-initial-notification-delay 0.5)
  (setq claude-code-ide-vterm-render-delay 0.05)
  (setq claude-code-ide-window-side 'right
        claude-code-ide-window-width 80)
  (claude-code-ide-emacs-tools-setup)
  (require 'claude-extra-mcp-tools)
  (claude-extra-mcp-tools-setup)
  (setq claude-code-ide-use-side-window nil))

(defun claude-code--display-buffer (buffer alist)
  (delete-other-windows)
  (display-buffer-in-direction buffer alist))

(add-to-list 'display-buffer-alist
             '("\\*claude-code*"
               (display-buffer-use-some-frame
                claude-code--display-buffer)
               (direction . right)
               (window-width . 0.5)))

(use-package agent-shell
  :bind (:map global-map
              ("C-c s a" . agent-shell))
  :config
  (setq agent-shell-header-style 'text)
  (setq agent-shell-show-config-icons nil)
  (setq agent-shell-show-welcome-message nil)
  (setq agent-shell-cursor-environment
        (agent-shell-make-environment-variables
         "HTTP_PROXY" "http://127.0.0.1:1087"
         "HTTPS_PROXY" "http://127.0.0.1:1087"
         :inherit-env t
         )))

(provide 'init-ai)
