;;; init-ai.el --- init-ai -*- lexical-binding: t -*-

(use-package claude-code-ide
  :ensure t
  :vc (:url "https://github.com/thinkiny/claude-code-ide.el" :rev "mine")
  :bind (:map global-map
              ("C-c y" . claude-code-ide-menu))
  :config
  (setq claude-code-ide-debug nil)
  (setq claude-code-ide-cli-debug nil)
  (setq claude-code-ide-emacs-prompt
"<IMPORTANT>
- Review MCP tools descriptions thoroughly. Prioritize MCP tools over any other tools or agents.
- Avoid reading any binary file (e.g., PDF or EPUB).
- Line numbers are 1-based; column numbers are 0-based.
</IMPORTANT>")

  (setq claude-code-ide-terminal-initialization-delay 1)
  (setq claude-code-ide-terminal-backend 'ghostel)
  (setq claude-code-ide-show-claude-window-in-ediff nil)
  (setq claude-code-ide-mcp-selection-delay 0.2)
  (setq claude-code-ide-mcp-initial-notification-delay 0.5)
  (setq claude-code-ide-vterm-render-delay 0.02)
  (setq claude-code-ide-window-side 'right
        claude-code-ide-window-width 80
        claude-code-ide-use-side-window nil)
  (claude-code-ide-emacs-tools-setup)

  ;; add more tools
  (require 'claude-extra-mcp-tools)
  (claude-extra-mcp-tools-setup)

  (add-to-list 'display-buffer-alist
               '("\\*claude-code"
                 (claude-code--display-buffer)
                 (direction . right)
                 (window-width . 0.5))))

(defun claude-code--display-buffer (buffer alist)
  (delete-other-windows)
  (display-buffer-in-direction buffer alist))

(use-package agent-shell
  :bind (:map global-map
              ("C-c s a" . agent-shell))
  :config
  (setq agent-shell-header-style 'text)
  (setq agent-shell-show-config-icons nil)
  (setq agent-shell-show-welcome-message nil)
  (let ((proxy-env-args
         (mapcan (lambda (pair)
                   (list (car pair) (cdr pair)))
                 (local-proxy-env-alist))))
    (setq agent-shell-cursor-environment
          (apply #'agent-shell-make-environment-variables
                 (append proxy-env-args '(:inherit-env t))))))

(provide 'init-ai)
