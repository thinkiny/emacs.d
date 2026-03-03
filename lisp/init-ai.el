;;; init-ai.el --- init-ai -*- lexical-binding: t -*-

(defconst use-claude-code-chill nil)

(use-package claude-code-ide
  :ensure t
  :vc (:url "https://github.com/thinkiny/claude-code-ide.el" :branch "tramp")
  :bind (:map global-map
              ("C-c y" . claude-code-ide-menu))
  :config
  ;; (setq claude-code-ide-debug t)
  (setq claude-code-ide-emacs-prompt
"# Constraints
- Examine Emacs MCP tools description carefully.
- Prioritize Emacs MCP tools over all built-in tools.
- Avoid reading binary files like PDFs.
# Coordinate system
- Lines: 1-based (Line 1 = first line).
- Columns: 0-based (Column 0 = first column).
")
  (setq claude-code-ide-terminal-initialization-delay 1)
  (setq claude-code-ide-terminal-backend 'vterm)
  (setq claude-code-ide-prevent-reflow-glitch (not use-claude-code-chill))
  (setq claude-code-ide-show-claude-window-in-ediff nil)
  (setq claude-code-ide-vterm-render-delay 0.03)
  (setq claude-code-ide-window-side 'right
        claude-code-ide-window-width 80)
  (setq claude-code-ide-diagnostics-backend 'flymake)
  (claude-code-ide-emacs-tools-setup)
  (require 'claude-extra-mcp-tools)
  (claude-extra-mcp-tools-setup)

  (setq claude-code-ide-use-side-window nil)
  (defun claude-code--display-buffer (buffer alist)
    (delete-other-windows)
    (display-buffer-in-direction buffer alist))
  (add-to-list 'display-buffer-alist
               '("\\*claude-code*"
                 (display-buffer-use-some-frame
                  claude-code--display-buffer)
                 (direction . right)
                 (window-width . 0.5)
                 )))

(use-package agent-shell
  :bind (:map global-map
              ("C-c s a" . agent-shell))
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
