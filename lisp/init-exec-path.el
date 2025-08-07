;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :demand t
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (dolist (var '("SSH_AUTH_SOCK"
                 "SSH_AGENT_PID"
                 "GPG_AGENT_INFO"
                 "LANG"
                 "LC_CTYPE"
                 "JAVA_HOME"
                 "ANTHROPIC_BASE_URL"
                 "ANTHROPIC_AUTH_TOKEN"
                 "ANTHROPIC_MODEL"
                 "ANTHROPIC_SMALL_FAST_MODEL"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
