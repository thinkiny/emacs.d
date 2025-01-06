;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :demand t
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(with-eval-after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "JAVA_HOME"))
    (add-to-list 'exec-path-from-shell-variables var)))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
