;;; init-whitespace.el --- Special handling for whitespace -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default show-trailing-whitespace nil)


;;; Whitespace

(defun sanityinc/show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t)
  (unless (file-remote-p default-directory)
    (whitespace-cleanup-mode)))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook maven-pom-mode shell-script-mode snippet-mode))
  (add-hook hook 'sanityinc/show-trailing-whitespace))

(require-package 'whitespace-cleanup-mode)

(setq whitespace-cleanup-mode-only-if-initially-clean nil)
(global-set-key [remap just-one-space] 'cycle-spacing)


(provide 'init-whitespace)
;;; init-whitespace.el ends here
