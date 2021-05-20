;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(after-load 'dired
  (require 'ls-lisp)
  (require 'dired-x)
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq dired-recursive-deletes 'top)
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|\\.cmd$\\|\\.d$")
  (setq dired-omit-verbose nil)
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

(provide 'init-dired)
;;; init-dired.el ends here
