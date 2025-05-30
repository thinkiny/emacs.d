(use-package eshell-up
  :commands eshell-up eshell-up-peek
  :config
  (setq eshell-up-ignore-case nil))

(with-eval-after-load 'eshell
  (add-to-list 'eshell-modules-list 'eshell-smart)
  (setq eshell-history-size 10000
        eshell-save-history-on-exit t
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t)

  (defun pcomplete/sudo ()
    (let ((prec (pcomplete-arg 'last -1)))
      (cond ((string= "sudo" prec)
             (while (pcomplete-here*
                     (funcall pcomplete-command-completion-function)
                     (pcomplete-arg 'last) t)))))))

;; (with-eval-after-load 'em-prompt
;;   (set-face-attribute 'eshell-prompt nil :foreground "DeepSkyBlue4"))

(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

(defun eshell/cdroot ()
  (when-let* ((project-root (projectile-project-root)))
    (eshell/cd project-root)))

(defun my-eshell-mode-hook()
  (setenv "TERM" "term")
  (setenv "PAGER" "cat")
  (setenv "GIT_EDITOR" "emacsclient")
  (eshell-cmpl-initialize)
  (define-key eshell-mode-map (kbd "C-a") 'eshell-maybe-bol))

(add-hook 'eshell-mode-hook #'my-eshell-mode-hook)

(provide 'init-eshell)
