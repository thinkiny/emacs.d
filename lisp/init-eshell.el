(when *use-helm*
  (use-package helm-switch-shell
    :after helm
    :demand t
    :bind (("C-x t" . 'helm-switch-shell))
    :config
    (require 'helm-elisp)
    (set-face-attribute 'helm-lisp-show-completion nil :background nil)))

(use-package eshell-up
  :commands eshell-up eshell-up-peek
  :config
  (setq eshell-up-ignore-case nil))

(after-load 'eshell
  (server-start)
  (require 'eshell-company-patch)
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

(after-load 'em-prompt
  (set-face-attribute 'eshell-prompt nil :foreground "DeepSkyBlue4"))

(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "TERM" "term")
            (setenv "PAGER" "cat")
            (setenv "GIT_EDITOR" "emacsclient")
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map (kbd "C-a") 'eshell-maybe-bol)
            (when *use-helm*
              (setq-local helm-show-completion-display-function 'helm-default-display-buffer)
              (define-key eshell-mode-map [remap completion-at-point] 'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history))))

(provide 'init-eshell)
