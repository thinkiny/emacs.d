(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :bind (:map flycheck-mode-map ("C-c e" . #'flycheck-list-errors))
  :config
  (setq flycheck-idle-change-delay 0.2
        flycheck-indication-mode 'left-fringe
        flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  (dolist (checker '(emacs-lisp-checkdoc go-build))
    (setq flycheck-checkers (delq checker flycheck-checkers)))

  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.30))))

(provide 'init-flycheck)
