(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-idle-change-delay 0.2
        flycheck-indication-mode 'left-fringe
        flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (set-face-attribute 'flycheck-warning nil :background nil)
  (set-face-attribute 'flycheck-error nil :background nil))
(provide 'init-flycheck)
