;;; init-aider.el --- init-aider -*- lexical-binding: t -*-

(use-package aidermacs
  :bind (:map global-map
              ("C-c y" . aidermacs-transient-menu))
  :config
  (aidermacs-setup-minor-mode)
  (setq aidermacs-use-architect-mode t)
  (setq aidermacs-backend 'vterm))

(provide 'init-aider)
