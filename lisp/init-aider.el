;;; init-aider.el --- init-aider -*- lexical-binding: t -*-

(use-package aidermacs
  :bind (:map global-map
              ("C-c y" . aidermacs-transient-menu))
  :config
  (aidermacs-setup-minor-mode)
  (setq aidermacs-default-chat-mode 'architect)
  (setq aidermacs-backend 'vterm))

(provide 'init-aider)
