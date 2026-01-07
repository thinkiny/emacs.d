;; -*- lexical-binding: t; -*-

(use-package eldoc
  :config
  (setq eldoc-echo-area-use-multiline-p 0.5)
  (setq eldoc-idle-delay 1)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose))

(use-package eldoc-box
  :after eldoc
  :demand t
  :config
  (setq eldoc-box-clear-with-C-g t)
  (setq eldoc-box-only-multi-line nil))

(provide 'init-eldoc)
