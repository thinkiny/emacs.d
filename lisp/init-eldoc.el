(use-package eldoc
  :config
  (setq eldoc-echo-area-use-multiline-p 0.5))

(use-package eldoc-box
  :after eldoc
  :config
  (setq eldoc-box-clear-with-C-g t))

(provide 'init-eldoc)