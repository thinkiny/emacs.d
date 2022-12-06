(use-package eldoc
  :config
  (setq eldoc-echo-area-use-multiline-p 0.5))

(use-package eldoc-box
  :after eldoc
  :demand t
  :config
  (setq eldoc-box-clear-with-C-g t)
  (setq eldoc-box-only-multi-line nil))

(provide 'init-eldoc)
