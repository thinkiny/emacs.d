(use-package eldoc
  :config
  (setq eldoc-echo-area-use-multiline-p 0.5))

(use-package eldoc-box
  :after eldoc
  :config
  (setq eldoc-box-clear-with-C-g t)
  (setq eldoc-box-only-multi-line nil))

(defun eldoc-count-line(str)
  (cl-count-if
   (lambda (x)
     (not (string-prefix-p "```" x)))
   (split-string str "\n" t)))

(provide 'init-eldoc)
