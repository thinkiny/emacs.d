(add-to-list 'display-buffer-alist
               `(,(rx bos "*Flymake diagnostics")
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.30)))


(defun my-flymake-mode-hook()
  ;;(setq flymake-show-diagnostics-at-end-of-line 'short)
  (sideline-mode)
  (define-key flymake-mode-map (kbd "C-c e") #'flymake-show-buffer-diagnostics)
  (define-key flymake-mode-map (kbd "C-c e") #'flymake-show-project-diagnostics)
  (remove-hook 'eldoc-documentation-functions #'flymake-eldoc-function t))

(add-hook 'flymake-mode-hook #'my-flymake-mode-hook)
(add-hook 'prog-mode-hook #'flymake-mode)

(provide 'init-flymake)
