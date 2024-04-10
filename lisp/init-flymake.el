(add-to-list 'display-buffer-alist
               `(,(rx bos "*Flymake diagnostics")
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.30)))

(with-eval-after-load 'flymake-mode
  (easy-menu-define nil flymake-mode-map nil
    (list "Flymake" :visible nil)))

(defun my-flymake-mode-hook()
  ;;(setq flymake-show-diagnostics-at-end-of-line 'short)
  (define-key flymake-mode-map (kbd "C-c e l") #'flymake-show-buffer-diagnostics)
  (define-key flymake-mode-map (kbd "C-c e p") #'flymake-show-project-diagnostics)
  (define-key flymake-mode-map (kbd "C-c e n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c e b") #'flymake-goto-prev-error)
  (remove-hook 'eldoc-documentation-functions #'flymake-eldoc-function t))

(add-hook 'flymake-mode-hook #'my-flymake-mode-hook)
(add-hook 'prog-mode-hook #'flymake-mode)

(provide 'init-flymake)
