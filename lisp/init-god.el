(use-package god-mode
  :demand t
  :config
  (setq god-mode-enable-function-key-translation nil)
  (define-key god-local-mode-map (kbd ".") #'repeat)
  (define-key god-local-mode-map (kbd "i") #'god-local-mode)
  (delete #'god-view-mode-p god-exempt-predicates))

(when window-system
  (defun my-god-mode-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'hollow
                      'box)))
  (add-hook 'god-mode-enabled-hook #'my-god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook #'my-god-mode-update-cursor))

(defun god-global-mode-report ()
  (if god-global-mode
      (message "God-Global mode enabled")
    (message "God-Global mode disabled")))

(advice-add #'god-mode-all :after #'god-global-mode-report)

(global-set-key (kbd "M-i") #'god-local-mode)
(global-set-key (kbd "M-'") #'god-mode-all)

(provide 'init-god)
