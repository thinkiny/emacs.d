(use-package god-mode
  :config
  (setq god-mode-enable-function-key-translation nil)
  (define-key god-local-mode-map (kbd ".") #'xref-find-definitions)
  (define-key god-local-mode-map (kbd ",") #'xref-find-references)
  (define-key god-local-mode-map (kbd "g") #'goto-workspace-symbol)
  (define-key god-local-mode-map (kbd "l") #'counsel-imenu)
  (define-key god-local-mode-map (kbd "q") #'kill-current-buffer)
  (define-key god-local-mode-map (kbd "[") #'xref-pop-curr-marker-stack)
  (define-key god-local-mode-map (kbd "i") #'god-local-mode)
  (add-to-list 'god-exempt-major-modes 'ivy-occur-mode)
  ;;(remove-hook 'after-change-major-mode-hook 'god-mode-maybe-activate)
  )

(defun goto-workspace-symbol()
  (interactive)
  (if (derived-mode-p 'lsp-mode)
      (lsp-ivy--workspace-symbol)
    (gtags-workspace-symbol)))

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
