(use-package helm
  :init
  (add-hook 'helm-before-initialize-hook
            (lambda ()
              (add-to-list 'helm-boring-buffer-regexp-list "\\*metals")
              (add-to-list 'helm-boring-buffer-regexp-list "\\*xwidget")
              (add-to-list 'helm-boring-buffer-regexp-list "\\*dash")
              (add-to-list 'helm-boring-buffer-regexp-list "\\*lsp-log")
              (add-to-list 'helm-boring-buffer-regexp-list "\\*gopls")
              (add-to-list 'helm-boring-buffer-regexp-list "\\*jdtls")
              (add-to-list 'helm-boring-buffer-regexp-list "\\*Flycheck")
              (add-to-list 'helm-boring-buffer-regexp-list "\\*ccls")
              (add-to-list 'helm-boring-buffer-regexp-list "\\*pdf-scroll-log\\*")))
  :config
  (require 'helm-config)
  (setq helm-input-idle-delay 0.5)
  (setq helm-cycle-resume-delay 0.2)
  (setq helm-follow-input-idle-delay 0.3)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  ;;(global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-c n") 'helm-cycle-resume)

  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)

  (setq helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-follow-mode-persistent t
        helm-scroll-amount 8
        helm-candidate-number-limit 40
        helm-ff-file-name-history-use-recentf t)
  (helm-autoresize-mode 1))

(use-package helm-projectile
  :after helm
  :init (helm-projectile-on))

(use-package helm-ag
  :after helm
  :config (setq helm-ag-use-agignore t))

(use-package helm-xref :after helm)

(add-hook 'after-init-hook
          (lambda ()
            (helm-mode 1)
            (diminish 'helm-mode)))

(global-set-key (kbd "C-c s") 'helm-do-ag)
(global-set-key (kbd "C-c l") 'helm-imenu)

(provide 'init-helm)
