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
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  ;;(global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-c c") 'helm-cycle-resume)

  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)

  (setq helm-split-window-in-side-p t
        helm-input-idle-delay 0.5
        helm-cycle-resume-delay 0
        helm-follow-input-idle-delay 0.3
        helm-move-to-line-cycle-in-source t
        helm-follow-mode-persistent t
        helm-scroll-amount 8
        helm-candidate-number-limit 40
        helm-ff-candidate-number-limit 500
        helm-ff-auto-update-initial-value t
        helm-ff-up-one-level-preselect nil
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

(use-package helm-themes)

(after-load 'helm
  (defun helm-follow-execute-persistent-action-maybe (&optional delay)
    "Execute persistent action in mode `helm-follow-mode'.

This happen after: DELAY or the 'follow-attr value of current
source or `helm-follow-input-idle-delay' or
`helm-input-idle-delay' secs."
    (let* ((src (helm-get-current-source))
           (curr-sel (helm-get-selection nil nil src)))
      (when (and (not (get-buffer-window helm-action-buffer 'visible))
                 (not (helm-pos-header-line-p))
                 (or (helm-follow-mode-p src)
                     (and helm-follow-mode-persistent
                          (member (assoc-default 'name src)
                                  helm-source-names-using-follow)))
                 (null (eq (assoc-default 'follow src) 'never))
                 curr-sel)
        (helm-follow-mode-set-source 1 src)
        (run-with-idle-timer helm-follow-input-idle-delay
                             nil
                             (lambda (curr-sel)
                               (when (and helm-alive-p (equal curr-sel (helm-get-selection)))
                                 (helm-execute-persistent-action)))
                             curr-sel)))))

(provide 'init-helm)
