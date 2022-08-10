(use-package sideline
  ;;:init
  ;; (setq sideline-backends-left '((sideline-blame . down)))
  ;; (setq sideline-backends-right '(sideline-lsp  sideline-flycheck))
  ;; (global-sideline-mode 1)
  )

(use-package sideline-lsp
  :after sideline
  :config
  (setq sideline-lsp-update-mode 'line)
  (setq sideline-lsp-ignore-duplicate t)
  (setq sideline-lsp-code-actions-prefix "")
  (with-eval-after-load 'sideline-lsp
   (if (is-custom-theme-dark)
       (set-face-foreground 'sideline-lsp-code-action "MediumPurple1")
     (set-face-foreground 'sideline-lsp-code-action "MediumPurple4"))))


(use-package sideline-flycheck
  :after sideline
  :config
  (add-hook 'flycheck-mode-hook 'sideline-flycheck-setup)
  )

(use-package sideline-blame
  :after sideline)

(defun enable-sideline-blame()
  (interactive)
  (setq sideline-backends-left '((sideline-blame . down))))

(defun disable-sideline-blame()
  (interactive)
  (setq sideline-backends-left nil))

(provide 'init-sideline)
