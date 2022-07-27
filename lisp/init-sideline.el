(use-package sideline
  :init
  (setq sideline-backends-left '((sideline-blame . down)))
  (setq sideline-backends-right '((sideline-lsp  . up)
                                  (sideline-flycheck . down)))
  (global-sideline-mode 1))

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
  :after sideline)

(use-package sideline-blame
  :after sideline)

(provide 'init-sideline)
