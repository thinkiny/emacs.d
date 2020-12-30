(add-hook 'after-init-hook (lambda () (recentf-mode 1)))
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude `("/tmp/"
                   "/ssh:"
                   ,(expand-file-name "~/\\.emacs\\.d/elpa/")
                   ,(expand-file-name "~/\\.emacs\\.d/workspace/")
                   "/usr/include"
                   "/usr/local/include"))

(provide 'init-recentf)
