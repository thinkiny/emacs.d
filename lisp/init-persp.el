(use-package perspective
  :bind (("C-x p k" . persp-kill-buffer*)
         ("C-x p d" . persp-remove-buffer))
  :custom
  (persp-mode-prefix-key (kbd "C-x p"))
  :config
  (setq persp-state-default-file (expand-file-name "persp.state" user-emacs-directory)))

(require 'perspective)
(persp-mode)
(add-hook 'kill-emacs-hook #'persp-state-save)

(provide 'init-persp)