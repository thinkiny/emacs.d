(use-package perspective
  :bind (("C-x p k" . persp-kill-buffer*)
         ("C-x p b" . persp-ivy-switch-buffer)
         ("C-x p d" . persp-remove-buffer)
         ("C-x p o" . persp-kill-other-buffers))
  :custom
  (persp-mode-prefix-key (kbd "C-x p"))
  :config
  (setq persp-modestring-short t)
  (setq persp-state-default-file (expand-file-name "persp.state" user-emacs-directory))
  (after-load-theme
   (set-face-attribute 'persp-selected-face nil :foreground (face-attribute 'mode-line :foreground))))

(require 'perspective)
(persp-mode)

(provide 'init-persp)
