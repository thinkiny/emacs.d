(use-package perspective
  :init
  (setq persp-show-modestring nil)
  (persp-mode)
  :bind (("C-x p d" . persp-remove-buffer)
         ("C-x p b" . persp-ivy-switch-buffer)
         ("C-x p k" . persp-kill-all-buffers)
         ("C-x p o" . persp-kill-other-buffers))
  :custom
  (persp-mode-prefix-key (kbd "C-x p"))
  :config
  (defun persp-kill-all-buffers ()
    "Kill all buffers in the current perspective excludes the perspective's scratch buffer."
    (interactive)
    (when (y-or-n-p "Are you sure you want to kill all buffers in the current perspective ? ")
      (cl-loop for buf in (persp-current-buffers)
               unless (eq buf (get-buffer (persp-scratch-buffer)))
               do (kill-buffer buf))))

  (setq persp-modestring-short t)
  (setq persp-state-default-file (expand-file-name "persp.state" user-emacs-directory))
  (after-load-theme
   (set-face-attribute 'persp-selected-face nil :foreground (face-attribute 'mode-line :foreground))))

(provide 'init-persp)
