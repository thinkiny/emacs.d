;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(use-package perspective
  :init
  (setq persp-show-modestring nil)
  :bind ((:map persp-mode-map
               ("C-x p d" . persp-remove-buffer)
               ("C-x p m" . persp-set-buffer)
               ("C-x p c" . persp-close-all-buffers)
               ("C-x p k" . persp-kill)
               ("C-x p o" . persp-kill-other-buffers)))
  :custom
  (persp-mode-prefix-key (kbd "C-x p"))
  :config
  (global-set-key (kbd "C-x b") 'persp-ivy-switch-buffer)
  (global-set-key (kbd "C-x k") 'persp-kill-buffer*)
  (advice-add 'persp-new :around #'set-new-presp-dir-to-home)
  (advice-add 'persp-kill-buffer* :after #'xwidget-ensure-single-window)
  (setq persp-modestring-short t)
  (setq persp-state-default-file (expand-file-name "persp.state" user-emacs-directory))
  (after-load-theme
    (set-face-attribute 'persp-selected-face nil :foreground (face-attribute 'mode-line :foreground)))
  (persp-mode))

(defun persp-close-all-buffers ()
  "Kill all buffers in the current perspective excludes the perspective's scratch buffer."
  (interactive)
  (when (y-or-n-p "Are you sure you want to kill all buffers in the current perspective ? ")
    (cl-loop for buf in (persp-current-buffers)
             unless (eq buf (get-buffer (persp-scratch-buffer)))
             do (kill-buffer buf))))

(defun set-new-presp-dir-to-home (orig &rest args)
  (let ((default-directory "~/"))
    (apply orig args)))

(defun xwidget-ensure-single-window (&optional _killed &rest _)
  "Move any non-selected window off a duplicate xwidget.
An xwidget is a single native widget and cannot render in two windows;
if a kill left a window duplicating an xwidget shown elsewhere, switch
that window to the perspective's scratch buffer.  The window is kept,
matching normal `kill-buffer' behavior."
  (dolist (win (window-list))
    (when (and (with-current-buffer (window-buffer win)
                 (derived-mode-p 'xwidget-webkit-mode))
               (> (length (get-buffer-window-list (window-buffer win) nil t)) 1)
               (not (eq win (selected-window))))
      (with-selected-window win
        (switch-to-buffer (persp-get-scratch-buffer))))))

(provide 'init-persp)
