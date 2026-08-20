;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defvar persp-shared-buffer-names '("*Messages*")
  "Buffer names that are members of every perspective.
`persp-pin-buffer' never pins them, so `persp-forget-foreign-buffers'
never removes them on switch.")

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
  (setq persp-state-default-file (expand-file-name "persp.state" user-emacs-directory))
  (persp-mode)
  (advice-add 'persp-new :around #'presp--set-home-dir)
  (advice-add 'persp-kill-buffer* :after #'persp--xwidget-ensure-single-window)
  (add-hook 'persp-switch-hook #'persp--tweak-buffers))


(defun persp-close-all-buffers ()
  "Kill all buffers in the current perspective excludes the perspective's scratch buffer."
  (interactive)
  (when (y-or-n-p "Are you sure you want to kill all buffers in the current perspective ? ")
    (cl-loop for buf in (persp-current-buffers)
             unless (eq buf (get-buffer (persp-scratch-buffer)))
             do (kill-buffer buf))))

(defun presp--set-home-dir (orig &rest args)
  (let ((default-directory "~/"))
    (apply orig args)))

(defun persp--xwidget-ensure-single-window (&optional _killed &rest _)
  "Move any non-selected window off a duplicate xwidget."
  (dolist (win (window-list))
    (when (and (with-current-buffer (window-buffer win)
                 (derived-mode-p 'xwidget-webkit-mode))
               (> (length (get-buffer-window-list (window-buffer win) nil t)) 1)
               (not (eq win (selected-window))))
      (with-selected-window win
        (switch-to-buffer (persp-get-scratch-buffer))))))

(defun persp--tweak-buffers (&rest _)
  (dolist (buffer-name persp-shared-buffer-names)
    (when-let* ((buffer (get-buffer buffer-name)))
      (persp-add-buffer buffer))))

(provide 'init-persp)
