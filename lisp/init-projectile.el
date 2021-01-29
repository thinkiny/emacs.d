(use-package projectile
  :config
  (add-to-list 'projectile-globally-ignored-directories ".metals")
  (add-to-list 'projectile-globally-ignored-directories ".bloop")
  (add-to-list 'projectile-globally-ignored-directories ".settings")
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-command-map (kbd "K") #'projectile-kill-not-project-buffers)
  (if *use-helm*
      (setq projectile-completion-system 'helm))
  (if *use-ivy*
      (setq projectile-completion-system 'ivy))
  (projectile-mode)
  (diminish 'projectile-mode))

(defun projectile-kill-not-project-buffers ()
  "Kill buffers not belongs to this project including dired-mode buffer"
  (interactive)
  (let* ((project (projectile-ensure-project (projectile-project-root)))
         (project-name (projectile-project-name project))
         (buffers (cl-remove-if
                   (lambda (buffer)
                     (projectile-project-buffer-p buffer project))
                   (buffer-list))))
    (progn
      (dolist (buffer buffers)
        (if (not (string-match "^\*" (string-trim-left (buffer-name buffer))))
            (kill-buffer buffer))))
    (message (format "Killed buffers not belongs to %s" project-name))))


(use-package ag
  :config
  (set-face-attribute 'ag-hit-face nil :inherit 'highlight)
  (setq ag-reuse-buffers t)
  (setq ag-group-matches nil)
  (setq ag-arguments (delete "--stats" ag-arguments))
  (setq compilation-scroll-output 'first-error)
  (add-hook 'ag-search-finished-hook
            (lambda ()
              (pop-to-buffer next-error-last-buffer))))

(provide 'init-projectile)
