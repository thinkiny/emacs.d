(use-package projectile
  :config
  (add-to-list 'projectile-globally-ignored-directories ".metals")
  (add-to-list 'projectile-globally-ignored-directories ".bloop")
  (add-to-list 'projectile-globally-ignored-directories ".settings")
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-command-map (kbd "K") #'projectile-kill-not-project-buffers)
  (define-key projectile-command-map (kbd "0") #'projectile-kill-no-files)
  (define-key projectile-command-map (kbd "u") #'projectile-revert-project-buffers)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root nil)
  (setq projectile-indexing-method 'native)
  (setq projectile-show-menu nil)
  (projectile-mode)

  ;; (defadvice projectile-project-root (around ignore-remote first activate)
  ;;   (unless (file-remote-p default-directory) ad-do-it))

  (require 'project-projectile)
  (add-hook 'project-find-functions #'project-projectile 'append)

  ;; don't use file-truename
  (ignore-file-truename 'projectile-project-root 'projectile-project-buffer-p)
  (unbind-key (kbd "C-c p t") 'projectile-mode-map)
  (diminish 'projectile-mode)

  ;; delete directory is too slow
  (add-hook 'projectile-mode-hook (lambda ()
                                    (advice-remove 'delete-file #'delete-file-projectile-remove-from-cache))))

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


(defun projectile-revert-project-buffers ()
  "Revert buffers belongs to this project"
  (interactive)
  (let* ((project (projectile-ensure-project (projectile-project-root)))
         (project-name (projectile-project-name project))
         (buffers (cl-remove-if
                   (lambda (buffer)
                     (not (projectile-project-buffer-p buffer project)))
                   (buffer-list))))
    (progn
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (if (buffer-file-name)
              (revert-buffer nil t)))))
    (message (format "Revert buffers for %s" project-name))))

(defun projectile-kill-no-files ()
  "Kill buffers not belongs to this project including dired-mode buffer"
  (interactive)
  (let* ((project (projectile-ensure-project (projectile-project-root)))
         (project-name (projectile-project-name project))
         (buffers (seq-filter
                   (lambda (buffer)
                     (and (projectile-project-buffer-p buffer project)
                          (not (buffer-file-name))))
                   (buffer-list))))
    (progn
      (dolist (buffer buffers)
        (unless (buffer-file-name buffer)
            (kill-buffer buffer)))))
    (message "Killed buffers not having files associcated"))


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
