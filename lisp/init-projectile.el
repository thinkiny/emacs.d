(use-package projectile
  :config
  (add-to-list 'projectile-globally-ignored-directories ".metals")
  (add-to-list 'projectile-globally-ignored-directories ".bloop")
  (add-to-list 'projectile-globally-ignored-directories ".settings")
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-command-map (kbd "K") #'projectile-kill-not-project-buffers)
  (setq projectile-completion-system 'ivy)
  (projectile-mode)

  ;; remove file-truename
  (defun projectile-project-root (&optional dir)
    (let ((dir (or dir default-directory)))
      (when (and (fboundp 'tramp-archive-file-name-archive)
                 (tramp-archive-file-name-p dir))
        (setq dir (file-name-directory (tramp-archive-file-name-archive dir))))
      (cl-subst nil 'none
                (or (let ((is-local (not (file-remote-p dir)))      ;; `true' if the file is local
                          (is-connected (file-remote-p dir nil t))) ;; `true' if the file is remote AND we are connected to the remote
                      (when (or is-local is-connected)
                        ;; Here is where all the magic happens.
                        ;; We run the functions in `projectile-project-root-functions' until we find a project dir.
                        (cl-some
                         (lambda (func)
                           (let* ((cache-key (format "%s-%s" func dir))
                                  (cache-value (gethash cache-key projectile-project-root-cache)))
                             (if (and cache-value (file-exists-p cache-value))
                                 cache-value
                               (let ((value (funcall func dir)))
                                 (puthash cache-key value projectile-project-root-cache)
                                 value))))
                         projectile-project-root-functions)))
                    'none))))
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
