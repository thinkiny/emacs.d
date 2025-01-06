(use-package projectile
  :config
  (setq projectile-globally-ignored-directories
        '("*node_modules$"
          ".bloop"
          ".metals"
          ".bsp"
          "*.github"
          ".clangd"
          "^\\.idea$"
          "^\\.vscode$"
          "*\\.cache$"
          "*\\.git$"
          "*\\.svn$"
          "*\\.hg$"
          "*\\.bzr$"
          "aarch64-darwin"
          "build"
          "target"
          "vendor"
          ))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-command-map (kbd "K") #'projectile-kill-not-project-buffers)
  (define-key projectile-command-map (kbd "0") #'projectile-kill-no-files)
  (define-key projectile-command-map (kbd "u") #'projectile-revert-project-buffers)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  ;;(setq projectile-auto-update-cache nil)
  (setq projectile-dynamic-mode-line nil)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-show-menu nil)
  (setq projectile-file-exists-remote-cache-expire nil)
  (projectile-mode)

  ;; (defun need_find_project()
  ;;   (if (file-remote-p default-directory)
  ;;       (with-parsed-tramp-file-name default-directory dir
  ;;         (s-starts-with? "/home/" dir-localname))
  ;;     t))

  ;; don't use file-truename
  (ignore-file-truename 'projectile-project-root 'projectile-project-buffer-p)
  (unbind-key (kbd "C-c p t") 'projectile-mode-map)
  (diminish 'projectile-mode)

  (add-hook 'projectile-mode-hook
            (lambda ()
              ;; delete directory is too slow
              (advice-remove 'delete-file #'delete-file-projectile-remove-from-cache)
              )))

;; ignore homedir
(defun my-projectile-ignore-homedir (dir)
  (if (and dir (equal dir (expand-file-name "~/")))
      nil
    dir))

(advice-add 'projectile-locate-dominating-file :filter-return #'my-projectile-ignore-homedir)

;; (defun my-project-ignore-homedir (project)
;;   (if (and project
;;            (equal (expand-file-name (nth 2 project)) (expand-file-name "~/")))
;;       nil
;;     project))
;; (advice-add 'project-try-vc :filter-return #'my-project-ignore-homedir)


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
