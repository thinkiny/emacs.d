;; -*- lexical-binding: t; -*-

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-globally-ignored-directories
        (append projectile-globally-ignored-directories
                '("node_modules" ".bloop" ".metals" ".bsp"
                  ".github" ".venv" "target" "vendor")))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-command-map (kbd "K") #'projectile-kill-not-project-buffers)
  (define-key projectile-command-map (kbd "0") #'projectile-kill-no-files)
  (define-key projectile-command-map (kbd "u") #'projectile-revert-project-buffers)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (setq projectile-dynamic-mode-line nil)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-show-menu nil)
  (setq projectile-enable-caching 'persistent)
  (setq projectile-file-exists-remote-cache-expire nil)

  (remove-hook 'projectile-find-dir-hook #'projectile-track-known-projects-find-file-hook)
  (remove-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook)

  ;; don't use file-truename
  ;; (ignore-file-truename 'projectile-project-root 'projectile-project-buffer-p)
  (unbind-key (kbd "C-c p t") 'projectile-mode-map)

  (add-hook 'projectile-mode-hook
            (lambda ()
              ;; delete directory is too slow
              (advice-remove 'delete-file #'delete-file-projectile-remove-from-cache)
              )))

;; other functions
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

;; tramp
;; (defun projectile-project-root-before(&optional dir)
;;    (let ((target-dir (or dir default-directory)))
;;      (not (file-remote-p target-dir))))
;; (advice-add 'projectile-project-root :before-while #'projectile-project-root-before)

;; cache projectile-root
(defcustom projectile-known-tramp-cache nil
  "List of known TRAMP project root paths.
Each element should be a string representing a project root directory path."
  :type '(repeat string)
  :group 'tramp-caches)

(defcustom projectile-ignored-root-paths '()
  "List of directory paths where projectile should stop searching for project roots."
  :type '(repeat string)
  :group 'projectile)

(defun projectile-root-stop-at-home-p (dir)
  (if (file-remote-p dir)
      (string-equal dir (concat (file-remote-p dir) "~/"))
    (string-equal dir (expand-file-name "~/"))))

(defun projectile-root-parent-at-home-p (dir)
  (when-let* ((parent-dir (file-name-directory (directory-file-name dir))))
    (projectile-root-stop-at-home-p parent-dir)))

(defun projectile-root-ignored-path-p (dir)
  (seq-some (lambda (path)
              (let ((expanded (expand-file-name path)))
                (string-equal dir expanded)))
            projectile-ignored-root-paths))

(defun projectile--tramp-cached-root (dir orig-fun args)
  "Get project root for remote DIR, using cache when possible."
  (or (find-longest-matching dir projectile-known-tramp-cache)
      (let ((root (apply orig-fun args)))
        (when (and root (not (member root projectile-known-tramp-cache)))
          (push root projectile-known-tramp-cache)
          (customize-save-variable 'projectile-known-tramp-cache projectile-known-tramp-cache))
        root)))

(defun projectile-project-root-stop-and-cache-advice (orig-fun &rest args)
  (let* ((current-dir (expand-file-name (or (car args) default-directory)))
         (root (cond
                ((projectile-root-ignored-path-p current-dir) nil)
                ((projectile-root-parent-at-home-p current-dir) current-dir)
                ((file-remote-p current-dir)
                 (projectile--tramp-cached-root current-dir orig-fun args))
                (t (apply orig-fun args)))))
    (unless (and root (projectile-root-stop-at-home-p root))
      root)))

(advice-add 'projectile-project-root :around #'projectile-project-root-stop-and-cache-advice)

(provide 'init-projectile)
