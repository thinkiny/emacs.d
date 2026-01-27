;; -*- lexical-binding: t; -*-

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
          "*\\.venv$"
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
  (setq projectile-dynamic-mode-line nil)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-show-menu nil)
  (setq projectile-enable-caching 'persistent)
  (setq projectile-file-exists-remote-cache-expire nil)
  (projectile-mode)

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

(defun get-default-args-for-ripgrep()
  (let ((file-name (buffer-file-name)))
    (concat (if file-name
                (pcase (file-name-extension file-name)
                  ("go" "-tgo")
                  ("py" "-tpython")
                  ("js" "-tjs")
                  ("cc" "-tcpp")
                  (_ ""))
              "")
            " -i")))

(defun counsel-projectile-rg-default()
  (interactive)
  (counsel-projectile-rg (get-default-args-for-ripgrep)))

(use-package counsel-projectile
  :init (counsel-projectile-mode)
  :bind (:map
         projectile-mode-map
         ("C-c p s s" . #'counsel-projectile-rg-default)))

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

(defcustom projectile-root-ignore-paths nil
  "List of directory paths where projectile should stop searching for project roots."
  :type '(repeat string)
  :group 'projectile)

(defconst projectile-current-user-home (expand-file-name "~/"))
(defun projectile-stop-at-home(dir)
  (if (file-remote-p dir)
      (string-equal dir (concat (file-remote-p dir) "~/"))
    (string-equal dir projectile-current-user-home)))

(defun projectile-project-root-around-advice (orig-fun &rest args)
  (let ((current-dir (expand-file-name (or (car args) default-directory))))
    (if (or (member current-dir projectile-root-ignore-paths)
            (projectile-stop-at-home current-dir))
        nil
      (if (file-remote-p current-dir)
          (if-let* ((cached-project (find-longest-matching current-dir projectile-known-tramp-cache)))
              cached-project
            (let ((project-root (apply orig-fun args)))
              (when project-root
                (unless (member project-root projectile-known-tramp-cache)
                  (push project-root projectile-known-tramp-cache)
                  (customize-save-variable 'projectile-known-tramp-cache projectile-known-tramp-cache)))
              project-root))
        (apply orig-fun args)))))

(advice-add 'projectile-project-root :around #'projectile-project-root-around-advice)

(provide 'init-projectile)
