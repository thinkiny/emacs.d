;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:
(with-eval-after-load 'vc-hooks
  (setq vc-handled-backends '(Git))
  (setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp)))


(use-package magit
  :demand t
  :config
  (setq transient-default-level 5
        magit-diff-refine-hunk t
        magit-auto-revert-mode nil
        magit-save-repository-buffers nil
        magit-commit-show-diff nil
        magit-branch-direct-configure nil
        magit-revision-insert-related-refs nil
        magit-tramp-pipe-stty-settings 'pty)

  (define-key magit-status-mode-map (kbd "R") 'magit-rsync-from-src)
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up)
  (define-key magit-file-section-map (kbd "<RET>") 'magit-diff-visit-file-other-window))

(global-set-key (kbd "C-x g") #'magit-status)
(ignore-tramp-ssh-control-master 'magit-status)

;; cache magit top level
(defcustom magit-toplevel-tramp-cache nil
  "Cache 'magit-toplevel'."
  :type '(alist :key-type string :value-type sexp)
  :group 'tramp-caches)

(defun cache-tramp-magit-toplevel (orig &optional directory)
  (cache-tramp-from-matching-value
   (expand-file-name (or directory default-directory))
   'magit-toplevel-tramp-cache orig directory))

(defun tramp-to-rsync-address (tramp-path)
  "Convert TRAMP-PATH to rsync address format.
Example: /ssh:user@host:/path/to/dir -> user@host:/path/to/dir"
  (if (tramp-tramp-file-p tramp-path)
        (let* ((parsed (tramp-dissect-file-name tramp-path))
               (method (tramp-file-name-method parsed))
               (user (tramp-file-name-user parsed))
               (host (tramp-file-name-host parsed))
               (localname (tramp-file-name-localname parsed)))
          (if (string= method "ssh")
              (let ((effective-user (or user (user-login-name))))
                (concat effective-user "@" host ":" localname))
            ;; For non-ssh methods, return original path
            tramp-path))
      ;; Not a TRAMP path, return as-is (likely local path)
      tramp-path))

(defun magit-rsync-from-src()
  "Find matching entry in `magit-toplevel-tramp-cache` and run rsync.
Looks for an entry where the value matches current project root,
then syncs from the key (source) to the value (destination)."
  (interactive)
  (let ((matching-entry nil))
    (dolist (entry magit-toplevel-tramp-cache)
      (when (string= (cdr entry) (magit-toplevel))
        (setq matching-entry entry)
        (cl-return)))

    (if matching-entry
        (when-let* ((src-tramp (car matching-entry))
                    (dest-tramp (cdr matching-entry))
                    (src-rsync (tramp-to-rsync-address src-tramp))
                    (dest-rsync (tramp-to-rsync-address dest-tramp)))

          (let* ((rsync-cmd (list "rsync" "-avur" "--delete" "--exclude" ".git/"
                                  src-rsync dest-rsync))
                 (magit-buffer (current-buffer))
                 (buffer (get-buffer-create "*magit-rsync*")))
            (with-current-buffer buffer
              (erase-buffer)
              (insert (mapconcat #'identity rsync-cmd " "))
              (let ((exit-code (apply 'call-process
                                      "rsync" nil t nil
                                      (cdr rsync-cmd))))
                (cond
                 ((= exit-code 0)
                  (with-current-buffer magit-buffer
                    (magit-refresh-buffer))
                  (when (buffer-live-p buffer)
                    (kill-buffer buffer)))
                 (t
                  (display-buffer buffer))))))))))

(advice-add 'magit-toplevel :around #'cache-tramp-magit-toplevel)

(defun sanityinc/magit-or-vc-log-file (&optional prompt)
  (interactive "P")
  (if (and (buffer-file-name)
           (eq 'Git (vc-backend (buffer-file-name))))
      (if prompt
          (magit-log-buffer-file-popup)
        (magit-log-buffer-file t))
    (vc-print-log)))

(remove-hook 'find-file-hooks 'vc-refresh-state)

;; Convenient binding for vc-git-grep
(with-eval-after-load 'vc
  (define-key vc-prefix-map (kbd "l") 'sanityinc/magit-or-vc-log-file)
  (define-key vc-prefix-map (kbd "f") 'vc-git-grep))

;; smerge-mode
(use-package smerge-mode
  :config
  ;; FIXME
  (setq smerge-command-prefix "\C-cd")
  (add-hook 'prog-mode-hook
            (lambda ()
              (run-with-timer 1 nil (lambda ()
                                      (smerge-mode 1))))))

(provide 'init-git)
;;; init-git.el ends here
