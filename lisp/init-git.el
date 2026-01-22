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

  (define-key magit-status-mode-map (kbd "C-c f") 'magit-rsync-from-src)
  (define-key magit-status-mode-map (kbd "C-c t") 'magit-rsync-to-src)
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up)
  (define-key magit-file-section-map (kbd "<RET>") 'magit-diff-visit-file-other-window))

(global-set-key (kbd "C-x g") #'magit-status)
(ignore-tramp-ssh-control-master 'magit-status)

;; cache magit top level
(defcustom magit-toplevel-tramp-cache nil
  "Cache 'magit-toplevel'."
  :type '(alist :key-type string :value-type string)
  :group 'tramp-caches)

(defun cache-tramp-magit-toplevel (orig &optional directory)
  (cache-tramp-from-matching-value
   (file-truename (or directory default-directory))
   'magit-toplevel-tramp-cache orig directory))

(defun magit-rsync-to-src()
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
        (magit-run-rsync (cdr matching-entry) (car matching-entry)))))

(defun magit-run-rsync(src-tramp dest-tramp)
  "Run rsync from SRC-TRAMP to DEST-TRAMP ."
  (interactive)
  (when-let* ((src-rsync (tramp-to-rsync-address src-tramp))
              (dest-rsync (tramp-to-rsync-address dest-tramp)))
    (let* ((rsync-cmd (list "rsync" "-avr" "--delete" "--exclude" ".git/"
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
            (message (format "sync %s => %s success" src-rsync dest-rsync))
            (with-current-buffer magit-buffer
              (magit-refresh-buffer))
            (when (buffer-live-p buffer)
              (kill-buffer buffer)))
           (t
            (display-buffer buffer))))))))

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
        (magit-run-rsync (car matching-entry) (cdr matching-entry)))))

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

(provide 'init-git)
;;; init-git.el ends here
