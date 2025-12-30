;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:
(with-eval-after-load 'vc-hooks
  (setq vc-handled-backends '(Git))

  (defun turn-vc-off-if-remote ()
    "Disable VC mode if the current file is remote (via TRAMP)."
    (if (file-remote-p (buffer-file-name))
        (setq-local vc-handled-backends nil)))
  (add-hook 'find-file-hook 'turn-vc-off-if-remote))

(use-package magit
  :demand t
  :config
  (setq transient-default-level 5
        magit-diff-refine-hunk t
        magit-auto-revert-mode nil
        magit-refresh-status-buffer nil
        magit-save-repository-buffers nil
        magit-commit-show-diff nil
        magit-branch-direct-configure nil
        magit-revision-insert-related-refs nil
        magit-tramp-pipe-stty-settings 'pty)
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
