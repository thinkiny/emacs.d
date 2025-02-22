;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
(use-package magit
  :demand t
  :config
  (setq transient-default-level 5
        magit-diff-refine-hunk t
        magit-auto-revert-mode nil
        magit-save-repository-buffers nil
        magit-revision-insert-related-refs nil)
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up)
  (define-key magit-file-section-map (kbd "<RET>") 'magit-diff-visit-file-other-window))

(global-set-key (kbd "C-x g") #'magit-status)
(ignore-tramp-ssh-control-master 'magit-status)

(defun sanityinc/magit-or-vc-log-file (&optional prompt)
  (interactive "P")
  (if (and (buffer-file-name)
           (eq 'Git (vc-backend (buffer-file-name))))
      (if prompt
          (magit-log-buffer-file-popup)
        (magit-log-buffer-file t))
    (vc-print-log)))

(setq vc-follow-symlinks nil)
(setq vc-handled-backends nil)
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
