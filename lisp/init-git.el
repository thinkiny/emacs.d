;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)

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

(defconst magit-mapping-file (expand-file-name "~/.emacs.d/magit-mapping.el"))
(defvar magit-mapping-list   (if (file-exists-p magit-mapping-file)
                                 (deserialize-from-file magit-mapping-file)
                               (list)))

(defun magit-set-mapping()
  (interactive)
  (when-let* ((src (magit-toplevel))
              (dest (read-file-name (format "%s => " src) "~" "./" t nil #'file-directory-p)))
    (if (assoc src magit-mapping-list)
        (setcdr (assoc src magit-mapping-list) dest)
      (add-to-list 'magit-mapping-list `(,src . ,dest)))
    (serialize-to-file magit-mapping-file magit-mapping-list)))

(defun magit-status-get-directory()
  (if (file-remote-p default-directory)
      (or (cdr (assoc (magit-toplevel) magit-mapping-list))
          default-directory)
    default-directory))

(defun magit-status-open ()
  (interactive)
  (magit-status-setup-buffer (magit-status-get-directory)))

(global-set-key (kbd "C-x g") #'magit-status-open)

(defun sanityinc/magit-or-vc-log-file (&optional prompt)
  (interactive "P")
  (if (and (buffer-file-name)
           (eq 'Git (vc-backend (buffer-file-name))))
      (if prompt
          (magit-log-buffer-file-popup)
        (magit-log-buffer-file t))
    (vc-print-log)))

;; Convenient binding for vc-git-grep
(after-load 'vc
  (define-key vc-prefix-map (kbd "l") 'sanityinc/magit-or-vc-log-file)
  (define-key vc-prefix-map (kbd "f") 'vc-git-grep))

(provide 'init-git)
;;; init-git.el ends here
