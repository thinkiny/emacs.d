;;; init-recentf.el  -*- lexical-binding: t -*-

(add-hook 'after-init-hook (lambda () (recentf-mode 1)))
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude `("/tmp/"
                   ,(expand-file-name "~/\\.emacs\\.d/elpa/")
                   ,(expand-file-name "~/\\.emacs\\.d/workspace/")
                   "/Applications/"
                   ".*/.metals/"
                   "/usr/include"
                   "/usr/local/include"))

(defun get-recentf-exclude-list(name)
  (if (s-suffix-p "//" name)
      (seq-filter (apply-partially #'s-starts-with-p (substring name 0 -1)) recentf-list)
    (list name)))

(defun delete-recentf-action(item)
  (defvar recentf-exclude)
  (let ((recentf-exclude (get-recentf-exclude-list item)))
    (call-interactively #'recentf-cleanup)))

(defun delete-from-recentf()
  "Delete selected folder from recentf list."
  (interactive)
  (ivy-read "Delete from recentf list: "
            (delete-dups (mapcar #'file-name-directory recentf-list))
            :action #'delete-recentf-action))

(provide 'init-recentf)
