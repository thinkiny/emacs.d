;;; init-recentf.el  -*- lexical-binding: t -*-

(add-hook 'after-init-hook (lambda () (recentf-mode 1)))
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude `("/tmp/"
                   ,(expand-file-name "~/\\.emacs\\.d/elpa/")
                   ,(expand-file-name "~/\\.emacs\\.d/workspace/")
                   ,(expand-file-name "~/\\.emacs\\.d/eglot-eclipse-jdt-cache/")
                   "/Applications/"
                   ".*/.metals/"
                   ".*/.cache/"))

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


(defun recentf-keep-predicate (file)
  "Return non-nil if FILE should be kept in the recent list.
It handles the case of remote files as well."
  (cond
   ((file-remote-p file) t)
   ((file-readable-p file))))

(setq recentf-keep '(recentf-keep-predicate))

(provide 'init-recentf)
