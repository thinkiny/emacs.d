(add-hook 'after-init-hook (lambda () (recentf-mode 1)))
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude `("/tmp/"
                   ,(expand-file-name "~/\\.emacs\\.d/elpa/")
                   ,(expand-file-name "~/\\.emacs\\.d/workspace/")
                   "/usr/include"
                   "/usr/local/include"))

(defun clean-recentf-action(x)
  (defvar recentf-exclude)
  (let ((recentf-exclude (list x)))
    (funcall #'recentf-cleanup)))

(defun clean-recentf()
  "Remove selected folder from recentf list."
  (interactive)
  (ivy-read "Remove from recentf list: "
            (delete-dups (mapcar #'file-name-directory recentf-list))
            :action #'clean-recentf-action))

(provide 'init-recentf)
