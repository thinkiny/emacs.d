;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(defun dired-keep-up-dir-on-top ()
  "Move .. entry right after the dired header and delete the . entry ."
  (when (and (buffer-live-p (current-buffer)) (derived-mode-p 'dired-mode))
    (save-excursion
      (goto-char (point-min))
      (let ((inhibit-read-only t)
            (case-fold-search nil) dotdot-line)
        (when (re-search-forward "^  d.* \\.$" nil t)
          ;; (setq dot-line (buffer-substring (line-beginning-position) (line-end-position)))
          (delete-region (line-beginning-position) (1+ (line-end-position))))
        (when (re-search-forward "^  d.* \\.\\.$" nil t)
          (setq dotdot-line (buffer-substring (line-beginning-position) (line-end-position)))
          (delete-region (line-beginning-position) (1+ (line-end-position))))
        (goto-char (point-min))
        (forward-line 1)
        (when (looking-at-p "  total") (forward-line 1))
        ;; (when dot-line (insert dot-line "\n"))
        (when dotdot-line (insert dotdot-line "\n"))))))

(with-eval-after-load 'dired
  (add-hook 'dired-after-readin-hook #'dired-keep-up-dir-on-top)
  ;;(require 'ls-lisp)
  ;; (setq ls-lisp-dirs-first t)
  ;; (setq ls-lisp-use-insert-directory-program nil)
  (setq dired-free-space nil)
  (setq dired-omit-verbose nil)
  (setq dired-listing-switches "-alh --group-directories-first")
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c '") 'wdired-change-to-wdired-mode))

(defun copy-filename ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      (dired-get-filename)
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message filename))))

(global-set-key (kbd "C-x w") #'copy-filename)

(provide 'init-dired)
;;; init-dired.el ends here
