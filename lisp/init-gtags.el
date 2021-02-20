(use-package global-tags
  :demand t)

(defconst gtags-include-pattern "^[[:space:]]*#\\(?:include\\|import\\)[[:space:]]+[\"<]\\(?:[./]*\\)?\\(.*?\\)[\">]")
(defconst gtags-type-pattern "^[[:space:]]*\\(?:extern\\|volatile\\|static\\|const\\)?[[:space:]]*\\(struct\\|class\\)[[:space:]]+\\(.*?\\)[[:space:]]+\\(?:.*?\\);")

;; imenu start
(defun ggtags-forward-to-line (line)
  "Move to line number LINE in current buffer."
  (cl-check-type line (integer 1))
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun ggtags-move-to-tag (tag)
  "Move to NAME tag in current line."
  ;; Do nothing if on the tag already i.e. by `ggtags-global-column'.
  (unless (or (not tag) (looking-at (concat (regexp-quote tag) "\\_>")))
    (let ((orig (point))
          (regexps (mapcar (lambda (fmtstr)
                             (format fmtstr (regexp-quote tag)))
                           '("\\_<%s\\_>" "%s\\_>" "%s"))))
      (beginning-of-line)
      (if (cl-loop for re in regexps
                   ;; Note: tag might not agree with current
                   ;; major-mode's symbol, so try harder. For
                   ;; example, in `php-mode' $cacheBackend is a
                   ;; symbol, but cacheBackend is a tag.
                   thereis (re-search-forward re (line-end-position) t))
          (goto-char (match-beginning 0))
        (goto-char orig)))))

(defun ggtags-goto-imenu-index (name line &rest _args)
  (ggtags-forward-to-line line)
  (ggtags-move-to-tag name))

(defun ggtags-build-imenu-index ()
  "A function suitable for `imenu-create-index-function'."
  (let ((file (and buffer-file-name (file-relative-name buffer-file-name))))
    (and file (with-temp-buffer
                (when (with-demoted-errors "ggtags-build-imenu-index: %S"
                        (zerop (process-file global-tags-global-command
                                             nil t nil "-x" "-f" file)))
                  (goto-char (point-min))
                  (cl-loop while (re-search-forward
                                  "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)" nil t)
                           collect (list (match-string 1)
                                         (string-to-number (match-string 2))
                                         'ggtags-goto-imenu-index)))))))
;; imenu end

(defun gtags-match-pattern (reg)
  (save-excursion
    (beginning-of-line)
    (looking-at reg)))

(defun global-tags-filter-current-file (tags)
  (let* ((match (seq-filter (lambda (x)
                              (string-equal (file-name-nondirectory (buffer-file-name))
                                            (cdr (car x))))
                            tags)))
    (or match tags)))

(defun global-tags-smart-get-locations (origin &rest args)
  (global-tags-filter-current-file
   (cond
    ((gtags-match-pattern gtags-include-pattern) (apply origin `(,(match-string 1) path)))
    ((gtags-match-pattern gtags-type-pattern)
     (let ((pattern (string-join `(,(match-string 1) ,(match-string 2) "{?" "$") "[[:space:]]*"))
           (res (apply origin args)))
       (seq-filter (lambda (x) (string-match-p pattern (cdr (nth 2 x)))) res)))
    (t (apply origin args)))))

(defun gtags-search-tag()
  (interactive)
  (ivy-read "Goto tag: "
            (xref-backend-identifier-completion-table (xref-find-backend))
            :require-match t
            :action #'xref-find-definitions))

(advice-add #'global-tags--get-locations :around #'global-tags-smart-get-locations)

(add-hook 'lua-mode-hook #'global-tags-exclusive-backend-mode)
(add-hook 'asm-mode-hook #'global-tags-exclusive-backend-mode)
(add-hook 'sh-mode-hook #'global-tags-exclusive-backend-mode)
(add-hook 'global-tags-exclusive-backend-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c w s") #'gtags-search-tag)
            (setq-local imenu-create-index-function #'ggtags-build-imenu-index)))

(provide 'init-gtags)
