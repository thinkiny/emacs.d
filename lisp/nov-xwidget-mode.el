(require 'nov-xwidget-webkit)

(defun nov-extract-epub()
  (unless (file-exists-p nov-temp-dir)
    (let ((exit-code (nov-unzip-epub nov-temp-dir buffer-file-name)))
      (when (not (integerp exit-code))
        (nov-clean-up)
        (error "EPUB extraction aborted by signal %s" exit-code))
      (when (> exit-code 1) ; exit code 1 is most likely a warning
        (nov-clean-up)
        (error "EPUB extraction failed with exit code %d (see *nov unzip* buffer)"
               exit-code)))
    (when (not (nov-epub-valid-p nov-temp-dir))
      (nov-clean-up)
      (error "Invalid EPUB file"))))

(define-derived-mode nov-xwidget-mode special-mode "EPUB"
  "Major mode for reading EPUB documents"
  (add-hook 'kill-emacs-hook 'nov-xwidget-save-all)
  (when (not buffer-file-name)
    (error "EPUB must be associated with file"))
  (when (not nov-unzip-program)
    (error "unzip executable not found, customize `nov-unzip-program'"))
  (setq nov-temp-dir (expand-file-name (file-name-nondirectory buffer-file-name) nov-xwidget-cache-dir))

  (unless (file-exists-p nov-xwidget-cache-dir)
    (make-directory nov-xwidget-cache-dir t))

  (nov-extract-epub)
  (let* ((content (nov-slurp (nov-container-filename nov-temp-dir) t))
         (content-file-name (nov-container-content-filename content))
         (content-file (nov-make-path nov-temp-dir content-file-name))
         (work-dir (file-name-directory content-file))
         (content (nov-slurp content-file t)))
    (setq nov-content-file content-file)
    (setq nov-epub-version (nov-content-version content))
    (setq nov-metadata (nov-content-metadata content))
    (setq nov-documents (apply 'vector (nov-content-files work-dir content)))
    (setq nov-documents-index 0))
  (setq buffer-undo-list t)
  (setq nov-file-name (buffer-file-name)) ; kept for compatibility reasons
  (setq-local bookmark-make-record-function
              'nov-bookmark-make-record)
  (let ((place (nov-saved-place (cdr (assq 'identifier nov-metadata)))))
    (if place
        (let ((index (cdr (assq 'index place)))
              (point (cdr (assq 'point place))))
          (if (nov--index-valid-p nov-documents index)
              (setq nov-documents-index index)
            (nov-warn "Couldn't restore last position")))))
  (let ((dummy-buf (current-buffer)))
    (nov-xwidget-inject-all-files)
    (nov-xwidget-view)
    (read-only-mode)
    (kill-buffer dummy-buf)))

(provide 'nov-xwidget-mode)
