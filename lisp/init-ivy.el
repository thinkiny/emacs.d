(use-package ivy
  :diminish ivy-mode
  :commands ivy-mode
  :init (ivy-mode 1)
  :config
  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'abbreviate
                ivy-count-format ""
                ivy-magic-tilde nil
                enable-recursive-minibuffers t
                ivy-use-selectable-prompt t)
  (define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)
  (define-key ivy-minibuffer-map (kbd "<down>") #'ivy-next-line-or-history)
  (bind-key "C-c c" 'ivy-resume))

(use-package counsel
  :after ivy
  :config
  (setq-default counsel-mode-override-describe-bindings t)
  (define-key counsel-find-file-map (kbd "C-l") #'counsel-up-directory)
  (global-set-key (kbd "C-c l") 'counsel-imenu)
  (counsel-mode))

(use-package counsel-projectile
  :init (counsel-projectile-mode))

;; icons
(when window-system
  (use-package all-the-icons-ivy-rich
    :init (all-the-icons-ivy-rich-mode 1)))

(use-package ivy-rich
  :diminish ivy-rich-mode
  :after (ivy counsel)
  :init (ivy-rich-mode 1)
  :config
  (setq ivy-rich-parse-remote-buffer nil
        ivy-rich-path-style 'abbrev)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; enable ivy-rich-switch-buffer cache
(eval-after-load 'ivy-rich
  (progn
    (defvar ek/ivy-rich-cache
      (make-hash-table :test 'equal))

    (defun ek/ivy-rich-cache-lookup (delegate candidate)
      (let ((result (gethash candidate ek/ivy-rich-cache)))
        (unless result
          (setq result (funcall delegate candidate))
          (puthash candidate result ek/ivy-rich-cache))
        result))

    (defun ek/ivy-rich-cache-reset ()
      (clrhash ek/ivy-rich-cache))

    (defun ek/ivy-rich-cache-rebuild ()
      (mapc (lambda (buffer)
              (ivy-rich--ivy-switch-buffer-transformer (buffer-name buffer)))
            (buffer-list)))

    (defun ek/ivy-rich-cache-rebuild-trigger ()
      (ek/ivy-rich-cache-reset)
      (run-with-idle-timer 1 nil 'ek/ivy-rich-cache-rebuild))

    (advice-add 'ivy-rich--ivy-switch-buffer-transformer :around 'ek/ivy-rich-cache-lookup)
    (advice-add 'ivy-switch-buffer :after 'ek/ivy-rich-cache-rebuild-trigger)))

(use-package ivy-xref
  :after ivy
  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs)
  (setq ivy-xref-use-file-path t)
  :config
  (defun ivy-xref-make-collection (xrefs)
    "Transform XREFS into a collection for display via `ivy-read'."
    (let ((collection nil))
      (dolist (xref xrefs)
        (with-slots (summary location) xref
          (let* ((line (xref-location-line location))
                 (file (xref-location-group location))
                 (candidate
                  (concat
                   (propertize
                    (concat
                     (if ivy-xref-use-file-path
                         ;; strip path
                         (string-trim-left file "\\(\\.\\./\\)*")
                       (file-name-nondirectory file))
                     (if (integerp line)
                         (format ":%d: " line)
                       ": "))
                    'face 'compilation-info)
                   (progn
                     (when ivy-xref-remove-text-properties
                       (set-text-properties 0 (length summary) nil summary))
                     summary))))
            (push `(,candidate . ,location) collection))))
      (nreverse collection))))

(provide 'init-ivy)
;;; init-ivy.el ends here
