(use-package ivy
  :diminish ivy-mode
  :commands ivy-mode
  :init (ivy-mode 1)
  :config
  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'abbreviate
                ivy-count-format ""
                ivy-wrap t
                enable-recursive-minibuffers t
                ivy-use-selectable-prompt t)
  (define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)
  (define-key ivy-minibuffer-map (kbd "<down>") #'ivy-next-line-or-history)
  (define-key ivy-minibuffer-map (kbd "C-l") #'ivy-backward-delete-char)
  (define-key ivy-minibuffer-map (kbd "C-o") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "C-c C-f") #'ivy-toggle-calling) ;; follow mode
  (define-key ivy-minibuffer-map (kbd "TAB") #'ivy-insert-current)
  (define-key ivy-occur-mode-map (kbd "n") 'ivy-occur-next-line)
  (define-key ivy-occur-mode-map (kbd "p") 'ivy-occur-previous-line)
  (bind-key "C-c c" 'ivy-resume))

(use-package counsel
  :after ivy
  :config
  (setq-default counsel-mode-override-describe-bindings t)
  ;;(define-key counsel-find-file-map (kbd "C-l") #'counsel-up-directory)
  (setq counsel-find-file-ignore-regexp (regexp-opt '(".cache" "bazel-.*" ".o" ".elc" ".so")))
  (global-set-key (kbd "C-c l") 'counsel-imenu)
  (counsel-mode))

(use-package counsel-projectile
  :init (counsel-projectile-mode))

(use-package ivy-hydra)
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
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-project-root-cache-mode 1))

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
