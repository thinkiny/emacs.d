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

(use-package ivy-xref
  :after ivy
  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))

(provide 'init-ivy)
;;; init-ivy.el ends here
