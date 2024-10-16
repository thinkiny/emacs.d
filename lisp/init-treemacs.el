(use-package treemacs
  :config
  (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay      0.5
        treemacs-directory-name-transformer    #'identity
        treemacs-display-in-side-window        t
        treemacs-eldoc-display                 nil
        treemacs-file-event-delay              5000
        treemacs-file-extension-regex          treemacs-last-period-regex-value
        treemacs-file-follow-delay             1
        treemacs-file-name-transformer         #'identity
        treemacs-follow-after-init             t
        treemacs-git-command-pipe              ""
        treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   2
        treemacs-indentation-string            " "
        treemacs-is-never-other-window         nil
        treemacs-max-git-entries               5000
        treemacs-missing-project-action        'ask
        treemacs-no-png-images                 nil
        treemacs-no-delete-other-windows       t
        treemacs-project-follow-cleanup        nil
        treemacs-persist-file                  (expand-file-name "cache/treemacs-persist" user-emacs-directory)
        treemacs-position                      'right
        treemacs-read-string-input             'from-minibuffer
        treemacs-recenter-distance             0.1
        treemacs-recenter-after-file-follow    nil
        treemacs-recenter-after-tag-follow     nil
        treemacs-recenter-after-project-jump   'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                   nil
        treemacs-show-hidden-files             t
        treemacs-silent-filewatch              t
        treemacs-silent-refresh                t
        treemacs-sorting                       'alphabetic-asc
        treemacs-space-between-root-nodes      nil
        treemacs-tag-follow-cleanup            t
        treemacs-tag-follow-delay              1.5
        treemacs-width                         40)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode nil)
  (treemacs-fringe-indicator-mode 'always)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'extended))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  :bind
  (:map global-map
        ("M-0"     . treemacs-select-window)
        ("C-c p t" . treemacs)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(ignore-file-truename 'treemacs--read-first-project-path
                      'treemacs-do-add-project-to-workspace
                      'treemacs--current-builtin-project-function
                      'treemacs--expand-root-node
                      'treemacs--expand-dir-node)

(defun treemacs-custom-filter (file _)
  (or (string-equal ".cache" file)
      (string-equal "bazel-out" file)))

(with-eval-after-load 'treemacs
  (set-face-attribute 'treemacs-root-face nil :height 1.0)
  (push #'treemacs-custom-filter treemacs-ignored-file-predicates)
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

(provide 'init-treemacs)
