;; -*- lexical-binding: t; -*-

(use-package treemacs
  :config
  (setq treemacs-file-event-delay              5000
        treemacs-file-follow-delay             1
;;        treemacs-no-png-images                 t
        treemacs-position                      'right
        treemacs-silent-filewatch              t
        treemacs-silent-refresh                t
        treemacs-persist-file                  (expand-file-name "cache/treemacs-persist" user-emacs-directory)
        treemacs-follow-after-init             t
        treemacs-space-between-root-nodes      nil
        treemacs-width                         40)

  (treemacs-follow-mode -1)
  ;; (treemacs-filewatch-mode nil)
  ;; (treemacs-load-theme "Default")
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

(use-package treemacs-persp
  :after (treemacs persp-mode)
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;; (ignore-file-truename 'treemacs--read-first-project-path
;;                       'treemacs-do-add-project-to-workspace
;;                       'treemacs--current-builtin-project-function
;;                       'treemacs--expand-root-node
;;                       'treemacs--expand-dir-node)

(defun treemacs-custom-filter (file _)
  (or (string-equal ".cache" file)
      (string-equal "bazel-out" file)))

(with-eval-after-load 'treemacs
  (set-face-attribute 'treemacs-root-face nil :height 1.0)
  (require 'doom-themes-ext-treemacs)
  (doom-themes-treemacs-config)
  (push #'treemacs-custom-filter treemacs-ignored-file-predicates)
  (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

(provide 'init-treemacs)
