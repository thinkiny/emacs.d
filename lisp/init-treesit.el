;; -*- lexical-binding: t -*-

(defvar treesit-use-build-in nil)
(defun tree-sitter-init()
  (require-package 'tree-sitter)
  (require-package 'tree-sitter-langs)
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (after-load-theme (set-face-attribute 'tree-sitter-hl-face:property nil :slant 'normal)))


(defun treesit-init()
  (setq treesit-use-build-in t)
  (require 'treesit)
  (dolist (lang '(("c" . c)
                  ("c++" . cpp)
                  ("go" . go)
                  ("rust" . rust)
                  ("scala" . scala)
                  ("java" . java)))
    (when-let* ((hook (intern (concat (car lang) "-mode-hook")))
                (ts-mode (intern (concat (car lang) "-ts-mode"))))
      (add-hook hook (lambda ()
                       (when (treesit-ready-p (cdr lang))
                         (funcall ts-mode)))))))

(if (and (fboundp 'treesit-available-p) (treesit-available-p))
    (treesit-init)
  (tree-sitter-init))

;; (tree-sitter-init)
(provide 'init-treesit)
