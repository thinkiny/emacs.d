;; -*- lexical-binding: t -*-

(defun tree-sitter-init()
  (require-package 'tree-sitter)
  (require-package 'tree-sitter-langs)
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (after-load-theme (set-face-attribute 'tree-sitter-hl-face:property nil :slant 'normal)))


(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (scala "https://github.com/tree-sitter/tree-sitter-scala")))

(defun treesit-init()
  (require 'treesit)
  (dolist (lang '(
                  ;; ("c" . c)
                  ;; ("c++" . cpp)
                  ("go" . go)
                  ("rust" . rust)
                  ("scala" . scala)
                  ("java" . java)))
    (when-let* ((hook (intern (concat (car lang) "-mode-hook")))
                (ts-mode (intern (concat (car lang) "-ts-mode"))))
      (add-hook hook (lambda ()
                       (when (treesit-ready-p (cdr lang))
                         (funcall ts-mode)
                         (yas-activate-extra-mode (intern (concat (car lang) "-mode")))))))))

(if (and (fboundp 'treesit-available-p) (treesit-available-p))
    (treesit-init)
  (tree-sitter-init))

;; (tree-sitter-init)
(provide 'init-treesit)
