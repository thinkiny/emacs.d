;; -*- lexical-binding: t -*-

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (java "https://github.com/tree-sitter/tree-sitter-java")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
     (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     ;; (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (scala "https://github.com/tree-sitter/tree-sitter-scala")
     (haskell "https://github.com/tree-sitter/tree-sitter-haskell")))

(defun treesit-install-all-languages()
  (interactive)
  (cl-loop for i in treesit-language-source-alist
           do (treesit-install-language-grammar (car i))))

;; (dolist (lang '(
;;                 ;; ("c" . c)
;;                 ;; ("c++" . cpp)
;;                 ("go" . go)
;;                 ("rust" . rust)
;;                 ("scala" . scala)
;;                 ("java" . java)))
;;   (when-let* ((hook (intern (concat (car lang) "-mode-hook")))
;;               (ts-mode (intern (concat (car lang) "-ts-mode"))))
;;     (add-hook hook (lambda ()
;;                      (when (treesit-ready-p (cdr lang))
;;                        (funcall ts-mode)
;;                        (yas-activate-extra-mode (intern (concat (car lang) "-mode")))))))))

(provide 'init-treesit)
