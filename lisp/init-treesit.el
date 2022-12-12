;; tree-sit

(defun tree-sitter-init()
  (require-package 'tree-sitter)
  (require-package 'tree-sitter-langs)
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (after-load-theme (set-face-attribute 'tree-sitter-hl-face:property nil :slant 'normal)))

(tree-sitter-init)

(provide 'init-treesit)
