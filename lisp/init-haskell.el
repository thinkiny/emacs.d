(require 'haskell-ts-mode)

(with-eval-after-load 'eglot
  (defvar eglot-server-programs)
  (add-to-list 'eglot-server-programs
               '(haskell-ts-mode . ("haskell-language-server-wrapper" "--lsp"))))

(defun my-haskell-mode-hook()
  (eglot-ensure))

(add-hook 'haskell-ts-mode-hook #'my-haskell-mode-hook)

(provide 'init-haskell)
