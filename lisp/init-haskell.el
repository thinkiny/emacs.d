(require 'haskell-ts-mode)

(with-eval-after-load 'eglot
  (haskell-ts-setup-eglot))

(defun my-haskell-mode-hook()
  (eglot-ensure))

(add-hook 'haskell-ts-mode-hook #'my-haskell-mode-hook)

(provide 'init-haskell)
