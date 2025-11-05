;; -*- lexical-binding: t; -*-

(maybe-require-package 'lua-mode)
(maybe-require-package 'ruby-end)

(add-hook 'lua-mode-hook (lambda()
                           ;;(semantic-mode 0)
                           (lsp-later)
                           (ruby-end-mode)))

(provide 'init-lua)
