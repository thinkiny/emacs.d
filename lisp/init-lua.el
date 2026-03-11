;; -*- lexical-binding: t; -*-

(use-package lua-mode)
(use-package ruby-end)

(add-hook 'lua-mode-hook (lambda()
                           ;;(semantic-mode 0)
                           (lsp-later)
                           (ruby-end-mode)))

(provide 'init-lua)
