(maybe-require-package 'lua-mode)
(maybe-require-package 'ruby-end)

(add-hook 'lua-mode-hook (lambda()
                           ;;(semantic-mode 0)
                           (eglot-ensure)
                           (ruby-end-mode)))

(provide 'init-lua)
