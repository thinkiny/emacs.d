(use-package erlang)

(add-hook 'erlang-mode-hook
          (lambda ()
            (setq-local lsp-enable-format-at-save nil)
            (lsp-later)))

(provide 'init-erlang)
