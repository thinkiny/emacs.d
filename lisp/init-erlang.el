(use-package erlang
  :mode (("\\.erl?$" . erlang-mode)
     ("rebar\\.config$" . erlang-mode)
     ("relx\\.config$" . erlang-mode)
     ("sys\\.config\\.src$" . erlang-mode)
     ("sys\\.config$" . erlang-mode)
     ("\\.config\\.src?$" . erlang-mode)
     ("\\.config\\.script?$" . erlang-mode)
     ("\\.hrl?$" . erlang-mode)
     ("\\.app?$" . erlang-mode)
     ("\\.app.src?$" . erlang-mode)
     ("\\Emakefile" . erlang-mode)))

(add-hook 'erlang-mode-hook
          (lambda ()
            (setq-local lsp-enable-format-at-save nil)
            (lsp-later)))

(provide 'init-erlang)
