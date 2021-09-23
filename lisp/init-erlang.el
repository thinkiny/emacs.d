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
     ("\\Emakefile" . erlang-mode))
  :config
  (setq erlang-max-files-to-visit-for-refining-xrefs 256)
  (unbind-key (kbd "RET") 'erlang-mode-map))

(add-hook 'erlang-mode-hook
          (lambda ()
            (setq-local lsp-enable-format-at-save nil)
            (lsp-later)))

(defun init-erlang-ls()
  (interactive)
  (when-let ((project-root (projectile-project-root)))
    (write-region
"apps_dirs:
  - \"apps/*\"
deps_dirs:
  - \"_build/default/lib/*\"
include_dirs:
  - \"apps\"
  - \"apps/*/include\"
  - \"_build/default/lib/\"
  - \"_build/default/lib/*/include\""
     nil
     (format "%s/erlang_ls.config" project-root))))

(provide 'init-erlang)
