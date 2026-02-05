;; -*- lexical-binding: t; -*-

(require-package 'typescript-mode)

(setq ts-lsp-modes '(js-ts-mode tsx-ts-mode typescript-ts-mode))

(with-eval-after-load 'eglot
  (set-eglot-server-program '((js-ts-mode :language-id "javascript")
                              (tsx-ts-mode :language-id "typescriptreact")
                              (typescript-ts-mode :language-id "typescript"))
                            `("typescript-language-server" "--stdio"
                              :initializationOptions
                              (:preferences
                               (:importModuleSpecifierPreference "project-relative"
                                :allowRenameOfImportPath t)))))

(dolist (hook (mapcar #'derived-mode-hook-name ts-lsp-modes))
  (add-hook hook 'eglot-ensure))

(add-auto-mode 'json-ts-mode "\\.json")
(add-auto-mode 'js-ts-mode "\\.js\\'" "\\.mjs\\'")
(add-auto-mode 'tsx-ts-mode "\\.jsx\\'")

(provide 'init-js)
