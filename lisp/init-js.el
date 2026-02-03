;; -*- lexical-binding: t; -*-

(require-package 'typescript-mode)

(setq ts-lsp-modes '(js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode))

(with-eval-after-load 'eglot
  (set-eglot-server-progam ts-lsp-modes
                           '("typescript-language-server" "--stdio"
                             :initializationOptions
                             (:preferences
                              (:importModuleSpecifierPreference "relative"
                               :allowRenameOfImportPath t
                               :includeInlayEnumMemberValueHints t
                               :includeInlayFunctionLikeReturnTypeHints t
                               :includeInlayFunctionParameterTypeHints t
                               :includeInlayParameterNameHints "all" ; "none" | "literals" | "all"
                               :includeInlayParameterNameHintsWhenArgumentMatchesName t
                               :includeInlayPropertyDeclarationTypeHints t
                               :includeInlayVariableTypeHints t
                               :includeInlayVariableTypeHintsWhenTypeMatchesName t)))))

(dolist (hook (mapcar #'derived-mode-hook-name ts-lsp-modes))
  (add-hook hook 'eglot-ensure))

(add-auto-mode 'json-ts-mode "\\.json")
(add-auto-mode 'js-ts-mode "\\.js\\'" "\\.mjs\\'")

(provide 'init-js)
