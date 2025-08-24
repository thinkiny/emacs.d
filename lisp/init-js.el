(require-package 'typescript-mode)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((js-ts-mode typescript-mode (typescript-ts-base-mode :language-id "typescript")) . (eglot-deno "deno" "lsp")))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list :enable t
          :lint t)))

(defun my-js-mode-hook ()
  (if (string= "json" (file-name-extension (buffer-file-name)))
      (so-long-mode)
    (eglot-ensure)))

;; (add-hook 'js-mode-hook #'my-js-mode-hook)
;; (add-hook 'typescript-mode-hook #'eglot-ensure)
;; (add-hook 'json-ts-mode #'my-json-mode-hook)

(defun my-json-mode-hook()
  (whitespace-cleanup-mode -1))

(add-auto-mode 'json-ts-mode "\\.json")
(add-auto-mode 'js-ts-mode "\\.js\\'" "\\.mjs\\'")

(provide 'init-js)
