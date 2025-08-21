(require-package 'typescript-mode)

(use-package js2-mode
  :mode (("\\.js?$" . js2-mode)
         ("\\.mjs?$" . js2-mode)
         ("\\.jsx?\\'" . js2-jsx-mode))
  :config
  (setq js-chain-indent t
        ;; These have become standard in the JS community
        js2-basic-offset 2
        ;; Don't mishighlight shebang lines
        js2-skip-preprocessor-directives t
        ;; let flycheck handle this
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        ;; Flycheck provides these features, so disable them: conflicting with
        ;; the eslint settings.
        js2-strict-missing-semi-warning nil
        ;; maximum fontification
        js2-highlight-level 3
        js2-idle-timer-delay 0.15))

;; (with-eval-after-load 'js-mode
;;   (unbind-key (kbd "M-.") 'js-mode-map))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((js-mode typescript-mode (typescript-ts-base-mode :language-id "typescript")) . (eglot-deno "deno" "lsp")))

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

(defun my-json-mode-hook()
  (whitespace-cleanup-mode -1))

(add-auto-mode 'json-ts-mode "\\.json")
(add-hook 'json-ts-mode #'my-json-mode-hook)


(provide 'init-js)
