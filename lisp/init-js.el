(require-package 'typescript-mode)
(use-package js2-mode
  :mode (("\\.js?$" . js2-mode)
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

(defun my-js-mode-hook ()
  (so-long-mode)
  (unless (string= "json" (file-name-extension (buffer-file-name)))
    (eglot-ensure)))

(add-hook 'js-mode-hook #'my-js-mode-hook)

(provide 'init-js)
