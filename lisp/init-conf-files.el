;; toml
(when (maybe-require-package 'toml-mode)
  (add-hook 'toml-mode-hook 'goto-address-prog-mode))

;; protobuf
(use-package protobuf-mode
  :mode (("\\.proto$" . protobuf-mode) ("\\.proto3$" . protobuf-mode))
  :config
  (add-hook 'protobuf-mode-hook
            (lambda ()
              (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t))))

;; dockerfile
(use-package dockerfile-mode)

;; jinja
(use-package jinja2-mode)
(add-auto-mode 'jinja2-mode "\\.jinja\\'")

(with-eval-after-load 'sgml-mode
  (unbind-key (kbd "C-c ]") 'sgml-mode-map))

;; yaml
(add-auto-mode 'yaml-ts-mode "\\.yml\\'" "\\.erb\\'")

(provide 'init-conf-files)
