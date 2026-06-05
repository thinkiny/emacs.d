;; toml  -*- lexical-binding: t; -*-

(use-package toml-mode
  :hook (toml-mode . goto-address-prog-mode))

;; protobuf
(use-package protobuf-mode
  :mode (("\\.proto\\'" . protobuf-mode)
         ("\\.proto3\\'" . protobuf-mode))
  :config
  (add-hook 'protobuf-mode-hook
            (lambda ()
              (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t))))

;; dockerfile
(use-package dockerfile-mode)
(add-auto-mode 'dockerfile-mode "Dockerfile*")

;; jinja
(use-package jinja2-mode)
(add-auto-mode 'jinja2-mode "\\.jinja\\'")

(with-eval-after-load 'sgml-mode
  (unbind-key (kbd "C-c ]") 'sgml-mode-map))

;; yaml
(add-auto-mode 'yaml-ts-mode "\\.yml\\'" "\\.erb\\'")

;; rpc-spec
(use-package rpm-spec-mode
  :mode "\\.spec")

;; csv
(use-package csv-mode)

;; hcl
(use-package hcl-mode
  :mode (("\\.conf\\'" . hcl-mode)))

;; shell script
(with-eval-after-load 'sh-script
  (add-hook 'sh-base-mode-hook
            (lambda()
              (add-to-list 'completion-at-point-functions 'cape-file))))

(provide 'init-conf-files)
