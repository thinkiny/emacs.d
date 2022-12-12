;; yaml
(when (maybe-require-package 'yaml-mode)
  (add-auto-mode 'yaml-mode "\\.yml\\.erb\\'")
  (add-hook 'yaml-mode-hook 'goto-address-prog-mode))

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

(provide 'init-conf-files)
