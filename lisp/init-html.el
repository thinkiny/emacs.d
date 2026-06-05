;; -*- lexical-binding: t; -*-

(use-package web-mode)
(use-package zencoding-mode)
(use-package htmlize)

(setq web-mode-enable-current-element-highlight t)
(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq zencoding-preview-default nil)

(with-eval-after-load 'nxml-mode
  (setq nxml-slash-auto-complete-flag t)
  (unbind-key (kbd "C-c ]") 'nxml-mode-map))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))

(defun my-html-mode-hook()
  (eglot-ensure)
  (zencoding-mode))

(defun my-css-mode-hook()
  (eglot-ensure))

(with-eval-after-load 'css-mode
  (setq css-indent-offset 2))

(add-hook 'web-mode-hook #'my-html-mode-hook)
(add-hook 'css-ts-mode-hook #'my-css-mode-hook)

(with-eval-after-load 'eglot
  ;; npm i -g @t1ckbase/vscode-langservers-extracted
  (set-eglot-server-program 'web-mode
                            '("vscode-html-language-server" "--stdio"))
  (set-eglot-server-program 'css-ts-mode
                            '("vscode-css-language-server" "--stdio"))
  (set-eglot-server-program 'json-ts-mode
                            '(eglot-json-ls "vscode-json-language-server" "--stdio")))

(use-package vue-mode)
(add-hook 'vue-mode-hook #'eglot-ensure)

(provide 'init-html)
