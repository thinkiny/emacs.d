;; -*- lexical-binding: t; -*-

(maybe-require-package 'web-mode)
(maybe-require-package 'zencoding-mode)
(require-package 'htmlize)

(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq zencoding-preview-default nil)
(setq web-mode-enable-auto-indentation nil)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

(defun my-html-mode-hook()
  (eglot-ensure)
  (zencoding-mode))

(defun my-css-mode-hook()
  (eglot-ensure))

(with-eval-after-load 'css-mode
  (setq css-indent-offset 2))

(add-hook 'web-mode-hook #'my-html-mode-hook)
(add-hook 'json-ts-mode-hook #'eglot-ensure)
(add-hook 'css-ts-mode-hook #'my-css-mode-hook)

(defun sort-json-buffer ()
  "Sort JSON keys in the current buffer using jq."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           "jq -S ." nil t))

(with-eval-after-load 'eglot
  ;; npm i -g @t1ckbase/vscode-langservers-extracted
  (defclass eglot-json-ls (eglot-lsp-server) ()
    :documentation "JSON language server.")

  (cl-defmethod eglot-execute ((_server eglot-json-ls) action)
    "Handle JSON LS commands locally when possible."
    (if (equal (plist-get action :command) "json.sort")
        (sort-json-buffer)
      (cl-call-next-method)))

  (set-eglot-server-program 'web-mode
                            '("vscode-html-language-server" "--stdio"))
  (set-eglot-server-program 'css-ts-mode
                            '("vscode-css-language-server" "--stdio"))
  (set-eglot-server-program 'json-ts-mode
                            '(eglot-json-ls "vscode-json-language-server" "--stdio")))

(use-package vue-mode)
(add-hook 'vue-mode-hook #'eglot-ensure)

(provide 'init-html)
