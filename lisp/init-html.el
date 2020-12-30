;;(require-package 'tagedit)
(maybe-require-package 'web-mode)
(maybe-require-package 'zencoding-mode)

;;(after-load 'web-mode
;;  (tagedit-add-paredit-like-keybindings)
;;  (define-key tagedit-mode-map (kbd "M-?") nil)
;;  (define-key tagedit-mode-map (kbd "M-s") nil)
;;  (add-hook 'web-mode-hook (lambda () (tagedit-mode 1))))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-hook 'web-mode-hook 'zencoding-mode)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq zencoding-preview-default nil)

(provide 'init-html)
