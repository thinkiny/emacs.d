(maybe-require-package 'yasnippet)
(require-package 'yasnippet-snippets)
(add-hook 'after-init-hook 'yas-global-mode)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(after-load 'yasnippet
  (diminish 'yas-minor-mode))

;; (add-hook 'yas-minor-mode-hook
;;    (lambda ()
;;      (define-key yas-keymap (kbd "C-f") 'yas-next-field)
;;      (define-key yas-keymap (kbd "C-b") 'yas-prev-field)))

(provide 'init-yasnippet)
