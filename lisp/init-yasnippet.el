(maybe-require-package 'yasnippet)
(require-package 'yasnippet-snippets)
(add-hook 'after-init-hook 'yas-global-mode)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(with-eval-after-load 'yasnippet
  (diminish 'yas-minor-mode)
  (defun gen-header-tag()
    (let* ((root (projectile-project-root))
           (path (string-trim-left
                  (file-name-sans-extension (buffer-file-name))
                  root))
           (name (replace-regexp-in-string "[./-]" "_" path)))
      (concat (upcase name) "_H_"))))

;; (add-hook 'yas-minor-mode-hook
;;    (lambda ()
;;      (define-key yas-keymap (kbd "C-f") 'yas-next-field)
;;      (define-key yas-keymap (kbd "C-b") 'yas-prev-field)))

(provide 'init-yasnippet)
