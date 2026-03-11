;; -*- lexical-binding: t; -*-

(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :config
  (defun gen-cpp-header-tag()
    (let* ((root (projectile-project-root))
           (path (string-trim-left
                  (file-name-sans-extension (buffer-file-name))
                  root))
           (name (replace-regexp-in-string "[./-]" "_" path)))
      (concat (upcase name) "_H_"))))

(use-package yasnippet-snippets
  :after yasnippet)


;; (add-hook 'yas-minor-mode-hook
;;    (lambda ()
;;      (define-key yas-keymap (kbd "C-f") 'yas-next-field)
;;      (define-key yas-keymap (kbd "C-b") 'yas-prev-field)))


(provide 'init-yasnippet)
