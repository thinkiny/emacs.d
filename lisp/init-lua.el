(maybe-require-package 'lua-mode)
(maybe-require-package 'ruby-end)
(add-hook 'lua-mode-hook (lambda() (semantic-mode 0)
						   (ruby-end-mode)
						   (indent-guide-mode)))

(provide 'init-lua)
