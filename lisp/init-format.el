(require-package 'format-all)

(setcdr (assoc "SQL" format-all-default-formatters) 'pgformatter)

(add-hook 'sql-mode-hook #'format-all-mode)
(add-hook 'protobuf-mode-hook #'format-all-mode)

(provide 'init-format)
