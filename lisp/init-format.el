(require-package 'format-all)

(require 'format-all)
(setcdr (assoc "SQL" format-all-default-formatters) '(pgformatter))

(defun toggle-format-all ()
  (interactive)
  (format-all-mode)
  (format-all-ensure-formatter))

;;(add-hook 'sql-mode-hook #'enable-format-all)
(add-hook 'protobuf-mode-hook #'toggle-format-all)

(provide 'init-format)
