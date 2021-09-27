(require-package 'format-all)

(require 'format-all)
(setcdr (assoc "SQL" format-all-default-formatters) '(pgformatter))

(defun enable-format-all ()
  (interactive)
  (format-all-mode)
  (format-all-ensure-formatter))

;;(add-hook 'sql-mode-hook #'enable-format-all)
(add-hook 'protobuf-mode-hook #'enable-format-all)

(provide 'init-format)
