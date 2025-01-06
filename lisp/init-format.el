(require-package 'format-all)

(require 'format-all)
(setcdr (assoc "SQL" format-all-default-formatters) '(pgformatter))

(defun toggle-format-all-mode ()
  (interactive)
  (format-all-mode)
  (format-all-ensure-formatter))

(defun format-current-buffer ()
  (interactive)
  (format-all-ensure-formatter)
  (format-all-buffer))

;;(add-hook 'sql-mode-hook #'enable-format-all)
(add-hook 'protobuf-mode-hook #'toggle-format-all-mode)

(provide 'init-format)
