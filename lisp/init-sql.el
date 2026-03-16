;;; init-sql.el --- Support for SQL -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package clutch
  :vc (:url "https://github.com/LuciusChen/clutch.git"
       :rev :newest)
  :mode (("\\.sql\\'" . clutch-mode))
  :bind
  (:map clutch-result-mode-map
        ("f" . clutch-result-next-cell)
        ("b" . clutch-result-prev-cell)
        ("F" . clutch-result-fullscreen-toggle)))

(use-package sqlite-mode-extras
  :bind (:map
         sqlite-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("b" . sqlite-mode-extras-backtab-dwim)
         ("f" . sqlite-mode-extras-tab-dwim)
         ("+" . sqlite-mode-extras-add-row)
         ("D" . sqlite-mode-extras-delete-row-dwim)
         ("C" . sqlite-mode-extras-compose-and-execute)
         ("E" . sqlite-mode-extras-execute)
         ("S" . sqlite-mode-extras-execute-and-display-select-query)
         ("DEL" . sqlite-mode-extras-delete-row-dwim)
         ("g" . sqlite-mode-extras-refresh)
         ("<backtab>" . sqlite-mode-extras-backtab-dwim)
         ("<tab>" . sqlite-mode-extras-tab-dwim)
         ("RET" . sqlite-mode-extras-ret-dwim)))

(provide 'init-sql)
;;; init-sql.el ends here
