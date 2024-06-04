(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package grip-mode
  :ensure t
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode))
  :config
  (setq grip-preview-use-webkit t)
  (setq grip-update-after-change nil))

(defun markdown-live-preview-window-xwidgets (file)
  "Preview FILE with eww.
To be used with `markdown-live-preview-window-function'."
  (xwidget-webkit-browse-url (concat "file://" file))
  (xwidget-buffer (xwidget-webkit-current-session)))

(setq markdown-live-preview-window-function #'markdown-live-preview-window-xwidgets)
(setq markdown-css-paths (list (concat "file://" (expand-file-name "~/.emacs.d/markdown/github.css"))))

(provide 'init-markdown)
