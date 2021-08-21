(defcustom markdown-enable-strapdown t
  "Whether to use strapdown"
  :group 'markdown
  :type 'boolean)

(defcustom markdown-strapdown-style "united"
  "Default strapdown style"
  :group 'markdown
  :type 'string)

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode))

(defun markdown-live-preview-window-xwidgets (file)
  "Preview FILE with eww.
To be used with `markdown-live-preview-window-function'."
  (xwidget-webkit-browse-url (concat "file://" file))
  (xwidget-buffer (xwidget-webkit-current-session)))

(setq markdown-live-preview-window-function #'markdown-live-preview-window-xwidgets)
(setq markdown-css-paths (list (concat "file://" (expand-file-name "~/.emacs.d/markdown/github.css"))))

(provide 'init-markdown)
