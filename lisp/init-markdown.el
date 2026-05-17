;; -*- lexical-binding: t; -*-

(use-package markdown-mode
  :hook (markdown-mode . my-markdown-mode)
  :config
  (setq markdown-command "multimarkdown")
  (setq markdown-fontify-code-blocks-natively t)
  (define-key markdown-mode-command-map (kbd "g") 'grip-mode))

(defun my-markdown-mode()
  (eglot-ensure))

(use-package grip-mode
  :commands grip-mode
  :config
  ;;go install github.com/chrishrb/go-grip@latest
  (setq grip-command 'go-grip)
  (setq grip-preview-use-webkit t)
  (add-to-list 'display-buffer-alist
               '("\\*WEB: go-grip*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.5))))

(defun markdown-live-preview-window-xwidgets (file)
  "Preview FILE with eww.
To be used with `markdown-live-preview-window-function'."
  (xwidget-webkit-browse-url (concat "file://" file) t)
  (xwidget-buffer (xwidget-webkit-current-session)))

(setq markdown-live-preview-window-function #'markdown-live-preview-window-xwidgets)
(setq markdown-css-paths (list (concat "file://" (expand-file-name "~/.emacs.d/markdown/github.css"))))

(provide 'init-markdown)
