;; -*- lexical-binding: t; -*-

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :hook (markdown-mode . my-markdown-mode-hook)
  :config
  (setq markdown-command "multimarkdown")
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-italic-underscore nil)
  (define-key markdown-mode-command-map (kbd "g") 'grip-mode))

(defun my-markdown-mode-hook()
  (eglot-ensure))

;; fmt-table
(use-package fmt-table
  :load-path "lisp"
  :after markdown-mode
  :config
  (define-key markdown-mode-command-map (kbd "r") 'fmt-table-at-point)
  (define-key markdown-mode-map (kbd "C-c '") 'markdown-edit-field-or-code-block))

(defun markdown-edit-field-or-code-block ()
  "Edit table cell if in a markdown table, otherwise edit code block."
  (interactive)
  (if (markdown-table-at-point-p)
      (call-interactively #'fmt-table-edit-field)
    (call-interactively #'markdown-edit-code-block)))

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
