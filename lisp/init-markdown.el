;; -*- lexical-binding: t; -*-

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :hook (markdown-mode . eglot-ensure)
  :config
  (setq markdown-command "multimarkdown"
        markdown-fontify-code-blocks-natively t))

;; preview
;; (use-package markdown-xwidget
;;   :after markdown-mode
;;   :vc (:url "https://github.com/cfclrk/markdown-xwidget" :rev "main")
;;   :bind (:map markdown-mode-command-map
;;               ("x" . markdown-xwidget-preview-mode))
;;   :config
;;   (setq markdown-xwidget-github-theme "light"))

(defun markdown-display-grip-preview-in-current-window
    (original-function url)
  (let ((display-buffer-overriding-action
         '((display-buffer-same-window))))
    (if grip-preview-in-webkit
        (xwidget-webkit-browse-open-url url t)
      (funcall original-function url))))

(use-package grip-mode
  :after markdown-mode
  :bind (:map markdown-mode-command-map
              ("p" . grip-browse-preview))
  :config
  ;;go install github.com/chrishrb/go-grip@latest
  (setq grip-command 'go-grip)
  (advice-add #'grip--browse-url :around
              #'markdown-display-grip-preview-in-current-window))

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

(provide 'init-markdown)
