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

;; go-grip
;; https://github.com/thinkiny/go-grip branch: emacs
(use-package grip-mode
  :vc (:url "https://github.com/thinkiny/grip-mode.git" :rev "emacs")
  :after markdown-mode
  :bind (:map markdown-mode-command-map
              ("p" . grip-browse-preview))
  :config
  (setq grip-command 'go-grip
        grip-project-root-function #'markdown-grip-project-root
        grip-preview-display-function
        #'markdown-display-grip-preview-in-current-window))

(defun markdown-grip-project-root (preview-file)
  "Return Projectile's project root for PREVIEW-FILE, or nil."
  (projectile-project-root (file-name-directory preview-file)))

(defun markdown-grip-preview-buffer-for-server ()
  "Return the xwidget-webkit buffer viewing this buffer's grip server, else nil."
  (let ((server-url-prefix (format "http://%s:%d/" grip-preview-host grip--port)))
    (catch 'found
      (dolist (candidate-buffer (buffer-list))
        (with-current-buffer candidate-buffer
          (let ((session (and (derived-mode-p 'xwidget-webkit-mode)
                              (xwidget-at (point-min)))))
            (when (and session
                       (string-prefix-p server-url-prefix
                                        (or (xwidget-webkit-uri session) "")))
              (throw 'found candidate-buffer)))))
      nil)))

(defun markdown-display-grip-preview-in-current-window (url preview-file)
  "Display URL in this window and map its xwidget buffer to PREVIEW-FILE."
  (when (and grip-preview-in-webkit
             (display-graphic-p)
             (featurep 'xwidget-internal))
    (let ((display-buffer-overriding-action
           '((display-buffer-same-window))))
      (if-let* ((preview-buffer (markdown-grip-preview-buffer-for-server)))
          (with-current-buffer preview-buffer
            (let ((session (xwidget-at (point-min))))
              (switch-to-buffer preview-buffer)
              (unless (equal (xwidget-webkit-uri session) url)
                (xwidget-webkit-goto-uri session url))
              (map-buffer-to-local-file preview-file)))
        (xwidget-webkit-browse-open-url url t preview-file)))
    t))

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
