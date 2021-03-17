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

(use-package markdown-preview-mode
  :config
  (setq markdown-preview-stylesheets (list "http://thomasf.github.io/solarized-css/solarized-light.min.css")))

(provide 'init-markdown)
