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
  :commands (markdown-mode gfm-mode)
  :config
  (setq markdown-xhtml-header-content "")
  (setq markdown-xhtml-body-epilogue "<script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>")
  (if markdown-enable-strapdown
      (defalias #'markdown-add-xhtml-header-and-footer #'markdown-add-xhtml-header-and-footer-strapdown)))

(defun markdown-add-xhtml-header-and-footer-strapdown (title)
  "Wrap XHTML header and footer with given TITLE around current buffer."
  (goto-char (point-min))
  (insert "<!DOCTYPE html>\n<html><head>\n<title>")
  (insert title)
  (insert "</title>\n")
  (unless (= (length markdown-content-type) 0)
    (insert
     (format
      "<meta http-equiv=\"Content-Type\" content=\"%s;charset=%s\"/>\n"
      markdown-content-type
      (or (and markdown-coding-system
               (coding-system-get markdown-coding-system
                                  'mime-charset))
          (coding-system-get buffer-file-coding-system
                             'mime-charset)
          "utf-8"))))
  (if (> (length markdown-css-paths) 0)
      (insert (mapconcat #'markdown-stylesheet-link-string
                         markdown-css-paths "\n")))
  (when (> (length markdown-xhtml-header-content) 0)
    (insert markdown-xhtml-header-content))
  (insert "\n</head>\n")
  (insert (format "<xmp theme=\"%s\" style=\"display:none;\">" markdown-strapdown-style))
  (when (> (length markdown-xhtml-body-preamble) 0)
    (insert markdown-xhtml-body-preamble "\n"))
  (goto-char (point-max))
  (when (> (length markdown-xhtml-body-epilogue) 0)
    (insert "\n" markdown-xhtml-body-epilogue))
  ;;(insert "</xmp>\n<script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script>")
  (insert "</xmp>\n<script src=\"http://cdn.ztx.io/strapdown/strapdown.min.js\"></script>")
  (insert "\n</html>"))

(provide 'init-markdown)
