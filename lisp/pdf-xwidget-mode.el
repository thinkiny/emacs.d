;;; pdf-xwidget-mode.el --- PDF viewer using xwidget-webkit  -*- lexical-binding: t; -*-

(require 'file-server)
(require 's)

(defconst pdf-xwidget-name-format "*PDF: %s")

(defun pdf-xwidget-view-url (file)
  (unless (s-starts-with? "/" file)
    (setq file (concat "/" file)))
  (format "http://localhost:%s%s/pdfjs/web/viewer.html?file=%s"
          file-server-port
          file-server-dir
          (url-hexify-string
           (format "http://localhost:%s%s"
                   file-server-port
                   file))))

(defun pdf-xwidget-view-name (file)
  (if (s-starts-with? "http" file)
      file
    (file-name-nondirectory file)))

;; pdf xwidget scroll
(defun pdf-xwidget-scroll (pixels)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   (format
    "document.getElementById(\"viewerContainer\").scrollBy({top: %d, behavior: 'instant'})"
    pixels)))

(defun pdf-xwidget-scroll-up-step ()
  (interactive)
  (pdf-xwidget-scroll percision-scroll-step-height))

(defun pdf-xwidget-scroll-up-page ()
  (interactive)
  (pdf-xwidget-scroll (get-precision-scroll-page-height)))

(defun pdf-xwidget-scroll-down-step ()
  (interactive)
  (pdf-xwidget-scroll (* -1 percision-scroll-step-height)))

(defun pdf-xwidget-scroll-down-page ()
  (interactive)
  (pdf-xwidget-scroll (* -1 (get-precision-scroll-page-height))))

;; pdf xwidget toolbar
(defvar-local pdf-xwidget-toolbar-show t)
(defconst pdf-xwidget-toggle-sidebar-script
  "document.getElementById(\"viewsManagerToggleButton\").click();")

(defconst pdf-xwidget-hide-toolbar-script
  (format "if(document.getElementById('viewsManager').checkVisibility({
  checkOpacity: true,
  checkVisibilityCSS: true
})) { %s }
   document.getElementById('toolbarContainer').style.visibility='hidden';
   document.getElementById('viewerContainer').style.inset='0';
" pdf-xwidget-toggle-sidebar-script))

(defconst pdf-xwidget-show-toolbar-script
  "document.getElementById('toolbarContainer').style.visibility='visible';
   document.getElementById('viewerContainer').style.inset='';")

(defun pdf-xwidget-toggle-sidebar-scripts ()
  (if pdf-xwidget-toolbar-show
      (list pdf-xwidget-toggle-sidebar-script)
    (list pdf-xwidget-show-toolbar-script pdf-xwidget-toggle-sidebar-script)))

(defun pdf-xwidget-toggle-sidebar ()
  (interactive)
  (xwidget-execute-scripts
   (pdf-xwidget-toggle-sidebar-scripts))
  (setq-local pdf-xwidget-toolbar-show t))

(defun pdf-xwidget-toggle-toolbar-scripts ()
  (if pdf-xwidget-toolbar-show
      (list pdf-xwidget-hide-toolbar-script)
    (list pdf-xwidget-show-toolbar-script)))

(defun pdf-xwidget-toggle-toolbar ()
  (interactive)
  (xwidget-execute-scripts
   (pdf-xwidget-toggle-toolbar-scripts))
  (setq-local pdf-xwidget-toolbar-show (not pdf-xwidget-toolbar-show)))

;; findbar
(defconst pdf-xwidget-toggle-findbar-script
  "document.getElementById(\"viewFindButton\").click();")

(defun pdf-xwidget-toggle-findbar-scripts ()
  (if pdf-xwidget-toolbar-show
      (list pdf-xwidget-toggle-findbar-script)
    (list pdf-xwidget-show-toolbar-script pdf-xwidget-toggle-findbar-script)))

(defun pdf-xwidget-toggle-findbar ()
  (interactive)
  (xwidget-execute-scripts
   (pdf-xwidget-toggle-findbar-scripts))
  (setq-local pdf-xwidget-toolbar-show t))

(defun pdf-xwidget-update-title (&optional session)
  (interactive)
  (let* ((xwidget (or session (xwidget-webkit-current-session)))
         (title (xwidget-webkit-title xwidget))
         (buffer (xwidget-buffer xwidget))
         (newbuf-name (format pdf-xwidget-name-format title)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless (string= newbuf-name (buffer-name))
          (force-mode-line-update)
          (rename-buffer newbuf-name))))))

(defvar pdf-xwidget-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map xwidget-webkit-mode-map)
    (define-key map (kbd "o") #'pdf-xwidget-toggle-sidebar)
    (define-key map (kbd "C-s") #'pdf-xwidget-toggle-findbar)
    (define-key map (kbd "C-r") #'pdf-xwidget-toggle-findbar)
    (define-key map (kbd "n") #'pdf-xwidget-scroll-up-step)
    (define-key map (kbd "C-n") #'pdf-xwidget-scroll-up-step)
    (define-key map (kbd "C-p") #'pdf-xwidget-scroll-down-step)
    (define-key map (kbd "t") #'pdf-xwidget-toggle-toolbar)
    (define-key map (kbd "p") #'pdf-xwidget-scroll-down-step)
    (define-key map (kbd "j") #'pdf-xwidget-scroll-up-step)
    (define-key map (kbd "k") #'pdf-xwidget-scroll-down-step)
    (define-key map (kbd "s") #'pdf-xwidget-scroll-up-step)
    (define-key map (kbd "w") #'pdf-xwidget-scroll-down-step)
    (define-key map (kbd "v") #'pdf-xwidget-scroll-up-page)
    (define-key map (kbd "SPC") #'pdf-xwidget-scroll-up-page)
    (define-key map (kbd "M-v") #'pdf-xwidget-scroll-down-page)
    (define-key map (kbd "C-v") #'pdf-xwidget-scroll-up-page)
    map)
  "Keymap for `pdf-xwidget-mode-map'.")

(defun pdf-xwidget-open (&optional open-file)
  (interactive)
  (file-server-start)
  (let* ((file (or open-file (read-from-minibuffer "open pdf: ")))
         (url (pdf-xwidget-view-url file))
         (name (pdf-xwidget-view-name file))
         (dir (file-name-directory file))
         (local-file (when (and (stringp file)
                                (not (string-match-p "\\`[A-Za-z]+://" file)))
                       (expand-file-name file))))
    (xwidget-webkit-new-session url)
    (when-let* ((session (xwidget-webkit-last-session))
                (buffer (xwidget-buffer session)))
      (with-current-buffer buffer
        ;; Keep URL-backed PDFs without a file path; MCP should track local files only.
        (setq-local buffer-file-name local-file)
        (setq-local buffer-read-only t)
        (set-buffer-modified-p nil)))

    (when (file-directory-p dir)
      (setq default-directory dir))
    (setq-local xwidget-webkit-buffer-name-format (format pdf-xwidget-name-format name))
    (setq-local cursor-type nil)
    (use-local-map pdf-xwidget-mode-map)))

(define-derived-mode pdf-xwidget-mode special-mode "PDF"
  "Major mode for reading pdf files."
  (let* ((init-buf (current-buffer)))
    (pdf-xwidget-open (buffer-file-name))
    (kill-buffer init-buf)))

(provide 'pdf-xwidget-mode)
;;; pdf-xwidget-mode.el ends here
