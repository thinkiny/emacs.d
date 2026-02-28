;;; pdf-xwidget-mode.el --- PDF viewer using xwidget-webkit  -*- lexical-binding: t; -*-

(require 'file-server)
(require 's)
(require 'caret-xwidget)

(defconst pdf-xwidget-name-format "*PDF: %s")
(defconst pdfjs-dir (expand-file-name "~/.emacs.d/assets/pdfjs"))

(defun pdf-xwidget-view-url (file)
  (unless (s-starts-with? "/" file)
    (setq file (concat "/" file)))
  (format "http://localhost:%s%s/web/viewer.html?file=%s"
          file-server-port
          pdfjs-dir
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
  (pdf-xwidget-scroll precision-scroll-step-height))

(defun pdf-xwidget-scroll-up-page ()
  (interactive)
  (pdf-xwidget-scroll (get-precision-scroll-page-height)))

(defun pdf-xwidget-scroll-down-step ()
  (interactive)
  (pdf-xwidget-scroll (* -1 precision-scroll-step-height)))

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
    (define-key map (kbd "o") #'pdf-xwidget-toggle-sidebar)
    (define-key map (kbd "C-s") #'pdf-xwidget-toggle-findbar)
    (define-key map (kbd "C-r") #'pdf-xwidget-toggle-findbar)
    (define-key map (kbd "t") #'pdf-xwidget-toggle-toolbar)
    (define-key map (kbd "M-<") #'caret-xwidget-beginning-of-page)
    (define-key map (kbd "M->") #'caret-xwidget-end-of-page)
    map)
  "Keymap for `pdf-xwidget-mode'.")

(defun pdf-xwidget-open (&optional open-file)
  "Open a PDF file in an xwidget-webkit session.
Creates the session, activates `pdf-xwidget-mode' in the viewing buffer,
and sets up buffer-local variables."
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
        (pdf-xwidget-mode)
        (setq-local buffer-file-name local-file)
        (setq-local buffer-read-only t)
        (set-buffer-modified-p nil)
        (setq-local xwidget-webkit-buffer-name-format (format pdf-xwidget-name-format name))
        (setq-local cursor-type nil)
        (when (file-directory-p dir)
          (setq-local default-directory dir))))))

(define-derived-mode pdf-xwidget-mode xwidget-webkit-mode "PDF"
  "Major mode for reading pdf files.
\\{pdf-xwidget-mode-map}")

;;;###autoload
(defun pdf-xwidget-view (&optional file)
  "Open FILE as a PDF in an xwidget-webkit viewer.
When FILE is nil, uses `buffer-file-name' (for `auto-mode-alist' use).
Creates a new xwidget session, activates `pdf-xwidget-mode' in it,
and kills the original file-visiting buffer."
  (interactive "fPDF file: ")
  (let* ((pdf-file (expand-file-name (or file buffer-file-name)))
         (init-buf (current-buffer)))
    (pdf-xwidget-open pdf-file)
    (kill-buffer init-buf)))

(provide 'pdf-xwidget-mode)
;;; pdf-xwidget-mode.el ends here
