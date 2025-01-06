;;; pdf-xwidget-mode

(defconst pdf-xwidget-name-format "*PDF: %s")

;; file server
(defvar file-server-port "8123")
(defvar file-server-dir (expand-file-name "~/.emacs.d/file-server"))

(defun file-server-start()
  (interactive)
  (unless (get-process "file-server")
    (set-process-sentinel
     (start-process "file-server" nil "python3" (concat file-server-dir "/server.py") file-server-port)
     (lambda (process event)
       (princ (format "%s: %s" process event))))))

(defun filer-server-pdf-view-url (file)
  (format "http://localhost:%s%s/pdfjs/web/viewer.html?file=%s"
          file-server-port
          file-server-dir
          (url-hexify-string
          (format
           "http://localhost:%s%s"
           file-server-port
           file))))

;; pdf xwidget scroll
(defun pdf-xwidget-scroll(lines)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   (format
    "document.getElementById(\"viewerContainer\").scrollBy({top: %d, behavior: 'smooth'})"
    (* lines (window-font-height)))))

(defun pdf-xwidget-scroll-up-line()
  (interactive)
  (pdf-xwidget-scroll xwidget-scroll-line))

(defun pdf-xwidget-scroll-up()
  (interactive)
  (pdf-xwidget-scroll xwidget-scroll-page))

(defun pdf-xwidget-scroll-down-line()
  (interactive)
  (pdf-xwidget-scroll (* -1 xwidget-scroll-line)))

(defun pdf-xwidget-scroll-down()
  (interactive)
  (pdf-xwidget-scroll (* -1 xwidget-scroll-page)))

;; pdf xwidget toolbar
(defvar-local pdf-xwidget-toolbar-show t)
(defconst pdf-xwidget-toggle-sidebar-script
  "document.getElementById(\"sidebarToggleButton\").click();")

(defconst pdf-xwidget-hide-toolbar-script
  (format "if(document.getElementById('sidebarContainer').checkVisibility({
  checkOpacity: true,
  checkVisibilityCSS: true
})) { %s }
   document.getElementById('toolbarContainer').style.visibility='hidden';
   document.getElementById('viewerContainer').style.inset='0';
" pdf-xwidget-toggle-sidebar-script))

(defconst pdf-xwidget-show-toolbar-script
  "document.getElementById('toolbarContainer').style.visibility='visible';
   document.getElementById('viewerContainer').style.inset='';")

(defun pdf-xwidget-toggle-sidebar-scripts()
  (if pdf-xwidget-toolbar-show
      (list pdf-xwidget-toggle-sidebar-script)
    (list pdf-xwidget-show-toolbar-script pdf-xwidget-toggle-sidebar-script)))

(defun pdf-xwidget-toggle-sidebar()
  (interactive)
  (xwidget-execute-scripts
   (pdf-xwidget-toggle-sidebar-scripts))
  (setq-local pdf-xwidget-toolbar-show t))

(defun pdf-xwidget-toggle-toolbar-scripts()
  (if pdf-xwidget-toolbar-show
      (list pdf-xwidget-hide-toolbar-script)
    (list pdf-xwidget-show-toolbar-script)))

(defun pdf-xwidget-toggle-toolbar()
  (interactive)
  (xwidget-execute-scripts
   (pdf-xwidget-toggle-toolbar-scripts))
  (setq-local pdf-xwidget-toolbar-show (not pdf-xwidget-toolbar-show)))


(defun pdf-xwidget-reload()
  (interactive)
  (xwidget-webkit-reload)
  (run-with-timer 3 nil #'pdf-xwidget-update-title))

(defun pdf-xwidget-update-title(&optional session)
  (interactive)
  (let* ((xwidget (or session (xwidget-webkit-current-session)))
         (title (xwidget-webkit-title xwidget))
         (buffer (xwidget-buffer xwidget))
         (newbuf-name (format pdf-xwidget-name-format title)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (unless (string= newbuf-name (buffer-name))
            (force-mode-line-update)
            (rename-buffer newbuf-name))))))

(defvar pdf-xwidget-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'pdf-xwidget-toggle-sidebar)
    (define-key map (kbd "n") #'pdf-xwidget-scroll-up-line)
    (define-key map (kbd "t") #'pdf-xwidget-toggle-toolbar)
    (define-key map (kbd "p") #'pdf-xwidget-scroll-down-line)
    (define-key map (kbd "j") #'pdf-xwidget-scroll-up-line)
    (define-key map (kbd "k") #'pdf-xwidget-scroll-down-line)
    (define-key map (kbd "s") #'pdf-xwidget-scroll-up-line)
    (define-key map (kbd "u") #'pdf-xwidget-reload)
    (define-key map (kbd "w") #'pdf-xwidget-scroll-down-line)
    (define-key map (kbd "v") #'pdf-xwidget-scroll-up)
    (define-key map (kbd "M-v") 'pdf-xwidget-scroll-down)
    (define-key map (kbd "g") #'xwidget-webkit-browse-url)
    (define-key map (kbd "M-c") 'xwidget-webkit-copy-selection-as-kill)
    (define-key map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
    map)
  "Keymap for `pdf-xwidget-mode-map'.")

(define-derived-mode pdf-xwidget-mode special-mode "PDF"
  "Major mode for reading pdf files.
\\{pdf-xwidget-mode-map}"
  :keymap pdf-xwidget-mode-map
  (file-server-start)
  (let ((url (filer-server-pdf-view-url (buffer-file-name)))
        (dummy-buf (current-buffer)))
    (xwidget-webkit-new-session url)
    (run-with-timer 3 nil #'pdf-xwidget-update-title (xwidget-webkit-last-session))
    (use-local-map pdf-xwidget-mode-map)
    (read-only-mode)
    (kill-buffer dummy-buf)))

(provide 'pdf-xwidget-mode)
;;; pdf-xwidget.el ends here
