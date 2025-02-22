;;; nov-xwidget-webkit.el --- nov-xwidget-webkit - the best epub reader in Emacs -*- lexical-binding: t; -*-

(require 'nov)
(require 'shr)
(require 'xwidget)
(require 'cl-lib)
(require 'evil-core nil 'noerror)

(defconst nov-xwidget-style 'light)
(defconst nov-xwidget-need-inject t)
(defconst nov-xwidget-need-remove-override-css t)
(defconst nov-xwidget-cache-dir (expand-file-name "cache/epub" user-emacs-directory))
(defvar nov-xwidget-cached-style nil)

(defcustom nov-xwidget-script ""
  "Javascript scripts used to run in the epub file."
  :group 'nov-xwidget
  :type 'string)

(defun nov-xwidget-read-style(file)
  (let ((path (expand-file-name (format "css/%s" file) user-emacs-directory)))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun nov-xwidget-style-light()
  (nov-xwidget-read-style "nov-light.css"))

(defun nov-xwidget-style-dark()
  (nov-xwidget-read-style "nov-dark.css"))

(defcustom nov-xwidget-browser-function 'nov-xwidget-webkit-browse-url-other-window
  "TODO: xwidget may not work in some systems, set it to an
alternative browser function."
  :group 'nov-xwidget
  :type browse-url--browser-defcustom-type)

(defcustom nov-xwidget-debug nil
  "Enable the debug feature."
  :group 'nov-xwidget
  :type 'directory)

(defcustom nov-xwidget-inject-output-dir
  (expand-file-name (concat user-emacs-directory ".cache/nov-xwidget/"))
  "The nov-xwidget injected output html directory."
  :group 'nov-xwidget
  :type 'directory)

(defun nov-xwidget-get-position-key()
  (if-let ((url (xwidget-webkit-uri (xwidget-webkit-current-session))))
      (concat "position-" url)))

(defun nov-xwidget-save-position()
  (if-let ((key (nov-xwidget-get-position-key)))
      (xwidget-execute-script
       (format "window.localStorage.setItem('%s', window.scrollY);" key))))

(defvar-local nov-xwidget-need-resume-position t)
(defun nov-xwidget-jump-prev-position()
  (if nov-xwidget-need-resume-position
      (if-let ((key (nov-xwidget-get-position-key)))
          (xwidget-execute-script
           (format "if(window.localStorage.getItem('%s') != null) { window.scroll(0, localStorage.getItem('%s')); }" key key)))
    (setq nov-xwidget-need-resume-position t)))

(defvar nov-xwidget-webkit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'nov-xwidget-goto-toc)
    (define-key map (kbd "L") #'nov-xwidget-list-source-file)
    (define-key map (kbd "v") #'nov-xwidget-scroll-up-page)
    (define-key map (kbd "M-v") #'nov-xwidget-scroll-down-page)
    (define-key map (kbd "N") #'nov-xwidget-next-document)
    (define-key map (kbd "P") #'nov-xwidget-previous-document)
    (define-key map (kbd "n") #'nov-xwidget-scroll-up-scan)
    (define-key map (kbd "j") #'nov-xwidget-scroll-up-scan)
    (define-key map (kbd "k") #'nov-xwidget-scroll-down-scan)
    (define-key map (kbd "s") #'nov-xwidget-scroll-up-scan)
    (define-key map (kbd "w") #'nov-xwidget-scroll-down-scan)
    (define-key map (kbd "G") #'xwidget-webkit-scroll-bottom)
    (define-key map (kbd "p") #'nov-xwidget-scroll-down-scan)
    map)
  "Keymap for `nov-xwidget-webkit-mode-map'.")

(define-derived-mode nov-xwidget-webkit-mode xwidget-webkit-mode "EPUB"
  "Major mode for reading epub files.
\\{nov-xwidget-webkit-mode-map}"
  :keymap nov-xwidget-webkit-mode-map
  (add-hook 'kill-buffer-hook #'nov-xwidget-save nil t))

(defun get-nov-xwidget-style()
  (unless nov-xwidget-cached-style
    (setq nov-xwidget-cached-style
          (pcase nov-xwidget-style
            ('light (nov-xwidget-style-light))
            ('dark (nov-xwidget-style-dark))
            ('auto (pcase (frame-parameter nil 'background-mode)
                     ('light (nov-xwidget-style-light))
                     ('dark (nov-xwidget-style-dark))
                     (_ (nov-xwidget-style-light)))))))
  nov-xwidget-cached-style)

(defun nov-xwidget-save ()
  "Delete temporary files of the current EPUB buffer."
  (when nov-temp-dir
    (let ((identifier (cdr (assq 'identifier nov-metadata)))
          (index (if (integerp nov-documents-index)
                     nov-documents-index
                   0)))
      (nov-save-place identifier index (point)))
    (nov-xwidget-save-position)))

(defun nov-xwidget-save-all ()
  "Delete temporary files of all opened EPUB buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (nov-xwidget-save))))

(defun nov-xwidget-fix-file-path (file)
  "Fix the FILE path by prefix _."
  (format "%s%s.%s"
          (or (file-name-directory file) "")
          (file-name-base file)
          (replace-regexp-in-string
           "x?html"
           "html"
           (file-name-extension file))))

;; modify dom functions
(defun nov-xwidget-is-override-css(node)
  (string-equal (dom-attr node 'href) "override_v1.css"))

(defun nov-xwidget-remove-override-css(dom)
  (if nov-xwidget-need-remove-override-css
      (if-let* ((head (car (dom-by-tag dom 'head)))
                (node (car (dom-search head 'nov-xwidget-is-override-css))))
          (dom-remove-node head node)
        ))
  dom)

(defun nov-xwidget-remove-calibre-class (cls)
  (replace-regexp-in-string "calibre[^ ]*" "" cls))

(defun nov-xwidget-remove-calibre-dom (dom)
  (cl-map 'list (lambda(x)
                  (let* ((cls (dom-attr x 'class))
                         (new-cls (nov-xwidget-remove-calibre-class cls)))
                    (dom-set-attribute x 'class new-cls)))
          (dom-elements dom 'class ".*calibre.*")))

(defun nov-xwdidget-fix-href-dom (dom)
  (cl-map 'list (lambda(x)
                  (let* ((href (dom-attr x 'href))
                         (new-href (nov-xwidget-fix-file-path href)))
                    (dom-set-attribute x 'href new-href)))
          ;; all elements that not start with http or https,
          ;; but matches htm.*
          (cl-remove-if
           (lambda(x)
             (string-match-p "https?.*"
                             (dom-attr x 'href)))
           (dom-elements dom 'href ".*htm.*"))))

(defun nov-xwidget-inject-head-elems (&optional title)
  (let ((base `(head nil
                     (meta ((charset . "utf-8")))
                     (style nil ,(get-nov-xwidget-style))
                     (script nil ,nov-xwidget-script))))
    (if title
        (append base `((title nil ,title)))
      base)))

(defun nov-xwidget-inject-new-head (dom &optional title)
  (dom-add-child-before dom (nov-xwidget-inject-head-elems title)))

(defun nov-xwidget-inject-append-head (head &optional title)
  (dolist (elm (drop 2 (nov-xwidget-inject-head-elems title)))
    (dom-append-child
     head
     elm)))

(defun nov-xwidget-inject-header(dom &optional title)
  (let ((head (car (dom-by-tag dom 'head))))
    (if head
        (nov-xwidget-inject-append-head head title)
      (nov-xwidget-inject-new-head dom title))))

(defun nov-xwidget-inject-dom (dom &optional title)
  (when dom
    (nov-xwdidget-fix-href-dom dom)
    (nov-xwidget-inject-header dom title)
    (nov-xwidget-remove-calibre-dom dom)
    (nov-xwidget-remove-override-css dom))
  dom)

(defun nov-xwidget-inject (file &optional callback)
  "Inject `nov-xwidget-script', `nov-xwidget-style-light', or `nov-xwidget-style-dark' into FILE."
  (when nov-xwidget-debug
    ;; create the nov-xwidget-inject-output-dir if not exists
    (unless (file-exists-p nov-xwidget-inject-output-dir)
      (make-directory nov-xwidget-inject-output-dir)))
  (let* ((native-path file)
         (output-native-file-name (file-name-nondirectory native-path))
         (output-native-path (expand-file-name output-native-file-name
                                               (if nov-xwidget-debug
                                                   nov-xwidget-inject-output-dir
                                                 (setq nov-xwidget-inject-output-dir (file-name-directory native-path)))))
         ;; create the html if not esists, insert the `nov-xwidget-script' as the html script
         (dom (with-temp-buffer
                (insert-file-contents native-path)
                (libxml-parse-html-region (point-min) (point-max))))
         (new-dom (nov-xwidget-inject-dom dom)))
    (if callback
        (funcall callback new-dom))
    (with-temp-file output-native-path
      (shr-dom-print new-dom)
      ;; (encode-coding-region (point-min) (point-max) 'utf-8)
      output-native-path)))

(defun nov-xwidget-inject-all-files()
  "Inject `nov-xwidget-style-dark', `nov-xwidget-style-light', or
`nov-xwidget-script' to all files in `nov-documents'. It should
be run once after the epub file is opened, so that it can fix all
the href and generate new injected-htmls beforehand. You could
also run it after modifing `nov-xwidget-style-dark',
`nov-xwidget-style-light', or `nov-xwidget-script'."
  (interactive)
  (if nov-documents
      (seq-do-indexed
       (lambda (document i)
         (let* ((file (cdr document))
                (new-file file))
           (when (string-equal "xhtml" (file-name-extension file))
             (setq new-file (format "%s%s.html"
                                    (or (file-name-directory file) "")
                                    (file-name-base file)))
             (if (file-exists-p file)
                 (rename-file file new-file)))
           (if nov-xwidget-need-inject
               (nov-xwidget-inject new-file))
           (aset nov-documents i (cons (car document) new-file))))
       nov-documents)))

(defun nov-xwidget-webkit-find-file (file &optional arg new-session)
  "Open a FILE with xwidget webkit."
  (interactive
   (list
    (pcase major-mode
      ('nov-mode
       (cdr (aref nov-documents nov-documents-index)))
      (_
       (read-file-name "Webkit find file: ")))
    current-prefix-arg))
  (let* ((path (replace-regexp-in-string
                " "
                "%20"
                (concat
                 "file:///"
                 file)))
         (final-path (if (string-equal (file-name-extension file) "ncx")
                         "about:blank"
                       path)))
    ;; workaround to view in windows
    ;; TODO it is able to support to browse in external browser
    ;; after supporting more advance html/style/scripts
    (cond
     ((eq nov-xwidget-browser-function 'nov-xwidget-webkit-browse-url-other-window)
      (nov-xwidget-webkit-browse-url-other-window final-path new-session 'switch-to-buffer)
      (unless (eq major-mode 'nov-xwidget-webkit-mode)
        (nov-xwidget-webkit-mode)))
     (t (funcall nov-xwidget-browser-function final-path)))))

(defun nov-xwidget-list-source-file ()
  "Open the source file."
  (interactive nil xwidget-webkit-mode)
  (find-file-other-window (cdr (aref nov-documents nov-documents-index))))

(defun nov-xwidget-webkit-browse-url-other-window (url &optional new-session switch-buffer-fun)
  "Ask xwidget-webkit to browse URL.
NEW-SESSION specifies whether to create a new xwidget-webkit session.
Interactively, URL defaults to the string looking like a url around point."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "URL: "
                                             (xwidget-webkit-current-url))))
  (or (featurep 'xwidget-internal)
      (user-error "Your Emacs was not compiled with xwidgets support"))
  (require 'xwidget)
  (when (stringp url)
    (if new-session
        (progn
          (switch-to-buffer (xwidget-webkit--create-new-session-buffer url #'nov-xwidget-webkit-callback))
          (xwidget-webkit-goto-uri (xwidget-webkit-last-session) url))
      (progn
        (xwidget-webkit-goto-url url)
        (if switch-buffer-fun
            (funcall switch-buffer-fun (xwidget-buffer (xwidget-webkit-current-session)))
          (pop-to-buffer (xwidget-buffer (xwidget-webkit-current-session))))))))

(defun nov-xwidget-view-index(index)
  (let* ((docs nov-documents)
         (path (cdr (aref docs index))))
    (if (eq index nov-toc-id)
        (nov-xwidget-webkit-find-file (nov-xwidget--write-toc) nil t)
      (nov-xwidget-webkit-find-file path))
    (with-current-buffer (buffer-name)
      (setq-local nov-documents-index index))))

(defun nov-xwidget-find-index-by-file (file)
  (if file
      (seq-position nov-documents
                    (url-unhex-string file)
                    (lambda (a b)
                      (string-equal b (cdr a))))))

(defun nov-xwidget-extract-file-name (uri)
  (if (and uri (string-match "file:///\\([^#]*\\)" uri))
      (match-string 1 uri)))

(defun nov-xwidget-webkit-callback (xwidget xwidget-event-type)
  "Callback for xwidgets.
XWIDGET instance, XWIDGET-EVENT-TYPE depends on the originating xwidget."
  (when (eq xwidget-event-type 'load-changed)
    (let* ((uri (xwidget-webkit-uri xwidget))
           (file (nov-xwidget-extract-file-name uri))
           (index (nov-xwidget-find-index-by-file file)))
      (if index
          (setq-local nov-documents-index index))
      (when (string-equal (nth 3 last-input-event) "load-finished")
        (nov-xwidget-jump-prev-position))))
  (xwidget-webkit-callback xwidget xwidget-event-type))

(defun nov-xwidget-view ()
  "View the current document in a xwidget webkit buffer."
  (interactive)
  (let* ((docs nov-documents)
         (index nov-documents-index)
         (toc nov-toc-id)
         (epub nov-epub-version)
         (metadata nov-metadata)
         (temp-dir nov-temp-dir)
         (file (cdr (aref docs index)))
         (epub-file-name nov-file-name))

    ;; open the html file
    (if (eq toc index)
        (nov-xwidget-webkit-find-file (nov-xwidget--write-toc) nil t)
      (nov-xwidget-webkit-find-file file nil t))

    ;; save nov related local variables
    (when (eq nov-xwidget-browser-function 'nov-xwidget-webkit-browse-url-other-window)
      (with-current-buffer (xwidget-buffer (xwidget-webkit-current-session))
        ;;(setq-local imenu-create-index-function 'my-nov-imenu-create-index)
        (setq-local nov-documents docs)
        (setq-local nov-documents-index index)
        (setq-local nov-toc-id toc)
        (setq-local nov-epub-version epub)
        (setq-local nov-temp-dir temp-dir)
        (setq-local nov-metadata metadata)
        (setq-local xwidget-webkit-buffer-name-format (format "*Epub: %s" (file-name-nondirectory epub-file-name)))))))

(defun nov-xwidget-next-document ()
  "Go to the next document and render it."
  (interactive)
  (when (< nov-documents-index (1- (length nov-documents)))
    (nov-xwidget-view-index (1+ nov-documents-index))))

(defun nov-xwidget-previous-document ()
  "Go to the previous document and render it."
  (interactive)
  (when (> nov-documents-index 0)
    (nov-xwidget-view-index (1- nov-documents-index))))

(defun nov-xwidget--walk-ncx-node (node)
  (let ((tag (dom-tag node))
        (children (seq-filter (lambda (child) (or (eq (dom-tag child) 'navPoint) (eq (dom-tag child) 'navpoint)))
                              (dom-children node))))
    (cond
     ((or (eq tag 'navMap) (eq tag 'navmap))
      (insert "<ol>\n")
      (mapc (lambda (node) (nov-xwidget--walk-ncx-node node)) children)
      (insert "</ol>\n"))
     ((or (eq tag 'navPoint) (eq tag 'navpoint))
      (let* ((label-node (esxml-query "navLabel>text,navlabel>text" node))
             (content-node (esxml-query "content" node))
             (href (nov-urldecode (dom-attr content-node 'src)))
             (label (car (dom-children label-node))))
        (when (not href)
          (error "Navigation point is missing href attribute"))
        (let ((link (format "<a href=\"%s\">%s</a>"
                            (xml-escape-string href)
                            (xml-escape-string (or label href)))))
          (if children
              (progn
                (insert (format "<li>\n%s\n<ol>\n" link))
                (mapc (lambda (node) (nov-xwidget--walk-ncx-node node))
                      children)
                (insert (format "</ol>\n</li>\n")))
            (insert (format "<li>\n%s\n</li>\n" link)))))))))

(defun nov-xwidget-ncx-to-html (path)
  "Convert NCX document at PATH to HTML."
  (let ((root (esxml-query "navMap,navmap" (nov-slurp path t))))
    (with-temp-buffer
      (nov-xwidget--walk-ncx-node root)
      (buffer-string))))

(defun nov-xwidget--write-toc-html(path html-path ncxp)
  (unless (file-exists-p html-path)
    (let* ((dom (with-temp-buffer
                  (if ncxp
                      (insert (nov-xwidget-ncx-to-html path))
                    (insert-file-contents path))
                  (libxml-parse-html-region (point-min) (point-max))))
           (new-dom (nov-xwidget-inject-dom dom "TOC")))
      (with-temp-file html-path
        (shr-dom-print new-dom)
        html-path))))

(defun nov-xwidget--write-toc()
  (let* ((docs nov-documents)
         (epub nov-epub-version)
         (ncxp (version< nov-epub-version "3.0"))
         (index (nov-find-document (lambda (doc) (eq (car doc) nov-toc-id))))
         (path (cdr (aref docs index)))
         (html-path (expand-file-name "toc.html" (file-name-directory path))))
    (when (not index)
      (error "Couldn't locate TOC"))
    (nov-xwidget--write-toc-html path html-path ncxp)
    html-path))

(defun nov-xwidget-goto-toc ()
  "Go to the TOC index and render the TOC document."
  (interactive)
  (let ((index nov-documents-index))
    (nov-xwidget-save-position)
    (nov-xwidget-webkit-find-file (nov-xwidget--write-toc))
    (with-current-buffer (buffer-name)
      (setq-local nov-documents-index index))))

;; Window size change functions; this arrangement below is not ideal, as it basically replicates the code in xwidget-webkit.el, which changes the size of the xwidget *if* the window's mode is xwidget-webkit-mode.
;; In the future, xwidget-webkit should accomodate generalized "windows containing a webkit instance" rather than just xwidget-webkit-mode specifically for this.
(defun nov-xwidget-webkit-auto-adjust-size (window)
  "Adjust the size of the webkit widget in the given nov-webkit WINDOW."
  (with-current-buffer (window-buffer window)
    (when (eq major-mode 'nov-xwidget-webkit-mode)
      (let ((xwidget (xwidget-webkit-current-session)))
        (xwidget-webkit-adjust-size-to-window xwidget window)))))

(defun nov-xwidget-webkit-adjust-size-in-frame (frame)
  "Dynamically adjust webkit widget for all nov-webkit windows of the FRAME."
  (walk-windows 'nov-xwidget-webkit-auto-adjust-size 'no-minibuf frame))

(eval-after-load 'nov-xwidget-webkit-mode
  (add-to-list 'window-size-change-functions
               'nov-xwidget-webkit-adjust-size-in-frame))

(defun nov-xwidget-next-scan-or-page-cb(end)
  (setq nov-xwidget-need-resume-position nil)
  (if (s-equals-p end "1")
      (nov-xwidget-next-document)
    (xwidget-webkit-scroll-up percision-scroll-scan-height)))

(defun nov-xwidget-scroll-up-page()
  (interactive)
  (xwidget-webkit-scroll-up (get-precision-scroll-page-height)))

(defun nov-xwidget-scroll-down-page()
  (interactive)
  (xwidget-webkit-scroll-down (get-precision-scroll-page-height)))

(defun nov-xwidget-scroll-up-scan()
  (interactive)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   "(function () {
    if (document.body.clientHeight + window.scrollY >= document.body.scrollHeight) {
        return \"1\";
    } else {
        return \"0\";
    }
})();" #'nov-xwidget-next-scan-or-page-cb))

(defun nov-xwidget-previous-scan-or-page-cb(end)
  (setq nov-xwidget-need-resume-position nil)
  (if (s-equals-p end "1")
      (progn
        (nov-xwidget-previous-document)
        (run-at-time 0.1 nil #'xwidget-webkit-scroll-bottom))
    (xwidget-webkit-scroll-down percision-scroll-scan-height)))

(defun nov-xwidget-scroll-down-scan()
  (interactive)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   "(function () {
    if (window.scrollY == 0) {
        return \"1\";
    } else {
        return \"0\";
    }
})();" #'nov-xwidget-previous-scan-or-page-cb))

(provide 'nov-xwidget-webkit)
;;; nov-xwidget-webkit.el ends here
