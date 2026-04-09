;;; nov-xwidget-mode.el --- EPUB reader using xwidget-webkit -*- lexical-binding: t; -*-

(require 'nov)
(require 'shr)
(require 'xwidget)
(require 'cl-lib)
(require 's)
(require 'caret-xwidget)

(defconst nov-xwidget-need-inject t)
(defconst nov-xwidget-need-remove-override-css t)
(defconst nov-xwidget-cache-dir (expand-file-name "cache/epub" user-emacs-directory))

(defvar-local nov-xwidget-toc-path nil)
(defvar-local nov-xwidget-need-resume-position t)

(defun nov-xwidget--sanitize-index (documents index)
  "Return a valid index for DOCUMENTS, derived from INDEX."
  (let ((len (length documents)))
    (cond
     ((<= len 0) 0)
     ((not (integerp index)) 0)
     ((< index 0) 0)
     ((>= index len) (1- len))
     (t index))))

;;; Style

(defun nov-xwidget--style-href ()
  (concat "file://"
          (expand-file-name "assets/css/nov-override.css"
                            user-emacs-directory)))

;;; Position save/restore

(defun nov-xwidget--position-key ()
  (when-let* ((uri (xwidget-webkit-uri (xwidget-webkit-current-session)))
              (url (url-generic-parse-url uri))
              (decode-url (url-unhex-string (url-filename url))))
    (concat "epub-pos-"
            (string-trim-left decode-url (concat "/" nov-xwidget-cache-dir "/")))))

(defun nov-xwidget-save-position ()
  (interactive)
  (when-let* ((key (nov-xwidget--position-key)))
    (xwidget-execute-script
     (format "window.localStorage.setItem('%s', window.scrollY);" key))))

(defun nov-xwidget-reset-position ()
  (interactive)
  (when-let* ((key (nov-xwidget--position-key)))
    (xwidget-execute-script
     (format "window.localStorage.removeItem('%s');" key))))

(defun nov-xwidget--restore-scroll-position ()
  (when nov-xwidget-need-resume-position
    (when-let* ((key (nov-xwidget--position-key)))
      (xwidget-execute-script
       (format "if(window.localStorage.getItem('%s') != null) { window.scroll(0, localStorage.getItem('%s')); }" key key))))
  (setq nov-xwidget-need-resume-position t))

;;; Keymap and mode

(defvar nov-xwidget-webkit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'nov-xwidget-goto-toc)
    (define-key map (kbd "V") #'nov-xwidget-view-source-file)
    (define-key map (kbd "N") #'nov-xwidget-next-document)
    (define-key map (kbd "P") #'nov-xwidget-previous-document)
    (define-key map (kbd "G") #'xwidget-webkit-scroll-bottom)
    map)
  "Keymap for `nov-xwidget-webkit-mode'.")

(define-derived-mode nov-xwidget-webkit-mode xwidget-webkit-mode "EPUB"
  "Major mode for reading epub files.
\\{nov-xwidget-webkit-mode-map}"
  :keymap nov-xwidget-webkit-mode-map
  (add-hook 'kill-buffer-hook #'nov-xwidget--save nil t)
  (add-hook 'kill-emacs-hook #'nov-xwidget--save nil t))

(defun nov-xwidget--save ()
  "Save current reading position."
  (when nov-temp-dir
    (let ((identifier (cdr (assq 'identifier nov-metadata)))
          (index (if (integerp nov-documents-index)
                     nov-documents-index
                   0)))
      (nov-save-place identifier index (point)))
    (nov-xwidget-save-position)))

;;; DOM transformation

(defun nov-xwidget--normalize-filepath (file)
  (pcase (file-name-extension file)
    ("xhtml" (format "%s%s.html"
                     (or (file-name-directory file) "")
                     (file-name-base file)))
    ("ncx" nov-xwidget-toc-path)
    (_ file)))

(defun nov-xwidget--normalize-href (file)
  (format "%s%s.%s"
          (or (file-name-directory file) "")
          (file-name-base file)
          (replace-regexp-in-string
           "x?html"
           "html"
           (file-name-extension file))))

(defun nov-xwidget--normalize-dom-hrefs (dom)
  (cl-map 'list (lambda (x)
                  (let ((new-href (nov-xwidget--normalize-href (dom-attr x 'href))))
                    (dom-set-attribute x 'href new-href)))
          (cl-remove-if
           (lambda (x)
             (string-match-p "https?.*" (dom-attr x 'href)))
           (dom-elements dom 'href ".*htm.*"))))

(defun nov-xwidget--inject-head (dom &optional title)
  (let ((head (car (dom-by-tag dom 'head)))
        (elems `((meta ((charset . "utf-8")))
                 (link ((rel . "stylesheet") (type . "text/css")
                        (href . ,(nov-xwidget--style-href)))))))
    (when title (push `(title nil ,title) elems))
    (if head
        (dolist (e (nreverse elems)) (dom-append-child head e))
      (dom-add-child-before dom `(head nil ,@(nreverse elems))))))

(defun nov-xwidget--override-css-p (node)
  (string-equal (dom-attr node 'href) "override_v1.css"))

(defun nov-xwidget--remove-override-css (dom)
  (when nov-xwidget-need-remove-override-css
    (when-let* ((head (car (dom-by-tag dom 'head)))
                (node (car (dom-search head 'nov-xwidget--override-css-p))))
      (dom-remove-node head node))))

(defun nov-xwidget--remove-calibre-classes (dom)
  "Remove calibre-generated class names from DOM elements."
  (dolist (x (dom-elements dom 'class ".*calibre.*"))
    (let ((new-cls (replace-regexp-in-string "calibre[^ ]*" "" (dom-attr x 'class))))
      (dom-set-attribute x 'class new-cls))))

(defun nov-xwidget--transform-dom (dom &optional title)
  "Transform DOM: fix hrefs, inject head, remove calibre classes, remove override CSS."
  (when dom
    (nov-xwidget--normalize-dom-hrefs dom)
    (nov-xwidget--inject-head dom title)
    (nov-xwidget--remove-calibre-classes dom)
    (nov-xwidget--remove-override-css dom))
  dom)

;;; DOM printing

(defun nov-dom-print (dom)
  "Print DOM at point as HTML/XML."
  (insert (format "<%s" (dom-tag dom)))
  (let ((attr (dom-attributes dom)))
    (dolist (elem attr)
      (insert (if (and (memq (car elem)
                             '(async autofocus autoplay checked
                                     contenteditable controls default
                                     defer disabled formNoValidate frameborder
                                     hidden ismap itemscope loop
                                     multiple muted nomodule novalidate open
                                     readonly required reversed
                                     cscoped selected typemustmatch))
                       (cdr elem))
                  (format " %s" (car elem))
                (format " %s=\"%s\"" (car elem)
                        (url-insert-entities-in-string (cdr elem)))))))
  (let ((children (dom-children dom)))
    (if (null children)
        (insert (format "></%s>" (dom-tag dom)))
      (insert ">")
      (dolist (child children)
        (if (stringp child)
            (insert (url-insert-entities-in-string child))
          (nov-dom-print child)))
      (insert (format "</%s>" (dom-tag dom))))))

;;; File injection

(defun nov-xwidget--transform-file (file &optional callback)
  "Inject styles into FILE, optionally running CALLBACK on the DOM."
  (let* ((dom (with-temp-buffer
                (insert-file-contents file)
                (libxml-parse-html-region (point-min) (point-max))))
         (new-dom (nov-xwidget--transform-dom dom)))
    (when callback
      (funcall callback new-dom))
    (with-temp-file file
      (insert "<!DOCTYPE html>\n")
      (nov-dom-print new-dom))
    file))

(defun nov-xwidget--transform-all-files ()
  "Inject styles into all files in `nov-documents'."
  (interactive)
  (when nov-documents
    (seq-do-indexed
     (lambda (document i)
       (let* ((file (cdr document))
              (new-file (nov-xwidget--normalize-filepath file)))
         (unless (file-exists-p new-file)
           (rename-file file new-file))
         (when nov-xwidget-need-inject
           (nov-xwidget--transform-file new-file))
         (aset nov-documents i (cons (car document) new-file))))
     nov-documents)))

;;; Browsing

(defun nov-xwidget--browse-url (url &optional new-session)
  "Browse URL in xwidget webkit. Create new session with callback when NEW-SESSION is non-nil."
  (if new-session
      (progn
        (switch-to-buffer (xwidget-webkit--create-new-session-buffer url #'nov-xwidget--webkit-callback))
        (xwidget-webkit-goto-uri (xwidget-webkit-last-session) url))
    (xwidget-webkit-goto-url url)
    (switch-to-buffer (xwidget-buffer (xwidget-webkit-current-session)))))

(defun nov-xwidget-view-source-file ()
  "Open the source file."
  (interactive nil nov-xwidget-webkit-mode)
  (find-file-other-window (cdr (aref nov-documents nov-documents-index))))

;;; Navigation

(defun nov-xwidget--find-index-by-file (file)
  (when file
    (seq-position nov-documents
                  (decode-coding-string (url-unhex-string file) 'utf-8)
                  (lambda (a b)
                    (string-collate-equalp b (nov-xwidget--normalize-filepath (cdr a)))))))

(defun nov-xwidget--extract-filepath (uri)
  (when (and uri (string-match "file:///\\([^#]*\\)" uri))
    (match-string 1 uri)))

(defun nov-xwidget--skip-restore-p (uri)
  (or (eq nov-documents-index nov-toc-id)
      (s-contains? "#" uri)))

(defun nov-xwidget--webkit-callback (xwidget xwidget-event-type)
  "Callback for xwidgets.
XWIDGET instance, XWIDGET-EVENT-TYPE depends on the originating xwidget."
  (when (and (eq xwidget-event-type 'load-changed)
             (string-equal (nth 3 last-input-event) "load-finished"))
    (when-let* ((uri (xwidget-webkit-uri xwidget))
                (file (nov-xwidget--extract-filepath uri)))
      (unless (nov-xwidget--skip-restore-p uri)
        (nov-xwidget--restore-scroll-position))
      (when-let* ((index (nov-xwidget--find-index-by-file file)))
        (setq-local nov-documents-index index))))
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
         (epub-file-name nov-file-name)
         (toc-path nov-xwidget-toc-path)
         (path (nov-xwidget--normalize-filepath file))
         (url (concat "file:///" path)))

    (nov-xwidget--browse-url url t)

    (with-current-buffer (xwidget-buffer (xwidget-webkit-current-session))
      (nov-xwidget-webkit-mode)
      (setq-local nov-xwidget-toc-path toc-path)
      (setq-local nov-documents docs)
      (setq-local nov-documents-index index)
      (setq-local nov-toc-id toc)
      (setq-local nov-epub-version epub)
      (setq-local nov-temp-dir temp-dir)
      (setq-local nov-metadata metadata)
      (setq-local buffer-file-name epub-file-name)
      (setq-local default-directory (file-name-directory epub-file-name))
      (setq-local cursor-type nil)
      (setq-local caret-xwidget-next-page-function #'nov-xwidget-next-document)
      (setq-local caret-xwidget-previous-page-function
                  (lambda ()
                    (nov-xwidget-previous-document)
                    (run-at-time 0.1 nil #'xwidget-webkit-scroll-bottom)))
      (set-buffer-modified-p nil)
      (setq-local xwidget-webkit-buffer-name-format (format "*Epub: %s" (file-name-nondirectory epub-file-name))))))

(defun nov-xwidget--goto-index (index)
  (let* ((docs nov-documents)
         (path (cdr (aref docs index)))
         (url (concat "file:///" (nov-xwidget--normalize-filepath path))))
    (nov-xwidget--browse-url url)
    (setq-local nov-documents-index index)))

(defun nov-xwidget-next-document ()
  "Go to the next document and render it."
  (interactive)
  (unless (integerp nov-documents-index)
    (setq nov-documents-index 0))
  (when (< nov-documents-index (1- (length nov-documents)))
    (nov-xwidget--goto-index (1+ nov-documents-index))))

(defun nov-xwidget-previous-document ()
  "Go to the previous document and render it."
  (interactive)
  (unless (integerp nov-documents-index)
    (setq nov-documents-index 0))
  (when (> nov-documents-index 0)
    (nov-xwidget--goto-index (1- nov-documents-index))))

;;; TOC

(defun nov-xwidget--walk-ncx-node (node)
  (let ((tag (dom-tag node))
        (children (seq-filter (lambda (child) (or (eq (dom-tag child) 'navPoint) (eq (dom-tag child) 'navpoint)))
                              (dom-children node))))
    (cond
     ((or (eq tag 'navMap) (eq tag 'navmap))
      (insert "<ol>\n")
      (mapc #'nov-xwidget--walk-ncx-node children)
      (insert "</ol>\n"))
     ((or (eq tag 'navPoint) (eq tag 'navpoint))
      (let* ((label-node (esxml-query "navLabel>text,navlabel>text" node))
             (content-node (esxml-query "content" node))
             (href (nov-urldecode (dom-attr content-node 'src)))
             (label (car (dom-children label-node))))
        (unless href
          (error "Navigation point is missing href attribute"))
        (let ((link (format "<a href=\"%s\">%s</a>"
                            (xml-escape-string href)
                            (xml-escape-string (or label href)))))
          (if children
              (progn
                (insert (format "<li>\n%s\n<ol>\n" link))
                (mapc #'nov-xwidget--walk-ncx-node children)
                (insert "</ol>\n</li>\n"))
            (insert (format "<li>\n%s\n</li>\n" link)))))))))

(defun nov-xwidget--ncx-to-html (path)
  "Convert NCX document at PATH to HTML."
  (let ((root (esxml-query "navMap,navmap" (nov-slurp path t))))
    (with-temp-buffer
      (nov-xwidget--walk-ncx-node root)
      (buffer-string))))

(defun nov-xwidget--build-toc-html (path html-path ncxp)
  (unless (file-exists-p html-path)
    (let* ((dom (with-temp-buffer
                  (if ncxp
                      (insert (nov-xwidget--ncx-to-html path))
                    (insert-file-contents path))
                  (libxml-parse-html-region (point-min) (point-max))))
           (new-dom (nov-xwidget--transform-dom dom "TOC")))
      (with-temp-file html-path
        (insert "<!DOCTYPE html>\n")
        (shr-dom-print new-dom)))))

(defun nov-xwidget--build-toc (docs)
  (let* ((ncxp (version< nov-epub-version "3.0"))
         (index (nov-find-document (lambda (doc) (eq (car doc) nov-toc-id)))))
    (unless (integerp index)
      (error "Couldn't locate TOC"))
    (let* ((path (cdr (aref docs index)))
           (html-path (expand-file-name "toc.html" (file-name-directory path))))
    (nov-xwidget--build-toc-html path html-path ncxp)
    html-path)))

(defun nov-xwidget-goto-toc ()
  "Go to the TOC index and render the TOC document."
  (interactive)
  (nov-xwidget-save-position)
  (setq-local nov-documents-index nov-toc-id)
  (let ((url (concat "file:///" nov-xwidget-toc-path)))
    (nov-xwidget--browse-url url)))

;;; Scroll helpers

(defun nov-xwidget--scroll-by (arg)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   (format "window.scrollBy({top: %d, behavior: 'instant'});" arg)))

(defun nov-xwidget--next-step-or-page-cb (end)
  (setq nov-xwidget-need-resume-position nil)
  (if (s-equals-p end "1")
      (nov-xwidget-next-document)
    (nov-xwidget--scroll-by precision-scroll-step-height)))

(defun nov-xwidget-scroll-up-page ()
  (interactive)
  (nov-xwidget--scroll-by (precision-scroll-page-height)))

(defun nov-xwidget-scroll-down-page ()
  (interactive)
  (nov-xwidget--scroll-by (* -1 (precision-scroll-page-height))))

(defun nov-xwidget-scroll-up-step ()
  (interactive)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   "(function () {
    if (document.documentElement.clientHeight + document.documentElement.scrollTop >= document.documentElement.scrollHeight) {
        return \"1\";
    } else {
        return \"0\";
    }
})();" #'nov-xwidget--next-step-or-page-cb))

(defun nov-xwidget--previous-step-or-page-cb (end)
  (setq nov-xwidget-need-resume-position nil)
  (if (s-equals-p end "1")
      (progn
        (nov-xwidget-previous-document)
        (run-at-time 0.1 nil #'xwidget-webkit-scroll-bottom))
    (nov-xwidget--scroll-by (* -1 precision-scroll-step-height))))

(defun nov-xwidget-scroll-down-step ()
  (interactive)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   "(function () {
    if (window.scrollY == 0) {
        return \"1\";
    } else {
        return \"0\";
    }
})();" #'nov-xwidget--previous-step-or-page-cb))

;;; Entry point mode
(defvar-local nov-temp-dir nil)

(defun nov-xwidget--extract-epub ()
  (unless (file-exists-p nov-temp-dir)
    (let ((exit-code (nov-unzip-epub nov-temp-dir buffer-file-name)))
      (unless (integerp exit-code)
        (nov-clean-up)
        (error "EPUB extraction aborted by signal %s" exit-code))
      (when (> exit-code 1)
        (nov-clean-up)
        (error "EPUB extraction failed with exit code %d (see *nov unzip* buffer)"
               exit-code)))
    (unless (nov-epub-valid-p nov-temp-dir)
      (nov-clean-up)
      (error "Invalid EPUB file"))
    t))

(define-derived-mode nov-xwidget-mode special-mode "EPUB"
  "Major mode for reading EPUB documents"
  (unless (file-exists-p nov-xwidget-cache-dir)
    (make-directory nov-xwidget-cache-dir t))
  (setq nov-temp-dir (expand-file-name (file-name-nondirectory buffer-file-name) nov-xwidget-cache-dir))
  (let* ((extracted (nov-xwidget--extract-epub))
         (container (nov-slurp (nov-container-filename nov-temp-dir) t))
         (content-file-name (nov-container-content-filename container))
         (content-file (nov-make-path nov-temp-dir content-file-name))
         (work-dir (file-name-directory content-file))
         (content (nov-slurp content-file t)))

    (setq nov-content-file content-file)
    (setq nov-epub-version (nov-content-version content))
    (setq nov-metadata (nov-content-metadata content))
    (setq nov-documents (apply 'vector (nov-content-files work-dir content)))
    (setq nov-xwidget-toc-path (nov-xwidget--build-toc nov-documents))
    (setq nov-file-name buffer-file-name)
    (when extracted
      (nov-xwidget--transform-all-files))

    (if-let* ((place (nov-saved-place (cdr (assq 'identifier nov-metadata))))
              (index (cdr (assq 'index place))))
        (setq nov-documents-index
              (nov-xwidget--sanitize-index nov-documents index))
      (setq nov-documents-index 0)))

  (setq-local bookmark-make-record-function 'nov-bookmark-make-record)
  (let ((init-buf (current-buffer)))
    (nov-xwidget-view)
    (kill-buffer init-buf)))

(provide 'nov-xwidget-mode)
;;; nov-xwidget-mode.el ends here
