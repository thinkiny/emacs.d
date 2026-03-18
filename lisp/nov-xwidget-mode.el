;;; nov-xwidget-mode.el --- EPUB reader using xwidget-webkit -*- lexical-binding: t; -*-

(require 'nov)
(require 'shr)
(require 'xwidget)
(require 'cl-lib)
(require 's)
(require 'caret-xwidget)

;;; Configuration

(defconst nov-xwidget-style 'light
  "Theme style for EPUB rendering: 'light, 'dark, or 'auto.")

(defconst nov-xwidget-need-inject t
  "Whether to inject custom CSS and modify DOM.")

(defconst nov-xwidget-need-remove-override-css t
  "Whether to remove Calibre's override_v1.css.")

(defconst nov-xwidget-cache-dir (expand-file-name "cache/epub" user-emacs-directory)
  "Directory for caching extracted EPUB files.")

(defvar-local nov-xwidget-toc-path nil
  "Path to the generated TOC HTML file.")

;;; Style Management

(defun nov-xwidget-get-style-href ()
  "Get the CSS file URL based on `nov-xwidget-style' setting."
  (let ((theme (pcase nov-xwidget-style
                 ('auto (or (frame-parameter nil 'background-mode) 'light))
                 (_ nov-xwidget-style))))
    (format "file://%s/assets/css/nov-%s.css"
            (expand-file-name user-emacs-directory)
            (if (eq theme 'dark) "dark" "light"))))

;;; Position Persistence

(defvar-local nov-xwidget-need-resume-position t
  "Whether to resume scroll position on next page load.")

(defun nov-xwidget--get-position-key ()
  "Generate localStorage key for current document's scroll position."
  (when-let* ((uri (xwidget-webkit-uri (xwidget-webkit-current-session)))
              (url (url-generic-parse-url uri))
              (filename (url-unhex-string (url-filename url))))
    (concat "epub-pos-"
            (string-trim-left filename (concat "/" nov-xwidget-cache-dir "/")))))

(defun nov-xwidget-save-position ()
  "Save current scroll position to browser localStorage."
  (interactive)
  (when-let* ((key (nov-xwidget--get-position-key)))
    (xwidget-execute-script
     (format "window.localStorage.setItem('%s', window.scrollY);" key))))

(defun nov-xwidget-reset-position ()
  "Clear saved scroll position for current document."
  (interactive)
  (when-let* ((key (nov-xwidget--get-position-key)))
    (xwidget-execute-script
     (format "window.localStorage.removeItem('%s');" key))))

(defun nov-xwidget-jump-prev-position ()
  "Restore previously saved scroll position."
  (interactive)
  (when (and nov-xwidget-need-resume-position
             (nov-xwidget--get-position-key))
    (let ((key (nov-xwidget--get-position-key)))
      (xwidget-execute-script
       (format "if(window.localStorage.getItem('%s') != null) { window.scroll(0, localStorage.getItem('%s')); }" key key))))
  (setq nov-xwidget-need-resume-position t))

;;; Keymap and Mode Definition

(defvar nov-xwidget-webkit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'nov-xwidget-goto-toc)
    (define-key map (kbd "N") #'nov-xwidget-next-document)
    (define-key map (kbd "P") #'nov-xwidget-previous-document)
    (define-key map (kbd "G") #'xwidget-webkit-scroll-bottom)
    map)
  "Keymap for `nov-xwidget-webkit-mode'.")

(define-derived-mode nov-xwidget-webkit-mode xwidget-webkit-mode "EPUB"
  "Major mode for reading EPUB files in xwidget-webkit.
\\{nov-xwidget-webkit-mode-map}"
  :keymap nov-xwidget-webkit-mode-map
  (add-hook 'kill-buffer-hook #'nov-xwidget-save nil t)
  (add-hook 'kill-emacs-hook #'nov-xwidget-save nil t))

(defun nov-xwidget-save ()
  "Save reading position and metadata for current EPUB."
  (when nov-temp-dir
    (let ((identifier (cdr (assq 'identifier nov-metadata)))
          (index (if (integerp nov-documents-index) nov-documents-index 0)))
      (nov-save-place identifier index (point)))
    (nov-xwidget-save-position)))

;;; File Path Handling

(defun nov-xwidget--unify-path (path)
  "Convert EPUB file paths to webkit-compatible format.
Converts .xhtml to .html and .ncx to TOC path."
  (pcase (file-name-extension path)
    ("xhtml" (format "%s%s.html"
                     (or (file-name-directory path) "")
                     (file-name-base path)))
    ("ncx" nov-xwidget-toc-path)
    (_ path)))

(defun nov-xwidget--fix-href (href)
  "Normalize href attribute for webkit rendering."
  (format "%s%s.%s"
          (or (file-name-directory href) "")
          (file-name-base href)
          (replace-regexp-in-string "x?html" "html" (file-name-extension href))))

;;; DOM Manipulation

(defun nov-xwidget--is-override-css-p (node)
  "Check if NODE is Calibre's override CSS."
  (string-equal (dom-attr node 'href) "override_v1.css"))

(defun nov-xwidget--remove-override-css (dom)
  "Remove Calibre's override_v1.css from DOM if present."
  (when nov-xwidget-need-remove-override-css
    (when-let* ((head (car (dom-by-tag dom 'head)))
                (node (car (dom-search head 'nov-xwidget--is-override-css-p))))
      (dom-remove-node head node))))

(defun nov-xwidget--remove-calibre-classes (dom)
  "Remove Calibre-generated class names from DOM elements."
  (dolist (elem (dom-elements dom 'class ".*calibre.*"))
    (let* ((class (dom-attr elem 'class))
           (cleaned (replace-regexp-in-string "calibre[^ ]*" "" class)))
      (dom-set-attribute elem 'class cleaned))))

(defun nov-xwidget--fix-href-attributes (dom)
  "Fix all href attributes in DOM to use .html extension."
  (dolist (elem (cl-remove-if
                 (lambda (x) (string-match-p "https?.*" (dom-attr x 'href)))
                 (dom-elements dom 'href ".*htm.*")))
    (let* ((href (dom-attr elem 'href))
           (fixed (nov-xwidget--fix-href href)))
      (dom-set-attribute elem 'href fixed))))

(defun nov-xwidget--inject-head (dom &optional title)
  "Inject CSS stylesheet and optional TITLE into DOM's head element."
  (let ((head (car (dom-by-tag dom 'head)))
        (elements `((meta ((charset . "utf-8")))
                    (link ((rel . "stylesheet")
                           (type . "text/css")
                           (href . ,(nov-xwidget-get-style-href)))))))
    (when title
      (push `(title nil ,title) elements))
    (if head
        (dolist (elem (nreverse elements))
          (dom-append-child head elem))
      (dom-add-child-before dom `(head nil ,@(nreverse elements))))))

(defun nov-xwidget--transform-dom (dom &optional title)
  "Apply all DOM transformations: fix hrefs, inject styles, remove Calibre artifacts."
  (when dom
    (nov-xwidget--fix-href-attributes dom)
    (nov-xwidget--inject-head dom title)
    (nov-xwidget--remove-calibre-classes dom)
    (nov-xwidget--remove-override-css dom))
  dom)

(defun nov-xwidget--dom-print (dom)
  "Print DOM as HTML string."
  (insert (format "<%s" (dom-tag dom)))
  (dolist (attr (dom-attributes dom))
    (insert (if (and (memq (car attr)
                           '(async autofocus autoplay checked contenteditable
                             controls default defer disabled formNoValidate
                             frameborder hidden ismap itemscope loop multiple
                             muted nomodule novalidate open readonly required
                             reversed cscoped selected typemustmatch))
                     (cdr attr))
                (format " %s" (car attr))
              (format " %s=\"%s\"" (car attr)
                      (url-insert-entities-in-string (cdr attr))))))
  (let ((children (dom-children dom)))
    (if (null children)
        (insert (format "></%s>" (dom-tag dom)))
      (insert ">")
      (dolist (child children)
        (if (stringp child)
            (insert (url-insert-entities-in-string child))
          (nov-xwidget--dom-print child)))
      (insert (format "</%s>" (dom-tag dom))))))

;;; File Processing

(defun nov-xwidget--process-file (source-path &optional callback)
  "Process EPUB file at SOURCE-PATH: parse, transform DOM, and write output.
Optional CALLBACK is called with the transformed DOM before writing."
  (let* ((output-path source-path)
         (dom (with-temp-buffer
                (insert-file-contents source-path)
                (libxml-parse-html-region (point-min) (point-max))))
         (transformed (nov-xwidget--transform-dom dom)))
    (when callback
      (funcall callback transformed))
    (with-temp-file output-path
      (insert "<!DOCTYPE html>\n")
      (nov-xwidget--dom-print transformed))
    output-path))

(defun nov-xwidget-process-all-documents ()
  "Process all EPUB documents: rename .xhtml to .html and inject styles.
Should be run once after EPUB extraction."
  (interactive)
  (when nov-documents
    (seq-do-indexed
     (lambda (document i)
       (let* ((original-path (cdr document))
              (unified-path (nov-xwidget--unify-path original-path)))
         (unless (string= original-path unified-path)
           (unless (file-exists-p unified-path)
             (rename-file original-path unified-path)))
         (when nov-xwidget-need-inject
           (nov-xwidget--process-file unified-path))
         (aset nov-documents i (cons (car document) unified-path))))
     nov-documents)))

;;; Webkit Integration

(defun nov-xwidget--browse-url (url &optional new-session switch-fn)
  "Browse URL in xwidget-webkit.
If NEW-SESSION, create new webkit session.
SWITCH-FN determines how to switch to the buffer."
  (if new-session
      (progn
        (switch-to-buffer
         (xwidget-webkit--create-new-session-buffer url #'nov-xwidget--webkit-callback))
        (xwidget-webkit-goto-uri (xwidget-webkit-last-session) url))
    (xwidget-webkit-goto-url url)
    (if switch-fn
        (funcall switch-fn (xwidget-buffer (xwidget-webkit-current-session)))
      (pop-to-buffer (xwidget-buffer (xwidget-webkit-current-session))))))

(defun nov-xwidget--open-file (path &optional new-session switch-fn)
  "Open EPUB document at PATH in xwidget-webkit."
  (let ((url (concat "file:///" (nov-xwidget--unify-path path))))
    (nov-xwidget--browse-url url new-session (or switch-fn 'switch-to-buffer))))

;;; Navigation

(defun nov-xwidget--view-document (index)
  "Display EPUB document at INDEX."
  (let ((path (cdr (aref nov-documents index))))
    (nov-xwidget--open-file path)
    (setq-local nov-documents-index index)))

(defun nov-xwidget--find-document-index (path)
  "Find document index for PATH in `nov-documents'."
  (when path
    (let ((decoded (decode-coding-string (url-unhex-string path) 'utf-8)))
      (seq-position nov-documents decoded
                    (lambda (doc file)
                      (string-collate-equalp file (nov-xwidget--unify-path (cdr doc))))))))

(defun nov-xwidget--extract-path-from-uri (uri)
  "Extract file path from webkit URI."
  (when (and uri (string-match "file:///\\([^#]*\\)" uri))
    (match-string 1 uri)))

(defun nov-xwidget--should-skip-position-restore-p (uri)
  "Check if position restoration should be skipped for URI."
  (or (eq nov-documents-index nov-toc-id)
      (string-match-p "#" uri)))

(defun nov-xwidget--webkit-callback (xwidget event-type)
  "Webkit callback handler for EPUB navigation.
Updates document index and restores scroll position on page load."
  (when (and (eq event-type 'load-changed)
             (string-equal (nth 3 last-input-event) "load-finished"))
    (when-let* ((uri (xwidget-webkit-uri xwidget)))
      (unless (nov-xwidget--should-skip-position-restore-p uri)
        (nov-xwidget-jump-prev-position))
      (when-let* ((path (nov-xwidget--extract-path-from-uri uri))
                  (index (nov-xwidget--find-document-index path)))
        (setq-local nov-documents-index index))))
  (xwidget-webkit-callback xwidget event-type))

(defun nov-xwidget-next-document ()
  "Navigate to the next EPUB document."
  (interactive)
  (unless (integerp nov-documents-index)
    (setq nov-documents-index 0))
  (when (< nov-documents-index (1- (length nov-documents)))
    (nov-xwidget--view-document (1+ nov-documents-index))))

(defun nov-xwidget-previous-document ()
  "Navigate to the previous EPUB document."
  (interactive)
  (unless (integerp nov-documents-index)
    (setq nov-documents-index 0))
  (when (> nov-documents-index 0)
    (nov-xwidget--view-document (1- nov-documents-index))))

;;; Webkit Buffer Setup

(defun nov-xwidget--setup-webkit-buffer (epub-path)
  "Configure webkit buffer with EPUB-specific settings and variables."
  (nov-xwidget-webkit-mode)
  (setq-local nov-xwidget-toc-path nov-xwidget-toc-path
              nov-documents nov-documents
              nov-documents-index nov-documents-index
              nov-toc-id nov-toc-id
              nov-epub-version nov-epub-version
              nov-temp-dir nov-temp-dir
              nov-metadata nov-metadata
              buffer-file-name epub-path
              default-directory (file-name-directory epub-path)
              cursor-type nil
              caret-xwidget-next-page-function #'nov-xwidget-next-document
              caret-xwidget-previous-page-function
              (lambda ()
                (nov-xwidget-previous-document)
                (run-at-time 0.1 nil #'xwidget-webkit-scroll-bottom))
              xwidget-webkit-buffer-name-format
              (format "*Epub: %s" (file-name-nondirectory epub-path)))
  (set-buffer-modified-p nil))

(defun nov-xwidget-view ()
  "Open current EPUB document in xwidget-webkit browser."
  (interactive)
  (let ((epub-path nov-file-name)
        (document-path (cdr (aref nov-documents nov-documents-index))))
    (nov-xwidget--open-file document-path)
    (with-current-buffer (xwidget-buffer (xwidget-webkit-current-session))
      (nov-xwidget--setup-webkit-buffer epub-path))))

;;; Table of Contents

(defun nov-xwidget--walk-ncx-node (node)
  "Recursively convert NCX navigation NODE to HTML list structure."
  (let ((tag (dom-tag node))
        (children (seq-filter (lambda (child)
                                (memq (dom-tag child) '(navPoint navpoint)))
                              (dom-children node))))
    (cond
     ((memq tag '(navMap navmap))
      (insert "<ol>\n")
      (mapc #'nov-xwidget--walk-ncx-node children)
      (insert "</ol>\n"))
     ((memq tag '(navPoint navpoint))
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

(defun nov-xwidget--ncx-to-html (ncx-path)
  "Convert NCX document at NCX-PATH to HTML."
  (let ((root (esxml-query "navMap,navmap" (nov-slurp ncx-path t))))
    (with-temp-buffer
      (nov-xwidget--walk-ncx-node root)
      (buffer-string))))

(defun nov-xwidget--generate-toc-html (toc-path output-path is-ncx)
  "Generate TOC HTML file from TOC-PATH and save to OUTPUT-PATH.
IS-NCX indicates whether the source is NCX format (EPUB2) or nav format (EPUB3)."
  (unless (file-exists-p output-path)
    (let* ((dom (with-temp-buffer
                  (if is-ncx
                      (insert (nov-xwidget--ncx-to-html toc-path))
                    (insert-file-contents toc-path))
                  (libxml-parse-html-region (point-min) (point-max))))
           (transformed (nov-xwidget--transform-dom dom "TOC")))
      (with-temp-file output-path
        (insert "<!DOCTYPE html>\n")
        (shr-dom-print transformed)))))

(defun nov-xwidget--prepare-toc ()
  "Generate and return path to TOC HTML file."
  (let* ((is-ncx (version< nov-epub-version "3.0"))
         (toc-index (nov-find-document (lambda (doc) (eq (car doc) nov-toc-id))))
         (toc-path (cdr (aref nov-documents toc-index)))
         (html-path (expand-file-name "toc.html" (file-name-directory toc-path))))
    (unless toc-index
      (error "Couldn't locate TOC"))
    (nov-xwidget--generate-toc-html toc-path html-path is-ncx)
    html-path))

(defun nov-xwidget-goto-toc ()
  "Navigate to the table of contents."
  (interactive)
  (nov-xwidget-save-position)
  (setq-local nov-documents-index nov-toc-id)
  (nov-xwidget--open-file nov-xwidget-toc-path))

;;; Scrolling (for compatibility)

(defun nov-xwidget--scroll (pixels)
  "Scroll webkit view by PIXELS (positive = down, negative = up)."
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   (format "window.scrollBy({top: %d, behavior: 'instant'});" pixels)))

(defun nov-xwidget-scroll-up-page ()
  "Scroll down one page."
  (interactive)
  (nov-xwidget--scroll (get-precision-scroll-page-height)))

(defun nov-xwidget-scroll-down-page ()
  "Scroll up one page."
  (interactive)
  (nov-xwidget--scroll (* -1 (get-precision-scroll-page-height))))

(defun nov-xwidget-scroll-up-step ()
  "Scroll down one step, or go to next document if at bottom."
  (interactive)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   "(function () {
    if (document.documentElement.clientHeight + document.documentElement.scrollTop >= document.documentElement.scrollHeight) {
        return \"1\";
    } else {
        return \"0\";
    }
})();"
   (lambda (at-end)
     (setq nov-xwidget-need-resume-position nil)
     (if (s-equals-p at-end "1")
         (nov-xwidget-next-document)
       (nov-xwidget--scroll precision-scroll-step-height)))))

(defun nov-xwidget-scroll-down-step ()
  "Scroll up one step, or go to previous document if at top."
  (interactive)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   "(function () {
    if (window.scrollY == 0) {
        return \"1\";
    } else {
        return \"0\";
    }
})();"
   (lambda (at-top)
     (setq nov-xwidget-need-resume-position nil)
     (if (s-equals-p at-top "1")
         (progn
           (nov-xwidget-previous-document)
           (run-at-time 0.1 nil #'xwidget-webkit-scroll-bottom))
       (nov-xwidget--scroll (* -1 precision-scroll-step-height))))))

;;; Entry Point

(defun nov-xwidget--extract-epub ()
  "Extract EPUB file to temporary directory. Return t if extraction succeeded."
  (unless (file-exists-p nov-temp-dir)
    (let ((exit-code (nov-unzip-epub nov-temp-dir buffer-file-name)))
      (unless (integerp exit-code)
        (nov-clean-up)
        (error "EPUB extraction aborted by signal %s" exit-code))
      (when (> exit-code 1)
        (nov-clean-up)
        (error "EPUB extraction failed with exit code %d (see *nov unzip* buffer)" exit-code)))
    (unless (nov-epub-valid-p nov-temp-dir)
      (nov-clean-up)
      (error "Invalid EPUB file"))
    t))

(define-derived-mode nov-xwidget-mode special-mode "EPUB"
  "Major mode for reading EPUB documents in xwidget-webkit."
  (setq nov-temp-dir (expand-file-name (file-name-nondirectory buffer-file-name)
                                       nov-xwidget-cache-dir))
  (unless (file-exists-p nov-xwidget-cache-dir)
    (make-directory nov-xwidget-cache-dir t))

  ;; Extract and parse EPUB
  (let* ((need-processing (nov-xwidget--extract-epub))
         (container (nov-slurp (nov-container-filename nov-temp-dir) t))
         (content-filename (nov-container-content-filename container))
         (content-path (nov-make-path nov-temp-dir content-filename))
         (content (nov-slurp content-path t)))

    ;; Set up nov.el variables
    (setq-local bookmark-make-record-function 'nov-bookmark-make-record
                nov-content-file content-path
                nov-epub-version (nov-content-version content)
                nov-metadata (nov-content-metadata content)
                nov-documents (apply 'vector (nov-content-files
                                              (file-name-directory content-path)
                                              content))
                nov-file-name buffer-file-name)

    ;; Process documents and generate TOC
    (when need-processing
      (nov-xwidget-process-all-documents))
    (setq nov-xwidget-toc-path (nov-xwidget--prepare-toc))

    ;; Restore saved position or start at beginning
    (setq nov-documents-index
          (if-let* ((place (nov-saved-place (cdr (assq 'identifier nov-metadata))))
                    (index (cdr (assq 'index place))))
              index
            0)))

  ;; Open webkit view and kill this buffer
  (let ((init-buffer (current-buffer)))
    (nov-xwidget-view)
    (kill-buffer init-buffer)))

(provide 'nov-xwidget-mode)
;;; nov-xwidget-mode.el ends here
