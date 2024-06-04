(defvar nov-use-xwidget t)
(defvar nov-scroll-step 10)
(use-package nov
  :mode (("\\.epub$" . nov-mode))
  :config
  (require 'nov-xwidget)
  (setq nov-xwidget-style 'light)
  (add-hook 'nov-mode-hook 'my-nov-mode-hook))

(with-eval-after-load 'nov-xwidget
  (defun get-nov-xwidget-style()
    (pcase nov-xwidget-style
      ('light nov-xwidget-style-light)
      ('dark nov-xwidget-style-dark)
      ('auto (pcase (frame-parameter nil 'background-mode)
               ('light nov-xwidget-style-light)
               ('dark nov-xwidget-style-dark)
               (_ nov-xwidget-style-light)))))

  (defun nov-xwidget-inject (file &optional callback)
    "Inject `nov-xwidget-script', `nov-xwidget-style-light', or `nov-xwidget-style-dark' into FILE.
Call CALLBACK on the final injected dom.
Input FILE should be  htm/html/xhtml
Output a new html file prefix by _."
    (when nov-xwidget-debug
      ;; create the nov-xwidget-inject-output-dir if not exists
      (unless (file-exists-p nov-xwidget-inject-output-dir)
        (make-directory nov-xwidget-inject-output-dir)) )
    (let* ((native-path file)
           ;; only work on html/xhtml file, rename xhtml as html
           ;; we need to save to a new html file, because the original file may be read only
           ;; saving to new html file is easier to tweak
           (output-native-file-name (if (or (string-equal (file-name-extension native-path) "htm")
                                            (string-equal (file-name-extension native-path) "html")
                                            (string-equal (file-name-extension native-path) "xhtml"))
                                        (format "_%s.html" (file-name-base native-path))
                                      (file-name-nondirectory native-path)))
           ;; get full path of the final html file
           (output-native-path (expand-file-name output-native-file-name (if nov-xwidget-debug
                                                                             nov-xwidget-inject-output-dir
                                                                           (setq nov-xwidget-inject-output-dir (file-name-directory native-path)))))
           ;; create the html if not esists, insert the `nov-xwidget-script' as the html script
           (dom (with-temp-buffer
                  (insert-file-contents native-path)
                  (libxml-parse-html-region (point-min) (point-max))))
           (new-dom (let ((dom dom))
                      ;; fix all href and point to the new html file
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
                               (dom-elements dom 'href ".*htm.*")))
                      (dom-append-child
                       (dom-by-tag dom 'head)
                       '(meta ((charset . "utf-8"))))
                      (dom-append-child
                       (dom-by-tag dom 'head)
                       `(style nil ,(get-nov-xwidget-style)))
                      (dom-append-child
                       (dom-by-tag dom 'head)
                       `(script nil ,nov-xwidget-script))
                      dom)))
      (if callback
          (funcall callback new-dom))
      (with-temp-file output-native-path
        (shr-dom-print new-dom)
        ;; (encode-coding-region (point-min) (point-max) 'utf-8)
        output-native-path))))

(defun modeline-nov-document-index()
  (format " %d/%d" nov-documents-index (length nov-documents)))

(defun nov-goto-next-line-or-page(&optional rest)
  (interactive)
  (if (>= (window-end) (point-max))
      (nov-next-document)
    (pixel-scroll-precision-scroll-down (line-pixel-height)))
  (if (get-text-property (point) 'display)
      (forward-line 1)))

(defun nov-goto-previous-line-or-page(&optional rest)
  (interactive)
  (if (and (<= (window-start) (point-min))
           (> nov-documents-index 0))
      (progn
        (nov-previous-document)
        (goto-char (point-max)))
    (pixel-scroll-precision-scroll-up (line-pixel-height)))
  (if (get-text-property (point) 'display)
      (forward-line -1)))

(defun setup-nov()
  (define-key nov-mode-map (kbd "N") 'nov-next-document)
  (define-key nov-mode-map (kbd "P") 'nov-previous-document)
  (define-key nov-mode-map (kbd "n") 'nov-goto-next-line-or-page)
  (define-key nov-mode-map (kbd "p") 'nov-goto-previous-line-or-page)
  (define-key nov-mode-map (kbd "o") 'nov-goto-toc)
  (define-key nov-mode-map (kbd "f") 'forward-char)
  (define-key nov-mode-map (kbd "b") 'backward-char)
  (define-key nov-mode-map (kbd "e") 'end-of-line)
  (define-key nov-mode-map (kbd "a") 'beginning-of-line)
  (define-key nov-mode-map (kbd "l") 'forward-char)
  (define-key nov-mode-map (kbd "M-[") 'nov-history-back)
  (define-key nov-mode-map (kbd "M-]") 'nov-history-forward)
  (define-key nov-mode-map (kbd "h") 'backward-char)
  (define-key nov-mode-map (kbd "0") 'beginning-of-line)
  (define-key nov-mode-map (kbd "$") 'end-of-line)
  (define-key nov-mode-map (kbd "j") 'nov-goto-next-line-or-page)
  (define-key nov-mode-map (kbd "k") 'nov-goto-previous-line-or-page)
  (define-key nov-mode-map (kbd "<down>") 'nov-goto-next-line-or-page)
  (define-key nov-mode-map (kbd "<up>") 'nov-goto-previous-line-or-page)
  (define-key nov-mode-map (kbd "=") 'er/expand-region)
  (define-key nov-mode-map (kbd ",") 'bing-dict-at-point)
  (define-key nov-mode-map (kbd "<double-mouse-1>") #'bing-dict-at-point))

(defun nov-xwidget-next-line-or-page-cb(end)
  (if (s-equals-p end "1")
      (nov-xwidget-next-document)
    (xwidget-webkit-scroll-up-line nov-scroll-step)))

(defun nov-xwidget-next-line-or-page()
  (interactive)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   "(function () {
    if (window.innerHeight + window.scrollY >= document.body.scrollHeight) {
        return \"1\";
    } else {
        return \"0\";
    }
})();" #'nov-xwidget-next-line-or-page-cb))

(defun nov-xwidget-previous-line-or-page-cb(end)
  (if (s-equals-p end "1")
      (progn
        (nov-xwidget-previous-document)
        (run-at-time 0.1 nil #'xwidget-webkit-scroll-bottom))
    (xwidget-webkit-scroll-down-line nov-scroll-step)))

(defun nov-xwidget-previous-line-or-page()
  (interactive)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   "(function () {
    if (window.scrollY == 0) {
        return \"1\";
    } else {
        return \"0\";
    }
})();" #'nov-xwidget-previous-line-or-page-cb))

(defun setup-nov-xwidget()
  (nov-xwidget-inject-all-files)
  (nov-xwidget-view)
  (let ((title (cdr (assq 'title nov-metadata))))
    (setq-local xwidget-webkit-buffer-name-format title)
    (rename-buffer title))
  (read-only-mode)
  (define-key xwidget-webkit-mode-map (kbd "N") 'nov-xwidget-next-document)
  (define-key xwidget-webkit-mode-map (kbd "P") 'nov-xwidget-previous-document)
  (define-key xwidget-webkit-mode-map (kbd "n") #'nov-xwidget-next-line-or-page)
  (define-key xwidget-webkit-mode-map (kbd "j") #'nov-xwidget-next-line-or-page)
  (define-key xwidget-webkit-mode-map (kbd "G") #'xwidget-webkit-scroll-bottom)
  (define-key xwidget-webkit-mode-map (kbd "p") #'nov-xwidget-previous-line-or-page)
  (define-key xwidget-webkit-mode-map (kbd "k") #'nov-xwidget-previous-line-or-page)
  (define-key xwidget-webkit-mode-map (kbd "o") 'nov-xwidget-goto-toc))

(defun my-nov-mode-hook()
  (setq-local mwheel-scroll-up-function
              #'nov-goto-next-line-or-page)
  (if (boundp 'mwheel-scroll-down-function)
      (setq-local mwheel-scroll-down-function
                  #'nov-goto-previous-line-or-page))
  (visual-line-mode)
  (if nov-use-xwidget
      (setup-nov-xwidget)
    (setup-nov)))

(provide 'init-epub)
