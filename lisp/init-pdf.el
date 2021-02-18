;; pdf-tools
(use-package pdf-tools
  :demand t
  :config
  ;;(pdf-tools-install t t t nil)
  (require 'pdf-continuous-scroll-mode)
  (setq pdf-view-use-unicode-ligther nil)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil)
  (setq pdf-tools-enabled-modes (remove 'pdf-sync-minor-mode pdf-tools-enabled-modes))
  (setq pdf-links-browse-uri-function #'xwidget-webkit-browse-url)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; HACK `pdf-tools-install-noverify' tries to "reset" open pdf-view-mode
  ;;      buffers, but does so incorrectly, causing errors when pdf-tools is
  ;;      loaded after opening a pdf file. We've done its job ourselves in
  ;;      `+pdf--install-epdfinfo-a' instead.
  (defadvice! +pdf--inhibit-pdf-view-mode-resets-a (orig-fn &rest args)
    :around #'pdf-tools-install-noverify
    (letf! ((#'pdf-tools-pdf-buffer-p #'ignore))
      (apply orig-fn args)))

  (defadvice! +pdf--install-epdfinfo-a (orig-fn &rest args)
    "Install epdfinfo after the first PDF file, if needed."
    :around #'pdf-view-mode
    ;; Prevent "epdfinfo not an executable" error short-circuiting this advice
    (prog1 (with-demoted-errors "%s" (apply orig-fn args))
      ;; ...so we can go ahead and install it afterwards.
      (cond ((file-executable-p pdf-info-epdfinfo-program))
            ((y-or-n-p "To read PDFs in Emacs the epdfinfo program must be built. Build it now?")
             (message nil) ; flush lingering prompt in echo-area
             ;; Make sure this doesn't run more than once
             (advice-remove #'pdf-view-mode #'+pdf--install-epdfinfo-a)
             (unless (or (pdf-info-running-p)
                         (ignore-errors (pdf-info-check-epdfinfo) t))
               ;; HACK On the first pdf you open (before pdf-tools loaded)
               ;;      `pdf-tools-install' throws errors because it has hardcoded
               ;;      opinions about what buffer should be focused when it is run.
               ;;      These errors cause `compile' to position the compilation
               ;;      window incorrectly or can interfere with the opening of the
               ;;      original pdf--sometimes aborting/burying it altogether. A
               ;;      timer works around this.
               (run-at-time
                0.1 nil
                (lambda ()
                  (with-current-buffer (pdf-tools-install t)
                    (add-hook! 'compilation-finish-functions :local
                               (dolist (buf (buffer-list))
                                 (with-current-buffer buf
                                   (and (buffer-file-name)
                                        (or (pdf-tools-pdf-buffer-p)
                                            (derived-mode-p 'pdf-view-mode))
                                        (revert-buffer t t))))))))))
            ((message "Aborted"))))))
(pdf-tools-install-noverify)

(defun pdf-view-next-page-start ()
  "View the next page in the PDF."
  (interactive)
  (pdf-view-goto-page (+ (pdf-view-current-page) 1))
  (pdf-view-goto-page-start))

(defun pdf-view-prev-page-start ()
  "View the pre page in the PDF."
  (interactive)
  (pdf-view-goto-page (- (pdf-view-current-page) 1))
  (pdf-view-goto-page-start))

(defun pdf-view-goto-page-start ()
  "Goto page start"
  (interactive)
  (image-set-window-vscroll 0)
  (pdf-view-redisplay t))

(defun pdf-traslate-under-mouse (ev)
  "Select word at mouse event EV and translate it"
  (interactive "@e")
  (let* ((posn (event-start ev))
         (xy (posn-object-x-y posn))
         (size (pdf-view-image-size))
         (page (pdf-view-current-page))
         (x (/ (car xy) (float (car size))))
         (y (/ (cdr xy) (float (cdr size))))
         (text (pdf-info-gettext page (list x y x y) 'word)))
    ;;(setq pdf-view-active-region (pdf-info-getselection page (list x y x y) 'word))
    ;;(pdf-view-display-region pdf-view-active-region)
    (if (> (length text) 0)
        (bing-dict-brief text))
    ;;(pdf-view-deactivate-region)
    ))

(defun pdf-view-mouse-set-region-wapper (event)
  (interactive "@e")
  (pdf-view-mouse-set-region event)
  (pdf-traslate-under-mouse event))

(defun pdf-translate-selection (&rest _)
  "Translate the selected word"
  (interactive)
  (let ((text (car (pdf-view-active-region-text))))
    (if (> (length text) 0)
        (bing-dict-brief text))))

(advice-add #'pdf-view--push-mark :after #'pdf-translate-selection)

;;Add retina support for pdf
(defun +pdf--supply-width-to-create-image-calls-a (orig-fn &rest args)
  (cl-letf* ((old-create-image (symbol-function #'create-image))
             ((symbol-function #'create-image)
              (lambda (file-or-data &optional type data-p &rest props)
                (apply old-create-image file-or-data type data-p
                       :width (car (pdf-view-image-size))
                       props))))
    (apply orig-fn args)))

(defun +pdf--util-frame-scale-factor-a (orig-fn)
  (if (and pdf-view-use-scaling
           (memq (pdf-view-image-type) '(imagemagick image-io))
           (fboundp 'frame-monitor-attributes))
      (funcall orig-fn)
    ;; Add special support for retina displays on MacOS
    (if (eq (framep-on-display) 'ns)
        2
      1)))

(defun +pdf--view-use-scaling-p-a ()
  "Returns t if on ns window-system on Emacs 27+."
  (and (eq (framep-on-display) 'ns)
       pdf-view-use-scaling))

(advice-add #'pdf-util-frame-scale-factor :around #'+pdf--util-frame-scale-factor-a)
(advice-add #'pdf-view-use-scaling-p :before-until #'+pdf--view-use-scaling-p-a)
(defadvice! +pdf--supply-width-to-create-image-calls-a (orig-fn &rest args)
  :around '(pdf-annot-show-annotation
            pdf-isearch-hl-matches
            pdf-view-display-region)
  (letf! (defun create-image (file-or-data &optional type data-p &rest props)
           (apply create-image file-or-data type data-p
                  :width (car (pdf-view-image-size))
                  props))
    (apply orig-fn args)))

(use-package pdf-view-restore
  :after pdf-tools
  :ensure t
  :demand t
  :commands (pdf-view-restore)
  :config
  (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore")
  :hook ((pdf-view-mode . pdf-view-restore-mode)))

(after-load 'pdf-annot
  (defun +pdf-cleanup-windows-h ()
    "Kill left-over annotation buffers when the document is killed."
    (when (buffer-live-p pdf-annot-list-document-buffer)
      (pdf-info-close pdf-annot-list-document-buffer))
    (when (buffer-live-p pdf-annot-list-buffer)
      (kill-buffer pdf-annot-list-buffer))
    (let ((contents-buffer (get-buffer "*Contents*")))
      (when (and contents-buffer (buffer-live-p contents-buffer))
        (kill-buffer contents-buffer))))
  (add-hook 'kill-buffer-hook #'+pdf-cleanup-windows-h nil t))

(add-hook 'pdf-view-mode-hook (lambda ()
                                (company-mode -1)
                                (blink-cursor-mode -1)
                                (eldoc-mode -1)
                                (whitespace-cleanup-mode -1)
                                (electric-pair-local-mode -1)
                                (electric-indent-local-mode -1)
                                (font-lock-mode -1)
                                (yas-minor-mode -1)
                                (setq-local left-fringe-width 1)
                                (pdf-view-midnight-minor-mode)
                                (pdf-continuous-scroll-mode)
                                (pdf-cscroll-toggle-mode-line)
                                (local-set-key (kbd "q") #'kill-current-buffer)
                                (local-set-key (kbd "0") #'pdf-view-goto-page-start)
                                (local-set-key (kbd "C-v") #'pdf-view-next-page-start)
                                (local-set-key (kbd "M-v") #'pdf-view-prev-page-start)
                                (define-key pdf-continuous-scroll-mode-map (kbd "n") #'pdf-continuous-scroll-forward)
                                (define-key pdf-continuous-scroll-mode-map (kbd "p") #'pdf-continuous-scroll-backward)
                                (local-set-key (kbd "<down-mouse-1>") #'pdf-view-mouse-set-region-wapper)
                                (local-set-key (kbd "<double-mouse-1>") #'pdf-traslate-under-mouse)))

(provide 'init-pdf)
