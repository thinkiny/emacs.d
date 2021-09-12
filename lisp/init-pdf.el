;; pdf-tools
(use-package pdf-tools
  :catch (lambda (keyword err)
           (message (format "Error: %s" (error-message-string err))))
  :init
  (require 'pdf-macs)
  (require 'pdf-cache)
  :demand t
  :config
  ;;(pdf-tools-install t t t nil)
  (defadvice! +pdf--install-epdfinfo-a (orig-fn &rest args)
  "Install epdfinfo after the first PDF file, if needed."
  :around #'pdf-info-check-epdfinfo
  (if (file-executable-p pdf-info-epdfinfo-program)
      (apply orig-fn args)
    ;; If we remain in pdf-view-mode, it'll spit out cryptic errors. This
    ;; graceful failure is better UX.
    (fundamental-mode)
    (message "Viewing PDFs in Emacs requires epdfinfo. Use `M-x pdf-tools-install' to build it"))


  (add-to-list 'display-buffer-alist
        '("\\*Outline .*pdf\\*"
          (display-buffer-in-side-window)
          (side . right)
          (window-width . 0.25)))

  (add-hook 'pdf-outline-minor-mode-hook
            (lambda ()
              (dolist (face '(outline-1 outline-2 outline-3))
                (make-local-variable face)
                (set-face-attribute face nil :height 1.0))))

  (setq pdf-view-use-unicode-ligther nil)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil)
  (setq pdf-tools-enabled-modes (remove 'pdf-sync-minor-mode pdf-tools-enabled-modes))
  (setq pdf-links-browse-uri-function #'xwidget-webkit-browse-url)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)))

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
(defadvice! +pdf--scale-up-on-retina-display-a (orig-fn &rest args)
  "Scale up the PDF on retina displays."
  :around #'pdf-util-frame-scale-factor
  (cond ((not pdf-view-use-scaling) 1)
        ((and (memq (pdf-view-image-type) '(imagemagick image-io))
              (fboundp 'frame-monitor-attributes))
         (funcall orig-fn))
        ;; Add special support for retina displays on MacOS
        ((eq (framep-on-display) 'ns) 2)
        (1)))

(defadvice! +pdf--use-scaling-on-ns-a ()
  :before-until #'pdf-view-use-scaling-p
  (eq (framep-on-display) 'ns))

(defadvice! +pdf--supply-width-to-create-image-calls-a (orig-fn &rest args)
  :around '(pdf-annot-show-annotation
            pdf-isearch-hl-matches
            pdf-view-display-region)
  (letf! (defun create-image (file-or-data &optional type data-p &rest props)
           (apply create-image file-or-data type data-p
                  :width (car (pdf-view-image-size))
                  props))
    (apply orig-fn args)))

(with-eval-after-load 'pdf-annot
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
                                (cua-mode -1)
                                (setq-local left-fringe-width 1)
                                (pdf-view-midnight-minor-mode)
                                (require 'pdf-continuous-scroll-mode)
                                (pdf-continuous-scroll-mode)
                                ;;(pdf-cscroll-toggle-mode-line)
                                (local-set-key (kbd "q") #'kill-current-buffer)
                                (local-set-key (kbd "0") #'pdf-view-goto-page-start)
                                (local-set-key (kbd "C-v") #'pdf-view-next-page-start)
                                (local-set-key (kbd "M-v") #'pdf-view-prev-page-start)
                                (define-key pdf-continuous-scroll-mode-map (kbd "n") #'pdf-continuous-scroll-forward)
                                (define-key pdf-continuous-scroll-mode-map (kbd "p") #'pdf-continuous-scroll-backward)
                                (local-set-key (kbd "<down-mouse-1>") #'pdf-view-mouse-set-region-wapper)
                                (local-set-key (kbd "<double-mouse-1>") #'pdf-traslate-under-mouse)))

(use-package pdf-view-restore
  :commands (pdf-view-restore-mode)
  :config
  (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore")
  :hook ((pdf-view-mode . pdf-view-restore-mode)))

(provide 'init-pdf)
