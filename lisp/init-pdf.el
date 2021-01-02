;; pdf-tools
(use-package pdf-tools
  :demand t
  :config
  (require 'pdf-continuous-scroll-mode)
  (pdf-tools-install nil t t nil)
  (set-face-attribute 'outline-1 nil :height 1.0)
  (setq pdf-view-use-unicode-ligther nil)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil)
  (setq pdf-tools-enabled-modes (remove 'pdf-sync-minor-mode pdf-tools-enabled-modes))
  (setq pdf-links-browse-uri-function #'xwidget-webkit-browse-url)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

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
                                (electric-indent-mode -1)
                                (font-lock-mode -1)
                                (yas-minor-mode -1)
                                (setq-local left-fringe-width 1)
                                (setq pdf-view-midnight-colors `(,(face-attribute 'default :foreground) . ,(face-attribute 'default :background)))
                                (pdf-view-midnight-minor-mode)
                                (pdf-continuous-scroll-mode)
                                (local-set-key (kbd "q") #'kill-current-buffer)
                                (local-set-key (kbd "0") #'pdf-view-goto-page-start)
                                (local-set-key (kbd "M-n") #'pdf-view-next-page-start)
                                (local-set-key (kbd "M-p") #'pdf-view-prev-page-start)
                                (local-set-key (kbd "n")  #'pdf-continuous-scroll-forward)
                                (local-set-key (kbd "p")  #'pdf-continuous-scroll-backward)
                                ;;(local-set-key (kbd "n") #'pdf-view-next-page-start)
                                ;;(local-set-key (kbd "p") #'pdf-view-prev-page-start)
                                (local-set-key (kbd "<down-mouse-1>") #'pdf-view-mouse-set-region-wapper)
                                (local-set-key (kbd "<double-mouse-1>") #'pdf-traslate-under-mouse)))

(provide 'init-pdf)
