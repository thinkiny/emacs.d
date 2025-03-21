;; init-pdf-tools.el -*- lexical-binding: t; -*-

(defconst pdf-tools-lisp-dir (expand-file-name "third-parties/pdf-tools/lisp" user-emacs-directory))
(defvar pdf-view-theme-auto-dark nil)

(require 'qpdf.el)

(defun pdf-fix-selection ()
  "Replace pdf with one where selection shows transparently."
  (interactive)
  (unless (equal (file-name-extension (buffer-file-name)) "pdf")
    (error "Buffer should visit a pdf file."))
  (unless (equal major-mode 'pdf-view-mode)
    (pdf-view-mode))
  ;; save file in QDF-mode
  (qpdf-run (list
             (concat "--infile="
                     (buffer-file-name))
             "--qdf --object-streams=disable"
             "--replace-input"))
  ;; do replacements
  (text-mode)
  (read-only-mode -1)
  (while (re-search-forward "3 Tr" nil t)
    (replace-match "7 Tr" nil nil))
  (save-buffer)
  (pdf-view-mode))

(when (file-directory-p pdf-tools-lisp-dir)
  (add-to-list 'load-path pdf-tools-lisp-dir)
  (require 'pdf-outline)
  (require 'pdf-roll)

  (require 'pdf-history)
  (unbind-key (kbd "N") 'pdf-history-minor-mode-map)

  (require 'pdf-view-restore)
  (setq pdf-view-restore-filename (expand-file-name ".pdf-view-restore" user-emacs-directory))

  ;; (defun calc-image-roll-size-mouse(arg)
  ;;   (cond
  ;;    ((= arg 1) 5)
  ;;    ((< arg 4) 20)
  ;;    (t 50)))

  ;; (defun pdf-image-roll-forward-mouse(&optional arg)
  ;;   (interactive "P")
  ;;   (defvar image-roll-step-size)
  ;;   (let ((image-roll-step-size (calc-image-roll-size-mouse arg)))
  ;;     (image-roll-scroll-forward)))

  ;; (defun pdf-image-roll-backward-mouse(&optional arg)
  ;;   (interactive "P")
  ;;   (defvar image-roll-step-size)
  ;;   (let ((image-roll-step-size (calc-image-roll-size-mouse arg)))
  ;;     (image-roll-scroll-backward)))

  (add-auto-mode 'pdf-view-mode "\\.pdf$")
  (add-hook 'pdf-view-mode-hook 'my-pdf-view-mode-hook))

(after-load-theme
 (require 'pdf-tools)
 (when (theme-dark-p)
   (add-to-list 'pdf-tools-enabled-modes 'pdf-view-dark-minor-mode)
   (setq pdf-view-midnight-colors '("#ffffff" . "#282c34"))
   (if pdf-view-theme-auto-dark
       (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode))))


(with-eval-after-load 'pdf-view
  ;; (setq pdf-view-use-unicode-ligther nil)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil)
  (setq pdf-continuous-scroll-step 15)
  (setq pdf-links-browse-uri-function #'xwidget-webkit-browse-url)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; Silence "File *.pdf is large (X MiB), really open?" prompts for pdfs
  (defadvice! +pdf-suppress-large-file-prompts-a (fn size op-type filename &optional offer-raw)
    :around #'abort-if-file-too-large
    (unless (string-match-p "\\.pdf\\'" filename)
      (funcall fn size op-type filename offer-raw)))

  (add-to-list 'display-buffer-alist
               '("\\*Outline .*pdf\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.25)))

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

  (defvar pdf-view-scroll-step 10)
  (defun pdf-view-scroll-up ()
    (interactive)
    (pdf-view-scroll-up-or-next-page pdf-view-scroll-step))

  (defun pdf-view-scroll-down ()
    (interactive)
    (pdf-view-scroll-down-or-previous-page pdf-view-scroll-step))

  (defun pdf-view-mouse-set-region-wapper (event)
    (interactive "@e")
    (pdf-view-mouse-set-region event)
    (pdf-traslate-under-mouse event))

  (defun pdf-translate-selection (&rest _)
    "Translate the selected word"
    (interactive)
    (run-with-idle-timer 0.1 nil (lambda ()
                                   (let ((text (car (pdf-view-active-region-text))))
                                     (if (> (length text) 0)
                                         (bing-dict-brief text))))))

  (advice-add #'pdf-view--push-mark :after #'pdf-translate-selection))

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

(defun my-pdf-view-mode-hook()
  (pdf-view-restore-mode)
  (pdf-outline-minor-mode)
  (pdf-history-minor-mode)
  (blink-cursor-mode -1)
  (eldoc-mode -1)
  (whitespace-cleanup-mode -1)
  (electric-pair-local-mode -1)
  (electric-indent-local-mode -1)
  (font-lock-mode -1)
  (yas-minor-mode -1)
  ;; (cua-mode -1)
  (pixel-scroll-precision-mode -1)
  (setq-local left-fringe-width 0)
  ;; (if (boundp 'mwheel-scroll-up-function)
  ;;     (setq-local mwheel-scroll-up-function
  ;;                 #'pdf-view-next-line-or-next-page))
  ;; (if (boundp 'mwheel-scroll-down-function)
  ;;     (setq-local mwheel-scroll-down-function
  ;;                 #'pdf-view-previous-line-or-previous-page))

  (define-key pdf-view-mode-map (kbd "q") #'kill-current-buffer)
  (define-key pdf-view-mode-map (kbd "0") #'pdf-view-goto-page-start)
  (define-key pdf-view-mode-map (kbd "n") #'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "p") #'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "j") #'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") #'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "f") #'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "M-c") #'pdf-view-kill-ring-save)
  (define-key pdf-view-mode-map (kbd "b") #'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "N") #'pdf-view-next-page)
  (define-key pdf-view-mode-map (kbd "P") #'pdf-view-previous-page)
  (define-key pdf-view-mode-map (kbd "M-v") #'pdf-view-scroll-down)
  (define-key pdf-view-mode-map (kbd "C-v") #'pdf-view-scroll-up)
  ;;(define-key pdf-view-mode-map (kbd "<down-mouse-1>") #'pdf-view-mouse-set-region-wapper)
  (define-key pdf-view-mode-map (kbd "<double-mouse-1>") #'pdf-traslate-under-mouse)
  ;;(add-function :after after-focus-change-function 'pdf-cscroll-close-window-when-dual)
)

(provide 'init-pdf-tools)
