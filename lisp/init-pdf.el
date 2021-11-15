;; pdf-tools
(use-package pdf-tools
  :mode ("\\.pdf$" . pdf-view-mode)
  :hook (pdf-view-mode . my-pdf-view-mode-hook)
  :config
  (setq pdf-view-use-unicode-ligther nil)
  (setq pdf-view-use-scaling t)
  (setq pdf-view-use-imagemagick nil)
  (setq pdf-continuous-scroll-step 15)
  (setq pdf-tools-enabled-modes (remove 'pdf-sync-minor-mode pdf-tools-enabled-modes))
  (setq pdf-links-browse-uri-function #'xwidget-webkit-browse-url)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  (defadvice! +pdf--install-epdfinfo-a (orig-fn &rest args)
    "Install epdfinfo after the first PDF file, if needed."
    :around #'pdf-info-check-epdfinfo
    (if (file-executable-p pdf-info-epdfinfo-program)
        (apply orig-fn args)
      ;; If we remain in pdf-view-mode, it'll spit out cryptic errors. This
      ;; graceful failure is better UX.
      (fundamental-mode)
      (message "Viewing PDFs in Emacs requires epdfinfo. Use `M-x pdf-tools-install' to build it")))
  (pdf-tools-install-noverify)

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

  (defun advice/after-pdf-outline-follow-link(&rest _)
    (pdf-cscroll-close-window-when-dual)
    (delete-other-windows-vertically))

  (advice-add #'pdf-outline-follow-link :after #'advice/after-pdf-outline-follow-link)

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
    (add-hook 'kill-buffer-hook #'+pdf-cleanup-windows-h nil t)))

(after-load-theme
 (setq pdf-view-midnight-colors
       `(,(face-attribute 'default :foreground) . ,(face-attribute 'default :background))))

(defun my-pdf-view-mode-hook()
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
  (pdf-cscroll-toggle-mode-line)
  (unbind-key (kbd "N") 'pdf-history-minor-mode-map)
  (local-set-key (kbd "q") #'kill-current-buffer)
  (local-set-key (kbd "0") #'pdf-view-goto-page-start)
  (local-set-key (kbd "N") #'pdf-view-scroll-up-or-next-page)
  (local-set-key (kbd "P") #'pdf-view-scroll-down-or-previous-page)
  (local-set-key (kbd "C-v") #'pdf-continuous-next-page)
  (local-set-key (kbd "M-v") #'pdf-continuous-previous-page)
  (define-key pdf-continuous-scroll-mode-map (kbd "n") #'pdf-continuous-scroll-forward)
  (define-key pdf-continuous-scroll-mode-map (kbd "p") #'pdf-continuous-scroll-backward)
  (local-set-key (kbd "<down-mouse-1>") #'pdf-view-mouse-set-region-wapper)
  (local-set-key (kbd "<double-mouse-1>") #'pdf-traslate-under-mouse)
  (add-function :after after-focus-change-function 'pdf-cscroll-close-window-when-dual))

(use-package pdf-view-restore
  :commands (pdf-view-restore-mode)
  :config
  (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore")
  :hook ((pdf-view-mode . pdf-view-restore-mode)))

(provide 'init-pdf)
