(setq inhibit-startup-message t)
(setq gnus-inhibit-startup-message t)

(menu-bar-mode 0)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'tab-bar-mode)
  (tab-bar-mode -1)
  (define-key ctl-x-map "t" nil))

(when window-system
  (scroll-bar-mode -1)
  (set-fringe-mode '(nil . 1)))

(setq frame-resize-pixelwise t)
(setq indicate-empty-lines t)
(setq-default line-number-display-limit-width 2000)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(setq-default window-divider-default-right-width 1)
(setq-default window-divider-default-right-width 1)

;;scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;; icons
(setq inhibit-compacting-font-caches t)
(require-package 'all-the-icons)
(require-package 'all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(defun set-frame-transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (set-frame-parameter (selected-frame) 'alpha value))

;; doom-themes
(require-package 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t
      doom-themes-treemacs-theme "doom-colors"
      doom-themes-treemacs-enable-variable-pitch nil)
;;(doom-themes-visual-bell-config)
(doom-themes-org-config)
(doom-themes-treemacs-config)
(after-load 'treemacs
  (remove-hook 'treemacs-mode-hook #'doom-themes-hide-modeline))

;; themes
(require-package 'spacemacs-theme)
(require-package 'leuven-theme)
(require-package 'flucui-themes)
(require-package 'cloud-theme)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(defcustom custom-gui-theme 'cloud
  "Theme in gui mode"
  :group 'faces
  :type 'string)

(defcustom custom-terminal-theme 'doom-one
  "Theme in terminal mode"
  :group 'faces
  :type 'string)

(add-hook 'after-init-hook
          (lambda ()
            (if window-system
                (load-theme custom-gui-theme t)
              (load-theme custom-terminal-theme t))
            (set-face-attribute 'button nil :background nil)
            (set-face-attribute 'fringe nil :background nil)))

;;fonts
(use-package cnfonts
  :demand t
  :config
  (setq cnfonts-use-face-font-rescale t)
  (if window-system
                (cnfonts-enable)))

;;size
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 1280)
            (add-to-list 'default-frame-alist '(width . 120))
          (add-to-list 'default-frame-alist '(width . 80)))
        (add-to-list 'default-frame-alist
                     (cons 'height (/ (- (x-display-pixel-height) 250)
                                      (frame-char-height)))))))

(set-frame-size-according-to-resolution)

;;mode line
(setq mode-line-percent-position nil)
(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

(defun mode-line-pdfview-page-number ()
  "Display page number in pdf-view mode."
  (format " %d/%d"
          (eval `(pdf-view-current-page))
          (pdf-cache-number-of-pages)))

(defun mode-line-docview-page-number ()
  "Display page number in doc-view mode."
  (format " %d/%d"
          (eval `(doc-view-current-page))
          (doc-view-last-page-number)))


(defun mode-line-linum()
  "Display line number."
  (cond ((eq 'pdf-view-mode major-mode) (mode-line-pdfview-page-number))
        ((eq 'doc-view-mode major-mode) (mode-line-docview-page-number))
        ((member major-mode '(term-mode xwidget-webkit-mode)) "")
        (t (format-mode-line " %l:%c"))))

(setq-default auto-revert-check-vc-info t)
(setq-default auto-revert-interval 3)
(setq-default mode-line-format
              '((:eval (mode-line-linum))
                " "
                mode-line-buffer-identification
                " ["
                mode-name
                ;;mode-line-process
                ;;minor-mode-alist
                "]"
                (vc-mode vc-mode)
                " "
                global-mode-string))

;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   :bind
;;   ("C-<prior>" . centaur-tabs-backward)
;;   ("C-<next>" . centaur-tabs-forward))

(provide 'init-ui)
