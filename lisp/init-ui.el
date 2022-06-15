(setq inhibit-startup-message t)
(setq gnus-inhibit-startup-message t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode 0)
(when (fboundp 'tab-bar-mode)
  (tab-bar-mode -1)
  (define-key ctl-x-map "t" nil))

(when window-system
  (scroll-bar-mode -1)
  (set-fringe-mode '(nil . 1)))

(setq frame-resize-pixelwise t)
(setq indicate-empty-lines t)
(setq-default line-number-display-limit-width 2000000)
(setq-default window-divider-default-right-width 1)
(setq ring-bell-function 'ignore)

(global-set-key (kbd "C-x c f") #'customize-face)
(global-set-key (kbd "C-x c v") #'customize-variable)
(global-set-key (kbd "C-x c g") #'customize-group)

;; scroll
;; (if (fboundp 'pixel-scroll-precision-mode)
;;     (pixel-scroll-precision-mode)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;; icons
(use-package all-the-icons)
(when window-system
  (use-package all-the-icons-dired)
  (window-divider-mode)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :init (all-the-icons-ibuffer-mode))

;; doom-themes
(require-package 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic nil
      doom-opera-brighter-modeline t
      doom-themes-treemacs-theme "doom-colors"
      doom-themes-treemacs-enable-variable-pitch nil)
;;(doom-themes-visual-bell-config)
(doom-themes-org-config)
(doom-themes-treemacs-config)
(with-eval-after-load 'treemacs
  (remove-hook 'treemacs-mode-hook #'doom-themes-hide-modeline))

;; frame transparency
(defcustom frame-transparency 100
  "The Transparency of frame"
  :group 'faces
  :type 'integer
  :set (lambda (var val)
         (set-default var val)
         (set-frame-parameter (selected-frame) 'alpha val)))

(defun set-transparency ()
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive)
  (when window-system
    (let* ((value (read-number "change frame transparency: " frame-transparency)))
      (customize-save-variable 'frame-transparency value))))

;; themes
(require-package 'cloud-theme)
(require-package 'modus-themes)
(require-package 'inkpot-theme)
(require-package 'vscode-dark-plus-theme)
(with-eval-after-load 'modus-themes
  (setq modus-themes-tabs-accented t
        modus-themes-paren-match '(bold intense)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(defcustom custom-theme (cons 'inkpot 'dark)
  "The current theme"
  :group 'faces
  :type 'cons)

(defun is-custom-theme-dark ()
  (eq 'dark (cdr custom-theme)))

(after-load-theme
 (set-face-attribute 'button nil :background nil)
 (set-face-attribute 'compilation-info nil :foreground "DeepSkyBlue4")
 (set-face-attribute 'fringe nil :background nil)
 (when (is-custom-theme-dark)
   (set-face-attribute 'ivy-completions-annotations nil :inherit 'italic)
   (set-face-attribute 'all-the-icons-ibuffer-size-face nil :inherit nil)
   (set-face-attribute 'all-the-icons-ivy-rich-size-face nil :inherit nil)
   (set-face-attribute 'default nil :foreground "#C4C4C4")
   (set-face-attribute 'ivy-virtual nil :foreground nil)))

(add-hook 'after-init-hook
          (lambda ()
            (load-theme (car custom-theme) t)
            (run-hooks 'load-theme-hook)))

;;fonts
(when window-system
  (use-package cnfonts
    :demand t
    :config
    (setq cnfonts-use-face-font-rescale t)
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
        ((member major-mode '(eshell-mode term-mode xwidget-webkit-mode)) "")
        (t (format-mode-line " %l"))))


;; lsp symbol
(defvar lsp-modeline-symbol "")

;; update lsp-symbol every two seconds
(run-at-time 2 2 (lambda ()
                   (if (fboundp 'lsp-modeline-get-symbol-name)
                       (setq lsp-modeline-symbol (lsp-modeline-get-symbol-name))
                     (setq lsp-modeline-symbol ""))))

(setq-default auto-revert-check-vc-info t)
(setq-default auto-revert-interval 3)
(setq-default mode-line-format
              '((:eval (mode-line-linum))
                " "
                mode-line-buffer-identification
                lsp-modeline-symbol
                " ["
                mode-name
                ;;minor-mode-alist
                "]"
                ;;(vc-mode vc-mode)
                " "
                global-mode-string
                ))

;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   :bind
;;   ("C-<prior>" . centaur-tabs-backward)
;;   ("C-<next>" . centaur-tabs-forward))

(defun counsel--load-theme-action (x)
  "Disable current themes and load theme X."
  (condition-case nil
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme (intern x) t)
        (run-hooks 'load-theme-hook))
    (error "Problem loading theme %s" x)))

(defun counsel--update-theme-action ()
  "Change theme to selected."
  (counsel--load-theme-action (ivy-state-current ivy-last)))

(defun counsel-load-theme ()
  "Forward to `load-theme'."
  (interactive)
  (let ((orig-theme (when custom-enabled-themes
                      (car custom-enabled-themes))))
    (ivy-read "Load custom theme: "
            (mapcar 'symbol-name (custom-available-themes))
            :action #'counsel--load-theme-action
            :preselect (symbol-name orig-theme)
            ;;:update-fn #'counsel--update-theme-action
            )))

;; hide-mode-line
(require-package 'hide-mode-line)

;; dashboard
(use-package dashboard
  :demand t
  :config
  (setq dashboard-startup-banner nil)
  (setq dashboard-set-init-info nil)
  (setq dashboard-set-navigator nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons nil)
  (setq dashboard-set-footer nil)
  (setq dashboard-bookmarks-show-base 'align)
  (setq initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name)))
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (setq dashboard-items '((recents  . 15)
                          (bookmarks . 10)
                          (agenda . 5)))
  (dashboard-setup-startup-hook)

  (defun switch-to-dashboard()
    (interactive)
    (let ((buf (get-buffer-create dashboard-buffer-name))
          (dashboard-force-refresh t))
      (dashboard-insert-startupify-lists)
      (switch-to-buffer buf)))

  (global-set-key (kbd "C-h h") #'switch-to-dashboard))

;; term-color
(defun justify-term-theme()
  (when (and (is-custom-theme-dark) (featurep 'term))
    (set-face-background 'term-color-black (face-attribute 'default :foreground))
    (set-face-foreground 'term-color-blue "skyblue3")
    (set-face-foreground 'term-color-red "IndianRed1")))

(after-load-theme (justify-term-theme))
(with-eval-after-load 'term
  (justify-term-theme))

(provide 'init-ui)
