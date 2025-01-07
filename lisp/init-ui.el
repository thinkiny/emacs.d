(setq inhibit-startup-message t)
(setq gnus-inhibit-startup-message t)

(when (fboundp 'tab-bar-mode)
  (tab-bar-mode -1)
  (define-key ctl-x-map "t" nil))

(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (set-fringe-mode '(nil . 1))
  (scroll-bar-mode -1)
  (setq mouse-avoidance-mode 'animate))

(setq frame-resize-pixelwise t)
(setq indicate-empty-lines t)
(setq-default line-number-display-limit-width 2000000)
;; (setq-default window-divider-default-right-width 1)
(setq ring-bell-function 'ignore)

(global-set-key (kbd "C-x c f") #'customize-face)
(global-set-key (kbd "C-x c v") #'customize-variable)
(global-set-key (kbd "C-x c g") #'customize-group)

;; scroll
(pixel-scroll-precision-mode)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      auto-window-vscroll nil)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;; icons
(use-package all-the-icons)
(use-package all-the-icons-dired)

(when window-system
  ;; (window-divider-mode)
  (add-hook 'dired-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (all-the-icons-dired-mode)))))

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
         (if *is-a-nt*
             (set-frame-parameter nil 'alpha val)
           (set-frame-parameter nil 'alpha-background val))))

(defun set-transparency ()
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive)
  (when window-system
    (let* ((value (read-number "change frame transparency: " frame-transparency)))
      (customize-save-variable 'frame-transparency value))))

;; themes
(require-package 'cloud-theme)
(require-package 'modus-themes)
(require-package 'ef-themes)
(with-eval-after-load 'modus-themes
  (setq modus-themes-tabs-accented t
        modus-themes-paren-match '(bold intense)))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(defcustom default-theme 'modus-operandi
  "The current theme"
  :group 'faces)

(defun theme-dark-p ()
  (eq 'dark (frame-parameter nil 'background-mode)))

(after-load-theme
 (set-face-attribute 'button nil :background 'unspecified)
 (set-face-attribute 'compilation-info nil :foreground "DeepSkyBlue4")
 (set-face-attribute 'ivy-virtual nil :foreground 'unspecified)
 (set-face-attribute 'variable-pitch-text nil :height 1.0)
 ;;(set-face-attribute 'fringe nil :background nil)
 (when (theme-dark-p)
   (set-face-attribute 'ivy-completions-annotations nil :inherit 'italic)
   ;;(set-face-attribute 'default nil :foreground "#C4C4C4")
   ))

(add-hook 'after-init-hook
          (lambda ()
            (load-theme default-theme t)
            (run-hooks 'load-theme-hook)))

;;fonts
(when window-system
  (use-package cnfonts
    :ensure t
    :demand t
    :config
    (setq cnfonts-use-face-font-rescale t)
    (cnfonts-enable)
    (unbind-all-keys cnfonts-mode-map)))

;;size
(defun set-large-frame-size()
  (add-to-list 'default-frame-alist (cons 'width 120))
  (add-to-list 'default-frame-alist (cons 'height 45)))

(defun set-small-frame-size()
  (add-to-list 'default-frame-alist (cons 'width 80))
  (add-to-list 'default-frame-alist (cons 'height 30)))

(defun adjust-frame-size ()
  (when window-system
   (if (> (x-display-pixel-width) 1280)
      (set-large-frame-size)
      (set-small-frame-size))))

(adjust-frame-size)

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
        ((eq 'nov-mode major-mode) (modeline-nov-document-index))
        ((member major-mode '(eshell-mode term-mode xwidget-webkit-mode)) "")
        (t (format-mode-line " %l:%C"))))

;; project-name in mode-line
(defun projectile-project-name-optional()
   "Return project name.
 If PROJECT is not specified acts on the current project."
   (let ((project-root (projectile-project-root)))
     (if project-root
         (funcall projectile-project-name-function project-root)
       "")))

(defun project-name-mode-line ()
  (cond
   ((bound-and-true-p eglot--managed-mode) (propertize
                                            (eglot-project-nickname (eglot-current-server))
                                            'face 'eglot-mode-line
                                            'keymap (let ((map (make-sparse-keymap)))
                                                      (define-key map [mode-line down-mouse-1] eglot-menu)
                                                      map)))
   (t (projectile-project-name-optional))))

(defun persp-with-project-name-mode-line()
  `("[" ,(persp-current-name) "] " ,(project-name-mode-line)))


(defun my-flymake-mode-line-counters ()
  (if (bound-and-true-p flymake-mode)
      (flymake--mode-line-counters) ""))

(setq-default auto-revert-check-vc-info t)
(setq-default auto-revert-interval 3)
(setq-default mode-line-format
              '((:eval (mode-line-linum))
                " "
                "%b"
                ;;" ["
                ;; mode-name
                ;;minor-mode-alist
                ;; "] "
                ;; global-mode-string
                ;; " "
                (:eval (my-flymake-mode-line-counters))
                " "
                (:eval (persp-with-project-name-mode-line))
                " "
                mode-line-misc-info
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

(defun change-theme ()
  "Change current theme."
  (interactive)
  (ivy-read "Load custom theme: "
            (mapcar 'symbol-name (custom-available-themes))
            :action #'counsel--load-theme-action
            :preselect (symbol-name (or (when custom-enabled-themes
                                          (car custom-enabled-themes))
                                        default-theme))
            ;;:update-fn #'counsel--update-theme-action
            ))

;; hide-mode-line
(require-package 'hide-mode-line)

(provide 'init-ui)
