;; -*- lexical-binding: t -*-

;;; Code:
(setq inhibit-startup-message t)
(setq gnus-inhibit-startup-message t)

(when (fboundp 'tab-bar-mode)
  (tab-bar-mode -1)
  (define-key ctl-x-map "t" nil))

(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

;; menu-bar
(defun enable-menu-bar-mode()
  (menu-bar-mode)
  (global-unset-key [menu-bar buffer])
  (remove-hook 'menu-bar-update-hook 'menu-bar-update-buffers)
  (with-eval-after-load 'imenu
    (remove-hook 'menu-bar-update-hook 'imenu-update-menubar)))

;; fringe
(defun set-fringe-based-on-mode(&optional window)
  (if (derived-mode-p 'xwidget-webkit-mode)
      (set-window-fringes nil 0)
    (set-window-fringes nil nil)))

(with-eval-after-load 'fringe
  ;; (set-fringe-mode '(1 . 0))
  (set-fringe-mode '(nil . 0))
  (add-hook 'window-configuration-change-hook #'set-fringe-based-on-mode))

;; line display params
(setq frame-resize-pixelwise t)
(setq indicate-empty-lines t)
(setq-default line-number-display-limit-width 2000000)
;; (setq-default window-divider-default-right-width 1)
(setq ring-bell-function 'ignore)

(global-set-key (kbd "C-x c f") #'customize-face)
(global-set-key (kbd "C-x c v") #'customize-variable)
(global-set-key (kbd "C-x c g") #'customize-group)

;; mouse && scroll
(when window-system
  (pixel-scroll-precision-mode)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq scroll-step 1
        scroll-conservatively 101
        ;; scroll-up-aggressively 0.01
        ;; scroll-down-aggressively 0.01
        ))

;; scroll functions
;; font-height: (/ (plist-get (font-face-attributes (face-attribute 'default :font)) :height) 10)
(defconst percision-scroll-step-height 90)
(defconst precision-scroll-taller-line 150)
(defconst precision-scroll-page-lines 20)

(defun get-precision-scroll-line-height()
  (frame-char-height))

(defun get-precision-scroll-page-height()
  (* precision-scroll-page-lines (get-precision-scroll-line-height)))

(defun is-taller-this-line()
  (> (car (window-line-height)) (frame-char-height)))

(defun precision-scroll-forward-line()
  (interactive)
  (if (is-taller-this-line)
      (ignore-errors
        (pixel-scroll-precision-scroll-down precision-scroll-taller-line))
    (vertical-motion 1)))

(defun precision-scroll-backward-line()
  (interactive)
  (if (is-taller-this-line)
      (ignore-errors
        (pixel-scroll-precision-scroll-up precision-scroll-taller-line))
    (vertical-motion -1)))

(defun precision-scroll-up-page()
  (interactive)
  (pixel-scroll-precision-scroll-down-page (get-precision-scroll-page-height)))

(defun precision-scroll-down-page()
  (interactive)
  (pixel-scroll-precision-scroll-up-page (get-precision-scroll-page-height)))

;; border
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;; icons
(require-package 'all-the-icons)
(require-package 'treemacs-icons-dired)

(when window-system
  (defun treemacs-icons-dired--display-before()
    (not (file-remote-p default-directory)))
  (advice-add 'treemacs-icons-dired--display :before-while #'treemacs-icons-dired--display-before)
  (treemacs-icons-dired-mode))

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
  "Set the transparency of the frame window from 0=transparent to 100=opaque."
  (interactive)
  (when window-system
    (let* ((value (read-number "change frame transparency: " frame-transparency)))
      (customize-save-variable 'frame-transparency value))))

;; themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(require-package 'modus-themes)
(require-package 'ef-themes)

(with-eval-after-load 'modus-themes
  (setq modus-themes-tabs-accented t
        modus-themes-paren-match '(bold intense)))

;; default-theme
(defcustom default-theme 'modus-operandi
  "The current theme"
  :group 'faces)

(defun theme-dark-p ()
  (eq 'dark (frame-parameter nil 'background-mode)))

(after-load-theme
 (set-face-attribute 'button nil :background 'unspecified)
 (set-face-attribute 'compilation-info nil :foreground "DeepSkyBlue4")
 (set-face-attribute 'ivy-virtual nil :foreground 'unspecified)
 ;;(set-face-attribute 'variable-pitch-text nil :height 1.0)
 (set-face-attribute 'fringe nil :background 'unspecified)
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
    :demand t
    :config
    (setq cnfonts-use-face-font-rescale t)
    (setq use-default-font-for-symbols nil)
    (cnfonts-mode)
    (unbind-all-keys cnfonts-mode-map)))

;;size
(defun set-large-frame-size()
  (add-to-list 'default-frame-alist (cons 'width 130))
  (add-to-list 'default-frame-alist (cons 'height 50)))

(defun set-small-frame-size()
  (add-to-list 'default-frame-alist (cons 'width 80))
  (add-to-list 'default-frame-alist (cons 'height 30)))

(when window-system
  (if (> (x-display-pixel-width) 1280)
      (set-large-frame-size)
    (set-small-frame-size)))

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
  (cond
   ;; ((eq 'pdf-view-mode major-mode) (mode-line-pdfview-page-number))
   ;; ((eq 'doc-view-mode major-mode) (mode-line-docview-page-number))
   ;; ((eq 'nov-mode major-mode) (modeline-nov-document-index))
   ((derived-mode-p '(special-mode vterm-mode)) "")
   ((eq 'nov-xwidget-webkit-mode major-mode) (modeline-nov-document-index))
   (t (format-mode-line " %l:%C"))))

;; project-name in mode-line
(defvar-local mode-line-project-name nil)
(defun mode-line-projectile-project-name()
  "Return project name."
  (if mode-line-project-name
      mode-line-project-name
    (setq mode-line-project-name "")
    (unless (tramp-tramp-file-p default-directory)
      (if-let* ((project-root (projectile-project-root))
                (project-name (funcall projectile-project-name-function project-root)))
          (setq mode-line-project-name project-name)))
    mode-line-project-name))

(defun persp-with-project-name-mode-line()
  (if (fboundp 'persp-current-name)
      `("[" ,(persp-current-name) "] " ,(mode-line-projectile-project-name))))

(defun my-flymake-mode-line-counters ()
  (if (bound-and-true-p flymake-mode)
      (flymake--mode-line-counters) ""))

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
                ;; " "
                ;; mode-line-misc-info
                ))

;; counsel theme
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

(defun set-current-theme-default()
  (interactive)
  (if-let* ((current-theme (car custom-enabled-themes)))
      (customize-save-variable 'default-theme current-theme)))

;; hide-mode-line
(require-package 'hide-mode-line)

(provide 'init-ui)
