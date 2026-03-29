;; -*- lexical-binding: t -*-

;;; Code:
(require 'cl-lib)
(setq inhibit-startup-message t)

(when (fboundp 'tab-bar-mode)
  (tab-bar-mode -1)
  (define-key ctl-x-map "t" nil))

(when window-system
  (blink-cursor-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

(defun enable-menu-bar ()
  (menu-bar-mode)
  (global-unset-key [menu-bar buffer])
  (remove-hook 'menu-bar-update-hook 'menu-bar-update-buffers)
  (with-eval-after-load 'imenu
    (remove-hook 'menu-bar-update-hook 'imenu-update-menubar)))

(setq frame-resize-pixelwise t)
(setq indicate-empty-lines t)
(setq-default line-number-display-limit-width 2000000)
(setq ring-bell-function 'ignore)

(global-set-key (kbd "C-x c f") #'customize-face)
(global-set-key (kbd "C-x c v") #'customize-variable)
(global-set-key (kbd "C-x c g") #'customize-group)

;; scrolling and pixel-scroll helpers
(when window-system
  (pixel-scroll-precision-mode)
  (setq mouse-autoselect-window nil)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq scroll-step 1
        scroll-conservatively 101))

(defconst precision-scroll-step-height 90)
(defconst precision-scroll-taller-line 150)
(defconst precision-scroll-page-lines 20)

(defun precision-scroll-line-height ()
  (frame-char-height))

(defun get-precision-scroll-page-height ()
  (* precision-scroll-page-lines (precision-scroll-line-height)))

(defun precision-scroll-current-line-taller-p ()
  (> (car (window-line-height)) (frame-char-height)))

(defun precision-scroll-forward-line ()
  (interactive)
  (if (precision-scroll-current-line-taller-p)
      (ignore-errors
        (pixel-scroll-precision-scroll-down precision-scroll-taller-line))
    (vertical-motion 1)))

(defun precision-scroll-backward-line ()
  (interactive)
  (if (precision-scroll-current-line-taller-p)
      (ignore-errors
        (pixel-scroll-precision-scroll-up precision-scroll-taller-line))
    (vertical-motion -1)))

(defun precision-scroll-up-page ()
  (interactive)
  (pixel-scroll-precision-scroll-down-page (get-precision-scroll-page-height)))

(defun precision-scroll-down-page ()
  (interactive)
  (pixel-scroll-precision-scroll-up-page (get-precision-scroll-page-height)))

;; icons/advice
(use-package all-the-icons)

(use-package treemacs-icons-dired
  :when window-system
  :config
  (defun treemacs-icons-dired-display-local-p ()
    (not (file-remote-p default-directory)))
  (advice-add 'treemacs-icons-dired--display :before-while #'treemacs-icons-dired-display-local-p)
  (treemacs-icons-dired-mode))

;; frame appearance and theme support
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(defcustom frame-transparency 100
  "Transparency of frame."
  :group 'faces
  :type 'integer
  :set (lambda (symbol val)
         (set-default symbol val)
         (if (bound-and-true-p *is-a-nt*) ; Windows
             (set-frame-parameter nil 'alpha (cons val val))
           (set-frame-parameter nil 'alpha-background val))))

(defun set-transparency ()
  "Set the transparency of the frame window from 0=transparent to 100=opaque."
  (interactive)
  (when window-system
    (let ((value (read-number "change frame transparency: " frame-transparency)))
      (customize-save-variable 'frame-transparency value))))

;; themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(use-package modus-themes)
(use-package ef-themes)

(defcustom default-theme 'modus-operandi
  "The current theme."
  :group 'faces
  :type 'symbol)

(defcustom reader-css-theme 'auto
  "Reader stylesheet theme selection.
`auto' follows the current Emacs theme. `light' always uses light
reader assets."
  :group 'faces
  :type '(choice (const :tag "Follow Emacs theme" auto)
                 (const :tag "Always use light reader assets" light)))

(defun theme-dark-p ()
  (eq 'dark (frame-parameter nil 'background-mode)))

(defun reader-css-theme-resolved ()
  "Return the effective reader stylesheet mode."
  (if (eq reader-css-theme 'light)
      'light
    (if (theme-dark-p) 'dark 'light)))

(defun reader-css-theme-use-stock-light-p ()
  "Return non-nil when the reader should use stock light assets."
  (and (eq reader-css-theme 'light)
       (theme-dark-p)))

(defun load-and-activate-theme (theme)
  "Disable active themes, load THEME, and run theme hooks."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (run-hooks 'load-theme-hook)
  (customize-save-variable 'default-theme theme))

(after-load-theme
 (set-face-attribute 'button nil :background 'unspecified)
 (set-face-attribute 'fringe nil :background 'unspecified))

(with-eval-after-load-theme
 'compile
 (set-face-attribute 'compilation-info nil :foreground "DeepSkyBlue4"))

(add-hook 'after-init-hook
          (lambda ()
            (load-and-activate-theme default-theme)))

;; fonts and frame sizing
(when window-system
  (setq face-font-selection-order '(:width :height :slant :weight))
  (use-package cnfonts
    :config
    (setq cnfonts-use-face-font-rescale t)
    (setq cnfonts-disable-italic nil)
    (setq use-default-font-for-symbols nil)
    (cnfonts-mode)
    (unbind-all-keys cnfonts-mode-map)))

(defun apply-frame-size-for-display (&optional frame)
  "Apply appropriate frame size based on display width."
  (when window-system
    (let* ((large-p (> (x-display-pixel-width) 1280))
           (w (if large-p 130 80))
           (h (if large-p 50 30)))
      (setf (alist-get 'width default-frame-alist) w)
      (setf (alist-get 'height default-frame-alist) h)
      (when frame
        (set-frame-size frame w h)))))

(apply-frame-size-for-display)
(add-hook 'after-make-frame-functions #'apply-frame-size-for-display)

;; disable fringe in xwidget-webkit-mode
(defun sync-fringe-for-xwidget (frame)
  "Update fringes only for the selected window in FRAME."
  (when (frame-live-p frame)
    (let ((window (frame-selected-window frame)))
      (unless (window-minibuffer-p window)
        (with-current-buffer (window-buffer window)
          (set-window-fringes window
                              (if (derived-mode-p 'xwidget-webkit-mode) 0 nil)
                              (if (derived-mode-p 'xwidget-webkit-mode) 0 nil)))))))

(add-hook 'window-state-change-functions #'sync-fringe-for-xwidget)

;; mode-line support
(use-package hide-mode-line)
(setq-default mode-line-percent-position nil
              mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

(defun mode-line-position ()
  "Display line number."
  (if (derived-mode-p '(special-mode vterm-mode))
      ""
    (format-mode-line " %l:%C")))

(cl-defstruct (mode-line-cache (:constructor mode-line-cache-create))
  project-name
  flymake-counters
  persp-project)

(defvar-local mode-line-cache nil)
(defvar mode-line-cache-timer nil)
(defconst mode-line-cache-refresh-interval 2)

(defun mode-line-cache-get ()
  (or mode-line-cache
      (setq mode-line-cache (mode-line-cache-create))))

(defun mode-line-cache--compute-project-name ()
  "Return current project name for mode-line cache."
  (let ((project-name
         (when (and (fboundp 'projectile-project-root)
                    (boundp 'projectile-project-name-function))
           (when-let* ((project-root (projectile-project-root)))
             (funcall projectile-project-name-function project-root)))))
    (or project-name "")))

(defun mode-line-cache--compute-persp-project (project-name)
  (if (bound-and-true-p persp-mode)
      `("[" ,(persp-current-name) "] " ,project-name)
    project-name))

(defun mode-line-cache--compute-flymake-counters ()
  (if (bound-and-true-p flymake-mode)
      (flymake--mode-line-counters)
    ""))

(defun mode-line-cache-refresh (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let* ((cache (mode-line-cache-get))
           (project-name (or (mode-line-cache-project-name cache)
                             (mode-line-cache--compute-project-name)))
           (flymake-counters (mode-line-cache--compute-flymake-counters))
           (persp-project (mode-line-cache--compute-persp-project project-name)))
      (setf (mode-line-cache-project-name cache) project-name)
      (setf (mode-line-cache-flymake-counters cache) flymake-counters)
      (setf (mode-line-cache-persp-project cache) persp-project))))

(defun mode-line-cache-refresh-current-buffer ()
  (let ((buffer (window-buffer (selected-window))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (mode-line-cache-refresh)))))

(defun mode-line-cache-start-timer ()
  (when (timerp mode-line-cache-timer)
    (cancel-timer mode-line-cache-timer))
  (mode-line-cache-refresh-current-buffer)
  (setq mode-line-cache-timer
        (run-with-timer mode-line-cache-refresh-interval
                        mode-line-cache-refresh-interval
                        #'mode-line-cache-refresh-current-buffer)))

(defun mode-line-persp-project ()
  (mode-line-cache-persp-project (mode-line-cache-get)))

(defun mode-line-flymake-counters ()
  (mode-line-cache-flymake-counters (mode-line-cache-get)))

(setq-default mode-line-format
              '((:eval (mode-line-position))
                " "
                "%b"
                (:eval (mode-line-flymake-counters))
                " "
                (:eval (mode-line-persp-project))))

(mode-line-cache-start-timer)

;; interactive theme commands
(defun change-theme--action (x)
  "Load theme X and run theme hooks once."
  (condition-case err
      (load-and-activate-theme (intern x))
    (error (user-error "%s" (error-message-string err)))))

(defun change-theme ()
  "Change current theme."
  (interactive)
  (ivy-read "Load custom theme: "
            (mapcar #'symbol-name (custom-available-themes))
            :action #'change-theme--action
            :preselect (symbol-name (or (when custom-enabled-themes
                                          (car custom-enabled-themes))
                                        default-theme))))

(defun current-theme-bg-hex ()
  "Return the current default face background color as #rrggbb."
  (let ((color (face-attribute 'default :background nil t)))
    (when (or (null color) (equal color "unspecified-bg"))
      (user-error "Default face background is not specified"))
    (let ((rgb (color-values color)))
      (unless rgb
        (user-error "Unable to resolve color values for %s" color))
      (format "#%02x%02x%02x"
              (/ (nth 0 rgb) 257)
              (/ (nth 1 rgb) 257)
              (/ (nth 2 rgb) 257)))))

(defun current-theme-fg-hex ()
  "Return the current default face foreground color as #rrggbb."
  (let ((color (face-attribute 'default :foreground nil t)))
    (when (or (null color) (equal color "unspecified-fg"))
      (user-error "Default face foreground is not specified"))
    (let ((rgb (color-values color)))
      (unless rgb
        (user-error "Unable to resolve color values for %s" color))
      (format "#%02x%02x%02x"
              (/ (nth 0 rgb) 257)
              (/ (nth 1 rgb) 257)
              (/ (nth 2 rgb) 257)))))

(defun sync-reader-theme-colors ()
  "Rebuild reader assets to match the current reader theme."
  (interactive)
  (let* ((theme (reader-css-theme-resolved))
         (use-stock-light (reader-css-theme-use-stock-light-p))
         (foreground (current-theme-fg-hex))
         (script (expand-file-name "scripts/update_reader_theme_colors.py"
                                   user-emacs-directory))
         (output-buffer (get-buffer-create "*update-reader-theme-colors*"))
         (default-directory user-emacs-directory)
         (args (if use-stock-light
                   (list script "--restore-stock-light-assets")
                 (list script (symbol-name theme) foreground)))
         (exit-code (with-current-buffer output-buffer
                      (erase-buffer)
                      (apply #'call-process "python3" nil output-buffer nil args))))
    (if (zerop exit-code)
        (progn
          (message
           (if use-stock-light
               "Restored stock light reader assets"
             (format "Rebuilt reader assets for %s theme with transparent background and foreground %s"
                     theme foreground)))
          (when (buffer-live-p output-buffer)
            (kill-buffer output-buffer)))
      (display-buffer output-buffer)
      (error "%s"
             (if use-stock-light
                 (format "Failed to restore stock light reader assets (exit %s)" exit-code)
               (format "Failed to rebuild reader assets for %s theme with transparent background and foreground %s (exit %s)"
                       theme foreground exit-code))))))

(defun cap-brightness-in-dark-theme()
  (when (theme-dark-p)
    (let* ((color (face-attribute 'default :foreground))
           (hsl (nth 2 (apply #'color-rgb-to-hsl (color-name-to-rgb color)))))
      (if (> hsl 0.9)
          (set-face-attribute 'default nil :foreground "#E0E0E0")))))

(after-load-theme
 (cap-brightness-in-dark-theme)
 (sync-reader-theme-colors))

(provide 'init-ui)
