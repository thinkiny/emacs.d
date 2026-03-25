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
  (defun my-treemacs-icons-dired-display-p ()
    (not (file-remote-p default-directory)))
  (advice-add 'treemacs-icons-dired--display :before-while #'my-treemacs-icons-dired-display-p)
  (treemacs-icons-dired-mode))

;; frame appearance and theme support
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

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
    (let ((value (read-number "change frame transparency: " frame-transparency)))
      (customize-save-variable 'frame-transparency value))))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(use-package modus-themes
  :config
  (setq modus-themes-tabs-accented t
        modus-themes-paren-match '(bold intense)))

(use-package ef-themes)

(defcustom default-theme 'modus-operandi
  "The current theme"
  :group 'faces)

(defun theme-dark-p ()
  (eq 'dark (frame-parameter nil 'background-mode)))

(after-load-theme
 (set-face-attribute 'button nil :background 'unspecified)
 (set-face-attribute 'fringe nil :background 'unspecified))

(with-eval-after-load 'compile
  (after-load-theme
   (set-face-attribute 'compilation-info nil :foreground "DeepSkyBlue4")))

(with-eval-after-load 'ivy
  (after-load-theme
   (set-face-attribute 'ivy-virtual nil :foreground 'unspecified)
   (when (theme-dark-p)
     (set-face-attribute 'ivy-completions-annotations nil :inherit 'italic))))

(add-hook 'after-init-hook
          (lambda ()
            (load-theme default-theme t)
            (run-hooks 'load-theme-hook)))

;; fonts and frame sizing
(when window-system
  (use-package cnfonts
    :config
    (setq cnfonts-use-face-font-rescale t)
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

(cl-defstruct (my-mode-line-cache (:constructor my-mode-line-cache-create))
  project-name
  flymake-counters
  persp-project)

(defvar-local my-mode-line-cache nil)
(defvar my-mode-line-cache-timer nil)
(defconst my-mode-line-cache-refresh-interval 2)

(defun my-mode-line-cache-get ()
  (or my-mode-line-cache
      (setq my-mode-line-cache (my-mode-line-cache-create))))

(defun my-mode-line-cache--compute-project-name ()
  "Return current project name for mode-line cache."
  (let ((project-name
         (when (and (fboundp 'projectile-project-root)
                    (boundp 'projectile-project-name-function))
           (when-let* ((project-root (projectile-project-root)))
             (funcall projectile-project-name-function project-root)))))
    (or project-name "")))

(defun my-mode-line-cache--compute-persp-project (project-name)
  (if (bound-and-true-p persp-mode)
      `("[" ,(persp-current-name) "] " ,project-name)
    project-name))

(defun my-mode-line-cache--compute-flymake-counters ()
  (if (bound-and-true-p flymake-mode)
      (flymake--mode-line-counters)
    ""))

(defun my-mode-line-cache-refresh (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let* ((cache (my-mode-line-cache-get))
           (project-name (or (my-mode-line-cache-project-name cache)
                             (my-mode-line-cache--compute-project-name)))
           (flymake-counters (my-mode-line-cache--compute-flymake-counters))
           (persp-project (my-mode-line-cache--compute-persp-project project-name)))
      (setf (my-mode-line-cache-project-name cache) project-name)
      (setf (my-mode-line-cache-flymake-counters cache) flymake-counters)
      (setf (my-mode-line-cache-persp-project cache) persp-project))))

(defun my-mode-line-cache-refresh-current-buffer ()
  (let ((buffer (window-buffer (selected-window))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (my-mode-line-cache-refresh)))))

(defun my-mode-line-cache-start-timer ()
  (when (timerp my-mode-line-cache-timer)
    (cancel-timer my-mode-line-cache-timer))
  (my-mode-line-cache-refresh-current-buffer)
  (setq my-mode-line-cache-timer
        (run-with-timer my-mode-line-cache-refresh-interval
                        my-mode-line-cache-refresh-interval
                        #'my-mode-line-cache-refresh-current-buffer)))

(defun mode-line-persp-project ()
  (my-mode-line-cache-persp-project (my-mode-line-cache-get)))

(defun mode-line-flymake-counters ()
  (my-mode-line-cache-flymake-counters (my-mode-line-cache-get)))

(setq-default mode-line-format
              '((:eval (mode-line-position))
                " "
                "%b"
                (:eval (mode-line-flymake-counters))
                " "
                (:eval (mode-line-persp-project))))

(my-mode-line-cache-start-timer)

;; interactive theme commands
(defun my-load-theme-action (x)
  "Disable current themes and load theme X."
  (condition-case nil
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme (intern x) t)
        (run-hooks 'load-theme-hook))
    (error "Problem loading theme %s" x)))

(defun change-theme ()
  "Change current theme."
  (interactive)
  (ivy-read "Load custom theme: "
            (mapcar #'symbol-name (custom-available-themes))
            :action #'my-load-theme-action
            :preselect (symbol-name (or (when custom-enabled-themes
                                          (car custom-enabled-themes))
                                        default-theme))))

(defun set-current-theme-default ()
  (interactive)
  (if-let* ((current-theme (car custom-enabled-themes)))
      (customize-save-variable 'default-theme current-theme)))

(provide 'init-ui)
