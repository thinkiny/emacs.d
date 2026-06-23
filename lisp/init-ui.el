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

(defun precision-scroll-nontext-height ()
  "Pixel threshold for detecting tall non-text content, scaled to window size."
  (/ (window-pixel-height) 5))

(defun precision-scroll--line-non-text-p (&optional dir)
  "Non-nil if point (or adjacent line in DIR) has tall non-text display content."
  (save-excursion
    (when dir (vertical-motion dir))
    (let* ((pos (point))
           (line-h (car (window-line-height)))
           (ovs (overlays-at pos))
           (has-display (or (get-char-property pos 'display)
                            (cl-some (lambda (ov) (overlay-get ov 'display))
                                     ovs))))
      (or (cl-some (lambda (ov) (eq (overlay-get ov 'face) 'shr-sliced-image))
                   ovs)
          (and has-display line-h
               (> line-h (precision-scroll-nontext-height)))))))

(defun precision-scroll--skip-to-text (dir)
  "Walk DIR visual lines from point until reaching a text line.
Returns the target point, or nil if no text line found before buffer boundary."
  (save-excursion
    (let ((limit 1000))
      (while (and (> limit 0)
                  (not (if (> dir 0) (eobp) (bobp)))
                  (precision-scroll--line-non-text-p))
        (vertical-motion dir)
        (cl-decf limit))
      (when (not (precision-scroll--line-non-text-p))
        (point)))))

(defun precision-scroll-line (&optional arg)
  "Move ARG visual lines (default 1, negative = backward).
Pixel-scrolls tall non-text content and repositions at window boundaries."
  (interactive "p")
  (let ((dir (if (>= arg 0) 1 -1))
        (n (abs arg)))
    (dotimes (_ n)
      (unless (if (> dir 0) (eobp) (bobp))
        (cond
         ((or (precision-scroll--line-non-text-p)
              (precision-scroll--line-non-text-p dir))
          (ignore-errors
            (if (precision-scroll--line-non-text-p)
                (let ((target (precision-scroll--skip-to-text dir)))
                  (when target
                    (goto-char target)
                    (set-window-vscroll nil 0 t)))
              (if (> dir 0)
                  (pixel-scroll-precision-scroll-down (precision-scroll-nontext-height))
                (pixel-scroll-precision-scroll-up (precision-scroll-nontext-height))))))
         (t
          (line-move dir t)))))))

(defun precision-scroll-next-line ()
  "Scroll down one visual line."
  (interactive)
  (precision-scroll-line 1))

(defun precision-scroll-prev-line ()
  "Scroll up one visual line."
  (interactive)
  (precision-scroll-line -1))

(defun precision-scroll-up-page ()
  "Scroll viewport down by 1/3 page.
Keep point if still visible; otherwise move to top of window."
  (interactive)
  (let ((opoint (point)))
    (scroll-up (/ (window-height) 3))
    (if (pos-visible-in-window-p opoint)
        (goto-char opoint)
      (goto-char (window-start)))))

(defun precision-scroll-down-page ()
  "Scroll viewport up by 1/3 page.
Keep point if still visible; otherwise move to bottom of window."
  (interactive)
  (let ((opoint (point)))
    (scroll-down (/ (window-height) 3))
    (if (pos-visible-in-window-p opoint)
        (goto-char opoint)
      (goto-char (window-end)))))

;; icons
(use-package nerd-icons)

(use-package nerd-icons-dired
  :when window-system
  :hook (dired-mode . nerd-icons-dired-mode)
  :config
  (advice-add 'nerd-icons-dired--refresh
              :before-until
              (lambda ()
                (file-remote-p default-directory))))

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
             (modify-all-frames-parameters
              (list (cons 'alpha (cons val val))))
           (modify-all-frames-parameters
            (list (cons 'alpha-background val))))))

(defun set-transparency ()
  "Set the transparency of the frame window from 0=transparent to 100=opaque."
  (interactive)
  (when window-system
    (let ((value (read-number "change frame transparency: " frame-transparency)))
      (customize-save-variable 'frame-transparency value))))

;; themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t))

(use-package ef-themes)
(use-package spacemacs-theme
  :config
  (setq spacemacs-theme-comment-italic t
        spacemacs-theme-org-height nil
        spacemacs-theme-comment-bg nil))

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

;; disable fringe based on major-mode
(defun no-fringe-mode-p ()
  "Return non-nil if current buffer should hide fringes."
  (derived-mode-p 'xwidget-webkit-mode 'vterm-mode))

(defun sync-fringe-by-mode (frame)
  "Sync fringe width for all non-minibuffer windows in FRAME."
  (when (frame-live-p frame)
    (let ((window (frame-selected-window frame)))
      (unless (window-minibuffer-p window)
        (with-current-buffer (window-buffer window)
          (set-window-fringes window
                              (if (no-fringe-mode-p) 1 nil)
                              (if (no-fringe-mode-p) 1 nil)))))))

(add-hook 'window-state-change-functions #'sync-fringe-by-mode)

;; mode-line support
(use-package hide-mode-line)
(setq-default mode-line-percent-position nil
              mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

(defun mode-line-scroll-percent ()
  "Return scroll percentage as a number."
  (cond
   ((derived-mode-p 'xwidget-webkit-mode)
    (caret-xwidget-scroll-percent))
   ((derived-mode-p '(ghostel-mode vterm-mode eshell-mode))
    nil)
   (t
    (if (= (point-max) (point-min)) 0
      (min 100 (round (* 100.0 (/ (float (point)) (- (point-max) (point-min))))))))))

(defun mode-line-position ()
  "Display line number and position."
  (let ((percent (mode-line-scroll-percent)))
    (cond
     ((derived-mode-p 'xwidget-webkit-mode)
      (format " %d%%%% %s" percent (buffer-name)))
     ((derived-mode-p '(ghostel-mode vterm-mode eshell-mode))
      (format-mode-line " %b"))
     (t
      (format " %d%%%% %d:%s"
               percent (line-number-at-pos) (buffer-name))))))

(cl-defstruct (mode-line-cache (:constructor mode-line-cache-create))
  project-name
  flymake-counters
  persp-project)

(defvar-local mode-line-cache nil)
(defvar mode-line-cache-timer nil)
(defconst mode-line-cache-refresh-interval 1.5)

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
  (concat (if (bound-and-true-p flymake-mode)
              (format-mode-line (flymake--mode-line-counters))
            "")
          " "))

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
        (mode-line-cache-refresh)
        (force-mode-line-update)))))

(defun mode-line-cache-start-timer ()
  (when (timerp mode-line-cache-timer)
    (cancel-timer mode-line-cache-timer))
  (setq mode-line-cache-timer
        (run-at-time  mode-line-cache-refresh-interval
                      mode-line-cache-refresh-interval
                      #'mode-line-cache-refresh-current-buffer)))

(defun mode-line-persp-project ()
  (mode-line-cache-persp-project (mode-line-cache-get)))

(defun mode-line-flymake-counters ()
  (mode-line-cache-flymake-counters (mode-line-cache-get)))

(setq-default mode-line-format
              '((:eval (mode-line-position))
                (:eval (mode-line-flymake-counters))
                (:eval (mode-line-persp-project))))

(mode-line-cache-start-timer)

;; interactive theme commands
(defun set-theme--action (x)
  "Load theme X and run theme hooks once."
  (condition-case err
      (load-and-activate-theme (intern x))
    (error (user-error "%s" (error-message-string err)))))

(defun set-theme ()
  "Change current theme."
  (interactive)
  (ivy-read "Load custom theme: "
            (mapcar #'symbol-name (custom-available-themes))
            :action #'set-theme--action
            :preselect (symbol-name (or (when custom-enabled-themes
                                          (car custom-enabled-themes))
                                        default-theme))))

(defun face-attribute-hex (attr unspec-tag)
  "Return face ATTR of the default face as #rrggbb."
  (let ((color (face-attribute 'default attr nil t)))
    (when (or (null color) (equal color unspec-tag))
      (user-error "Default face %s is not specified" attr))
    (let ((rgb (color-values color)))
      (unless rgb
        (user-error "Unable to resolve color values for %s" color))
      (format "#%02x%02x%02x"
              (/ (nth 0 rgb) 257)
              (/ (nth 1 rgb) 257)
              (/ (nth 2 rgb) 257)))))

(defun current-theme-bg-hex ()
  "Return the current default face background color as #rrggbb."
  (face-attribute-hex :background "unspecified-bg"))

(defun current-theme-fg-hex ()
  "Return the current default face foreground color as #rrggbb."
  (face-attribute-hex :foreground "unspecified-fg"))

(defun reader-css-theme-colors ()
  "Return (FOREGROUND BACKGROUND) for the reader."
  (if (and (eq reader-css-theme 'light) (theme-dark-p))
      (list (current-theme-bg-hex) (current-theme-fg-hex))
    (list (current-theme-fg-hex) "transparent")))

(defun sync-reader-theme-colors ()
  "Rebuild reader assets to match the current reader theme."
  (interactive)
  (pcase-let* ((theme (reader-css-theme-resolved))
               (`(,foreground ,background) (reader-css-theme-colors))
               (script (expand-file-name "scripts/update_reader_theme_colors.py"
                                         user-emacs-directory))
               (output-buffer (get-buffer-create "*update-reader-theme-colors*"))
               (default-directory user-emacs-directory)
               (args (if (equal background "transparent")
                         (list script (symbol-name theme) foreground)
                       (list script "--background" background (symbol-name theme) foreground)))
               (exit-code (with-current-buffer output-buffer
                            (erase-buffer)
                            (apply #'call-process "python3" nil output-buffer nil args))))
    (if (zerop exit-code)
        (progn
          (message "Rebuilt reader assets for %s theme" theme)
          (when (buffer-live-p output-buffer)
            (kill-buffer output-buffer)))
      (display-buffer output-buffer)
      (error "Failed to rebuild reader assets for %s theme (exit %s)"
             theme exit-code))))

(defun cap-brightness-in-dark-theme ()
  "Cap foreground lightness for faces without explicit background in dark themes.
Preserves hue and saturation. Skips faces with their own background
\(highlight/selection faces that need white-on-dark contrast)."
  (when (theme-dark-p)
    (let ((max-lightness 0.85))
      (dolist (face (face-list))
        (let ((fg (face-attribute face :foreground nil t)))
          (when (stringp fg)
            (condition-case nil
                (pcase-let ((`(,h ,s ,l)
                             (apply #'color-rgb-to-hsl (color-name-to-rgb fg))))
                  (when (> l max-lightness)
                    (set-face-attribute
                     face nil :foreground
                     (apply #'color-rgb-to-hex
                            (append (color-hsl-to-rgb h s max-lightness) '(2))))))
              (error nil))))))))

(after-load-theme
 (cap-brightness-in-dark-theme)
 (sync-reader-theme-colors))

(provide 'init-ui)
