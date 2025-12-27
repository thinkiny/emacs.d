;; -*- lexical-binding: t; -*-

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq dired-use-ls-dired nil)

(setq-default locate-command "mdfind")
(setq ns-use-thin-smoothing t)

;; start new emacs
(defun start-new-emacs ()
  (interactive)
  (let ((default-directory temporary-file-directory))
    (shell-command "open -n -a /Applications/Emacs.app")))

(global-set-key (kbd "C-c C-n") #'start-new-emacs)
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-unset-key (kbd "C-c C-n"))))

;; ;; Visit files opened outside of Emacs in existing frame, not a new one
(setq ns-pop-up-frames nil)

;; sane trackpad/mouse scroll settings
(setq mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil)

;;locale
(require 'init-utf8)

(if window-system
    (menu-bar-mode t))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(after-load-theme
 (if (theme-dark-p)
     (add-to-list 'default-frame-alist '(ns-appearance . dark))
   (add-to-list 'default-frame-alist '(ns-appearance . light))))

(setq ns-use-proxy-icon nil)
;;(setq frame-transparency 96)

(with-eval-after-load 'org2ctex
  (setq org2ctex-latex-fonts
        (mapcar (lambda (x)
                  (append (list (car x) "PingFang SC") (cdr x)))
                org2ctex-latex-fonts)))

;; dwim-shell-command
(with-eval-after-load 'dwim-shell-command
  (require 'dwim-shell-commands)
  (defun dwim-shell-commands-toggle-menu-bar-macos()
    "Toggle macOS menu bar."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Toggle menu bar auto-hide."
     "current_status=$(osascript -e 'tell application \"System Events\" to get autohide menu bar of dock preferences')

if [ \"$current_status\" = \"true\" ]; then
    osascript -e 'tell application \"System Events\" to set autohide menu bar of dock preferences to false'
    echo \"Auto-hide menu disabled.\"
else
    osascript -e 'tell application \"System Events\" to set autohide menu bar of dock preferences to true'
    echo \"Auto-hide menu enabled.\"
fi"
     :utils "osascript"
     :silent-success t))

  (defun dwim-shell-commands-show-dock-macos (status on-completion)
    "Control macOS dock shown."
    (let ((cmd (if status "false" "true" ))
          (title (if status "Show" "Hide")))
      (dwim-shell-command-on-marked-files
       (concat title " dock.")
       (format "osascript -e 'tell application \"System Events\" to set autohide of dock preferences to %s'" cmd)
       :utils "osascript"
       :silent-success t
       :on-completion on-completion))))

;; maximize window
(defun get-dock-autohide-macos()
  (string-equal "true\n" (shell-command-to-string "osascript -e 'tell application \"System Events\" to get autohide of dock preferences'")))

(defconst dock-autohide-macos-at-start (get-dock-autohide-macos))
(defun restore-dock-autohide-macos()
  (interactive)
  (unless (eq dock-autohide-macos-at-start (get-dock-autohide-macos))
    (dwim-shell-commands-show-dock-macos (not dock-autohide-macos-at-start) nil)
    (sit-for 0.5)))

(add-hook 'kill-emacs-hook #'restore-dock-autohide-macos)

(defun toggle-frame-maximized-macos-callback(buffer _)
  (kill-buffer buffer)
  (run-with-timer 0.1 0 #'toggle-frame-maximized))

(defun is-on-mac-screen()
  (string= (frame-monitor-attribute 'name) "Built-in Retina Display"))

(defun toggle-frame-maximized-macos()
  (let ((frame-status (get-current-frame-maximized))
        (dock-status (get-dock-autohide-macos)))
    (if frame-status
        (if dock-status
            ;; show
            (dwim-shell-commands-show-dock-macos t #'toggle-frame-maximized-macos-callback)
          (toggle-frame-maximized))
      (if dock-status
          (toggle-frame-maximized)
        (if (is-on-mac-screen)
            ;; show
            (dwim-shell-commands-show-dock-macos nil #'toggle-frame-maximized-macos-callback)
          (toggle-frame-maximized))))))

;; italic font
(defun set-italic-font-if-available(&rest args)
  (when-let* ((font (font-face-attributes (face-attribute 'italic :font)))
              (family (plist-get font :family))
              (italic-famlily (concat family " Italic")))

    (if (member italic-famlily (font-family-list))
        (set-face-attribute 'italic nil :family italic-famlily))))

(if window-system
    (advice-add #'cnfonts-set-font :after #'set-italic-font-if-available))

(provide 'init-mac)
