(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

(setq-default locate-command "mdfind")

(defun new-emacs ()
  (interactive)
  (let ((default-directory temporary-file-directory))
    (shell-command "open -n -a /Applications/Emacs.app")))

(global-set-key (kbd "C-c C-n") #'new-emacs)
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-unset-key (kbd "C-c C-n"))))

;; Curse Lion and its sudden but inevitable fullscreen mode!
;; NOTE Meaningless to railwaycat's emacs-mac build
(setq ns-use-native-fullscreen nil)

;; Visit files opened outside of Emacs in existing frame, not a new one
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

(provide 'init-mac)
