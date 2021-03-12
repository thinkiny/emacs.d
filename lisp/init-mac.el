;;(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)
(setq dired-use-ls-dired nil)

(setq-default locate-command "mdfind")

(defun new-emacs ()
  (interactive)
  (shell-command "open -n -a /Applications/Emacs.app"))

(global-set-key (kbd "C-c C-n") #'new-emacs)
(add-hook 'c-mode-common-hook (lambda ()
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

(setq ccls-initialization-options
      `(:clang ,(list :extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                                  "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                                  "-isystem/usr/local/include"]
                      :resourceDir (string-trim (shell-command-to-string "clang -print-resource-dir")))))


(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))
;; (load-theme 'vscode-dark-plus t)
;; (set-frame-transparency 96)

(provide 'init-mac)
