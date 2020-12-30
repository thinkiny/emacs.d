;;(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)
(setq dired-use-ls-dired nil)

(setq-default locate-command "mdfind")

(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

(defun new-emacs ()
  (interactive)
  (shell-command "open -n -a /Applications/Emacs.app"))

(global-set-key (kbd "C-c C-n") #'new-emacs)
(add-hook 'c-mode-common-hook (lambda ()
                                (local-unset-key (kbd "C-c C-n"))))
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

(setq delete-by-moving-to-trash t)

(provide 'init-mac)
