;;; init-tramp.el  -*- lexical-binding: t -*-

(use-package tramp
  :config
  (setenv "SHELL" "/bin/bash")
  (setq tramp-default-remote-shell "/bin/bash")
  (setq tramp-allow-unsafe-temporary-files t)
  (setq enable-remote-dir-locals t)
  (setq tramp-verbose 0)
  (setq vc-handled-backends '(Git))
  (setq tramp-default-method "ssh")
  ;;(setq tramp-chunksize 500)
  ;;(setq tramp-verbose 9)
  (setq remote-file-name-inhibit-cache 30)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))

  (setq debug-ignored-errors
        (cons 'remote-file-error debug-ignored-errors))

  (defun tramp-jssh-file-name-p (filename)
    "Check if it's a FILENAME for jssh."
    (and (tramp-tramp-file-p filename)
         (string= (tramp-file-name-method (tramp-dissect-file-name filename))
                  "jssh")))

  (tramp-register-foreign-file-name-handler #'tramp-jssh-file-name-p
                                            #'tramp-sh-file-name-handler)

  (defvar tramp-ssh-controlmaster-options)
  (setq tramp-ssh-controlmaster-options (concat
                                       "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                                       "-o ControlMaster=auto -o ControlPersist=yes"))

  (add-to-list 'tramp-methods
             `("jssh"
               (tramp-login-program      "jssh")
               (tramp-login-args         (("%u") ("%h")))
               (tramp-remote-shell       ,tramp-default-remote-shell)
               (tramp-remote-shell-login ("-l"))
               (tramp-remote-shell-args  ("-i" "-c")))))

(defconst tramp-ssh-without-controlmaster-options " -o ControlMaster=no -o ControlPath=none -o ControlPersist=no ")
(use-package docker-tramp :after tramp)

;; fix tramp master-control
(defun advice/ignore-tramp-ssh-control-master (func &rest args)
  (defvar tramp-ssh-controlmaster-options)
  (let ((tramp-ssh-controlmaster-options tramp-ssh-without-controlmaster-options))
    (apply func args)))

(defun ignore-tramp-ssh-control-master (&rest funcs)
  (dolist (func funcs)
    (advice-add func :around #'advice/ignore-tramp-ssh-control-master)))

(provide 'init-tramp)
