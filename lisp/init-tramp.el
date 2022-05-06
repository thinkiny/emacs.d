;;; init-tramp.el  -*- lexical-binding: t -*-

(require 'tramp)
(setenv "SHELL" "/bin/bash")
(setq tramp-default-remote-shell "/bin/bash")
(setq tramp-allow-unsafe-temporary-files t)
(setq enable-remote-dir-locals t)
(setq tramp-verbose 0)
(setq vc-handled-backends '(Git))
;;(setq tramp-default-method "ssh")
;;(setq tramp-chunksize 500)
(setq remote-file-name-inhibit-cache nil)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

(with-eval-after-load 'tramp-sh
  (setq tramp-sh-file-name-handler-alist
        (cl-remove-if (lambda (x)
                        (member (car x) '(lock-file unlock-file file-locked-p)))
                      tramp-sh-file-name-handler-alist)))

(setq debug-ignored-errors
      (cons 'remote-file-error debug-ignored-errors))

(defvar tramp-ssh-controlmaster-options)
(setq tramp-ssh-controlmaster-options (concat
                                       "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                                       "-o ControlMaster=auto -o ControlPersist=yes"))

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

(require 'tramp-jumper)

(provide 'init-tramp)
