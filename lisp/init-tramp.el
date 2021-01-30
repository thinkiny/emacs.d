(use-package tramp
  :config
  (setenv "SHELL" "/bin/bash")
  (setq enable-remote-dir-locals t)
  (setq tramp-verbose 0)
  (setq vc-handled-backends '(Git))
  (setq tramp-default-method "ssh")
  ;;(setq tramp-chunksize 500)
  (setq remote-file-name-inhibit-cache 30)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(require 'tramp-sh)
;;same as ~/.ssh/config
(setq tramp-ssh-controlmaster-options (concat
                                       "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                                       "-o ControlMaster=auto -o ControlPersist=yes"))
(defconst tramp-ssh-without-controlmaster-options " -o ControlMaster=no -o ControlPath=none -o ControlPersist=no ")
;; fix tramp master-control
(defun tramp-ssh-control-master-none (func &rest args)
  (let ((origin-options tramp-ssh-controlmaster-options))
    (setq tramp-ssh-controlmaster-options tramp-ssh-without-controlmaster-options)
    (let ((res (apply func args)))
      (setq tramp-ssh-controlmaster-options origin-options)
      res)))

(defun ignore-tramp-ssh-control-master (&rest funcs)
  (dolist (func funcs)
    (advice-add func :around #'tramp-ssh-control-master-none)))

(use-package docker-tramp :after tramp)

(provide 'init-tramp)
