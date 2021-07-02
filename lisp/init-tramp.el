(use-package tramp
  :config
  (setenv "SHELL" "/bin/bash")
  (setq tramp-allow-unsafe-temporary-files t)
  (setq enable-remote-dir-locals t)
  (setq tramp-verbose 0)
  (setq vc-handled-backends '(Git))
  (setq tramp-default-method "ssh")
  ;;(setq tramp-chunksize 500)
  (setq remote-file-name-inhibit-cache 30)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))
  (setq debug-ignored-errors
      (cons 'remote-file-error debug-ignored-errors))

  (defvar tramp-ssh-controlmaster-options)
  (setq tramp-ssh-controlmaster-options (concat
                                       "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                                       "-o ControlMaster=auto -o ControlPersist=yes")))
(defconst tramp-ssh-without-controlmaster-options " -o ControlMaster=no -o ControlPath=none -o ControlPersist=no ")
(use-package docker-tramp :after tramp)

(provide 'init-tramp)
