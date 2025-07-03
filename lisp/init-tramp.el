;;; init-tramp.el  -*- lexical-binding: t -*-

(require 'tramp)
(setq tramp-allow-unsafe-temporary-files t)
(setq enable-remote-dir-locals t)
(setq remote-file-name-inhibit-locks t)
(setq remote-file-name-inhibit-auto-save-visited t)
(setq remote-file-name-inhibit-auto-save t)
(setq tramp-use-scp-direct-remote-copying t)
(setq tramp-default-method "ssh")
(setq tramp-verbose 0)
(setq tramp-copy-size-limit (* 2 1024 1024))
;; (setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp))
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
(setq remote-file-name-inhibit-cache 600)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; (with-eval-after-load 'tramp-sh
;;   (setq tramp-sh-file-name-handler-alist
;;         (cl-remove-if (lambda (x)
;;                         (member (car x) '(lock-file unlock-file file-locked-p)))
;;                       tramp-sh-file-name-handler-alist)))

(setq debug-ignored-errors
      (cons 'remote-file-error debug-ignored-errors))

(defvar tramp-ssh-controlmaster-options)
(setq tramp-ssh-controlmaster-options (concat
                                       "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                                       "-o ControlMaster=auto -o ControlPersist=yes"))

(defconst tramp-ssh-without-controlmaster-options " -o ControlMaster=no -o ControlPath=none -o ControlPersist=no ")

;; fix tramp master-control
(defun advice/ignore-tramp-ssh-control-master (func &rest args)
  (defvar tramp-ssh-controlmaster-options)
  (let ((tramp-ssh-controlmaster-options tramp-ssh-without-controlmaster-options))
    (apply func args)))

(defun ignore-tramp-ssh-control-master (&rest funcs)
  (dolist (func funcs)
    (advice-add func :around #'advice/ignore-tramp-ssh-control-master)))

(with-eval-after-load 'tramp-sh
  (setq tramp-default-remote-shell "/bin/bash")
  (add-to-list 'tramp-methods
               `("ssh"
                 (tramp-login-program        "ssh")
                 (tramp-login-args           (("-l" "%u") ("-p" "%p") ("%c")
                                              ("-e" "none") ("%h") ))
                 (tramp-async-args           (("-q")))
                 (tramp-direct-async         ("-t" "-t"))
                 (tramp-remote-shell         ,tramp-default-remote-shell)
                 (tramp-remote-shell-login   ("-l"))
                 (tramp-remote-shell-args    ("-c"))
                 (tramp-copy-program         "scp")
                 (tramp-copy-args            (("-P" "%p") ("-p" "%k")
                                              ("%x") ("%y") ("%z")
                                              ("-q") ("-r") ("%c")))
                 (tramp-copy-keep-date       t)
                 (tramp-copy-recursive       t))))

(require 'tramp-jumper)

(provide 'init-tramp)
