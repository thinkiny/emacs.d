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
;; fix tramp master-control
(defun tramp-ssh-control-master-none (func &rest args)
  (let ((origin-options tramp-ssh-controlmaster-options))
    (setq tramp-ssh-controlmaster-options "-o ControlMaster=no -o ControlPath=none -o ControlPersist=no")
    (let ((res (apply func args)))
      (setq tramp-ssh-controlmaster-options origin-options)
      res)))

(after-load 'helm
  (defun helm-follow-execute-persistent-action-maybe (&optional delay)
    "Execute persistent action in mode `helm-follow-mode'.

This happen after: DELAY or the 'follow-attr value of current
source or `helm-follow-input-idle-delay' or
`helm-input-idle-delay' secs."
    (let* ((src (helm-get-current-source))
           (curr-sel (helm-get-selection nil nil src)))
      (when (and (not (get-buffer-window helm-action-buffer 'visible))
                 (not (helm-pos-header-line-p))
                 (or (helm-follow-mode-p src)
                     (and helm-follow-mode-persistent
                          (member (assoc-default 'name src)
                                  helm-source-names-using-follow)))
                 (null (eq (assoc-default 'follow src) 'never))
                 curr-sel)
        (helm-follow-mode-set-source 1 src)
        (run-with-idle-timer helm-follow-input-idle-delay nil (lambda (curr-sel)
                                      (when (and helm-alive-p (equal curr-sel (helm-get-selection)))
                                        (helm-execute-persistent-action)))
                             curr-sel)))))

(dolist (func '(lsp lsp-restart-workspace helm-execute-persistent-action helm-projectile-ag helm-ag--persistent-action))
  (advice-add func :around #'tramp-ssh-control-master-none))

(use-package docker-tramp :after tramp)

(provide 'init-tramp)
