;;; init-tramp.el  -*- lexical-binding: t -*-

(require 'tramp)
(setq tramp-allow-unsafe-temporary-files t)
(setq enable-remote-dir-locals nil)
(setq remote-file-name-inhibit-locks t)
(setq remote-file-name-inhibit-auto-save-visited t)
(setq remote-file-name-inhibit-auto-save t)
(setq tramp-use-scp-direct-remote-copying t)
(setq tramp-default-method "ssh")
(setq tramp-verbose 0)
(setq tramp-chunksize 2048)
(setq tramp-copy-size-limit (* 2 1024 1024))
(setq remote-file-name-inhibit-cache 600)

;; enable tramp-direct-async-process
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

;; (setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp))
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

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

;; (with-eval-after-load 'tramp-sh
;;   (setq tramp-default-remote-shell "/bin/bash")
;;   (add-to-list 'tramp-methods
;;                `("ssh"
;;                  (tramp-login-program        "ssh")
;;                  (tramp-login-args           (("-l" "%u") ("-p" "%p") ("%c")
;;                                               ("-e" "none") ("%h") ))
;;                  (tramp-async-args           (("-q")))
;;                  (tramp-direct-async         ("-t" "-t"))
;;                  (tramp-remote-shell         ,tramp-default-remote-shell)
;;                  (tramp-remote-shell-login   ("-l"))
;;                  (tramp-remote-shell-args    ("-c"))
;;                  (tramp-copy-program         "scp")
;;                  (tramp-copy-args            (("-P" "%p") ("-p" "%k")
;;                                               ("%x") ("%y") ("%z")
;;                                               ("-q") ("-r") ("%c")))
;;                  (tramp-copy-keep-date       t)
;;                  (tramp-copy-recursive       t))))

(require 'tramp-jumper)


;; tramp-hlo
(use-package tramp-hlo
  :ensure t
  :demand t
  :config
  (tramp-hlo-setup))

;; tramp caches
(defgroup tramp-caches ()
  "Caching system for remote file operations."
  :group 'tramp)

(defun cache-tramp-from-matching(key cache-symbol orig-fn &rest args)
  "Cache a value if the key is a remote path.
KEY is the cache key (usually a file path).
CACHE-SYMBOL is the symbol holding the cache alist.
ORIG-FN is the original function to call if cache miss.
ARGS are the arguments to pass to ORIG-FN."
  (if (and key (file-remote-p default-directory))
      (if-let* ((cache-value (symbol-value cache-symbol))
                (cached (find-longest-matching key cache-value)))
          cached
        (let ((result (apply orig-fn args)))
          (when result
            (push result cache-value)
            (customize-save-variable cache-symbol cache-value))
          result))
    (apply orig-fn args)))

(defun cache-tramp-from-matching-value(key cache-symbol orig-fn &rest args)
  "Cache a value if the key is a remote path.
KEY is the cache key (usually a file path).
CACHE-SYMBOL is the symbol holding the cache alist.
ORIG-FN is the original function to call if cache miss.
ARGS are the arguments to pass to ORIG-FN."
  (if (and key (file-remote-p default-directory))
      (if-let* ((cache-value (symbol-value cache-symbol))
                (cached-result (find-longest-matching-value key cache-value)))
          cached-result
        (let ((result (apply orig-fn args)))
          ;; Update the customizable variable
          (customize-save-variable cache-symbol
                                   (cons (cons key result) cache-value))
            result))
    (apply orig-fn args)))

(defun cache-tramp-from-kv(key cache-symbol orig-fn &rest args)
  "Cache a value if the key is a remote path.
KEY is the cache key (usually a file path).
CACHE-SYMBOL is the symbol holding the cache alist.
ORIG-FN is the original function to call if cache miss.
ARGS are the arguments to pass to ORIG-FN."
  (if (and key (file-remote-p default-directory))
      (if-let* ((cache-value (symbol-value cache-symbol))
                (cached-entry (assoc key cache-value)))
          (cdr cached-entry)
        (let ((result (apply orig-fn args)))
          ;; Update the customizable variable
          (customize-save-variable cache-symbol
                                   (cons (cons key result) cache-value))
            result))
    (apply orig-fn args)))

(provide 'init-tramp)
