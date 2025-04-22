;; -*- lexical-binding: t; -*-

(defconst tramp-jumper-method "jssh")
(defconst tramp-jumper-exec "jssh")

(defun tramp-jumper-handle-make-process-async (&rest args)
  "An alternative `make-process' implementation for Tramp files."
  (when args
    (with-parsed-tramp-file-name (expand-file-name default-directory) nil
      (let ((default-directory (tramp-compat-temporary-file-directory))
            (name (plist-get args :name))
            (buffer (plist-get args :buffer))
            (command (plist-get args :command))
            (coding (plist-get args :coding))
            (noquery (plist-get args :noquery))
            (connection-type (plist-get args :connection-type))
            (filter (plist-get args :filter))
            (sentinel (plist-get args :sentinel))
            (stderr (plist-get args :stderr)))
        (unless (stringp name)
          (signal 'wrong-type-argument (list #'stringp name)))
        (unless (or (null buffer) (bufferp buffer) (stringp buffer))
          (signal 'wrong-type-argument (list #'stringp buffer)))
        (unless (consp command)
          (signal 'wrong-type-argument (list #'consp command)))
        (unless (or (null coding)
                    (and (symbolp coding) (memq coding coding-system-list))
                    (and (consp coding)
                         (memq (car coding) coding-system-list)
                         (memq (cdr coding) coding-system-list)))
          (signal 'wrong-type-argument (list #'symbolp coding)))
        (unless (or (null connection-type) (memq connection-type '(pipe pty)))
          (signal 'wrong-type-argument (list #'symbolp connection-type)))
        (unless (or (null filter) (functionp filter))
          (signal 'wrong-type-argument (list #'functionp filter)))
        (unless (or (null sentinel) (functionp sentinel))
          (signal 'wrong-type-argument (list #'functionp sentinel)))
        (unless (or (null stderr) (bufferp stderr))
          (signal 'wrong-type-argument (list #'bufferp stderr)))

        (let* ((buffer
                (if buffer
                    (get-buffer-create buffer)
                  ;; BUFFER can be nil.  We use a temporary buffer.
                  (generate-new-buffer tramp-temp-buffer-name)))
               ;; We use as environment the difference to toplevel
               ;; `process-environment'.
               (env (mapcar
                     (lambda (elt)
                       (when (string-match-p "=" elt) elt))
                     tramp-remote-process-environment))
               ;; We use as environment the difference to toplevel
               ;; `process-environment'.
               (env (dolist (elt process-environment env)
                      (when
                          (and
                           (string-match-p "=" elt)
                           (not
                            (member
                             elt (default-toplevel-value 'process-environment))))
                        (setq env (cons elt env)))))
               (env (setenv-internal
                     env "INSIDE_EMACS" (tramp-inside-emacs) 'keep))
               (env (mapcar #'tramp-shell-quote-argument (delq nil env)))
               ;; Quote command.
               (command (mapconcat #'tramp-shell-quote-argument command " "))
               ;; Set cwd and environment variables.
               (command
                (append `("cd" ,localname "&&" "stty raw -echo" ";env") env `(,command))))

          (let* ((login-program (tramp-get-method-parameter v 'tramp-login-program))
                 login-args p)
            (setq login-args
                  (append
                   (tramp-compat-flatten-tree
                    (mapcar
                     (lambda (x) (split-string x " "))
                     (tramp-expand-args v 'tramp-login-args ?u (or user "") ?h (or host "")))))
                  p (make-process
                     :name name :buffer buffer
                     :command (append `(,login-program) login-args command)
                     :coding coding :noquery noquery :connection-type connection-type
                     :filter filter :sentinel sentinel :stderr stderr))
            (tramp-message v 6 "%s" (string-join (process-command p) " "))
            (call-process-shell-command "sleep 5")
            p))))))

(defun tramp-jumper-handle-make-process (&rest args)
  (if (tramp-direct-async-process-p args)
      (apply #'tramp-jumper-handle-make-process-async args)
    (apply #'tramp-sh-handle-make-process args)))

(defconst tramp-jumper-file-name-handler-alist
  '((access-file . tramp-handle-access-file)
    (add-name-to-file . tramp-sh-handle-add-name-to-file)
    ;; `byte-compiler-base-file-name' performed by default handler.
    (copy-directory . tramp-sh-handle-copy-directory)
    (copy-file . tramp-sh-handle-copy-file)
    (delete-directory . tramp-sh-handle-delete-directory)
    (delete-file . tramp-sh-handle-delete-file)
    ;; `diff-latest-backup-file' performed by default handler.
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . tramp-handle-directory-files)
    (directory-files-and-attributes
     . tramp-sh-handle-directory-files-and-attributes)
    (dired-compress-file . tramp-sh-handle-dired-compress-file)
    (dired-uncache . tramp-handle-dired-uncache)
    (exec-path . tramp-sh-handle-exec-path)
    (expand-file-name . tramp-sh-handle-expand-file-name)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . tramp-sh-handle-file-acl)
    (file-attributes . tramp-sh-handle-file-attributes)
    (file-directory-p . tramp-sh-handle-file-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-executable-p . tramp-sh-handle-file-executable-p)
    (file-exists-p . tramp-sh-handle-file-exists-p)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . tramp-sh-handle-file-local-copy)
    (file-locked-p . tramp-handle-file-locked-p)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . tramp-sh-handle-file-name-all-completions)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p)
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    ;; `file-name-sans-versions' performed by default handler.
    (file-newer-than-file-p . tramp-handle-file-newer-than-file-p)
    (file-notify-add-watch . tramp-sh-handle-file-notify-add-watch)
    (file-notify-rm-watch . tramp-handle-file-notify-rm-watch)
    (file-notify-valid-p . tramp-handle-file-notify-valid-p)
    (file-ownership-preserved-p . tramp-sh-handle-file-ownership-preserved-p)
    (file-readable-p . tramp-sh-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . tramp-sh-handle-file-selinux-context)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-system-info . tramp-sh-handle-file-system-info)
    (file-truename . tramp-sh-handle-file-truename)
    (file-writable-p . tramp-sh-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-sh-handle-insert-directory)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (load . tramp-handle-load)
    (lock-file . tramp-handle-lock-file)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . tramp-sh-handle-make-directory)
    ;; `make-directory-internal' performed by default handler.
    (make-lock-file-name . tramp-handle-make-lock-file-name)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-process . tramp-jumper-handle-make-process)
    (make-symbolic-link . tramp-sh-handle-make-symbolic-link)
    (process-file . tramp-sh-handle-process-file)
    (rename-file . tramp-sh-handle-rename-file)
    (set-file-acl . tramp-sh-handle-set-file-acl)
    (set-file-modes . tramp-sh-handle-set-file-modes)
    (set-file-selinux-context . tramp-sh-handle-set-file-selinux-context)
    (set-file-times . tramp-sh-handle-set-file-times)
    (set-visited-file-modtime . tramp-sh-handle-set-visited-file-modtime)
    (shell-command . tramp-handle-shell-command)
    (start-file-process . tramp-handle-start-file-process)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-handle-temporary-file-directory)
    (tramp-get-remote-gid . tramp-sh-handle-get-remote-gid)
    (tramp-get-remote-uid . tramp-sh-handle-get-remote-uid)
    (tramp-set-file-uid-gid . tramp-sh-handle-set-file-uid-gid)
    (unhandled-file-name-directory . ignore)
    (unlock-file . tramp-handle-unlock-file)
    (vc-registered . tramp-sh-handle-vc-registered)
    (verify-visited-file-modtime . tramp-sh-handle-verify-visited-file-modtime)
    (write-region . tramp-sh-handle-write-region))
  "Alist of handler functions.
Operations not mentioned here will be handled by the normal Emacs functions.")

(defun tramp-jumper-file-name-p (filename)
  "Check if it's a FILENAME for jssh."
  (and (tramp-tramp-file-p filename)
       (string= (tramp-file-name-method (tramp-dissect-file-name filename))
                tramp-jumper-method)))

(defun tramp-jumper-file-name-handler (operation &rest args)
  "Invoke remote-shell Tramp file name handler.
Fall back to normal file name handler if no Tramp handler exists."
  (if-let* ((fn (assoc operation tramp-jumper-file-name-handler-alist)))
      (save-match-data (apply (cdr fn) args))
    (tramp-run-real-handler operation args)))

(tramp-register-foreign-file-name-handler #'tramp-jumper-file-name-p
                                          #'tramp-jumper-file-name-handler)

(add-to-list 'tramp-methods
             `(,tramp-jumper-method
               (tramp-login-program      ,tramp-jumper-exec)
               (tramp-login-args         (("%u") ("%h")))
               (tramp-direct-async t)
               (tramp-remote-shell       ,tramp-default-remote-shell)
               (tramp-remote-shell-login ("-l"))
               (tramp-remote-shell-args  ("-i" "-c"))))

(provide 'tramp-jumper)
