;;; init-tramp.el  -*- lexical-binding: t -*-

(require 'tramp)
(setq tramp-allow-unsafe-temporary-files t)
(setq enable-remote-dir-locals nil)
(setq remote-file-name-inhibit-locks t)
(setq remote-file-name-inhibit-auto-save-visited t)
(setq remote-file-name-inhibit-auto-save t)
;; (setq tramp-use-scp-direct-remote-copying t)
(setq tramp-verbose 0)
(setq tramp-chunksize nil)
(setq tramp-inline-compress-start-size (* 2 1024 1024))
(setq tramp-copy-size-limit (* 1024 1024))
(setq remote-file-name-inhibit-cache nil)

;; enable tramp-direct-async-process
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))

(setq debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))
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

;; tramp-hlo
(use-package tramp-hlo
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
            (if (custom-variable-p cache-symbol)
                (customize-save-variable cache-symbol cache-value)
              (set cache-symbol cache-value)))
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
        (let* ((result (apply orig-fn args))
               (new-cache (cons (cons key result) cache-value)))
          (if (custom-variable-p cache-symbol)
              (customize-save-variable cache-symbol new-cache)
            (set cache-symbol new-cache))
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
        (let* ((result (apply orig-fn args))
               (new-cache (cons (cons key result) cache-value)))
          (if (custom-variable-p cache-symbol)
              (customize-save-variable cache-symbol new-cache)
            (set cache-symbol new-cache))
            result))
    (apply orig-fn args)))

(require 'tramp-jumper)

;; tramp-rpc
(use-package tramp-rpc
  :vc (:url "https://github.com/ArthurHeymans/emacs-tramp-rpc.git"
       :rev :newest
       :lisp-dir "lisp")
  :config
  (setq tramp-rpc-deploy-git-build-policy 'release))

;; tramp rsync functions
(defun tramp-vec-to-rsync-address (vec)
  "Build rsync destination string from TRAMP vector VEC."
  (let ((user (tramp-file-name-user vec))
        (host (tramp-file-name-host vec))
        (remote-path (tramp-file-name-localname vec)))
    (if user
        (format "%s@%s:%s" user host remote-path)
      (format "%s:%s" host remote-path))))

(defun tramp-to-rsync-address (tramp-path)
  "Convert TRAMP-PATH to rsync address format.
Example: /ssh:user@host:/path/to/dir -> user@host:/path/to/dir"
  (if (tramp-tramp-file-p tramp-path)
        (let* ((parsed (tramp-dissect-file-name tramp-path))
               (method (tramp-file-name-method parsed)))
          (if (string= method "ssh")
              (tramp-vec-to-rsync-address parsed)
            tramp-path))
    tramp-path))

;; async save buffer via rsync for SSH-based TRAMP files
(defun save-buffer-async--rsync-save (file-name vec)
  "Save buffer asynchronously using rsync for SSH-based TRAMP files.
FILE-NAME is the remote file path, VEC is the parsed TRAMP vector."
  (let* ((buffer (current-buffer))
         (coding (or buffer-file-coding-system 'utf-8-unix))
         (temp-file (make-temp-file "emacs-rsync-"))
         (rsync-dest (tramp-vec-to-rsync-address vec))
         (mod-time (current-time))
         (tick (buffer-modified-tick)))
    (ignore-errors (run-hooks 'before-save-hook))
    (let ((coding-system-for-write coding))
      (write-region (point-min) (point-max) temp-file nil 'quiet))
    (let ((process (start-process "rsync-save" nil
                                  "rsync" "-a" "--inplace"
                                  temp-file rsync-dest)))
      (set-process-sentinel
       process
       (lambda (_ event)
         (unwind-protect
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (if (string-match-p "finished" event)
                     (progn
                       (let ((tramp-file-name-handler-alist nil))
                         (with-parsed-tramp-file-name file-name nil
                           (tramp-flush-file-properties v localname)
                           (tramp-flush-directory-properties v (file-name-directory localname))))
                       (set-visited-file-modtime mod-time)
                       (when (= tick (buffer-modified-tick))
                         (set-buffer-modified-p nil))
                       (ignore-errors (run-hooks 'after-save-hook))
                       (message "Wrote %s" file-name))
                   (message "Rsync save error: %s" (string-trim event)))))
           (ignore-errors (delete-file temp-file))))))))

(defun save-buffer-async-ssh ()
  "Save buffer asynchronously for SSH-based TRAMP files.
Use rsync for SSH-based TRAMP methods, regular 'save-buffer' for local files and non-SSH TRAMP methods."
  (interactive)
  (cond
   ((not (buffer-file-name)) nil)
   ((not (buffer-modified-p)) nil)
   ((file-remote-p (buffer-file-name))
    (let* ((file-name (buffer-file-name))
           (vec (tramp-dissect-file-name file-name))
           (method (tramp-file-name-method vec)))
      (if (member method '("ssh" "sshx" "scp" "rsync"))
          (save-buffer-async--rsync-save file-name vec)
        (save-buffer))))
   (t
    (save-buffer))))

(global-set-key (kbd "C-x C-s") 'save-buffer-async-ssh)

(provide 'init-tramp)
