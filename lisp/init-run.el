(defun load-run-conf-from-file (file)
  (if (file-exists-p file)
      (deserialize-from-file file)
    (make-hash-table :test 'equal)))

(defmacro register-run-template (mode get-args execute-args)
  "Register run template."
  (let* ((prefix (string-trim-right (symbol-name mode) "-mode"))
         (conf-file (expand-file-name (concat "~/.emacs.d/" prefix "-run.el")))
         (table-name (intern (concat prefix "-run-table")))
         (reset-name (intern (concat prefix "-reset-run-conf")))
         (get-conf-name (intern (concat prefix "-get-run-conf")))
         (run-name (intern (concat prefix "-run")))
         (hook-name (intern (concat (symbol-name mode) "-hook")))
         (map-name (intern (concat (symbol-name mode) "-map"))))
    `(progn
       (defvar ,table-name)
       (defvar ,map-name)
       (setq ,table-name (load-run-conf-from-file ,conf-file))
       (if ,get-args
           (defun ,reset-name()
             (interactive)
             (let ((conf (funcall ,get-args)))
               (puthash (buffer-file-name) conf ,table-name)
               (serialize-to-file ,conf-file ,table-name)
               conf))

         (defun ,reset-name()
           (buffer-file-name)))

       (defun ,get-conf-name()
         (interactive)
         (let ((conf (gethash (buffer-file-name) ,table-name)))
           (if conf
               conf
             (,reset-name))))

       (defun ,run-name()
         (interactive)
         (funcall ,execute-args (,get-conf-name)))

       (add-hook (quote ,hook-name) (lambda ()
                                      (define-key ,map-name (kbd "C-c <RET>") (quote ,run-name)))))))


(defun run-output-split-window ()
  (if (>= (window-width) 160)
      (split-window-horizontally)
    (split-window-vertically)))

(defun refresh-run-output-buffer (buffer-name)
  "Kill buffer and process associated with BUFFER-NAME."
  (when-let ((buffer (get-buffer buffer-name)))
    (if-let ((process (get-buffer-process buffer)))
        (delete-process process))
    (kill-buffer buffer))
  (generate-new-buffer buffer-name))

(defun run-command-with-output(name dir command)
  "Run command with output."
  (interactive)
  (let* ((output-buffer (refresh-run-output-buffer name)))
    (save-excursion
      (delete-other-windows)
      (let ((target-window (run-output-split-window))
            (default-directory dir))
        (set-process-filter (start-file-process name
                                                output-buffer
                                                "/bin/sh"
                                                "-c"
                                                command)
                            #'process-insertion-filter)
        ;;(select-window target-window)
        (set-window-buffer target-window output-buffer)))))

;; c++
(defun cpp-dir-has-makefile (dir)
  (seq-find (lambda (x) (file-exists-p (format "%s/%s" dir x)))
            '("Makefile" "makefile" "MakeFile")))

(defun cpp-dir-has-workspace (dir)
  (and dir (file-exists-p (format "%s/WORKSPACE" dir))))

(defun cpp-get-execute-command (bin-dir program)
  "Get run command based on PROGRAM."
  (let ((proj-root (projectile-project-root)))
    (cond ((cpp-dir-has-makefile bin-dir) (concat "make && " (file-local-name program)))
          ((cpp-dir-has-workspace proj-root) (format "(cd %s && bazel build ... ) && %s" (file-local-name proj-root) (file-local-name program)))
        (t program))))

(register-run-template c++-mode
                       (lambda () (read-file-name "Select program: "))
                       (lambda (program)
                         (let* ((bin-dir (file-name-directory program))
                                (command (cpp-get-execute-command bin-dir program)))
                           (run-command-with-output (format "*CppRun-%s*" program) bin-dir command))))

;;scala
(register-run-template scala-mode
                       nil
                       (lambda (file)
                         (let* ((dir (file-name-directory file))
                                (file-name (file-name-nondirectory file))
                                (command (concat "scala-cli " file-name)))
                           (run-command-with-output (format "*ScalaRun-%s*" file-name) dir command))))


(register-run-template go-mode
                       nil
                       (lambda (file)
                         (let* ((dir (or (lsp-workspace-root file) (file-name-directory file)))
                                (file-name (string-remove-prefix (concat dir "/") file))
                                (command (concat "go run " file-name)))
                           (run-command-with-output (format "*GoRun-%s*" file-name) dir command))))

;;java
(defun java-get-run-args-template ()
  "Java run args template."
  (list :type "java"
        :args ""
        :noDebug t
        :cwd nil
        :host "localhost"
        :request "launch"
        :modulePaths []
        :classPaths nil
        :name (concat "JavaRun-" (buffer-name))
        :projectName nil
        :mainClass nil))

(defun java-get-run-args ()
  "Return java run args."
  (let* ((debug-args (dap-variables-expand-in-launch-configuration (java-get-run-args-template))))
    (-some-> (plist-get debug-args :type)
      (gethash dap--debug-providers)
      (funcall debug-args))))

(register-run-template java-mode
                       #'java-get-run-args
                       (lambda (conf)
                         (dap-delete-all-sessions)
                         (dap-debug conf)))
(provide 'init-run)
