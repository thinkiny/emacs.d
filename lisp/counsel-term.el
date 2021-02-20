;;; counsel-term.el --- counsel terminal management -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)

(defconst counsel-mt-buffer-header "term-")
(defun counsel-mt/terminal-buffers ()
  "Filter for buffers that are terminals only."
  (cl-loop for buf in (buffer-list)
           if (member (buffer-local-value 'major-mode buf) '(eshell-mode term-mode))
           collect (cons (string-trim (buffer-name buf) counsel-mt-buffer-header)
                         buf)))

(defun counsel-mt/get-buffer-process-cwd ()
  "The current working directory of the process of buffer BUF, or nil (depends on lsof)."
  (let* ((process (get-buffer-process (current-buffer)))
         (pid (process-id process))
         (command (format "lsof -Fn -a -dcwd -p%d" pid))
         (stdout (shell-command-to-string command)))
    (string-match "^n\\(.*\\)" stdout)
    (match-string 1 stdout))
  (nil))

(defun counsel-mt/get-terminal-name()
  (or (projectile-project-name)
      (counsel-mt/get-buffer-process-cwd)
      default-directory))

(defun counsel-get-term-cmd()
  (if (and (featurep 'tramp)
             (tramp-tramp-file-p default-directory))
    (with-parsed-tramp-file-name default-directory path
      (let ((method (cadr (assoc `tramp-login-program (assoc path-method tramp-methods)))))
        (list method "-t"
              (if path-port
                  (concat (when path-user (concat path-user "@")) path-host " -p " path-port)
                (concat (when path-user (concat path-user "@")) path-host))
              (concat "cd '" path-localname "'; exec $SHELL -l"))))
    (list (or (getenv "SHELL")
              (getenv "ESHELL")
              "/bin/sh") "-l")))

(defun counsel-term-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc)))))))

(defun counsel-open-term (name)
  (let ((cmd (counsel-get-term-cmd)))
    (setq term-ansi-buffer-name (apply 'make-term name (car cmd) nil (cdr cmd)))
    (set-buffer term-ansi-buffer-name)
    (term-mode)
    (term-char-mode)
    (counsel-term-handle-close)
    (switch-to-buffer term-ansi-buffer-name)))

(defun counsel-mt/launch-terminal ()
  "Launch a terminal in a new buffer."
  (counsel-open-term (generate-new-buffer-name
                      (format "%s%s"
                              counsel-mt-buffer-header
                              (counsel-mt/get-terminal-name)))))

(defun counsel-mt/source-terminals ()
  "Counsel source with candidates for all terminal buffers."
  (cons '("Launch new terminal") (counsel-mt/terminal-buffers)))

(defun counsel-term ()
  (interactive)
  (ivy-read "Terminal: " (counsel-mt/source-terminals)
            :require-match t
            :preselect (counsel-mt/get-terminal-name)
            :action (lambda (candidate)
                      (let ((buf (cdr candidate)))
                        (if buf
                            (switch-to-buffer buf)
                          (counsel-mt/launch-terminal))))
            :caller 'counsel-term))

(provide 'counsel-term)
;;; counsel-term.el ends here
