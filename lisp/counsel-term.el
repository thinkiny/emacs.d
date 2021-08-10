;;; counsel-term.el --- counsel terminal management -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 's)

(defvar counsel-mt-shell-type 'vterm)
(defconst counsel-mt-buffer-header "*term: ")
(defun counsel-mt-get-terminal-buffers ()
  "Filter for buffers that are terminals only."
  (cl-loop for buf in (buffer-list)
           if (member (buffer-local-value 'major-mode buf) '(eshell-mode term-mode vterm-mode))
           collect (cons (string-trim (buffer-name buf) counsel-mt-buffer-header)
                         buf)))

(defun counsel-mt-get-buffer-process-cwd ()
  "The current working directory of the process of buffer BUF, or nil (depends on lsof)."
  (let ((process (get-buffer-process (current-buffer))))
    (if process
        (let* ((pid (process-id process))
               (command (format "lsof -Fn -a -dcwd -p%d" pid))
               (stdout (shell-command-to-string command)))
          (string-match "^n\\(.*\\)" stdout)
          (match-string 1 stdout))
      nil)))

(defun counsel-term-get-term-cmd()
  (if (tramp-tramp-file-p default-directory)
      (with-parsed-tramp-file-name default-directory term
        (pcase term-method
          ("ssh"
           (flatten-list
            (list "ssh"
                  (split-string tramp-ssh-without-controlmaster-options)
                  "-t"
                  (concat (when term-user (concat term-user "@")) term-host)
                  (if term-port
                      (list "-p" term-port))
                  (concat "\"cd '" term-localname "' && $SHELL -l\""))))
          (tramp-jumper-method
           (list tramp-jumper-exec term-user term-host
                 (concat "cd '" term-localname "'")))))
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
  (let* ((cmd (counsel-term-get-term-cmd))
         (buf (apply 'make-term name (car cmd) nil (cdr cmd))))
    (set-buffer buf)
    (term-mode)
    (term-char-mode)
    (counsel-term-handle-close)
    (switch-to-buffer buf)
    (rename-buffer name)))

(defun counsel-open-eshell (name)
  (call-interactively 'eshell)
  (rename-buffer name))

(defun counsel-open-vterm (name)
  (defvar vterm-shell)
  (let ((vterm-shell (s-join " " (counsel-term-get-term-cmd))))
    (switch-to-buffer (vterm name))
    (if (tramp-tramp-file-p default-directory)
        (with-parsed-tramp-file-name default-directory term
          (setq-local tramp-default-method term-method)))
    (rename-buffer name)))

(defun counsel-mt-get-terminal-name()
  (let ((project-root (projectile-project-root)))
    (if project-root
        (file-name-nondirectory (directory-file-name project-root))
      (or
       (counsel-mt-get-buffer-process-cwd)
       (expand-file-name default-directory)))))

(defun counsel-mt-generate-name()
  (generate-new-buffer-name
   (format "%s%s"
           counsel-mt-buffer-header
           (counsel-mt-get-terminal-name))))

(defun counsel-mt-launch-terminal ()
  "Launch a terminal in a new buffer."
  (let ((name (counsel-mt-generate-name)))
    (pcase counsel-mt-shell-type
      ('eshell (counsel-open-eshell name))
      ('term (counsel-open-term name))
      ('vterm (counsel-open-vterm name))
      )))

(defun counsel-mt-list-terminals ()
  "Counsel source with candidates for all terminal buffers."
  (cons '("Launch new terminal") (counsel-mt-get-terminal-buffers)))

(defun counsel-term ()
  (interactive)
  (ivy-read "Terminal: " (counsel-mt-list-terminals)
            :require-match t
            :preselect (counsel-mt-get-terminal-name)
            :action (lambda (candidate)
                      (let ((buf (cdr candidate)))
                        (if buf
                            (switch-to-buffer buf)
                          (counsel-mt-launch-terminal))))
            :caller 'counsel-term))

(provide 'counsel-term)
;;; counsel-term.el ends here
