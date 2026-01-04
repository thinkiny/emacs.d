;; -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 'vterm)

(defvar counsel-mt-shell-type 'vterm)
(defconst counsel-mt-name-header "*TERM-")

(defun counsel-mt/get-dir(&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (if (tramp-tramp-file-p default-directory)
        (with-parsed-tramp-file-name default-directory term
          (concat (or term-domain term-host) " :: " term-localname))
      (expand-file-name default-directory))))

(defun counsel-mt/get-dir-with-index()
  (format "[%d] %s" (counsel-mt/get-buf-index) (counsel-mt/get-dir)))

(defun counsel-mt/new-buffer-name(idx)
  (generate-new-buffer-name
   (format "%s[%d] %s" counsel-mt-name-header idx (counsel-mt/get-dir))))

(defun counsel-mt/get-continuation (seq)
  (let ((next 0))
    (cl-loop for i in seq
             if (not (= i next)) return next
             else do (setq next (+ 1 next))
             finally return next)))

(defun counsel-mt/is-managed(buf)
  (and (member (buffer-local-value 'major-mode buf) '(eshell-mode term-mode vterm-mode))
       (buffer-local-boundp 'counsel-mt-index buf)))

(defun counsel-mt/get-next-index()
  (counsel-mt/get-continuation
   (cl-sort (cl-loop for buf in (buffer-list)
                     when (counsel-mt/is-managed buf)
                     collect (counsel-mt/get-buf-index buf))
            #'< )))

(defun counsel-mt/get-buf-index(&optional buf)
  (buffer-local-value 'counsel-mt-index (or buf (current-buffer))))

(defun counsel-mt/get-buf-index-pair(pair)
  (counsel-mt/get-buf-index (cdr pair)))

(defun counsel-mt/sort-dir-pair(a b)
  (let ((a-dir (counsel-mt/get-dir (cdr a)))
        (b-dir (counsel-mt/get-dir (cdr b))))
    (if (string-lessp a-dir b-dir)
        t
      (and (string-equal a-dir b-dir)
           (< (counsel-mt/get-buf-index-pair a) (counsel-mt/get-buf-index-pair b))))))

(defun counsel-mt/list-persp-by-dir()
  "Sort all terminal buffer ordered by current directory"
  (cl-sort (cl-loop for buf in (persp-current-buffers* t)
                    when (counsel-mt/is-managed buf)
                    collect (with-current-buffer buf
                              (rename-buffer (format "%s%s" counsel-mt-name-header (counsel-mt/get-dir-with-index)))
                              (cons (counsel-mt/get-dir-with-index) buf)))
           #'counsel-mt/sort-dir-pair))

(defun counsel-mt/list-dirs-by-index()
  (mapcar #'car (cl-sort (cl-loop for buf in (buffer-list)
                                  when (counsel-mt/is-managed buf)
                                  collect (cons (counsel-mt/get-dir buf) buf))
                         #'< :key #'counsel-mt/get-buf-index-pair)))

(defun counsel-mt/ivy-pselect-max-common()
  (let* ((dir  (counsel-mt/get-dir))
         (dirs (counsel-mt/list-dirs-by-index)))
    (if (length> dirs 0)
        (-max-by (lambda (a b)
                   (>= (common-string-length a dir)
                       (common-string-length b dir))) dirs)
      dir)))

(defun counsel-mt/ivy-pselect()
  (if (buffer-local-boundp 'counsel-mt-index (current-buffer))
      (format "[%d]" (counsel-mt/get-buf-index))
    (counsel-mt/ivy-pselect-max-common)))

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
                  (concat "\"cd '" term-localname "' && /bin/bash -l\""))))
          ("docker"
           (list "docker" "exec" "-it" term-host "/bin/bash"))
          (tramp-jumper-method
           (list tramp-jumper-exec term-user term-host
                 (concat "cd " term-localname)))))
    (list (getenv "SHELL") "-l")))

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
  (switch-to-buffer (vterm name))
  ;; (if (tramp-tramp-file-p default-directory)
  ;;     (with-parsed-tramp-file-name default-directory term
  ;;       (vterm--flush-output (format "cd '%s'\n" term-localname))
  ;;       (vterm--flush-output "clear\n")))
  (rename-buffer name))

(defun counsel-mt/launch()
  "Launch a terminal in a new buffer."
  (let* ((idx (counsel-mt/get-next-index))
         (name (counsel-mt/new-buffer-name idx)))
    (pcase counsel-mt-shell-type
      ('eshell (counsel-open-eshell name))
      ('term (counsel-open-term name))
      ('vterm (counsel-open-vterm name)))
    (setq-local counsel-mt-index idx)))


(defun counsel-mt/list ()
  "Counsel source with candidates for all terminal buffers."
  (cons '("Launch new terminal") (counsel-mt/list-persp-by-dir)))

(defun counsel-term ()
  (interactive)
  (ivy-read "Terminal: " (counsel-mt/list)
            :require-match t
            :preselect (counsel-mt/ivy-pselect)
            :action (lambda (candidate)
                      (let ((buf (cdr candidate)))
                        (if buf
                            (switch-to-buffer buf)
                          (counsel-mt/launch))))
            :caller 'counsel-term))

(ignore-tramp-ssh-control-master 'counsel-mt/launch)

(provide 'counsel-term)
;;; counsel-term.el ends here
