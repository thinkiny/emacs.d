;; -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 's)

(defvar counsel-mt-shell-type 'vterm)
(defconst counsel-mt-name-header "*term: ")
(defvar-local counsel-mt-create-time nil)

(defun counsel-mt/get-name()
  (if (tramp-tramp-file-p default-directory)
      (with-parsed-tramp-file-name default-directory term
        (concat (or term-domain term-host) " :: " term-localname))
    (expand-file-name default-directory)))

(defun counsel-mt/new-buffer-name()
  (generate-new-buffer-name
   (format "%s%s"
           counsel-mt-name-header
           (counsel-mt/get-name))))

(defun counsel-mt/create-name-with-idx(name-count)
  (let* ((name (counsel-mt/get-name))
         (idx (gethash name name-count 1)))
    (puthash name (+ idx 1) name-count)
    (if (= idx 1)
        name
      (format "%s<%d>" name idx))))


(defun counsel-mt/get-create-time (buf)
  (buffer-local-value 'counsel-mt-create-time buf))

(defun counsel-mt/get-sorted-buffer-list ()
  (sort (buffer-list)
        (lambda (a b)
          (time-less-p (counsel-mt/get-create-time a)
                       (counsel-mt/get-create-time b)))))

(defun counsel-mt/list-opened ()
  "List opened directory"
  (let ((name-count (make-hash-table :test 'equal)))
    (cl-loop for buf in (counsel-mt/get-sorted-buffer-list)
             when (member (buffer-local-value 'major-mode buf) '(eshell-mode term-mode vterm-mode))
             collect (with-current-buffer buf
                       (let* ((name-idx (counsel-mt/create-name-with-idx name-count))
                              (new-buf-name (format "%s%s" counsel-mt-name-header name-idx)))
                         (unless (string-equal new-buf-name (buffer-name))
                           (if-let ((curr-buf (get-buffer new-buf-name)))
                               (with-current-buffer curr-buf
                                 (rename-buffer (format "%s-old" new-buf-name))))
                           (rename-buffer new-buf-name))
                         (cons name-idx buf))))))

(defun counsel-mt/list-names()
  (mapcar 'car (counsel-mt/list-opened)))

(defun counsel-mt/ivy-pselect()
  (let* ((name (counsel-mt/get-name))
         (names (counsel-mt/list-names)))
    (if (length> names 0)
        (-max-by (lambda (a b)
                   (> (common-string-length a name)
                      (common-string-length b name))) names)
      name)))

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
                  (concat "\"cd " term-localname " && /bin/bash -l\""))))
          ("docker"
           (list "docker" "exec" "-it" term-host "/bin/bash"))
          (tramp-jumper-method
           (list tramp-jumper-exec term-user term-host
                 (concat "cd " term-localname)))))
    (list "/bin/bash" "-l")))

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
          ;; (vterm--flush-output (format "cd %s\n" term-localname))
          ;; (vterm--flush-output "clear\n")
          (setq-local tramp-default-method term-method)))
    (rename-buffer name)))

(defun counsel-mt/launch()
  "Launch a terminal in a new buffer."
  (let ((name (counsel-mt/new-buffer-name)))
    (pcase counsel-mt-shell-type
      ('eshell (counsel-open-eshell name))
      ('term (counsel-open-term name))
      ('vterm (counsel-open-vterm name))))
  (setq-local counsel-mt-create-time (current-time)))

(defun counsel-mt/list ()
  "Counsel source with candidates for all terminal buffers."
  (cons '("Launch new terminal") (counsel-mt/list-opened)))

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

(provide 'counsel-term)
;;; counsel-term.el ends here
