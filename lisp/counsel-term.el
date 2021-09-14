;;; counsel-term.el --- counsel terminal management -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 's)

(defvar counsel-mt-shell-type 'vterm)
(defconst counsel-mt-buffer-header "*term: ")

(defun counsel-mt-get-terminal-name()
  (if (tramp-tramp-file-p default-directory)
      (with-parsed-tramp-file-name default-directory term
        (concat (or term-domain term-host) " :: " term-localname))
    (expand-file-name default-directory)))

(defun counsel-mt-new-buffer-name()
  (generate-new-buffer-name
   (format "%s%s"
           counsel-mt-buffer-header
           (counsel-mt-get-terminal-name))))

(defun counsel-mt-get-terminal-name-with-idx(name-count)
  (let* ((name (counsel-mt-get-terminal-name))
         (idx (gethash name name-count 1)))
    (puthash name (+ idx 1) name-count)
    (if (= idx 1)
        name
      (format "%s<%d>" name idx))))

(defun counsel-mt-get-sorted-buffer-list ()
  (sort (buffer-list)
        (lambda (a b)
          (string< (buffer-name a) (buffer-name b)))))

(defun counsel-mt-list-opened-terminals ()
  "List opened directory"
  (let ((name-count (make-hash-table :test 'equal)))
    (cl-loop for buf in (counsel-mt-get-sorted-buffer-list)
             when (member (buffer-local-value 'major-mode buf) '(eshell-mode term-mode vterm-mode))
             collect (with-current-buffer buf
                       (let* ((name (counsel-mt-get-terminal-name-with-idx name-count))
                              (buf-name (format "%s%s" counsel-mt-buffer-header name))
                              (buf-name-buf (get-buffer buf-name)))
                         (if buf-name-buf
                             (with-current-buffer buf-name-buf
                               (rename-buffer "counsel-mt-list-temp")))
                         (rename-buffer buf-name)
                         (cons name buf))))))

(defun counsel-mt-list-terminal-names()
  (mapcar 'car (counsel-mt-list-opened-terminals)))

(defun counsel-mt-ivy-pselect()
  (let* ((name (counsel-mt-get-terminal-name))
         (names (counsel-mt-list-terminal-names))
         (matches (seq-filter (lambda (x)
                                (string-prefix-p x name)) names)))
    (if matches
        (car matches)
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

(defun counsel-mt-launch-terminal()
  "Launch a terminal in a new buffer."
  (let ((name (counsel-mt-new-buffer-name)))
    (pcase counsel-mt-shell-type
      ('eshell (counsel-open-eshell name))
      ('term (counsel-open-term name))
      ('vterm (counsel-open-vterm name)))))

(defun counsel-mt-list-terminals ()
  "Counsel source with candidates for all terminal buffers."
  (cons '("Launch new terminal") (counsel-mt-list-opened-terminals)))

(defun counsel-term ()
  (interactive)
  (ivy-read "Terminal: " (counsel-mt-list-terminals)
            :require-match t
            :preselect (counsel-mt-ivy-pselect)
            :action (lambda (candidate)
                      (let ((buf (cdr candidate)))
                        (if buf
                            (switch-to-buffer buf)
                          (counsel-mt-launch-terminal))))
            :caller 'counsel-term))

(provide 'counsel-term)
;;; counsel-term.el ends here
