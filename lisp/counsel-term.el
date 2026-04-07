;;; counsel-term.el --- Ivy-based terminal buffer manager  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Provides `counsel-term', an ivy-based picker for managed terminal
;; buffers (vterm or eshell).  Bound to `C-x t' in init-term.el.

;;; Code:

(require 'cl-lib)
(require 'ivy)
(require 'tramp)
(require 'vterm)
(require 'perspective)

(defvar counsel-mt-shell-type 'vterm
  "Terminal backend to use.  Either `vterm' or `eshell'.")

(defcustom counsel-mt-refresh-interval 1
  "Idle seconds between terminal buffer name refreshes."
  :type 'number :group 'convenience)

(defconst counsel-mt-name-header "*TERM-"
  "Prefix string for managed terminal buffer names.")

(defvar-local counsel-mt-index nil
  "Per-buffer index assigned when a terminal is launched.")

(defun counsel-mt--get-dir (&optional buf)
  "Return a display string for the working directory of BUF.
For TRAMP buffers the string includes the remote host."
  (with-current-buffer (or buf (current-buffer))
    (if (tramp-tramp-file-p default-directory)
        (with-parsed-tramp-file-name default-directory term
          (concat (or term-domain term-host) " :: " term-localname))
      (expand-file-name default-directory))))

(defun counsel-mt--buf-index (&optional buf)
  "Return the `counsel-mt-index' of BUF, or nil if BUF is not managed.
Defaults to the current buffer."
  (buffer-local-value 'counsel-mt-index (or buf (current-buffer))))

(defun counsel-mt--buf-name (idx &optional buf)
  "Return the buffer name for a managed terminal with index IDX.
Uses the working directory of BUF, defaulting to the current buffer."
  (format "%s[%d] %s" counsel-mt-name-header idx (counsel-mt--get-dir buf)))

(defun counsel-mt--next-index ()
  "Return the smallest non-negative integer not used as a terminal index."
  (let ((used (mapcar #'counsel-mt--buf-index (buffer-list)))
        (n 0))
    (while (memq n used) (cl-incf n))
    n))

(defun counsel-mt--candidates ()
  "Build the candidate list for `counsel-term'.
Renames managed buffers to reflect their current directory,
collects them as alist pairs, sorts by directory then index,
and prepends a \"Launch new terminal\" entry."
  (let (pairs)
    (dolist (buf (persp-current-buffers* t))
      (when (counsel-mt--buf-index buf)
        (with-current-buffer buf
          (let ((label (format "[%d] %s" (counsel-mt--buf-index) (counsel-mt--get-dir))))
            (rename-buffer (counsel-mt--buf-name (counsel-mt--buf-index)))
            (push (cons label buf) pairs)))))
    (cons '("Launch new terminal")
          (cl-sort pairs
                   (lambda (a b)
                     (let ((a-dir (counsel-mt--get-dir (cdr a)))
                           (b-dir (counsel-mt--get-dir (cdr b))))
                       (if (string-lessp a-dir b-dir)
                           t
                         (and (string-equal a-dir b-dir)
                              (< (counsel-mt--buf-index (cdr a))
                                 (counsel-mt--buf-index (cdr b)))))))))))

(defun counsel-mt--refresh-names ()
  "Rename all managed terminal buffers to reflect current directory."
  (dolist (buf (buffer-list))
    (when-let* ((idx (buffer-local-value 'counsel-mt-index buf)))
      (with-current-buffer buf
        (let ((new-name (counsel-mt--buf-name idx)))
          (unless (equal (buffer-name) new-name)
            (rename-buffer new-name)))))))

(defun counsel-mt--preselect ()
  "Return the ivy preselect string for `counsel-term'.
If the current buffer is managed, preselect its index.
Otherwise, find the managed buffer whose directory is the
longest prefix of the current directory."
  (if (counsel-mt--buf-index (current-buffer))
      (format "[%d]" (counsel-mt--buf-index))
    (let ((dir (counsel-mt--get-dir))
          (best nil))
      (dolist (buf (buffer-list))
        (when (counsel-mt--buf-index buf)
          (let ((buf-dir (counsel-mt--get-dir buf)))
            (when (and (string-prefix-p buf-dir dir)
                       (or (null best)
                           (> (length buf-dir) (length best))))
              (setq best buf-dir)))))
      (or best dir))))

(defun counsel-mt--launch ()
  "Launch a terminal in a new buffer."
  (let* ((idx (counsel-mt--next-index))
         (name (generate-new-buffer-name (counsel-mt--buf-name idx))))
    (pcase counsel-mt-shell-type
      ('eshell (call-interactively 'eshell) (rename-buffer name))
      ('vterm (switch-to-buffer (vterm name)) (rename-buffer name)))
    (setq-local counsel-mt-index idx)))

(defun counsel-term ()
  "Open ivy-based terminal buffer picker."
  (interactive)
  (ivy-read "Terminal: " (counsel-mt--candidates)
            :require-match t
            :preselect (counsel-mt--preselect)
            :action (lambda (candidate)
                      (let ((buf (cdr candidate)))
                        (if buf
                            (switch-to-buffer buf)
                          (counsel-mt--launch))))
            :caller 'counsel-term))

(ignore-tramp-ssh-control-master 'counsel-mt--launch)

(defvar counsel-mt--refresh-timer nil)

(defun counsel-mt--start-refresh-timer ()
  "Start or restart the idle timer for buffer name refresh."
  (counsel-mt-stop-refresh-timer)
  (setq counsel-mt--refresh-timer
        (run-with-idle-timer counsel-mt-refresh-interval t
                             #'counsel-mt--refresh-names)))

(defun counsel-mt-stop-refresh-timer ()
  "Cancel the buffer name refresh timer."
  (when counsel-mt--refresh-timer
    (cancel-timer counsel-mt--refresh-timer)
    (setq counsel-mt--refresh-timer nil)))

(counsel-mt--start-refresh-timer)

(provide 'counsel-term)
;;; counsel-term.el ends here
