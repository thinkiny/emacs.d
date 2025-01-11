;;; init-utils.el --- some useful functions -*- lexical-binding: t -*-
(require 'subr-x)
(require 'cl-lib)

;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun process-insertion-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (display-buffer (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      ;;(read-only-mode 0)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc))))
      ;;(read-only-mode 1)
      )))

(defun unbind-all-keys (keymap)
  (map-keymap (lambda (key _)
                (define-key keymap (vector key) nil))
              keymap))

(defun deserialize-from-file (file)
  "Read a lisp expression from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (cl-first (read-from-string
               (buffer-substring-no-properties (point-min) (point-max))))))

(defun serialize-to-file (file to-persist)
  "Serialize TO-PERSIST to FILE."
  (with-demoted-errors
      "Failed to persist file: %S"
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (erase-buffer)
      (insert (prin1-to-string to-persist)))))

(defun execute-command(name cmd)
  (interactive)
  (let ((process (start-file-process name nil "/bin/bash" "-c" "-l" cmd " &")))
    (set-process-sentinel process
                          (lambda (process event)
                            (princ (format "%s: %s" process event))))))

;; resolve symlink correct
(defun advice/ignore-file-truename (old-fn &rest args)
  (cl-letf (((symbol-function 'file-truename) #'identity))
    (apply old-fn args)))

(defun ignore-file-truename (&rest funcs)
  (dolist (func funcs)
    (advice-add func :around #'advice/ignore-file-truename)))

(require 'doom-utils)

;; init platform
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defconst *is-a-nt* (eq system-type 'windows-nt))

;; themes
(defvar load-theme-hook nil)
(defmacro after-load-theme (&rest body)
  `(add-hook 'load-theme-hook (lambda () ,@body)))

(defmacro with-eval-after-load-theme (mode &rest body)
  `(progn
     (eval-after-load ,mode (lambda () ,@body))
     (add-hook 'load-theme-hook
               (lambda ()
                 (when (featurep ,mode)
                   ,@body)))))

;; proxy
(defconst enable-local-proxy t)
(defvar url-proxy-services-local
  '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
    ("http" . "127.0.0.1:1087")
    ("https" . "127.0.0.1:1087")))

(defun set-proxy()
  (interactive)
  (setq url-proxy-services url-proxy-services-local))

(defun unset-proxy()
  (interactive)
  (setq url-proxy-services nil))

(defun advice/use-proxy-local (func &rest args)
  (defvar url-proxy-services)
  (if enable-local-proxy
      (let ((url-proxy-services url-proxy-services-local))
        (apply func args))
    (apply func args)))

(defun use-proxy-local(&rest funcs)
  (dolist (func funcs)
    (advice-add func :around #'advice/use-proxy-local)))

;; common string
(defun common-string-length (a b &optional idx)
  (setq idx (or idx 0))
  (if (and (< idx (length a))
           (< idx (length b))
           (char-equal (aref a idx) (aref b idx)))
      (common-string-length a b (+ idx 1))
    idx))

;; ignore errors
(defun advice/ignore-errors (old-fn &rest args)
  (ignore-errors
    (apply old-fn args)))

(provide 'init-utils)
