;; -*- lexical-binding: t; -*-

;; Packages
(use-package format-all
  :config
  (setq format-all-show-errors 'never)
  (setcdr (assoc "SQL" format-all-default-formatters) '(pgformatter))
  (setcdr (assoc "Python" format-all-default-formatters) '(ruff)))

(use-package whitespace-cleanup-mode)

;; Unified format-at-save variable
(defvar-local enable-format-at-save t)
(put 'enable-format-at-save 'safe-local-variable #'always)

(setq-default show-trailing-whitespace nil)
(setq whitespace-cleanup-mode-only-if-initially-clean nil)
(global-set-key [remap just-one-space] 'cycle-spacing)

;; Eglot format wrapper — checks eglot at save time, not setup time
(defun maybe-eglot-format-buffer ()
  (when (and (bound-and-true-p eglot--managed-mode)
             (eglot-server-capable :documentFormattingProvider))
    (eglot-format-buffer)))

;; Interactive toggle commands
(defun enable-format-on-save ()
  (interactive)
  (setq-local enable-format-at-save t)
  (setq-local show-trailing-whitespace t)
  (unless (file-remote-p default-directory)
    (whitespace-cleanup-mode 1))
  (add-hook 'before-save-hook #'maybe-eglot-format-buffer -10 t))

(defun disable-format-on-save ()
  (interactive)
  (setq-local enable-format-at-save nil)
  (setq-local show-trailing-whitespace t)
  (whitespace-cleanup-mode -1)
  (when (bound-and-true-p format-all-mode)
    (format-all-mode -1))
  (remove-hook 'before-save-hook #'maybe-eglot-format-buffer t))

(defun toggle-format-on-save ()
  "Set up or tear down format-on-save for current buffer based on `enable-format-at-save'."
  (when (derived-mode-p 'prog-mode 'text-mode 'conf-mode)
    (if enable-format-at-save
        (enable-format-on-save)
      (disable-format-on-save))))

;; Run after dir-locals so `enable-format-at-save' from .dir-locals.el is respected
(add-hook 'hack-local-variables-hook #'toggle-format-on-save)

(defun disable-format-on-save-project ()
  (interactive)
  (when-let* ((project-root (projectile-project-root))
              (file (format "%s.dir-locals.el" project-root)))
    (let* ((existing (when (file-exists-p file)
                       (with-temp-buffer
                         (insert-file-contents file)
                         (ignore-errors (read (current-buffer))))))
           (nil-vars (assq-delete-all 'enable-format-at-save (cdr (assoc nil existing))))
           (rest (assq-delete-all nil existing)))
      (with-temp-file file
        (pp (cons (cons nil (cons '(enable-format-at-save . nil) nil-vars)) rest)
            (current-buffer)))
      (message "wrote %s" file))))

;; format-all helpers
(defun enable-format-all-mode ()
  (interactive)
  (when enable-format-at-save
    (format-all-mode 1)
    (format-all-ensure-formatter)))

(defun format-current-buffer ()
  (interactive)
  (format-all-ensure-formatter)
  (format-all-buffer))

;;(add-hook 'sql-mode-hook #'enable-format-all-mode)
(add-hook 'protobuf-mode-hook #'enable-format-all-mode)

(provide 'init-format)
