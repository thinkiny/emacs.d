;; -*- lexical-binding: t; -*-

(require 'xref)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

(setq xref-marker-ring-length 10240)
(defun print-xref()
  (interactive)
  (let ((i 0)
        (n (ring-length xref--marker-ring)))
    (while (< i n)
      (prin1 (ring-ref xref--marker-ring i))
      (setq i (+ i 1)))))

;; projectile && eglot
(defun get-xref-eglot-project()
  (if (bound-and-true-p eglot--managed-mode)
      (if-let* ((server (eglot-current-server)))
          (eglot--project server))))

(defun get-xref-elisp-project()
  (if (eq major-mode 'emacs-lisp-mode)
      "elisp"))

(defun get-xref-project()
  (or
   (get-xref-elisp-project)
   (get-xref-eglot-project)
   (projectile-project-name)))

(defvar projectile-params--store (make-hash-table :test 'equal)
  "The store of project parameters.")

(defun projectile-param-get-parameter (param)
  "Return project parameter PARAM, or nil if unset."
  (let ((key (cons (get-xref-project) param)))
    (gethash key projectile-params--store nil)))

(defun projectile-param-set-parameter (param value)
  "Set the project parameter PARAM to VALUE."
  (let ((key (cons (get-xref-project) param)))
    (puthash key value projectile-params--store))
  value)

(defun projectile-param-xref-history (&optional new-value)
  "Return project-local xref history for the current projectile.

Override existing value with NEW-VALUE if it's set."
  (if new-value
      (projectile-param-set-parameter 'xref--history new-value)
    (or (projectile-param-get-parameter 'xref--history)
        (projectile-param-set-parameter 'xref--history (xref--make-xref-history)))))

(setq xref-history-storage #'projectile-param-xref-history)

;; dumb-jump
(use-package dumb-jump
  :demand t
  :config
  (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-prefer-searcher 'rg)
  (setq xref-backend-functions (remq 'etags--xref-backend xref-backend-functions))
  ;; (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate)
  )

(defvar-local my/xref-fallback-backends nil
  "Buffer-local list of fallback xref backend functions when primary fails.")

(defun my/xref-find-definitions ()
  "Find definitions with fallback when backend fails."
  (interactive)
  (if (null my/xref-fallback-backends)
      (xref-find-definitions (xref-backend-identifier-at-point (xref-find-backend)))
    (condition-case nil
        (xref-find-definitions (xref-backend-identifier-at-point (xref-find-backend)))
      (error
       (let ((xref-backend-functions my/xref-fallback-backends))
         (xref-find-definitions (xref-backend-identifier-at-point (xref-find-backend))))))))

(defun my/xref-find-references ()
  "Find references, merging results from primary and fallback backends."
  (interactive)
  (let* ((primary-backend (xref-find-backend))
         (identifier (xref-backend-identifier-at-point primary-backend))
         (all-xrefs nil))
    ;; Collect from primary backend
    (condition-case nil
        (when-let ((xrefs (xref-backend-references primary-backend identifier)))
          (setq all-xrefs (append all-xrefs xrefs)))
      (error nil))
    ;; Collect from fallback backends
    (dolist (backend-fn my/xref-fallback-backends)
      (when-let ((backend (funcall backend-fn)))
        (condition-case nil
            (let ((fallback-id (xref-backend-identifier-at-point backend)))
              (when-let ((xrefs (xref-backend-references backend fallback-id)))
                (setq all-xrefs (append all-xrefs xrefs))))
          (error nil))))
    ;; Display results
    (if all-xrefs
        (xref-show-xrefs (lambda () all-xrefs) nil)
      (user-error "No references found"))))

(global-set-key (kbd "M-.") #'my/xref-find-definitions)
(global-set-key (kbd "M-,") #'my/xref-find-references)
(global-set-key (kbd "M-[") #'xref-go-back)
(global-set-key (kbd "M-]") #'xref-go-forward)

(provide 'init-xref)
