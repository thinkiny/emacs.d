;; -*- lexical-binding: t; -*-

(require 'xref)

(global-set-key (kbd "M-[") #'xref-go-back)
(global-set-key (kbd "M-]") #'xref-go-forward)
(global-set-key (kbd "M-,") #'xref-find-references)

(setq xref-prompt-for-identifier nil)
(defun xref-find-last-match(marker)
  (let ((idx (- (ring-length xref--marker-ring) 1))
        (found nil))
    (while (and (not found) (> idx 1))
      (if (equal (ring-ref xref--marker-ring idx) marker)
          (setq found t)
        (setq idx (- idx 1))))
    idx))

(defun xref-pop-curr-marker-stack ()
  "Pop back to where \\[xref-find-definitions] was last invoked."
  (interactive)
  (let ((ring xref--marker-ring))
    (when (ring-empty-p ring)
      (user-error "Marker stack is empty"))
    (let* ((idx (- (xref-find-last-match (mark-marker)) 1))
           (before (ring-remove ring idx))
           (marker (ring-remove ring idx)))
      (set-marker before nil nil)
      (switch-to-buffer (or (marker-buffer marker)
                            (user-error "The marked buffer has been deleted")))
      (goto-char (marker-position marker))
      (set-marker marker nil nil)
      (run-hooks 'xref-after-return-hook))))

(setq xref-marker-ring-length 10240)
(defun xref-pop-marker-stack-maybe(&rest _)
  (when (get-buffer-window xref-buffer-name)
    (ring-remove xref--marker-ring 0)
    (ring-remove xref--marker-ring 0)))

(defun xref-push-check()
  (or (ring-empty-p xref--marker-ring)
      (not (equal (point-marker) (ring-ref xref--marker-ring 0)))))

(defun xref-push-marker-stack-once(&rest _)
  (xref--push-markers))

(defun xref-push-marker-stack-twice(&rest _)
  (xref--push-markers)
  (xref--push-markers))

(defun print-xref()
  (interactive)
  (let ((i 0)
        (n (ring-length xref--marker-ring)))
    (while (< i n)
      (prin1 (ring-ref xref--marker-ring i))
      (setq i (+ i 1)))))

(defun cleanup-xref()
  (interactive)
  (setq xref--marker-ring (make-ring xref-marker-ring-length)))


;;(advice-add 'counsel-imenu-action :before #'xref-push-marker-stack-once)
;;(advice-add 'minibuffer-keyboard-quit :before #'xref-pop-marker-stack-maybe)

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
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq xref-backend-functions (remq 'etags--xref-backend xref-backend-functions))
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate t))

(provide 'init-xref)
