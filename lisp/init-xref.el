;; -*- lexical-binding: t; -*-

(require 'xref)

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

(global-set-key (kbd "M-[") #'xref-go-back)
(global-set-key (kbd "M-]") #'xref-go-forward)
(global-set-key (kbd "M-,") #'xref-find-references)

;; dumb-jump
(use-package dumb-jump
  :demand t
  :config
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq xref-backend-functions (remq 'etags--xref-backend xref-backend-functions))
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate t))

(provide 'init-xref)
