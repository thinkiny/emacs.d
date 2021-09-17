(require-package 'ace-window)

(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x x")  'ace-swap-window)
(global-set-key (kbd "C-c [")  'windmove-left)
(global-set-key (kbd "C-c ]") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c j")  'windmove-down)

(defun crux-transpose-windows (arg)
  "Transpose the buffers shown in two windows.
Prefix ARG determines if the current windows buffer is swapped
with the next or previous window, and the number of
transpositions to execute in sequence."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (cl-plusp arg) (1- arg) (1+ arg))))))

(use-package winner
  :init (winner-mode t)
  :config
  (global-set-key (kbd "C-c 9") #'winner-undo)
  (global-set-key (kbd "C-c 0") #'winner-redo))

(global-set-key (kbd "C-c TAB") 'crux-transpose-windows)
(global-set-key (kbd "C-c m m") 'toggle-frame-maximized)
(global-set-key (kbd "C-c m f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)


(require 'project)
;; unset allkeys
(setf (cdr project-prefix-map) nil)

;; workgroups2
(require-package 'workgroups2)
(setq wg-prefix-key "C-x p")
(setq wg-first-wg-name "main")
(setq wg-session-file (expand-file-name ".emacs_workgroups" user-emacs-directory))
(setq wg-session-load-on-start nil)
(workgroups-mode 1)
(unbind-all-keys workgroups-mode-map)
(define-key workgroups-mode-map (kbd "C-x p c") #'wg-create-workgroup)
(define-key workgroups-mode-map (kbd "C-x p o") #'wg-open-workgroup)
(define-key workgroups-mode-map (kbd "C-x p s") #'wg-switch-to-workgroup)
(define-key workgroups-mode-map (kbd "C-x p w") (lambda ()
                                                  (interactive)
                                                  (wg-save-session)))

(with-eval-after-load 'pdf-outline
  (wg-support 'pdf-outline-buffer-mode 'pdf-outline
    `((deserialize . ,(lambda (_buffer _vars)
                        (let ((buf (get-buffer (car _vars))))
                          (if buf
                              (pdf-outline-noselect buf)))))
      (serialize . ,(lambda (_buffer)
                      (with-selected-window (pdf-outline-get-pdf-window)
                        (list (buffer-name))))))))

(provide 'init-windows)
