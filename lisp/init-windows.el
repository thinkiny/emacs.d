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

;;(require 'init-workgroups)
(require 'init-persp)

(provide 'init-windows)
