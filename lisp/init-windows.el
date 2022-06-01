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


;; change window size
(require 'windmove)
(defun change-window-size-up()
  (interactive)
  (if (windmove-find-other-window 'up)
      (call-interactively 'enlarge-window)
    (call-interactively 'shrink-window)))

(defun change-window-size-down()
  (interactive)
  (let ((down-wind (windmove-find-other-window 'down)))
    (if (and down-wind (not (eq down-wind (minibuffer-window))))
        (call-interactively 'enlarge-window)
      (call-interactively 'shrink-window))))

(defun change-window-size-left()
  (interactive)
  (if (windmove-find-other-window 'left)
      (call-interactively 'enlarge-window-horizontally)
    (call-interactively 'shrink-window-horizontally)))

(defun change-window-size-right()
  (interactive)
  (if (windmove-find-other-window 'right)
      (call-interactively 'enlarge-window-horizontally)
    (call-interactively 'shrink-window-horizontally)))

(global-set-key (kbd "C-c TAB") 'crux-transpose-windows)
(global-set-key (kbd "C-c m m") 'toggle-frame-maximized)
(global-set-key (kbd "C-c m f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-M-<left>") 'change-window-size-left)
(global-set-key (kbd "C-M-<right>") 'change-window-size-right)
(global-set-key (kbd "C-M-<down>") 'change-window-size-down)
(global-set-key (kbd "C-M-<up>") 'change-window-size-up)

(require 'project)
;; unset allkeys
(setf (cdr project-prefix-map) nil)

;;(require 'init-workgroups)
(require 'init-persp)

(provide 'init-windows)
