;; -*- lexical-binding: t; -*-

(use-package gcmh
   :demand t
   :config
   (setq gcmh-low-cons-threshold (* 16 1024 1024)
         gcmh-high-cons-threshold (* 2048 1024 1024))
   (gcmh-mode 1))

(defvar my/gc-timer nil
  "Timer for garbage collection. See
`my/garbage-collect-on-focus-lost'.")

(defun my/garbage-collect-on-focus-lost ()
  "Garbage collect when Emacs loses focus.

Garbage collection is only triggered thirty seconds after losing
focus, and only once."
  (if (frame-focus-state)
      (when (timerp my/gc-timer)
       (cancel-timer my/gc-timer))
    (setq my/gc-timer (run-with-idle-timer 5 nil #'garbage-collect))))

;; (add-function :after after-focus-change-function #'my/garbage-collect-on-focus-lost)

(defun profile-user-start ()
  (interactive)
  (profiler-start 'cpu))

(defun profile-user-stop()
  (interactive)
  (profiler-stop)
  (profiler-report))

(global-set-key (kbd "C-x c s") #'profile-user-start)
(global-set-key (kbd "C-x c r") #'profile-user-stop)

(defun increase-profile-cpu-line-width (&optional arg)
  (interactive "ncpu format width: ")
  (setq profiler-report-cpu-line-format
        `((,arg left)
          (24 right ((19 right)
                     (5 right)))))
  (profiler-report-rerender-calltree))

(with-eval-after-load 'profiler
  (define-key profiler-report-mode-map (kbd "w") #'increase-profile-cpu-line-width))

(provide 'init-performance)
