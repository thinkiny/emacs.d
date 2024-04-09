(use-package gcmh
  :init
  (setq gcmh-low-cons-threshold (* 64 1024 1024)
        gcmh-high-cons-threshold most-positive-fixnum)
  (gcmh-mode 1))

(defun profile-cpu-start ()
  (interactive)
  (profiler-start 'cpu))

(defun profile-cpu-stop()
  (interactive)
  (profiler-stop)
  (profiler-report))

(global-set-key (kbd "C-x c s") #'profile-cpu-start)
(global-set-key (kbd "C-x c r") #'profile-cpu-stop)

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
