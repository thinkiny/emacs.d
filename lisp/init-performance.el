(use-package gcmh
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 128 1024 1024))
  (gcmh-mode 1))

(global-set-key (kbd "C-x p s") (lambda ()
                                  (interactive)
                                  (profiler-start 'cpu)))

(defun increase-profile-cpu-line-width (&optional arg)
  (interactive "ncpu format width: ")
  (setq profiler-report-cpu-line-format
        `((,arg left)
          (24 right ((19 right)
                     (5 right)))))
  (profiler-report-rerender-calltree))

(after-load 'profiler
  (define-key profiler-report-mode-map (kbd "w") #'increase-profile-cpu-line-width))

(global-set-key (kbd "C-x p r") (lambda ()
                                  (interactive)
                                  (profiler-report)
                                  (profiler-stop)))

(provide 'init-performance)
