(use-package gcmh
  :init
  (setq gcmh-idle-delay 5
        gcmh-low-cons-threshold (* 16 1024 1024)
        gcmh-high-cons-threshold (* 128 1024 1024))
  (gcmh-mode 1))

(provide 'init-gc)
