(set-language-environment 'Chinese-GB)
(setq locale-coding-system 'euc-cn)
(set-default-coding-systems 'euc-cn)
(set-clipboard-coding-system 'euc-cn)
(set-selection-coding-system 'euc-cn)
(setenv "HOME" (getenv "UserProfile"))

(setq w32-get-true-file-attributes nil   ; decrease file IO workload
      w32-pipe-read-delay 0              ; faster IPC
      w32-pipe-buffer-size (* 128 1024))) ; read more at a time (was 4K)

(provide 'init-nt)
