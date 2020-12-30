(defun my-asm-mode-hook ()
  ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
  (local-unset-key (vector asm-comment-char))
  (electric-indent-local-mode)
  (setq tab-always-indent (default-value 'tab-always-indent))

  (defun asm-calculate-indentation ()
    (or
     ;; Flush labels to the left margin.
     (and (looking-at "\\.section") 0)
     (and (looking-at "[.@_[:word:]]+:") 0)
     ;; Same thing for `;;;' comments.
     (and (looking-at "\\s<\\s<\\s<") 0)
     ;; %if nasm macro stuff goes to the left margin
     (and (looking-at "%") 0)
     (and (looking-at "c?global\\|section\\|default\\|align\\|INIT_..X") 0)
     ;; Simple `;' comments go to the comment-column
                                        ;(and (looking-at "\\s<\\(\\S<\\|\\'\\)") comment-column)
     (indent-next-tab-stop 0))))
(add-hook 'asm-mode-hook #'my-asm-mode-hook)
(provide 'init-asm)
