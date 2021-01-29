;;Remote Directory Tracking: https://www.emacswiki.org/emacs/AnsiTermHints#h5o-5
(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-c C-y") 'term-paste)
                            (define-key term-raw-map (kbd "C-c C-k") 'term-char-mode)
                            (define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)))

;; support different ssh port
(use-package multi-term
  :init
  (defvar multi-term-tramp-default-dir nil)
  :config
  (setq multi-term-program "/bin/bash")
  (setq multi-term-program-switches "--login")

  (defun multi-term-switch-buffer (term-buffer default-dir)
    "If we are in `tramp-mode', switch to TERM-BUFFER based on DEFAULT-DIR."
    (switch-to-buffer term-buffer)
    ;; Just test tramp file when library `tramp' is loaded.
    (setq multi-term-tramp-default-dir default-dir)
    (when (and (featurep 'tramp)
               (tramp-tramp-file-p default-dir))
      (setq multi-term-tramp-default-dir default-dir)
      (with-parsed-tramp-file-name default-dir path
        (let ((method (cadr (assoc `tramp-login-program (assoc path-method tramp-methods)))))
          (if path-port
              (term-send-raw-string (concat method " " (when path-user (concat path-user "@")) path-host " -p " path-port "\C-m"))
            (term-send-raw-string (concat method " " (when path-user (concat path-user "@")) path-host "\C-m")))
          (term-send-raw-string (concat "cd '" path-localname "'\C-m"))
          (term-send-raw-string "clear\C-m"))))))

(when *use-helm*
  (use-package helm-mt
    :config
    (defun helm-mt/launch-terminal (name prefix mode)
      "Launch a terminal in a new buffer.
NAME is the desired name of the buffer.  Pass \"%cwd\" to use the
working directory of the launched terminal process.  Additionally, the
buffer name will be prefixed with the given mode and made unique.
PREFIX is passed on to the function that creates the terminal as a
prefix argument.  MODE is either 'term or 'shell."
      (setq current-prefix-arg prefix)
      (cl-case mode
        ('term
         (setq name-prefix "terminal")
         (call-interactively 'multi-term))
        ('shell
         (setq name-prefix "shell")
         (call-interactively 'shell)))
      (when (string-equal name "%cwd")
        (setq name (or multi-term-tramp-default-dir (helm-mt/get-buffer-process-cwd (current-buffer)))))
      (rename-buffer (generate-new-buffer-name (format "*%s<%s>*" name-prefix name)))))
  (global-set-key (kbd "C-x t") 'helm-mt))

(provide 'init-mt)
