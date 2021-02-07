(defun gtags-get-rootpath ()
  (let (path buffer)
    (save-excursion
      (setq buffer (generate-new-buffer (generate-new-buffer-name "*rootdir*")))
      (set-buffer buffer)
      (if (= (call-process "global" nil t nil "-pr") 0)
          (setq path (file-name-as-directory (buffer-substring (point-min)(1- (point-max))))))
      (kill-buffer buffer))
    path))

(when *use-helm*
  (use-package helm-gtags
    :config
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-path-style 'relative
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-suggested-key-mapping t))

  (add-hook 'helm-gtags-mode-hook
            (lambda ()
              (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-find-pattern)
              (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-symbol)
              (define-key helm-gtags-mode-map (kbd "C-c f") 'helm-gtags-find-rtag)
              (define-key helm-gtags-mode-map (kbd "C-c l") 'helm-gtags-parse-file))))

(when *use-ivy*
  (use-package counsel-gtags
    :bind (:map counsel-gtags-mode-map
                ("M-," . counsel-gtags-find-reference)
                ("M-." . counsel-gtags-find-definition)
                ("C-c f" . counsel-gtags-find-symbol))
    :config
    (setq counsel-gtags-auto-update t)))

(defun gtags-mode-on ()
  (interactive)
  (when *use-helm*
    (helm-gtags-mode))
  (when *use-ivy*
    (counsel-gtags-mode)))

(add-hook 'lua-mode-hook 'gtags-mode-on)
(add-hook 'asm-mode-hook 'gtags-mode-on)

(provide 'init-gtags)
