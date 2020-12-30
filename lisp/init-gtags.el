(use-package helm-gtags
  :config
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-path-style 'relative
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-suggested-key-mapping t))

(defun gtags-get-rootpath ()
  (let (path buffer)
    (save-excursion
      (setq buffer (generate-new-buffer (generate-new-buffer-name "*rootdir*")))
      (set-buffer buffer)
      (if (= (call-process "global" nil t nil "-pr") 0)
          (setq path (file-name-as-directory (buffer-substring (point-min)(1- (point-max))))))
      (kill-buffer buffer))
    path))

(add-hook 'helm-gtags-mode-hook
          (lambda ()
            (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-find-pattern)
            (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-symbol)
            (define-key helm-gtags-mode-map (kbd "C-c f") 'helm-gtags-find-rtag)
            (define-key helm-gtags-mode-map (kbd "C-c l") 'helm-gtags-parse-file)))

(add-hook 'lua-mode-hook 'helm-gtags-mode)
;;(add-hook 'go-mode-hook 'helm-gtags-mode)

(provide 'init-gtags)
