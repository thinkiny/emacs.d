(use-package sideline
  :init
  ;;(setq sideline-backends-left '((sideline-blame . down)))
  (setq sideline-backends-right '((sideline-flymake . down))))
  ;;(global-sideline-mode 1))

(use-package sideline-flymake
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-flymake-display-mode 'line))

(use-package sideline-blame
  :after sideline)

(defun enable-sideline-blame()
  (interactive)
  (setq sideline-backends-left '((sideline-blame . down))))

(defun disable-sideline-blame()
  (interactive)
  (setq sideline-backends-left nil))

(provide 'init-sideline)
