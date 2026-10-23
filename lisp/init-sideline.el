;; -*- lexical-binding: t; -*-

(use-package sideline
  :config
  (setq sideline-force-display-if-exceeds t)
  (setq sideline-truncate nil)
  ;;(setq sideline-backends-left '((sideline-blame . down)))
  (setq sideline-backends-right '((sideline-flymake . down)
                                  ;; (sideline-eglot . up)
                                  ))
  :hook (prog-mode . sideline-mode))

(use-package sideline-flymake
  :config
  (setq sideline-flymake-display-mode 'line)
  :hook (flymake-mode . sideline-mode))

(use-package sideline-blame
  :after sideline)

(defun enable-sideline-blame()
  (interactive)
  (setq sideline-backends-left '((sideline-blame . down))))

(defun disable-sideline-blame()
  (interactive)
  (setq sideline-backends-left nil))

(provide 'init-sideline)
