(use-package rtags
  :ensure t)

(use-package helm-rtags
  :ensure t
  :config
  (setq rtags-use-helm t)
  (setq rtags-display-result-backend 'helm))

(use-package company-rtags
  :ensure t)

(use-package flycheck-rtags
  :ensure t
  :config
  (set-face-background 'rtags-errline nil)
  (set-face-background 'rtags-fixitline nil)
  (set-face-background 'rtags-warnline nil))
;; (set-face-background 'rtags-warnline "orange")

(use-package cmake-ide
  :ensure t
  :config
  (setq cmake-ide-build-dir "./.build"))

(defvar loaded-rtags nil)

(defun enable-rtags()
  (unless loaded-rtags
    (require 'flycheck-rtags)
    (setq rtags-completions-enabled t)
    (push 'company-rtags company-backends)
    (setq rtags-autostart-diagnostics t)
    (rtags-enable-standard-keybindings)

    (flycheck-select-checker 'rtags)
    (rtags-diagnostics)

    (dolist (mode-map (list c-mode-map c++-mode-map))
      (define-key mode-map (kbd "M-.") 'rtags-find-symbol-at-point)
      (define-key mode-map (kbd "M-,") 'rtags-find-references-at-point)
      ;;(define-key mode-map (kbd "M-;") (function tags-find-file))
      (define-key mode-map (kbd "C-c s") 'rtags-find-symbol)
      (define-key mode-map (kbd "C-c f") 'rtags-find-references)
      (define-key mode-map (kbd "C-c r") 'rtags-rename-symbol)
      (define-key mode-map (kbd "C-c j") 'rtags-next-match)
      (define-key mode-map (kbd "C-c k") 'rtags-previous-match)
      (define-key mode-map (kbd "C-c v") 'rtags-find-virtuals-at-point)
      (define-key mode-map (kbd "C-c l") 'rtags-taglist)
      (define-key mode-map (kbd "M-*") 'rtags-location-stack-back))
    (cmake-ide-setup)
    (setq loaded-rtags t))
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil)
  (rtags-start-process-unless-running))

(provide 'init-rtags)
