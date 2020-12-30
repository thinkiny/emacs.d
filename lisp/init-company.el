(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

(after-load 'company
  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (global-set-key (kbd "M-/") 'company-complete)
  ;;(setq company-begin-commands '(self-insert-command))
  (setq company-require-match nil)
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-align-annotations t)
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-backends (delete 'company-clang company-backends))
  (diminish 'company-mode))

;; company-theme
(defun light-company()
  (custom-set-faces
   '(company-preview
     ((t (:foreground "darkgray" :underline t))))
   '(company-preview-common
     ((t (:inherit company-preview))))
   '(company-tooltip
     ((t (:background "lightgray" :foreground "black"))))
   '(company-tooltip-selection
     ((t (:background "steelblue" :foreground "white smoke"))))
   '(company-tooltip-common
     ((((type x)) (:inherit company-tooltip :weight bold))
      (t (:inherit company-tooltip))))
   '(company-tooltip-common-selection
     ((((type x)) (:inherit company-tooltip-selection :weight bold))
      (t (:inherit company-tooltip-selection))))))

(defun dark-company()
  (require 'color)
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-preview ((t (:background, (color-lighten-name bg 10)))))
     `(company-preview-common ((t (:background, (color-lighten-name bg 5)))))
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground "gray75"))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

(provide 'init-company)
