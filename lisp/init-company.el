(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

(defun company-complete-remove-params(str)
  (string-trim-right (substring-no-properties str) "[<(].*[>)]"))

(defun company-complete-selection-partial ()
  "Insert the selected candidate."
  (interactive)
  (when (and (company-manual-begin) company-selection)
    (let* ((result (nth company-selection company-candidates))
           (text (company-complete-remove-params result)))
      (company-finish text))))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-h") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection-partial)
  (global-set-key (kbd "M-/") 'company-complete)
  (setq company-format-margin-function nil)
  (setq company-require-match nil)
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-backends (delete 'company-clang company-backends))
  ;;(set-face-attribute 'company-preview nil :inherit 'company-tooltip)
  (diminish 'company-mode))

(after-load-theme (set-face-attribute 'company-preview nil :inherit 'company-tooltip))

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

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  )

(provide 'init-company)
