;; -*- lexical-binding: t; -*-

(use-package corfu
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-cycle t)
  (corfu-auto-delay 0.4)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :init
  (global-corfu-mode)
  (global-set-key (kbd "M-/") 'completion-at-point))

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic))
  ;; (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  ;; (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp))))

(with-eval-after-load 'minibuffer
  (setq completion-category-overrides
        '((command (styles orderless+initialism))
          (symbol (styles orderless+initialism))
          (variable (styles orderless+initialism))
          (file (styles basic partical-completion))
          (eglot (styles orderless))
          (eglot-capf (styles orderless))
          )))

(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil))))

(with-eval-after-load 'corfu
  (corfu-popupinfo-mode))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(provide 'init-corfu)
