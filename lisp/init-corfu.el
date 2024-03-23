(use-package corfu
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-cycle t)
  (corfu-auto-delay 0.4)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :init
  (global-corfu-mode)
  (global-set-key (kbd "M-/") 'completion-at-point))

(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil))))

(use-package corfu-terminal)

(with-eval-after-load 'corfu
  (corfu-popupinfo-mode)
  (unless window-system
    (corfu-terminal-mode)))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight)))

(provide 'init-corfu)
