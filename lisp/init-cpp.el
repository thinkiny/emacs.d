(require-package 'cff)
(require-package 'cmake-mode)

;;(require 'init-rtags)
(require 'init-gtags)

(use-package bazel-mode :mode "\\.BUILD$")
(use-package ccls
  :config
  (setq ccls-sem-highlight-method 'font-lock))

(after-load 'ccls
  (require 'dap-cpptools)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection ccls-executable)
    :major-modes '(c-mode c++-mode cuda-mode objc-mode)
    :server-id 'ccls-remote
    :multi-root nil
    :remote? t
    :notification-handlers
    (lsp-ht ("$ccls/publishSkippedRanges" #'ccls--publish-skipped-ranges)
            ("$ccls/publishSemanticHighlight" #'ccls--publish-semantic-highlight))
    :initialization-options nil
    :library-folders-fn nil)))

;; styles
(require 'google-c-style)
(defun my-c-mode-hook ()
  ;; ;;echo "" | g++ -v -x c++ -E -
  (c-add-style "Google" google-c-style t)
  (define-key c-mode-base-map (kbd "C-c x") 'cff-find-other-file)
  (if (gtags-get-rootpath)
      (helm-gtags-mode)
    (lsp)))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

(provide 'init-cpp)
