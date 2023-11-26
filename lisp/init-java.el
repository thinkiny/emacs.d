(require-package 'java-snippets)
(require-package 'groovy-mode)
(require-package 'java-ts-mode)
(use-package android-mode)

(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))

(defconst java-style
  '((c-basic-offset . 4)
    (tab-width . 4)
    (c-offsets-alist . ((statement-cont . (max c-lineup-cascaded-calls
                                               (min c-lineup-multi-inher c-lineup-java-inher)))))))
(with-eval-after-load 'eglot
  (require 'init-eglot-java))

(defun my-java-mode-hook()
  (eglot-ensure)
  ;;(google-set-c-style-with-offset 4)
  ;;(c-toggle-auto-newline -1)
  (setq fill-column 100)
  (local-set-key (kbd "C-c t") #'eglot-java-run-test)
  (local-set-key (kbd "C-c b") #'eglot-java-project-build-task)
  (local-set-key (kbd "C-c u") #'eglot-java-project-build-refresh))

(add-hook 'java-mode-hook #'my-java-mode-hook)
(add-hook 'java-ts-mode-hook #'my-java-mode-hook)

(provide 'init-java)
;;; init-java ends here
