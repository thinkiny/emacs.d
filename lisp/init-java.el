(require-package 'java-snippets)
(require-package 'groovy-mode)
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))

(use-package android-mode)

(defconst java-style
  '((c-basic-offset . 4)
    (tab-width . 4)
    (c-offsets-alist . ((statement-cont . (max c-lineup-cascaded-calls
                                               (min c-lineup-multi-inher c-lineup-java-inher)))))))
(with-eval-after-load 'eglot
  (require 'init-eglot-java))

(defun my-java-hook()
  (eglot-ensure)
  (google-set-c-style-with-offset 4)
  (c-toggle-auto-newline -1)
  (setq fill-column 100)
  (local-set-key (kbd "C-c j n") #'eglot-java-file-new)
  (local-set-key (kbd "C-c j r") #'eglot-java-run-main)
  (local-set-key (kbd "C-c j t") #'eglot-java-run-test)
  (local-set-key (kbd "C-c j N") #'eglot-java-project-new)
  (local-set-key (kbd "C-c j T") #'eglot-java-project-build-task)
  (local-set-key (kbd "C-c j R") #'eglot-java-project-build-refresh))

(add-hook 'java-mode-hook #'my-java-hook)

(provide 'init-java)
;;; init-java ends here
