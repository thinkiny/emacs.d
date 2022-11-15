;;(require 'lsp-java-boot)
;;(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

(require-package 'java-snippets)
(require-package 'groovy-mode)
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))

(use-package android-mode)

(use-package eglot-java
  :after eglot
  :config
  (setq eglot-java-server-install-dir (concat user-emacs-directory "java/eclipse.jdt.ls"))
  (setq eglot-java-eclipse-jdt-args
        `("-XX:+UseG1GC"
          "-XX:MaxGCPauseMillis=50"
          "-Dsun.zip.disableMemoryMapping=true"
          "-Xms100m"
          ,(concat "-javaagent:" (expand-file-name "~/.emacs.d/java/lombok.jar"))))
  )

(defconst java-style
  '((c-basic-offset . 4)
    (tab-width . 4)
    (c-offsets-alist . ((statement-cont . (max c-lineup-cascaded-calls
                                               (min c-lineup-multi-inher c-lineup-java-inher)))))))

(defun my-java-hook()
  (eglot-ensure)
  (google-set-c-style-with-offset 4)
  (define-key eglot-java-mode-map (kbd "C-c j n") #'eglot-java-file-new)
  (define-key eglot-java-mode-map (kbd "C-c j r") #'eglot-java-run-main)
  (define-key eglot-java-mode-map (kbd "C-c j t") #'eglot-java-run-test)
  (define-key eglot-java-mode-map (kbd "C-c j N") #'eglot-java-project-new)
  (define-key eglot-java-mode-map (kbd "C-c j T") #'eglot-java-project-build-task)
  (define-key eglot-java-mode-map (kbd "C-c j R") #'eglot-java-project-build-refresh)
  (c-toggle-auto-newline -1)
  (setq fill-column 100))

(add-hook 'java-mode-hook #'my-java-hook)

(provide 'init-java)
;;; init-java ends here
