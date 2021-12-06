;;(require 'lsp-java-boot)
;;(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

(require-package 'java-snippets)
(require-package 'groovy-mode)
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))

(use-package android-mode)

(use-package lsp-java
  :after lsp-mode)

(with-eval-after-load 'lsp-java
  (require 'init-pom)
  (require 'dap-java)

  (setq lsp-java-import-maven-enabled t
        lsp-java-maven-download-sources t
        lsp-java-inhibit-message t
        lsp-java-decompiler-fernflower-ind "    "
        lsp-java-decompiler-fernflower-ren t
        lsp-java-format-on-type-enabled nil
        lsp-java-decompiler-fernflower-dgs t
        lsp-java-content-provider-preferred "cfr"
        lsp-java-format-settings-url (lsp--path-to-uri "~/.emacs.d/java/formatter.xml")
        lsp-java-format-settings-profile "my-java")

  (add-to-list 'lsp-java-vmargs
               (concat "-javaagent:" (expand-file-name "~/.emacs.d/java/lombok.jar")))

  (lsp-register-custom-settings
   '(("java.decompiler.fernflower.ind" lsp-java-decompiler-fernflower-ind)
     ("java.decompiler.fernflower.ren" lsp-java-decompiler-fernflower-ren t)
     ("java.decompiler.fernflower.dgs" lsp-java-decompiler-fernflower-dgs t))))

(defconst java-style
  '((c-basic-offset . 4)
    (tab-width . 4)
    (c-offsets-alist . ((statement-cont . (max c-lineup-cascaded-calls
                                               (min c-lineup-multi-inher c-lineup-java-inher)))))))

(defun my-java-hook()
  (setq-local lsp-response-timeout 30)
  ;;(setq-local lsp-ui-sideline-show-code-actions nil)
  (lsp-later)
  (google-set-c-style-with-offset 4)
  (define-key java-mode-map (kbd "C-c a") 'lsp-java-add-import)
  (define-key java-mode-map (kbd "C-c t") 'lsp-java-add-throws)
  (define-key java-mode-map (kbd "C-c u") 'lsp-java-add-unimplemented-methods)
  (define-key java-mode-map (kbd "C-c o") 'lsp-java-generate-overrides)
  (define-key java-mode-map (kbd "C-c g") 'lsp-java-generate-getters-and-setters)
  (c-toggle-auto-newline -1)
  (setq fill-column 100))

(add-hook 'java-mode-hook #'my-java-hook)

(provide 'init-java)
;;; init-java ends here
