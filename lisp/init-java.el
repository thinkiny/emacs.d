;;(require 'lsp-java-boot)
;;(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

(require-package 'java-snippets)

(use-package lsp-java
  :after lsp-mode)

(after-load 'lsp-java
  (require 'init-pom)
  (require 'dap-java)

  (setq  lsp-java-import-maven-enabled t
         lsp-java-maven-download-sources t
         lsp-java-inhibit-message t
         lsp-java-decompiler-fernflower-ind "    "
         lsp-java-decompiler-fernflower-ren t
         lsp-java-format-on-type-enabled nil
         lsp-java-decompiler-fernflower-dgs t
         lsp-java-content-provider-preferred "fernflower"
         lsp-java-format-settings-url (lsp--path-to-uri "~/.emacs.d/java/formatter.xml")
         lsp-java-format-settings-profile "my-java")

  (setq lsp-java-vmargs `("-noverify"
                          "-Xss4m"
                          "-Xmx4G"
                          "-XX:+UseG1GC"
                          "-XX:+UseStringDeduplication"
                          "-XX:+AggressiveOpts"
                          "-DinitializingOptions="
                          ,(concat "-javaagent:" (expand-file-name "~/.emacs.d/java/lombok.jar"))))

  (lsp-register-custom-settings
   '(("java.decompiler.fernflower.ind" lsp-java-decompiler-fernflower-ind)
     ("java.decompiler.fernflower.ren" lsp-java-decompiler-fernflower-ren t)
     ("java.decompiler.fernflower.dgs" lsp-java-decompiler-fernflower-dgs t))))

(defconst java-style
  '((c-basic-offset . 4)
    (tab-width . 4)
    (c-offsets-alist . ((statement-cont . (max c-lineup-cascaded-calls
                                               (min c-lineup-multi-inher c-lineup-java-inher)))))))
(add-hook 'java-mode-hook (lambda ()
                            (lsp)
                            (setq-local lsp-ui-sideline-show-code-actions nil)
                            (c-add-style "java-style" java-style t)
                            (define-key java-mode-map (kbd "C-c a") 'lsp-java-add-import)
                            (define-key java-mode-map (kbd "C-c t") 'lsp-java-add-throws)
                            (define-key java-mode-map (kbd "C-c u") 'lsp-java-add-unimplemented-methods)
                            (define-key java-mode-map (kbd "C-c o") 'lsp-java-generate-overrides)
                            (define-key java-mode-map (kbd "C-c g") 'lsp-java-generate-getters-and-setters)
                            (c-toggle-auto-newline -1)
                            (setq fill-column 100)))

(provide 'init-java)
;;; init-java ends here
