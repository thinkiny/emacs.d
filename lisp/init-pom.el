;;; maven-pom-mode.el --- A major mode for pom files
;;; Commentary:

;; A major mode for pom files

(defgroup maven-pom-mode nil
  "Major mode for editting pom.xml files")

(add-to-list 'auto-mode-alist '("pom\\.xml\\'" . maven-pom-mode))
(add-to-list 'auto-mode-alist '("\\.pom\\'" . maven-pom-mode))

;; Define maven-pom mode.
(define-derived-mode maven-pom-mode nxml-mode
  "maven-pom-mode" "Major mode for editting Maven pom files {maven-pom-mode-map}"
  (use-local-map maven-pom-mode-map)
  (run-mode-hooks 'maven-pom-mode-hook)
  (local-unset-key (kbd "C-c ]"))
  (local-unset-key (kbd "C-c C-n"))
  (local-set-key (kbd "C-c u") 'eglot-java-project-build-refresh))

(provide 'init-pom)
;;; init-pom ends here
