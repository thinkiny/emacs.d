(require-package 'eglot-java)
(require 'eglot-java)

(setq eglot-java-server-install-dir (concat user-emacs-directory "java/eclipse.jdt.ls"))
(setq eglot-java-eclipse-jdt-cache-directory "~/.emacs.d/eglot-eclipse-jdt-cache")
(setq eglot-java-eclipse-jdt-args
      `("-Xms100m"
        "-Dsun.zip.disableMemoryMapping=true"
        "--add-modules=ALL-SYSTEM"
        "--add-opens=java.base/java.util=ALL-UNNAMED"
        "--add-opens=java.base/java.lang=ALL-UNNAMED"
        ,(concat "-javaagent:" (expand-file-name "~/.emacs.d/java/lombok.jar"))))

;; jdecomp
(use-package jdecomp
  :commands (jdecomp-mode)
  :config
  (setq jdecomp-decompiler-type 'fernflower
        jdecomp-decompiler-paths `((fernflower . ,(file-name-concat eglot-java-server-install-dir "java" "bundles" "dg.jdt.ls.decompiler.fernflower-0.0.3.jar")))
        jdecomp-decompiler-options '((fernflower "-hes=0" "-hdc=0" "-fdi=0"))))

(provide 'init-eglot-java)
