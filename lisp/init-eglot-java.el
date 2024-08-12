(require-package 'eglot-java)
(require 'eglot-java)

(setq eglot-java-server-install-dir (concat user-emacs-directory "java/eclipse.jdt.ls"))
(setq eglot-java-eclipse-jdt-cache-directory "~/.emacs.d/eglot-eclipse-jdt-cache")
(setq eglot-java-eclipse-jdt-args
      `("-XX:+UseG1GC"
        "-Xms100m"
        "-XX:MaxGCPauseMillis=50"
        "-Dsun.zip.disableMemoryMapping=true"
        "--add-modules=ALL-SYSTEM"
        "--add-opens=java.base/java.util=ALL-UNNAMED"
        "--add-opens=java.base/java.lang=ALL-UNNAMED"
        ,(concat "-javaagent:" (expand-file-name "~/.emacs.d/java/lombok.jar"))))

;; ----------------------- Support jdt.ls extra commands -----------------------
(defun java-apply-workspaceEdit (arguments)
  "Command `java.apply.workspaceEdit' handler."
  (mapc #'eglot--apply-workspace-edit arguments))

(defun java-action-overrideMethodsPrompt (arguments)
  "Command `java.action.overrideMethodsPrompt' handler."
  (let* ((argument (aref arguments 0))
         (list-methods-result (jsonrpc-request (eglot--current-server-or-lose)
                                               :java/listOverridableMethods
                                               argument))
         (methods (plist-get list-methods-result :methods))
         (menu-items (mapcar (lambda (method)
                               (let* ((name (plist-get method :name))
                                      (parameters (plist-get method :parameters))
                                      (class (plist-get method :declaringClass)))
                                 (cons (format "%s(%s) class: %s" name (string-join parameters ", ") class) method)))
                             methods))
         (selected-methods (cl-map 'vector
                                   (lambda (choice) (alist-get choice menu-items nil nil 'equal))
                                   (delete-dups
                                    (completing-read-multiple "overridable methods: " menu-items))))
         (add-methods-result (jsonrpc-request (eglot--current-server-or-lose)
                                              :java/addOverridableMethods
                                              (list :overridableMethods selected-methods :context argument))))
    (eglot--apply-workspace-edit add-methods-result)))

(defun +java/execute-command (server _command)
  (eglot--dbind ((Command) command arguments) _command
    (pcase command
      ("java.apply.workspaceEdit" (java-apply-workspaceEdit arguments))
      ("java.action.overrideMethodsPrompt" (java-action-overrideMethodsPrompt arguments))
      (_ (eglot--request server :workspace/executeCommand _command)))))

(defun +java/eglot-execute (server action)
  "Ask SERVER to execute ACTION.
ACTION is an LSP object of either `CodeAction' or `Command' type."
  (eglot--dcase action
    (((Command)) (+java/execute-command server action))
    (((CodeAction) edit command)
     (when edit (eglot--apply-workspace-edit edit))
     (when command (+java/execute-command server command)))))

(cl-defmethod eglot-execute (server action &context (major-mode java-mode))
  (+java/eglot-execute server action))

(cl-defmethod eglot-execute (server action &context (major-mode java-ts-mode))
  (+java/eglot-execute server action))

;; jdecomp
(use-package jdecomp
  :commands (jdecomp-mode)
  :config
  (setq jdecomp-decompiler-type 'fernflower
        jdecomp-decompiler-paths `((fernflower . ,(file-name-concat eglot-java-server-install-dir "java" "bundles" "dg.jdt.ls.decompiler.fernflower-0.0.3.jar")))
        jdecomp-decompiler-options '((fernflower "-hes=0" "-hdc=0" "-fdi=0"))))

(provide 'init-eglot-java)
