(require-package 'eglot-java)
(require 'eglot-java)
(setq eglot-java-server-install-dir (concat user-emacs-directory "java/eclipse.jdt.ls"))

(defun eglot-java-jdt-make-jvm-arg (arg)
  (concat "--jvm-arg=" arg))

(setq eglot-java-jdt-args
      (mapcar #'eglot-java-jdt-make-jvm-arg
              `("-XX:+UseG1GC"
                "-Xms100m"
                "-XX:MaxGCPauseMillis=50"
                "-Dsun.zip.disableMemoryMapping=true"
                "--add-modules=ALL-SYSTEM"
                "--add-opens=java.base/java.util=ALL-UNNAMED"
                "--add-opens=java.base/java.lang=ALL-UNNAMED"
                ,(concat "-javaagent:" (expand-file-name "~/.emacs.d/java/lombok.jar")))))

;; ----------------------- Support URI jdt:// protocol -----------------------
(defun +eglot/jdtls-uri-to-path (uri)
  "Support Eclipse jdtls `jdt://' uri scheme."
  (when-let* ((jdt-scheme-p (string-prefix-p "jdt://" uri))
              (filename (when (string-match "^jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
                          (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))
              (source-dir (file-name-concat (project-root (eglot--current-project)) ".eglot"))
              (source-file (expand-file-name (file-name-concat source-dir filename))))
    (unless (file-directory-p source-dir)
      (make-directory source-dir t))
    (unless (file-readable-p source-file)
      (let ((content (jsonrpc-request (eglot--current-server-or-lose)
                                      :java/classFileContents
                                      (list :uri uri))))
        (with-temp-file source-file (insert content))))
    (puthash source-file uri eglot-path-uri-cache)
    source-file))

(cl-defmethod +eglot/ext-uri-to-path (uri &context (major-mode java-mode))
  (+eglot/jdtls-uri-to-path uri))

(cl-defmethod +eglot/ext-uri-to-path (uri &context (major-mode java-ts-mode))
  (+eglot/jdtls-uri-to-path uri))

(defun eglot-java-workspace-dir ()
  (let ((workspace (expand-file-name (md5 (project-root (eglot--current-project)))
                                     (expand-file-name "~/.emacs.d/eglot-eclipse-jdt-cache"))))
    (unless (file-directory-p workspace)
      (make-directory workspace t))
    workspace))

(add-to-list 'eglot-server-programs '((java-mode java-ts-mode) . eglot--eclipse-jdt-contact))
(defun eglot--eclipse-jdt-contact (interactive)
  "Return cons (CLASS . ARGS) for connecting to Eclipse JDT.
If INTERACTIVE, prompt user for details."
  (cons 'eglot-eclipse-jdt
        (nconc
         (list "jdtls"
               "-data" (eglot-java-workspace-dir))
         eglot-java-jdt-args)))

(defclass eglot-eclipse-jdt (eglot-lsp-server) ()
  :documentation "Eclipse's Java Development Tools Language Server.")

(cl-defmethod eglot-initialization-options ((server eglot-eclipse-jdt))
  "Passes through required JDT initialization options."
  `(:workspaceFolders
    [,@(cl-delete-duplicates
        (mapcar #'eglot--path-to-uri
                (let* ((root (project-root (eglot--project server))))
                  (cons root
                        (mapcar
                         #'file-name-directory
                         (append
                          (file-expand-wildcards (concat root "*/pom.xml"))
                          (file-expand-wildcards (concat root "*/.git"))
                          (file-expand-wildcards (concat root "*/build.gradle"))
                          (file-expand-wildcards (concat root "*/.project")))))))
        :test #'string=)]
    :settings (:import (:gradle (:enabled t)
                                :wrapper (:enabled t)))
    :extendedClientCapabilities ( :classFileContentsSupport t
                                  :overrideMethodsPromptSupport t
                                  :advancedOrganizeImportsSupport t
                                  :generateConstructorsPromptSupport t
                                  :advancedGenerateAccessorsSupport t
                                  :advancedExtractRefactoringSupport t
                                  :moveRefactoringSupport t
                                  :resolveAdditionalTextEditsSupport t )
    :format (:settings (:url ,(expand-file-name "~/.emacs.d/java/formatter.xml")
                             :profile "my-java"))
    :maven (:downloadSources t)
    ;; :autobuild (:enabled t)
    ;; https://github.com/dgileadi/vscode-java-decompiler
    :bundles ,(let ((bundles-dir (concat eglot-java-server-install-dir "/bundles" ))
                    jdtls-bundles)
                (->> (when (file-directory-p bundles-dir)
                       (directory-files bundles-dir t "\\.jar$"))
                     (append jdtls-bundles)
                     (apply #'vector)))
    :contentProvider (:preferred "fernflower")
    :decompiler (:fernflower
                 (:ind "    "
                       :ren t
                       :dgs t))))

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
