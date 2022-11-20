(require-package 'eglot-java)
(require 'eglot-java)
(setq eglot-java-server-install-dir (concat user-emacs-directory "java/eclipse.jdt.ls"))

(defun eglot-java-jdt-make-jvm-arg (arg)
  (concat "--jvm-arg=" arg))

(setq eglot-java-jdt-args
      (mapcar #'eglot-java-jdt-make-jvm-arg
              `("-XX:+UseG1GC"
                "-XX:MaxGCPauseMillis=50"
                "-Dsun.zip.disableMemoryMapping=true"
                "-Xms100m"
                ,(concat "-javaagent:" (expand-file-name "~/.emacs.d/java/lombok.jar")))))

(defun eglot-java-handle-uri (fn url)
  (if (and (stringp url) (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" url))
      (eglot-java--resolve-uri url)
    (funcall fn url)))

(defun eglot-java--ensure-dir (path)
  "Ensure that directory PATH exists."
  (unless (file-directory-p path)
    (make-directory path t)))

(defun eglot-java--get-metadata-location (file-location)
  "Given a FILE-LOCATION return the file containing the metadata for the file."
  (format "%s.%s.metadata"
          (file-name-directory file-location)
          (file-name-base file-location)))

(defun eglot-java--get-filename (url)
  "Get the name of the buffer calculating it based on URL."
  (or (save-match-data
        (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" url)
          (format "%s.java"
                  (replace-regexp-in-string "/" "." (match-string 2 url) t t))))
      (-when-let ((_ file-name _ jar)
                  (s-match
                   "jdt://.*?/\\(.*?\\)\\?=\\(.*?\\)/.*/\\(.*\\)"
                   (url-unhex-string url)))
        (format "%s(%s)" file-name
                (->> jar
                     (s-replace "/" "")
                     (s-replace "\\" ""))))
      (save-match-data
        (when (string-match "chelib://\\(.*\\)" url)
          (let ((matched (match-string 1 url)))
            (replace-regexp-in-string (regexp-quote ".jar") "jar" matched t t))))
      (error "Unable to match %s" url)))

(defun eglot-java--resolve-uri (uri)
  "Load a file corresponding to URI executing request to the jdt server."
  (let* ((buffer-name (eglot-java--get-filename uri))
         (file-location (concat (eglot-java-workspace-dir) "/jdt.ls-java-project/src/" buffer-name)))
    (unless (file-readable-p file-location)
      (eglot-java--ensure-dir (file-name-directory file-location))
      (let ((content (jsonrpc-request
                      (eglot--current-server-or-lose)
                      :java/classFileContents
                      (list :uri uri))))
        (with-temp-file file-location
          (insert content))
        (with-temp-file (eglot-java--get-metadata-location file-location)
          (insert uri))
        ))
    file-location))

(advice-add 'eglot--uri-to-path :around #'eglot-java-handle-uri)

(defun eglot-java-workspace-dir ()
  (let ((workspace (expand-file-name (md5 (project-root (eglot--current-project)))
                                     (expand-file-name "~/.emacs.d/eglot-eclipse-jdt-cache"))))
    (unless (file-directory-p workspace)
      (make-directory workspace t))
    workspace))

(add-to-list 'eglot-server-programs '(java-mode . eglot--eclipse-jdt-contact))
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
                          (file-expand-wildcards (concat root "*/build.gradle"))
                          (file-expand-wildcards (concat root "*/.project")))))))
        :test #'string=)]
    :settings (:import (:gradle (:enabled t)))
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

(provide 'init-eglot-java)
