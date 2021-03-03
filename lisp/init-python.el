(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(setq python-shell-interpreter "python2")

(require-package 'pip-requirements)
(require-package 'indent-guide)

;; use microsoft pyls
(use-package lsp-python-ms
  :demand t
  :config
  (defun lsp-python-ms--get-python-ver-and-syspath-remote (&optional workspace-root)
    "Return list with pyver-string and list of python search paths.

The WORKSPACE-ROOT will be prepended to the list of python search
paths and then the entire list will be json-encoded."
    (let* ((python (and t (lsp-python-ms-locate-python)))
           (workspace-root (and python (or workspace-root ".")))
           (default-directory (and workspace-root workspace-root))
           (init (and default-directory
                      "from __future__ import print_function; import sys; sys.path = list(filter(lambda p: p != '', sys.path)); import json;"))
           (ver (and init "v=(\"%s.%s\" % (sys.version_info[0], sys.version_info[1]));"))
           (sp (and ver (concat "sys.path.insert(0, '" workspace-root "'); p=sys.path;")))
           (ex (and sp "e=sys.executable;"))
           (val (and ex "print(json.dumps({\"version\":v,\"paths\":p,\"executable\":e}))")))
      (when val
        (with-temp-buffer
          (with-parsed-tramp-file-name python parsed
            (let ((default-directory (file-name-directory python)))
              (process-file parsed-localname nil t nil "-c"
                            (concat init ver sp ex val))))
          (let* ((json-array-type 'vector)
                 (json-key-type 'string)
                 (json-object-type 'hash-table)
                 (json-string (buffer-string))
                 (json-hash (json-read-from-string json-string)))
            (list
             (gethash "version" json-hash)
             (gethash "paths" json-hash)
             (gethash "executable" json-hash)))))))

  (defun lsp-python-ms--extra-init-params-remote (&optional workspace)
    "Return form describing parameters for language server.

Old lsp will pass in a WORKSPACE, new lsp has a global
lsp-workspace-root function that finds the current buffer's
workspace root.  If nothing works, default to the current file's
directory"
    (let ((workspace-root (if workspace (lsp--workspace-root workspace) (lsp-python-ms--workspace-root))))
      (when lsp-python-ms-parse-dot-env-enabled
        (lsp-python-ms--parse-dot-env workspace-root))
      (cl-destructuring-bind (pyver pysyspath pyintpath)
          (lsp-python-ms--get-python-ver-and-syspath-remote workspace-root)
        `(:interpreter
          (:properties
           (:InterpreterPath ,pyintpath :UseDefaultDatabase t :Version ,pyver))
          ;; preferredFormat "markdown" or "plaintext"
          ;; experiment to find what works best -- over here mostly plaintext
          :displayOptions (:preferredFormat
                           "markdown"
                           :trimDocumentationLines :json-false
                           :maxDocumentationLineLength 0
                           :trimDocumentationText :json-false
                           :maxDocumentationTextLength 0)
          :searchPaths ,(vconcat lsp-python-ms-extra-paths pysyspath)
          :analysisUpdates t
          :asyncStartup t
          :logLevel ,lsp-python-ms-log-level
          :typeStubSearchPaths ,(vector (expand-file-name (f-join lsp-python-ms-dir "Typeshed")))))))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "Microsoft.Python.LanguageServer")
    :major-modes (append '(python-mode) lsp-python-ms-extra-major-modes)
    :server-id 'mspyls-remote
    :remote? t
    :priority 1
    :initialization-options 'lsp-python-ms--extra-init-params-remote
    :notification-handlers (lsp-ht ("python/languageServerStarted" 'lsp-python-ms--language-server-started-callback)
                                   ("telemetry/event" 'ignore)
                                   ("python/reportProgress" 'lsp-python-ms--report-progress-callback)
                                   ("python/beginProgress" 'lsp-python-ms--begin-progress-callback)
                                   ("python/endProgress" 'lsp-python-ms--end-progress-callback))
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration (lsp-configuration-section "python")))))))

;; (after-load 'lsp-pyls
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-tramp-connection
;;                                    (lambda () lsp-clients-python-command))
;;                   :major-modes '(python-mode cython-mode)
;;                   :priority -1
;;                   :remote? t
;;                   :server-id 'pyls-remote
;;                   :library-folders-fn (lambda (_workspace) lsp-clients-python-library-directories)
;;                   :initialized-fn (lambda (workspace)
;;                                     (with-lsp-workspace workspace
;;                                       (lsp--set-configuration (lsp-configuration-section "pyls")))))))

(add-hook 'python-mode-hook (lambda ()
                              (setq-local lsp-enable-save-format nil)
                              (lsp-later)
                              (indent-guide-mode)))
(provide 'init-python)
