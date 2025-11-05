;; -*- lexical-binding: t; -*-

(use-package scala-ts-mode)

(with-eval-after-load 'eglot
  (defclass metals-eglot-lsp-server (eglot-lsp-server) nil
    :documentation "Eglot LSP server subclass for metals.")

  (cl-defmethod eglot-initialization-options ((server metals-eglot-lsp-server))
    `(:decorationProvider t
      :inlineDecorationProvider t
      :enableSemanticHighlighting  nil
      :autoImportBuild "all"))

  (defun metals-eglot-lsp-server-connect (interactive)
    (cons 'metals-eglot-lsp-server (list "metals")))

  (defun metals-eglot-ignore-requests(actions)
    (member "Open doctor." (mapcar (lambda (obj) (plist-get obj :title)) actions)))

  (cl-defmethod eglot-handle-request
    ((server metals-eglot-lsp-server) (_method (eql window/showMessageRequest)) &key type message actions &allow-other-keys)
    "Handle server request window/showMessageRequest."
    (unless (metals-eglot-ignore-requests actions)
      (let* ((actions (append actions nil)) ;; gh#627
             (label (completing-read
                     (concat
                      (format (propertize "[eglot] Server reports (type=%s): %s"
                                          'face (if (or (not type) (<= type 1)) 'error))
                              type message)
                      "\nChoose an option: ")
                     (or (mapcar (lambda (obj) (plist-get obj :title)) actions)
                         '("OK"))
                     nil t (plist-get (elt actions 0) :title))))
        (if label `(:title ,label) :null))))

  (add-to-list 'eglot-server-programs '((scala-mode scala-ts-mode) .  metals-eglot-lsp-server-connect))
  ;; (add-to-list 'eglot-server-programs '((scala-mode scala-ts-mode) .  ("metals"
  ;;                                                                     :initializationOptions
  ;;                                                                     (:decorationProvider t
  ;;                                                                      :inlineDecorationProvider t
  ;;                                                                     )
  ;;                                                                     )))
  )

(defun my-scala-mode-hook()
  (yas-activate-extra-mode 'scala-mode)
  (setq-local tab-width 2)
  (eglot-ensure)
  ;; (let ((ext (file-name-extension (buffer-file-name))))
  ;;   (cond
  ;;    ((string= ext "sbt") (yas-activate-extra-mode 'maven-pom-mode))))
  )

(add-hook 'scala-ts-mode-hook #'my-scala-mode-hook)
(add-hook 'scala-mode-hook #'my-scala-mode-hook)

(defun sbt-shell()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (let ((target-window (split-window-vertically (floor (* 0.8 (window-height))))))
      (select-window target-window)
      (set-window-buffer target-window (sbt-start)))))

;; sbt-mode
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(provide 'init-scala)
