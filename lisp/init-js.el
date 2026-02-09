;; -*- lexical-binding: t; -*-

(require-package 'typescript-mode)

(setq ts-lsp-modes '(js-ts-mode tsx-ts-mode typescript-ts-mode))

(with-eval-after-load 'eglot
  ;; npm install -g typescript-language-server typescript
  ;; (set-eglot-server-program '((js-ts-mode :language-id "javascript")
  ;;                             (tsx-ts-mode :language-id "typescriptreact")
  ;;                             (typescript-ts-mode :language-id "typescript"))
  ;;                           `("typescript-language-server" "--stdio"
  ;;                             :initializationOptions
  ;;                             (:preferences
  ;;                              (:importModuleSpecifierPreference "project-relative"
  ;;                               :allowRenameOfImportPath t))))

  ;; npm install -g @vtsls/language-server
  (set-eglot-server-program
   '((js-ts-mode :language-id "javascript")
     (tsx-ts-mode :language-id "typescriptreact")
     (typescript-ts-mode :language-id "typescript"))
   `("vtsls" "--stdio"))
  )


;;; DVA Xref Backend
;; Navigate dva-style model references like 'namespace/action'

(defun dva-identifier-at-point ()
  "Return plist (:namespace NS :action ACTION :raw RAW) if in a dva string."
  (let ((ppss (syntax-ppss)))
    (when (nth 3 ppss)
      (save-excursion
        (goto-char (nth 8 ppss))
        (when (looking-at "['\"`]\\([a-zA-Z][a-zA-Z0-9_]*\\)/\\([a-zA-Z][a-zA-Z0-9_]*\\)['\"`]")
          (list :namespace (match-string-no-properties 1)
                :action (match-string-no-properties 2)
                :raw (match-string-no-properties 0)))))))


(defun dva-find-model-file (namespace)
  "Find model file for NAMESPACE in ./models/."
  (when buffer-file-name
    (let ((dir (expand-file-name "models" (file-name-directory buffer-file-name))))
      (cl-find-if #'file-exists-p
                  (mapcar (lambda (ext) (expand-file-name (concat namespace ext) dir))
                          '(".ts" ".tsx" ".js" ".jsx"))))))

(defun dva-find-function-in-file (file action)
  "Find ACTION in FILE. Returns (LINE . COL) or nil."
  (let* ((pattern (format "^\\s*(\\*?%s\\s*[(:=]|%s\\s*:)" action action))
         (output (shell-command-to-string
                  (format "rg -n --no-heading '%s' %s | head -1"
                          pattern (shell-quote-argument file)))))
    (when (string-match "^\\([0-9]+\\):\\(.*\\)" output)
      (cons (string-to-number (match-string 1 output))
            (or (string-match (regexp-quote action) (match-string 2 output)) 0)))))

(defun dva-xref-backend ()
  "Return 'dva for dva identifiers in JS/TS modes."
  (when (and (memq major-mode ts-lsp-modes)
             (or (dva-identifier-at-point) (thing-at-point 'symbol t)))
    'dva))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql dva)))
  "Return identifier with dva properties."
  (if-let* ((id (dva-identifier-at-point)))
      (propertize (plist-get id :raw)
                  :dva-namespace (plist-get id :namespace)
                  :dva-action (plist-get id :action))
    (when-let* ((sym (thing-at-point 'symbol t))
                (ns (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
      (propertize (format "%s/%s" ns sym) :dva-namespace ns :dva-action sym))))

(cl-defmethod xref-backend-definitions ((_backend (eql dva)) identifier)
  "Find definitions for dva IDENTIFIER."
  (let ((ns (get-text-property 0 :dva-namespace identifier))
        (act (get-text-property 0 :dva-action identifier)))
    (when-let* ((file (dva-find-model-file ns))
                (pos (or (dva-find-function-in-file file act) '(1 . 0))))
      (list (xref-make (format "%s/%s" ns act)
                       (xref-make-file-location file (car pos) (cdr pos)))))))

(cl-defmethod xref-backend-references ((_backend (eql dva)) identifier)
  "Find references for dva IDENTIFIER."
  (let* ((ns (get-text-property 0 :dva-namespace identifier))
         (act (get-text-property 0 :dva-action identifier))
         (pattern (format "%s/%s" (or ns "[a-zA-Z0-9_]+") act))
         (default-directory (or (project-root (project-current)) default-directory))
         (output (shell-command-to-string
                  (format "rg -n --no-heading -t js -t ts '%s' ." pattern))))
    (cl-loop for line in (split-string output "\n" t)
             when (string-match "^\\([^:]+\\):\\([0-9]+\\):\\(.*\\)$" line)
             collect (let* ((file (expand-file-name (match-string 1 line)))
                            (lnum (string-to-number (match-string 2 line)))
                            (content (match-string 3 line))
                            (col (or (string-match (regexp-quote pattern) content) 0)))
                       (xref-make content (xref-make-file-location file lnum col))))))

(defun my-js-ts-mode-hook()
  (setq-local tab-width 2)
  (eglot-ensure)
  (setq my/xref-fallback-backends '(dva-xref-backend)))

(dolist (hook (mapcar #'derived-mode-hook-name ts-lsp-modes))
  (add-hook hook 'my-js-ts-mode-hook))

(add-auto-mode 'json-ts-mode "\\.json")
(add-auto-mode 'js-ts-mode "\\.js\\'" "\\.mjs\\'" "\\.jsx\\'")
(add-auto-mode 'tsx-ts-mode "\\.tsx\\'")
(add-auto-mode 'typescript-ts-mode "\\.ts\\'")

(provide 'init-js)
