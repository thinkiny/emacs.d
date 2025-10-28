(require-package 'go-mode)

;; project
(require 'project)

(defun project-find-go-module (dir)
  (when-let* ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

;; (add-hook 'project-find-functions #'project-find-go-module)

;; (defun lsp-enable-go-bazel()
;;   (interactive)
;;   (setq-local lsp-go-env (make-hash-table))
;;   ;; (puthash "GOPACKAGESDRIVER_BAZEL_TARGETS" "//app/..." lsp-go-env)
;;   ;; (puthash "GOPACKAGESDRIVER_BAZEL_QUERY" "kind(go_library, //app/...)" lsp-go-env)
;;   (puthash "GOPACKAGESDRIVER" (concat (projectile-project-root) "/gopackagesdriver.sh") lsp-go-env)
;;   (my-lsp-workspace-restart))

(defun go-insert-generate-tag (name)
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (if (string-match "^\\s-+\\(\\S-+\\)\\s-+\\S-+$" line)
        (let ((json-tag (format "  `%s:\"%s%s\"`"
                                name
                                (downcase (substring (match-string 1 line) 0 1))
                                (substring (match-string 1 line) 1))))
          (end-of-line)
          (insert json-tag)))))

(defun go-generate-tag (name)
  (save-excursion
    (let ((start (re-search-backward "{$" nil t)))
      (forward-line 1)
      (while (not (= (char-after (line-beginning-position)) ?}))
        (go-insert-generate-tag name)
        (forward-line 1)))))

(defun go-generate-tag-json()
  (interactive)
  (go-generate-tag "json"))

(defun go-generate-tag-form()
  (interactive)
  (go-generate-tag "form"))


(defun my-go-mode-hook()
  (setq eglot-workspace-configuration `((:gopls . ((staticcheck . t)
                                                   (analyses . ((ST1003 . :json-false)))))))

  (eglot-ensure)
  (subword-mode)

  (define-key go-mode-map (kbd "C-c g j") #'go-generate-tag-json)
  (define-key go-mode-map (kbd "C-c g f") #'go-generate-tag-form))

(with-eval-after-load 'go-ts-mode
  (setq go-ts-mode-indent-offset 4))

(add-hook 'go-mode-hook #'my-go-mode-hook)
(add-hook 'go-ts-mode-hook #'my-go-mode-hook)

(provide 'init-golang)
