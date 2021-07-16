(require-package 'cff)
(require-package 'cmake-mode)

(use-package bazel :demand t)
;;(use-package bazel-mode
;;  :mode "\\.BUILD$"
;;  :config
;;  (add-hook 'bazel-mode-hook (lambda ()
;;                              (setq-local yas-indent-line 'fixed))))

;; use clangd
(with-eval-after-load 'lsp-clangd
  ;;(require 'dap-cpptools)
  (setq lsp-clients-clangd-args '("--header-insertion-decorators=0" "--log=error" "--header-insertion=never"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection-fast 'lsp-clients--clangd-command)
                    :major-modes '(c-mode c++-mode objc-mode)
                    :server-id 'clangd-remote
                    :remote? t))

  (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql clangd-remote)))
    "Extract a representative line from clangd's CONTENTS, to show in the echo area.
This function tries to extract the type signature from CONTENTS,
or the first line if it cannot do so. A single line is always
returned to avoid that the echo area grows uncomfortably."
    (with-temp-buffer
      (-let [value (lsp:markup-content-value contents)]
        (insert value)
        (goto-char (point-min))
        (if (re-search-forward (rx (seq "```cpp\n"
                                        (opt (group "//"
                                                    (zero-or-more nonl)
                                                    "\n"))
                                        (group
                                         (one-or-more
                                          (not (any "`")))
                                         "\n")
                                        "```")) nil t nil)
            (progn (narrow-to-region (match-beginning 2) (match-end 2))
                   (lsp--render-element (lsp-make-marked-string
                                         :language "cpp"
                                         :value (lsp-clangd-join-region (point-min) (point-max)))))
          (car (s-lines (lsp--render-element contents)))))))

  (cl-defmethod lsp-diagnostics-flycheck-error-explainer (e (_server-id (eql clangd-remote)))
    "Explain a `flycheck-error' E that was generated by the Clangd language server."
    (cond ((string-equal "clang-tidy" (flycheck-error-group e))
           (lsp-cpp-flycheck-clang-tidy-error-explainer e))
          (t (flycheck-error-message e)))))

(defun switch-c-header-source()
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (lsp-clangd-find-other-file)
    (cff-find-other-file)))

;; generate-compdb
(defun generate-compdb-bazel (root)
  (execute-command "bazel-compdb" (format "cd %s && bazel-compdb -s" root)))

(defun generate-compdb-cmake (root compdb)
  (execute-command
   (format "cd %s && cmake -DCMAKE_EXPORT_COMPILE_COMMAND=1 %s"
           (directory-file-name compdb) root)))

(defun generate-compdb-make (compdb)
  (execute-command
   (format "cd %s && bear make -j4" (directory-file-name compdb))))

(defun generate-compdb()
  (interactive)
  (when-let* ((root (projectile-project-root))
              (compdb (file-truename (format "%s/compile_commands.json" root)))
              (root-local (file-local-name root))
              (compdb-local (file-local-name compdb)))
    (cond
     ((file-exists-p (format "%s/WORKSPACE" root)) (generate-compdb-bazel root-local))
     ((file-exists-p (format "%s/CMakeLists.txt" root)) (generate-compdb-cmake root-local compdb-local))
     (t (generate-compdb-make compdb-local)))))

;; build
(defun build-bazel-project (root)
  (async-shell-command (format "cd %s && bazel build ... ; bazel-compdb -s" root)))

(defun build-make-project (compdb)
  (async-shell-command
   (format "cd %s && make -j4" (directory-file-name compdb))))

(defun build-c-project()
  (interactive)
  (when-let* ((root (projectile-project-root))
              (compdb (file-truename (format "%s/compile_commands.json" root)))
              (root-local (file-local-name root))
              (compdb-local (file-local-name compdb)))
    (if (file-exists-p (format "%s/WORKSPACE" root))
        (build-bazel-project root-local)
      (build-make-project compdb-local))))

;; styles
(require 'google-c-style)

;; hook
(defun my-c-mode-hook ()
  ;; ;;echo "" | g++ -v -x c++ -E -
  (c-add-style "Google" google-c-style t)
  (define-key c-mode-base-map (kbd "C-c x") 'switch-c-header-source)
  (define-key c-mode-base-map (kbd "C-c b g") 'generate-compdb)
  (define-key c-mode-base-map (kbd "C-c b b") 'build-c-project)
  (if (gtags-get-rootpath)
      (global-tags-exclusive-backend-mode)
    (lsp-later)))

(defun json-to-vector()
  (interactive)
  (if (region-active-p)
      (save-excursion
        (let* ((beg (region-beginning))
               (end (region-end))
               (str (buffer-substring-no-properties beg end)))
          (kill-region beg end)
          (insert (s-replace "]" "}" (s-replace "[" "{" str)))))))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tcc\\'" . c++-mode))
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

(provide 'init-cpp)
