(require-package 'cff)
(require-package 'cmake-mode)
(require-package 'disaster)

(use-package cuda-mode
  :hook (cuda-mode . eglot-ensure))

(use-package bazel :demand t)
;;(use-package bazel-mode
;;  :mode "\\.BUILD$"
;;  :config
;;  (add-hook 'bazel-mode-hook (lambda ()
;;                              (setq-local yas-indent-line 'fixed))))

(defun eglot-clangd-find-other-file (&optional new-window)
  (interactive)
  (let
      ((other-file (jsonrpc-request
                    (eglot--current-server-or-lose)
                    :textDocument/switchSourceHeader
                    (eglot--TextDocumentIdentifier))))
    (unless (s-present? other-file)
      (user-error "Could not find other file"))
    (funcall (if new-window #'find-file-other-window #'find-file)
             (eglot--uri-to-path other-file))))

(defun switch-cpp-header-source()
  (interactive)
  (if (bound-and-true-p eglot--cached-server)
      (eglot-clangd-find-other-file)
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

(defun build-cpp-project()
  (interactive)
  (when-let* ((root (projectile-project-root))
              (compdb (file-truename (format "%s/compile_commands.json" root)))
              (root-local (file-local-name root))
              (compdb-local (file-local-name compdb)))
    (if (file-exists-p (format "%s/WORKSPACE" root))
        (build-bazel-project root-local)
      (build-make-project compdb-local))))

(ignore-tramp-ssh-control-master #'generate-compdb #'build-c-project)

;; styles
(require 'google-c-style)
(defun google-set-c-style-with-offset (n)
  (interactive "noffset: ")
  (let* ((copy (copy-tree google-c-style))
         (new-style (mapcar (lambda (x)
              (if (eq (car x) 'c-basic-offset)
                  `(c-basic-offset . ,n)
                x)) copy)))
    (make-local-variable 'c-tab-always-indent)
    (setq c-tab-always-indent t)
    (c-add-style "Google" new-style t)))

;; hook
(defun my-cpp-mode-hook ()
  ;; ;;echo "" | g++ -v -x c++ -E -
  (c-add-style "Google" google-c-style t)
  (local-set-key (kbd "C-c x") 'switch-cpp-header-source)
  (local-set-key (kbd "C-c b g") 'generate-compdb)
  (local-set-key (kbd "C-c b b") 'build-cpp-project)
  (local-set-key (kbd "C-c a") 'disaster)
  (if (gtags-get-rootpath)
      (gtags-mode)
    (eglot-ensure)))

(defun json-to-vector()
  (interactive)
  (if (region-active-p)
      (save-excursion
        (let* ((beg (region-beginning))
               (end (region-end))
               (str (buffer-substring-no-properties beg end)))
          (kill-region beg end)
          (insert (s-replace "]" "}" (s-replace "[" "{" str)))))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((c-mode c++-mode) . ("clangd" "--header-insertion-decorators=0" "--log=error" "--clang-tidy" "--import-insertions" "--function-arg-placeholders"))))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tcc\\'" . c++-mode))
(add-hook 'c-mode-hook 'my-cpp-mode-hook)
(add-hook 'c++-mode-hook 'my-cpp-mode-hook)

(provide 'init-cpp)
