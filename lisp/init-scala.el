(use-package scala-mode :mode "\\.s\\(cala\\|bt\\|c\\)$"
  :hook (scala-mode . my-scala-mode-hook))

;; scala3
(defun is-scala3-project ()
  (projectile-with-default-dir (projectile-project-root)
    (file-exists-p ".scala3")))

(defun disable-scala-indent ()
  (when (is-scala3-project)
    (setq-local indent-line-function 'indent-relative-maybe)))

(defun my-scala-mode-hook()
  (eglot-ensure)
  (disable-scala-indent)
  (let ((ext (file-name-extension buffer-file-name)))
    (cond
     ;;((string= ext "sc")  (setq-local lsp-enable-format-at-save nil))
     ((string= ext "sbt") (yas-activate-extra-mode 'maven-pom-mode)))))

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
