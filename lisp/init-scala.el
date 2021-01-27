(use-package lsp-metals)

(use-package scala-mode
  :after lsp-mode
  :hook (scala-mode . my-scala-mode-hook)
  :mode "\\.s\\(cala\\|bt\\|c\\)$")

(defun my-scala-mode-hook()
  (lsp)
  (lsp-lens-mode 1)
  (let ((ext (file-name-extension buffer-file-name)))
    (cond
     ((string= ext "sc")  (setq-local lsp-skip-format-at-save t))
     ((string= ext "sbt") (yas-activate-extra-mode 'maven-pom-mode)))))

(defun sbt-shell()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (let ((target-window (split-window-vertically (floor (* 0.8 (window-height))))))
      (select-window target-window)
      (set-window-buffer target-window (sbt-start)))))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(setq lsp-metals-sbt-script (expand-file-name "~/.emacs.d/scala/sbt"))
(provide 'init-scala)
