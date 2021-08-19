(use-package scala-mode :mode "\\.s\\(cala\\|bt\\|c\\)$")
(use-package lsp-metals
  :custom
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))
  :hook (scala-mode . my-scala-mode-hook))

(defun my-scala-mode-hook()
  (lsp-later)
  (lsp-lens-mode)
  (let ((ext (file-name-extension buffer-file-name)))
    (cond
     ((string= ext "sc")  (setq-local lsp-enable-format-at-save nil))
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
