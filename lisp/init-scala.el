(require 'scala-ts-mode)

(use-package lsp-metals
  :config
  (setq lsp-metals-sbt-script (expand-file-name "~/.emacs.d/third-parties/sbt")
        ;; lsp-metals-show-implicit-arguments t
        ;; lsp-metals-show-inferred-type t
        ;; lsp-metals-show-implicit-conversions-and-classes t
        )
  ;; :custom
  ;; (lsp-metals-server-args '("-J-Dmetals.showInferredType=on" "-J-Dmetals.showImplicitArguments=on" "-J-Dmetals.showImplicitConversionsAndClasses=on"))
  :hook (scala-ts-mode . my-scala-mode-hook))

(defun my-scala-mode-hook()
  (lsp-later)
  (yas-activate-extra-mode 'scala-mode)
  (setq-local tab-width 2)
  (let ((ext (file-name-extension buffer-file-name)))
    (cond
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
