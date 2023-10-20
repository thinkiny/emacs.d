(require 'scala-ts-mode)


(with-eval-after-load 'lsp-bridge
  (push '((scala-ts-mode) . "metals") lsp-bridge-single-lang-server-mode-list)
  (push 'scala-ts-mode-hook lsp-bridge-default-mode-hooks))

(defun my-scala-mode-hook()
  (setq-local tab-width 2)
  (let ((ext (file-name-extension buffer-file-name)))
    (cond
     ((string= ext "sbt") (yas-activate-extra-mode 'maven-pom-mode)))))

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
