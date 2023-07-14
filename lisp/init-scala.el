(require 'scala-ts-mode)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((scala-mode scala-ts-mode) . ("metals"))))

(defun my-scala-mode-hook()
  (eglot-ensure)
  (setq-local tab-width 2)
  (let ((ext (file-name-extension buffer-file-name)))
    (cond
     ((string= ext "sbt") (yas-activate-extra-mode 'maven-pom-mode)))))

(add-hook 'scala-ts-mode-hook #'my-scala-mode-hook)

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
