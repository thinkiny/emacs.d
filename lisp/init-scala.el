(use-package scala-mode :mode "\\.s\\(cala\\|bt\\|c\\)$")
(use-package lsp-metals
  :config
  (setq lsp-metals-sbt-script (expand-file-name "~/.emacs.d/third-parties/sbt")
        lsp-metals-show-implicit-arguments t
        lsp-metals-show-inferred-type t
        lsp-metals-show-implicit-conversions-and-classes t)
  :custom
  (lsp-metals-server-args '("-J-Dmetals.showInferredType=on" "-J-Dmetals.showImplicitArguments=on" "-J-Dmetals.showImplicitConversionsAndClasses=on"))
  :hook (scala-mode . my-scala-mode-hook))

;; scala3
(defun scala3-project-p ()
  (projectile-with-default-dir (projectile-project-root)
    (file-exists-p ".scala3")))

(setq scala3-indent-keywords (list
                              "def"
                              "if"
                              "object"
                              "class"
                              "trait"
                              "enum"
                              "while"))

(defun scala3-indent-line ()
  (interactive "P")
  (if (and abbrev-mode
           (eq (char-syntax (preceding-char)) ?w))
      (expand-abbrev))
  (let ((start-column (current-column))
        last-word
        indent)
    (save-excursion
      (beginning-of-line)
      (if (re-search-backward "^[^\n]" nil t)
          (let ((end (save-excursion (forward-line 1) (point))))
            (skip-chars-forward " \t" end)
            (setq last-word (current-word))
            (or (= (point) end) (setq indent (current-column))))))
    (cond (indent
           (let ((opoint (point-marker)))
             (if (member last-word scala3-indent-keywords)
                 (indent-to (+ indent 2))
               (indent-to indent))
             (if (> opoint (point))
                 (goto-char opoint))
             (move-marker opoint nil)))
          (t nil))))

(defun set-scala3-indent ()
  (when (scala3-project-p)
    (setq-local indent-line-function 'scala3-indent-line)))

(defun my-scala-mode-hook()
  (lsp-later)
  (lsp-lens-mode)
  (set-scala3-indent)
  (setq-local tab-width 2)
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
