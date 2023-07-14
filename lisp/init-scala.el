(use-package scala-mode :mode "\\.s\\(cala\\|bt\\|c\\)$"
  :hook (scala-mode . my-scala-mode-hook))

;; scala3
(defun scala3-project-p ()
  (projectile-with-default-dir (projectile-project-root)
    (file-exists-p ".scala3")))

(setq scala3-indent-start-words '("def"
                                  "if"
                                  "object"
                                  "class"
                                  "trait"
                                  "enum"
                                  "while"
                                  "catch"
                                  "case"
                                  "try"))

(setq scala3-indent-end-words '("match"
                                "=>"))

(defun scala3-indent-line ()
  (interactive "P")
  (if (and abbrev-mode
           (eq (char-syntax (preceding-char)) ?w))
      (expand-abbrev))
  (let (start-word
        end-word
        indent)
    (save-excursion
      (beginning-of-line)
      (if (re-search-backward "^[^\n]" nil t)
          (let ((end (save-excursion (forward-line 1) (point))))
            (skip-chars-forward " \t" end)
            (setq start-word (current-word))
            (or (= (point) end) (setq indent (current-column))))))
    (save-excursion
      (forward-line 1)
      (end-of-line)
      (backward-char 1)
      (setq end-word (current-word)))
    (cond (indent
           (let ((opoint (point-marker)))
             (if (member start-word scala3-indent-start-words)
                 (setq indent (+ indent 2)))
             (if (member end-word scala3-indent-end-words)
                 (setq indent (+ indent 2)))
             (indent-to indent)
             (if (> opoint (point))
                 (goto-char opoint))
             (move-marker opoint nil)))
          (t (tab-to-tab-stop)))))

(defun set-scala3-indent ()
  (when (scala3-project-p)
    (message "enable scala3 indent")
    (setq-local indent-line-function 'scala3-indent-line)))

(defun my-scala-mode-hook()
  (eglot-ensure)
  (set-scala3-indent)
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
