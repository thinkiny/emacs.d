(use-package counsel-dash
  :config
  (setq dash-docs-enable-debugging nil)
  ;;(setq dash-docs-browser-func #'eww-browse-url)
  (setq dash-docs-browser-func #'xwidget-webkit-browse-url)
  (setq dash-docs-candidate-format "%d %n (%t)")
  (setq dash-docs-min-length 3)
  :bind (:map global-map
              ("C-c d"  . #'counsel-dash)
              ("C-." . #'counsel-dash-at-current-point)))

(defun counsel-dash-at-current-point ()
  "Bring up a `counsel-dash' search interface with symbol at point."
  (interactive)
  (counsel-dash
   (save-excursion
     (if (re-search-backward "[{( ]" nil t)
         (let ((start (+ (match-beginning 0) 1)))
           (goto-char start)
           (re-search-forward "[ ()]" nil t)
           (buffer-substring start (match-beginning 0)))
       (substring-no-properties (or (thing-at-point 'symbol) ""))))))

(add-hook 'c++-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("C++" "C" "Boost" "Man_Pages" "Linux" "x86_64_asm"))))

(add-hook 'c-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("C" "Man_Pages" "Linux" "x86_64_asm"))))


(add-hook 'asm-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("x86_64_asm"))))

(add-hook 'scala-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("Scala" "Java_SE11"))))

(add-hook 'java-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("Java_SE11"))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("Emacs_Lisp"))))

(add-hook 'go-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("Go"))))

(add-hook 'python-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("Python_3" "Python_2"))))

(add-hook 'cmake-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("CMake"))))

(add-hook 'sh-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("Bash" "Linux"))))

(add-hook 'erlang-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("Erlang"))))

(provide 'init-dash)
