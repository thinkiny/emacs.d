(when *use-helm*
  (use-package helm-dash
    :config
    (setq dash-docs-enable-debugging nil)
    ;;(setq dash-docs-browser-func #'eww-browse-url)
    (setq dash-docs-browser-func #'xwidget-webkit-browse-url)
    (setq dash-docs-min-length 3)
    :bind (:map global-map
                ("C-c d"  . helm-dash)
                ("C-." . helm-dash-at-point))))

(add-hook 'c++-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("C++" "C" "Boost" "Man_Pages" "Linux"))))

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

(add-hook 'cmake-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("CMake"))))

(add-hook 'sh-mode-hook
          (lambda ()
            (setq-local dash-docs-docsets '("Bash" "Linux"))))

(provide 'init-dash)
