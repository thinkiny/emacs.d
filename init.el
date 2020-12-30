(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq inhibit-compacting-font-caches t)
(setq gc-cons-threshold (* 256 1024 1024))
(setq read-process-output-max (* 2048 1024))

(require 'init-package)
(require 'init-utils)
(require 'init-exec-path)

(require 'init-treemacs)
(require 'init-ui)
(require 'init-platform)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-recentf)

(require 'init-helm)
(require 'init-company)
(require 'init-windows)
(require 'init-editing-utils)

(require 'init-projectile)
(require 'init-yasnippet)
;;(require 'init-eshell)
(require 'init-mt)
(require 'init-rpm)
(require 'init-engine)
(require 'init-git)
(require 'init-http)
(require 'init-dired)
(require 'init-whitespace)
(require 'init-xwidget-webkit)
(require 'init-dash)
(require 'init-god)
(require 'init-tramp)

;;language
(require 'init-flycheck)
(require 'init-lisp)
(require 'init-lsp)
(require 'init-cpp)
(require 'init-python)
(require 'init-org)
(require 'init-lua)
(require 'init-golang)
(require 'init-html)
(require 'init-systemtap)
(require 'init-java)
(require 'init-scala)
(require 'init-rust)
(require 'init-sql)
(require 'init-asm)
(require 'init-run)

;; file
(require 'init-markdown)
(require 'init-toml)
(require 'init-yaml)
(when window-system
  (require 'init-pdf))

(when (file-exists-p custom-file)
  (load custom-file))
(provide 'init)
