(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "third-parties" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-utils)
(require 'init-package)
(require 'init-performance)
(require 'init-exec-path)

(require 'init-editing-utils)
(require 'init-xref)
(require 'init-treemacs)
(require 'init-platform)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-recentf)
(require 'init-projectile)
(require 'init-ivy)
(require 'init-ui)
(require 'init-company)
(require 'init-windows)
(require 'init-yasnippet)
(require 'init-dired)
(require 'init-whitespace)
(require 'init-tramp)

;;language
(require 'init-flycheck)
(require 'init-lisp)
(require 'init-lsp)
(require 'init-gtags)
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

;;file
(require 'init-markdown)
(require 'init-toml)
(require 'init-yaml)
(when window-system
  (require 'init-pdf))

;;other
(require 'init-term)
(require 'init-eshell)
(require 'init-rpm)
(require 'init-git)
(require 'init-http)
(require 'init-xwidget-webkit)
(require 'init-dash)
(require 'init-god)

(when (file-exists-p custom-file)
  (load custom-file))
(provide 'init)
