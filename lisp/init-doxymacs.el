(defconst doxymacs-dir (expand-file-name "third-parties/doxymacs" user-emacs-directory))

(when (file-directory-p doxymacs-dir)
  (add-to-list 'load-path doxymacs-dir)
  (require 'doxymacs)
  (setq doxymacs-void-types "void Unit Void")
  (add-hook 'prog-mode-hook 'doxymacs-mode))

(provide 'init-doxymacs)
