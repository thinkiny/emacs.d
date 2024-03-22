(use-package flutter)
(use-package flutter-l10n-flycheck)

(use-package dart-mode
  :hook (dart-mode . my-dart-mode-hook))

(defun flutter-maybe-hotload()
  (if (flutter--running-p)
      (flutter-hot-reload)))

(defun my-dart-mode-hook()
  (eglot-ensure)
  (add-hook 'after-save-hook 'flutter-maybe-hotload nil t))

(provide 'init-flutter)
