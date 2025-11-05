;; -*- lexical-binding: t; -*-

(use-package flutter
  :after dart-mode)

(use-package dart-mode
  :hook (dart-mode . my-dart-mode-hook))

(defun flutter-maybe-hotload()
  (if (flutter--running-p)
      (flutter-hot-reload)))

(defun flutter-maybe-restart()
  (interactive)
  (if (flutter--running-p)
      (flutter-hot-restart)
    (flutter-run)))

(defun my-dart-mode-hook()
  (eglot-ensure)
  (flutter-test-mode)
  (define-key dart-mode-map (kbd "C-c <RET>") #'flutter-maybe-restart)
  (add-hook 'after-save-hook 'flutter-maybe-hotload nil t))

(provide 'init-flutter)
