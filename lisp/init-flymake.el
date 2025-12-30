;; -*- lexical-binding: t; -*-

(add-to-list 'display-buffer-alist
               `(,(rx bos "*Flymake diagnostics")
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.30)))

(with-eval-after-load 'flymake
  ;; (setq flymake-show-diagnostics-at-end-of-line t)
  ;; (setq flymake-indicator-type 'auto)
  ;; (setq flymake-fringe-indicator-position 'left-fringe)
  ;; (setq flymake-margin-indicator-position nil)
  (setq flymake-no-changes-timeout 1)
  (setq flymake-mode-line-counter-format
  '(":["
    flymake-mode-line-error-counter
    flymake-mode-line-warning-counter
    flymake-mode-line-note-counter "]"))
  (easy-menu-define nil flymake-mode-map nil
    (list "Flymake" :visible nil)))

(defun clear-flymake-from-current-line(start stop pre-change-len)
  ;;(mapc #'flymake--delete-overlay (flymake--really-all-overlays))
  (when flymake-show-diagnostics-at-end-of-line
    (when-let* ((start (line-end-position))
                (end (min (1+ start) (point-max)))
                (eolovs (cl-remove-if-not
                         (lambda (o) (overlay-get o 'flymake-overlay))
                         (overlays-in start end))))
      (dolist (ov eolovs)
        (flymake--delete-overlay ov)))))

(defun my-flymake-mode-hook()
  (remove-hook 'eldoc-documentation-functions #'flymake-eldoc-function t)
  ;;(remove-hook 'after-change-functions 'flymake-after-change-function t)
  ;;(add-hook 'after-change-functions #'clear-flymake-from-current-line t)
  (define-key flymake-mode-map (kbd "C-c e l") #'flymake-show-buffer-diagnostics)
  (define-key flymake-mode-map (kbd "C-c e p") #'flymake-show-project-diagnostics)
  (define-key flymake-mode-map (kbd "C-c e n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c e b") #'flymake-goto-prev-error))

(add-hook 'flymake-mode-hook #'my-flymake-mode-hook)
(add-hook 'prog-mode-hook #'flymake-mode)

(provide 'init-flymake)
