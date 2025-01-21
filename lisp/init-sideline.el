(use-package sideline
  :demand t
  :config
  ;;(setq sideline-backends-left '((sideline-blame . down)))
  (setq sideline-backends-right '((sideline-flymake . down)))
  :hook (prog-mode . sideline-mode))

;; (with-eval-after-load 'sideline
;;   (defun sideline-truncate-candidate (text)
;;     (let ((max-width (window-width)))
;;       (if (> (string-width text) max-width)
;;           (substring text 0 (- max-width 3))
;;         text)))

;;   (defun advice/filter-sideline--create-ov(args)
;;     (setf (nth 1 args) (sideline-truncate-candidate (nth 1 args)))
;;     args)

;;   (advice-add 'sideline--create-ov :filter-args #'advice/filter-sideline--create-ov))

(use-package sideline-flymake
  :config
  (setq sideline-flymake-display-mode 'line)
  :hook (flymake-mode . sideline-mode))

(use-package sideline-blame
  :after sideline)

(defun enable-sideline-blame()
  (interactive)
  (setq sideline-backends-left '((sideline-blame . down))))

(defun disable-sideline-blame()
  (interactive)
  (setq sideline-backends-left nil))

(provide 'init-sideline)
