(use-package nov
  :mode (("\\.epub$" . nov-mode))
  :config
  (require 'nov-xwidget)
  (setq nov-step-size 10)
  (add-hook 'nov-mode-hook 'my-nov-mode-hook))

(defun nov-goto-next-line-or-page()
  (interactive)
  (if (eq (line-end-position) (point-max))
      (nov-next-document)
    (forward-line nov-step-size)))


(defun nov-goto-previous-line-or-page()
  (interactive)
  (if (eq (line-beginning-position) (point-min))
      (nov-previous-document)
    (forward-line (* -1 nov-step-size))))

(defun my-nov-mode-hook()
  (define-key nov-mode-map (kbd "N") 'nov-next-document)
  (define-key nov-mode-map (kbd "P") 'nov-previous-document)
  (define-key nov-mode-map (kbd "n") 'nov-goto-next-line-or-page)
  (define-key nov-mode-map (kbd "p") 'nov-goto-previous-line-or-page)
  (define-key nov-mode-map (kbd "o") 'nov-goto-toc))

(provide 'init-epub)
