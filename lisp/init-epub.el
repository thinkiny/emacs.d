(use-package nov
  :mode (("\\.epub$" . nov-mode))
  :config
  (require 'nov-xwidget)
  (add-hook 'nov-mode-hook 'my-nov-mode-hook))

(defun nov-goto-next-line-or-page(&optional arg)
  (interactive)
  (if (eq (line-end-position) (point-max))
      (nov-next-document)
    (forward-line 1)))

(defun nov-goto-previous-line-or-page(&optional arg)
  (interactive)
  (if (eq (line-beginning-position) (point-min))
      (nov-previous-document)
    (forward-line -1)))

(defun my-nov-mode-hook()
  (if (boundp 'mwheel-scroll-up-function)
      (setq-local mwheel-scroll-up-function
                  #'nov-goto-next-line-or-page))
  (if (boundp 'mwheel-scroll-down-function)
      (setq-local mwheel-scroll-down-function
                  #'nov-goto-previous-line-or-page))
  (define-key nov-mode-map (kbd "N") 'nov-next-document)
  (define-key nov-mode-map (kbd "P") 'nov-previous-document)
  (define-key nov-mode-map (kbd "n") 'nov-goto-next-line-or-page)
  (define-key nov-mode-map (kbd "p") 'nov-goto-previous-line-or-page)
  (define-key nov-mode-map (kbd "o") 'nov-goto-toc))

(provide 'init-epub)
