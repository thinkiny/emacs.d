(use-package nov
  :config
  (add-hook 'nov-mode-hook 'my-nov-mode-hook))

(defun nov-goto-next-line-or-page(&optional rest)
  (interactive)
  (if (>= (window-end) (point-max))
      (nov-next-document)
    (pixel-scroll-forward-line)))

(defun nov-goto-previous-line-or-page(&optional rest)
  (interactive)
  (if (and (<= (window-start) (point-min))
           (> nov-documents-index 0))
      (progn
        (nov-previous-document)
        (goto-char (point-max)))
    (pixel-scroll-backward-line)))

(defun setup-nov()
  (define-key nov-mode-map (kbd "N") 'nov-next-document)
  (define-key nov-mode-map (kbd "P") 'nov-previous-document)
  (define-key nov-mode-map (kbd "n") 'nov-goto-next-line-or-page)
  (define-key nov-mode-map (kbd "p") 'nov-goto-previous-line-or-page)
  (define-key nov-mode-map (kbd "w") 'nov-goto-next-line-or-page)
  (define-key nov-mode-map (kbd "s") 'nov-goto-previous-line-or-page)
  (define-key nov-mode-map (kbd "o") 'nov-goto-toc)
  (define-key nov-mode-map (kbd "f") 'forward-char)
  (define-key nov-mode-map (kbd "b") 'backward-char)
  (define-key nov-mode-map (kbd "e") 'end-of-line)
  (define-key nov-mode-map (kbd "a") 'beginning-of-line)
  (define-key nov-mode-map (kbd "l") 'forward-char)
  (define-key nov-mode-map (kbd "M-[") 'nov-history-back)
  (define-key nov-mode-map (kbd "M-]") 'nov-history-forward)
  (define-key nov-mode-map (kbd "h") 'backward-char)
  (define-key nov-mode-map (kbd "0") 'beginning-of-line)
  (define-key nov-mode-map (kbd "$") 'end-of-line)
  (define-key nov-mode-map (kbd "j") 'nov-goto-next-line-or-page)
  (define-key nov-mode-map (kbd "k") 'nov-goto-previous-line-or-page)
  (define-key nov-mode-map (kbd "<down>") 'nov-goto-next-line-or-page)
  (define-key nov-mode-map (kbd "<up>") 'nov-goto-previous-line-or-page)
  (define-key nov-mode-map (kbd "=") 'er/expand-region)
  (define-key nov-mode-map (kbd ",") 'bing-dict-at-point)
  (define-key nov-mode-map (kbd "<double-mouse-1>") #'bing-dict-at-point))

(defun my-nov-mode-hook()
  (setq-local mwheel-scroll-up-function
              #'nov-goto-next-line-or-page)
  (if (boundp 'mwheel-scroll-down-function)
      (setq-local mwheel-scroll-down-function
                  #'nov-goto-previous-line-or-page))
  (visual-line-mode)
  (setup-nov))


(provide 'init-nov)
