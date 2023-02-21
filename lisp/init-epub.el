(use-package nov
  :mode (("\\.epub$" . nov-mode))
  :config
  (require 'nov-xwidget)
  (add-hook 'nov-mode-hook 'my-nov-mode-hook))

(defun modeline-nov-document-index()
  (format " %d/%d" nov-documents-index (length nov-documents)))

(defun nov-goto-next-line-or-page(&optional arg)
  (interactive)
  (unless (line-move 1 t)
    (nov-next-document)))

(defun nov-goto-previous-line-or-page(&optional arg)
  (interactive)
  (unless (line-move -1 t)
    (nov-previous-document)
    (goto-char (point-max))
    (beginning-of-line)))


(defun my-nov-mode-hook()
  (if (boundp 'mwheel-scroll-up-function)
      (setq-local mwheel-scroll-up-function
                  #'nov-goto-next-line-or-page))
  (if (boundp 'mwheel-scroll-down-function)
      (setq-local mwheel-scroll-down-function
                  #'nov-goto-previous-line-or-page))
  (setq-local line-spacing 0.8)
  (visual-line-mode)
  (define-key nov-mode-map (kbd "N") 'nov-next-document)
  (define-key nov-mode-map (kbd "P") 'nov-previous-document)
  (define-key nov-mode-map (kbd "n") 'nov-goto-next-line-or-page)
  (define-key nov-mode-map (kbd "p") 'nov-goto-previous-line-or-page)
  (define-key nov-mode-map (kbd "o") 'nov-goto-toc)
  (define-key nov-mode-map (kbd "f") 'forward-char)
  (define-key nov-mode-map (kbd "b") 'backward-char)
  (define-key nov-mode-map (kbd "e") 'end-of-line)
  (define-key nov-mode-map (kbd "a") 'beginning-of-line)
  (define-key nov-mode-map (kbd "l") 'forward-char)
  (define-key nov-mode-map (kbd "h") 'backward-char)
  (define-key nov-mode-map (kbd "j") 'nov-goto-next-line-or-page)
  (define-key nov-mode-map (kbd "k") 'nov-goto-previous-line-or-page)
  (define-key nov-mode-map (kbd "=") 'er/expand-region)
  (define-key nov-mode-map (kbd ",") 'bing-dict-at-point))

(provide 'init-epub)
