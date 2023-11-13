(defvar nov-use-xwidget t)
(defvar nov-scroll-step 10)
(use-package nov
  :mode (("\\.epub$" . nov-mode))
  :config
  (when nov-use-xwidget
    (require 'nov-xwidget)
    (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))
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

(defun setup-nov()
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


(defun nov-xwidget-next-line-or-page-cb(end)
  (if (s-equals-p end "1")
      (nov-xwidget-next-document)
    (xwidget-webkit-scroll-up-line nov-scroll-step)))

(defun nov-xwidget-next-line-or-page()
  (interactive)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   "(function () {
    if (window.innerHeight + window.scrollY >= document.body.scrollHeight) {
        return \"1\";
    } else {
        return \"0\";
    }
})();" #'nov-xwidget-next-line-or-page-cb))

(defun nov-xwidget-previous-line-or-page-cb(end)
  (if (s-equals-p end "1")
      (progn
        (nov-xwidget-previous-document)
        (run-at-time 0.1 nil #'xwidget-webkit-scroll-bottom))
    (xwidget-webkit-scroll-down-line nov-scroll-step)))

(defun nov-xwidget-previous-line-or-page()
  (interactive)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   "(function () {
    if (window.scrollY == 0) {
        return \"1\";
    } else {
        return \"0\";
    }
})();" #'nov-xwidget-previous-line-or-page-cb))

(defun setup-nov-xwidget()
  (nov-xwidget-view)
  (let ((title (cdr (assq 'title nov-metadata))))
    (setq-local xwidget-webkit-buffer-name-format title)
    (rename-buffer title))
  (read-only-mode)
  (define-key xwidget-webkit-mode-map (kbd "N") 'nov-xwidget-next-document)
  (define-key xwidget-webkit-mode-map (kbd "P") 'nov-xwidget-previous-document)
  (define-key xwidget-webkit-mode-map (kbd "n") #'nov-xwidget-next-line-or-page)
  (define-key xwidget-webkit-mode-map (kbd "G") #'xwidget-webkit-scroll-bottom)
  (define-key xwidget-webkit-mode-map (kbd "p") #'nov-xwidget-previous-line-or-page)
  (define-key xwidget-webkit-mode-map (kbd "o") 'nov-xwidget-goto-toc))

(defun my-nov-mode-hook()
  (if (boundp 'mwheel-scroll-up-function)
      (setq-local mwheel-scroll-up-function
                  #'nov-goto-next-line-or-page))
  (if (boundp 'mwheel-scroll-down-function)
      (setq-local mwheel-scroll-down-function
                  #'nov-goto-previous-line-or-page))
  (setq-local line-spacing 0.8)
  (visual-line-mode)
  (if nov-use-xwidget
      (setup-nov-xwidget)
    (setup-nov)))

(provide 'init-epub)
