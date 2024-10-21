;;; init-feed.el  -*- lexical-binding: t -*-

(use-package elfeed
  :config
  (setq elfeed-curl-extra-arguments '("-x" "http://localhost:1087"))
  (setq elfeed-db-directory "~/.emacs.d/elfeed")
  (setq elfeed-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36")

  (defun elfeed-show-visit (&optional use-generic-p)
    "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'."
    (interactive "P")
    (let* ((link (elfeed-entry-link elfeed-show-entry)))
      (if use-generic-p
          (browse-url-generic link)
        (browse-url link))
      (with-current-buffer (xwidget-webkit-get-browse-buffer)
        (add-hook 'quit-window-hook #'kill-elfeed-buffer nil t))))


  (with-eval-after-load 'elfeed-search
    (define-key elfeed-search-mode-map (kbd "j") #'next-line)
    (define-key elfeed-search-mode-map (kbd "k") #'previous-line))

  (defun my-elfeed-show-mode-hook()
    (visual-line-mode)
    (define-key elfeed-show-mode-map (kbd "n") #'scroll-down-line)
    (define-key elfeed-show-mode-map (kbd "b") #'backward-char)
    (define-key elfeed-show-mode-map (kbd "f") #'forward-char)
    (define-key elfeed-show-mode-map (kbd "a") #'beginning-of-line)
    (define-key elfeed-show-mode-map (kbd "e") #'end-of-line)
    (define-key elfeed-show-mode-map (kbd ",") #'bing-dict-at-point)
    (define-key elfeed-show-mode-map (kbd "v") #'scroll-up)
    ;;(define-key elfeed-show-mode-map (kbd "M-v") #'scroll-down)
    (define-key elfeed-show-mode-map (kbd "o") #'elfeed-show-visit)
    (define-key elfeed-show-mode-map (kbd "p") #'scroll-up-line)
    (define-key elfeed-show-mode-map (kbd "j") #'scroll-down-line)
    (define-key elfeed-show-mode-map (kbd "k") #'scroll-up-line)
    (define-key elfeed-show-mode-map (kbd "w") #'elfeed-show-yank)
    (define-key elfeed-show-mode-map (kbd "N") #'elfeed-show-next)
    (define-key elfeed-show-mode-map (kbd "P") #'elfeed-show-prev)
    (define-key elfeed-show-mode-map (kbd "M-c") #'kill-ring-save)
    (define-key elfeed-show-mode-map (kbd "<double-mouse-1>") #'bing-dict-at-point))

  (elfeed-goodies/setup)
  (add-hook 'elfeed-show-mode-hook #'my-elfeed-show-mode-hook))

(defun kill-elfeed-buffer()
  (let* ((buffer (get-buffer "*elfeed-entry*")))
    (kill-buffer buffer)))

(use-package elfeed-goodies
  :after elfeed
  :ensure t
  :config
  (defun elfeed-goodies/show-mode-setup ()
  "Setup function providing defaults for show mode buffer."
  (setq header-line-format '(:eval (elfeed-goodies/entry-header-line))
        left-margin-width elfeed-goodies/show-mode-padding
        right-margin-width elfeed-goodies/show-mode-padding)
  (define-key elfeed-show-mode-map (kbd "M-v") #'scroll-down)))

;; setup feeds
(defun feed-github-commit (repo)
  (let ((name (car (last (split-string repo "/")))))
    `(,(format "https://github.com/%s/commits.atom" repo) ,(intern name) github)))

(defun feed-github-release (repo)
  (let ((name (car (last (split-string repo "/")))))
    `(,(format "https://github.com/%s/releases.atom" repo) ,(intern name) github)))

(with-eval-after-load 'elfeed
  (require 'feeds nil 'noerror))

(global-set-key (kbd "C-x e") 'elfeed)
(use-proxy-local 'elfeed-search-show-entry)

(provide 'init-feed)
