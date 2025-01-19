;;; init-feed.el  -*- lexical-binding: t -*-

(use-package elfeed
  :config
  (setq elfeed-curl-extra-arguments '("-x" "http://localhost:1087"))
  (setq elfeed-db-directory "~/.emacs.d/elfeed")
  (setq elfeed-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36")
  (defvar need-kill-elfeed nil)

  (defun kill-elfeed-buffer()
    (when need-kill-elfeed
      (let* ((buffer (get-buffer "*elfeed-entry*")))
        (kill-buffer buffer))
      (setq need-kill-elfeed nil)))

  (defun elfeed-show-visit (&optional use-generic-p)
    "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'."
    (interactive "P")
    (setq need-kill-elfeed t)
    (let* ((link (elfeed-entry-link elfeed-show-entry)))
      (if use-generic-p
          (browse-url-generic link)
        (browse-url link))
      (with-current-buffer (xwidget-webkit-get-browse-buffer)
        (add-hook 'quit-window-hook #'kill-elfeed-buffer nil t))))

  (defun elfeed-open-entry-in-chrome(entry)
    (interactive)
    (let* ((link (elfeed-entry-link entry)))
      (setq elfeed-prev-frame (selected-frame))
      (browse-url-chrome link)
      (run-with-timer 0.3 nil
                      (lambda ()
                        (select-frame-set-input-focus elfeed-prev-frame)))))

  (defun elfeed-open-directly-in-chrome(entry)
    "Display the currently selected item in a buffer."
    (interactive (list (elfeed-search-selected :ignore-region)))
    (when (elfeed-entry-p entry)
      (elfeed-untag entry 'unread)
      (elfeed-search-update-entry entry)
      (unless elfeed-search-remain-on-entry (forward-line))
      (elfeed-open-entry-in-chrome entry)))

  (with-eval-after-load 'elfeed-search
    (define-key elfeed-search-mode-map (kbd "j") #'next-line)
    (define-key elfeed-search-mode-map (kbd "k") #'previous-line)
    (define-key elfeed-search-mode-map (kbd "d") #'elfeed-search-untag-all-unread)
    (define-key elfeed-search-mode-map (kbd "o") #'elfeed-open-directly-in-chrome))

  (defun elfeed-open-in-chrome()
    (interactive)
    (elfeed-open-entry-in-chrome elfeed-show-entry)
    (kill-buffer (current-buffer)))

  (defun my-elfeed-show-mode-hook()
    (visual-line-mode)
    (unbind-key (kbd "v") 'shr-map)
    (unbind-key (kbd "w") 'shr-map)
    (define-key elfeed-show-mode-map (kbd "n") #'pixel-scroll-forward-line)
    (define-key elfeed-show-mode-map (kbd "b") #'backward-word)
    (define-key elfeed-show-mode-map (kbd "f") #'forward-word)
    (define-key elfeed-show-mode-map (kbd "a") #'beginning-of-line)
    (define-key elfeed-show-mode-map (kbd "e") #'end-of-line)
    (define-key elfeed-show-mode-map (kbd ",") #'bing-dict-at-point)
    (define-key elfeed-show-mode-map (kbd "v") #'pixel-scroll-up-page)
    (define-key elfeed-show-mode-map (kbd "M-v") #'pixel-scroll-down-page)
    (define-key elfeed-show-mode-map (kbd "SPC") #'pixel-scroll-up-page)
    (define-key elfeed-show-mode-map (kbd "o") #'elfeed-open-in-chrome)
    (define-key elfeed-show-mode-map (kbd "i") #'elfeed-show-visit)
    (define-key elfeed-show-mode-map (kbd "p") #'pixel-scroll-backward-line)
    (define-key elfeed-show-mode-map (kbd "j") #'pixel-scroll-forward-line)
    (define-key elfeed-show-mode-map (kbd "k") #'pixel-scroll-backward-line)
    (define-key elfeed-show-mode-map (kbd "w") #'pixel-scroll-backward-line)
    (define-key elfeed-show-mode-map (kbd "s") #'pixel-scroll-forward-line)
    (define-key elfeed-show-mode-map (kbd "y") #'elfeed-show-yank)
    (define-key elfeed-show-mode-map (kbd "N") #'elfeed-show-next)
    (define-key elfeed-show-mode-map (kbd "P") #'elfeed-show-prev)
    (define-key elfeed-show-mode-map (kbd "M-c") #'kill-ring-save)
    (define-key elfeed-show-mode-map (kbd "M-w") #'kill-ring-save)
    (define-key elfeed-show-mode-map (kbd "<double-mouse-1>") #'bing-dict-at-point))

  (add-hook 'elfeed-show-mode-hook #'my-elfeed-show-mode-hook))

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

(defun feed-github-commit-branch (repo branch)
  (let ((name (car (last (split-string repo "/")))))
    `(,(format "https://github.com/%s/commits/%s.atom" repo branch) ,(intern name) github)))

(defun feed-github-release (repo)
  (let ((name (car (last (split-string repo "/")))))
    `(,(format "https://github.com/%s/releases.atom" repo) ,(intern name) github)))

(with-eval-after-load 'elfeed
  (require 'feeds nil 'noerror)
  (elfeed-goodies/setup))

(global-set-key (kbd "C-x e") 'elfeed)
(use-proxy-local 'elfeed-search-show-entry)

(provide 'init-feed)
