;;; init-feed.el  -*- lexical-binding: t -*-

(use-package elfeed
  :config
  (setq elfeed-curl-extra-arguments '("-x" "http://localhost:1087"))
  (setq elfeed-db-directory "~/.emacs.d/elfeed")
  (setq elfeed-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36"))

(with-eval-after-load 'elfeed-show
  (defun elfeed-show-visit (&optional use-generic-p)
    "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'."
    (interactive "P")
    (defvar el-buffer (current-buffer))
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
        (message "Sent to browser: %s" link)
        (if use-generic-p
            (browse-url-generic link)
          (browse-url link))
        (with-current-buffer (xwidget-webkit-get-browse-buffer)
          (add-hook 'quit-window-hook (lambda () (kill-buffer el-buffer)) nil t))))))

(add-hook 'elfeed-show-mode-hook (lambda ()
                                   (define-key elfeed-show-mode-map (kbd "n") #'next-line)
                                   (define-key elfeed-show-mode-map (kbd "p") #'previous-line)
                                   (define-key elfeed-show-mode-map (kbd "j") #'next-line)
                                   (define-key elfeed-show-mode-map (kbd "k") #'previous-line)
                                   (define-key elfeed-show-mode-map (kbd "N") #'elfeed-show-next)
                                   (define-key elfeed-show-mode-map (kbd "P") #'elfeed-show-prev)))

(use-package elfeed-goodies
  :after elfeed
  :ensure t)

(elfeed-goodies/setup)

;; setup feeds
(defun feed-github-commit (repo)
  (let ((name (car (last (split-string repo "/")))))
    `(,(format "https://github.com/%s/commits.atom" repo) ,(intern name) github)))

(defun feed-github-release (repo)
  (let ((name (car (last (split-string repo "/")))))
    `(,(format "https://github.com/%s/releases.atom" repo) ,(intern name) github)))

(with-eval-after-load 'elfeed
  (setq elfeed-feeds
        `(("https://rsshub.app/oschina/news/industry" tech)
          ("https://rsshub.app/cnbeta" news)
          ("https://rsshub.app/36kr/hot-list" news)
          ("https://rsshub.app/apnews/topics/apf-topnews" news)
          ("https://rsshub.app/v2ex/tab/hot" discuss)
          ("https://rsshub.app/sspai/index" news)
          ("https://rsshub.app/51cto/index/recommend" tech)
          ("https://rsshub.app/cloudnative/blog" tech)
          ("https://rsshub.app/meituan/tech" tech)
          ("https://rsshub.app/techcrunch/news" tech)
          ,(feed-github-commit "emacs-mirror/emacs")
          ,(feed-github-commit "scalameta/metals"))))

(global-set-key (kbd "C-x e") 'elfeed)

(provide 'init-feed)
