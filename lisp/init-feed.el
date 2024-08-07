(use-package elfeed
  :config
  (setq elfeed-curl-extra-arguments '("-x" "http://localhost:1087"))
  (setq elfeed-db-directory "~/.emacs.d/elfeed")
  (setq elfeed-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36")
  (define-key elfeed-show-mode-map (kbd "n") #'next-line)
  (define-key elfeed-show-mode-map (kbd "p") #'previous-line)
  (define-key elfeed-show-mode-map (kbd "j") #'next-line)
  (define-key elfeed-show-mode-map (kbd "k") #'previous-line)
  (define-key elfeed-show-mode-map (kbd "N") #'elfeed-show-next)
  (define-key elfeed-show-mode-map (kbd "P") #'elfeed-show-prev))

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

(feed-github-commit "emacs-mirror/emacs")

(setq elfeed-feeds
      `(("https://rsshub.app/oschina/news/industry" news)
        ("https://rsshub.app/cnbeta" news)
        ("https://rsshub.app/36kr/hot-list" news)
        ("https://rsshub.app/apnews/topics/apf-topnews" news)
        ("https://rsshub.app/v2ex/tab/hot" discuss)
        ("https://rsshub.app/sspai/index" news)
        ("https://rsshub.app/51cto/index/recommend" tech)
        ("https://rsshub.app/meituan/tech" tech)
        ("https://rsshub.app/techcrunch/news" tech)
        ,(feed-github-commit "emacs-mirror/emacs")
        ,(feed-github-commit "scalameta/metals")))

(global-set-key (kbd "C-x e") 'elfeed)

(provide 'init-feed)
