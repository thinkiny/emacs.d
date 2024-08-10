;;; init-feed.el  -*- lexical-binding: t -*-

(use-package elfeed
  :config
  (setq elfeed-curl-extra-arguments '("-x" "http://localhost:1087"))
  (setq elfeed-db-directory "~/.emacs.d/elfeed")
  (setq elfeed-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36"))

(defun kill-elfeed-buffer()
  (let* ((buffer (get-buffer "*elfeed-entry*")))
    (kill-buffer buffer)))

(with-eval-after-load 'elfeed
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

  (defun my-elfeed-show-mode-hook()
    (visual-line-mode)
    (define-key elfeed-show-mode-map (kbd "n") #'next-line)
    (define-key elfeed-show-mode-map (kbd "v") #'elfeed-show-visit)
    (define-key elfeed-show-mode-map (kbd "p") #'previous-line)
    (define-key elfeed-show-mode-map (kbd "j") #'next-line)
    (define-key elfeed-show-mode-map (kbd "k") #'previous-line)
    (define-key elfeed-show-mode-map (kbd "N") #'elfeed-show-next)
    (define-key elfeed-show-mode-map (kbd "P") #'elfeed-show-prev))

  (add-hook 'elfeed-show-mode-hook #'my-elfeed-show-mode-hook))

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
          ("https://rsshub.app/v2ex/tab/hot" discuss)
          ("https://rsshub.app/sspai/index" news)
          ("https://www.solidot.org/index.rss" news)
          ("https://rsshub.app/egsea/flash" news)
          ("https://www.geekpark.net/rss" tech)
          ("https://www.biede.com/feed/" news)
          ("https://rsshub.app/hellobtc/kepu/latest" tech)
          ("https://rsshub.app/hellobtc/news" news)
          ("https://rsshub.app/juejin/posts/1838039172387262" tech)
          ("http://www.ruanyifeng.com/blog/atom.xml" tech)
          ("https://rsshub.app/51cto/index/recommend" tech)
          ("https://rsshub.app/cloudnative/blog" tech)
          ("https://rsshub.app/deeplearning/thebatch" tech)
          ("https://rsshub.app/cloudnative/blog" tech)
          ("https://rsshub.app/meituan/tech" tech)
          ,(feed-github-commit "emacs-mirror/emacs")
          ,(feed-github-commit "scalameta/metals"))))

(global-set-key (kbd "C-x e") 'elfeed)

(provide 'init-feed)
