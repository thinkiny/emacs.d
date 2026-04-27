;;; init-feed.el  -*- lexical-binding: t -*-

(use-package elfeed
  :config
  (when-let* ((proxy-url (local-proxy-http-url)))
    (setq elfeed-curl-extra-arguments
          (list "-x" proxy-url)))
  (setq elfeed-db-directory "~/.emacs.d/elfeed")
  (setq elfeed-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36"))

(with-eval-after-load 'shr
  (setq shr-use-colors nil))

(defun kill-elfeed-buffer()
  (let* ((buffer (get-buffer "*elfeed-entry*")))
    (kill-buffer buffer)))

(defun elfeed-show-open-xwidget ()
  (interactive)
  (when-let* ((link (elfeed-entry-link elfeed-show-entry)))
    (xwidget-webkit-browse-open-url link)
    (with-current-buffer (xwidget-webkit-get-browse-buffer)
      (add-hook 'quit-window-hook #'kill-elfeed-buffer nil t))))

(defun elfeed-open-entry-in-chrome(entry &optional background)
  (let* ((link (or (get-text-property (point) 'shr-url)
                   (elfeed-entry-link entry))))
    (when link
      (xwidget-webkit-open-url-in-chrome link background))))

(defun elfeed-open-selected-in-chrome(entry &optional background)
  "Open the selected search entry in Chrome."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (when (elfeed-entry-p entry)
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (unless elfeed-search-remain-on-entry (forward-line))
    (elfeed-open-entry-in-chrome entry background)))

(defun elfeed-open-selected-in-chrome-background(entry)
  "Open the selected search entry in Chrome in the background."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (elfeed-open-selected-in-chrome entry t))

(with-eval-after-load 'elfeed-search
  (define-key elfeed-search-mode-map (kbd "j") #'next-line)
  (define-key elfeed-search-mode-map (kbd "k") #'previous-line)
  (define-key elfeed-search-mode-map (kbd "d") #'elfeed-search-untag-all-unread)
  (define-key elfeed-search-mode-map (kbd "O") #'elfeed-open-selected-in-chrome)
  (define-key elfeed-search-mode-map (kbd "o") #'elfeed-open-selected-in-chrome-background))

(defun elfeed-open-current-in-chrome(&optional background)
  (interactive)
  (elfeed-open-entry-in-chrome elfeed-show-entry background)
  (kill-buffer (current-buffer)))

(defun elfeed-open-current-in-chrome-background()
  (interactive)
  (elfeed-open-current-in-chrome t))

(defun my-elfeed-show-mode-hook()
  ;; from elfeed-goodies/show-mode-setup
  (setq header-line-format '(:eval (elfeed-goodies/entry-header-line))
        left-margin-width elfeed-goodies/show-mode-padding
        right-margin-width elfeed-goodies/show-mode-padding)
  (visual-line-mode)
  (eldoc-mode -1)
  (font-lock-mode -1)
  (unbind-key (kbd "v") 'shr-map)
  (unbind-key (kbd "w") 'shr-map)
  (face-remap-add-relative 'shr-text :inherit 'default)
  (define-key elfeed-show-mode-map (kbd "=") #'selection/expand)
  (define-key elfeed-show-mode-map (kbd "n") #'precision-scroll-next-line)
  (define-key elfeed-show-mode-map (kbd "p") #'precision-scroll-prev-line)
  (define-key elfeed-show-mode-map (kbd "j") #'precision-scroll-next-line)
  (define-key elfeed-show-mode-map (kbd "k") #'precision-scroll-prev-line)
  (define-key elfeed-show-mode-map (kbd "h") #'backward-word)
  (define-key elfeed-show-mode-map (kbd "l") #'forward-word-begin)
  (define-key elfeed-show-mode-map (kbd "w") #'precision-scroll-prev-line)
  (define-key elfeed-show-mode-map (kbd "s") #'precision-scroll-next-line)
  (define-key elfeed-show-mode-map (kbd "b") #'backward-word)
  (define-key elfeed-show-mode-map (kbd "f") #'forward-word-begin)
  (define-key elfeed-show-mode-map (kbd "a") #'beginning-of-line)
  (define-key elfeed-show-mode-map (kbd "e") #'end-of-line)
  (define-key elfeed-show-mode-map (kbd ",") #'translate-at-point)
  (define-key elfeed-show-mode-map (kbd "v") #'precision-scroll-up-page)
  (define-key elfeed-show-mode-map (kbd "M-v") #'precision-scroll-down-page)
  (define-key elfeed-show-mode-map (kbd "SPC") #'precision-scroll-up-page)
  (define-key elfeed-show-mode-map (kbd "O") #'elfeed-open-current-in-chrome)
  (define-key elfeed-show-mode-map (kbd "o") #'elfeed-open-current-in-chrome-background)
  (define-key elfeed-show-mode-map (kbd "C-g") #'selection/quit)
  (define-key elfeed-show-mode-map (kbd "x") #'elfeed-show-open-xwidget)
  (define-key elfeed-show-mode-map (kbd "y") #'elfeed-show-yank)
  (define-key elfeed-show-mode-map (kbd "N") #'elfeed-show-next)
  (define-key elfeed-show-mode-map (kbd "P") #'elfeed-show-prev)
  (define-key elfeed-show-mode-map (kbd "M-c") #'kill-ring-save)
  (define-key elfeed-show-mode-map (kbd "M-w") #'kill-ring-save)
  (define-key elfeed-show-mode-map (kbd "<double-mouse-1>") #'translate-at-point))

(add-hook 'elfeed-show-mode-hook #'my-elfeed-show-mode-hook)

(use-package elfeed-goodies
  :after elfeed
  :config
  (elfeed-goodies/setup)
  (remove-hook 'elfeed-show-mode-hook #'elfeed-goodies/show-mode-setup)
  (setq elfeed-show-entry-switch #'switch-to-buffer))

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

(global-set-key (kbd "C-x e") 'elfeed)
(use-proxy-local 'url-retrieve)

(provide 'init-feed)
