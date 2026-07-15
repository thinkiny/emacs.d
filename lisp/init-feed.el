;;; init-feed.el  -*- lexical-binding: t -*-

(use-package elfeed
  :bind (:map global-map
              ("C-x e" . elfeed--switch))
  :config
  (use-proxy-local 'url-retrieve)
  (when-let* ((proxy-url (local-proxy-http-url)))
    (setq elfeed-curl-extra-arguments (list "-x" proxy-url)))

  (defconst elfeed-local-mapping "~/org/*elfeed*")
  (setq elfeed-db-directory "~/.emacs.d/elfeed")
  (setq elfeed-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36")
  (setq elfeed-search-print-entry-function #'elfeed-search-print-entry--custom)
  (define-key elfeed-search-mode-map (kbd "j") #'next-line)
  (define-key elfeed-search-mode-map (kbd "v") #'scroll-up)
  (define-key elfeed-search-mode-map (kbd "k") #'previous-line)
  (define-key elfeed-search-mode-map (kbd "O") #'elfeed-open-selected-in-chrome)
  (define-key elfeed-search-mode-map (kbd "o") #'elfeed-open-selected-in-chrome-background))

;; shr
(with-eval-after-load 'shr
  (setq shr-max-width nil)
  (setq shr-use-colors nil)
  (setq shr-sliced-image-height 0.1)
  (set-face-attribute 'variable-pitch nil :family 'unspecified)
  (set-face-attribute 'variable-pitch-text nil :height 1.0)
  (unbind-key (kbd "v") 'shr-map)
  (unbind-key (kbd "w") 'shr-map))

;; elfeed options
(defun elfeed--switch ()
  "Switch to *elfeed-entry* if it exists, otherwise run elfeed."
  (interactive)
  (if (get-buffer "*elfeed-entry*")
      (switch-to-buffer "*elfeed-entry*")
    (elfeed)))

(defun kill-elfeed-show-buffer()
  (when-let* ((buffer (get-buffer "*elfeed-entry*")))
    (kill-buffer buffer)))

(defun elfeed-show-open-xwidget ()
  (interactive)
  (when-let* ((link (elfeed-entry-link elfeed-show-entry)))
    (kill-elfeed-show-buffer)
    (xwidget-webkit-browse-open-url link t elfeed-local-mapping)))

(defun elfeed-show-open-url (url)
  "Prompt for a URL (default: current entry link) and act on it.
A plain RET reloads the current entry.  Editing the URL opens it
via `browse-url' (your xwidget), mirroring `xwidget-webkit-browse-open-url'."
  (interactive
   (list (read-string "open URL: "
                      (elfeed-entry-link elfeed-show-entry))))
  (unless (stringp url)
    (user-error "No URL"))
  (unless (string-match "\\`[A-Za-z]+:" url)
    (setq url (concat "https://" url)))
  (if (string= url (elfeed-entry-link elfeed-show-entry))
      (elfeed-show-refresh)
    (browse-url url)))

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

(defun elfeed-open-current-in-chrome(&optional background)
  (interactive)
  (elfeed-open-entry-in-chrome elfeed-show-entry background)
  (kill-buffer (current-buffer)))

(defun elfeed-open-current-in-chrome-background()
  (interactive)
  (elfeed-open-current-in-chrome t))

(defun elfeed--show-quit ()
  "Kill the elfeed entry buffer and return to elfeed search."
  (interactive)
  (kill-buffer)
  (elfeed))

(defun my-elfeed-show-mode-hook()
  (visual-line-mode))

(with-eval-after-load 'elfeed-show
  (define-key elfeed-show-mode-map (kbd "<double-mouse-1>") #'translate-at-point)
  (define-key elfeed-show-mode-map (kbd "=")   #'selection/expand)
  (define-key elfeed-show-mode-map (kbd "r")   #'revert-buffer)
  (define-key elfeed-show-mode-map (kbd "n")   #'precision-scroll-next-line)
  (define-key elfeed-show-mode-map (kbd "p")   #'precision-scroll-prev-line)
  (define-key elfeed-show-mode-map (kbd "j")   #'precision-scroll-next-line)
  (define-key elfeed-show-mode-map (kbd "k")   #'precision-scroll-prev-line)
  (define-key elfeed-show-mode-map (kbd "h")   #'backward-word)
  (define-key elfeed-show-mode-map (kbd "l")   #'forward-word-begin)
  (define-key elfeed-show-mode-map (kbd "w")   #'precision-scroll-prev-line)
  (define-key elfeed-show-mode-map (kbd "s")   #'precision-scroll-next-line)
  (define-key elfeed-show-mode-map (kbd "b")   #'backward-word)
  (define-key elfeed-show-mode-map (kbd "f")   #'forward-word-begin)
  (define-key elfeed-show-mode-map (kbd "a")   #'beginning-of-line)
  (define-key elfeed-show-mode-map (kbd "e")   #'end-of-line)
  (define-key elfeed-show-mode-map (kbd "(")   #'backward-sentence)
  (define-key elfeed-show-mode-map (kbd ")")   #'forward-sentence)
  (define-key elfeed-show-mode-map (kbd "q")   #'elfeed--show-quit)
  (define-key elfeed-show-mode-map (kbd ",")   #'translate-at-point)
  (define-key elfeed-show-mode-map (kbd "SPC") #'selection/toggle-mark)
  (define-key elfeed-show-mode-map (kbd "v")   #'precision-scroll-up-page)
  (define-key elfeed-show-mode-map (kbd "C-v") #'precision-scroll-up-page)
  (define-key elfeed-show-mode-map (kbd "M-v") #'precision-scroll-down-page)
  (define-key elfeed-show-mode-map (kbd "O")   #'elfeed-open-current-in-chrome)
  (define-key elfeed-show-mode-map (kbd "o")   #'elfeed-open-current-in-chrome-background)
  (define-key elfeed-show-mode-map (kbd "C-g") #'selection/quit)
  (define-key elfeed-show-mode-map (kbd "x")   #'elfeed-show-open-xwidget)
  (define-key elfeed-show-mode-map (kbd "g")   #'elfeed-show-open-url)
  (define-key elfeed-show-mode-map (kbd "y")   #'elfeed-show-yank)
  (define-key elfeed-show-mode-map (kbd "N")   #'elfeed-show-next)
  (define-key elfeed-show-mode-map (kbd "P")   #'elfeed-show-prev)
  (define-key elfeed-show-mode-map (kbd "M-c") #'kill-ring-save)
  (define-key elfeed-show-mode-map (kbd "M-w") #'kill-ring-save))

(defun elfeed-mapping-local-file()
  (map-buffer-to-local-file elfeed-local-mapping))

;; set hooks
(defun my-elfeed-show-update-hook ()
  "Jump to the main content, past the header metadata."
  (forward-paragraph)
  (forward-line)
  (elfeed-mapping-local-file))

(defun elfeed--search-clear-modified (&rest _)
  "Clear the modified flag on the elfeed search buffer after showing an entry."
  (when-let* ((buf (get-buffer "*elfeed-search*")))
    (with-current-buffer buf
      (map-buffer-to-local-file-clear-modified))))

(advice-add 'elfeed-search-show-entry :after #'elfeed--search-clear-modified)
(advice-add 'elfeed-search-untag-all-unread :after #'elfeed--search-clear-modified)
(add-hook 'elfeed-search-update-hook #'map-buffer-to-local-file-clear-modified)
(add-hook 'elfeed-show-update-hook #'my-elfeed-show-update-hook)
(add-hook 'elfeed-show-mode-hook #'my-elfeed-show-mode-hook)
(add-hook 'elfeed-search-mode-hook #'elfeed-mapping-local-file)

;; setup feeds
(defun feed-github-commit (repo)
  (let ((name (car (last (split-string repo "/")))))
    `(,(format "https://github.com/%s/commits.atom" repo) ,(intern name))))

(defun feed-github-commit-branch (repo branch)
  (let ((name (car (last (split-string repo "/")))))
    `(,(format "https://github.com/%s/commits/%s.atom" repo branch) ,(intern name))))

(defun feed-github-release (repo)
  (let ((name (car (last (split-string repo "/")))))
    `(,(format "https://github.com/%s/releases.atom" repo) ,(intern name))))

;; elfeed-search--feed-display-name
(defvar elfeed-search--source-column-width 16
  "Width of the feed source column in elfeed-search.")

(defvar elfeed-search--tags-column-width 20
  "Width of the tags column in elfeed-search.")

(defvar elfeed-search-source-map nil
  "Alist mapping URL prefixes to display names for the source column.
Each entry is (PREFIX . DISPLAY-NAME).  If a feed URL starts with
PREFIX, DISPLAY-NAME is shown instead of the feed title.")

(defun elfeed-search--feed-display-name (feed)
  "Return display name for FEED, consulting `elfeed-search-source-map'."
  (let ((name (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed)))))
    (or (cl-loop for (prefix . alias) in elfeed-search-source-map
                 when (and name (string-prefix-p prefix name))
                 return alias)
        name)))

(defun elfeed-search-print-entry--custom (entry)
  "Print ENTRY to the buffer with source, tags, title, date columns."
  (let* ((title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed-title (elfeed-search--feed-display-name (elfeed-entry-feed entry)))
         (tags (concat "[" (mapconcat #'identity
                                      (mapcar #'symbol-name (elfeed-entry-tags entry)) ",") "]"))
         (date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title-width (- (or (window-width (get-buffer-window)) (frame-width))
                         elfeed-search--source-column-width
                         elfeed-search--tags-column-width
                         (string-width date) 3)))
    (insert (propertize (elfeed-format-column (or feed-title "") elfeed-search--source-column-width :left)
                        'face 'elfeed-search-feed-face) " "
                        (propertize (elfeed-format-column tags elfeed-search--tags-column-width :left)
                                    'face 'elfeed-search-tag-face) " "
                        (propertize (elfeed-format-column
                                     title (elfeed-clamp elfeed-search-title-min-width
                                                         title-width elfeed-search-title-max-width) :left)
                                    'face title-faces 'kbd-help title) " "
                        (propertize date 'face 'elfeed-search-date-face))))

(provide 'init-feed)
