;;; init-feed.el  -*- lexical-binding: t -*-

(use-package elfeed
  :config
  (setq elfeed-curl-extra-arguments '("-x" "http://localhost:1087"))
  (setq elfeed-db-directory "~/.emacs.d/elfeed")
  (setq elfeed-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36"))

(defun kill-elfeed-buffer()
  (let* ((buffer (get-buffer "*elfeed-entry*")))
    (kill-buffer buffer)))

(defun elfeed-show-visit-xwidget ()
  (interactive)
  (when-let* ((link (elfeed-entry-link elfeed-show-entry)))
    (xwidget-webkit-browse-open-url link)
    (with-current-buffer (xwidget-webkit-get-browse-buffer)
      (add-hook 'quit-window-hook #'kill-elfeed-buffer nil t))))

(defun elfeed-open-entry-in-chrome(entry &optional background)
  (interactive)
  (let* ((link (or (get-text-property (point) 'shr-url)
                   (elfeed-entry-link entry))))
    (setq elfeed-prev-frame (selected-frame))
    (browse-url-chrome link))
  (if background
      (run-with-timer 0.3 nil
                      (lambda ()
                        (select-frame-set-input-focus elfeed-prev-frame)))))

(defun elfeed-open-directly-in-chrome(entry &optional background)
  "Display the currently selected item in a buffer."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (when (elfeed-entry-p entry)
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (unless elfeed-search-remain-on-entry (forward-line))
    (elfeed-open-entry-in-chrome entry background)))

(defun elfeed-open-directly-in-chrome-background(entry)
  "Display the currently selected item in a buffer."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (when (elfeed-entry-p entry)
    (elfeed-open-directly-in-chrome entry t)))

(with-eval-after-load 'elfeed-search
  (define-key elfeed-search-mode-map (kbd "j") #'next-line)
  (define-key elfeed-search-mode-map (kbd "k") #'previous-line)
  (define-key elfeed-search-mode-map (kbd "d") #'elfeed-search-untag-all-unread)
  (define-key elfeed-search-mode-map (kbd "O") #'elfeed-open-directly-in-chrome)
  (define-key elfeed-search-mode-map (kbd "o") #'elfeed-open-directly-in-chrome-background))

(defun elfeed-open-in-chrome(&optional background)
  (interactive)
  (elfeed-open-entry-in-chrome elfeed-show-entry background)
  (kill-buffer (current-buffer)))

(defun elfeed-open-in-chrome-background()
  (interactive)
  (elfeed-open-in-chrome t))

(defvar-local elfeed--pre-selection-point nil
  "Point position before `elfeed-expand-selection' was first invoked.")

(defconst elfeed--sentence-end-re "\\(?:[.!?]\\s-\\|[。！？]\\)"
  "Regexp matching a sentence boundary (ASCII or CJK).")

(defun elfeed--sentence-bounds ()
  "Return (START . END) of the sentence at point, or nil."
  (save-excursion
    (unless (re-search-backward elfeed--sentence-end-re nil t)
      (goto-char (point-min)))
    (when (looking-at "[.!?。！？]") (forward-char 1))
    (skip-chars-forward " \t\n")
    (let ((start (point))
          (end (if (re-search-forward elfeed--sentence-end-re nil t)
                   (point)
                 (point-max))))
      (and (< start end) (cons start end)))))

(defun elfeed-expand-selection ()
  "Expand selection progressively: word → sentence."
  (interactive)
  (if (not (region-active-p))
      (progn
        (setq elfeed--pre-selection-point (point))
        (when-let* ((bounds (bounds-of-thing-at-point 'word)))
          (goto-char (car bounds))
          (push-mark (cdr bounds) nil t)))
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      (unless (string-match-p "[.!?。！？]\\|\\S-\\s-+\\S-" text)
        (goto-char (region-beginning))
        (when-let* ((bounds (elfeed--sentence-bounds)))
          (goto-char (car bounds))
          (push-mark (cdr bounds) nil t))))))

(defun elfeed-keyboard-quit()
  (interactive)
  (if (region-active-p)
      (progn
        (when elfeed--pre-selection-point
          (goto-char elfeed--pre-selection-point)
          (setq elfeed--pre-selection-point nil))
        (deactivate-mark))
    (keyboard-quit)))

(defun my-elfeed-show-mode-hook()
  (visual-line-mode)
  (eldoc-mode -1)
  (font-lock-mode -1)
  (unbind-key (kbd "v") 'shr-map)
  (unbind-key (kbd "w") 'shr-map)
  (face-remap-add-relative 'shr-text :inherit 'default)
  (define-key elfeed-show-mode-map (kbd "=") #'elfeed-expand-selection)
  (define-key elfeed-show-mode-map (kbd "n") #'precision-scroll-forward-line)
  (define-key elfeed-show-mode-map (kbd "b") #'backward-word)
  (define-key elfeed-show-mode-map (kbd "f") #'forward-word-begin)
  (define-key elfeed-show-mode-map (kbd "a") #'beginning-of-line)
  (define-key elfeed-show-mode-map (kbd "e") #'end-of-line)
  (define-key elfeed-show-mode-map (kbd ",") #'translate-at-point)
  (define-key elfeed-show-mode-map (kbd "v") #'precision-scroll-up-page)
  (define-key elfeed-show-mode-map (kbd "M-v") #'precision-scroll-down-page)
  (define-key elfeed-show-mode-map (kbd "SPC") #'precision-scroll-up-page)
  (define-key elfeed-show-mode-map (kbd "O") #'elfeed-open-in-chrome)
  (define-key elfeed-show-mode-map (kbd "C-g") #'elfeed-keyboard-quit)
  (define-key elfeed-show-mode-map (kbd "o") #'elfeed-open-in-chrome-background)
  (define-key elfeed-show-mode-map (kbd "V") #'elfeed-show-visit-xwidget)
  (define-key elfeed-show-mode-map (kbd "p") #'precision-scroll-backward-line)
  (define-key elfeed-show-mode-map (kbd "j") #'precision-scroll-forward-line)
  (define-key elfeed-show-mode-map (kbd "k") #'precision-scroll-backward-line)
  (define-key elfeed-show-mode-map (kbd "h") #'backward-word)
  (define-key elfeed-show-mode-map (kbd "l") #'forward-word-begin)
  (define-key elfeed-show-mode-map (kbd "w") #'precision-scroll-backward-line)
  (define-key elfeed-show-mode-map (kbd "s") #'precision-scroll-forward-line)
  (define-key elfeed-show-mode-map (kbd "y") #'elfeed-show-yank)
  (define-key elfeed-show-mode-map (kbd "N") #'elfeed-show-next)
  (define-key elfeed-show-mode-map (kbd "P") #'elfeed-show-prev)
  (define-key elfeed-show-mode-map (kbd "M-c") #'kill-ring-save)
  (define-key elfeed-show-mode-map (kbd "M-w") #'kill-ring-save)
  (define-key elfeed-show-mode-map (kbd "<double-mouse-1>") #'translate-at-point))

(add-hook 'elfeed-show-mode-hook #'my-elfeed-show-mode-hook)

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
  (elfeed-goodies/setup))

(global-set-key (kbd "C-x e") 'elfeed)
(use-proxy-local 'elfeed-search-show-entry)

(provide 'init-feed)
