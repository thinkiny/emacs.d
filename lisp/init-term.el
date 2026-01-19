;; -*- lexical-binding: t; -*-

;; vterm
;; https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking
;; if [ "$TERM" = "xterm-256color" ]; then
;;     PS1=$PS1'\[$(vterm_prompt_end)\]'
;; fi
(use-package vterm
  :commands (vterm-mode vterm)
  :config
  (add-to-list 'vterm-tramp-shells '("ssh" "/bin/bash -l"))
  (ignore-tramp-ssh-control-master 'vterm-mode)
  (setq vterm-always-compile-module t)
  (setq vterm-min-window-width 60)
  (setq vterm-max-scrollback 5000)
  (setq vterm-shell (concat shell-file-name " -l"))

  ;; key bindings
  (define-key vterm-mode-map (kbd "M-w") 'vterm-copy-text)
  (define-key vterm-mode-map (kbd "C-c v") 'vterm-copy-mode)
  (define-key vterm-mode-map (kbd "C-v") 'vterm-move-down)
  (define-key vterm-mode-map (kbd "M-v") 'vterm-move-up)
  (define-key vterm-copy-mode-map (kbd "C-c v") 'vterm-copy-mode))

(with-eval-after-load 'vterm
  (require 'vterm-anti-flicker-filter)
  (remove-hook 'vterm-mode-hook #'vterm-anti-flicker-filter-enable)

  (defun vterm--get-directory (path)
    "Get normalized directory to PATH."
    (when path
      (let (directory)
        (if (string-match "^\\(.*?\\)@\\(.*?\\):\\(.*?\\)$" path)
            (progn
              (let ((user (match-string 1 path))
                    (host (match-string 2 path))
                    (dir (match-string 3 path)))
                (if (and (string-equal user user-login-name)
                         (string-equal host (system-name)))
                    (progn
                      (when (file-directory-p dir)
                        (setq directory (file-name-as-directory dir))))
                  (setq directory (file-name-as-directory (concat "/" tramp-default-method ":" path))))))
          (when (file-directory-p path)
            (setq directory (file-name-as-directory path))))
        directory))))

(defun my-vterm-mode-hook()
  (unless (string-prefix-p "*claude-code" (buffer-name))
    (vterm-anti-flicker-filter-enable)))

(add-hook 'vterm-mode-hook #'my-vterm-mode-hook)

;; other staff
(defun vterm-copy-text ()
  (interactive)
  (when (region-active-p)
    (save-excursion
      (vterm--enter-copy-mode)
      (kill-ring-save (region-beginning) (region-end))
      (vterm--exit-copy-mode))))

(defun vterm-move-up()
  (interactive)
  (let ((current-prefix-arg (/ (window-height) 2)))
    (call-interactively #'previous-line)))

(defun vterm-move-down()
  (interactive)
  (let ((current-prefix-arg (/ (window-height) 2)))
    (call-interactively #'next-line)))

(defun my/vterm-toggle-scroll (&rest _)
  (when (eq major-mode 'vterm-mode)
    (if (>= (window-end) (buffer-size))
        (when vterm-copy-mode
          (vterm-copy-mode-done nil))
      (vterm-copy-mode 1))))

(advice-add 'set-window-vscroll :after #'my/vterm-toggle-scroll)

;; counsel-term
(require 'counsel-term)
(global-set-key (kbd "C-x t") 'counsel-term)

;; term-color
(with-eval-after-load-theme 'term
                            (when (theme-dark-p)
                              ;; (set-face-background 'term-color-black (face-attribute 'default :foreground))
                              (set-face-foreground 'term-color-blue "skyblue3")
                              (set-face-foreground 'term-color-red "IndianRed1")))


;; eat
(require-package 'eat)
(setq eat-term-name "xterm-256color")

(with-eval-after-load 'eshell
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode))

(provide 'init-term)
