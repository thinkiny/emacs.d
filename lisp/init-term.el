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
  (setq vterm-min-window-width 40)
  (setq vterm-max-scrollback 10000)
  (setq vterm-shell (concat shell-file-name " -l"))

  ;; fix Ctrl-B not working in claude code in some cases
  (defun vterm-send-key-left()
    (interactive)
    (vterm-send-key "<left>"))

  ;; key bindings
  (define-key vterm-mode-map (kbd "M-w") 'kill-ring-save)
  (define-key vterm-mode-map (kbd "C-c v") 'vterm-copy-mode)
  (define-key vterm-mode-map (kbd "C-v") 'scroll-up-command)
  ;; (define-key vterm-mode-map (kbd "C-b") 'vterm-send-key-left)
  (define-key vterm-mode-map (kbd "M-v") 'scroll-down-command)
  (define-key vterm-copy-mode-map (kbd "C-c v") 'vterm-copy-mode)

  ;; scroll wrapper for loopback mode handling
  (defun vterm-pixel-scroll-precision (event)
    "Wrapper for pixel-scroll-precision in vterm - handles loopback mode."
    (interactive "e")
    (when (nth 4 event)
      (let ((delta (round (cdr (nth 4 event)))))
        (unless (zerop delta)
          ;; Handle loopback before scrolling up
          (if (> delta 0)
            (vterm--pixel-scroll-precision-up))
          ;; Call the original pixel-scroll-precision
          (pixel-scroll-precision event)
          ;; Handle loopback after scrolling down
          (if (< delta 0)
            (vterm--pixel-scroll-precision-down))))))

  (define-key vterm-mode-map [remap pixel-scroll-precision] #'vterm-pixel-scroll-precision)
  (define-key vterm-copy-mode-map [remap pixel-scroll-precision] #'vterm-pixel-scroll-precision)
  (add-hook 'vterm-copy-mode-hook #'my-vterm-copy-mode-hook))

(with-eval-after-load 'vterm
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

;; claude-chill loopback
(defvar-local vterm-claude-loopback-mode-enabled nil
  "Track whether claude loopback mode is enabled.")

(defvar-local vterm-claude--last-modified-tick nil
  "Track last buffer modified tick for claude vterm buffer.")

(defun vterm-claude--buffer-changed-p ()
  (let ((current (buffer-modified-tick)))
    (unless (eq current vterm-claude--last-modified-tick)
      (setq vterm-claude--last-modified-tick current)
      t)))

(defun toggle-vterm-claude-loopback-mode ()
  (interactive)
  (vterm-claude-loopback-mode (not vterm-claude-loopback-mode-enabled)))

(defun my-vterm-copy-mode-hook()
  (if vterm-copy-mode
      (vterm-claude-loopback-mode t)
    (vterm-claude-loopback-mode nil)))

(defun vterm-claude-loopback-mode (enable)
  (when (and (string-prefix-p "*claude-code" (buffer-name))
             (not claude-code-ide-prevent-reflow-glitch))
    (unless (eq enable vterm-claude-loopback-mode-enabled)
      (if enable
          (when (vterm-claude--buffer-changed-p)
            (vterm-send-string "\x1e"))
        (vterm-send-string "\x1e"))
      (setq vterm-claude-loopback-mode-enabled enable))))

;; vterm--pixel-scroll
(defvar-local vterm--scroll-timer nil)
(defun vterm--pixel-scroll-precision-up ()
  (when vterm--scroll-timer
    (cancel-timer vterm--scroll-timer)
    (setq vterm--scroll-timer nil))
  (unless vterm-copy-mode
    (vterm-copy-mode 1)))

(defun vterm--scroll-session-end ()
  "Handle scroll session end."
  (setq vterm--scroll-timer nil)
  (when (and (>= (window-end) (point-max)) vterm-copy-mode)
    (vterm-copy-mode -1)))

(defun vterm--pixel-scroll-precision-down ()
  (when vterm--scroll-timer
    (cancel-timer vterm--scroll-timer))
  (let ((buf (current-buffer)))
    (setq vterm--scroll-timer
          (run-with-timer 0.1 nil
                          (lambda ()
                            (when (buffer-live-p buf)
                              (with-current-buffer buf
                                (vterm--scroll-session-end))))))))

;; counsel-term
(require 'counsel-term)
(global-set-key (kbd "C-x t") 'counsel-term)

;; term-color
(with-eval-after-load-theme
 'term
 (when (theme-dark-p)
   ;; (set-face-background 'term-color-black (face-attribute 'default :foreground))
   (set-face-foreground 'term-color-blue "skyblue3")
   (set-face-foreground 'term-color-red "IndianRed1")))

;; eat
(require-package 'eat)
(setq eat-term-name "xterm-256color")
(ignore-tramp-ssh-control-master 'eat-exec)

(with-eval-after-load 'eshell
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode))

(provide 'init-term)
