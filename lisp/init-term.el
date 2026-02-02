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
  (define-key vterm-copy-mode-map (kbd "C-c v") 'vterm-copy-mode))

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

;; other staff
(defvar-local vterm-claude-loopback-mode-enabled nil
  "Track whether claude loopback mode is enabled.")

(defun vterm-claude-loopback-mode (arg)
  "Toggle claude loopback mode.
With positive ARG, enable. With negative ARG, disable."
  (when (and (string-prefix-p "*claude-code" (buffer-name))
             (not claude-code-ide-prevent-reflow-glitch))
    (let ((enable (> arg 0)))
      (unless (eq enable vterm-claude-loopback-mode-enabled)
        (vterm-send-string "\x1e")
        ;; (sit-for 0.1)
        )
      (setq vterm-claude-loopback-mode-enabled enable))))

;; handle claude-chill loopback
(defvar-local my/scroll-timer nil)
(defvar-local my/scrolling-p nil)

(defun my/is-scroll-command-p (cmd)
  "Return non-nil if CMD is a user-initiated scroll command."
  (or (memq cmd '(mwheel-scroll scroll-up-command scroll-down-command
                                scroll-up scroll-down mwheel-scroll-all))
      (get cmd 'scroll-command)))

(defun my/scroll-session-start ()
  "Handle scroll session start before command executes."
  (when (my/is-scroll-command-p this-command)
    (unless vterm-copy-mode
      (setq my/scrolling-p t)
      (vterm-claude-loopback-mode 1)
      (vterm-copy-mode 1))))

(defun my/scroll-session-end ()
  "Handle scroll session end."
  (setq my/scrolling-p nil)
  (setq my/scroll-timer nil)
  (when (and vterm-copy-mode (>= (window-end) (point-max)))
    (vterm-copy-mode -1)
    (vterm-claude-loopback-mode -1)))

(defun my/scroll-session-debounce ()
  "Debounce scroll session end."
  (when (my/is-scroll-command-p this-command)
    (when my/scroll-timer
      (cancel-timer my/scroll-timer))
    (let ((buf (current-buffer)))
      (setq my/scroll-timer
            (run-with-timer 0.1 nil
                            (lambda ()
                              (when (buffer-live-p buf)
                                (with-current-buffer buf
                                  (my/scroll-session-end)))))))))

;; add vterm hook
(defun my-vterm-mode-hook()
  (add-hook 'pre-command-hook #'my/scroll-session-start nil t)
  (add-hook 'post-command-hook #'my/scroll-session-debounce nil t)
  )

(add-hook 'vterm-mode-hook #'my-vterm-mode-hook)

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
