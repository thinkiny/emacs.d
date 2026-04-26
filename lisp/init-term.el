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
  (add-to-list 'vterm-tramp-shells '("rpc" "/bin/bash -l"))
  (ignore-tramp-ssh-control-master 'vterm-mode)
  (setq vterm-always-compile-module t)
  (setq vterm-min-window-width 60)
  (setq vterm-max-scrollback 10000)
  (setq vterm-shell (concat shell-file-name " -l"))

  ;; fix Ctrl-B not working in claude code in some cases
  (defun vterm-send-key-left()
    (interactive)
    (vterm-send-key "<left>"))

  ;; key bindings
  (define-key vterm-mode-map (kbd "M-w") 'kill-ring-save)
  (define-key vterm-mode-map (kbd "C-c i") 'vterm-copy-mode)
  (define-key vterm-mode-map (kbd "C-v") 'scroll-up-command)
  (define-key vterm-mode-map (kbd "C-b") 'vterm-send-key-left)
  (define-key vterm-mode-map (kbd "M-v") 'scroll-down-command)
  (define-key vterm-mode-map (kbd "C-c C-v") 'vterm--self-insert)
  (define-key vterm-mode-map [remap pixel-scroll-precision] #'vterm-pixel-scroll-precision)
  (define-key vterm-copy-mode-map (kbd "C-c i") 'vterm-copy-mode)
  (define-key vterm-copy-mode-map (kbd "=") #'selection/expand)
  (define-key vterm-copy-mode-map (kbd "C-g") #'vterm-copy-mode)
  (define-key vterm-copy-mode-map (kbd ",") #'translate-at-point)
  (define-key vterm-copy-mode-map (kbd "n") #'precision-scroll-next-line)
  (define-key vterm-copy-mode-map (kbd "p") #'precision-scroll-prev-line)
  (define-key vterm-copy-mode-map (kbd "j") #'precision-scroll-next-line)
  (define-key vterm-copy-mode-map (kbd "k") #'precision-scroll-prev-line)
  (define-key vterm-copy-mode-map (kbd "b") #'backward-word)
  (define-key vterm-copy-mode-map (kbd "f") #'forward-word-begin)
  (define-key vterm-copy-mode-map (kbd "a") #'beginning-of-line)
  (define-key vterm-copy-mode-map (kbd "e") #'end-of-line)
  (define-key vterm-copy-mode-map [remap pixel-scroll-precision] #'vterm-pixel-scroll-precision)

(defun vterm--get-directory(path)
  "Get normalized directory to PATH, handling TRAMP paths specifically,
else mirroring original vterm logic."
  (when path
    (if (string-match "^\\(.*?\\)@\\(.*?\\):\\(.*?\\)$" path)
        (let ((dir (match-string 3 path))
              (remote-prefix (file-remote-p default-directory)))
          (if remote-prefix
              (file-name-as-directory (concat remote-prefix dir))
            (file-name-as-directory dir)))
      (file-name-as-directory path))))
)

(use-package vterm-editor
  :after vterm
  :bind (:map vterm-mode-map
         ("C-c e" . vterm-editor-open)))

;; vterm--pixel-scroll
(defvar-local vterm--scroll-timer nil)
(defun vterm--pixel-scroll-precision-up ()
  (when vterm--scroll-timer
    (cancel-timer vterm--scroll-timer)
    (setq vterm--scroll-timer nil))
  (when (and (derived-mode-p 'vterm-mode)
             (not vterm-copy-mode))
    (vterm-copy-mode 1)))

(defun vterm--scroll-session-end ()
  "Handle scroll session end."
  (setq vterm--scroll-timer nil)
  (when (and (derived-mode-p 'vterm-mode)
             vterm-copy-mode
             (>= (window-end) (point-max)))
    (vterm-copy-mode -1)))

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

(defun vterm--pixel-scroll-precision-down ()
  (when vterm--scroll-timer
    (cancel-timer vterm--scroll-timer))
  (let ((buf (current-buffer)))
    (setq vterm--scroll-timer
          (run-with-timer 0.05 nil
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
(use-package eat
  :config
  (setq eat-term-name "xterm-256color")
  (ignore-tramp-ssh-control-master 'eat-exec)
  (with-eval-after-load 'eshell
    (eat-eshell-mode)
    (eat-eshell-visual-command-mode)))

(provide 'init-term)
