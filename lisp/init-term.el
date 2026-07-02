;; -*- lexical-binding: t; -*-

;;; Shared terminal utilities

(defconst term--claude-previous-re "^\\(?:❯ \\|⏺ \\)"
  "Regex matching Claude Code prompt lines.")

(defun term--claude-buffer-p ()
  "Return non-nil if current buffer is a Claude Code terminal."
  (string-prefix-p "*claude-code" (buffer-name)))

(defun term--goto-previous-claude-prompt ()
  "Search backward for a Claude Code prompt line (❯ or ⏺)."
  (beginning-of-line)
  (when (re-search-backward term--claude-previous-re nil t)
    (beginning-of-line))
  (unless (pos-visible-in-window-p (point) (selected-window))
    (recenter 0)))

;;; ghostel

(use-package ghostel
  :config
  (setq ghostel-tramp-shell-integration t)
  (add-to-list 'ghostel-tramp-shells '("rpc" "/bin/bash")))

(defun ghostel-send-C-v ()
  "Send C-v to the terminal."
  (interactive)
  (ghostel--send-string "\x16"))

(defun ghostel-semi-char-kill-line ()
  "Save cursor-to-EOL text to Emacs kill ring, then send C-k to terminal."
  (interactive)
  (let ((cursor ghostel--cursor-char-pos))
    (when cursor
      (save-excursion
        (goto-char cursor)
        (let ((eol (line-end-position)))
          (when (< cursor eol)
            (kill-new (buffer-substring-no-properties cursor eol)))))))
  (ghostel-send-string "\x0b"))

(defun ghostel-readonly-scroll-up ()
  "Scroll up, exiting readonly mode when reaching buffer end."
  (interactive)
  (condition-case nil
      (progn
        (precision-scroll-up-page)
        (when (pos-visible-in-window-p (point-max))
          (ghostel-readonly-exit)))
    (error (ghostel-readonly-exit))))

(defun ghostel-mode-backward-prompt ()
  "In *claude-code buffers, enter copy mode and jump to previous prompt (❯ or ⏺).
Otherwise, enter copy mode and go to window start."
  (interactive)
  (ghostel-force-redraw)
  (ghostel-copy-mode)
  (if (term--claude-buffer-p)
      (term--goto-previous-claude-prompt)
    (goto-char (window-start))))

(defun ghostel-copy-mode-goto-buffer-end ()
  "Go to end of buffer if region is active, otherwise exit copy mode."
  (interactive)
  (if (use-region-p)
      (goto-char (point-max))
    (ghostel-readonly-exit)))

(defun ghostel-copy-mode-quit ()
  "Deactivate region, otherwise exit ghostel readonly mode."
  (interactive)
  (selection/quit #'ghostel-readonly-exit))

;; ghostel semi-char mode keybindings
(define-key ghostel-semi-char-mode-map (kbd "M-w") #'kill-ring-save)
(define-key ghostel-semi-char-mode-map (kbd "C-e") #'ghostel--send-event)
(define-key ghostel-semi-char-mode-map (kbd "C-c v") #'ghostel-copy-mode)
(define-key ghostel-semi-char-mode-map (kbd "C-g") #'keyboard-quit)
(define-key ghostel-semi-char-mode-map (kbd "C-c C-g") #'ghostel-send-C-g)
(define-key ghostel-semi-char-mode-map (kbd "C-v") #'scroll-up-command)
(define-key ghostel-semi-char-mode-map (kbd "M-v") #'scroll-down-command)
(define-key ghostel-semi-char-mode-map (kbd "M-<") #'ghostel-mode-backward-prompt)
(define-key ghostel-semi-char-mode-map (kbd "M->") #'end-of-buffer)
(define-key ghostel-semi-char-mode-map (kbd "C-c C-v") #'ghostel-send-C-v)
(define-key ghostel-semi-char-mode-map (kbd "C-k") #'ghostel-semi-char-kill-line)
(define-key ghostel-semi-char-mode-map [remap pixel-scroll-precision] #'ghostel-pixel-scroll-precision)

(require 'ghostel-editor)
(define-key ghostel-semi-char-mode-map (kbd "C-c e") #'ghostel-editor-open)

;; ghostel readonly/copy mode keybindings
(define-key ghostel-readonly-mode-map (kbd "M-w") 'kill-ring-save)
(define-key ghostel-readonly-mode-map (kbd "<SPC>") #'selection/toggle-mark)
(define-key ghostel-readonly-mode-map (kbd "C-c v") #'ghostel-readonly-exit)
(define-key ghostel-readonly-mode-map (kbd "=") #'selection/expand)
(define-key ghostel-readonly-fast-exit-mode-map (kbd "C-g") #'ghostel-copy-mode-quit)
(define-key ghostel-readonly-mode-map (kbd ",") #'translate-at-point)
(define-key ghostel-readonly-mode-map (kbd "n") #'precision-scroll-next-line)
(define-key ghostel-readonly-mode-map (kbd "p") #'precision-scroll-prev-line)
(define-key ghostel-readonly-mode-map (kbd "j") #'precision-scroll-next-line)
(define-key ghostel-readonly-mode-map (kbd "k") #'precision-scroll-prev-line)
(define-key ghostel-readonly-mode-map (kbd "b") #'backward-word)
(define-key ghostel-readonly-mode-map (kbd "f") #'forward-word-begin)
(define-key ghostel-readonly-mode-map (kbd "a") #'backward-sentence)
(define-key ghostel-readonly-mode-map (kbd "e") #'forward-sentence)
(define-key ghostel-readonly-mode-map (kbd "v") #'ghostel-readonly-scroll-up)
(define-key ghostel-readonly-mode-map (kbd "C-v") #'ghostel-readonly-scroll-up)
(define-key ghostel-readonly-mode-map (kbd "M-v") #'precision-scroll-down-page)
(define-key ghostel-readonly-mode-map (kbd "M-<") #'term-copy-mode-backward-prompt)
(define-key ghostel-readonly-mode-map (kbd "M->") #'ghostel-copy-mode-goto-buffer-end)
(define-key ghostel-readonly-mode-map [remap pixel-scroll-precision] #'ghostel-pixel-scroll-precision)

;; ghostel pixel-scroll
(defvar-local ghostel--scroll-timer nil)

(defun ghostel--pixel-scroll-precision-up ()
  (when ghostel--scroll-timer
    (cancel-timer ghostel--scroll-timer)
    (setq ghostel--scroll-timer nil))
  (unless (memq ghostel--input-mode '(copy emacs))
    (ghostel-force-redraw)
    (ghostel-copy-mode)))

(defun ghostel--scroll-session-end ()
  "Handle scroll session end."
  (setq ghostel--scroll-timer nil)
  (when (and (memq ghostel--input-mode '(copy emacs))
             (>= (window-end) (point-max)))
    (ghostel-readonly-exit)))

(defun ghostel-pixel-scroll-precision (event)
  "Wrapper for pixel-scroll-precision in ghostel - handles copy mode."
  (interactive "e")
  (when (nth 4 event)
    (let ((delta (round (cdr (nth 4 event)))))
      (unless (zerop delta)
        (when (> delta 0)
          (ghostel--pixel-scroll-precision-up))
        (pixel-scroll-precision event)
        (when (< delta 0)
          (ghostel--pixel-scroll-precision-down))))))

(defun ghostel--pixel-scroll-precision-down ()
  (when ghostel--scroll-timer
    (cancel-timer ghostel--scroll-timer))
  (let ((buf (current-buffer)))
    (setq ghostel--scroll-timer
          (run-with-timer 0.05 nil
                          (lambda ()
                            (when (buffer-live-p buf)
                              (with-current-buffer buf
                                (ghostel--scroll-session-end))))))))


;;; vterm
;; https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking
;; if [ "$TERM" = "xterm-256color" ]; then
;;     PS1=$PS1'\[$(vterm_prompt_end)\]'
;; fi

(use-package vterm
  :defer t
  :commands (vterm-mode vterm)
  :config
  (add-to-list 'vterm-tramp-shells '("ssh" "/bin/bash -l"))
  (add-to-list 'vterm-tramp-shells '("rpc" "/bin/bash -l"))
  (ignore-tramp-ssh-control-master 'vterm-mode)
  (setq vterm-always-compile-module t)
  (setq vterm-min-window-width 40)
  (setq vterm-max-scrollback 10000)
  (setq vterm-shell (concat shell-file-name " -l"))

  (define-key vterm-mode-map (kbd "M-w") 'kill-ring-save)
  (define-key vterm-mode-map (kbd "C-c v") 'vterm-copy-mode)
  (define-key vterm-mode-map (kbd "C-v") 'scroll-up-command)
  (define-key vterm-mode-map (kbd "C-b") 'vterm-send-key-left)
  (define-key vterm-mode-map (kbd "M-v") 'scroll-down-command)
  (define-key vterm-mode-map (kbd "C-c C-v") 'vterm--self-insert)
  (define-key vterm-mode-map [remap pixel-scroll-precision] #'vterm-pixel-scroll-precision)
  (define-key vterm-mode-map (kbd "M-<") #'vterm-mode-goto-previous)
  (define-key vterm-mode-map (kbd "M->") #'vterm-mode-goto-buffer-end)
  (define-key vterm-mode-map (kbd "S-<return>") #'vterm-send-newline-escaped)
  (define-key vterm-mode-map (kbd "C-<escape>") #'vterm-send-escape-key)
  (define-key vterm-copy-mode-map (kbd "<SPC>") #'selection/toggle-mark)
  (define-key vterm-copy-mode-map (kbd "C-c v") 'vterm-copy-mode)
  (define-key vterm-copy-mode-map (kbd "=") #'selection/expand)
  (define-key vterm-copy-mode-map (kbd "C-g") #'vterm-copy-mode-quit)
  (define-key vterm-copy-mode-map (kbd ",") #'translate-at-point)
  (define-key vterm-copy-mode-map (kbd "n") #'precision-scroll-next-line)
  (define-key vterm-copy-mode-map (kbd "p") #'precision-scroll-prev-line)
  (define-key vterm-copy-mode-map (kbd "j") #'precision-scroll-next-line)
  (define-key vterm-copy-mode-map (kbd "k") #'precision-scroll-prev-line)
  (define-key vterm-copy-mode-map (kbd "b") #'backward-word)
  (define-key vterm-copy-mode-map (kbd "f") #'forward-word-begin)
  (define-key vterm-copy-mode-map (kbd "a") #'backward-sentence)
  (define-key vterm-copy-mode-map (kbd "e") #'forward-sentence)
  (define-key vterm-copy-mode-map (kbd "C-v") #'vterm-copy-mode-scroll-up)
  (define-key vterm-copy-mode-map (kbd "v") #'vterm-copy-mode-scroll-up)
  (define-key vterm-copy-mode-map [remap pixel-scroll-precision] #'vterm-pixel-scroll-precision)
  (define-key vterm-copy-mode-map (kbd "M->") #'vterm-copy-mode-goto-buffer-end)
  (define-key vterm-copy-mode-map (kbd "M-<") #'term-copy-mode-backward-prompt))

(defun vterm-send-key-left()
  "Fix Ctrl-B not working in claude code in some cases."
  (interactive)
  (vterm-send-key "<left>"))

(defun vterm-send-newline-escaped ()
  "Send backslash + return to insert a newline in terminal."
  (interactive)
  (vterm-send-string "\\")
  (sit-for 0.1)
  (vterm-send-return))

(defun vterm-send-escape-key ()
  "Send escape key to terminal."
  (interactive)
  (vterm-send-escape))

(defun vterm-mode-goto-previous ()
  "Enter `vterm-copy-mode' and go to the start of the window.
In *claude-code buffers, go to the last prompt line (❯ or ⏺) instead."
  (interactive)
  (vterm-copy-mode 1)
  (let ((buf (current-buffer)))
    (run-at-time 0.1 nil
                 (lambda ()
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (if (term--claude-buffer-p)
                           (term--goto-previous-claude-prompt)
                         (goto-char (window-start)))))))))

(defun vterm-mode-goto-buffer-end ()
  "Go to end of buffer."
  (interactive)
  (vterm-reset-cursor-point))

(defun vterm-copy-mode-scroll-up ()
  "Scroll up, exiting copy mode when reaching buffer end."
  (interactive)
  (condition-case nil
      (progn
        (precision-scroll-up-page)
        (when (pos-visible-in-window-p (point-max))
          (vterm-copy-mode -1)
          (vterm-reset-cursor-point)))
    (error (vterm-copy-mode -1)
           (vterm-reset-cursor-point))))

(defun vterm-copy-mode-goto-buffer-end ()
  "Go to end of buffer if region is active, otherwise quit `vterm-copy-mode."
  (interactive)
  (if (use-region-p)
      (goto-char (point-max))
    (vterm-copy-mode -1)
    (vterm-reset-cursor-point)))

(defun vterm-copy-mode-quit ()
  "Deactivate region, otherwise exit `vterm-copy-mode'."
  (interactive)
  (selection/quit #'vterm-copy-mode))

(defun term-copy-mode-backward-prompt ()
  "In *claude-code buffers, jump to the last or previous prompt (❯ or ⏺).
Otherwise go to the beginning of the buffer."
  (interactive)
  (if (term--claude-buffer-p)
      (progn
        (beginning-of-line)
        (if (looking-at term--claude-previous-re)
            (progn (forward-line -1) (term--goto-previous-claude-prompt))
          (term--goto-previous-claude-prompt)))
    (goto-char (point-min))))

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
        (if (> delta 0)
            (vterm--pixel-scroll-precision-up))
        (pixel-scroll-precision event)
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

(use-package vterm-editor
  :after vterm
  :bind (:map vterm-mode-map
              ("C-c e" . vterm-editor-open)))

;;; Shared appearance

(with-eval-after-load-theme
 'term
 (when (theme-dark-p)
   (set-face-foreground 'term-color-blue "skyblue3")
   (set-face-foreground 'term-color-red "IndianRed1")))

;;; counsel-term

(require 'counsel-term)
(global-set-key (kbd "C-x t") 'counsel-term)

(provide 'init-term)
