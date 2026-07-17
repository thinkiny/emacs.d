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

(defun ghostel--readonly-mode-p ()
  "Non-nil when in a readonly/navigation input mode (copy or emacs)."
  (memq ghostel--input-mode '(copy emacs)))

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
(define-key ghostel-readonly-mode-map (kbd "C-c e") #'ghostel-editor-open)
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
  (unless (ghostel--readonly-mode-p)
    (ghostel-force-redraw)
    (ghostel-copy-mode)))

(defun ghostel--scroll-session-end ()
  "Handle scroll session end."
  (setq ghostel--scroll-timer nil)
  (when (and (ghostel--readonly-mode-p)
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
