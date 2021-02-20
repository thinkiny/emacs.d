;;Remote Directory Tracking: https://www.emacswiki.org/emacs/AnsiTermHints#h5o-5

;; counsel-term
(require 'counsel-term)
(global-set-key (kbd "C-x t") 'counsel-term)

;; term-mode key-bindings
(defcustom term-unbind-key-list
  '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")
  "The key list that will need to be unbind."
  :type 'list
  :group 'term)

(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

(defun term-send-return ()
  "Use term-send-raw-string \"\C-m\" instead term-send-input.
Because term-send-input have bug that will duplicate input when you C-a and C-m in terminal."
  (interactive)
  (term-send-raw-string "\C-m")
  )

(defun term-send-backward-kill-word ()
  "Backward kill word in term mode."
  (interactive)
  (term-send-raw-string "\C-w"))

(defun term-send-forward-kill-word ()
  "Kill word in term mode."
  (interactive)
  (term-send-raw-string "\ed"))

(defun term-send-backward-word ()
  "Move backward word in term mode."
  (interactive)
  (term-send-raw-string "\eb"))

(defun term-send-forward-word ()
  "Move forward word in term mode."
  (interactive)
  (term-send-raw-string "\ef"))

(defun term-send-reverse-search-history ()
  "Search history reverse."
  (interactive)
  (term-send-raw-string "\C-r"))

(defun term-send-delete-word ()
  "Delete word in term mode."
  (interactive)
  (term-send-raw-string "\ed"))

(defun term-send-quote ()
  "Quote the next character in term-mode.
Similar to how `quoted-insert' works in a regular buffer."
  (interactive)
  (term-send-raw-string "\C-v"))

(defun term-send-M-x ()
  "Type M-x in term-mode."
  (interactive)
  (term-send-raw-string "\ex"))

(defcustom term-bind-key-alist
  '(
    ("C-c C-c" . term-interrupt-subjob)
    ("C-c C-e" . term-send-esc)
    ("C-p" . previous-line)
    ("C-n" . next-line)
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward)
    ("C-m" . term-send-return)
    ("C-y" . term-paste)
    ("M-f" . term-send-forward-word)
    ("M-b" . term-send-backward-word)
    ("M-o" . term-send-backspace)
    ("M-p" . term-send-up)
    ("M-n" . term-send-down)
    ("M-M" . term-send-forward-kill-word)
    ("M-N" . term-send-backward-kill-word)
    ("<C-backspace>" . term-send-backward-kill-word)
    ("M-r" . term-send-reverse-search-history)
    ("M-d" . term-send-delete-word)
    ("M-," . term-send-raw)
    ("M-." . comint-dynamic-complete))
  "The key alist that will need to be bind.
If you do not like default setup, modify it, with (KEY . COMMAND) format."
  :type 'alist
  :group 'term)

(defun term-keystroke-setup ()
  "Keystroke setup of `term-char-mode'.
By default, the key bindings of `term-char-mode' conflict with user's keystroke.
So this function unbinds some keys with `term-raw-map',
and binds some keystroke with `term-raw-map'."
  (let (bind-key bind-command)
    ;; Unbind base key that conflict with user's keys-tokes.
    (cl-dolist (unbind-key term-unbind-key-list)
      (cond
       ((stringp unbind-key) (setq unbind-key (read-kbd-macro unbind-key)))
       ((vectorp unbind-key) nil)
       (t (signal 'wrong-type-argument (list 'array unbind-key))))
      (define-key term-raw-map unbind-key nil))
    ;; Add some i use keys.
    ;; If you don't like my keystroke,
    ;; just modified `term-bind-key-alist'
    (cl-dolist (element term-bind-key-alist)
      (setq bind-key (car element))
      (setq bind-command (cdr element))
      (cond
       ((stringp bind-key) (setq bind-key (read-kbd-macro bind-key)))
       ((vectorp bind-key) nil)
       (t (signal 'wrong-type-argument (list 'array bind-key))))
      (define-key term-raw-map bind-key bind-command))))

(add-hook 'term-mode-hook (lambda ()
                            (term-keystroke-setup)
                            (define-key term-raw-map (kbd "C-c C-y") 'term-paste)
                            (define-key term-raw-map (kbd "C-c C-k") 'term-char-mode)
                            (define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)))

(provide 'init-term)
