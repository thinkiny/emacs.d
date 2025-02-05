(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
              blink-cursor-interval 1
              buffers-menu-max-size 30
              case-fold-search t
              visible-bell nil
              tab-width 4
              make-backup-files nil
              auto-save-default nil
              indent-tabs-mode nil
              create-lockfiles nil
              bookmark-save-flag 1
              save-abbrevs nil
              truncate-lines t
              large-file-warning-threshold nil
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              mouse-yank-at-point t
              save-interprogram-paste-before-kill t
              scroll-preserve-screen-position 'always
              set-mark-command-repeat-pop t
              tooltip-delay 1.5
              truncate-partial-width-windows nil
              nxml-slash-auto-complete-flag t
              warning-minimum-level :error
              select-safe-coding-system-function nil
              enable-local-eval t
              line-number-mode t)

(put 'erase-buffer 'disabled nil)
(delete-selection-mode 1)
(add-hook 'after-init-hook 'transient-mark-mode)
(make-local-variable 'truncate-lines)
(global-set-key (kbd "C-c $") 'toggle-truncate-lines)

;;; Optimizations
(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 4 1024 1024))
;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly from 0.5s:
(setq idle-update-delay 1.0)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but we inhibit it there anyway. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help with performance while scrolling.
(setq redisplay-skip-fontification-on-input t)

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when *is-a-nt*
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-pipe-read-delay 0              ; faster IPC
        w32-pipe-buffer-size (* 128 1024))) ; read more at a time (was 4K)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless *is-a-mac*   (setq command-line-ns-option-alist nil))
(unless *is-a-linux* (setq command-line-x-option-alist nil))

;; Huge files
(require-package 'vlf)
(require 'vlf-setup)
(setq-default vlf-application 'dont-ask)
(setq-default vlf-batch-size 4096000)
(setq-default vlf-tune-enabled nil)

;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook 'show-paren-mode)

;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
(require-package 'expand-region)
(defun er/mark-parameter ()
  (interactive)
  (when (re-search-backward "[(\\|,]\\([^, )]+\\)" nil t)
    (goto-char (match-beginning 1))
    (set-mark (point))
    (re-search-forward "[,\\|)]")
    (goto-char (match-beginning 0))
    (exchange-point-and-mark)))

(defvar er/try-expand-list nil)
(add-hook 'c-mode-common-hook
          (lambda ()
            (add-to-list 'er/try-expand-list 'er/mark-parameter)))
(global-set-key (kbd "M-=") 'er/expand-region)

;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; multiple-cursors
(require-package 'multiple-cursors)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
(global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up

(defun sanityinc/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)

;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

(require-package 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'"  . json-mode))


;; symbol-overlay
(require-package 'symbol-overlay)
(global-set-key (kbd "M-i") 'symbol-overlay-put)
(global-set-key (kbd "M-n") 'symbol-overlay-jump-next)
(global-set-key (kbd "M-p") 'symbol-overlay-jump-prev)
(add-hook 'prog-mode-hook 'symbol-overlay-mode)

;; (define-globalized-minor-mode global-symbol-overlay-mode symbol-overlay-mode symbol-overlay-mode)
;; (global-symbol-overlay-mode)

(require 'scroll-other-window)
(fset 'yes-or-no-p 'y-or-n-p)
(setq goto-address-mail-face 'link)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;hs-minor-mode
(add-hook 'prog-mode-hook
          (lambda()
            (hs-minor-mode)
            (diminish 'hs-minor-mode)
            (local-set-key "\C-c-" 'hs-hide-block)
            (local-set-key "\C-c=" 'hs-show-block)
            (local-set-key "\C-c_" 'hs-hide-all)
            (local-set-key "\C-c+" 'hs-show-all)))

(diminish 'eldoc-mode)
(diminish 'abbrev-mode)

(require-package 'htmlize)

;; dict
(use-package bing-dict
  :init
  (defun bing-dict-at-point()
    (interactive)
    (let ((word (if (use-region-p)
                    (buffer-substring-no-properties
                     (region-beginning) (region-end))
                  (let ((text (thing-at-point 'word)))
                    (if text (substring-no-properties text))))))
      (if word
          (bing-dict-brief word)
        (message "can't find word at point"))))
  :commands (bing-dict-brief)
  :config
  (setq bing-dict-cache-auto-save t)
  :bind (:map global-map
              ("C-,"  . 'bing-dict-at-point)))

(defvar-local auto-translate-mouse-selection nil)
(defun translate-mouse-selection()
  (interactive)
  (when auto-translate-mouse-selection
    (if (eq major-mode 'nov-xwidget-webkit-mode)
        (xwidget-translate-range)
      (bing-dict-at-point))))
(advice-add #'mouse-set-region-1 :after #'translate-mouse-selection)

;; view-mode
(with-eval-after-load 'view
  (setq view-read-only t)
  (unbind-all-keys view-mode-map))

;; long line mode
(use-package so-long
  :demand t
  :config
  ;; Don't disable syntax highlighting and line numbers, or make the buffer
  ;; read-only, in `so-long-minor-mode', so we can have a basic editing
  ;; experience in them, at least. It will remain off in `so-long-mode',
  ;; however, because long files have a far bigger impact on Emacs performance.
  (delq! 'font-lock-mode so-long-minor-modes)
  (delq! 'display-line-numbers-mode so-long-minor-modes)
  (delq! 'buffer-read-only so-long-variable-overrides 'assq)
  ;; ...but at least reduce the level of syntax highlighting
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  ;; ...and insist that save-place not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; But disable everything else that may be unnecessary/expensive for large or
  ;; wide buffers.
  (appendq! so-long-minor-modes
            '(spell-fu-mode
              eldoc-mode
              highlight-numbers-mode
              better-jumper-local-mode
              ws-butler-mode
              auto-composition-mode
              undo-tree-mode
              highlight-indent-guides-mode
              hl-fill-column-mode
              ;; These are redundant on Emacs 29+
              flycheck-mode
              smartparens-mode
              smartparens-strict-mode))
  ;;(global-so-long-mode)
  )

;; tab indent
(defun tab-indent-on()
  (interactive)
  (setq-local indent-tabs-mode t)
  (setq-local c-basic-offset tab-width))

(defun tab-indent-off()
  (interactive)
  (setq-local indent-tabs-mode nil)
  (kill-local-variable 'c-basic-offset))

;; smex
(require-package 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; clipetty
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

;; ;; nxml-mode
(with-eval-after-load 'nxml-mode
  (unbind-key (kbd "C-c ]") 'nxml-mode-map))

;; disable spook
(fmakunbound 'spook)

;; disable text scale with mouse
(unbind-key (kbd "C-<wheel-down>") 'global-map)
(unbind-key (kbd "C-<wheel-up>") 'global-map)

;; native-compile
(defun native-compile-dir()
  (interactive)
  (let* ((counsel--find-file-predicate #'file-directory-p)
         (selected-directory
          (ivy-read
           "Choose directory: "
           #'read-file-name-internal
           :matcher #'counsel--find-file-matcher)))
    (native-compile-async selected-directory 'recursively)))

;; tabs
(defun set-tab-width()
  (interactive)
  (let* ((value (read-number "set tab width: ")))
    (setq-local tab-width value)
    (setq-local c-basic-offset value)))

;; Handlebars
(use-package handlebars-mode
  :mode (("\\.hbs$" . handlebars-mode)))

;; hcl-mode
(use-package hcl-mode
  :mode (("\\.conf$" . hcl-mode)))

;; dwim-shell-command
(use-package dwim-shell-command
  :demand t
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command)))

;; forward-word to beginning
(defun forward-word-begin (arg)
  "Move forward a word and end up with the point being at the beginning of the
next word.  Move point forward ARG words (backward if ARG is negative).
If ARG is omitted or nil, move point forward one word."
  (interactive "p")
  (dotimes (_ (or arg 1))
    (forward-whitespace 1)))

(provide 'init-editing-utils)
