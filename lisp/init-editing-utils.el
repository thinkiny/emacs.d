;; -*- lexical-binding: t; -*-

;;----------------------------------------------------------------------------
;; Core Editing Defaults
;;----------------------------------------------------------------------------
(setq load-prefer-newer t)

(setq-default tab-width 4
              indent-tabs-mode nil
              case-fold-search t
              truncate-lines t
              truncate-partial-width-windows nil
              mouse-yank-at-point t
              save-interprogram-paste-before-kill t
              scroll-preserve-screen-position 'always
              set-mark-command-repeat-pop t)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))

(put 'erase-buffer 'disabled nil)
(delete-selection-mode 1)
(add-hook 'after-init-hook 'transient-mark-mode)

(global-set-key (kbd "C-c $") 'toggle-truncate-lines)

;;----------------------------------------------------------------------------
;; Paren and Sexp
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook 'show-paren-mode)

(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up

;;----------------------------------------------------------------------------
;; Selection and Marking
;;----------------------------------------------------------------------------
(use-package expand-region
  :bind ("M-=" . er/expand-region)
  :config
  (defun er/mark-parameter ()
    (interactive)
    (when (re-search-backward "[(,]\\([^, )]+\\)" nil t)
      (goto-char (match-beginning 1))
      (set-mark (point))
      (re-search-forward "[,)]")
      (goto-char (match-beginning 0))
      (exchange-point-and-mark)))
  (add-hook 'c-mode-common-hook
            (lambda ()
              (make-local-variable 'er/try-expand-list)
              (push 'er/mark-parameter er/try-expand-list))))

(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c m r" . set-rectangular-region-anchor)
         ("C-c m c" . mc/edit-lines)
         ("C-c m e" . mc/edit-ends-of-lines)
         ("C-c m a" . mc/edit-beginnings-of-lines)))

;; Progressive selection expansion (word -> sentence)
(defvar-local selection--saved-caret nil
  "Original point before `selection/expand'. `selection/quit' restores to this.")

(defun selection--sentence-bounds ()
  "Return (START . END) of the sentence at point, limited by empty lines."
  (save-excursion
    (save-restriction
      (narrow-to-region
       (save-excursion (if (re-search-backward "^\n" nil t) (point) (point-min)))
       (save-excursion (if (re-search-forward  "^\n" nil t) (point) (point-max))))
      (let ((start (if (re-search-backward "[.!?。！？]" nil t)
                       (progn (forward-char) (point))
                     (point-min)))
            (end   (if (re-search-forward  "[.!?。！？]" nil t)
                       (point)
                     (point-max))))
        (goto-char start)
        (skip-chars-forward " \t\n")
        (when (< (point) end)
          (cons (point) end))))))

(defun selection/expand ()
  "Expand selection progressively: word -> sentence."
  (interactive)
  (if (not (region-active-p))
      (progn
        (setq selection--saved-caret (point))
        (skip-syntax-backward "w")
        (mark-word 1))
    (when (eq last-command 'selection/expand)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (unless (string-match-p "[.!?。！？]\\|\\S-\\s-+\\S-" text)
          (goto-char (region-beginning))
          (when-let* ((bounds (selection--sentence-bounds)))
            (push-mark (car bounds) nil t)
            (goto-char (cdr bounds))))))))

(defun selection/toggle-mark ()
  "Toggle the mark, like `set-mark-command'."
  (interactive)
  (setq selection--saved-caret nil)
  (if (region-active-p)
      (deactivate-mark)
    (push-mark (point) nil t)))

(defun selection/quit (&optional quit-function)
  "Deactivate mark; if `selection/expand' set saved caret, restore point there.
When no region is active, call QUIT-FUNCTION if non-nil, else `keyboard-quit'."
  (interactive)
  (if (region-active-p)
      (progn
        (when selection--saved-caret
          (goto-char selection--saved-caret)
          (setq selection--saved-caret nil))
        (deactivate-mark))
    (if quit-function
        (funcall quit-function)
      (keyboard-quit))))

;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;----------------------------------------------------------------------------
;; Line and Indent
;;----------------------------------------------------------------------------
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

(defun tab-indent-on()
  (interactive)
  (setq-local indent-tabs-mode t)
  (setq-local c-basic-offset tab-width))

(defun tab-indent-off()
  (interactive)
  (setq-local indent-tabs-mode nil)
  (kill-local-variable 'c-basic-offset))

(defun set-tab-width()
  (interactive)
  (let* ((value (read-number "set tab width: ")))
    (setq-local tab-width value)
    (setq-local c-basic-offset value)))

;;----------------------------------------------------------------------------
;; Navigation
;;----------------------------------------------------------------------------
(require 'scroll-other-window)

(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)))

(defun forward-word-begin ()
  "Move point to the beginning of the next word.
When region is active, extend selection to end of next word."
  (interactive)
  (if (region-active-p)
      (let ((anchor (region-beginning))
            (end (region-end)))
        (goto-char end)
        (forward-word 1)
        (push-mark anchor nil t))
    (forward-word 1)
    (forward-word 1)
    (backward-word 1)))

;;----------------------------------------------------------------------------
;; Code Folding
;;----------------------------------------------------------------------------
(require 'init-fold)
(fold-global-mode)

;;----------------------------------------------------------------------------
;; File and Buffer Management
;;----------------------------------------------------------------------------
(setq-default bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
              make-backup-files nil
              auto-save-default nil
              auto-save-list-file-name nil
              create-lockfiles nil
              save-abbrevs nil
              large-file-warning-threshold nil)

;; Huge files
(use-package vlf
  :config
  (require 'vlf-setup)
  (setq-default vlf-application 'dont-ask
                vlf-batch-size 4096000
                vlf-tune-enabled nil))

;; long line mode
(use-package so-long
  :config
  (setq so-long-minor-modes (delq 'font-lock-mode so-long-minor-modes))
  (setq so-long-variable-overrides (delq (assq 'buffer-read-only so-long-variable-overrides) so-long-variable-overrides))
  ;; ...but at least reduce the level of syntax highlighting
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  ;; ...and insist that save-place not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; But disable everything else that may be unnecessary/expensive for large or
  ;; wide buffers.
  (setq so-long-minor-modes
        (append so-long-minor-modes
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
                  smartparens-strict-mode)))
  ;;(global-so-long-mode)
  )

;; auto-revert
(require 'autorevert)
(setq auto-revert-mode-text "")
(setq auto-revert-avoid-polling t)
(setq auto-revert-remote-files t)
(defun enable-auto-revert ()
  "Enable 'auto-revert-mode, configuring interval based on file location."
  (if (file-remote-p default-directory)
      (setq-local auto-revert-interval 4)
    (setq-local auto-revert-interval 2))
  (auto-revert-mode 1))

(defun toggle-auto-revert ()
  "Toggle 'auto-revert-mode, configuring interval based on file location."
  (interactive)
  (if auto-revert-mode
      (auto-revert-mode -1)
    (enable-auto-revert))
  (message "Auto-revert %s" (if auto-revert-mode "enabled" "disabled")))

;; view-mode
(with-eval-after-load 'view
  (setq view-read-only t)
  (unbind-all-keys view-mode-map))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;----------------------------------------------------------------------------
;; Performance
;;----------------------------------------------------------------------------
(setq-default buffers-menu-max-size 30
              max-mini-window-height 0.4
              visible-bell nil
              warning-minimum-level :error
              cursor-in-non-selected-windows nil
              line-number-mode -1
              tooltip-delay 1.5
              select-safe-coding-system-function nil
              enable-local-eval :safe)

(setq read-process-output-max (* 4 1024 1024))
(setq auto-mode-case-fold nil)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq ffap-machine-p-known 'reject)
(setq frame-inhibit-implied-resize t)
(setq idle-update-delay 1.0)
(setq inhibit-compacting-font-caches t)
(setq redisplay-skip-fontification-on-input t)
(setq kill-do-not-save-duplicates t)
(setq window-combination-resize t)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless *is-a-mac*   (setq command-line-ns-option-alist nil))
(unless *is-a-linux* (setq command-line-x-option-alist nil))

;; jit-lock
(setq jit-lock-stealth-time 1)
(setq jit-lock-stealth-nice 1)
(setq jit-lock-defer-time 0)
(setq jit-lock-chunk-size 4096)

;;----------------------------------------------------------------------------
;; External Tools
;;----------------------------------------------------------------------------
;; direnv
(use-package direnv
  :config
  (setq direnv-always-show-summary nil)
  (add-hook 'find-file-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (direnv-update-directory-environment)))))

;; dwim-shell-command
(use-package dwim-shell-command
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command)))

(defun map-buffer-to-local-file (local-file)
  "Map the current buffer to a `LOCAL-FILE."
  (when-let* ((file (and local-file (expand-file-name local-file)))
              (dir (file-name-directory file))
              ((file-directory-p dir)))
    (setq default-directory dir
          buffer-file-name file)
    (add-hook 'kill-buffer-query-functions
              (lambda () (set-buffer-modified-p nil) t) nil t)))

;;----------------------------------------------------------------------------
;; Text Utilities
;;----------------------------------------------------------------------------
(defun fix-quotes ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[""]" nil t) (replace-match "\""))
    (goto-char (point-min))
    (while (re-search-forward "[''']" nil t) (replace-match "'"))))

(global-set-key (kbd "C-c q f") #'fix-quotes)

;; text-mode
(add-hook 'text-mode-hook 'visual-line-mode)


;;----------------------------------------------------------------------------
;; Miscellaneous
;;----------------------------------------------------------------------------
(fmakunbound 'spook)

;; disable text scale with mouse
(unbind-key (kbd "C-<wheel-down>") 'global-map)
(unbind-key (kbd "C-<wheel-up>") 'global-map)

(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init-editing-utils)
