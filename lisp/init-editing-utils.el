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
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              mouse-yank-at-point t
              save-interprogram-paste-before-kill t
              scroll-preserve-screen-position 'always
              set-mark-command-repeat-pop t
              tooltip-delay 1.5
              truncate-lines nil
              truncate-partial-width-windows nil
              nxml-slash-auto-complete-flag t
              warning-minimum-level :error
              line-number-mode t)

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

(delete-selection-mode 1)
(diminish 'visual-line-mode)

(add-hook 'after-init-hook 'transient-mark-mode)

(global-set-key "\C-c$" 'toggle-truncate-lines)

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

(require-package 'highlight-symbol)
(setq highlight-symbol-idle-delay 1)
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(add-hook 'org-mode-hook 'highlight-symbol-nav-mode)
(after-load 'highlight-symbol
  (diminish 'highlight-symbol-mode))

(require 'scroll-other-window)
(fset 'yes-or-no-p 'y-or-n-p)
(setq goto-address-mail-face 'link)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;hs-minor-mode
(add-hook 'prog-mode-hook
          '(lambda()
             (hs-minor-mode)
             (diminish 'hs-minor-mode)
             (local-set-key "\C-c-" 'hs-hide-block)
             (local-set-key "\C-c=" 'hs-show-block)
             (local-set-key "\C-c_" 'hs-hide-all)
             (local-set-key "\C-c+" 'hs-show-all)))

(diminish 'eldoc-mode)
(diminish 'abbrev-mode)

(require-package 'htmlize)

;;(global-set-key "\C-w" 'clipboard-kill-region)
;;(global-set-key "\M-w" 'clipboard-kill-ring-save)
;;(global-set-key "\C-y" 'clipboard-yank)
(setq xref-prompt-for-identifier nil)
(global-set-key (kbd "M-[") #'xref-pop-marker-stack)
(global-set-key (kbd "M-,") #'xref-find-references)

(require-package 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(use-package bing-dict
  :config
  (setq bing-dict-cache-auto-save t)
  :bind (:map global-map
              ("C-?"  . 'bing-dict-brief)))

(defun set-proxy()
  (interactive)
  (setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
     ("http" . "127.0.0.1:1087")
     ("https" . "127.0.0.1:1087"))))

(defun unset-proxy()
  (interactive)
  (setq url-proxy-services nil))

;; view-mode
(after-load 'view
  (setq view-read-only t)
  (unset-all-keys view-mode-map))

;; long line mode
(unless (fboundp 'global-so-long-mode)
  (require-package 'so-long))
(global-so-long-mode 1)

;; tab indent
(defun tab-indent-on()
  (interactive)
  (setq-local indent-tabs-mode t)
  (setq-local c-basic-offset tab-width))

(defun tab-indent-off()
  (interactive)
  (setq-local indent-tabs-mode nil)
  (kill-local-variable c-basic-offset))

(provide 'init-editing-utils)
