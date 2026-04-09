;; -*- lexical-binding: t; -*-

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
              buffers-menu-max-size 30
              case-fold-search t
              max-mini-window-height 0.4
              visible-bell nil
              tab-width 4
              make-backup-files nil
              auto-save-default nil
              indent-tabs-mode nil
              create-lockfiles nil
              save-abbrevs nil
              truncate-lines t
              large-file-warning-threshold nil
              mouse-yank-at-point t
              save-interprogram-paste-before-kill t
              scroll-preserve-screen-position 'always
              set-mark-command-repeat-pop t
              tooltip-delay 1.5
              truncate-partial-width-windows nil
              warning-minimum-level :error
              select-safe-coding-system-function nil
              enable-local-eval t
              line-number-mode -1)

(put 'erase-buffer 'disabled nil)
(delete-selection-mode 1)
(add-hook 'after-init-hook 'transient-mark-mode)
(make-local-variable 'truncate-lines)
(global-set-key (kbd "C-c $") 'toggle-truncate-lines)
(global-set-key (kbd "<RET>") #'newline-and-indent)

;;; Optimizations
(setq read-process-output-max (* 4 1024 1024))
(setq auto-mode-case-fold nil)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq ffap-machine-p-known 'reject)
(setq frame-inhibit-implied-resize t)
(setq idle-update-delay 1.0)
(setq inhibit-compacting-font-caches t)
(setq redisplay-skip-fontification-on-input t)
(setq kill-do-not-save-duplicates t)
(setq window-combination-resize t)

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
(use-package vlf
  :config
  (require 'vlf-setup)
  (setq-default vlf-application 'dont-ask
                vlf-batch-size 4096000
                vlf-tune-enabled nil))

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
(use-package expand-region
  :bind ("M-=" . er/expand-region)
  :config
  (defun er/mark-parameter ()
    (interactive)
    (when (re-search-backward "[(\\|,]\\([^, )]+\\)" nil t)
      (goto-char (match-beginning 1))
      (set-mark (point))
      (re-search-forward "[,\\|)]")
      (goto-char (match-beginning 0))
      (exchange-point-and-mark)))
  (add-hook 'c-mode-common-hook
            (lambda ()
              (make-local-variable 'er/try-expand-list)
              (push 'er/mark-parameter er/try-expand-list))))

;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c m r" . set-rectangular-region-anchor)
         ("C-c m c" . mc/edit-lines)
         ("C-c m e" . mc/edit-ends-of-lines)
         ("C-c m a" . mc/edit-beginnings-of-lines)))

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

(use-package json-mode
  :mode "\\.json\\'"
  :bind (:map json-mode-map
              ("C-c C-f" . json-pretty-print-buffer)))

;; symbol-overlay
(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)))

;; (define-globalized-minor-mode global-symbol-overlay-mode symbol-overlay-mode symbol-overlay-mode)
;; (global-symbol-overlay-mode)

(require 'scroll-other-window)
(fset 'yes-or-no-p 'y-or-n-p)
(setq goto-address-mail-face 'link)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;hs-minor-mode
(defun my/hs-minor-mode()
  (hs-minor-mode)
  (local-set-key "\C-c-" 'hs-hide-block)
  (local-set-key "\C-c=" 'hs-show-block)
  (local-set-key "\C-c_" 'hs-hide-all)
  (local-set-key "\C-c+" 'hs-show-all))

(add-hook 'prog-mode-hook  #'my/hs-minor-mode)

;; view-mode
(with-eval-after-load 'view
  (setq view-read-only t)
  (unbind-all-keys view-mode-map))

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

;; tab indent
(defun tab-indent-on()
  (interactive)
  (setq-local indent-tabs-mode t)
  (setq-local c-basic-offset tab-width))

(defun tab-indent-off()
  (interactive)
  (setq-local indent-tabs-mode nil)
  (kill-local-variable 'c-basic-offset))

;; clipetty
;; (use-package clipetty
;;   :ensure t
;;   :hook (after-init . global-clipetty-mode))

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

;; hcl-mode
(use-package hcl-mode
  :mode (("\\.conf\\'" . hcl-mode)))

;; dwim-shell-command
(use-package dwim-shell-command
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command)))

;; forward-word to beginning
(defun forward-word-begin ()
  "Move point to the beginning of the next word."
  (interactive)
  (forward-word 1)
  (forward-word 1)
  (backward-word 1))

;; direnv
(use-package direnv
  :config
  (setq direnv-always-show-summary nil)
  (dolist (hook '(find-file-hook))
    (add-hook hook
              (lambda ()
                (unless (file-remote-p default-directory)
                  (direnv-update-directory-environment))))))

;; string-inflection
(use-package string-inflection)

;; jit-lock
;; (setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 1)
(setq jit-lock-stealth-nice 1)
(setq jit-lock-defer-time 0)
(setq jit-lock-chunk-size 4096)

;; async save buffer when in tramp
(defun save-buffer-async--rsync-save (file-name vec)
  "Save buffer asynchronously using rsync for SSH-based TRAMP files.
FILE-NAME is the remote file path, VEC is the parsed TRAMP vector."
  (let* ((buffer (current-buffer))
         (coding (or buffer-file-coding-system 'utf-8-unix))
         (temp-file (make-temp-file "emacs-rsync-"))
         (rsync-dest (tramp-vec-to-rsync-address vec))
         (mod-time (current-time))
         (tick (buffer-modified-tick)))
    (ignore-errors (run-hooks 'before-save-hook))
    (let ((coding-system-for-write coding))
      (write-region (point-min) (point-max) temp-file nil 'quiet))
    (let ((process (start-process "rsync-save" nil
                                  "rsync" "-a" "--inplace"
                                  temp-file rsync-dest)))
      (set-process-sentinel
       process
       (lambda (_ event)
         (unwind-protect
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (if (string-match-p "finished" event)
                     (progn
                       ;; update tramp
                       (let ((tramp-file-name-handler-alist nil))
                         (with-parsed-tramp-file-name file-name nil
                           ;; (set-file-times file-name mod-time)
                           (tramp-flush-file-properties v localname)
                           (tramp-flush-directory-properties v (file-name-directory localname))))

                       ;; update local
                       (set-visited-file-modtime mod-time)
                       (when (= tick (buffer-modified-tick))
                         (set-buffer-modified-p nil))
                       (ignore-errors (run-hooks 'after-save-hook))
                       (message "Wrote %s" file-name))
                   (message "Rsync save error: %s" (string-trim event)))))
           (ignore-errors (delete-file temp-file))))))))

(defun my-save-buffer ()
  "Save buffer asynchronously for SSH-based TRAMP files.
Use rsync for SSH-based TRAMP methods, regular 'save-buffer' for local files and non-SSH TRAMP methods."
  (interactive)
  (cond
   ((not (buffer-file-name)) nil)
   ((not (buffer-modified-p)) nil)
   ((file-remote-p (buffer-file-name))
    (let* ((file-name (buffer-file-name))
           (vec (tramp-dissect-file-name file-name))
           (method (tramp-file-name-method vec)))
      (if (member method '("ssh" "sshx" "scp" "rsync"))
          (save-buffer-async--rsync-save file-name vec)
        (save-buffer))))
   (t
    (save-buffer))))

(global-set-key (kbd "C-x C-s") 'my-save-buffer)

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
    (enable-auto-revert)))

;; shell-mode
(with-eval-after-load 'sh-script
  (add-hook 'sh-base-mode-hook
            (lambda()
              (add-to-list 'completion-at-point-functions 'cape-file))))

(defun fix-quotes ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[“”＂]" nil t) (replace-match "\""))
    (goto-char (point-min))
    (while (re-search-forward "[‘’＇]" nil t) (replace-match "'"))))

(global-set-key (kbd "C-c q f") #'fix-quotes)

(provide 'init-editing-utils)
