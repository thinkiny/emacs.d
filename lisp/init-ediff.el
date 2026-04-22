;; -*- lexical-binding: t; -*-

(require 'ediff)

;; ztree
(use-package ztree
  :bind (:map global-map
         ("C-c d e" . ediff-directories)
         ("C-c d z" . ztree-diff)
         ("C-c d ." . ztree-dir)
         :map ztree-mode-map
         ("n" . ztree-next-line)
         ("p" . ztree-previous-line))
  :config
  (define-key ztreediff-mode-map (kbd "g") #'ztree-diff-partial-rescan)
  (advice-add 'ztree-diff-ediff-quit-hook-function :after
              (lambda () (ztree-diff-partial-rescan))))

;; ediff basic settings
(setq-default ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain)

;; ediff visual-line
(defvar my-ediff-visual-line-mode-states nil)

(defun my-ediff-prepare-buffer-hook ()
  "Save `visual-line-mode` state and enable it for ediff."
  (let ((buf (current-buffer)))
    (setq my-ediff-visual-line-mode-states
          (assq-delete-all buf my-ediff-visual-line-mode-states))
    (push (cons buf visual-line-mode)
          my-ediff-visual-line-mode-states))
  (visual-line-mode 1))

(defun my-ediff-cleanup-hook ()
  "Restore `visual-line-mode` state after ediff quits."
  (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-buffer-C))
    (when (buffer-live-p buf)
      (if-let* ((entry (assq buf my-ediff-visual-line-mode-states)))
          (with-current-buffer buf
            (visual-line-mode (if (cdr entry) 1 -1))))))

  (setq my-ediff-visual-line-mode-states
        (cl-remove-if-not (lambda (entry) (buffer-live-p (car entry)))
                          my-ediff-visual-line-mode-states)))

(with-eval-after-load 'ediff-init
  (add-hook 'ediff-prepare-buffer-hook #'my-ediff-prepare-buffer-hook)
  (add-hook 'ediff-cleanup-hook #'my-ediff-cleanup-hook))

;; ediff-quit
(with-eval-after-load 'ediff-util
  (defun ediff-quit(reverse-default-keep-variants)
    "Remove y-or-n-p in edifff-util."
    (interactive "P")
    (ediff-barf-if-not-control-buffer)
    (let ((ctl-buf (current-buffer))
          (ctl-frm (selected-frame))
          (minibuffer-auto-raise t))
      (setq this-command 'ediff-quit) ; bug#38219
      (set-buffer ctl-buf)
      (ediff-really-quit reverse-default-keep-variants)
      (select-frame ctl-frm)
      (raise-frame ctl-frm))))

;; ediff-directories: make top-level =h consistent with sub-session =h.
;; override to compare only common files.
(with-eval-after-load 'ediff-diff
  (defun ediff-same-file-contents-lists (entries-1 entries-2 filter-re)
    (let* ((names-1 (mapcar #'file-name-nondirectory entries-1))
           (names-2 (mapcar #'file-name-nondirectory entries-2))
           (common-names (seq-intersection names-1 names-2 #'string=))
           (continue t))
      (dolist (name common-names continue)
        (when continue
          (let ((f1 (seq-find (lambda (e) (string= (file-name-nondirectory e) name))
                              entries-1))
                (f2 (seq-find (lambda (e) (string= (file-name-nondirectory e) name))
                              entries-2)))
            (unless (ediff-same-contents f1 f2 filter-re)
              (setq continue nil))))))))

(defun ediff-hide-identical-sessions ()
  "Toggle hiding of identical sessions."
  (interactive)
  (let* ((hidden (bound-and-true-p ediff--identical-hidden))
         (from (if hidden ?I ?H))
         (to (if hidden nil ?I)))
    (unless hidden
      (ediff-meta-mark-equal-files ?h))
    (dolist (elt (cdr ediff-meta-list))
      (when (eq (ediff-get-session-status elt) from)
        (ediff-set-session-status elt to)))
    (setq-local ediff--identical-hidden (not hidden))
    (ediff-update-meta-buffer (current-buffer) 'must-redraw)))

(add-hook 'ediff-meta-buffer-keymap-setup-hook
          (lambda ()
            (define-key ediff-meta-buffer-map "H"
                        #'ediff-hide-identical-sessions)))

(add-hook 'ediff-after-session-group-setup-hook #'ediff-hide-identical-sessions)

(provide 'init-ediff)
