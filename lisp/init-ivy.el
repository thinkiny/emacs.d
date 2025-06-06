;; -*- lexical-binding: t; -*-

(use-package ivy
  :diminish ivy-mode
  :commands ivy-mode
  :init (ivy-mode 1)
  :config
  (setq-default ivy-use-virtual-buffers 'recentf
                ivy-virtual-abbreviate 'abbreviate
                ivy-count-format ""
                ivy-wrap t
                enable-recursive-minibuffers t
                ivy-use-selectable-prompt t)
  (define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)
  (define-key ivy-minibuffer-map (kbd "<down>") #'ivy-next-line-or-history)
  (define-key ivy-minibuffer-map (kbd "C-l") #'ivy-backward-delete-char)
  (define-key ivy-minibuffer-map (kbd "C-o") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "C-h") #'hydra-ivy/body)
  (define-key ivy-minibuffer-map (kbd "C-c C-f") #'ivy-toggle-calling) ;; follow mode
  (define-key ivy-minibuffer-map (kbd "TAB") #'ivy-insert-current)
  (add-to-list 'ivy-ignore-buffers "\\*TERM")

  (defun ivy-occur-calling-auto ()
    (run-at-time 0.1 nil (lambda ()
                           (ivy-occur-toggle-calling)
                           (goto-char (point-min))
                           ;;(ivy-occur-next-line)
                           )))

  ;; (add-hook 'ivy-occur-mode-hook #'ivy-occur-calling-auto)
  (global-set-key (kbd "C-c z") 'ivy-resume)
  (global-set-key (kbd "C-x b") 'persp-ivy-switch-buffer)
  (after-load-theme
   ;;(set-face-attribute 'swiper-line-face nil :background (face-attribute 'highlight :background))
   (set-face-attribute 'ivy-virtual nil :inherit nil)))

(use-package counsel
  :after ivy
  :config
  (setq-default counsel-mode-override-describe-bindings t)
  (setq counsel-preselect-current-file t)
  (add-to-list 'ivy-more-chars-alist '(counsel-ag . 2))
  (add-to-list 'ivy-more-chars-alist '(counsel-rg . 2))
  (define-key counsel-find-file-map (kbd "C-l") #'counsel-up-directory)
  (setq counsel-find-file-ignore-regexp (regexp-opt '(".cache$" ".metals$" "bazel-.*" ".o$" ".elc$" ".so$")))

  (defun counsel-ag-here()
    (interactive)
    (counsel-ag "" default-directory))

  (defun counsel-ag-filetype(type)
    (interactive (list (read-string "Search file type: ")))
    (counsel-ag "" default-directory (concat "--" type)))

  (global-set-key (kbd "C-c l") 'counsel-imenu)
  (global-set-key (kbd "C-c s s") 'counsel-ag)
  (global-set-key (kbd "C-c s .") 'counsel-ag-here)
  (global-set-key (kbd "C-c s t") 'counsel-ag-filetype)
  (counsel-mode))


(use-package ivy-hydra)

(use-package ivy-rich
  :diminish ivy-rich-mode
  :after (ivy counsel)
  :init (ivy-rich-mode 1)
  :config
  (setq ivy-rich-parse-remote-buffer nil
        ivy-rich-path-style 'abbrev)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-project-root-cache-mode 1))

(use-package ivy-xref
  :after ivy
  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs)
  (setq ivy-xref-use-file-path t)
  :config
  (defun ivy-xref-trim-path (file)
    ;; (let ((root (projectile-project-root)))
    ;;   (if (and root (s-starts-with? "/" file))
    ;;       (string-trim-left file root)
    ;;     (string-trim-left file "\\(\\.\\./\\)*")))
    (file-relative-name file default-directory))

  (defun ivy-xref-make-collection (xrefs)
    "Transform XREFS into a collection for display via `ivy-read'."
    (let ((collection nil))
      (dolist (xref xrefs)
        (let* ((summary (xref-item-summary xref))
               (location (xref-item-location xref))
               (line (xref-location-line location))
               (file (xref-location-group location))
               (candidate
                (concat
                 (propertize
                  (concat
                   (if ivy-xref-use-file-path
                       ;; strip path
                       (ivy-xref-trim-path file)
                     (file-name-nondirectory file))
                   (if (integerp line)
                       (format ":%d: " line)
                     ": "))
                  'face 'compilation-info)
                 (progn
                   (when ivy-xref-remove-text-properties
                     (set-text-properties 0 (length summary) nil summary))
                   summary))))
          (push `(,candidate . ,location) collection)))
      (nreverse collection))))


;; ivy-posframe
;; (require-package 'ivy-posframe)
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-center)))
;; (ivy-posframe-mode 1)

;; smex
(require-package 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(provide 'init-ivy)
;;; init-ivy.el ends here
