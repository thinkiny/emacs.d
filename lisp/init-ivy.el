;; -*- lexical-binding: t; -*-

(use-package ivy
  :config
  (setq-default ivy-use-virtual-buffers 'recentf
                ivy-virtual-abbreviate 'abbreviate
                ivy-count-format ""
                ivy-wrap t
                enable-recursive-minibuffers t
                ivy-case-fold-search-default t
                ivy-use-selectable-prompt t)
  (ivy-mode)
  (define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)
  (define-key ivy-minibuffer-map (kbd "<down>") #'ivy-next-line-or-history)
  (define-key ivy-minibuffer-map (kbd "C-l") #'ivy-backward-delete-char)
  (define-key ivy-minibuffer-map (kbd "C-o") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "C-h") #'hydra-ivy/body)
  (define-key ivy-minibuffer-map (kbd "C-c C-f") #'ivy-toggle-calling) ;; follow mode
  (define-key ivy-minibuffer-map (kbd "TAB") #'ivy-insert-current)
  (add-to-list 'ivy-ignore-buffers "\\*TERM")

  (define-key ivy-occur-mode-map (kbd "n") #'ivy-occur-next-line)
  (define-key ivy-occur-mode-map (kbd "p") #'ivy-occur-previous-line)
  (global-set-key (kbd "C-c z") 'ivy-resume)
  (add-hook 'ivy-occur-mode-hook #'ivy-occur-focus)
  (after-load-theme
   ;;(set-face-attribute 'swiper-line-face nil :background (face-attribute 'highlight :background))
   (set-face-attribute 'ivy-virtual nil :inherit nil)))

(defun ivy-occur-focus ()
  (run-at-time 0.1 nil (lambda ()
                         ;; (ivy-occur-toggle-calling)
                         (goto-char (point-min))
                         (ivy-occur-next-line))))

;; counsel
(use-package counsel
  :after ivy
  :config
  (setq-default counsel-mode-override-describe-bindings t)
  (setq counsel-preselect-current-file t)
  (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")
  (add-to-list 'ivy-more-chars-alist '(counsel-rg . 2))
  (define-key counsel-find-file-map (kbd "C-l") #'counsel-up-directory)
  (setq counsel-find-file-ignore-regexp (regexp-opt '(".cache$" ".metals$" "bazel-.*" ".o$" ".elc$" ".so$")))
  (global-set-key (kbd "C-c l") 'counsel-imenu)
  (global-set-key (kbd "C-c s s") 'counsel-rg)
  (global-set-key (kbd "C-c s d") 'counsel-rg-here)
  (global-set-key (kbd "C-c s t") 'counsel-rg-filetype)
  (counsel-mode))

(defun counsel-rg-here()
  (interactive)
  (counsel-rg "" default-directory))

(defun counsel-rg-filetype(type)
  (interactive (list (read-string "Search file type: ")))
  (counsel-rg "" default-directory (concat "-t" type)))

(use-package ivy-rich
  :after (ivy counsel)
  :config
  (setq ivy-rich-parse-remote-buffer nil
        ivy-rich-path-style 'abbrev)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1)
  (ivy-rich-project-root-cache-mode 1))

;; ivy-posframe
;; (use-package ivy-posframe)
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-center)))
;; (ivy-posframe-mode 1)

;; smex
;; (use-package smex)
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)

(provide 'init-ivy)
;;; init-ivy.el ends here
