;; init-org.el --- Org-mode config -*- lexical-binding: t -*-

(use-package async)
;; org-roam
(use-package org-roam
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         (:map global-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle))))
  :after org
  :config
  (setq org-roam-directory (file-truename "~/org/roam"))
  (setq org-roam-db-node-include-function
        (lambda ()
          (pcase (org-element-type (org-element-context))
            ('headline (member "ROAM_INCLUDE" (org-get-tags)))
            (_ t)))))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t
        org-roam-ui-browser-function #'browse-url-chromium))

;; org-modern
(use-package org-modern
  :hook (org-mode .  org-modern-mode))

(with-eval-after-load 'org
  ;; auto download
  (require 'org-remoteimg)
  (setq url-cache-directory "~/.emacs.d/cache/url")
  (setq org-display-remote-inline-images 'cache)

  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   org-startup-with-inline-images t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  ;; Ellipsis styling
  (setq org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
  ;;(org-roam-db-autosync-mode)
  )

;; ;; org-appear
(use-package org-appear
  :after org
  :config
  (setq org-appear-autolinks t))

;; (use-package org-pdftools
;;   :hook (org-mode . org-pdftools-setup-link))

(defvar org-global-prefix-map (make-sparse-keymap)
  "A keymap for handy global access to org helpers, particularly clocking.")

(define-key org-global-prefix-map (kbd "j") 'org-clock-jump-to-current-clock)
(define-key org-global-prefix-map (kbd "i") 'org-clock-in)
(define-key org-global-prefix-map (kbd "o") 'org-clock-out)
(define-key org-global-prefix-map (kbd "s") 'org-sidebar-tree)
(define-key global-map (kbd "C-c o") org-global-prefix-map)

;; Re-align tags when window shape changes
(with-eval-after-load 'org-agenda
  (unless (file-directory-p org-directory)
    (mkdir org-directory))

  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))


;;; Capturing
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c g") 'org-agenda)

(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
         "* TODO %?\nSCHEDULED: %T\n" :clock-resume t)
        ("d" "doing" entry (file "")  ; "" => `org-default-notes-file'
         "* NEXT %?\nSCHEDULED: %T\n" :clock-in t :clock-resume t)
        ("n" "note" entry (file "note.org")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
        ))

;;; Refiling
(setq org-refile-use-cache nil)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

;; Exclude DONE state tasks from refile targets
(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

(defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)


;;; To-do settings

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state t)

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))

(add-hook 'org-after-todo-state-change-hook
          'org-clock-todo-change)

(defun org-clock-todo-change ()
  (if (string= org-state "NEXT")
      (org-clock-in)
    (org-clock-out)))


;;; Agenda views
(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
(let ((active-project-match "-INBOX/PROJECT"))
  (setq org-stuck-projects
        `(,active-project-match ("NEXT")))

  (setq org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-start-on-weekday nil
        org-agenda-span 'day
        org-agenda-include-diary nil
        org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up))
        org-agenda-window-setup 'current-window
        org-agenda-custom-commands
        `(("g" "GTD"
           ((agenda "" nil)
            (tags "INBOX"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)))
            (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")
                    (org-agenda-tags-todo-honor-ignore-options t)
                    (org-tags-match-list-sublevels t)
                    (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
            (tags-todo ,active-project-match
                       ((org-agenda-overriding-header "Projects")
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX/-NEXT"
                       ((org-agenda-overriding-header "Orphaned Tasks")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                                (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/WAITING"
                       ((org-agenda-overriding-header "Waiting")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/DELEGATED"
                       ((org-agenda-overriding-header "Delegated")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "On Hold")
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            ;; (tags-todo "-NEXT"
            ;;            ((org-agenda-overriding-header "All other TODOs")
            ;;             (org-match-list-sublevels t)))
            )))))


(add-hook 'org-agenda-mode-hook 'hl-line-mode)


;;; Org clock

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(with-eval-after-load 'org
  (org-clock-persistence-insinuate)
  (setq org-clock-persist t)
  (setq org-clock-in-resume t)

  ;; Save clock data and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Save state changes in the LOGBOOK drawer
  (setq org-log-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)

  ;; Show clock sums as hours and minutes, not "n days" etc.
  (setq org-time-clocksum-format
        '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;;; Archiving
  (setq org-archive-mark-done nil)
  (setq org-archive-location "%s_archive::* Archive"))

;; (require-package 'org-pomodoro)
;; (setq org-pomodoro-keep-killed-pomodoro-time t)
;; (with-eval-after-load 'org-agenda
;;   (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))

(use-package ox-reveal)
(use-package ob-go)
(use-package ox-gfm)
;;(use-package org-sidebar)

(defhydra hydra-org-table (:color blue :hint nil)
  "
^Rows^            ^Columns^           ^size^
-----------------------------------------------------------------
_j_: insert row   _h_: insert column  _w_: widen
_k_: delete row   _l_: delete column  _s_: shorten
"
  ("j" #'table-insert-row)
  ("k" #'table-delete-row)
  ("h" #'table-insert-column)
  ("l" #'table-delete-column)
  ("w" #'table-widen-cell)
  ("s" #'table-shorten-cell))

;; custom links
(defun org-link-complete-docview (&optional _)
  "Create a file link using completion."
  (interactive)
  (concat "docview:"
          (read-file-name "DocFile: " "~/Documents" "./")))

(defun org-link-complete-chrome (&optional _)
  "Create a file link using completion."
  (interactive)
  (let ((text (current-kill 0)))
    (if (s-starts-with? "http" text)
        (concat "chrome:" text)
      "chrome:")))

;; delete link at point with associated file
(defun org-delete-link-at-point()
  (interactive)
  (let* ((link (org-element-context))
         (type (org-element-type link))
         (beg (org-element-property :begin link))
         (end (org-element-property :end link))
         (from (org-element-property :type link))
         (path (org-element-property :path link)))
    (delete-region beg end)
    (when (and (s-equals? from "file") (eq type 'link))
      (delete-file path))))

(with-eval-after-load 'org
  ;; Various preferences
  (setq org-log-done t
        org-edit-timestamp-down-means-later t
        org-hide-emphasis-markers t
        org-catch-invisible-edits 'show
        org-export-coding-system 'utf-8
        org-fast-tag-selection-single-key 'expert
        org-html-validation-link nil
        org-export-kill-product-buffer-when-displayed t
        org-cycle-include-plain-lists 'integrate
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation nil
        org-fontify-whole-heading-line t
        org-edit-src-content-indentation 0
        org-export-time-stamp-file nil
        org-html-html5-fancy t
        org-html-doctype "html5"
        org-default-notes-file (convert-standard-filename "~/org/inbox.org")
        org-support-shift-select t)

  ;; use find-file
  (setf (alist-get 'file org-link-frame-setup) 'find-file)

  ;; custom links
  (org-link-set-parameters "docview" :complete 'org-link-complete-docview)
  (org-link-set-parameters "chrome"
                           :complete 'org-link-complete-chrome
                           :follow 'browse-url-chrome)

  (require 'ox-reveal)
  (setq org-reveal-root (expand-file-name "~/.emacs.d/third-parties/reveal.js"))
  (setq org-reveal-theme "league")
  ;; (require 'ox-gfm)
  ;; (org-beamer-mode)

  ;; keybindings
  (unbind-key (kbd "C-,") org-mode-map)
  (unbind-key (kbd "C-c $") org-mode-map)
  (unbind-key (kbd "C-c C-m") org-mode-map)
  (unbind-key (kbd "C-c [") org-mode-map)
  (unbind-key (kbd "C-c ]") org-mode-map)

  (define-key org-mode-map (kbd "C-n") #'precision-scroll-forward-line)
  (define-key org-mode-map (kbd "C-p") #'precision-scroll-backward-line)
  (define-key org-mode-map (kbd "C-v") #'precision-scroll-up-page)
  (define-key org-mode-map (kbd "M-v") #'precision-scroll-down-page)
  (define-key org-mode-map (kbd "C-c v") #'org-overview)
  (define-key org-mode-map (kbd "C-c t l") #'org-toggle-link-display)
  (define-key org-mode-map (kbd "C-c t i") #'org-toggle-inline-images)
  (define-key org-mode-map (kbd "C-c i") #'org-clip-paste)
  (define-key org-mode-map (kbd "C-M-<up>") #'org-up-element)
  (define-key org-mode-map (kbd "M-.") #'org-open-at-point)
  ;;(define-key org-mode-map (kbd "C-c i l") #'org-insert-link)
  (define-key org-mode-map (kbd "C-c d l") #'org-delete-link-at-point)

  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((R . t)
     (ditaa . t)
     (dot . t)
     (go . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ocaml . nil)
     (octave . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     ;;     (scala . t)
     (shell . t)
     (sql . t)
     (sqlite . t))))

(use-package ob-sql-mode
  :config
  (setq org-confirm-babel-evaluate
        (lambda (lang body)
          (not (string= lang "sql-mode")))))

(use-package org2ctex
  :after org
  :config
  (setq org-latex-logfiles-extensions (append '("bbl" "pyg") org-latex-logfiles-extensions))
  (setq org-latex-listings 'minted)
  (setq org2ctex-latex-packages-alist (list (string-join (mapcar (apply-partially 'format "\\usepackage{%s}")
                                                                 '("minted" "tikz" "graphicx"))
                                                         "\n")))
  (setq org-latex-minted-options '(("breaklines")
                                   ("fontsize" "\\footnotesize")
                                   ("breakbefore" ".")))

  (setq org2ctex-latex-classes
        '(("ctexart"
           "\\documentclass[fontset=none,UTF8,zihao=-4]{ctexart}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ("ctexrep"
           "\\documentclass[fontset=none,UTF8,zihao=-4]{ctexrep}"
           ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
          ("ctexbook"
           "\\documentclass[fontset=none,UTF8,zihao=-4]{ctexbook}"
           ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
          ("beamer"
           "\\documentclass[presentation]{beamer}
\\usepackage[fontset=none,UTF8,zihao=-4]{ctex}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
  (setq org2ctex-latex-commands
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;;:hook (org-mode . org2ctex-mode)
  )

(require 'org-tempo nil 'noerror)
(require-package 'org-preview-html)
(setq org-preview-html-viewer 'xwidget)

(defun org-level-reset-height()
  (dolist (face '(outline-1 outline-2 outline-3 org-level-1 org-level-2 org-level-3))
    ;;(make-local-variable face)
    (set-face-attribute face nil :height 1.0)))

;;(after-load-theme (org-level-reset-height))

(defun my-org-mode-hook()
  (toggle-truncate-lines)
  (setq-local electric-pair-inhibit-predicate (lambda (c) (char-equal c ?<)))
  (setq-local org-download-image-dir (concat (buffer-name) "-assets/images")))

(add-hook 'org-mode-hook #'my-org-mode-hook)

;; clipboard
(require-package 'org-cliplink)
(defun org-download-annotate-empty (_) "")

(with-eval-after-load 'org
  (require 'org-download)
  (setq org-download-annotate-function #'org-download-annotate-empty)

  (defun my-org-download-clipboard (&optional basename)
    "Capture the image from the clipboard and insert the resulting file."
    (interactive)
    (let ((org-download-screenshot-method
           (cl-case system-type
             (gnu/linux
              (if (string= "wayland" (getenv "XDG_SESSION_TYPE"))
                  (if (executable-find "wl-paste")
                      "wl-paste -t image/png > %s"
                    (user-error
                     "Please install the \"wl-paste\" program included in wl-clipboard"))
                (if (executable-find "xclip")
                    "xclip -selection clipboard -t image/png -o > %s"
                  (user-error
                   "Please install the \"xclip\" program"))))
             ((windows-nt cygwin)
              (if (executable-find "magick")
                  "magick convert clipboard: %s"
                (user-error
                 "Please install the \"magick\" program included in ImageMagick")))
             ((darwin berkeley-unix)
              (if (executable-find "pngpaste")
                  "pngpaste %s"
                (user-error
                 "Please install the \"pngpaste\" program from Homebrew."))))))
      (save-excursion
        (org-download-screenshot basename))))

  (defun org-clip-paste()
    (interactive)
    (let ((clip-text (substring-no-properties (gui-get-selection 'CLIPBOARD 'STRING))))
      (if (s-starts-with? "http" clip-text)
          (org-cliplink)
        (my-org-download-clipboard))))
  )

(provide 'init-org)
;;; init-org.el ends here
