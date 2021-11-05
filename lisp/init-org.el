;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;; Among settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.  I have an "inbox.org" file with a header
;; including

;;     #+CATEGORY: Inbox
;;     #+FILETAGS: INBOX

;; and then set this file as `org-default-notes-file'.  Captured org
;; items will then go into this file with the file-level tag, and can
;; be refiled to other locations as necessary.

;; Those other locations are generally other org files, which should
;; be added to `org-agenda-files-list' (along with "inbox.org" org).
;; With that done, there's then an agenda view, accessible via the
;; `org-agenda' command, which gives a convenient overview.
;; `org-todo-keywords' is customised here to provide corresponding
;; TODO states, which should make sense to GTD adherents.

;;; Code:

(require-package 'org-cliplink)
(require-package 'org-plus-contrib)

(defvar org-global-prefix-map (make-sparse-keymap)
  "A keymap for handy global access to org helpers, particularly clocking.")

(define-key org-global-prefix-map (kbd "j") 'org-clock-jump-to-current-clock)
(define-key org-global-prefix-map (kbd "i") 'org-clock-in)
(define-key org-global-prefix-map (kbd "o") 'org-clock-out)
(define-key org-global-prefix-map (kbd "s") 'org-sidebar-tree)
(define-key global-map (kbd "C-c o") org-global-prefix-map)


;; Various preferences
(setq org-log-done t
      org-edit-timestamp-down-means-later t
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-tags-column 100
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-preserve-indentation nil
      org-fontify-whole-heading-line t
      org-edit-src-content-indentation 0
      org-export-time-stamp-file nil
      org-html-html5-fancy t
      org-html-doctype "html5")

;; Re-align tags when window shape changes
(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

(setq org-support-shift-select t)

;;; Capturing

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
         "* NEXT %?\n%U\n" :clock-resume t)
        ("n" "note" entry (file "")
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
      org-todo-repeat-to-state "NEXT")

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))



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
        `(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("g" "GTD"
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
  (org-clock-persistence-insinuate))
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
(setq org-archive-location "%s_archive::* Archive")

(require-package 'org-pomodoro)
(setq org-pomodoro-keep-killed-pomodoro-time t)
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))

(use-package ox-reveal)
(use-package ob-go)

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


(defun org-link-complete-docview (&optional _)
  "Create a file link using completion."
  (interactive)
  (concat "docview:"
          (read-file-name "DocFile: " "~/Documents" "./")))

(with-eval-after-load 'org
  ;; open file in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (org-link-set-parameters "docview" :complete 'org-link-complete-docview)

  (require 'ox-reveal)
  (require 'ox-confluence)
  (org-beamer-mode)
  (unbind-key (kbd "C-,") org-mode-map)
  (unbind-key (kbd "C-c C-m") org-mode-map)
  (unbind-key (kbd "C-c [") org-mode-map)
  (unbind-key (kbd "C-c ]") org-mode-map)
  (define-key org-mode-map (kbd "C-c t t") #'hydra-org-table/body)
  (define-key org-mode-map (kbd "C-c t l") #'org-toggle-link-display)
  (define-key org-mode-map (kbd "C-c C-p") #'org-cliplink)
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil)
    (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link))
  (electric-pair-local-mode)
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
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (sql . t)
     (sqlite . t))))

(use-package org2ctex
  :config
  (setq org-latex-logfiles-extensions (append '("tex" "bbl" "pyg") org-latex-logfiles-extensions))
  (setq org-latex-listings 'minted)
  (setq org2ctex-latex-packages-alist (list (string-join (mapcar (apply-partially 'format "\\usepackage{%s}")
                                                                 '("minted" "tikz" "CJKulem" "graphicx"))
                                                         "\n")))
  (setq org-latex-minted-options '(("breaklines")
                                   ("fontsize" "\\footnotesize")
                                   ("breakbefore" ".")))
  (setq org2ctex-latex-commands
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  :hook (org-mode . org2ctex-mode))

(require 'org-tempo nil 'noerror)
(require-package 'org-preview-html)
(setq org-preview-html-viewer 'xwidget)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; sidebar
(use-package org-sidebar)
(with-eval-after-load 'org-mode
  (require 'org-sidebar)
  (advice-add 'org-sidebar-tree :after #'advice/after-org-sidebar-tree))

(defun advice/after-org-sidebar-tree()
  (dolist (face '(org-level-1 org-level-2 org-level-3))
    (make-local-variable face)
    (set-face-attribute face nil :height 1.0)))

(add-hook 'org-mode-hook (lambda ()
                           (setq-local electric-pair-inhibit-predicate
                                       `(lambda (c)
                                          (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(provide 'init-org)
;;; init-org.el ends here
