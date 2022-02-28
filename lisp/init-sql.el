;;; init-sql.el --- Support for SQL -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'sql
  ;; sql-mode pretty much requires your psql to be uncustomised from stock settings
  (push "--no-psqlrc" sql-postgres-options))

(defun sanityinc/fix-postgres-prompt-regexp ()
  "Work around https://debbugs.gnu.org/cgi/bugreport.cgi?bug=22596.
Fix for the above hasn't been released as of Emacs 25.2."
  (when (eq sql-product 'postgres)
    (setq-local sql-prompt-regexp "^[[:alnum:]_]*=[#>] ")
    (setq-local sql-prompt-cont-regexp "^[[:alnum:]_]*[-(][#>] ")))

(add-hook 'sql-interactive-mode-hook 'sanityinc/fix-postgres-prompt-regexp)

(defun sanityinc/pop-to-sqli-buffer ()
  "Switch to the corresponding sqli buffer."
  (interactive)
  (if (and sql-buffer (buffer-live-p sql-buffer))
      (progn
        (pop-to-buffer sql-buffer)
        (goto-char (point-max)))
    (sql-set-sqli-buffer)
    (when sql-buffer
      (sanityinc/pop-to-sqli-buffer))))

(setq-default sql-input-ring-file-name
              (expand-file-name ".sqli_history" user-emacs-directory))

;; See my answer to https://emacs.stackexchange.com/questions/657/why-do-sql-mode-and-sql-interactive-mode-not-highlight-strings-the-same-way/673
(defun sanityinc/font-lock-everything-in-sql-interactive-mode ()
  (unless (eq 'oracle sql-product)
    (sql-product-font-lock nil nil)))
(add-hook 'sql-interactive-mode-hook 'sanityinc/font-lock-everything-in-sql-interactive-mode)

;; Package ideas:
;;   - PEV
(defun sanityinc/sql-explain-region-as-json (beg end &optional copy)
  "Explain the SQL between BEG and END in detailed JSON format.
This is suitable for pasting into tools such as
http://tatiyants.com/pev/.

When the prefix argument COPY is non-nil, do not display the
resulting JSON, but instead copy it to the kill ring.

If the region is not active, uses the current paragraph, as per
`sql-send-paragraph'.

Connection information is taken from the special sql-* variables
set in the current buffer, so you will usually want to start a
SQLi session first, or otherwise set `sql-database' etc.

This command currently blocks the UI, sorry."
  (interactive "rP")
  (unless (eq sql-product 'postgres)
    (user-error "This command is for PostgreSQL only"))
  (unless (use-region-p)
    (setq beg (save-excursion (backward-paragraph) (point))
          end (save-excursion (forward-paragraph) (point))))
  (let ((query (buffer-substring-no-properties beg end)))
    (with-current-buffer (if (sql-buffer-live-p sql-buffer)
                             sql-buffer
                           (current-buffer))
      (let* ((process-environment
              (append (list (concat "PGDATABASE=" sql-database)
                            (concat "PGHOST=" sql-server)
                            (concat "PGUSER=" sql-user))
                      process-environment))
             (args (list "--no-psqlrc"
                         "-qAt"
                         "-w"             ; Never prompt for password
                         "-E"
                         "-c" (concat "EXPLAIN (ANALYZE, COSTS, VERBOSE, BUFFERS, FORMAT JSON) " query ";")
                         ))
             (err-file (make-temp-file "sql-explain-json")))
        (with-current-buffer (get-buffer-create "*sql-explain-json*")
          (setq buffer-read-only nil)
          (delete-region (point-min) (point-max))
          (let ((retcode (apply 'call-process sql-postgres-program nil (list (current-buffer) err-file) nil args)))
            (if (zerop retcode)
                (progn
                  (json-mode)
                  (read-only-mode 1)
                  (if copy
                      (progn
                        (kill-ring-save (buffer-substring-no-properties (point-min) (point-max)))
                        (message "EXPLAIN output copied to kill-ring."))
                    (display-buffer (current-buffer))))
              (with-current-buffer (get-buffer-create "*sql-explain-errors*")
                (let ((inhibit-read-only t))
                  (insert-file-contents err-file nil nil nil t))
                (display-buffer (current-buffer))
                (user-error "EXPLAIN failed")))))))))

;; mysql-to-org
;; (require-package 'mysql-to-org)
;; (add-hook 'mysql-to-org-mode-hook
;;           (lambda ()
;;             (define-key mysql-to-org-mode-map (kbd "C-c e") 'mysql-to-org-eval)
;;             (define-key mysql-to-org-mode-map (kbd "C-c p") 'mysql-to-org-eval-string-at-point)
;;             (define-key mysql-to-org-mode-map (kbd "C-c s") 'mysql-to-org-scratch)
;;             (define-key mysql-to-org-mode-map (kbd "C-c 1") 'mysql-to-org-only-show-output-window)
;;             (define-key mysql-to-org-mode-map (kbd "C-c r") 'mysql-to-org-reload-completion-candidates)))

(defun make-sql-align-column (n &optional str)
  (setq str (or str ""))
  (if (= n 0)
      (concat str "\\(\\s-+\\)")
    (make-sql-align-column
     (- n 1)
     (concat str "\\s-+[^( ]+\\(?:(.*)\\)?\\(?:\\s-+unsigned\\|UNSIGNED\\)?"))))

(defun align-create-table()
  (interactive)
  (save-excursion
    (let ((start (re-search-backward "($" nil t))
          (end (re-search-forward "^)" nil t)))
      (align-regexp start end (make-sql-align-column 0) 1 2 nil)
      (align-regexp start end (make-sql-align-column 1) 1 2 nil)
      (align-regexp start end (make-sql-align-column 2) 1 2 nil))))

(with-eval-after-load 'lsp-sqls
  (defun lsp-sql-execute-current (&optional command)
  "Execute COMMAND on paragraph against current database."
  (interactive)
  (let ((start (or (save-excursion (re-search-backward "\n\\|;" nil t)) 0))
        (end (or (save-excursion (search-forward ";" nil t)) (point-max))))
    (lsp-sql-execute-query command start end)))

  (defun lsp-sqls--show-results (result)
    (with-current-buffer (get-buffer-create "*sqls results*")
      (goto-char (point-max))
      (insert result)
      (display-buffer (current-buffer))
      (set-window-point (get-buffer-window) (point-max))))


  (defun lsp-sql-execute-file (&optional command)
    "Execute COMMAND on file against current database."
    (interactive)
    (lsp-sql-execute-query command (point-min) (point-max))))

(defun my-sql-hook()
  (setq-local lsp-enable-format-at-save nil)
  (lsp)
  (local-set-key (kbd "C-c C-t") #'align-create-table)
  (local-set-key (kbd "C-c C-f") #'sqlformat)
  (local-set-key (kbd "C-x C-e") #'lsp-sql-execute-current)
  (local-set-key (kbd "C-x C-p") #'lsp-sql-execute-paragraph)
  (local-set-key (kbd "C-x C-l") #'lsp-sql-execute-file))

(add-hook 'sql-mode-hook #'my-sql-hook)

(provide 'init-sql)
;;; init-sql.el ends here
