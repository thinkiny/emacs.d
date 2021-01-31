;;; counsel-mt.el --- counsel multi-term management -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'multi-term)

(defconst counsel-mt-buffer-header "*term-")
(defun counsel-mt/terminal-buffers ()
  "Filter for buffers that are terminals only.
Includes buffers managed by `multi-term' (excludes dedicated term
buffers) and buffers in `shell-mode'."
  (cl-loop for buf in (buffer-list)
           if (or (member buf multi-term-buffer-list)
                  (eq (buffer-local-value 'major-mode buf) 'shell-mode))
           collect (cons (string-trim (buffer-name buf) counsel-mt-buffer-header)
                         buf)))

(defun counsel-mt/get-buffer-process-cwd ()
  "The current working directory of the process of buffer BUF, or nil (depends on lsof)."
  (let* ((process (get-buffer-process (current-buffer)))
         (pid (process-id process))
         (command (format "lsof -Fn -a -dcwd -p%d" pid))
         (stdout (shell-command-to-string command)))
    (string-match "^n\\(.*\\)" stdout)
    (match-string 1 stdout))
  (nil))

(defun counsel-mt/get-terminal-name()
  (or (projectile-project-name)
      multi-term-default-dir
      (counsel-mt/get-buffer-process-cwd)
      default-directory))

(defun counsel-mt/launch-terminal ()
  "Launch a terminal in a new buffer."
  (call-interactively 'multi-term)
  (rename-buffer (generate-new-buffer-name (format "%s%s" counsel-mt-buffer-header (counsel-mt/get-terminal-name)))))

(defun counsel-mt/source-terminals ()
  "Counsel source with candidates for all terminal buffers."
  (cons '("Launch new terminal") (counsel-mt/terminal-buffers)))

;;;###autoload
(defun counsel-mt ()
  (interactive)
  (ivy-read "Terminal: " (counsel-mt/source-terminals)
            :require-match t
            :preselect (counsel-mt/get-terminal-name)
            :action (lambda (candidate)
                      (let ((buf (cdr candidate)))
                        (if buf
                            (switch-to-buffer buf)
                          (counsel-mt/launch-terminal))))
            :caller 'counsel-mt))

(provide 'counsel-mt)
;;; counsel-mt.el ends here
