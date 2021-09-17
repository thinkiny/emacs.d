;; workgroups2
(require-package 'workgroups2)
(setq wg-first-wg-name "main")
(setq wg-session-file (expand-file-name ".emacs_workgroups" user-emacs-directory))
(add-hook 'kill-emacs-hook #'wg-save-session)
(add-hook 'after-init-hook (lambda ()
                             (workgroups-mode 1)
                             (unbind-all-keys workgroups-mode-map)
                             (define-key workgroups-mode-map (kbd "C-x w a") #'wg-create-workgroup-cmd)
                             (define-key workgroups-mode-map (kbd "C-x w s") #'wg-save-session-cmd)
                             (define-key workgroups-mode-map (kbd "C-x w r") #'wg-rename-workgroup-cmd)
                             (define-key workgroups-mode-map (kbd "C-x w d") #'wg-delete-workgroup-cmd)
                             (define-key workgroups-mode-map (kbd "C-x w k") #'wg-kill-workgroup-cmd)
                             (define-key workgroups-mode-map (kbd "C-x w l") #'wg-list-workgroup-cmd)))

(defun wg-save-session-cmd()
  (interactive)
  (wg-save-session))

(defun wg-workgroup-get-current-name ()
  (let ((group (wg-current-workgroup t)))
    (if group
        (wg-workgroup-name group)
      nil)))

(defun wg-workgroup-get-names ()
  "Get all workgroup names."
  (when (and wg-session-file
             (file-exists-p wg-session-file))
    (mapcar (lambda (group)
              (wg-workgroup-name group))
              (wg-session-workgroup-list
               (read (wg-read-text wg-session-file))))))

(defun wg-kill-workgroup(group)
  (if group
      (let ((buf-uid (car (wg-wconfig-buf-uids (wg-workgroup-working-wconfig group)))))
        (mapc (lambda (buf)
                (if (equal (buffer-local-value 'wg-buffer-uid buf) buf-uid)
                    (kill-buffer buf)))
              (buffer-list)))))

(defun wg-kill-workgroup-by-name (name)
  (wg-kill-workgroup (wg-find-workgroup-by :name name t)))

(defun wg-create-workgroup-cmd()
  (interactive)
  (let ((curr-name (wg-workgroup-get-current-name))
        (new-name (wg-read-new-workgroup-name)))
    (wg-create-workgroup new-name)
    (wg-kill-workgroup-by-name curr-name)))

(defun wg-kill-workgroup-cmd()
  (interactive)
  (ivy-read "Kill workgroup: " (wg-workgroup-get-names)
            :action (lambda (name)
                      (wg-kill-workgroup-by-name name)))
            :preselect (wg-workgroup-get-current-name))

(defun wg-delete-workgroup-cmd()
  (interactive)
  (ivy-read "Remove workgroup: " (wg-workgroup-get-names)
            :action (lambda (name)
                      (wg-delete-workgroup (wg-find-workgroup-by :name name t))
                      (wg-save-session))
            :preselect (wg-workgroup-get-current-name)))

(defun wg-list-workgroup-cmd()
  (interactive)
  (ivy-read "Select workgroup: " (wg-workgroup-get-names)
            :action (lambda (name)
                      (let ((curr-name (wg-workgroup-get-current-name)))
                        (unless (string= name curr-name)
                          (wg-kill-workgroup-by-name curr-name)
                          (wg-open-workgroup name))))
            :preselect (wg-workgroup-get-current-name)))

(defun wg-rename-workgroup-cmd()
  (interactive)
  (let ((group (wg-current-workgroup t)))
    (if group
        (let* ((new-name (read-string (format "Rename %s to: " (wg-workgroup-name group)))))
          (wg-delete-workgroup group)
          (setf (wg-workgroup-name group) new-name)
          (wg-check-and-add-workgroup group)
          (wg-save-session))
      (message "This frame doesn't belong to any workgroup"))))

(with-eval-after-load 'pdf-outline
  (wg-support 'pdf-outline-buffer-mode 'pdf-outline
    `((deserialize . ,(lambda (_buffer _vars)
                        (let ((buf (get-buffer (car _vars))))
                          (if buf
                              (pdf-outline-noselect buf)))))
      (serialize . ,(lambda (_buffer)
                      (with-selected-window (pdf-outline-get-pdf-window)
                        (list (buffer-name))))))))

(provide 'init-workgroups)
