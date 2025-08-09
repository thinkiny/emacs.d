(add-to-list 'display-buffer-alist
             '("\\*sqls\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (reusable-frames . visible)
               (window-height . 0.3)))

(defclass eglot-sqls (eglot-lsp-server) ()
  :documentation "SQL's Language Server")

(cl-defmethod eglot-execute ((server eglot-sqls) command)
  (let* ((uri (eglot-path-to-uri (buffer-file-name)))
         (beg (eglot--pos-to-lsp-position (if (use-region-p)
                                              (region-beginning)
                                            (point-min))))
         (end (eglot--pos-to-lsp-position (if (use-region-p)
                                              (region-end)
                                            (point-max))))
         (res (condition-case err
                  (eglot--request server :workspace/executeCommand
                                  `(:command ,(format "%s" (plist-get command :command))
                                             :range (:start ,beg :end ,end)
                                             :arguments [,uri]
                                             :timeout 0.5))
                (error (message "Failed to execute query: %s"
                                (error-message-string err))
                       nil))))
    (when res
      (let ((buffer (get-buffer-create "*sqls*")))
        (with-current-buffer buffer
          (erase-buffer)
          (insert (format "Result: %s" res))

          (goto-char (point-min))
          (delete-region (point) (search-forward "Result: "))
          (goto-char (point-min))
          (replace-regexp "\\+" "|" nil (point) (point-max))
          (goto-char (point-min))
          (while (re-search-forward (rx (seq (group alnum)
                                             (group (or "|" "_"))
                                             (group alnum))
                                        )
                                    nil t)
            (if-let* ((string-for-replacement (match-string-no-properties 2)))
                (cond
                 ((string= string-for-replacement "|")
                  (replace-match "\\1\\\\vert{}\\3" t))
                 ((string= string-for-replacement "_")
                  (replace-match "\\1\\\\under{}\\3" t)))))
          (org-mode)
          (setq org-pretty-entities t))
        (pop-to-buffer buffer)))))

(provide 'eglot-sqls)
