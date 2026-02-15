;;; file-server.el --- Local HTTP file server  -*- lexical-binding: t; -*-

(defgroup file-server nil
  "Local HTTP file server."
  :group 'tools)

(defcustom file-server-port "8123"
  "Port for the local file server."
  :type 'string
  :group 'file-server)

(defcustom file-server-script (expand-file-name "~/.emacs.d/scripts/file_server.py")
  "Directory containing the file server script."
  :type 'directory
  :group 'file-server)

(defun file-server-start ()
  (interactive)
  (unless (get-process "file-server")
    (set-process-sentinel
     (start-process "file-server" "*file-server*" "python3" file-server-script file-server-port)
     (lambda (process event)
       (message (format "%s: %s" process event))))
    (sit-for 1)))

(provide 'file-server)
;;; file-server.el ends here
