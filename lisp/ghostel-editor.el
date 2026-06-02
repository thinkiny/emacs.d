;;; ghostel-editor.el --- Edit text in a buffer and send it to ghostel -*- lexical-binding: t; -*-

;; Author: thinkiny
;; Package-Requires: ((emacs "28.1") (ghostel "0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Edit text in a regular Emacs buffer and send it to a ghostel terminal.
;;
;; Usage:
;;   1. From a ghostel buffer, call `ghostel-editor-open'.
;;   2. Write your text in the editor buffer that appears.
;;   3. Press C-c C-c to send the text to ghostel, or C-c C-k to cancel.

;;; Code:

(require 'ghostel)
(require 'subr-x)

(defvar-local ghostel-editor--source-buffer nil
  "The ghostel buffer where the edited text will be sent.")

(defvar ghostel-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ghostel-editor-finish)
    (define-key map (kbd "C-c C-k") #'ghostel-editor-abort)
    map)
  "Keymap for `ghostel-editor-mode'.")

(define-minor-mode ghostel-editor-mode
  "Minor mode active in the ghostel editor buffer.
\\{ghostel-editor-mode-map}"
  :lighter nil
  :keymap ghostel-editor-mode-map)

;;;###autoload
(defun ghostel-editor-open ()
  "Open a temporary buffer to compose text for the current ghostel.
The editor buffer uses `text-mode' with `ghostel-editor-mode' enabled.
Press \\[ghostel-editor-finish] to send the text to ghostel,
or \\[ghostel-editor-abort] to cancel."
  (interactive)
  (unless (derived-mode-p 'ghostel-mode)
    (user-error "Not in a ghostel buffer"))
  (let ((source (current-buffer))
        (buf (get-buffer-create "*ghostel-editor*")))
    (with-current-buffer buf
      (erase-buffer)
      (text-mode)
      (ghostel-editor-mode 1)
      (setq ghostel-editor--source-buffer source)
      (setq header-line-format
            (substitute-command-keys
             "Edit, then \\[ghostel-editor-finish] to send or \\[ghostel-editor-abort] to cancel")))
    (pop-to-buffer buf)))

(defun ghostel-editor-finish ()
  "Send the buffer content to the source ghostel and close the editor."
  (interactive)
  (let ((content (string-trim-right (buffer-string)))
        (source ghostel-editor--source-buffer))
    (unless (buffer-live-p source)
      (user-error "Source ghostel buffer no longer exists"))
    (quit-window t)
    (with-current-buffer source
      (ghostel-paste-string content))))

(defun ghostel-editor-abort ()
  "Close the editor buffer without sending anything to ghostel."
  (interactive)
  (quit-window t))

(provide 'ghostel-editor)
;;; ghostel-editor.el ends here
