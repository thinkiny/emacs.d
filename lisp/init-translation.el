;;; init-translation.el --- Translation configuration -*- lexical-binding: t -*-

;;; Code:

;; bing
(use-package bing-dict)
(defun bing-dict-at-point()
  (interactive)
  (let ((word (if (region-active-p)
                  (buffer-substring-no-properties
                   (region-beginning) (region-end))
                (let ((text (thing-at-point 'word)))
                  (if text (substring-no-properties text))))))
    (if word
        (bing-dict-brief word)
      (message "can't find word at point"))))

;; google
(require 'posframe)
(use-package google-translate
  :config
  (setq google-translate-show-phonetic t)
  (setq google-translate-output-destination 'posframe)
  (setq google-translate-default-source-language "auto")
  (setq google-translate-default-target-language "zh-CN"))

(defun google-translate-brief(text)
  (google-translate-translate
   google-translate-default-source-language google-translate-default-target-language
   text))

(defun google-translate--word-p (text)
  (not (string-match-p " " (string-trim text))))

(defun google-translate--format-word-output (gtos &optional detail)
  (let ((translation (gtos-translation gtos))
        (detailed-translation (gtos-detailed-translation gtos)))
    (concat
     (gtos-text gtos)
     (google-translate--text-phonetic gtos " [%s]")
     (if (and detail detailed-translation)
         (google-translate--detailed-translation
          detailed-translation translation "\n%s: " "\n%d. %s ")
       (concat ": " translation)))))

(defun google-translate--format-sentence-output (gtos)
  (concat
   (replace-regexp-in-string "\n" " " (gtos-text gtos))
   "\n"
   (replace-regexp-in-string "\n" " " (gtos-translation gtos))))

(defun google-translate--format-output (gtos &optional detail)
  "Format translation output from GTOS."
  (google-translate--trim-string
   (if (google-translate--word-p (gtos-text gtos))
       (google-translate--format-word-output gtos detail)
     (google-translate--format-sentence-output gtos))))

(defun google-translate-echo-area-output-translation (gtos)
  "Output translation to the echo area. "
  (message "%s" (google-translate--format-output gtos)))


(defun google-translate-posframe-output-translation (gtos)
  "Output translation to the posframe tooltip using `posframe'
package."
  (let ((cleanup-hook nil))
    (with-current-buffer (get-buffer-create " *google-translate-posframe*")
      (erase-buffer)
      (insert (google-translate--format-output gtos t)))
    (posframe-show " *google-translate-posframe*"
                   :position (if (derived-mode-p 'xwidget-webkit-mode)
                                 (or caret-xwidget-translate-pos
                                     (cdr (mouse-pixel-position)))
                               (point))
                   :max-width (round (* 0.95 (window-width)))
                   :internal-border-width 5
                   :border-color (face-background 'default)
                   :background-color (face-background 'default)
                   )
    (when (use-region-p)
      (deactivate-mark))
    (setq cleanup-hook
          (lambda ()
            (posframe-delete " *google-translate-posframe*")
            (remove-hook 'pre-command-hook cleanup-hook)))
    (add-hook 'pre-command-hook cleanup-hook)))

(use-proxy-local 'google-translate-request)

;; auto translate
(defvar-local auto-translate-mouse-selection nil)
(defun translate-mouse-selection()
  (interactive)
  (when auto-translate-mouse-selection
    (if (derived-mode-p 'nov-xwidget-webkit-mode)
        (xwidget-translate-range)
      (translate-at-point))))

(advice-add #'mouse-set-region-1 :after #'translate-mouse-selection)

;; tranlate functions
(defvar translate-backend 'google
  "Translation backend to use. Either 'bing or 'google.")

(defvar-local caret-xwidget-translate-pos nil
  "Saved pixel position (X . Y) for posframe in xwidget buffers.")

(defun translate-brief (text &optional sync-p)
  "Translate TEXT using configured backend."
  (pcase translate-backend
    ('bing (bing-dict-brief text sync-p))
    ('google (google-translate-brief text))
    (_ (error "Unknown translation backend: %s" translate-backend))))

(defun translate-at-point ()
  "Translate word at point or region using configured backend."
  (interactive)
  (let ((word (if (region-active-p)
                  (buffer-substring-no-properties
                   (region-beginning) (region-end))
                (let ((text (thing-at-point 'word)))
                  (if text (substring-no-properties text))))))
    (if word
        (translate-brief word)
      (message "can't find word at point"))))

(global-set-key (kbd "C-,") 'translate-at-point)

(provide 'init-translation)
