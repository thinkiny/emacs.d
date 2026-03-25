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
(use-package google-translate
  :config
  (setq google-translate-show-phonetic t)
  (setq google-translate-output-destination 'echo-area)
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
          detailed-translation translation "\n%s: " "%d. %s ")
       (concat ": " translation)))))

(defun google-translate--format-sentence-output (gtos)
  (concat
   (replace-regexp-in-string "\n" "" (gtos-text gtos))
   "\n"
   (replace-regexp-in-string "\n" "" (gtos-translation gtos))))


(with-eval-after-load 'google-translate
  (defun google-translate-echo-area-output-translation (gtos)
    "Output translation to the echo area (See
http://www.gnu.org/software/emacs/manual/html_node/elisp/The-Echo-Area.html)"
    (message
     (google-translate--trim-string
      (if (google-translate--word-p (gtos-text gtos))
          (google-translate--format-word-output gtos)
        (google-translate--format-sentence-output gtos))))))

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
