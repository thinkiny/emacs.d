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
(use-package posframe)
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
        (text-phonetic (gtos-text-phonetic gtos))
        (detailed-translation (gtos-detailed-translation gtos)))
    (concat
     (when (not (string-equal text-phonetic ""))
       (google-translate-paragraph
        text-phonetic
        'google-translate-phonetic-face
        " [%s]"))
     (if (and detail detailed-translation)
         (google-translate--detailed-translation
          detailed-translation translation "\n- %s:" "\n%d.%s ")
       (concat ": " translation)))))

(defun google-translate--format-sentence-output (gtos)
  (concat
   (replace-regexp-in-string "\n" " " (gtos-translation gtos))
   "\n"
   (replace-regexp-in-string "\n" " " (gtos-text gtos))
   ))

(defun google-translate--format-output (gtos &optional detail)
  "Format translation output from GTOS."
  (google-translate--trim-string
   (if (google-translate--word-p (gtos-text gtos))
       (google-translate--format-word-output gtos detail)
     (google-translate--format-sentence-output gtos))))

(defun google-translate-echo-area-output-translation (gtos)
  "Output translation to the echo area. "
  (message "%s" (google-translate--format-output gtos)))


(defun google-translate-posframe-pos()
  "Return (PX-POS . MAX-WIDTH) for posframe, clamped to frame right edge.
PX-POS is a cons (X . Y) in pixels."
  (let* ((min-cols 25)
         (px-pos (if (derived-mode-p 'xwidget-webkit-mode)
                     (or caret-xwidget-translate-pos
                         (cdr (mouse-pixel-position)))
                   (posn-x-y (posn-at-point (point)))))
         (avail (/ (- (frame-pixel-width) (car px-pos)) (frame-char-width))))
    (when (< avail min-cols)
      (setcar px-pos (max 0 (- (frame-pixel-width) (* min-cols (frame-char-width)))))
      (setq avail min-cols))
    (cons px-pos avail)))

(defun google-translate-posframe-output-translation (gtos)
  "Output translation to the posframe tooltip using `posframe'
package."
  (let* ((pos+width (google-translate-posframe-pos))
         (px-pos (car pos+width))
         (max-width (cdr pos+width))
         (cleanup-hook nil))
    (with-current-buffer (get-buffer-create " *google-translate-posframe*")
      (erase-buffer)
      (insert (google-translate--format-output gtos t)))
    (posframe-show " *google-translate-posframe*"
                   :position px-pos
                   :max-width (- max-width 5)
                   :internal-border-width 5
                   :border-color (face-background 'default)
                   :background-color (face-background 'default))
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
