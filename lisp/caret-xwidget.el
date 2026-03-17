;;; caret-xwidget.el --- Emacs-like caret navigation for xwidget-webkit -*- lexical-binding: t -*-

(require 'xwidget)

(defgroup caret-xwidget nil
  "Emacs-like caret navigation for xwidget-webkit."
  :group 'xwidget-webkit
  :prefix "caret-xwidget-")

(defcustom caret-xwidget-js-file
  (expand-file-name "~/.emacs.d/assets/js/caret.js")
  "Path to the caret.js file.
The file is read at load time and injected into xwidget-webkit pages."
  :type 'file)

(defcustom caret-xwidget-reload-js-on-inject nil
  "If non-nil, reload `caret-xwidget-js-file' on every injection."
  :type 'boolean)

;; ---------------------------------------------------------------------------
;; Load caret.js source from file
;; ---------------------------------------------------------------------------

(defvar caret-xwidget--js-source nil
  "The caret.js source code to inject into xwidget pages.")

(defun caret-xwidget--load-js ()
  "Load the caret.js source from `caret-xwidget-js-file'."
  (let ((file (expand-file-name caret-xwidget-js-file)))
    (unless (file-readable-p file)
      (user-error "caret-xwidget: cannot read %s" file))
    (setq caret-xwidget--js-source
          (with-temp-buffer
            (insert-file-contents file)
            (buffer-string)))))

;; Load the JS source at require time.
(caret-xwidget--load-js)

;; ---------------------------------------------------------------------------
;; Injection
;; ---------------------------------------------------------------------------

(defun caret-xwidget--inject ()
  "Inject caret.js into the current xwidget-webkit session."
  (when-let* ((xw (xwidget-webkit-current-session)))
    (when caret-xwidget-reload-js-on-inject
      (caret-xwidget--load-js))
    (let ((post caret-xwidget--after-inject-js))
      (setq caret-xwidget--after-inject-js nil)
      (xwidget-webkit-execute-script xw
        (concat caret-xwidget--js-source
                (and post (concat "\n" caret-xwidget--js-prefix post)))))))

(defun caret-xwidget--callback-advice (orig-fn xwidget event-type)
  "Advice around `xwidget-webkit-callback' to re-inject caret.js on load."
  (funcall orig-fn xwidget event-type)
  (when (and (eq event-type 'load-changed)
             (string-equal (nth 3 last-input-event) "load-finished"))
    (caret-xwidget--inject)))

;; ---------------------------------------------------------------------------
;; Auto-pagination hooks
;; ---------------------------------------------------------------------------

(defvar-local caret-xwidget-next-page-function nil
  "Function called when movement reaches the end of the document.
Set this to navigate to the next document/chapter.")

(defvar-local caret-xwidget-previous-page-function nil
  "Function called when movement reaches the start of the document.
Set this to navigate to the previous document/chapter.")

(defvar-local caret-xwidget--after-inject-js nil
  "JS method to call on CaretEmacs after next caret.js injection.")

;; ---------------------------------------------------------------------------
;; Key dispatch helpers
;; ---------------------------------------------------------------------------

(defconst caret-xwidget--js-prefix
  "window.__caretEmacs && window.__caretEmacs."
  "JS guard prefix for calling methods on the CaretEmacs instance.")

(defun caret-xwidget--js-call (method &optional callback)
  "Execute a JS METHOD call on the CaretEmacs instance."
  (caret-xwidget--exec (concat caret-xwidget--js-prefix method) callback))

(defun caret-xwidget--exec (js &optional callback)
  "Execute JS string in the current xwidget-webkit session."
  (when-let* ((xw (xwidget-webkit-current-session)))
    (xwidget-webkit-execute-script xw js callback)))

(defun caret-xwidget--handle-boundary-result (result)
  "Auto-paginate based on boundary RESULT from moveWithBoundaryCheck."
  (when-let* ((fn (pcase (string-trim (or result "") "\"" "\"")
                    ("at-end"   caret-xwidget-next-page-function)
                    ("at-start" caret-xwidget-previous-page-function))))
    (setq caret-xwidget--after-inject-js
          (if (equal fn caret-xwidget-previous-page-function)
              "endOfBuffer()"
            "beginningOfBuffer()"))
    (funcall fn)))

(defun caret-xwidget--exec-with-boundary-cb (js)
  "Execute JS in xwidget; auto-paginate on boundary result."
  (when-let* ((xw (xwidget-webkit-current-session)))
    (if (or caret-xwidget-next-page-function
            caret-xwidget-previous-page-function)
        (let ((buf (current-buffer)))
          (xwidget-webkit-execute-script xw js
            (lambda (result)
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (caret-xwidget--handle-boundary-result result))))))
      ;; No page functions -- just execute the movement.
      (xwidget-webkit-execute-script xw js))))

(defun caret-xwidget--js-call-with-boundary (method granularity)
  "Call CaretEmacs METHOD with GRANULARITY, auto-paginating at boundaries."
  (caret-xwidget--exec-with-boundary-cb
   (concat caret-xwidget--js-prefix
           (format "moveWithBoundaryCheck('%s', '%s')" method granularity))))

;; ---------------------------------------------------------------------------
;; Movement commands
;; ---------------------------------------------------------------------------

(defun caret-xwidget-forward-char ()
  "Move caret forward one character."
  (interactive)
  (caret-xwidget--js-call "forward('character')"))

(defun caret-xwidget-backward-char ()
  "Move caret backward one character."
  (interactive)
  (caret-xwidget--js-call "backward('character')"))

(defun caret-xwidget-forward-word ()
  "Move caret forward one word."
  (interactive)
  (caret-xwidget--js-call "forward('word')"))

(defun caret-xwidget-backward-word ()
  "Move caret backward one word."
  (interactive)
  (caret-xwidget--js-call "backward('word')"))

(defun caret-xwidget-next-line ()
  "Move caret to next line."
  (interactive)
  (caret-xwidget--js-call-with-boundary "forward" "line"))

(defun caret-xwidget-previous-line ()
  "Move caret to previous line."
  (interactive)
  (caret-xwidget--js-call-with-boundary "backward" "line"))

(defun caret-xwidget-end-of-line ()
  "Move caret to end of line."
  (interactive)
  (caret-xwidget--js-call "forward('lineboundary')"))

(defun caret-xwidget-beginning-of-line ()
  "Move caret to beginning of line."
  (interactive)
  (caret-xwidget--js-call "backward('lineboundary')"))

(defun caret-xwidget-scroll-up ()
  "Scroll content up (advance forward in the document)."
  (interactive)
  (caret-xwidget--js-call "pageDown()"))

(defun caret-xwidget-scroll-down ()
  "Scroll content down (go back in the document)."
  (interactive)
  (caret-xwidget--js-call "pageUp()"))

(defun caret-xwidget-beginning-of-buffer ()
  "Move caret to the beginning of the document."
  (interactive)
  (caret-xwidget--js-call "beginningOfBuffer()"))

(defun caret-xwidget-end-of-buffer ()
  "Move caret to the end of the document."
  (interactive)
  (caret-xwidget--js-call "endOfBuffer()"))

(defun caret-xwidget-beginning-of-page ()
  "Move caret to the beginning of the current PDF page."
  (interactive)
  (caret-xwidget--js-call "beginningOfPage()"))

(defun caret-xwidget-end-of-page ()
  "Move caret to the end of the current PDF page."
  (interactive)
  (caret-xwidget--js-call "endOfPage()"))

(defun caret-xwidget-toggle-mark ()
  "Toggle mark (selection mode)."
  (interactive)
  (caret-xwidget--js-call "toggleMark()"))

(defun caret-xwidget-click ()
  "Simulate a mouse click at the current caret position."
  (interactive)
  (caret-xwidget--js-call "clickAtCaret()"))

(defun caret-xwidget-quit-mark ()
  "Deactivate mark (clear selection) and call `keyboard-quit'."
  (interactive)
  (caret-xwidget--js-call "_setMark(false)")
  (keyboard-quit))

(defun caret-xwidget-expand-selection ()
  "Progressively expand selection: word, then sentence, then paragraph."
  (interactive)
  (caret-xwidget--js-call "expandSelection()"))

(defconst caret-xwidget--word-at-caret-js
  (concat "(function(){var s=window.getSelection();"
          "if(!s.isCollapsed)return s.toString();"
          "var n=s.focusNode,o=s.focusOffset;"
          "if(n.nodeType!==3)return'';"
          "var t=n.textContent;"
          "var b=o;while(b>0&&/[\\w]/.test(t[b-1]))b--;"
          "var m=t.slice(b).match(/^[\\w]+(-[\\w]+)*/);"
          "return m?m[0]:''})()"))

(defun caret-xwidget-translate-word ()
  "Translate the word at caret, or the active selection if any."
  (interactive)
  (caret-xwidget--exec caret-xwidget--word-at-caret-js
    (lambda (text)
      (let ((word (string-trim (or text "") "\"" "\"")))
        (when (not (string-empty-p word))
          (translate-brief word))))))

;; ---------------------------------------------------------------------------
;; Debug helpers (caret.js)
;; ---------------------------------------------------------------------------

(defun caret-xwidget-debug-enable ()
  "Enable caret.js debug logging."
  (interactive)
  (caret-xwidget--js-call "enableDebug(true)"))

(defun caret-xwidget-debug-disable ()
  "Disable caret.js debug logging."
  (interactive)
  (caret-xwidget--js-call "enableDebug(false)"))

(defun caret-xwidget-debug-clear ()
  "Clear caret.js debug log."
  (interactive)
  (caret-xwidget--js-call "clearDebug()"))

(defun caret-xwidget-debug-dump ()
  "Dump caret.js debug log to *caret-debug* buffer."
  (interactive)
  (caret-xwidget--exec (concat caret-xwidget--js-prefix "dumpDebug()")
    (lambda (result)
      (let ((payload (string-trim (or result "") "\"" "\"")))
        (with-current-buffer (get-buffer-create "*caret-debug*")
          (erase-buffer)
          (insert payload)
          (goto-char (point-min))
          (pop-to-buffer (current-buffer)))))))

(defun caret-xwidget-debug-dump-to-file (&optional file)
  "Dump caret.js debug log to FILE (defaults to ~/.emacs.d/caret.log)."
  (interactive)
  (let ((target (expand-file-name (or file "~/.emacs.d/caret.log"))))
    (caret-xwidget--exec (concat caret-xwidget--js-prefix "dumpDebug()")
      (lambda (result)
        (let ((payload (string-trim (or result "") "\"" "\"")))
          (with-temp-buffer
            (insert payload)
            (write-region (point-min) (point-max) target nil 'silent))
          (message "caret.js debug log written to %s" target))))))

(defun caret-xwidget-reload-js ()
  "Reload caret.js from disk and re-inject it into the current xwidget."
  (interactive)
  (caret-xwidget--load-js)
  (caret-xwidget--inject)
  (message "caret.js reloaded and injected"))

;; ---------------------------------------------------------------------------
;; Keybindings -- directly in xwidget-webkit-mode-map
;; ---------------------------------------------------------------------------

(with-eval-after-load 'xwidget
  (define-key xwidget-webkit-mode-map (kbd "C-f")   #'caret-xwidget-forward-char)
  (define-key xwidget-webkit-mode-map (kbd "C-b")   #'caret-xwidget-backward-char)
  (define-key xwidget-webkit-mode-map (kbd "C-n")   #'caret-xwidget-next-line)
  (define-key xwidget-webkit-mode-map (kbd "C-p")   #'caret-xwidget-previous-line)
  (define-key xwidget-webkit-mode-map (kbd "C-a")   #'caret-xwidget-beginning-of-line)
  (define-key xwidget-webkit-mode-map (kbd "C-e")   #'caret-xwidget-end-of-line)
  (define-key xwidget-webkit-mode-map (kbd "C-v")   #'caret-xwidget-scroll-up)
  (define-key xwidget-webkit-mode-map (kbd "M-v")   #'caret-xwidget-scroll-down)
  (define-key xwidget-webkit-mode-map (kbd "SPC")   #'caret-xwidget-scroll-up)
  (define-key xwidget-webkit-mode-map (kbd "C-SPC") #'caret-xwidget-toggle-mark)
  (define-key xwidget-webkit-mode-map (kbd "C-g")   #'caret-xwidget-quit-mark)
  (define-key xwidget-webkit-mode-map (kbd "q")     #'caret-xwidget-quit-mark)
  (define-key xwidget-webkit-mode-map (kbd "M-<")   #'caret-xwidget-beginning-of-buffer)
  (define-key xwidget-webkit-mode-map (kbd "M->")   #'caret-xwidget-end-of-buffer)
  (define-key xwidget-webkit-mode-map (kbd "RET")   #'caret-xwidget-click)
  ;; Vi-style / WASD-style plain-key aliases (no modifier needed).
  (define-key xwidget-webkit-mode-map (kbd "f")     #'caret-xwidget-forward-word)
  (define-key xwidget-webkit-mode-map (kbd "b")     #'caret-xwidget-backward-word)
  (define-key xwidget-webkit-mode-map (kbd "a")     #'caret-xwidget-beginning-of-line)
  (define-key xwidget-webkit-mode-map (kbd "e")     #'caret-xwidget-end-of-line)
  (define-key xwidget-webkit-mode-map (kbd "n")     #'caret-xwidget-next-line)
  (define-key xwidget-webkit-mode-map (kbd "p")     #'caret-xwidget-previous-line)
  (define-key xwidget-webkit-mode-map (kbd "j")     #'caret-xwidget-next-line)
  (define-key xwidget-webkit-mode-map (kbd "k")     #'caret-xwidget-previous-line)
  (define-key xwidget-webkit-mode-map (kbd "s")     #'caret-xwidget-next-line)
  (define-key xwidget-webkit-mode-map (kbd "w")     #'caret-xwidget-previous-line)
  (define-key xwidget-webkit-mode-map (kbd ",")     #'caret-xwidget-translate-word)
  (define-key xwidget-webkit-mode-map (kbd "=")     #'caret-xwidget-expand-selection)

  ;; Inject caret.js on every page load (initial and subsequent navigations).
  (advice-add 'xwidget-webkit-callback :around #'caret-xwidget--callback-advice))

(provide 'caret-xwidget)

;;; caret-xwidget.el ends here
