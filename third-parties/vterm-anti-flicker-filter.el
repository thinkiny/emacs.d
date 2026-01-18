;;; vterm-anti-flicker-filter.el --- Reduce flickering in vterm by buffering rapid output -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Martin Baillie

;; Author: Martin Baillie <martin@baillie.id>
;; Keywords: terminals, vterm, convenience
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (vterm "0.0.1"))
;; URL: https://github.com/mbaillie/vterm-anti-flicker-filter

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an anti-flicker filter for vterm that reduces
;; screen flickering when applications redraw multi-line content rapidly.
;;
;; The filter detects patterns commonly used for redrawing terminal content
;; (cursor positioning, line clearing, cursor movement) and buffers output
;; briefly when multiple escape sequences are detected, processing them in
;; a single batch to prevent flickering.
;;
;; Usage:
;;
;;   (require 'vterm-anti-flicker-filter)
;;
;; The filter is automatically enabled for all new vterm buffers.
;; You can also manually control it:
;;
;;   M-x vterm-anti-flicker-filter-enable   ; Enable in current buffer
;;   M-x vterm-anti-flicker-filter-disable  ; Disable in current buffer
;;   M-x vterm-anti-flicker-filter-toggle   ; Toggle in current buffer
;;
;; This is particularly useful when using TUI applications that frequently
;; redraw their interface, such as:
;; - Text editors (vim, Emacs in terminal)
;; - Interactive CLIs (Claude Code, AMP Code, K9s)
;; - File managers (ranger, lf)
;; - Process monitors (htop, btop)

;;; Code:

(require 'vterm)

(defgroup vterm-anti-flicker nil
  "Anti-flicker filter for vterm."
  :group 'vterm
  :prefix "vterm-anti-flicker-")

(defvar-local vterm-anti-flicker--output-buffer nil
  "Buffer for accumulating vterm output during potential flickering sequences.")

(defvar-local vterm-anti-flicker--output-timer nil
  "Timer for processing buffered vterm output.")

(defvar-local vterm-anti-flicker--original-filter nil
  "Original vterm process filter function before anti-flicker was enabled.")

(defun vterm-anti-flicker--flush-buffer (buffer)
  "Flush buffered output in BUFFER to the vterm process."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when vterm-anti-flicker--output-buffer
        (let ((data vterm-anti-flicker--output-buffer)
              (proc (get-buffer-process buffer)))
          (setq vterm-anti-flicker--output-buffer nil
                vterm-anti-flicker--output-timer nil)
          (when (and proc (process-live-p proc))
            (vterm--filter proc data)))))))

(defun vterm-anti-flicker--filter (process input)
  "Filter PROCESS INPUT to reduce vterm flickering by buffering rapid redraws.

Detects when INPUT contains patterns typical of terminal redrawing:
- Multiple escape sequences (3 or more)
- Line clearing sequences (ESC[K)
- Cursor positioning (ESC[n;mH)
- Cursor movement (ESC[nA/B/C/D)

When these patterns are detected, output is buffered for 16ms
and then processed in a single batch to prevent flickering."
  (with-current-buffer (process-buffer process)
    ;; Credit for these patterns: https://github.com/yuya373/claude-code-emacs.
    ;; Check if this looks like multi-line input box redraw
    ;; Common patterns when redrawing multi-line input:
    ;; - ESC[K (clear to end of line)
    ;; - ESC[<n>;<m>H (cursor positioning)
    ;; - ESC[<n>A/B/C/D (cursor movement)
    ;; - Multiple of these in sequence
    (let ((has-clear-line (string-match-p "\033\\[K" input))
          (has-cursor-pos (string-match-p "\033\\[[0-9]+;[0-9]+H" input))
          (has-cursor-move (string-match-p "\033\\[[0-9]*[ABCD]" input))
          (escape-count (cl-count ?\033 input)))
      ;; If we see multiple escape sequences that look like redrawing,
      ;; or we're already buffering, add to buffer
      (if (or (and (>= escape-count 3)
                   (or has-clear-line has-cursor-pos has-cursor-move))
              vterm-anti-flicker--output-buffer)
          (progn
            ;; Buffer this output
            (setq vterm-anti-flicker--output-buffer (concat vterm-anti-flicker--output-buffer input))
            ;; Reset timer
            (when vterm-anti-flicker--output-timer
              (cancel-timer vterm-anti-flicker--output-timer))
            (setq vterm-anti-flicker--output-timer
                  (run-at-time 0.016 nil #'vterm-anti-flicker--flush-buffer (current-buffer))))
        ;; Normal output, process immediately
        (vterm--filter process input)))))

;;;###autoload
(defun vterm-anti-flicker-filter-enable ()
  "Enable anti-flicker filter for the current vterm buffer.

Installs a process filter that detects and buffers rapid terminal
redraw sequences to prevent flickering.  The original filter is
preserved and can be restored with `vterm-anti-flicker-filter-disable'.

This function is automatically called for new vterm buffers when
the package is loaded."
  (interactive)
  (when (derived-mode-p 'vterm-mode)
    (when-let ((proc (get-buffer-process (current-buffer))))
      (unless vterm-anti-flicker--original-filter
        (setq vterm-anti-flicker--original-filter (process-filter proc)))
      (set-process-filter proc #'vterm-anti-flicker--filter)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when vterm-anti-flicker--output-timer
                    (cancel-timer vterm-anti-flicker--output-timer))) nil t))))

;;;###autoload
(defun vterm-anti-flicker-filter-disable ()
  "Disable anti-flicker filter for the current vterm buffer.

Restores the original vterm process filter and flushes any
buffered output immediately.  Cancels any pending timer."
  (interactive)
  (when (derived-mode-p 'vterm-mode)
    (when-let ((proc (get-buffer-process (current-buffer))))
      (when vterm-anti-flicker--output-timer
        (cancel-timer vterm-anti-flicker--output-timer)
        (setq vterm-anti-flicker--output-timer nil))
      (when vterm-anti-flicker--output-buffer
        (vterm--filter proc vterm-anti-flicker--output-buffer)
        (setq vterm-anti-flicker--output-buffer nil))
      (set-process-filter proc (or vterm-anti-flicker--original-filter #'vterm--filter))
      (setq vterm-anti-flicker--original-filter nil))))

;;;###autoload
(defun vterm-anti-flicker-filter-toggle ()
  "Toggle anti-flicker filter for the current vterm buffer.

If the filter is currently enabled, disable it.  If disabled, enable it."
  (interactive)
  (if vterm-anti-flicker--original-filter
      (vterm-anti-flicker-filter-disable)
    (vterm-anti-flicker-filter-enable)))

;; Auto-enable for new vterm buffers
(add-hook 'vterm-mode-hook #'vterm-anti-flicker-filter-enable)

(provide 'vterm-anti-flicker-filter)
;;; vterm-anti-flicker-filter.el ends here.
