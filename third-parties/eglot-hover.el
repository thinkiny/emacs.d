;;; eglot-hover.el --- Better hover information for eglot -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Tony Zorman
;;
;; Author: Tony Zorman <mail@tony-zorman.com>
;; Keywords: languages
;; Version: 0.1
;; Package-Requires: ((emacs "30.1") (dash "2.13.0") (s "1.10.0"))
;; Homepage: https://codeberg.org/slotThe/eglot-hover

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A minor mode to improve eglot's default hover messages.

;;; Code:

(require 'eglot)
(require 's)
(require 'dash)

(defgroup eglot-hover nil
  "Better hover messages for eglot."
  :group 'eglot)

(defcustom eglot-hover-assocs '((c-mode . "cpp") (rustic-mode . "rust") rust-mode
                                python-mode haskell-mode (tuareg-mode . "ocaml"))
  "A list of major modes with their associated code block names.
This is probably language server dependent, as e.g., clangd returns
`cpp' even for plain C buffers.  An element of this list can either be a
cons pair of (MODE . NAME), or just a symbol MODE, in which case the
name is inferred to be the same as MODE without the suffix \"-mode\"."
  :set (lambda (s v) (set-default-toplevel-value s (--map (if (consp it) it (cons it (s-chop-suffix "-mode" (symbol-name it)))) v)))
  :group 'eglot-hover
  :type '(repeat (choice (cons symbol string) symbol)))

(defun eglot-hover--hl-string (str mode)
  "Syntax highlight STR according to MODE."
  (with-temp-buffer
    (insert str)
    (delay-mode-hooks (funcall mode))
    (-each (--remove (-contains? '(nil rustic-setup-lsp eglot--managed-mode eldoc-mode flymake-mode-off) it)
                     (--mapcat (ignore-errors (symbol-value it)) delayed-mode-hooks))
      #'funcall)
    (font-lock-ensure)
    (buffer-string)))

(defun eglot-hover--get (lang str)
  "Get LANGs hover information in STR.
Original implementation (for rust-analyzer only) from
https://github.com/emacs-lsp/lsp-mode/pull/1740."
  (cl-flet ((join (sep strings)
              ;; This shields against python shenanigans like
              ;;
              ;; def f(
              ;;   a,
              ;;   b
              ;; )
              (--reduce (concat acc
                                (if (or (s-suffix? "(" acc)
                                        (s-prefix? ")" it))
                                    it
                                  (concat sep it)))
                        strings)))
    (let* ((start (concat "```" lang))
           (groups (--filter (or (s-equals? start (car it))
                                 (s-equals? start (cadr it)))
                             (-partition-by #'s-blank? (s-lines (s-trim str)))))
           (name-at-point (symbol-name (symbol-at-point)))
           (type-sig-group (car
                            (--filter (--any? (s-contains? name-at-point it) it)
                                      groups))))
      (->> (or type-sig-group (car groups))
           (--drop-while (not (s-prefix? start it)))
           (-drop 1)                    ; ``` LANG
           (-drop-last 1)               ; ```
           (-map #'s-trim)
           (--filter (not (s-matches? comment-start-skip it))) ; e.g. rust-analyzer puts size hints here
           (join " ")
           (s-chop-suffixes '("," "```" "``` ---"))))))

(defun eglot-hover--eldoc-function (cb &rest _ignored)
  "Override for `eglot-hover-eldoc-function', which see."
  (when (eglot-server-capable :hoverProvider)
    (let ((buf (current-buffer)))
      (eglot--async-request
       (eglot--current-server-or-lose)
       :textDocument/hover (eglot--TextDocumentPositionParams)
       :success-fn (eglot--lambda ((Hover) contents range)
                     (eglot--when-buffer-window buf
                       (let* ((info (unless (seq-empty-p contents)
                                      (eglot--hover-info contents range)))
                              (echo
                               (if (and (--any? (eq major-mode it) (-map #'car eglot-hover-assocs))
                                        (stringp info))
                                   (eglot-hover--hl-string
                                    (eglot-hover--get (alist-get major-mode eglot-hover-assocs)
                                                      (substring-no-properties info))
                                    major-mode)
                                 (let ((pos (and info (string-match "\n" info))))
                                   (while (and pos (get-text-property pos 'invisible info))
                                     (setq pos (string-match "\n" info (1+ pos))))
                                   pos))))
                         (funcall cb info :echo echo))))
       :hint :textDocument/hover))
    t))

;;;###autoload
(define-minor-mode eglot-hover-mode
  "Improve eglot's hover information."
  :lighter " eglot-hover"
  (unless (eglot-managed-p)
    (user-error "eglot-hover-mode requires eglot to be running."))
  (if eglot-hover-mode
      (advice-add 'eglot-hover-eldoc-function :override #'eglot-hover--eldoc-function)
    (advice-remove 'eglot-hover-eldoc-function #'eglot-hover--eldoc-function)))

(provide 'eglot-hover)
;;; eglot-hover.el ends here
