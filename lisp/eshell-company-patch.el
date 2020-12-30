

;;; eshell-company-patch.el -*- lexical-binding: t; -*-

;; Copyright (C) 1999-2020 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; The `eshell-complete-parse-arguments' function has been modified to work
;; better with the `company-mode' package.
;;
;; The function was originally taken from the source file `em-cmpl',
;; for which original source can be found here:
;;   https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/eshell/em-cmpl.el
;;
;; Notable changes:
;;   - Catching of pcompleted inside `eshell-complete-parse-arguments'
;;   - Removal of some `throw's
;;   - Changing the remaining `throw's second parameter to be:
;;       `(nil ,begin ,end)
;;     rather than `t'.

(defun doom-eshell-overrides ()
  (defun eshell-complete-parse-arguments ()
    "Parse the command line arguments for `pcomplete-argument'."
    (catch 'pcompleted
      (unless (and eshell-no-completion-during-jobs
                   (eshell-interactive-process))
        (let ((end (point-marker))
              (begin (save-excursion (eshell-bol) (point)))
              (posns (list t))
              args delim)
          (when (memq this-command '(pcomplete-expand
                                     pcomplete-expand-and-complete))
            (run-hook-with-args 'eshell-expand-input-functions begin end)
            (if (= begin end)
                (end-of-line))
            (setq end (point-marker)))
          (if (setq delim
                    (catch 'eshell-incomplete
                      (ignore
                       (setq args (eshell-parse-arguments begin end)))))
              (cond ((memq (car delim) '(?\{ ?\<))
                     (setq begin (1+ (cadr delim))
                           args (eshell-parse-arguments begin end)))
                    (t (throw 'pcompleted `(nil ,begin ,end)))))
          (when (get-text-property (1- end) 'comment)
            (throw 'pcompleted `(nil ,begin ,end)))
          (let ((pos begin))
            (while (< pos end)
              (if (get-text-property pos 'arg-begin)
                  (nconc posns (list pos)))
              (setq pos (1+ pos))))
          (setq posns (cdr posns))
          (cl-assert (= (length args) (length posns)))
          (let ((a args)
                (i 0)
                l)
            (while a
              (if (and (consp (car a))
                       (eq (caar a) 'eshell-operator))
                  (setq l i))
              (setq a (cdr a) i (1+ i)))
            (and l
                 (setq args (nthcdr (1+ l) args)
                       posns (nthcdr (1+ l) posns))))
          (cl-assert (= (length args) (length posns)))
          (when (and args (eq (char-syntax (char-before end)) ? )
                     (not (eq (char-before (1- end)) ?\\)))
            (nconc args (list ""))
            (nconc posns (list (point))))
          (cons (mapcar
                 (function
                  (lambda (arg)
                    (let ((val
                           (if (listp arg)
                               (let ((result
                                      (eshell-do-eval
                                       (list 'eshell-commands arg) t)))
                                 (cl-assert (eq (car result) 'quote))
                                 (cadr result))
                             arg)))
                      (if (numberp val)
                          (setq val (number-to-string val)))
                      (or val ""))))
                 args)
                posns))))))

(add-hook 'eshell-cmpl-load-hook #'doom-eshell-overrides)

(provide 'eshell-company-patch)
;; eshell-company-patch.el ends here
