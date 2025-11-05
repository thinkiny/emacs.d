;; -*- lexical-binding: t; -*-

(use-package erlang
  :mode (("\\.erl?$" . erlang-mode)
         ("rebar\\.config$" . erlang-mode)
         ("relx\\.config$" . erlang-mode)
         ("sys\\.config\\.src$" . erlang-mode)
         ("sys\\.config$" . erlang-mode)
         ("\\.config\\.src?$" . erlang-mode)
         ("\\.config\\.script?$" . erlang-mode)
         ("\\.hrl?$" . erlang-mode)
         ("\\.app?$" . erlang-mode)
         ("\\.app.src?$" . erlang-mode)
         ("\\Emakefile" . erlang-mode))
  :config
  (setq erlang-max-files-to-visit-for-refining-xrefs 256)
  (setq erlang-electric-commands (delete 'erlang-electric-gt erlang-electric-commands))
  (unbind-key (kbd "RET") 'erlang-mode-map))

(add-hook 'erlang-mode-hook
          (lambda ()
            (setq-local lsp-enable-format-at-save nil)
            (lsp-later)))

(defun init-erlang-ls()
  (interactive)
  (when-let* ((project-root (projectile-project-root)))
    (write-region
     "apps_dirs:
  - \"apps/*\"
deps_dirs:
  - \"_build/default/lib/*\"
include_dirs:
  - \"apps\"
  - \"apps/*/include\"
  - \"_build/default/lib/\"
  - \"_build/default/lib/*/include\""
     nil
     (format "%s/erlang_ls.config" project-root))))


(with-eval-after-load 'erlang-mode
  (defun erlang-calculate-stack-indent (indent-point state)
    "From the given last position and state (stack) calculate indentation.
Return nil if inside string, t if in a comment."
    (let* ((stack (and state (car state)))
           (token (nth 1 state))
           (stack-top (and stack (car stack))))
      (cond ((null state)                 ;No state
             0)
            ((nth 3 state)
             ;; Return nil or t.
             (eq (nth 3 state) 'comment))
            ((null stack)
             (if (looking-at "when[^_a-zA-Z0-9]")
                 erlang-indent-guard
               0))
            ((eq (car stack-top) '\()
             ;; Element of list, tuple or part of an expression,
             (cond ((null erlang-argument-indent)
                    ;; indent to next column.
                    (1+ (nth 2 stack-top)))
                   ((= (char-syntax (following-char)) ?\))
                    (goto-char (nth 1 stack-top))
                    (cond ((erlang-record-or-function-args-p)
                           ;; Line ends with parenthesis.
                           (let ((previous (erlang-indent-find-preceding-expr))
                                 (stack-pos (nth 2 stack-top)))
                             (if (>= previous stack-pos) stack-pos
                               (- (+ previous erlang-argument-indent) 1))))
                          (t
                           (nth 2 stack-top))))
                   ((looking-at "||")
                    (erlang-indent-element stack-top indent-point token))
                   ((memq (following-char) '(?, ?|))
                    ;; a comma at the start of the line: line up with opening parenthesis.
                    (min (nth 2 stack-top)
                         (erlang-indent-element stack-top indent-point token)))
                   (t
                    (erlang-indent-element stack-top indent-point token))))
            ;;
            ((eq (car stack-top) '<<)
             ;; Element of binary (possible comprehension) expression,
             (cond ((null erlang-argument-indent)
                    ;; indent to next column.
                    (+ 2 (nth 2 stack-top)))
                   ((looking-at "\\(>>\\)[^_a-zA-Z0-9]")
                    (nth 2 stack-top))
                   ((= (following-char) ?,)
                    (min (+ (nth 2 stack-top) 1)
                         (- (erlang-indent-to-first-element stack-top 2) 1)))
                   (t
                    (erlang-indent-to-first-element stack-top 2))))

            ((memq (car stack-top) '(icr fun spec_arg))
             ;; The default indentation is the column of the option
             ;; directly following the keyword. (This does not apply to
             ;; `case'.)  Should no option be on the same line, the
             ;; indentation is the indentation of the keyword +
             ;; `erlang-indent-level'.
             ;;
             ;; `after' should be indented to the same level as the
             ;; corresponding receive.
             (cond ((looking-at "\\(after\\|of\\)\\($\\|[^_a-zA-Z0-9]\\)")
                    (nth 2 stack-top))
                   ((looking-at "when[^_a-zA-Z0-9]")
                    ;; Handling one when part
                    (+ (nth 2 stack-top) erlang-indent-level erlang-indent-guard))
                   (t
                    (save-excursion
                      (goto-char (nth 1 stack-top))
                      (if (and erlang-icr-indent
                               (looking-at "\\(if\\|case\\|receive\\|try\\)[^_a-zA-Z0-9]"))
                          (+ (nth 2 stack-top) erlang-indent-level)
                        (if (looking-at "\\(case\\|receive\\|try\\)[^_a-zA-Z0-9]")
                            (+ (nth 2 stack-top) erlang-indent-level)
                          (skip-chars-forward "a-z")
                          (skip-chars-forward " \t")
                          (if (memq (following-char) '(?% ?\n))
                              (+ (nth 2 stack-top) erlang-indent-level)
                            (current-column))))))))
            ((and (eq (car stack-top) '||) (looking-at "\\(]\\|>>\\)[^_a-zA-Z0-9]"))
             (nth 2 (car (cdr stack))))
            ;; Real indentation, where operators create extra indentation etc.
            ((memq (car stack-top) '(-> || try begin))
             (if (looking-at "\\(of\\)[^_a-zA-Z0-9]")
                 (nth 2 stack-top)
               (goto-char (nth 1 stack-top))
               ;; Check if there is more code after the '->' on the
               ;; same line. If so use this indentation as base, else
               ;; use parent indentation + 2 * level as base.
               (let ((off erlang-indent-level)
                     (skip 2))
                 (cond ((null (cdr stack))) ; Top level in function.
                       ((eq (car stack-top) 'begin)
                        (setq skip 5))
                       ((eq (car stack-top) 'try)
                        (setq skip 5))
                       ((eq (car stack-top) '->)
                        ;; If in fun definition use standard indent level not double
                        (prin1 stack-top)
                        (if (not (eq (car (car (cdr stack))) 'try))
                            ;; Removed it made multi clause Named fun's look too bad
                            (setq off (+ erlang-indent-level
                                         (if (not erlang-icr-indent)
                                             erlang-indent-level
                                           erlang-icr-indent))))))
                 (let ((base (erlang-indent-find-base stack indent-point off skip)))
                   ;; Special cases
                   (goto-char indent-point)
                   (cond ((looking-at "\\(;\\|end\\|after\\)\\($\\|[^_a-zA-Z0-9]\\)")
                          (if (eq (car stack-top) '->)
                              (erlang-pop stack))
                          (cond ((and stack (looking-at ";"))
                                 (+ (caddr (car stack)) (- erlang-indent-level 2)))
                                (stack (caddr (car stack)))
                                (t off)))
                         ((looking-at "catch\\b\\($\\|[^_a-zA-Z0-9]\\)")
                          ;; Are we in a try
                          (let ((start (if (eq (car stack-top) '->)
                                           (car (cdr stack))
                                         stack-top)))
                            (if (null start) nil
                              (goto-char (nth 1 start)))
                            (cond ((looking-at "try\\($\\|[^_a-zA-Z0-9]\\)")
                                   (progn
                                     (if (eq (car stack-top) '->)
                                         (erlang-pop stack))
                                     (if stack
                                         (caddr (car stack))
                                       0)))
                                  (t (erlang-indent-standard indent-point token base 'nil))))) ;; old catch
                         ;; Indent result types
                         ((eq (car (car (cdr stack))) 'spec_arg)
                          (setq base (+ (caddr (car (last stack))) erlang-indent-level))
                          (erlang-indent-standard indent-point token base 'nil))
                         (t
                          (erlang-indent-standard indent-point token base 'nil)
                          ))))
               ))
            ((eq (car stack-top) 'when)
             (goto-char (nth 1 stack-top))
             (if (looking-at "when\\s *\\($\\|%\\)")
                 (progn
                   (erlang-pop stack)
                   (if (and stack (memq (nth 0 (car stack)) '(icr fun)))
                       (progn
                         (goto-char (nth 1 (car stack)))
                         (+ (nth 2 (car stack)) erlang-indent-guard
                            ;; receive XYZ    or    receive
                            ;;                          XYZ
                            ;; This if thing does not seem to be needed
                            ;;(if (looking-at "[a-z]+\\s *\\($\\|%\\)")
                            ;;    erlang-indent-level
                            ;;  (* 2 erlang-indent-level))))
                            (* 2 erlang-indent-level)))
                     ;;erlang-indent-level))
                     (+ erlang-indent-level erlang-indent-guard)))
               ;; "when" is followed by code, let's indent to the same
               ;; column.
               (forward-char 4)           ; Skip "when"
               (skip-chars-forward " \t")
               (current-column)))
            ;; Type and Spec indentation
            ((eq (car stack-top) '::)
             (if (looking-at "[},)]")
                 ;; Closing function spec, record definition with types,
                 ;; or a comma at the start of the line
                 ;; pop stack and recurse
                 (erlang-calculate-stack-indent indent-point
                                                (cons (erlang-pop stack) (cdr state)))
               (cond ((null erlang-argument-indent)
                      ;; indent to next column.
                      (+ 2 (nth 2 stack-top)))
                     ((looking-at "::[^_a-zA-Z0-9]")
                      (nth 2 stack-top))
                     (t
                      (let ((start-alternativ (if (looking-at "|") 2 0)))
                        (goto-char (nth 1 stack-top))
                        (- (cond ((looking-at "::\\s *\\($\\|%\\)")
                                  ;; Line ends with ::
                                  (if (eq (car (car (last stack))) 'spec)
                                      (+ (erlang-indent-find-preceding-expr 1)
                                         erlang-argument-indent)
                                    (+ (erlang-indent-find-preceding-expr 2)
                                       erlang-argument-indent)))
                                 (t
                                  ;; Indent to the same column as the first
                                  ;; argument.
                                  (goto-char (+ 2 (nth 1 stack-top)))
                                  (skip-chars-forward " \t")
                                  (current-column))) start-alternativ))))))
            ))))

(provide 'init-erlang)
