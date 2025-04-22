;;; haskell-ts-mode.el --- A treesit based major mode for haskell -*- lexical-binding:t -*-

;; Copyright (C) 2024, 2025 Pranshu Sharma

;; Author: Pranshu Sharma <pranshu@bauherren.ovh>
;; URL: https://codeberg.org/pranshu/haskell-ts-mode
;; Package-Requires: ((emacs "29.3"))
;; Version: 1.01
;; Keywords: languages, haskell

;; This program is free software; you can redistribute it and/or modify
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

;; This is a major mode that uses treesitter to provide all the basic
;; major mode stuff, like indentation, font lock, etc...
;; It uses the grammer at: https://github.com/tree-sitter/tree-sitter-haskell

;;; Code:

(require 'comint)
(require 'treesit)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-prev-sibling "treesit.c")
(declare-function treesit-node-next-sibling "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-type "treesit.c")

(defgroup haskell-ts-mode nil
  "Group that contains haskell-ts-mode variables"
  :group 'langs)

(defcustom haskell-ts-ghci "ghci"
  "The command to be called to run ghci."
  :type 'string)

(defcustom haskell-ts-ghci-buffer-name "Inferior Haskell"
  "Buffer name for the ghci prcoess."
  :type 'string)

(defcustom haskell-ts-use-indent t
  "Set to non-nil to use the indentation provided by haskell-ts-mode"
  :type 'boolean)

(defcustom haskell-ts-font-lock-level 4
  "Level of font lock, 1 for minimum highlghting and 4 for maximum."
  :type '(choice (const :tag "Minimal Highlighting" 1)
                 (const :tag "Low Highlighting" 2)
                 (const :tag "High Highlighting" 3)
                 (const :tag "Maximum Highlighting" 4)))

(defcustom haskell-ts-prettify-words nil
  "Prettify some words to unicode symbols.
This will concat `haskell-ts-prettify-words-alist' to
`prettify-symbols-alist' in `haskell-ts-mode'."
  :type 'boolean)

(defvar haskell-ts-font-lock-feature-list
  `((comment str pragma parens)
    (type definition function args module import operator)
    (match keyword)
    (otherwise signature type-sig)))

(defvar haskell-ts-prettify-symbols-alist
  '(("\\" . "λ")
    ("/=" . "≠")
    ("->" . "→")
    ("=>" . "⇒")
    ("<-" . "←")
    ("<=" . "≤")
    (">=" . "≥")
    ("/<" . "≮")
    ("/>" . "≯")
    ("&&" . "∧")
    ("||" . "∨")
    ("==" . "≡"))
  "`prettify-symbols-alist' for `haskell-ts-mode'.
This variable contains all the symbol for `haskell-ts-mode' to unicode
character.  See `haskell-ts-prettify-words-alist' for mappign words to
alternative unicode character.")

(defvar haskell-ts-prettify-words-alist
  '(("forall"           . "∀")
    ("exists"           . "∃")
    ("elem"             . "∈")
    ("notElem"          . "∉")
    ("member"           . "∈")
    ("notMember"        . "∉")
    ("union"            . "∪")
    ("intersection"     . "∩")
    ("isSubsetOf"       . "⊆")
    ("isProperSubsetOf" . "⊂")
    ("mempty"           . "∅"))
  "Additional symbols to prettify for `haskell-ts-mode'.
This is added to `prettify-symbols-alist' for `haskell-ts-mode' buffers
when `haskell-ts-prettify-words' is non-nil.")

(defvar haskell-ts-font-lock
  (treesit-font-lock-rules
   :language 'haskell
   :feature 'keyword
   `(["module" "import" "data" "let" "where" "case" "type"
      "if" "then" "else" "of" "do" "in" "instance" "class" "newtype"]
     @font-lock-keyword-face)
   :language 'haskell
   :feature 'otherwise
   :override t
   `(((match (guards guard: (boolean (variable) @font-lock-keyword-face)))
      (:match "otherwise" @font-lock-keyword-face)))

   ;; This needs to be positioned above where we apply
   ;; font-lock-operator-face to comma
   :language 'haskell
   :override t
   :feature 'signature
   '((signature (function) @haskell-ts--fontify-type)
     (context (function) @haskell-ts--fontify-type)
     (signature "::" @font-lock-operator-face))

   ;; TODO: It is weird that we use operator face for parenthesses and also for operators.
   ;;   I see two other, possibly better solutions:
   ;;   1. Use delimiter face for parenthesses, ::, -> and similar, and operator face for operators.
   ;;   2. Keep using operator face for parenthesses and co, but use
   ;;   function call face for operators (since they are functions at
   ;;   the end).
   :language 'haskell
   :feature 'operator
   :override t
   '((operator) @font-lock-operator-face
     "," @font-lock-operator-face)

   :language 'haskell
   :feature 'module
   '((module (module_id) @font-lock-type-face))

   :language 'haskell
   :feature 'import
   '((import ["qualified" "as"] @font-lock-keyword-face))

   :language 'haskell
   :feature 'type-sig
   '((signature (binding_list (variable) @font-lock-doc-markup-face))
     (signature (variable) @font-lock-doc-markup-face))

   :language 'haskell
   :feature 'args
   :override 'keep
   '((function (infix left_operand: (_) @haskell-ts--fontify-arg))
     (function (infix right_operand: (_) @haskell-ts--fontify-arg))
     (generator :anchor (_) @haskell-ts--fontify-arg)
     (patterns) @haskell-ts--fontify-arg)

   :language 'haskell
   :feature 'type
   '((type) @font-lock-type-face
     (constructor) @font-lock-type-face
     (declarations (type_synomym (name) @font-lock-type-face))
     (declarations (data_type name: (name) @font-lock-type-face))
     (declarations (newtype name: (name) @font-lock-type-face)))

   :language 'haskell
   :feature 'match
   `((match ("|" @font-lock-doc-face) ("=" @font-lock-doc-face))
     (list_comprehension ("|" @font-lock-doc-face
                          (qualifiers (generator "<-" @font-lock-doc-face))))
     (match ("->" @font-lock-doc-face)))

   :language 'haskell
   :feature 'comment
   `(((comment) @font-lock-comment-face)
     ((haddock) @font-lock-doc-face))

   :language 'haskell
   :feature 'pragma
   `((pragma) @font-lock-preprocessor-face
     (cpp) @font-lock-preprocessor-face)

   :language 'haskell
   :feature 'str
   :override t
   `((char) @font-lock-string-face
     (string) @font-lock-string-face
     (quasiquote (quoter) @font-lock-type-face)
     (quasiquote (quasiquote_body) @font-lock-preprocessor-face))

   :language 'haskell
   :feature 'parens
   :override t
   `(["(" ")" "[" "]"] @font-lock-operator-face
     (infix operator: (_) @font-lock-operator-face))

   :language 'haskell
   :feature 'function
   :override t
   '((function name: (variable) @font-lock-function-name-face)
     (function (infix (operator)  @font-lock-function-name-face))
     (function (infix (infix_id (variable) @font-lock-function-name-face)))
     (bind :anchor (_) @haskell-ts--fontify-params)
     (function arrow: _ @font-lock-operator-face)))
  "The treesitter font lock settings for haskell.")

(defun haskell-ts--stand-alone-parent (_ parent bol)
  (save-excursion
    (goto-char (treesit-node-start parent))
    (let ((type (treesit-node-type parent)))
      (if (and (not bol)
               (or (looking-back "^[ \t]*" (line-beginning-position))
                   (member
                    type
                    '("when" "where" "do" "let" "local_binds" "function"))))
          (treesit-node-start parent)
        (haskell-ts--stand-alone-parent 1 (funcall
                                           (if bol #'treesit-node-parent #'identity)
                                           (treesit-node-parent parent))
                                        nil)))))

(defvar haskell-ts--ignore-types
  (regexp-opt '("comment" "cpp" "haddock"))
  "Node types that will be ignored by indentation.")

(defvar haskell-ts-indent-rules
  (let* ((p-sib
          (lambda (node &optional arg)
            (let* ((func (if arg
                             #'treesit-node-prev-sibling
                           #'treesit-node-next-sibling))
                   (n (funcall func node)))
              (while (and n (string-match haskell-ts--ignore-types
                                          (treesit-node-type n)))
                (setq n (funcall func n)))
              n)))
         (p-prev-sib
          (lambda (node &optional _ _) (treesit-node-start (funcall p-sib node t))))
         (p-n-prev (lambda (node) (funcall p-sib node t)))
         (parent-first-child (lambda (_ parent _)
                               (treesit-node-start (treesit-node-child parent 0)))))
    `((haskell
       ((node-is "^cpp$") column-0 0)
       ((parent-is "^comment$") column-0 0)
       ((parent-is "^haddock$") column-0 0)
       ((parent-is "^imports$") column-0 0)
       ;; Infix
       ((n-p-gp nil "infix" "infix")
        (lambda (_ node _)
          (let ((first-inf nil))
            (while (string= "infix"
                            (treesit-node-type
                             (setq node (treesit-node-parent node))))
              (setq first-inf node))
            (funcall ,parent-first-child nil first-inf nil)))
        0)
       ((node-is "^infix$") ,parent-first-child 0)

       ;; Lambda
       ((parent-is "^lambda\\(_case\\)?$") standalone-parent 2)

       ((parent-is "^class_declarations$") prev-sibling 0)

       ((node-is "^where$") parent 2)

       ;; in
       ((node-is "^in$") parent 0)

       ((parent-is "qualifiers") parent 0)

       ;; list
       ((node-is "^]$") parent 0)
       ((parent-is "^list$") standalone-parent 2)

       ;; If then else
       ((node-is "^then$") parent 2)
       ((node-is "^else$") parent 2)

       ((parent-is "^apply$") haskell-ts--stand-alone-parent 1)
       ((node-is "^quasiquote$") grand-parent 2)
       ((parent-is "^quasiquote_body$") (lambda (_ _ c) c) 0)
       ((lambda (node parent bol)
          (when-let ((n (treesit-node-prev-sibling node)))
            (while (string= "comment" (treesit-node-type n))
              (setq n (treesit-node-prev-sibling n)))
            (string= "do" (treesit-node-type n))))
        haskell-ts--stand-alone-parent
        3)
       ((parent-is "^do$") ,p-prev-sib 0)

       ((parent-is "^alternatives$") ,p-prev-sib 0)

       ;; prev-adaptive-prefix is broken sometimes
       (no-node
        (lambda (_ _ _)
          (save-excursion
            (goto-char (line-beginning-position 0))
            (back-to-indentation)
            (point)))
        0)

       ((parent-is "^data_constructors$") parent 0)

       ;; where
       ((lambda (node _ _)
          (let ((n (treesit-node-prev-sibling node)))
            (while (string= "comment" (treesit-node-type n))
              (setq n (treesit-node-prev-sibling n)))
            (string= "where" (treesit-node-type n))))

        (lambda (_ b _)
          (+ 1 (treesit-node-start (treesit-node-prev-sibling b))))
        3)
       ((parent-is "local_binds\\|instance_declarations") ,p-prev-sib 0)

       ;; Match
       ((lambda (node _ _)
          (and (string= "match" (treesit-node-type node))
               (string-match (regexp-opt '("patterns" "variable"))
                             (treesit-node-type (funcall ,p-n-prev node)))))
        standalone-parent 2)

       ((node-is "match") ,p-prev-sib 0)
       ((parent-is "match") standalone-parent 2)

       ((parent-is "^haskell$") column-0 0)
       ((parent-is "^declarations$") column-0 0)

       ((parent-is "^record$") standalone-parent 2)

       ((parent-is "^exports$")
        (lambda (_ b _) (treesit-node-start (treesit-node-prev-sibling b)))
        0)
       ((n-p-gp nil "signature" "foreign_import") grand-parent 3)
       ((parent-is "^case$") standalone-parent 4)
       ((node-is "^alternatives$")
        (lambda (_ b _)
          (treesit-node-start (treesit-node-child b 0)))
        2)
       ((node-is "^comment$")
        (lambda (node parent _)
          (pcase node
            ;; (relevent means type not it haskell-ts--ignore-types)
            ;; 1. next relevent sibling if exists
            ((app ,p-sib (and (pred (not null)) n))
             (treesit-node-start n))
            ;; 2. previous relevent sibling if exists
            ((app ,p-prev-sib (and (pred (not null)) n))
             n)
            ;; 3. parent
            (_ (treesit-node-start parent))))
        0)

       ((node-is "|") parent 1)
       
       ;; Backup
       (catch-all parent 2))))
  "\"Simple\" treesit indentation rules for haskell.")

(defvar haskell-ts-mode-syntax-table
  (eval-when-compile
    (let ((table (make-syntax-table))
          (syntax-list
           `((" " " \t\n\r\f\v")
             ("_" "!#$%&*+./<=>?\\^|-~:")
             ("w" ?_ ?\')
             ("." ",:@")
             ("\"" ?\")
             ("()" ?\()
             (")(" ?\))
             ("(]" ?\[)
             (")[" ?\])
             ("$`" ?\`)
             ("(}1nb" ?\{ )
             ("){4nb" ?\} )
             ("_ 123" ?- )
             (">" "\r\n\f\v"))))
      (dolist (ls syntax-list table)
        (dolist (char (if (stringp (cadr ls))
                          (string-to-list (cadr ls))
                        (cdr ls)))
          (modify-syntax-entry char (car ls) table)))))
  "The syntax table for haskell.")

(defun haskell-ts-sexp (node)
  "Returns non-nil on a sexp node."
  (let ((node-text (treesit-node-text node 1)))
    (and
     (not (member node-text '( "{" "}" "[" "]" "(" ")" ";")))
     (not (and (string= "operator" (treesit-node-field-name node))
               (= 1 (length node-text)))))))

(defvar haskell-ts-thing-settings
  `((haskell
     (sexp haskell-ts-sexp)
     (sentence "match")
     (string "string")
     (text "string")))
  "`treesit-thing-settings' for `haskell-ts-mode'.")

(defmacro haskell-ts-imenu-name-function (check-func)
  `(lambda (node)
     (let ((nn (treesit-node-child node 0 node)))
       (if (funcall ,check-func node)
           (if (string= (treesit-node-type nn) "infix")
               (treesit-node-text (treesit-node-child nn 1))
             (haskell-ts-defun-name node))
         nil))))

(defvar-keymap  haskell-ts-mode-map
  :doc "Keymap for haskell-ts-mode."
  "C-c C-c" #'haskell-ts-compile-region-and-go
  "C-c C-r" #'run-haskell)

;;;###autoload
(define-derived-mode haskell-ts-mode prog-mode "haskell ts mode"
  "Major mode for Haskell files using tree-sitter."
  :table haskell-ts-mode-syntax-table
  (unless (treesit-ready-p 'haskell)
    (error "Tree-sitter for Haskell is not available"))
  (setq treesit-primary-parser (treesit-parser-create 'haskell))
  (setq treesit-language-at-point-function
        (lambda (&rest _) 'haskell))
  (setq-local treesit-defun-type-regexp "\\(?:\\(?:function\\|struct\\)_definition\\)")
  ;; Indent
  (when haskell-ts-use-indent
    (setq-local treesit-simple-indent-rules haskell-ts-indent-rules)
    (setq-local indent-tabs-mode nil))
  ;; Comment
  (setq-local comment-start "-- ")
  (setq-local comment-use-syntax t)
  (setq-local comment-start-skip "\\(?: \\|^\\)-+")
  ;; Electric
  (setq-local electric-pair-pairs
              '((?` . ?`) (?\( . ?\)) (?{ . ?}) (?\" . ?\") (?\[ . ?\])))
  ;; Navigation
  (setq-local treesit-defun-name-function 'haskell-ts-defun-name)
  (setq-local treesit-thing-settings haskell-ts-thing-settings)
  (setq-local treesit-defun-type-regexp
              ;; Since haskell is strict functional, any 2nd level
              ;; entity is defintion
              (cons ".+"
                    (lambda (node)
                      (and (not (string-match haskell-ts--ignore-types (treesit-node-type node)))
                           (string= "declarations" (treesit-node-type (treesit-node-parent node)))))))
  (setq-local prettify-symbols-alist
              (append haskell-ts-prettify-symbols-alist
                      (and haskell-ts-prettify-words
                           haskell-ts-prettify-words-alist)))

  ;; Imenu
  (setq-local treesit-simple-imenu-settings
              `((nil haskell-ts-imenu-func-node-p nil
                     ,(haskell-ts-imenu-name-function #'haskell-ts-imenu-func-node-p))
                ("Signatures.." haskell-ts-imenu-sig-node-p nil
                 ,(haskell-ts-imenu-name-function #'haskell-ts-imenu-sig-node-p))
                ("Data..." haskell-ts-imenu-data-type-p nil
                 (lambda (node)
                   (treesit-node-text (treesit-node-child node 1))))))
  ;; font-lock
  (setq-local treesit-font-lock-level haskell-ts-font-lock-level)
  (setq-local treesit-font-lock-settings haskell-ts-font-lock)
  (setq-local treesit-font-lock-feature-list
              haskell-ts-font-lock-feature-list)
  (treesit-major-mode-setup))

(defun haskell-ts--fontify-func (node face)
  (if (string= "variable" (treesit-node-type node))
      (put-text-property
       (treesit-node-start node)
       (treesit-node-end node)
       'face face)
    (mapc (lambda (n) (haskell-ts--fontify-func n face))
          (treesit-node-children node))))

(defun haskell-ts--fontify-arg (node &optional _ _ _)
  (haskell-ts--fontify-func node 'font-lock-variable-name-face))

(defun haskell-ts--fontify-params (node &optional _ _ _)
  (haskell-ts--fontify-func node 'font-lock-function-name-face))

(defun haskell-ts--fontify-type (node &optional _ _ _)
  (let ((last-child (treesit-node-child node -1)))
    (if (string= (treesit-node-type last-child) "function")
        (haskell-ts--fontify-type last-child)
      (put-text-property
       (treesit-node-start last-child)
       (treesit-node-end last-child)
       'face 'font-lock-variable-name-face))))

(defun haskell-ts-imenu-node-p (regex node)
  (and (string-match-p regex (treesit-node-type node))
       (string= (treesit-node-type (treesit-node-parent node)) "declarations")))

(defun haskell-ts-imenu-func-node-p (node)
  (haskell-ts-imenu-node-p "function\\|bind" node))

(defun haskell-ts-imenu-sig-node-p (node)
  (haskell-ts-imenu-node-p "signature" node))

(defun haskell-ts-imenu-data-type-p (node)
  (haskell-ts-imenu-node-p "data_type" node))

(defun haskell-ts-defun-name (node)
  (treesit-node-text (treesit-node-child node 0)))

(defun haskell-ts-compile-region-and-go ()
  "Compile the text from START to END in the haskell proc.
If region is not active, reload the whole file."
  (interactive)
  (let ((hs (haskell-ts-haskell-session)))
    (if (region-active-p)
        (let ((str (buffer-substring-no-properties
                    (region-beginning)
                    (region-end))))
          (comint-send-string hs ":{\n")
          (comint-send-string
           hs
           ;; Things that may cause problem to ghci need to be
           ;; escaped.  TODO examine if other lines that start with
           ;; colons might cause problems
           (replace-regexp-in-string "^:\\}" "\\:}" str nil t))
          (comint-send-string hs "\n:}\n"))
      (comint-send-string hs ":r\n"))))

;;;###autoload
(defun run-haskell ()
  "Run an inferior Haskell process."
  (interactive)
  (let ((buffer (concat "*" haskell-ts-ghci-buffer-name "*")))
    (pop-to-buffer-same-window
     (if (comint-check-proc buffer)
         buffer
       (make-comint haskell-ts-ghci-buffer-name haskell-ts-ghci nil buffer-file-name)))))

(defun haskell-ts-haskell-session ()
  (get-buffer-process (concat "*" haskell-ts-ghci-buffer-name "*")))

(when (treesit-ready-p 'haskell)
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-ts-mode)))

(provide 'haskell-ts-mode)

;; derive from `haskell-mode' on emacs v30+
(when (functionp 'derived-mode-add-parents)
  (derived-mode-add-parents 'haskell-ts-mode '(haskell-mode)))

;;; haskell-ts-mode.el ends here
