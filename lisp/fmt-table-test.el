;; -*- lexical-binding: t; -*-
;;; fmt-table-test.el --- ERT tests for fmt-table.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'fmt-table)

(defmacro fmt-table--with-table (table-text &rest body)
  "Insert TABLE-TEXT in a temp gfm-mode buffer, eval BODY."
  (declare (indent 1) (debug (stringp body)))
  `(with-temp-buffer
     (gfm-mode)
     (insert ,table-text)
     (goto-char (point-min))
     ,@body))

;; ;;;; fmt-table--parse-cells

(ert-deftest fmt-table-parse-cells-simple ()
  "Parse a simple pipe-delimited row."
  (should (equal (fmt-table--parse-cells "| A | B | C |")
                 '("A" "B" "C"))))

(ert-deftest fmt-table-parse-cells-no-border-pipes ()
  "Parse a row without leading/trailing pipes."
  (should (equal (fmt-table--parse-cells "A | B | C")
                 '("A" "B" "C"))))

(ert-deftest fmt-table-parse-cells-escaped-pipe ()
  "Parse a row with escaped \\| inside a cell."
  (should (equal (fmt-table--parse-cells "| a\\|b | c |")
                 '("a\\|b" "c"))))

(ert-deftest fmt-table-parse-cells-empty-cells ()
  "Parse a row with some empty cells."
  (should (equal (fmt-table--parse-cells "| | B | |")
                 '("" "B" ""))))

(ert-deftest fmt-table-parse-cells-whitespace-trimmed ()
  "Cell content is trimmed."
  (should (equal (fmt-table--parse-cells "|  A  |  B  |")
                 '("A" "B"))))

;; ;;;; fmt-table--hline-p

(ert-deftest fmt-table-hline-p-basic ()
  "Detect a basic separator row."
  (should (fmt-table--hline-p "| --- | --- |")))

(ert-deftest fmt-table-hline-p-aligned ()
  "Detect aligned separator rows."
  (should (fmt-table--hline-p "| :--- | ---: | :---: |")))

(ert-deftest fmt-table-hline-p-not-data ()
  "Data row is not a separator."
  (should-not (fmt-table--hline-p "| A | B |")))

;; ;;;; fmt-table--all-empty-p

(ert-deftest fmt-table-all-empty-p-all-blank ()
  "All-empty cells return t."
  (should (fmt-table--all-empty-p '("" "" ""))))

(ert-deftest fmt-table-all-empty-p-some-filled ()
  "Some-filled cells return nil."
  (should-not (fmt-table--all-empty-p '("" "x" ""))))

(ert-deftest fmt-table-all-empty-p-whitespace-only ()
  "Whitespace-only cells count as empty."
  (should (fmt-table--all-empty-p '("  " " " "   "))))

;; ;;;; fmt-table--markdown-aligns

(ert-deftest fmt-table-markdown-aligns-left ()
  ":--- is left-aligned."
  (should (equal (fmt-table--markdown-aligns '(":---" " --- " "---"))
                 '(left nil nil))))

(ert-deftest fmt-table-markdown-aligns-right ()
  "---: is right-aligned."
  (should (equal (fmt-table--markdown-aligns '("---:" " --- " "---"))
                 '(right nil nil))))

(ert-deftest fmt-table-markdown-aligns-center ()
  ":---: is center-aligned."
  (should (equal (fmt-table--markdown-aligns '(":---:" " --- " "---"))
                 '(center nil nil))))

;; ;;;; fmt-table--merge-continuations

(ert-deftest fmt-table-merge-continuations-simple ()
  "Single-line rows pass through unchanged."
  (should (equal (fmt-table--merge-continuations
                  '(("A" "B") ("C" "D")))
                 '(("A" "B") ("C" "D")))))

(ert-deftest fmt-table-merge-continuations-with-empty-boundaries ()
  "All-empty rows create boundary markers."
  (should (equal (fmt-table--merge-continuations
                  '(("A" "B") ("" "") ("C" "D")))
                 '(("A" "B") boundary ("C" "D")))))

(ert-deftest fmt-table-merge-continuations-merge-continuation ()
  "Partial continuation row merges with previous."
  (should (equal (fmt-table--merge-continuations
                  '(("hello world" "short") ("" "continued")))
                 '(("hello world" "short continued")))))

;; ;;;; fmt-table--wrap-cell

(ert-deftest fmt-table-wrap-cell-fits ()
  "Text that fits width stays on one line."
  (should (equal (fmt-table--wrap-cell "hello" 10)
                 '("hello"))))

(ert-deftest fmt-table-wrap-cell-wraps ()
  "Text wider than width wraps to multiple lines."
  (should (equal (fmt-table--wrap-cell "hello world" 6)
                 '("hello" "world"))))

(ert-deftest fmt-table-wrap-cell-empty ()
  "Empty text returns a list with one empty string."
  (should (equal (fmt-table--wrap-cell "" 10)
                 '(""))))

(ert-deftest fmt-table-wrap-cell-overflow-token ()
  "A token wider than width overflows on its own line."
  (should (equal (fmt-table--wrap-cell "superlongword" 5)
                 '("superlongword"))))

(ert-deftest fmt-table-wrap-cell-code-span ()
  "Backtick code spans stay as single tokens."
  (should (equal (fmt-table--wrap-cell "`code span` rest" 8)
                 '("`code span`" "rest"))))

;; ;;;; fmt-table--cjk-char-p / fmt-table--join-with-space-p

(ert-deftest fmt-table-cjk-char-p-ascii ()
  "ASCII characters are not CJK."
  (should-not (fmt-table--cjk-char-p ?A)))

(ert-deftest fmt-table-cjk-char-p-han ()
  "CJK ideographs are CJK."
  (should (fmt-table--cjk-char-p ?中)))

(ert-deftest fmt-table-join-with-space-p-cjk-boundary ()
  "No space inserted at CJK/Latin boundary."
  (should-not (fmt-table--join-with-space-p "hello" "中"))
  (should-not (fmt-table--join-with-space-p "中" "word")))

(ert-deftest fmt-table-join-with-space-p-latin-boundary ()
  "Space inserted between Latin words."
  (should (fmt-table--join-with-space-p "hello" "world")))

;; ;;;; fmt-table--split-cjk

(ert-deftest fmt-table-split-cjk-mixed ()
  "Mixed CJK/Latin text is split per CJK char."
  (should (equal (fmt-table--split-cjk "ab中cd")
                 '("ab" "中" "cd"))))

(ert-deftest fmt-table-split-cjk-pure-ascii ()
  "Pure ASCII stays as one token."
  (should (equal (fmt-table--split-cjk "hello")
                 '("hello"))))

;; ;;;; fmt-table--tokenize

(ert-deftest fmt-table-tokenize-plain ()
  "Plain text splits on whitespace."
  (should (equal (fmt-table--tokenize "hello world")
                 '("hello" "world"))))

(ert-deftest fmt-table-tokenize-backtick-code ()
  "Backtick code spans stay intact."
  (should (equal (fmt-table--tokenize "use `cmd` here")
                 '("use" "`cmd`" "here"))))

(ert-deftest fmt-table-tokenize-double-backtick ()
  "Double backtick code spans stay intact."
  (should (equal (fmt-table--tokenize "``code here`` end")
                 '("``code here``" "end"))))

;; ;;;; fmt-table--distribute-widths

(ert-deftest fmt-table-distribute-widths-natural-fits ()
  "When natural widths fit, return them unchanged."
  (should (equal (fmt-table--distribute-widths '(5 5) '(3 3) 30 2)
                 '(5 5))))

(ert-deftest fmt-table-distribute-widths-shrink ()
  "When columns must shrink, respect min-widths."
  (let ((result (fmt-table--distribute-widths '(10 10) '(3 3) 20 2)))
    ;; 20 - 7 (border) = 13 content; min 3+3=6 < 13, so each >= 3
    (should (>= (nth 0 result) 3))
    (should (>= (nth 1 result) 3))
    (should (= (apply #'+ result) 13))))

;; ;;;; fmt-table--pad-cell

(ert-deftest fmt-table-pad-cell-left ()
  "Default (nil) alignment pads on the right."
  (should (equal (fmt-table--pad-cell "hi" 5 nil)
                 "hi   ")))

(ert-deftest fmt-table-pad-cell-right ()
  "Right alignment pads on the left."
  (should (equal (fmt-table--pad-cell "hi" 5 'right)
                 "   hi")))

(ert-deftest fmt-table-pad-cell-center ()
  "Center alignment splits padding."
  (should (equal (fmt-table--pad-cell "hi" 6 'center)
                 "  hi  ")))

(ert-deftest fmt-table-pad-cell-overflow ()
  "Cell wider than width overflows without truncation."
  (should (equal (fmt-table--pad-cell "hello" 3 nil)
                 "hello")))

;; ;;;; fmt-table--render-separator

(ert-deftest fmt-table-render-separator-default ()
  "Default alignment renders all dashes."
  (should (string= (fmt-table--render-separator '(5 5) '(nil nil))
                   "| ----- | ----- |")))

(ert-deftest fmt-table-render-separator-aligned ()
  "Aligned columns render colons."
  (should (string= (fmt-table--render-separator '(5 5 5) '(left right center))
                   "| :---- | ----: | :---: |")))

;; ;;;; fmt-table--render-markdown

(ert-deftest fmt-table-render-markdown-simple ()
  "Render a simple 2-col table."
  (let ((output (fmt-table--render-markdown
                 '("H1" "H2") '(nil nil)
                 '(("A" "B") ("C" "D"))
                 80 2)))
    (should (string-match-p "| H1 | H2 |" output))
    (should (string-match-p "| -+ | -+ |" output))
    (should (string-match-p "| A  | B  |" output))
    (should (string-match-p "| C  | D  |" output))))

(ert-deftest fmt-table-render-markdown-boundary-no-multiline ()
  "Boundary markers produce no empty rows when not multi-line."
  (let* ((output (fmt-table--render-markdown
                  '("H1" "H2") '(nil nil)
                  '(("A" "B") boundary ("C" "D"))
                  80 2))
         (lines (split-string output "\n" t)))
    ;; No empty-row separators when not multi-line
    (should-not (string-match-p "|  +" output))))

(ert-deftest fmt-table-render-markdown-boundary-with-multiline ()
  "Boundary markers produce empty rows when multi-line."
  (let* ((output (fmt-table--render-markdown
                  '("H1" "H2") '(nil nil)
                  '(("long content here" "B") boundary ("C" "D"))
                  20 2))
         (lines (split-string output "\n" t)))
    ;; Should have an empty-row separator for multi-line wrapping
    (should (cl-some (lambda (l) (string-match-p "^|  + |  + |$" l)) lines))))

;; ;;;; fmt-table--parse-at-point

(ert-deftest fmt-table-parse-at-point-simple ()
  "Parse a simple markdown table."
  (fmt-table--with-table
   "| H1 | H2 |\n| --- | --- |\n| A | B |\n| C | D |"
   (forward-line 2)
   (pcase-let* ((`(,headers ,aligns ,rows ,_beg ,_end)
                 (fmt-table--parse-at-point)))
     (should (equal headers '("H1" "H2")))
     (should (equal rows '(("A" "B") ("C" "D"))))
     (should (equal aligns '(nil nil))))))

(ert-deftest fmt-table-parse-at-point-aligned ()
  "Parse alignment from separator."
  (fmt-table--with-table
   "| H1 | H2 |\n| :--- | ---: |\n| A | B |"
   (forward-line 2)
   (pcase-let* ((`(,_headers ,aligns ,_rows ,_beg ,_end)
                 (fmt-table--parse-at-point)))
     (should (equal aligns '(left right))))))

(ert-deftest fmt-table-parse-at-point-not-in-table ()
  "Error when not in a table."
  (fmt-table--with-table
   "This is just plain text, no table here."
   (should-error (fmt-table--parse-at-point) :type 'user-error)))

;; ;;;; Round-trip: shrink → expand preserves row count

(ert-deftest fmt-table-round-trip-row-count ()
  "Shrink then expand a table preserves the logical row count."
  (fmt-table--with-table
   "| H1 | H2 | H3 |\n| --- | --- | --- |\n| long content here for wrapping test | short | another cell |\n| row2c1 | row2c2 data | row2c3 |"
   (forward-line 2)
   (let* ((parse1 (fmt-table--parse-at-point))
          (data1 (cl-remove-if (lambda (r) (eq r 'boundary)) (nth 2 parse1)))
          (cnt1 (length data1)))
     (fmt-table-at-point 40)
     (forward-line 2)
     (let* ((parse2 (fmt-table--parse-at-point))
            (data2 (cl-remove-if (lambda (r) (eq r 'boundary)) (nth 2 parse2)))
            (cnt2 (length data2)))
       (should (= cnt1 cnt2))
       (fmt-table-at-point 120)
       (forward-line 2)
       (let* ((parse3 (fmt-table--parse-at-point))
              (data3 (cl-remove-if (lambda (r) (eq r 'boundary)) (nth 2 parse3)))
              (cnt3 (length data3)))
         (should (= cnt1 cnt3)))))))

(ert-deftest fmt-table-round-trip-row-count-cjk ()
  "CJK table round-trip preserves row count."
  (fmt-table--with-table
   "| 字段 | 类型 |\n| --- | --- |\n| 对象ID列表，当un_mark_all为false时必填 | string[] |\n| 分类ID列表 | int[] |"
   (forward-line 2)
   (let* ((parse1 (fmt-table--parse-at-point))
          (data1 (cl-remove-if (lambda (r) (eq r 'boundary)) (nth 2 parse1)))
          (cnt1 (length data1)))
     (fmt-table-at-point 30)
     (forward-line 2)
     (let* ((parse2 (fmt-table--parse-at-point))
            (data2 (cl-remove-if (lambda (r) (eq r 'boundary)) (nth 2 parse2)))
            (cnt2 (length data2)))
       (should (= cnt1 cnt2))))))

;; ;;;; Round-trip: content preserved

(ert-deftest fmt-table-round-trip-content-preserved ()
  "Shrink then expand preserves cell content."
  (fmt-table--with-table
   "| H1 | H2 |\n| --- | --- |\n| hello world | foo bar |"
   (forward-line 2)
   (let ((original-data (nth 2 (fmt-table--parse-at-point))))
     (fmt-table-at-point 25)
     (forward-line 2)
     (fmt-table-at-point 120)
     (forward-line 2)
     (let ((restored-data (nth 2 (fmt-table--parse-at-point))))
       (should (equal original-data restored-data))))))

;; ;;;; fmt-table-at-point integration

(ert-deftest fmt-table-at-point-wraps-narrow ()
  "fmt-table-at-point wraps a table to narrow width."
  (fmt-table--with-table
   "| Header1 | Header2 |\n| --- | --- |\n| long content here | short |"
   (forward-line 2)
   (fmt-table-at-point 20)
   ;; Should have more lines after wrapping
   (should (> (count-lines (point-min) (point-max)) 4))))

(ert-deftest fmt-table-at-point-keeps-separator ()
  "Wrapped table still has a separator row."
  (fmt-table--with-table
   "| H1 | H2 |\n| --- | --- |\n| A | B |"
   (forward-line 2)
   (fmt-table-at-point 40)
   (goto-char (point-min))
   (should (re-search-forward "^|[-: |]+" nil t))))

;; ;;;; fmt-table--column-min-widths

(ert-deftest fmt-table-column-min-widths-basic ()
  "Min-width respects longest unbreakable token."
  (let ((result (fmt-table--column-min-widths '("H1" "H2")
                                               '(("longword" "x") ("y" "z"))
                                               2)))
    (should (>= (nth 0 result) 8))  ; "longword" is 8 chars
    (should (>= (nth 1 result) 1))))

;; ;;;; fmt-table--logical-row-at-point

(ert-deftest fmt-table-logical-row-at-point-simple ()
  "Logical row for a single-line table."
  (fmt-table--with-table
   "| H1 | H2 |\n| --- | --- |\n| A | B |\n| C | D |"
   (forward-line 2)
   (should (= (fmt-table--logical-row-at-point) 0))
   (forward-line 1)
   (should (= (fmt-table--logical-row-at-point) 1))))

(ert-deftest fmt-table-logical-row-at-point-wrapped ()
  "Logical row in a wrapped table counts continuation rows."
  (fmt-table--with-table
   "| H1 | H2 |\n| --- | --- |\n| long content here | B |\n|  | continued |\n| C | D |"
   (forward-line 3)
   (should (= (fmt-table--logical-row-at-point) 0))
   (forward-line 2)
   (should (= (fmt-table--logical-row-at-point) 1))))

;; ;;;; fmt-table--col-at-point

(ert-deftest fmt-table-col-at-point ()
  "Column index at point."
  (fmt-table--with-table
   "| H1 | H2 | H3 |\n| --- | --- | --- |\n| A | B | C |"
   (forward-line 2)
   (forward-char 2)
   (should (= (fmt-table--col-at-point) 0))
   (search-forward "B")
   (backward-char 1)
   (should (= (fmt-table--col-at-point) 1))))

;; ;;;; fmt-table-at-point preserves no-boundary round-trip

(ert-deftest fmt-table-expand-drops-boundary-empty-rows ()
  "Expanding a wrapped table drops boundary empty-rows (no spurious data)."
  (fmt-table--with-table
   "| H1 | H2 |\n| --- | --- |\n| A | B |\n| C | D |"
   (forward-line 2)
   ;; Shrink to force multi-line, then expand
   (fmt-table-at-point 15)
   (forward-line 2)
   (fmt-table-at-point 120)
   (forward-line 2)
   (let* ((parse (fmt-table--parse-at-point))
          (data (cl-remove-if (lambda (r) (eq r 'boundary)) (nth 2 parse))))
     ;; 2 data rows, not 3 or 4
     (should (= (length data) 2)))))

(provide 'fmt-table-test)
;;; fmt-table-test.el ends here
