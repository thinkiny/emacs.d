;;; fmt-table.el --- Word-wrap markdown pipe tables to fit width  -*- lexical-binding: t; -*-

;;; Commentary:
;; Self-contained table word-wrapping for markdown-mode.
;; No external dependencies.  Entry point: `fmt-table-at-point'.

;;; Code:

(require 'cl-lib)
(require 'markdown-mode)

;;;; Cell splitting

(defun fmt-table--parse-cells (line)
  "Split pipe-table LINE into a list of trimmed cell strings.
Handle escaped \\| in markdown tables by protecting before split."
  (let* ((trimmed (string-trim line))
         (escaped (string-search "\\" trimmed))
         (body (if escaped
                   (replace-regexp-in-string "\\\\|" "\x00PIPE\x00" trimmed)
                 trimmed)))
    ;; Strip leading/trailing pipes.
    (when (string-prefix-p "|" body)
      (setq body (substring body 1)))
    (when (string-suffix-p "|" body)
      (setq body (substring body 0 -1)))
    (mapcar (lambda (cell)
              (string-trim (if escaped
                               (replace-regexp-in-string "\x00PIPE\x00" "\\|" cell t t)
                             cell)))
            (split-string body "|"))))

;;;; Row classification

(defun fmt-table--hline-p (line)
  "Return non-nil if LINE is a markdown separator row."
  (string-match-p "^|[-:|[:space:]]+|$" (string-trim line)))

(defun fmt-table--all-empty-p (cells)
  "Return non-nil if every cell in CELLS is empty after trimming."
  (cl-every (lambda (c) (string-empty-p (string-trim c))) cells))

;;;; Alignment extraction

(defun fmt-table--markdown-aligns (sep-cells)
  "Extract alignment list from markdown separator cells.
\\='left, \\='right, \\='center, or nil."
  (mapcar (lambda (cell)
            (let ((c (string-trim cell)))
              (cond ((and (string-prefix-p ":" c) (string-suffix-p ":" c)) 'center)
                    ((string-suffix-p ":" c) 'right)
                    ((string-prefix-p ":" c) 'left)
                    (t nil))))
          sep-cells))

;;;; Continuation row merging

(defun fmt-table--split-segments (visual-rows)
  "Split VISUAL-ROWS on all-empty or nil rows.
Return a list of segments (lists of visual rows)."
  (let ((segments nil)
        (cur-seg nil))
    (dolist (vr visual-rows)
      (if (or (null vr) (fmt-table--all-empty-p vr))
          (progn (when cur-seg (push (nreverse cur-seg) segments))
                 (setq cur-seg nil))
        (push vr cur-seg)))
    (when cur-seg (push (nreverse cur-seg) segments))
    (nreverse segments)))

(defun fmt-table--continuation-p (prev-nonempty prev-all-full nonempty-idxs all-full)
  "Return non-nil if a visual row continues the previous logical row.
NONEMPTY-IDXS are the current row's non-empty column indices; ALL-FULL
is non-nil when every column is filled.  PREV-NONEMPTY (a list of indices
or nil) and PREV-ALL-FULL describe the previous visual row.  A row
continues the previous one when its filled cells are a subset of the
previous row's and the two rows are not both full."
  (and prev-nonempty
       (cl-every (lambda (i) (memq i prev-nonempty)) nonempty-idxs)
       (not (and prev-all-full all-full))))

(defun fmt-table--merge-segment (seg)
  "Merge continuation visual rows in SEG into logical rows.
A visual row is a continuation if its non-empty cells are a subset
of the previous row's non-empty cells (and the fullness didn't
change between all-full).

Note: this is heuristic.  A sparse foreign table whose rows have empty
trailing cells and no blank separator between them can be mis-merged.
The renderer avoids this by emitting a blank separator between every
logical row, so the tool's own output always round-trips correctly."
  (let* ((num-cols (length (car seg)))
         (logical-rows nil)
         (merged (make-vector num-cols nil))
         (prev-nonempty nil)
         (prev-all-full nil))
    (dolist (vr seg)
      (let* ((nonempty-idxs (cl-loop for c in vr for i from 0
                                      when (not (string-empty-p (string-trim c)))
                                      collect i))
             (all-full (= (length nonempty-idxs) num-cols))
             (is-continuation (fmt-table--continuation-p
                               prev-nonempty prev-all-full nonempty-idxs all-full)))
        (unless is-continuation
          (when prev-nonempty
            (push (cl-loop for i below num-cols collect (or (aref merged i) ""))
                  logical-rows)
            (setq merged (make-vector num-cols nil))))
        (cl-loop for c in vr for i from 0 do
          (let ((trimmed (string-trim c)))
            (unless (string-empty-p trimmed)
              (let* ((prev (aref merged i))
                     (need-space (and prev (fmt-table--join-with-space-p prev trimmed))))
                (aset merged i
                      (if prev
                          (concat prev (if need-space " " "") trimmed)
                        trimmed))))))
        (setq prev-nonempty nonempty-idxs
              prev-all-full all-full)))
    (when prev-nonempty
      (push (cl-loop for i below num-cols collect (or (aref merged i) ""))
            logical-rows))
    (nreverse logical-rows)))

(defun fmt-table--merge-continuations (visual-rows)
  "Merge continuation visual-rows in VISUAL-ROWS into logical rows.
All-empty rows are explicit row boundaries.  Return a list of logical
rows interspersed with the symbol \\='boundary where an all-empty row
occurred in the input."
  (let ((result nil)
        (first t))
    (dolist (seg (fmt-table--split-segments visual-rows))
      (unless first (push 'boundary result))
      (setq first nil)
      (dolist (r (fmt-table--merge-segment seg))
        (push r result)))
    (nreverse result)))

;;;; Table parsing

(defun fmt-table--parse-at-point ()
  "Parse the markdown table at point.
Return (HEADERS ALIGNS ROWS BEG END) where HEADERS and ROWS
are lists of cell-string lists, ALIGNS is a list of alignment
symbols, and BEG/END are buffer positions."
  (unless (markdown-table-at-point-p)
    (user-error "Not at a markdown table"))
  (let* ((beg (markdown-table-begin))
         (end (max beg (1- (markdown-table-end))))
         (text (buffer-substring-no-properties beg end))
         (lines (split-string text "\n" t))
         (header-rows nil)
         (sep-cells nil)
         (data-rows nil)
         (found-sep nil))
    (dolist (line lines)
      (let ((trimmed (string-trim line)))
        (cond
         ((string-empty-p trimmed) nil)
         ((and (not found-sep) (fmt-table--hline-p line))
          (setq found-sep t)
          (setq sep-cells (fmt-table--parse-cells trimmed)))
         ((not found-sep)
          (push (fmt-table--parse-cells trimmed) header-rows))
         (t
          (push (fmt-table--parse-cells trimmed) data-rows)))))
    (setq header-rows (nreverse header-rows))
    (setq data-rows (nreverse data-rows))
    (let* ((headers (car (fmt-table--merge-continuations header-rows)))
           (num-cols (length headers))
           (rows (fmt-table--merge-continuations data-rows))
           (aligns (if sep-cells
                       (fmt-table--markdown-aligns sep-cells)
                     (make-list num-cols nil))))
      (list headers aligns rows beg end))))

;;;; Cell word-wrapping

(defun fmt-table--cjk-char-p (ch)
  "Return non-nil if character CH is a CJK ideograph or CJK punctuation."
  (or (and (>= ch #x3000) (<= ch #x30FF))   ; CJK symbols/punct, hiragana, katakana
      (and (>= ch #x3400) (<= ch #x9FFF))   ; CJK ideographs (ext A + unified)
      (and (>= ch #xF900) (<= ch #xFAFF))   ; CJK compat ideographs
      (and (>= ch #xFF00) (<= ch #xFFEF))   ; fullwidth forms
      (>= ch #x20000)))                      ; CJK ext B+

(defun fmt-table--split-cjk (token)
  "Split TOKEN into sub-tokens, one per CJK character.
Non-CJK runs stay together so ASCII identifiers and code are not broken."
  (let ((result nil)
        (latin-acc nil))
    (cl-loop for c across token do
      (if (fmt-table--cjk-char-p c)
          (progn
            (when latin-acc
              (push (apply #'string (nreverse latin-acc)) result)
              (setq latin-acc nil))
            (push (char-to-string c) result))
        (push c latin-acc)))
    (when latin-acc
      (push (apply #'string (nreverse latin-acc)) result))
    (nreverse result)))

(defun fmt-table--join-with-space-p (prev next)
  "Return non-nil if a space should separate PREV and NEXT when merging.
A space is inserted only when both fragments meet at non-CJK characters;
CJK text is not space-separated, so CJK boundaries join directly.  This
keeps a shrink/expand round-trip from inserting spurious spaces."
  (let ((pl (length prev)) (nl (length next)))
    (and (> pl 0) (> nl 0)
         (not (fmt-table--cjk-char-p (aref prev (1- pl))))
         (not (fmt-table--cjk-char-p (aref next 0))))))

(defun fmt-table--tokenize (text)
  "Split TEXT into tokens, keeping backtick code spans intact.
Plain text is split on whitespace.  `code` and `` code `` spans
are kept as single tokens.  Return a list of strings."
  (let ((tokens nil)
        (pos 0)
        (len (length text)))
    (while (< pos len)
      ;; Skip leading whitespace.
      (while (and (< pos len) (memq (aref text pos) '(?\s ?\t)))
        (setq pos (1+ pos)))
      (when (< pos len)
        (cond
         ;; Double backtick: `` content ``
         ((and (< (1+ pos) len)
               (= (aref text pos) ?`)
               (= (aref text (1+ pos)) ?`))
          (let ((end (string-match "``" text (+ pos 2))))
            (if end
                (progn (push (substring text pos (+ end 2)) tokens)
                       (setq pos (+ end 2)))
              (push (substring text pos) tokens)
              (setq pos len))))
         ;; Single backtick: `content`
         ((= (aref text pos) ?`)
          (let ((end (string-match "`" text (1+ pos))))
            (if end
                (progn (push (substring text pos (1+ end)) tokens)
                       (setq pos (1+ end)))
              (push (substring text pos) tokens)
              (setq pos len))))
         ;; Plain text — up to whitespace or backtick (CJK split per char)
         (t
          (let ((end pos))
            (while (and (< end len)
                        (not (memq (aref text end) '(?\s ?\t ?`))))
              (setq end (1+ end)))
            (dolist (s (fmt-table--split-cjk (substring text pos end)))
              (push s tokens))
            (setq pos end))))))
    (nreverse tokens)))

(defun fmt-table--wrap-cell (text width)
  "Word-wrap TEXT to fit within WIDTH columns.
Split on whitespace, keeping `code` spans intact.  A token wider than
WIDTH is never broken — it overflows on its own line so it survives a
shrink/expand round-trip intact.  Return a list of line strings."
  (if (or (<= width 0) (string-empty-p text))
      (list (or text ""))
    (let ((tokens (fmt-table--tokenize text))
          (lines nil)
          (line "")
          (line-len 0))
      (dolist (token tokens)
        (let* ((tok-width (string-width token))
               (sep-w (if (string= line "") 0
                        (if (fmt-table--join-with-space-p line token) 1 0))))
          (cond
           ;; Token wider than the column — overflow on its own line
           ((> tok-width width)
            (when (> line-len 0)
              (push line lines))
            (push token lines)
            (setq line "" line-len 0))
           ;; Fits on the current line
           ((or (= line-len 0)
                (<= (+ line-len sep-w tok-width) width))
            (setq line (if (= line-len 0)
                           token
                         (concat line (make-string sep-w ?\s) token)))
            (setq line-len (+ line-len sep-w tok-width)))
           ;; Doesn't fit — wrap to a new line
           (t
            (push line lines)
            (setq line token line-len tok-width)))))
      (when (> line-len 0)
        (push line lines))
      (or (nreverse lines) (list "")))))

;;;; Width distribution

(defun fmt-table--column-min-widths (headers rows num-cols)
  "Return per-column longest-token widths (minimum 1).
HEADERS is a list of header cells; ROWS is a list of cell lists.
Used as the shrink floor so a short column is never broken below its
longest unbreakable word while a wide neighbor absorbs the wrapping."
  (let ((mins (make-vector num-cols 1)))
    (dolist (row (cons headers rows))
      (cl-loop for cell in row for i from 0 while (< i num-cols) do
        (let ((best 1))
          (dolist (tok (fmt-table--tokenize (or cell "")))
            (setq best (max best (string-width tok))))
          (aset mins i (max (aref mins i) best)))))
    (append mins nil)))

(defun fmt-table--give-remainder (widths natural-widths content-space num-cols)
  "Hand leftover space in WIDTHS to columns with most NATURAL-WIDTHS.
Distributes CONTENT-SPACE minus the sum of WIDTHS one unit at a time
to the column with the highest natural width that hasn't reached it yet."
  (let ((leftover (- content-space (apply #'+ widths))))
    (dotimes (_ (max 0 leftover))
      (let ((widest-col -1) (widest-nat -1))
        (dotimes (i num-cols)
          (when (and (< (nth i widths) (nth i natural-widths))
                     (>= (nth i natural-widths) widest-nat))
            (setq widest-col i widest-nat (nth i natural-widths))))
        (when (>= widest-col 0)
          (setf (nth widest-col widths) (1+ (nth widest-col widths))))))
    widths))

(defun fmt-table--distribute-widths (natural-widths min-widths available num-cols)
  "Distribute AVAILABLE total width across NUM-COLS columns.
NATURAL-WIDTHS is the max single-line content width per column.
MIN-WIDTHS is the longest unbreakable token per column; a column is
never shrunk below this unless the tokens themselves cannot fit.
Return a list of column content widths."
  (let* ((border-overhead (+ (* 3 num-cols) 1)) ; | cell | cell | = 3 per col + 1
         (content-space (max num-cols (- available border-overhead)))
         (total-natural (apply #'+ natural-widths))
         (total-min (apply #'+ min-widths)))
    (cond
     ;; Everything fits at natural width — keep it compact.
     ((<= (+ total-natural border-overhead) available)
      (copy-sequence natural-widths))
     ;; Tokens fit individually — floor each at its longest token and
     ;; give the leftover to columns that still have room to grow.
     ((<= total-min content-space)
      (let* ((widths (copy-sequence min-widths))
             (slack (cl-loop for n in natural-widths for m in min-widths
                             sum (max 0 (- n m)))))
        (when (> slack 0)
          (let ((extra (- content-space total-min)))
            (cl-loop for i from 0 below num-cols do
              (let ((room (- (nth i natural-widths) (nth i min-widths))))
                (when (> room 0)
                  (setf (nth i widths)
                        (+ (nth i min-widths)
                           (floor (* extra (/ (float room) slack))))))))))
        (fmt-table--give-remainder widths natural-widths content-space num-cols)))
     ;; Not enough room for every token: water-fill so columns with the
     ;; smallest token widths are satisfied first.
     (t
      (let ((widths (make-vector num-cols 0))
            (order (sort (number-sequence 0 (1- num-cols))
                         (lambda (a b) (< (nth a min-widths)
                                          (nth b min-widths)))))
            (remaining content-space)
            (count num-cols))
        (dolist (i order)
          (let* ((share (/ (float remaining) count))
                 (min-w (nth i min-widths))
                 (alloc (if (<= min-w share) min-w (max 1 (floor share)))))
            (aset widths i alloc)
            (setq remaining (- remaining alloc))
            (setq count (1- count))))
        (fmt-table--give-remainder (append widths nil) natural-widths content-space num-cols))))))

;;;; Cell padding

(defun fmt-table--pad-cell (cell width align)
  "Pad CELL string to WIDTH using ALIGN (\\='left, \\='right, \\='center, or nil).
If the cell content is wider than WIDTH, it overflows rather than being
truncated — this preserves data on shrink/expand round-trips."
  (let* ((vis (string-width cell))
         (pad (max 0 (- width vis))))
    (pcase align
      ('right  (concat (make-string pad ?\s) cell))
      ('center (let ((l (/ pad 2)))
                 (concat (make-string l ?\s) cell (make-string (- pad l) ?\s))))
      (_       (concat cell (make-string pad ?\s))))))

;;;; Rendering

(defun fmt-table--render-separator (col-widths aligns)
  "Render markdown separator line for COL-WIDTHS and ALIGNS."
  (concat "| "
          (mapconcat #'identity
                     (cl-mapcar
                      (lambda (w align)
                        (let ((dashes (make-string (max 1 w) ?-)))
                          (pcase align
                            ('left    (if (>= w 2) (concat ":" (substring dashes 1)) ":"))
                            ('right   (if (>= w 2) (concat (substring dashes 1) ":") ":"))
                            ('center  (if (>= w 3) (concat ":" (substring dashes 2) ":")
                                         (if (>= w 2) "::" ":")))
                            (_ dashes))))
                      col-widths aligns)
                     " | ")
          " |"))

(defun fmt-table--render-row (cells col-widths aligns num-cols)
  "Wrap and pad CELLS, return list of pipe-delimited line strings.
NUM-COLS is used to pad short rows with empty cells."
  (let* ((padded (append cells (make-list (max 0 (- num-cols (length cells))) "")))
         (wrapped (cl-mapcar #'fmt-table--wrap-cell padded col-widths))
         (max-h (apply #'max (mapcar #'length wrapped))))
    (cl-loop for li below max-h collect
             (concat "| "
                     (mapconcat #'identity
                                 (cl-mapcar (lambda (lines w a)
                                              (fmt-table--pad-cell (or (nth li lines) "") w a))
                                            wrapped col-widths aligns)
                                 " | ")
                     " |"))))

(defun fmt-table--render-markdown (headers aligns rows width num-cols)
  "Render a markdown table with multi-line cell wrapping.
The symbol \\='boundary in ROWS is rendered as an empty-row separator."
  (let* ((data-rows (cl-remove-if (lambda (r) (eq r 'boundary)) rows))
         (natural-widths
          (let ((ws (make-vector num-cols 0)))
            (cl-loop for c in headers for i from 0 do
              (aset ws i (max (aref ws i) (string-width c))))
            (dolist (row data-rows)
              (cl-loop for c in row for i from 0 do
                (aset ws i (max (aref ws i) (string-width c)))))
            (append ws nil)))
         (min-widths (fmt-table--column-min-widths headers data-rows num-cols))
         (col-widths (fmt-table--distribute-widths natural-widths min-widths width num-cols))
         (rendered-data (mapcar (lambda (row)
                                  (fmt-table--render-row row col-widths aligns num-cols))
                                data-rows))
         (header-rendered (fmt-table--render-row headers col-widths aligns num-cols))
         (sep (fmt-table--render-separator col-widths aligns))
         (multi-line (cl-some (lambda (lines) (> (length lines) 1))
                              rendered-data))
         (empty-row (concat "| "
                            (mapconcat (lambda (w) (make-string w ?\s)) col-widths " | ")
                            " |")))
    ;; Assemble output — walk original ROWS to place boundaries correctly
    (mapconcat
     #'identity
     (append
      header-rendered
      (list sep)
      (let ((data-idx 0)
            (result nil)
            (first-data t))
        (dolist (r rows)
          (cond
           ((eq r 'boundary) nil)    ; skip — separation handled below
           (t
            (when (and multi-line (not first-data))
              (push empty-row result))
            (setq first-data nil)
            (dolist (line (nth data-idx rendered-data))
              (push line result))
            (setq data-idx (1+ data-idx)))))
        (nreverse result)))
     "\n")))

;;;; Edit field

(defvar-local fmt-table--edit-marker nil
  "Marker pointing to the cursor position before editing a table cell.")
(defvar-local fmt-table--edit-window-config nil
  "Window configuration saved before editing a table field.")
(defvar-local fmt-table--edit-state nil
  "Plist for current edit-field session.
Keys: :parse (headers aligns rows beg end), :row-idx, :col-idx, :width.")

(defsubst fmt-table--col-at-point ()
  "Return 0-based column index of point within current table row."
  (1- (markdown-table-get-column)))

(defun fmt-table--logical-row-at-point ()
  "Return 0-based logical row index of point in the table at point.
Handles continuation rows (multi-line cells): visual lines belonging
to the same logical row are counted together."
  (save-excursion
    (let ((target-line (line-number-at-pos))
          (row-idx -1)
          (found-sep nil)
          (prev-nonempty nil)
          (prev-all-full nil))
      (goto-char (markdown-table-begin))
      (catch 'done
        (while t
          (let ((trimmed (string-trim
                          (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))))
            (cond
             ((not found-sep)
              (when (fmt-table--hline-p trimmed) (setq found-sep t)))
             ((string-empty-p trimmed))
             ((fmt-table--hline-p trimmed))
             (found-sep
              (let* ((cells (fmt-table--parse-cells trimmed))
                     (nonempty-idxs (cl-loop for c in cells for i from 0
                                             when (not (string-empty-p (string-trim c)))
                                             collect i))
                     (num-cols (length cells))
                     (all-full (= (length nonempty-idxs) num-cols)))
                (unless (fmt-table--all-empty-p cells)
                  (let ((is-continuation (fmt-table--continuation-p
                                          prev-nonempty prev-all-full nonempty-idxs all-full)))
                    (unless is-continuation
                      (setq row-idx (1+ row-idx))))
                (setq prev-nonempty nonempty-idxs
                      prev-all-full all-full))))))
          (when (>= (line-number-at-pos) target-line)
            (throw 'done nil))
          (forward-line 1)))
      (max 0 row-idx))))

(defvar fmt-table--edit-field-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'fmt-table-finish-edit-field)
    (define-key map (kbd "C-c C-k") #'fmt-table-abort-edit-field)
    map)
  "Keymap for `*Fmt Table Edit Field*' buffer.")

;;;###autoload
(defun fmt-table-edit-field ()
  "Edit current markdown table cell in a separate window.
Finish with C-c C-c to commit (the table re-wraps automatically).
Abort with C-c C-k."
  (interactive)
  (unless (markdown-table-at-point-p)
    (user-error "Not at a markdown table"))
  (let* ((parse (fmt-table--parse-at-point))
         (data-rows (cl-remove-if (lambda (r) (eq r 'boundary)) (nth 2 parse)))
         (row-idx (fmt-table--logical-row-at-point))
         (col-idx (fmt-table--col-at-point))
         (num-cols (length (nth 0 parse)))
         (cell-text (if (and (>= row-idx 0) (< row-idx (length data-rows))
                             (>= col-idx 0) (< col-idx num-cols))
                        (nth col-idx (nth row-idx data-rows))
                      ""))
         (pos (point-marker))
         (wc (current-window-configuration)))
    (pop-to-buffer "*Fmt Table Edit Field*")
    (erase-buffer)
    (insert "# Edit cell and finish with C-c C-c\n#\n")
    (insert cell-text)
    (goto-char (point-max))
    (use-local-map fmt-table--edit-field-mode-map)
    (setq-local fmt-table--edit-marker pos)
    (setq-local fmt-table--edit-window-config wc)
    (setq-local fmt-table--edit-state
                (list :parse parse
                      :row-idx row-idx
                      :col-idx col-idx
                      :width (floor (* (window-width) 0.9))))
    (message "Edit and finish with C-c C-c")))

(defun fmt-table-abort-edit-field ()
  "Abort editing a table field, restoring the previous window."
  (interactive)
  (let* ((wc fmt-table--edit-window-config)
         (pos fmt-table--edit-marker)
         (cb (current-buffer)))
    (set-window-configuration wc)
    (kill-buffer cb)
    (when pos
      (goto-char pos)
      (move-marker pos nil))
    (message "Edit aborted")))

(defun fmt-table-finish-edit-field ()
  "Finish editing a table field.
Strip comment lines, join newlines, replace the cell, and re-wrap."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^#.*\n?" nil t) (replace-match ""))
  (while (re-search-forward "[ \t]*\n[ \t\n]*" nil t) (replace-match " "))
  (let* ((text (string-trim (buffer-string)))
         (pos fmt-table--edit-marker)
         (wc fmt-table--edit-window-config)
         (state fmt-table--edit-state)
         (parse (plist-get state :parse))
         (headers (nth 0 parse))
         (aligns (nth 1 parse))
         (rows (nth 2 parse))
         (beg (nth 3 parse))
         (end (nth 4 parse))
         (row-idx (plist-get state :row-idx))
         (col-idx (plist-get state :col-idx))
         (width (plist-get state :width))
         (cb (current-buffer))
         (source-buffer (marker-buffer pos)))
    ;; Modify the cell in the parsed rows
    (let ((data-idx 0))
      (dolist (r rows)
        (unless (eq r 'boundary)
          (when (= data-idx row-idx)
            (setcar (nthcdr col-idx r) text))
          (setq data-idx (1+ data-idx)))))
    ;; Kill edit buffer and restore window before touching source
    (set-window-configuration wc)
    (kill-buffer cb)
    ;; Now work in the source buffer
    (with-current-buffer source-buffer
      (let* ((num-cols (length headers))
             (rendered (fmt-table--render-markdown headers aligns rows width num-cols)))
        (delete-region beg end)
        (goto-char beg)
        (insert rendered)))
    (when pos (move-marker pos nil))
    (message "Field updated")))

;;;; Main entry point

;;;###autoload
(defun fmt-table-at-point (&optional width)
  "Wrap the markdown table at point to fit WIDTH columns.
Defaults to 90% of the current window width."
  (interactive
   (list (read-number "Wrap table width: "
            (floor (* (window-width) 0.9)))))
  (pcase-let* ((`(,headers ,aligns ,rows ,beg ,end)
                (fmt-table--parse-at-point))
               (num-cols (length headers))
               (rendered (fmt-table--render-markdown headers aligns rows
                            (or width (floor (* (window-width) 0.9)))
                            num-cols)))
    (delete-region beg end)
    (goto-char beg)
    (insert rendered)))

(provide 'fmt-table)
;;; fmt-table.el ends here
