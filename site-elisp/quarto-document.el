;;; quarto-document.el --- Shared Quarto source model -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'awc-project)

(defgroup quarto nil "Editing Quarto documents." :group 'text)
(cl-defstruct quarto-chunk id marker start head-end body-end end language label
              prefix fence options closed executable label-definitions)
(cl-defstruct quarto-document revision chunks headings labels references front-matter diagnostics targets)
(defvar-local quarto-document--model nil)
(defvar-local quarto-document--serial 0)
(defvar-local quarto-document--owner nil)
(defvar quarto-document-change-hook nil
  "Functions called with SOURCE and earliest changed position after an edit.")

(defun quarto-source-buffer ()
  "Return the document owning this source, indirect buffer or results pane."
  (or quarto-document--owner (buffer-base-buffer) (current-buffer)))

(defun quarto-source-p ()
  "Whether this buffer belongs to a Quarto source."
  (with-current-buffer (quarto-source-buffer)
    (and buffer-file-name (string-match-p "\\.qmd\\'" buffer-file-name))))

(defun quarto-document--prefix (line)
  "Return the Markdown container prefix on LINE."
  (when (string-match "\\`[ \t]*\\(?:>[ \t]*\\)*\\(?:[-+*][ \t]+\\|[0-9]+[.)][ \t]+\\)?" line)
    (match-string 0 line)))

(defun quarto-document--option (text)
  "Read a simple YAML scalar TEXT; structured metadata comes from Quarto."
  (setq text (string-trim text))
  (when (string-match "\\`\\([^\"'#]*?\\)[ \t]+#.*\\'" text)
    (setq text (string-trim-right (match-string 1 text))))
  (cond ((string-match "\\`[\"']\\(.*\\)[\"']\\(?:[ \t]+#.*\\)?\\'" text) (match-string 1 text))
        ((member text '("false" "FALSE")) :false)
        ((member text '("true" "TRUE")) t)
        (t text)))

(defun quarto-document--mask-inline-code (line)
  "Mask inline code in LINE, retaining source character positions."
  (let ((result (copy-sequence line)) (offset 0))
    (while (string-match "`+" line offset)
      (let* ((start (match-beginning 0)) (fence (match-string 0 line))
             (end (match-end 0)) (cursor end) found)
        (while (and (not found) (string-match "`+" line cursor))
          (setq cursor (match-end 0))
          (when (equal fence (match-string 0 line)) (setq found cursor)))
        (when found
          (setq result (concat (substring result 0 start) (make-string (- found start) ?\s)
                               (substring result found))))
        (setq offset (or found end))))
    result))

(defun quarto-document--references (text start)
  "Locate reference keys in TEXT at source START, excluding inline code."
  (let ((line (quarto-document--mask-inline-code text)) (offset 0) references)
    ;; Link destinations and URLs are literal text even if they contain @.
    (while (string-match "\\]([^)\n]*)\\|https?://[^ \t\n<>]+\\|<[^>\n]*>" line offset)
      (let ((begin (match-beginning 0)) (end (match-end 0)))
        (setq line (concat (substring line 0 begin) (make-string (- end begin) ?\s) (substring line end))
              offset end)))
    (setq offset 0)
    (while (string-match "\\(?:\\`\\|[^[:alnum:]\\\\]\\)@\\([[:alnum:]_][[:alnum:]_:.+-]*\\|\\)" line offset)
      (let ((key (match-string 1 line)) (position (+ start (match-beginning 1))) (next (match-end 0)))
        (push (cons (replace-regexp-in-string "[.:]+\\'" "" key) position) references)
        (setq offset next)))
    references))

(defun quarto-document--parse ()
  "Index this widened source once; never execute code or start a process."
  (let ((old (and quarto-document--model (quarto-document-chunks quarto-document--model)))
        (old-positions (make-hash-table :test #'eql))
        chunks headings labels references diagnostics targets front open comment-open caption-key caption-indent)
    (dolist (chunk old) (puthash (marker-position (quarto-chunk-marker chunk)) chunk old-positions))
    (goto-char (point-min))
    (when (looking-at "---[ \t]*$")
      (let ((start (point)))
        (forward-line 1)
        (if (re-search-forward "^\\(?:---\\|\\.\\.\\.\\)[ \t]*$" nil t)
            (progn (forward-line 1) (setq front (cons start (point))))
          (push (list start 'error "Unclosed YAML front matter") diagnostics)
          (setq front (cons start (point-max))) (goto-char (point-max)))))
    (while (< (point) (point-max))
      (let* ((start (point)) (line (buffer-substring-no-properties start (line-end-position)))
             (prefix (quarto-document--prefix line))
             (text (substring line (length prefix)))
             (end (progn (forward-line 1) (point))))
        (unless open
          ;; Mask comments without moving source positions.  Literal examples
          ;; must never become destinations or edits in a label rename.
          (let ((offset 0) begin finish)
            (while (and (< offset (length line))
                        (setq begin (if comment-open offset (string-match "<!--" line offset))))
              (setq finish (string-match "-->" line (if comment-open begin (+ begin 4)))
                    comment-open (not finish)
                    finish (if finish (+ finish 3) (length line)))
              (setq line (concat (substring line 0 begin) (make-string (- finish begin) ?\s)
                                 (substring line finish)))
              (setq offset finish)))
          (setq text (substring line (length prefix))))
        (cond
         (open
          (let ((fence (quarto-chunk-fence open)))
            (if (string-match-p
                 (format "\\`%s\\{%d,\\}[ \t]*\\'" (regexp-quote (substring fence 0 1)) (length fence)) text)
                (progn
                  (setf (quarto-chunk-body-end open) start (quarto-chunk-end open) end
                        (quarto-chunk-closed open) t)
                  (push open chunks) (setq open nil caption-key nil))
              (if (and caption-key (string-match "\\`#|\\([ \t]+\\)\\(.*\\)" text)
                       (> (length (match-string 1 text)) caption-indent))
                  (let ((content (match-string 2 text))
                        (position (+ start (length prefix) (match-beginning 2))))
                    (setcdr (assoc caption-key (quarto-chunk-options open))
                            (string-trim (concat (cdr (assoc caption-key (quarto-chunk-options open))) " " content)))
                    (setq references (append (quarto-document--references content position) references)))
                (setq caption-key nil)
                (when (and (quarto-chunk-executable open)
                           (string-match "\\`#|[ \t]*\\([^: \t]+\\):[ \t]*\\(.*\\)" text))
                  (let* ((key (match-string 1 text)) (raw (match-string 2 text))
                         (position (+ start (length prefix) (match-beginning 2)))
                         (value (quarto-document--option raw)))
                    (push (cons key value) (quarto-chunk-options open))
                    (when (member key '("fig-cap" "tbl-cap"))
                      (if (member value '("|" ">" "|-" ">-" "|+" ">+"))
                          (progn
                            (setq caption-key key
                                  caption-indent (progn (string-match "\\`#|\\([ \t]*\\)" text)
                                                        (length (match-string 1 text))))
                            (setcdr (assoc key (quarto-chunk-options open)) ""))
                        (when (and (stringp value) (string-match (regexp-quote value) raw))
                          (setq references (append (quarto-document--references value (+ position (match-beginning 0))) references)))))
                    (when (equal key "label")
                      (setf (quarto-chunk-label open) value)
                      (when (and (stringp value)
                                 (string-match (concat "\\`[ \t]*[\"']?\\(" (regexp-quote value) "\\)") raw))
                        (push (list :key value :start (+ position (match-beginning 1))
                                    :end (+ position (match-end 1)))
                              (quarto-chunk-label-definitions open))))))))))
         ((string-match "\\`\\(`\\{3,\\}\\|~\\{3,\\}\\)\\(.*\\)\\'" text)
          (let* ((fence (match-string 1 text)) (info (string-trim (match-string 2 text)))
                 (executable (string-match "\\`{\\([[:alnum:]_+-]+\\)\\(?:[ ,]\\([^}]*\\)\\)?}\\'" info))
                 (language (and executable (downcase (match-string 1 info))))
                 (extra (and executable (match-string 2 info)))
                 (label (and extra (car (split-string extra "[, \t]+" t))))
                 (previous (gethash start old-positions)))
            (when (and label (string-match-p "=" label)) (setq label nil))
            (setq open (make-quarto-chunk
                        :id (if previous (quarto-chunk-id previous) (cl-incf quarto-document--serial))
                        :marker (if previous (quarto-chunk-marker previous) (copy-marker start t))
                        :start start :head-end end :body-end (point-max) :end (point-max)
                        :prefix (replace-regexp-in-string
                                 "\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\'"
                                 (lambda (match) (make-string (length match) ?\s)) prefix)
                        :fence fence :language language :label label
                        :executable (and executable (member language '("r" "python")))))
            (when (and label
                       (string-match (concat "\\`" (regexp-quote fence)
                                             "[ \t]*{[[:alnum:]_+-]+[ ,]+\\(" (regexp-quote label) "\\)") text))
              (push (list :key label :start (+ start (length prefix) (match-beginning 1))
                          :end (+ start (length prefix) (match-end 1)))
                    (quarto-chunk-label-definitions open)))))
         (t
          (when (string-match "\\`\\(#+\\)[ \t]+\\(.+\\)" text)
            (push (list (match-string 2 text) start (length (match-string 1 text))) headings))
          (setq line (quarto-document--mask-inline-code line))
          (let ((offset 0))
            (while (string-match "{#\\([[:alnum:]_:.+-]+\\)\\(?:[ } ]\\)" line offset)
              (push (cons (match-string 1 line) (+ start (match-beginning 1))) labels)
              (push (list :key (match-string 1 line) :start (+ start (match-beginning 1))
                          :end (+ start (match-end 1))
                          :description (string-trim (substring line 0 (match-beginning 0)))) targets)
              (setq offset (match-end 0))))
          (setq references (append (quarto-document--references line start) references))))))
    (when open
      (push (list (quarto-chunk-start open) 'error "Unclosed code fence; execution disabled") diagnostics)
      (setf (quarto-chunk-executable open) nil)
      (push open chunks))
    (dolist (chunk chunks)
      (when (stringp (quarto-chunk-label chunk))
        (push (cons (quarto-chunk-label chunk) (quarto-chunk-start chunk)) labels))
      (dolist (target (quarto-chunk-label-definitions chunk))
        (push (append target
                      (list :chunk (quarto-chunk-id chunk)
                            :description (or (cdr (assoc "fig-cap" (quarto-chunk-options chunk)))
                                             (cdr (assoc "tbl-cap" (quarto-chunk-options chunk)))
                                             (format "%s chunk" (quarto-chunk-language chunk))))) targets)))
    (let ((seen (make-hash-table :test #'equal)))
      (dolist (label (reverse labels))
        (when (gethash (car label) seen)
          (push (list (cdr label) 'error (concat "Duplicate label: " (car label))) diagnostics))
        (puthash (car label) t seen))
      (dolist (ref references)
        (when (and (string-match-p "\\`\\(?:fig\\|tbl\\|sec\\|eq\\|lst\\|thm\\|lem\\|def\\|exm\\)-" (car ref))
                   (not (gethash (car ref) seen)))
          (push (list (cdr ref) 'warning (concat "Unresolved cross-reference: " (car ref))) diagnostics))))
    (let ((live (make-hash-table :test #'eql)))
      (dolist (chunk chunks) (puthash (quarto-chunk-id chunk) t live))
      (dolist (previous old)
        (unless (gethash (quarto-chunk-id previous) live) (set-marker (quarto-chunk-marker previous) nil))))
    (make-quarto-document :revision (buffer-chars-modified-tick) :chunks (nreverse chunks)
                         :headings (nreverse headings) :labels (nreverse labels)
                         :references (nreverse references) :front-matter front
                         :targets (nreverse targets)
                         :diagnostics (nreverse diagnostics))))

(defun quarto-document-model ()
  "Return the current document index, shared by all indirect buffers."
  (with-current-buffer (quarto-source-buffer)
    (save-excursion
      (save-restriction
        (widen)
        (unless (and quarto-document--model
                     (= (quarto-document-revision quarto-document--model) (buffer-chars-modified-tick)))
          (setq quarto-document--model (quarto-document--parse)))
        quarto-document--model))))

(defun quarto-document-metadata ()
  "Return front matter without fences, including its unfinished last line."
  (with-current-buffer (quarto-source-buffer)
    (if-let* ((front (quarto-document-front-matter (quarto-document-model))))
        (save-excursion (save-restriction
          (widen) (goto-char (car front)) (forward-line 1)
          (let* ((start (point))
                 (closed (re-search-forward "^\\(?:---\\|\\.\\.\\.\\)[ \t]*$" (cdr front) t)))
            (buffer-substring-no-properties start (if closed (match-beginning 0) (cdr front))))))
      "")))

(defun quarto-document-reference-at (&optional position)
  "Return the indexed reference containing POSITION, including an empty @."
  (let ((pos (or position (point))))
    (cl-find-if (lambda (ref) (<= (cdr ref) pos (+ (cdr ref) (length (car ref)))))
                (quarto-document-references (quarto-document-model)))))

(defun quarto-chunk-at (&optional position)
  "Return the chunk containing POSITION, including its fences."
  (let ((pos (or position (point))))
    (cl-find-if (lambda (c) (and (<= (quarto-chunk-start c) pos) (< pos (quarto-chunk-end c))))
                (quarto-document-chunks (quarto-document-model)))))

(defun quarto-document-match (language part ahead)
  "Polymode boundary matcher for LANGUAGE, PART and search direction AHEAD."
  (let* ((pos (point))
         (ranges (cl-loop for c in (quarto-document-chunks (quarto-document-model))
                          when (equal language (quarto-chunk-language c))
                          collect (if (eq part 'head)
                                      (cons (quarto-chunk-start c) (quarto-chunk-head-end c))
                                    (cons (quarto-chunk-body-end c) (quarto-chunk-end c)))))
         (match (if (< ahead 0)
                    (cl-find-if (lambda (r) (<= (car r) pos)) (reverse ranges))
                  (cl-find-if (lambda (r) (>= (car r) pos)) ranges))))
    (when match (goto-char (if (< ahead 0) (car match) (cdr match))) match)))

(defun quarto-chunk-code (chunk &optional start end)
  "Return CHUNK code from START to END with container indentation removed."
  (with-current-buffer (quarto-source-buffer)
    (save-restriction
      (widen)
      (let* ((text (buffer-substring-no-properties (or start (quarto-chunk-head-end chunk))
                                                 (or end (quarto-chunk-body-end chunk))))
             (prefix (quarto-chunk-prefix chunk))
             (width (length prefix)))
        (mapconcat (lambda (line)
                     (cond ((string-prefix-p prefix line) (substring line width))
                           ((string-match-p (format "\\`[ \t]\\{%d\\}" width) line) (substring line width))
                           (t line)))
                   (split-string text "\n") "\n")))))

(defun quarto-document-projection (language)
  "Return (TEXT . MAP) for LANGUAGE, retaining source line numbers.
MAP contains (projection-start source-start length chunk-id) for code lines.
Only these ranges are writable; prose and fence lines are blank."
  (with-current-buffer (quarto-source-buffer)
    (save-excursion
      (save-restriction
        (widen)
        (let ((chunks (cl-remove-if-not (lambda (c) (and (equal language (quarto-chunk-language c)) (quarto-chunk-closed c)))
                                         (quarto-document-chunks (quarto-document-model))))
              (position 1) text mapping)
          (goto-char (point-min))
          (while (< (point) (point-max))
            (while (and chunks (>= (point) (quarto-chunk-body-end (car chunks)))) (pop chunks))
            (let* ((start (point)) (end (line-end-position))
                   (chunk (and chunks (<= (quarto-chunk-head-end (car chunks)) start) (car chunks)))
                   (line (if chunk (quarto-chunk-code chunk start end) ""))
                   (source-start (- end (length line))))
              (when chunk (push (list position source-start (length line) (quarto-chunk-id chunk)) mapping))
              (push line text) (cl-incf position (1+ (length line))) (forward-line 1)))
          (cons (concat (mapconcat #'identity (nreverse text) "\n") "\n") (nreverse mapping)))))))

(defun quarto-map-position (position mapping &optional to-source)
  "Map POSITION through MAPPING, or nil outside a code line.
TO-SOURCE maps from projection to source.  Positions are Emacs characters."
  (let* ((from (if to-source 0 1)) (to (if to-source 1 0))
         (entry (cl-find-if (lambda (m) (<= (nth from m) position (+ (nth from m) (nth 2 m)))) mapping)))
    (and entry (+ (nth to entry) (- position (nth from entry))))))

(defun quarto-document-after-change (begin _end _length)
  (run-hook-with-args 'quarto-document-change-hook (quarto-source-buffer) begin))

(provide 'quarto-document)
;;; quarto-document.el ends here
