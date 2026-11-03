;;; org-scribe-planner-io.el --- Plan files for org-scribe-planner -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Javier Castilla
;; URL: https://codeberg.org/jcastp/org-scribe

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Saving and loading plans as Org headings (the property table and the
;; Schedule table), finding a project's plan file, and the active-plan
;; lookup.
;;
;; Part of the writing planner; load it through `org-scribe-planner',
;; which requires every part in order.

;;; Code:

(require 'org)
(require 'org-element)
(require 'calendar)
(require 'cl-lib)
(require 'org-scribe-planner-engine)

(declare-function org-scribe--project-marker-get "core/org-scribe-core")
(declare-function org-scribe--project-marker-set "core/org-scribe-core")
(declare-function org-scribe-plan "org-scribe-planner")
(declare-function org-scribe-planner-gate "core/org-scribe-core")
(declare-function org-scribe-project-root "core/org-scribe-core")

;;; File Selection Helper

(defun org-scribe-planner--project-root ()
  "Return the current project's root via `org-scribe-planner-project-root-function'.
Nil outside a project or when no function is set."
  (when org-scribe-planner-project-root-function
    (ignore-errors (funcall org-scribe-planner-project-root-function))))

(defun org-scribe-planner--select-plan-file (prompt &optional directory)
  "Prompt user to select a plan file with PROMPT.
If DIRECTORY is non-nil, use it as the starting directory instead of
`org-scribe-planner-directory'.  Returns the selected file path."
  (read-file-name prompt
                  (or directory
                      (org-scribe-planner--project-root)
                      org-scribe-planner-directory)
                  nil
                  t
                  nil
		  ;; this allows now for path manipulation in the minibuffer
		  (lambda (name)
                    (or (file-directory-p name)
			(string-match-p "\\.org$" name)))))

(defun org-scribe-planner--get-current-plan (&optional allow-prompt)
  "Return the current active plan and its file path as (plan . file).
If no plan is active and ALLOW-PROMPT is non-nil, prompt the user to load one.
If no plan is active and ALLOW-PROMPT is nil, signal an error.
Returns nil if user cancels the prompt."
  (cond
   ;; Current plan is available
   ((and org-scribe-planner--current-plan
         org-scribe-planner--current-plan-file)
    (cons org-scribe-planner--current-plan
          org-scribe-planner--current-plan-file))

   ;; No current plan and prompting is allowed
   (allow-prompt
    (if (y-or-n-p "No active plan. Would you like to load one? ")
        (let* ((root (org-scribe-planner--project-root))
               (project-plan (and root (org-scribe-planner--find-plan-file root)))
               (file (if (and project-plan
                              (org-scribe-planner--plan-file-valid-p project-plan))
                         project-plan
                       (org-scribe-planner--select-plan-file "Select plan file: " root))))
          (when file
            (let ((plan (org-scribe-planner--load-plan file)))
              (setq org-scribe-planner--current-plan plan)
              (setq org-scribe-planner--current-plan-file file)
              (cons plan file))))
      nil))

   ;; No current plan and no prompting allowed
   (t
    (error "No active plan. Use `org-scribe-planner-load-plan' to load a plan first"))))

;;; Org-mode Integration

(defconst org-scribe-planner--note-colon-placeholder "\x01"
  "Substituted for literal colons when serializing a note into the
DATE:WORDS:NOTE:TARGET DAILY_WORD_COUNTS property, which is parsed by
splitting naively on \":\".  A control character is used because it
cannot be typed via `read-string' and so never collides with real
note text, unlike a printable escape sequence.")

(defconst org-scribe-planner--note-pipe-placeholder "\x02"
  "Substituted for literal pipes when serializing a note into a
Schedule table cell, which is delimited by \"|\".")

(defun org-scribe-planner--escape-note-for-property (note)
  "Escape colons in NOTE so it survives the DAILY_WORD_COUNTS round-trip."
  (replace-regexp-in-string ":" org-scribe-planner--note-colon-placeholder note))

(defun org-scribe-planner--unescape-note-from-property (note)
  "Reverse `org-scribe-planner--escape-note-for-property'."
  (replace-regexp-in-string org-scribe-planner--note-colon-placeholder ":" note))

(defun org-scribe-planner--escape-note-for-table (note)
  "Escape pipes in NOTE so it does not shift columns in the Schedule table."
  (replace-regexp-in-string "|" org-scribe-planner--note-pipe-placeholder note))

(defun org-scribe-planner--unescape-note-from-table (note)
  "Reverse `org-scribe-planner--escape-note-for-table'."
  (replace-regexp-in-string org-scribe-planner--note-pipe-placeholder "|" note))

(defun org-scribe-planner--split-daily-counts-str (str)
  "Split STR into individual daily-count entry strings.
Splits only on commas that are immediately followed by a date pattern
\\(YYYY-MM-DD:DIGIT\\), so commas inside note text are preserved."
  (let ((result nil)
        (start 0))
    (while (string-match ",\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}:[0-9]\\)" str start)
      (let ((comma-pos (match-beginning 0))
            (date-pos  (match-beginning 1)))
        (push (string-trim (substring str start comma-pos)) result)
        (setq start date-pos)))
    (push (string-trim (substring str start)) result)
    (nreverse (cl-remove-if #'string-empty-p result))))

(defun org-scribe-planner--parse-schedule-table ()
  "Parse the Schedule table in the current buffer and extract notes.
Returns an alist of (date . note) pairs for entries with notes."
  (save-excursion
    (goto-char (point-min))
    (let ((notes-alist nil))
      ;; Find the Schedule heading
      (when (re-search-forward "^\\*\\* Schedule" nil t)
        ;; Find the table start
        (when (re-search-forward "^|.*Date.*Target.*Notes.*|" nil t)
          (let* ((header (split-string (match-string 0) "|"))
                 ;; Find Notes by its header, not a fixed position: the
                 ;; table gained a column, and plan files saved before
                 ;; that still have the old one until they are saved again.
                 (before-notes (- (cl-position "Notes" header
                                               :test (lambda (name cell)
                                                       (string= name (string-trim cell))))
                                  2))
                 (row-regexp (concat "^|[[:space:]]+\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[[:space:]]+|"
                                     (apply #'concat (make-list before-notes "[^|]*|"))
                                     "[[:space:]]*\\([^|]+?\\)[[:space:]]*|")))
          ;; Skip the separator line (|------+-----+....|)
          (forward-line 1)
          ;; Now we should be at the first data row - skip separator
          (when (looking-at "^|[-+]+|")
            (forward-line 1))
          ;; Parse table rows
          (while (looking-at row-regexp)
            (let* ((date (match-string 1))
                   (note-raw (match-string 2))
                   ;; Trim whitespace from note, then reverse the pipe
                   ;; escaping applied when the note was written into the table
                   (note (org-scribe-planner--unescape-note-from-table
                          (string-trim note-raw))))
              ;; Only add if note is not empty and not just the default
              ;; "Spare day" / "Rest day" label
              (when (and note
                        (not (string-empty-p note))
                        (not (member note '("Spare day" "Rest day"))))
                (push (cons date note) notes-alist)))
            (forward-line 1)))))
      ;; Return in chronological order
      (nreverse notes-alist))))

(defun org-scribe-planner--buffer-safe-to-erase-p (buffer filepath)
  "Check if BUFFER at FILEPATH is safe to erase completely.
Returns t if safe to erase, nil if caution is needed.
Also checks for unsaved changes and prompts user if needed."
  (with-current-buffer buffer
    (let ((file-exists (file-exists-p filepath))
          (buffer-modified (buffer-modified-p))
          (has-content (> (buffer-size) 0)))

      (cond
       ;; Buffer has unsaved changes - warn user
       (buffer-modified
        (if (y-or-n-p (format "Buffer '%s' has unsaved changes. Overwrite anyway? "
                             (buffer-name)))
            t
          (error "Save cancelled to preserve unsaved changes")))

       ;; New file, no content - safe to write
       ((not has-content)
        t)

       ;; File has content but no org headings (e.g. a comment-only placeholder
       ;; like the plan.org stub shipped in project templates) - safe to overwrite
       ((not (save-excursion
               (goto-char (point-min))
               (re-search-forward "^\\* " nil t)))
        t)

       ;; Existing file with content - check if it's our plan file
       (file-exists
        (org-scribe-planner--is-single-plan-file-p buffer))

       ;; Default to safe (new buffer, no file)
       (t t)))))

(defun org-scribe-planner--plan-file-valid-p (filepath)
  "Return t if FILEPATH contains a saved plan (has a heading with TOTAL_WORDS).
Returns nil for empty files, comment-only placeholders, or non-plan org files."
  (when (and filepath (file-exists-p filepath))
    (with-temp-buffer
      (insert-file-contents filepath)
      (goto-char (point-min))
      (and (re-search-forward "^\\* " nil t)
           (re-search-forward "^:TOTAL_WORDS:" nil t)))))

(defun org-scribe-planner--is-single-plan-file-p (buffer)
  "Check if BUFFER contains a single org-scribe-planner plan.
Returns t if the file appears to be a plan file we created (single heading
with our properties), nil otherwise."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((heading-count 0)
            (has-plan-properties nil))

        ;; Count top-level headings
        (while (re-search-forward "^\\* " nil t)
          (setq heading-count (1+ heading-count)))

        ;; Check for our properties at first heading
        (goto-char (point-min))
        (when (re-search-forward "^\\* " nil t)
          (setq has-plan-properties
                (and (org-entry-get nil "TOTAL_WORDS")
                     (org-entry-get nil "DAILY_WORDS")
                     (org-entry-get nil "DAYS"))))

        ;; Safe if exactly one heading with our properties
        (and (= heading-count 1) has-plan-properties)))))

;;;; Plan properties

(defconst org-scribe-planner--plan-properties
  '(("TOTAL_WORDS"         total-words         int :default 0)
    ("DAILY_WORDS"         daily-words         int :default 0)
    ("DAYS"                days                int :default 0)
    ("START_DATE"          start-date          string)
    ("END_DATE"            end-date            string)
    ("CURRENT_WORDS"       current-words       int :default 0)
    ("PREEXISTING_WORDS"   preexisting-words   int :default 0)
    ;; Derived for plans that don't store it (older files)
    ("WRITING_DAYS"        writing-days        int
     :get org-scribe-planner--writing-days)
    ("LOCKED_GOALS"        locked-goals        date-goals)
    ("SCHEDULE_ADJUSTMENT" schedule-adjustment int :omit (0))
    ;; Kept even with no adjustment: a recalculation that credited the
    ;; day with its goal is out of date if the day ends below it
    ("REBASED_FROM"        rebased-from        string)
    ("REBASED_ON"          rebased-on          string :parent "REBASED_FROM")
    ("REBASED_WORDS"       rebased-words       int    :parent "REBASED_FROM")
    ("PATTERN"             pattern             symbol :omit (flat))
    ("PATTERN_CONFIG"      pattern-config      pattern-config :parent "PATTERN")
    ("WEEKEND_MODE"        weekend-mode        symbol :omit (same))
    ("WEEKEND_DAYS"        weekend-days        int-list
     :parent "WEEKEND_MODE" :default (6 0))
    ("WEEKEND_DAILY"       weekend-daily       int
     :parent "WEEKEND_MODE" :default 0
     :save-if org-scribe-planner--weekend-goal-p)
    ("OUTSIDE_WORDS"       outside-words       int :omit (0))
    ("OUTSIDE_FILES"       outside-files       plists)
    ("WORDS_BASIS"         words-basis         basis)
    ("CUSTOM_GOALS"        custom-goals        sorted-date-goals)
    ("SPARE_DAYS"          spare-days          string-list)
    ("DAILY_WORD_COUNTS"   daily-word-counts   daily-counts))
  "How each `org-scribe-plan' slot is stored as a property of the plan heading.
Each entry is (PROPERTY SLOT CODEC . OPTIONS), written in this order by
`org-scribe-planner--write-plan-properties' and read back by
`org-scribe-planner--read-plan-properties'.  CODEC names the text format
\(see `org-scribe-planner--encode-property').  OPTIONS:

  :default VALUE  used when the slot is nil on save, or the property is
                  missing on load (otherwise the slot keeps its default)
  :omit VALUES    not written when the value is one of VALUES (a value
                  with nothing to encode, such as nil, is never written)
  :parent PROP    written and read only along with PROP
  :get FN         FN of the plan gives the value to write, not the slot
  :save-if FN     written only when FN of the plan is non-nil")

(defun org-scribe-planner--weekend-goal-p (plan)
  "Return non-nil when PLAN's weekends have a goal of their own."
  (eq (org-scribe-plan-weekend-mode plan) 'different))

(defun org-scribe-planner--same-value-p (a b)
  "Return non-nil when A and B are the same value; numbers compare with `='."
  (if (and (numberp a) (numberp b)) (= a b) (eq a b)))

(defun org-scribe-planner--format-daily-counts (daily-counts)
  "Write DAILY-COUNTS as the DAILY_WORD_COUNTS property text.
Note-only entries have no :words and are left out (they live in the
Schedule table); see `org-scribe-planner--format-daily-count-entry'."
  (mapconcat #'org-scribe-planner--format-daily-count-entry
             (org-scribe-planner--counts-with-words daily-counts)
             ","))

(defun org-scribe-planner--parse-daily-count-entry (text)
  "Read one DAILY_WORD_COUNTS entry TEXT into (DATE . PLIST).
Entries are date:words, date:words:note, date:words::target or
date:words:note:target (see `org-scribe-planner--format-daily-count-entry')."
  (let* ((parts (split-string text ":" nil " ")) ; keep empty parts for ::
         (note (nth 2 parts))
         (target (nth 3 parts))
         (data (list :words (string-to-number (nth 1 parts))
                     :note (org-scribe-planner--unescape-note-from-property
                            (or note "")))))
    (cons (nth 0 parts)
          (if (and target (not (string-empty-p target)))
              (append data (list :target (string-to-number target)))
            data))))

(defun org-scribe-planner--parse-date-goals (text)
  "Read TEXT, \"DATE:GOAL,DATE:GOAL\", into an alist of (DATE . GOAL)."
  (mapcar (lambda (item)
            (let ((parts (split-string item ":")))
              (cons (car parts) (string-to-number (cadr parts)))))
          (split-string text "," t " ")))

(defun org-scribe-planner--format-date-goals (goals)
  "Write GOALS, an alist of (DATE . GOAL), as \"DATE:GOAL,DATE:GOAL\"."
  (mapconcat (lambda (goal) (format "%s:%d" (car goal) (cdr goal))) goals ","))

(defun org-scribe-planner--encode-property (codec value)
  "Return VALUE as CODEC's property text, or nil when there is nothing to write."
  (let ((text
         (when value
           (pcase codec
             ('int (number-to-string value))
             ('string value)
             ('symbol (symbol-name value))
             ('int-list (mapconcat #'number-to-string value ","))
             ('string-list (mapconcat #'identity value ","))
             ('date-goals (org-scribe-planner--format-date-goals value))
             ('sorted-date-goals
              (org-scribe-planner--format-date-goals
               (sort (copy-sequence value) (lambda (a b) (string< (car a) (car b))))))
             ('pattern-config (org-scribe-planner--format-pattern-config value))
             ('plists (prin1-to-string value))
             ('basis (and (eq value 'manuscript) "manuscript"))
             ('daily-counts (org-scribe-planner--format-daily-counts value))
             (_ (error "Unknown plan property codec: %s" codec))))))
    (unless (equal text "") text)))

(defun org-scribe-planner--decode-property (codec text)
  "Return the value CODEC's property TEXT stands for."
  (pcase codec
    ('int (string-to-number text))
    ('string text)
    ('symbol (intern text))
    ('int-list (mapcar #'string-to-number (split-string text "," t " ")))
    ('string-list (split-string text "," t " "))
    ((or 'date-goals 'sorted-date-goals) (org-scribe-planner--parse-date-goals text))
    ('pattern-config (org-scribe-planner--parse-pattern-config text))
    ;; Data only: a list of plists, read without evaluating
    ('plists (condition-case nil
                 (let ((value (car (read-from-string text))))
                   (when (and (listp value) (cl-every #'listp value)) value))
               (error nil)))
    ('basis (and (eq (intern-soft text) 'manuscript) 'manuscript))
    ('daily-counts (mapcar #'org-scribe-planner--parse-daily-count-entry
                           (org-scribe-planner--split-daily-counts-str text)))
    (_ (error "Unknown plan property codec: %s" codec))))

(defun org-scribe-planner--write-plan-properties (plan)
  "Set PLAN's properties on the heading at point.
See `org-scribe-planner--plan-properties'."
  (let ((written nil))
    (pcase-dolist (`(,property ,slot ,codec . ,options) org-scribe-planner--plan-properties)
      (let* ((getter (plist-get options :get))
             (value (or (if getter
                            (funcall getter plan)
                          (cl-struct-slot-value 'org-scribe-plan slot plan))
                        (plist-get options :default)))
             (parent (plist-get options :parent))
             (save-if (plist-get options :save-if))
             (text (and (or (null parent) (member parent written))
                        (or (null save-if) (funcall save-if plan))
                        (not (cl-member value (plist-get options :omit)
                                        :test #'org-scribe-planner--same-value-p))
                        (org-scribe-planner--encode-property codec value))))
        (when text
          (org-set-property property text)
          (push property written))))))

(defun org-scribe-planner--read-plan-properties (plan)
  "Fill PLAN's slots from the properties of the heading at point.
See `org-scribe-planner--plan-properties'."
  (pcase-dolist (`(,property ,slot ,codec . ,options) org-scribe-planner--plan-properties)
    (let ((parent (plist-get options :parent)))
      (when (or (null parent) (org-entry-get nil parent))
        (let ((text (org-entry-get nil property)))
          (cond
           (text
            (setf (cl-struct-slot-value 'org-scribe-plan slot plan)
                  (org-scribe-planner--decode-property codec text)))
           ((plist-member options :default)
            (setf (cl-struct-slot-value 'org-scribe-plan slot plan)
                  (copy-tree (plist-get options :default))))))))))

(defun org-scribe-planner--save-plan (plan &optional filepath)
  "Save PLAN to an Org-mode file.
If FILEPATH is not provided, generate a default filename in org-scribe-planner-directory."
  (let* ((filename (concat (downcase (replace-regexp-in-string
                                     "[^[:alnum:]]" "-"
                                     (org-scribe-plan-title plan)))
                          ".org"))
         (filepath (or filepath
                      (expand-file-name filename org-scribe-planner-directory))))

    ;; Ensure directory exists and is writable
    (let ((save-dir (file-name-directory filepath)))
      (unless (file-exists-p save-dir)
        (make-directory save-dir t))
      (unless (file-writable-p save-dir)
        (error "Cannot save plan: directory '%s' is not writable. Check directory permissions" save-dir)))

    ;; Check if file exists and is writable (if it exists)
    (when (file-exists-p filepath)
      (unless (file-writable-p filepath)
        (error "Cannot save plan: file '%s' is not writable. Check file permissions" filepath)))

    ;; END_DATE and DAYS are derived from the writing days
    (when (and (org-scribe-plan-start-date plan)
               (org-scribe-planner--writing-days plan))
      (org-scribe-planner--sync-window plan))

    ;; Create or update org file
    (let ((buf (find-file-noselect filepath)))
      (with-current-buffer buf
        ;; Safety check before erasing
        (unless (org-scribe-planner--buffer-safe-to-erase-p buf filepath)
          (error "Cannot save plan: file '%s' contains multiple headings or non-plan content. Please use a dedicated file for this plan" filepath))

        ;; Preserve any "** Agenda Entries" subtree (created separately by
        ;; `org-scribe-planner-sync-agenda') across the erase-buffer below —
        ;; otherwise agenda integration is wiped out on every save, which
        ;; happens automatically on every word-count sync.
        (let ((agenda-entries-text
               (save-excursion
                 (goto-char (point-min))
                 (when (re-search-forward "^\\*\\* Agenda Entries" nil t)
                   (goto-char (line-beginning-position))
                   (buffer-substring-no-properties (point) (point-max))))))

        (erase-buffer)
        (org-mode)

        ;; Insert heading with properties
        (insert (format "* %s\n" (org-scribe-plan-title plan)))
        (org-scribe-planner--write-plan-properties plan)

        ;; Add schedule as content
        (goto-char (point-max))
        (insert "\n** Schedule\n\n")
        (let ((schedule (org-scribe-planner--generate-day-schedule plan))
              (daily-counts (org-scribe-plan-daily-word-counts plan)))
          ;; Planned and actual totals in two columns.  One "Cumulative"
          ;; column used to show the actual total on days with an entry
          ;; and a projection from it on the others, so it fell after a
          ;; missed day and ended short of the plan's total.
          (insert "| Date | Target | Planned total | Actual total | Actual | Progress % | Notes |\n")
          (insert "|------+--------+---------------+--------------+--------+------------+-------|\n")
          (dolist (day schedule)
            (let* ((date (plist-get day :date))
                   (daily-entry (assoc date daily-counts))
                   (daily-data (when daily-entry (cdr daily-entry)))
                   ;; Use stored target if available, otherwise use current plan's daily-words
                   (stored-target (when daily-data
                                   (plist-get daily-data :target)))
                   (target (or stored-target (plist-get day :words)))
                   (is-spare (plist-get day :is-spare-day))
                   ;; This day's net delta, derived from the cumulative total
                   ;; stored in daily-data (nil when no entry exists for date)
                   (actual (when daily-data
                            (org-scribe-planner--entry-delta daily-counts date
                             (org-scribe-plan-preexisting-words plan))))
                   (note (when daily-data
                          (org-scribe-planner--escape-note-for-table
                           (or (plist-get daily-data :note) ""))))
                   (percentage (if (and actual (not is-spare) (> target 0))
                                  (format "%.1f%%" (* 100.0 (/ (float actual) target)))
                                "")))

              (insert (format "| %s | %s | %d | %s | %s | %s | %s |\n"
                             date
                             (if is-spare "REST" (number-to-string target))
                             (org-scribe-planner--planned-total plan day)
                             ;; The entry's :words is the manuscript total
                             ;; as of that date; a note-only entry has none.
                             (let ((total (plist-get daily-data :words)))
                               (if (numberp total) (number-to-string total) ""))
                             (if actual (number-to-string actual) "")
                             percentage
                             (cond
                              ;; If there's a custom note (even for spare days), show it
                              ((and note (not (string-empty-p note))) note)
                              ;; Otherwise, if it's a spare day, show default message
                              (is-spare (if (plist-get day :spare) "Spare day" "Rest day"))
                              ;; No note and not spare day
                              (t "")))))))

        (org-table-align)

        (when agenda-entries-text
          (goto-char (point-max))
          (insert "\n" agenda-entries-text))

        (save-buffer)
        (message "Plan saved to %s" filepath))))))

(defun org-scribe-planner--format-pattern-config (config)
  "Write pattern CONFIG, a plist, as \"peak=1.5 valley=0.7\" for PATTERN_CONFIG."
  (let ((parts nil))
    (while config
      (push (format "%s=%s" (substring (symbol-name (car config)) 1) (cadr config)) parts)
      (setq config (cddr config)))
    (mapconcat #'identity (nreverse parts) " ")))

(defun org-scribe-planner--parse-pattern-config (text)
  "Read TEXT, as written by `org-scribe-planner--format-pattern-config'."
  (when text
    (apply #'append
           (mapcar (lambda (item)
                     (let ((parts (split-string item "=")))
                       (list (intern (concat ":" (car parts)))
                             (string-to-number (cadr parts)))))
                   (split-string text " " t)))))

(defun org-scribe-planner--load-plan (filepath)
  "Load a writing plan from FILEPATH."
  ;; Check if file exists and is readable
  (unless (file-exists-p filepath)
    (error "Cannot load plan: file does not exist at '%s'" filepath))
  (unless (file-readable-p filepath)
    (error "Cannot load plan: file is not readable at '%s'. Check file permissions" filepath))

  (with-current-buffer (find-file-noselect filepath)
    (goto-char (point-min))
    (let ((plan (make-org-scribe-plan)))
      (unless (re-search-forward "^\\*" nil t)
        (error "Cannot load plan: no org heading found in '%s'. File may be corrupted or not a valid plan file" filepath))

      ;; Heading was found, extract plan data
      (setf (org-scribe-plan-title plan)
            (org-get-heading t t t t))
      (org-scribe-planner--read-plan-properties plan)

      (when (and (org-scribe-plan-start-date plan)
                 (org-scribe-planner--writing-days plan))
        (org-scribe-planner--sync-window plan)
        (org-scribe-planner--sync-daily plan))

      (setf (org-scribe-plan-org-heading-marker plan)
            (point-marker))

      ;; Parse the Schedule table to extract manually added notes
      (let ((table-notes (org-scribe-planner--parse-schedule-table))
            (notes-updated nil))
        (when table-notes
          ;; Merge table notes into daily-word-counts
          ;; Table notes take precedence over property notes
          (dolist (table-entry table-notes)
            (let* ((date (car table-entry))
                   (note (cdr table-entry))
                   (existing-entry (assoc date (org-scribe-plan-daily-word-counts plan))))
              (if existing-entry
                  ;; Update existing entry with note from table
                  (let* ((data (cdr existing-entry))
                         (old-note (or (plist-get data :note) "")))
                    ;; Only update if note actually changed
                    (unless (string= old-note note)
                      (setq notes-updated t)
                      ;; Update the :note field
                      (plist-put data :note note)))
                ;; No existing entry - create a note-only entry (no :words),
                ;; matching `org-scribe-planner--add-spare-day-note'.  A note
                ;; appears in the table for spare/future days that never had
                ;; an actual word count; giving it :words 0 would make
                ;; downstream code (cumulative/expected totals, remaining
                ;; working-day counts) treat it as real zero-word "actual
                ;; data" for that day (H8).
                (setq notes-updated t)
                (push (cons date (list :note note))
                      (org-scribe-plan-daily-word-counts plan)))))

          ;; If we updated any notes, persist them to the properties.
          ;; Filter through `--counts-with-words' first: note-only entries
          ;; have no :words key, and `--format-daily-count-entry' requires one.
          (when notes-updated
            (org-set-property "DAILY_WORD_COUNTS"
                              (org-scribe-planner--format-daily-counts
                               (org-scribe-plan-daily-word-counts plan)))
            (save-buffer)
            (message "Merged %d note(s) from schedule table into plan properties"
                     (length table-notes)))))

      ;; Last, so nothing above writes converted numbers without the basis.
      ;; The next save writes both.
      (let ((shifted (org-scribe-planner--convert-to-manuscript-basis
                      plan (org-scribe-planner--outside-base filepath))))
        (when shifted
          (message "Plan now counts the manuscript only: %d words outside it are shown but not counted. If the total included them, lower it: What changed? -> Total words."
                   shifted)))

      plan)))

;;; Helper Functions for Daily Word Counts

(defun org-scribe-planner--format-daily-count-entry (entry)
  "Format daily-word-count ENTRY as a string for the DAILY_WORD_COUNTS property.
ENTRY must be in new format: (date . (:words N :note \"...\" :target M)).
Returns one of:
  \"DATE:WORDS\"
  \"DATE:WORDS:NOTE\"
  \"DATE:WORDS::TARGET\"
  \"DATE:WORDS:NOTE:TARGET\"
Colons in NOTE are escaped (see `org-scribe-planner--escape-note-for-property')
so a note like \"fixed plot: rewrote scene 3\" does not get mis-split on load."
  (let* ((date       (car entry))
         (data       (cdr entry))
         (word-count (plist-get data :words))
         (note       (org-scribe-planner--escape-note-for-property
                      (or (plist-get data :note) "")))
         (target     (plist-get data :target)))
    (cond
     (target
      (if (and note (not (string-empty-p note)))
          (format "%s:%d:%s:%d" date word-count note target)
        (format "%s:%d::%d" date word-count target)))
     ((and note (not (string-empty-p note)))
      (format "%s:%d:%s" date word-count note))
     (t
      (format "%s:%d" date word-count)))))

;;; Plan discovery

(defun org-scribe-planner--find-plan-file (root)
  "Search ROOT for a plan .org file and return its path, or nil if none found.

Search order:
  0. Path recorded in .org-scribe-project as \"# Plan: <path>\" (fastest)
  1. <root>/plan.org
  2. <root>/<title>-plan.org  (title derived from .org-scribe-project)
  3. Any single .org file in ROOT with a TOTAL_WORDS property"
  (let ((candidate-1 (expand-file-name "plan.org" root)))
    (cond
     ;; 0. Path recorded in the .org-scribe-project marker file.  Resolved
     ;; against ROOT: a project-relative path (the format written since
     ;; this fix) expands normally, while a legacy absolute path written
     ;; before this fix is returned unchanged by `expand-file-name'.
     ((let* ((recorded (org-scribe--project-marker-get root "Plan"))
             (resolved (when recorded (expand-file-name recorded root))))
        (when (and resolved (file-exists-p resolved))
          resolved)))
     ;; 1. Plain plan.org
     ((file-exists-p candidate-1) candidate-1)
     ;; 2. <title>-plan.org derived from the marker file
     ;; Both the current title and the name at creation are tried, so a
     ;; plan named before a title change is still found after it.
     ((cl-find-if
       #'file-exists-p
       (mapcar (lambda (title)
                 (expand-file-name
                  (concat (downcase (replace-regexp-in-string
                                     "[^[:alnum:]]" "-" title))
                          "-plan.org")
                  root))
               (delq nil (list (org-scribe--project-marker-get root "Title")
                               (org-scribe--project-marker-get root "Writing project"))))))
     ;; 3. Any single .org file in ROOT with a TOTAL_WORDS property
     ((cl-find-if
       (lambda (f)
         (with-temp-buffer
           (insert-file-contents f)
           (goto-char (point-min))
           (re-search-forward "^:TOTAL_WORDS:" nil t)))
       (directory-files root t "\\.org$" t))))))

(defvar org-scribe-planner--auto-loading nil
  "Non-nil while `org-scribe-planner--auto-load-plan' is loading a plan.")

(defun org-scribe-planner--auto-load-plan ()
  "Try to auto-load the plan associated with the current org-scribe project.
Silently does nothing if the project has not opted into the planner
\(see `org-scribe-planner-gate'), no plan candidate is found, or a plan
is already active.

It also does nothing when called from inside its own load: visiting the
plan file turns on `org-scribe-mode' there, whose hook calls this again
before the outer call has set the current plan, so the plan was loaded
twice (two messages, past goals locked and the after-load hook run twice)."
  (when (and (featurep 'org-scribe)
             (not org-scribe-planner--auto-loading)
             (not org-scribe-planner--current-plan))
    (when-let* ((root (org-scribe-project-root))
                (_ (eq (org-scribe-planner-gate root) 'yes))
                (plan-file (org-scribe-planner--find-plan-file root))
                (plan (let ((org-scribe-planner--auto-loading t))
                        (ignore-errors (org-scribe-planner--load-plan plan-file)))))
      (org-scribe-planner--lock-past-goals plan)
      (setq org-scribe-planner--current-plan plan)
      (setq org-scribe-planner--current-plan-file plan-file)
      (run-hook-with-args 'org-scribe-planner-after-plan-load-hook plan plan-file)
      (message "org-scribe-planner: loaded plan \"%s\"." (org-scribe-plan-title plan)))))

;;; Plan-path persistence

(defun org-scribe-planner--save-plan-path (_plan plan-file)
  "Record PLAN-FILE in the .org-scribe-project marker for fast future discovery.
_PLAN is the plan struct passed by the hook; it is not used here.
The path is stored relative to the project root (see
`org-scribe-planner--find-plan-file') so the marker stays valid when the
whole project directory is synced (Nextcloud, etc.) to a different
machine or home path."
  (when (featurep 'org-scribe)
    (when-let* ((root (org-scribe-project-root))
                (relative-path (file-relative-name plan-file root)))
      (org-scribe--project-marker-set root "Plan" relative-path))))

(provide 'org-scribe-planner-io)

;;; org-scribe-planner-io.el ends here
