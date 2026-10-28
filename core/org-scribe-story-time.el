;;; org-scribe-story-time.el --- Story time parser for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A pure parser, with no UI, for the text a writer puts in a scene's
;; `:Timeline:' property.  It is to story time what `org-scribe-mythes.el'
;; is to the thesaurus: a string in, data out, testable in batch with no
;; project.
;;
;; Three things are kept apart.  The *notation* is what the writer typed and
;; is never rewritten.  The *key* is one integer computed from it, in
;; minutes from the start of Day 1 (negative before it); every view sorts on
;; the key alone.  The *display* is always the notation.  A value that cannot
;; be read gets no key, is never guessed, and goes to an \"undated\" group.
;;
;; Notations:
;;
;;   Day 3 / Day 3, 14:00      relative days (no Day 0: Day -1 is the day
;;   Night 3 / Night 3, 02:00  before Day 1).  Night N runs from Day N's
;;   Year -9 / Year -9, Day 40 evening to Day N+1's dawn.  A year is 365 days
;;                             for sorting only.
;;   <2026-03-14 Sat 18:00>    an Org timestamp, counted from the project's
;;                             Day 1 date.
;;   14 Frostmoon 312, 18:00   a date in the writer's invented calendar.
;;
;; The words (Day/Night/Year) come from the language packs' `:story-time'
;; section; every registered language's spelling is accepted, accents
;; optional, case ignored.  A Day with no clock sorts at 06:00 and a Night
;; with none at 20:00; a clock time always wins.
;;
;; `org-scribe-story-time-key' takes a CONTEXT, a plist built once per run
;; by `org-scribe-story-time-context':
;;
;;   :mode      `relative' (default), `dates' or `calendar'
;;   :day-one   absolute day number (see `calendar-absolute-from-gregorian')
;;              of Day 1, for timestamps; nil when the project has no anchor
;;   :calendar  (:months ((NAME . DAYS) ...) :era ERA :day-one N) or nil,
;;              N being Day 1's day count in that calendar

;;; Code:

(require 'cl-lib)
(require 'calendar)
(require 'org)
(require 'ucs-normalize)
(require 'org-scribe-core)
(require 'org-scribe-lang)

(defconst org-scribe-story-time--minutes-per-day 1440)
(defconst org-scribe-story-time--days-per-year 365
  "Length of a year, for sorting only; the display keeps the writer's text.")
(defconst org-scribe-story-time--default-day-clock 360
  "Minutes after midnight at which a Day with no clock sorts (06:00).")
(defconst org-scribe-story-time--default-night-clock 1200
  "Minutes after midnight at which a Night with no clock sorts (20:00).")

;;; Text normalization

(defun org-scribe-story-time--normalize (string)
  "Return STRING lowercased, without accents, with a plain minus sign."
  (let ((s (replace-regexp-in-string
            "[−‒–]" "-"
            (downcase (ucs-normalize-NFD-string string)))))
    (replace-regexp-in-string "[̀-ͯ]" "" s)))

(defun org-scribe-story-time--words (concept)
  "Return the normalized spellings of CONCEPT in every registered language."
  (delete-dups
   (mapcar #'org-scribe-story-time--normalize
           (or (org-scribe-lang-all :story-time concept) nil))))

(defun org-scribe-story-time--alternation (words)
  "Return a regexp matching any of WORDS."
  (regexp-opt words))

;;; Clock

(defun org-scribe-story-time--clock (hours minutes)
  "Return HOURS and MINUTES (strings or nil) as minutes after midnight, or nil.
Nil when the clock is absent or out of range."
  (when (and hours minutes)
    (let ((h (string-to-number hours))
          (m (string-to-number minutes)))
      (when (and (<= 0 h 23) (<= 0 m 59))
        (+ (* h 60) m)))))

(defconst org-scribe-story-time--clock-regexp
  "\\(?:\\([0-9]\\{1,2\\}\\):\\([0-9][0-9]\\)\\)"
  "Matches HH:MM; groups are the hours and the minutes.")

;;; Relative notation

(defun org-scribe-story-time--day-start (n)
  "Return the minute at which relative day N starts (no Day 0), or nil for 0."
  (cond ((> n 0) (* (1- n) org-scribe-story-time--minutes-per-day))
        ((< n 0) (* n org-scribe-story-time--minutes-per-day))))

(defun org-scribe-story-time--year-start (n)
  "Return the minute at which year N starts (no year 0), or nil for 0."
  (let ((days org-scribe-story-time--days-per-year))
    (cond ((> n 0) (* (1- n) days org-scribe-story-time--minutes-per-day))
          ((< n 0) (* n days org-scribe-story-time--minutes-per-day)))))

(defun org-scribe-story-time--parse-relative (text)
  "Parse the already normalized TEXT as Year/Day/Night notation, or nil."
  (let* ((year (org-scribe-story-time--alternation
                (org-scribe-story-time--words 'year)))
         (day (org-scribe-story-time--alternation
               (org-scribe-story-time--words 'day)))
         (night (org-scribe-story-time--alternation
                 (org-scribe-story-time--words 'night)))
         (re (concat "\\`[ \t]*"
                     "\\(?:" year "[ \t]+\\(-?[0-9]+\\)\\)?[ \t,]*"
                     "\\(?:\\(" day "\\|" night "\\)[ \t]+\\(-?[0-9]+\\)\\)?[ \t,]*"
                     org-scribe-story-time--clock-regexp "?[ \t,]*\\'")))
    (when (string-match re text)
      (let* ((y (and (match-string 1 text) (string-to-number (match-string 1 text))))
             (word (match-string 2 text))
             (d (and (match-string 3 text) (string-to-number (match-string 3 text))))
             (clock-text (match-string 4 text))
             (clock (org-scribe-story-time--clock
                     (match-string 4 text) (match-string 5 text)))
             (night-p (and word (string-match-p (concat "\\`\\(?:" night "\\)\\'") word))))
        (when (and (or y d)
                   (not (and clock-text (null clock))))
          (org-scribe-story-time--combine y d night-p clock))))))

(defun org-scribe-story-time--combine (year day night-p clock)
  "Return (:key :precision) from YEAR, DAY, NIGHT-P and CLOCK, or nil.
YEAR and DAY are integers or nil; CLOCK is minutes after midnight or nil."
  (let ((base 0) (valid t))
    (when year
      (let ((ys (org-scribe-story-time--year-start year)))
        (if ys (setq base ys) (setq valid nil))))
    (when (and valid day)
      (let ((ds (if year
                    ;; Within a year, Day N is the Nth day of it (no Day 0).
                    (and (> day 0)
                         (* (1- day) org-scribe-story-time--minutes-per-day))
                  (org-scribe-story-time--day-start day))))
        (if ds (setq base (+ base ds)) (setq valid nil))))
    (when valid
      (let* ((offset (cond
                      (clock
                       (if (and night-p (< clock 720))
                           (+ clock org-scribe-story-time--minutes-per-day)
                         clock))
                      (night-p org-scribe-story-time--default-night-clock)
                      (day org-scribe-story-time--default-day-clock)
                      (t 0))))
        (list :key (+ base offset)
              :precision (cond (clock 'clock) (day 'day) (t 'year))
              ;; kept so a far-off day can be named by its year
              :year year :day (and year day))))))

;;; Org timestamps

(defconst org-scribe-story-time--bare-date-regexp
  "\\`[ \t]*\\([0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]\\(?: [0-9]\\{1,2\\}:[0-9][0-9]\\)?\\)[ \t]*\\'"
  "A date typed without Org's angle brackets: 2026-03-14 or 2026-03-14 18:00.")

(defun org-scribe-story-time--parse-timestamp (text context)
  "Parse an Org timestamp in TEXT against CONTEXT's Day 1 date, or nil.
A bare YYYY-MM-DD (with an optional clock) counts as one."
  (when (string-match org-scribe-story-time--bare-date-regexp text)
    (setq text (format "<%s>" (match-string 1 text))))
  (when (string-match
         "[<[]\\([0-9]\\{4\\}\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\\(?: [^]> 0-9]+\\)?\\(?: \\([0-9]\\{1,2\\}\\):\\([0-9][0-9]\\)\\)?[^]>]*[]>]"
         text)
    (let ((day-one (plist-get context :day-one))
          (year (string-to-number (match-string 1 text)))
          (month (string-to-number (match-string 2 text)))
          (day (string-to-number (match-string 3 text)))
          (clock (org-scribe-story-time--clock (match-string 4 text)
                                               (match-string 5 text))))
      (when (and day-one (<= 1 month 12) (<= 1 day 31))
        (list :key (+ (* (- (calendar-absolute-from-gregorian
                             (list month day year))
                            day-one)
                         org-scribe-story-time--minutes-per-day)
                      (or clock org-scribe-story-time--default-day-clock))
              :precision (if clock 'clock 'day))))))

;;; Invented calendar

(defun org-scribe-story-time--calendar-count (calendar year month-index day)
  "Return the day count of YEAR, MONTH-INDEX (0-based), DAY in CALENDAR."
  (let* ((months (plist-get calendar :months))
         (year-length (apply #'+ (mapcar #'cdr months)))
         (before (apply #'+ (mapcar #'cdr (seq-take months month-index)))))
    (+ (* year year-length) before (1- day))))

(defun org-scribe-story-time--parse-calendar (text context)
  "Parse TEXT as a date in CONTEXT's invented calendar, or nil.
TEXT is already normalized."
  (when-let* ((calendar (plist-get context :calendar))
              (months (plist-get calendar :months))
              (names (mapcar (lambda (m) (org-scribe-story-time--normalize (car m)))
                             months))
              (era (plist-get calendar :era))
              (day-one (plist-get calendar :day-one)))
    (let ((re (concat "\\`[ \t]*\\([0-9]+\\)[ \t]+"
                      "\\(" (org-scribe-story-time--alternation names) "\\)"
                      "[ \t]+\\(-?[0-9]+\\)"
                      (if (org-string-nw-p era)
                          (concat "\\(?:[ \t]+" (regexp-quote
                                                 (org-scribe-story-time--normalize era))
                                  "\\)?")
                        "")
                      "\\(?:[ \t]*,[ \t]*" org-scribe-story-time--clock-regexp "\\)?"
                      "[ \t,]*\\'")))
      (when (string-match re text)
        (let* ((day (string-to-number (match-string 1 text)))
               (index (cl-position (match-string 2 text) names :test #'string=))
               (year (string-to-number (match-string 3 text)))
               (clock-text (match-string 4 text))
               (clock (org-scribe-story-time--clock (match-string 4 text)
                                                    (match-string 5 text))))
          (when (and (<= 1 day (cdr (nth index months)))
                     (not (and clock-text (null clock))))
            (list :key (+ (* (- (org-scribe-story-time--calendar-count
                                 calendar year index day)
                                day-one)
                             org-scribe-story-time--minutes-per-day)
                          (or clock org-scribe-story-time--default-day-clock))
                  :precision (if clock 'clock 'day))))))))

;;; Public entry points

(defun org-scribe-story-time-key (string context)
  "Return (:key MINUTES :precision day|clock|year) for STRING, or nil.
CONTEXT is the plist described in the Commentary.  Nil means the text
cannot be placed: it is blank, unreadable, or a date with no anchor to
count from.  See `org-scribe-story-time-problem' for which."
  (when (org-string-nw-p string)
    (or (org-scribe-story-time--parse-timestamp string context)
        (let ((text (org-scribe-story-time--normalize string)))
          (or (org-scribe-story-time--parse-relative text)
              (org-scribe-story-time--parse-calendar text context))))))

(defun org-scribe-story-time-problem (string context)
  "Say why STRING has no key under CONTEXT.
Returns nil when it has one, `blank' for an empty value, `no-anchor' for a
timestamp or calendar date with no Day 1 to count from, `unreadable'
otherwise."
  (cond ((not (org-string-nw-p string)) 'blank)
        ((org-scribe-story-time-key string context) nil)
        ((string-match-p "[<[][0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]" string)
         (if (plist-get context :day-one) 'unreadable 'no-anchor))
        ((string-match-p org-scribe-story-time--bare-date-regexp string)
         (if (plist-get context :day-one) 'unreadable 'no-anchor))
        (t 'unreadable)))

;;; Naming a day

(defun org-scribe-story-time--calendar-day-name (calendar count)
  "Return day COUNT of CALENDAR written as \"14 Frostmoon 312\", or nil."
  (when-let* ((months (plist-get calendar :months))
              (year-length (apply #'+ (mapcar #'cdr months)))
              ((> year-length 0)))
    (let* ((year (floor count year-length))
           (rest (- count (* year year-length)))
           (index 0))
      (while (and (< index (1- (length months))) (>= rest (cdr (nth index months))))
        (setq rest (- rest (cdr (nth index months))))
        (setq index (1+ index)))
      (format "%d %s %d" (1+ rest) (car (nth index months)) year))))

(defun org-scribe-story-time-day-label (parsed context)
  "Name the day of PARSED (the plist `org-scribe-story-time-key' returns).
Returns the date for a project that counts in `dates' or `calendar', the
year and day for a time written with a year (\"Year -9, Day 40\"), and nil
for plain relative days, which the page numbers itself."
  (let ((day (floor (plist-get parsed :key) org-scribe-story-time--minutes-per-day))
        (mode (plist-get context :mode)))
    (cond
     ((and (eq mode 'dates) (plist-get context :day-one))
      (pcase-let ((`(,m ,d ,y) (calendar-gregorian-from-absolute
                                (+ (plist-get context :day-one) day))))
        (format "%04d-%02d-%02d" y m d)))
     ((and (eq mode 'calendar)
           (integerp (plist-get (plist-get context :calendar) :day-one)))
      (org-scribe-story-time--calendar-day-name
       (plist-get context :calendar)
       (+ (plist-get (plist-get context :calendar) :day-one) day)))
     ((plist-get parsed :year)
      (let ((org-scribe-message-language (org-scribe-project-language)))
        (if (plist-get parsed :day)
            (org-scribe-msg 'story-time-label-year-day
                            (plist-get parsed :year) (plist-get parsed :day))
          (org-scribe-msg 'story-time-label-year (plist-get parsed :year))))))))

;;; Context from the project

(defun org-scribe-story-time-mode (&optional root)
  "Return the main notation of ROOT's project: `relative', `dates' or `calendar'.
Read from the \"# Story-time:\" line of the marker file.  A missing or
unrecognized value is `relative', so every earlier project is unchanged."
  (let* ((value (and root (org-scribe--project-marker-get root "Story-time")))
         (mode (and value (intern-soft (downcase (string-trim value))))))
    (if (memq mode '(relative dates calendar)) mode 'relative)))

(defun org-scribe-story-time--read-calendar (file)
  "Return the calendar described by the Calendar heading of FILE, or nil.
The result is (:months ((NAME . DAYS) ...) :era ERA :day-one N)."
  (when (and file (file-exists-p file))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (when-let* ((pos (org-scribe--find-section-heading 'calendar)))
         (goto-char pos)
         (let* ((era (org-entry-get nil "ERA"))
                (day-one (org-entry-get nil "DAY-ONE"))
                (end (save-excursion (org-end-of-subtree t t) (point)))
                months)
           (save-restriction
             (narrow-to-region pos end)
             (goto-char (point-min))
             (when (re-search-forward org-table-line-regexp nil t)
               (dolist (row (ignore-errors (org-table-to-lisp)))
                 (when (and (listp row) (= (length row) 2)
                            (org-string-nw-p (car row))
                            (string-match-p "\\`[0-9]+\\'" (string-trim (cadr row)))
                            (not (string-match-p "\\`[0-9]+\\'" (string-trim (car row)))))
                   (push (cons (string-trim (car row))
                               (string-to-number (cadr row)))
                         months)))))
           (setq months (nreverse months))
           (when months
             (let ((calendar (list :months months :era era)))
               (list :months months :era era
                     :day-one (and (org-string-nw-p day-one)
                                   (org-scribe-story-time--calendar-date-count
                                    calendar day-one)))))))))))

(defun org-scribe-story-time--calendar-date-count (calendar text)
  "Return the day count of the date TEXT written in CALENDAR, or nil."
  (let ((context (list :calendar (append calendar (list :day-one 0)))))
    (when-let* ((parsed (org-scribe-story-time--parse-calendar
                         (org-scribe-story-time--normalize text) context)))
      ;; The key counted from day-one 0 with the default clock; undo both.
      (/ (- (plist-get parsed :key) org-scribe-story-time--default-day-clock)
         org-scribe-story-time--minutes-per-day))))

(defun org-scribe-story-time--parse-date (string)
  "Return the absolute day number of the YYYY-MM-DD STRING, or nil."
  (when (and string
             (string-match "\\`[ \t]*\\([0-9]\\{4\\}\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)"
                           string))
    (let ((y (string-to-number (match-string 1 string)))
          (m (string-to-number (match-string 2 string)))
          (d (string-to-number (match-string 3 string))))
      (when (and (<= 1 m 12) (<= 1 d 31))
        (calendar-absolute-from-gregorian (list m d y))))))

(defun org-scribe-story-time-context (&optional root)
  "Return the story-time CONTEXT of ROOT's project (default: the current one)."
  (let* ((root (or root (org-scribe-project-root)))
         (mode (org-scribe-story-time-mode root))
         (design (plist-get (org-scribe-project-structure) :design-file))
         (calendar (and (eq mode 'calendar)
                        (org-scribe-story-time--read-calendar design))))
    (list :mode mode
          :day-one (org-scribe-story-time--parse-date
                    (org-scribe--project-marker-get root "Story-day-one"))
          :calendar calendar)))

;;; Settings

(defun org-scribe-story-time-revision (root)
  "Return a short string that changes when ROOT's story-time settings do.
They live in the marker file, so no buffer tick tells the companion page."
  (substring (secure-hash 'md5 (format "%S" (list (org-scribe--project-marker-get root "Story-time")
                                                  (org-scribe--project-marker-get root "Story-day-one"))))
             0 8))

(declare-function org-scribe-companion--announce-change "org-scribe-companion")
(declare-function org-scribe-companion--clear-cache "org-scribe-companion-model")

(defun org-scribe-story-time--mode-choices ()
  "Return the alist (LABEL . MODE) offered by `org-scribe-set-story-time'."
  (mapcar (lambda (mode)
            (cons (org-scribe-msg (intern (format "story-time-mode-%s" mode))) mode))
          '(relative dates calendar)))

;;;###autoload
(defun org-scribe-set-story-time (mode &optional day-one)
  "Choose how this project's scenes write their `:Timeline:' text.
MODE is `relative' (Day 3, 14:00), `dates' (Org timestamps, counted from
DAY-ONE, a YYYY-MM-DD string) or `calendar' (the writer's invented
calendar; the notebook's Calendar heading is created first when it is
missing).  Written to the project's marker file, which the companion
page notices."
  (interactive
   (let* ((root (org-scribe--require-project))
          (label (completing-read (org-scribe-msg 'story-time-prompt-mode)
                                  (mapcar #'car (org-scribe-story-time--mode-choices))
                                  nil t))
          (mode (cdr (assoc label (org-scribe-story-time--mode-choices)))))
     (list mode
           (when (eq mode 'dates)
             (read-string (org-scribe-msg 'story-time-prompt-day-one)
                          (org-scribe--project-marker-get root "Story-day-one"))))))
  (let ((root (org-scribe--require-project)))
    (pcase mode
      ('relative (org-scribe--project-marker-set root "Story-time" "relative"))
      ('dates
       (unless (and day-one (org-scribe-story-time--parse-date day-one))
         (user-error "%s" (org-scribe-msg 'story-time-invalid-day-one (or day-one ""))))
       (org-scribe--project-marker-set root "Story-day-one" (string-trim day-one))
       (org-scribe--project-marker-set root "Story-time" "dates"))
      ('calendar
       (if (let ((design (plist-get (org-scribe-project-structure) :design-file)))
             (and design (file-exists-p design)
                  (with-current-buffer (find-file-noselect design)
                    (org-with-wide-buffer (org-scribe--find-section-heading 'calendar)))))
           (org-scribe--project-marker-set root "Story-time" "calendar")
         (call-interactively #'org-scribe-calendar-create)))
      (_ (user-error "%s" (org-scribe-msg 'story-time-prompt-mode))))
    (when (featurep 'org-scribe-companion-model)
      (org-scribe-companion--clear-cache))
    (when (featurep 'org-scribe-companion)
      (org-scribe-companion--announce-change))
    (message "%s" (org-scribe-msg 'story-time-set
                                  (car (rassq mode (org-scribe-story-time--mode-choices)))))))

;;; Opt-in invented calendar

(declare-function org-scribe--capture-goto-section "capture/org-scribe-capture")

;;;###autoload
(defun org-scribe-calendar-create (era day-one)
  "Add a Calendar heading to the design notebook and switch to it.
ERA names the era (\"AE\") and DAY-ONE is Day 1 written as a date of the
calendar (\"3 Thawmoon 312\").  The heading goes in the World part with an
empty month table for the writer to fill, and the project's \"# Story-time:\"
marker becomes `calendar'.  Not shipped in the templates: an empty month
table in every contemporary novel would be noise."
  (interactive
   (list (read-string (org-scribe-msg 'calendar-prompt-era))
         (read-string (org-scribe-msg 'calendar-prompt-day-one))))
  (let ((design (plist-get (org-scribe-project-structure) :design-file))
        (root (org-scribe-project-root)))
    (unless (and design (file-exists-p design))
      (user-error "%s" (org-scribe-msg 'calendar-no-notebook)))
    (with-current-buffer (find-file-noselect design)
      (when (org-scribe--find-section-heading 'calendar)
        (user-error "%s" (org-scribe-msg 'calendar-exists)))
      (org-with-wide-buffer
       (org-scribe--capture-goto-section 'calendar)
       (org-end-of-subtree t t)
       (unless (bolp) (insert "\n"))
       (insert (format ":PROPERTIES:\n:ERA: %s\n:DAY-ONE: %s\n:END:\n"
                       (string-trim era) (string-trim day-one))
               "| Month | Days |\n|-------+------|\n|       |      |\n")
       (forward-line -1)
       (org-table-align))
      (save-buffer))
    (org-scribe--project-marker-set root "Story-time" "calendar")
    (message "%s" (org-scribe-msg 'calendar-created
                                  (file-name-nondirectory design)))))

(provide 'org-scribe-story-time)

;;; org-scribe-story-time.el ends here
