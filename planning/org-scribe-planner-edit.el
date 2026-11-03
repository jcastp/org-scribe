;;; org-scribe-planner-edit.el --- Changing and recalculating plans for org-scribe-planner -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Javier Castilla
;; URL: https://codeberg.org/jcastp/org-scribe

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Recording progress, "What changed?" and its handlers, moving the start
;; date, recalculations from progress with previews, and the offer to
;; refresh a stale recalculation.
;;
;; Part of the writing planner; load it through `org-scribe-planner',
;; which requires every part in order.

;;; Code:

(require 'org)
(require 'org-element)
(require 'calendar)
(require 'cl-lib)
(require 'org-scribe-planner-engine)
(require 'org-scribe-planner-io)
(require 'org-scribe-planner-views)
(require 'org-scribe-planner-create)

(declare-function org-scribe-ews-org-count-words "counting/org-scribe-wordcount")
(declare-function org-scribe-project-structure "core/org-scribe-core")
(declare-function org-scribe-set-word-target "templates/org-scribe-project")

;;; A recalculation that went out of date

(defcustom org-scribe-planner-offer-refresh-on-new-day t
  "When non-nil, offer to recalculate a stale plan on the first save of a day.
A recalculation from progress is stale when words were written, or a goal
was missed, on the day it ran after it ran (see
`org-scribe-planner--rebase-stale').  The offer shows the outcome first
and applies nothing unless you say yes; it comes once a day, and never in
batch mode."
  :type 'boolean
  :group 'org-scribe-planner)

(defvar org-scribe-planner--refresh-offered nil
  "Where and when the stale-recalculation offer was last made, or nil.
A cons (PLAN-FILE . DATE), so an offer for one project doesn't silence
another's the same day.")

(defun org-scribe-planner--offer-refresh (plan file)
  "Offer to recalculate PLAN, saved in FILE, when its recalculation is stale.
Once a day.  Changes nothing unless the answer is yes.  Return non-nil
when the plan was recalculated."
  (let* ((today (org-scribe-planner--get-today-date))
         (offered (cons (and file (expand-file-name file)) today)))
    (when-let* (((and org-scribe-planner-offer-refresh-on-new-day
                      (not noninteractive)
                      (not (equal org-scribe-planner--refresh-offered offered))))
                (stale (org-scribe-planner--rebase-stale plan))
                (as-of (org-scribe-planner--default-recalculation-date plan))
                ((not (org-scribe-planner--recalculation-problem plan as-of))))
      (setq org-scribe-planner--refresh-offered offered)
      (let ((preview (org-scribe-planner--copy-plan plan)))
        (org-scribe-planner--rebase-keeping-end-date preview as-of)
        (when (y-or-n-p (format "%s.\n%s\nRecalculate now? "
                                (org-scribe-planner--describe-stale stale)
                                (org-scribe-planner--describe-remaining-days-change
                                 plan preview as-of)))
          (org-scribe-planner--rebase-keeping-end-date plan as-of)
          (org-scribe-planner--save-plan plan file)
          (org-scribe-planner--refresh-agenda-if-synced plan file)
          t)))))

;;; Plan Modification and Recalculation

(defun org-scribe-planner--recount-manuscript-if-live ()
  "Run a fresh word count on the manuscript buffer when it is already open.
Only fires when `org-scribe-planner-wordcount-function' is set (planner
integration active), `org-context-extended' is available (so counts are
accurate), and the manuscript file is currently visited in a live buffer.
Skips silently when any of those conditions is not met."
  (when (and org-scribe-planner-wordcount-function
             (fboundp 'org-scribe-accurate-wordcount-p)
             (org-scribe-accurate-wordcount-p))
    (when-let* ((struct (ignore-errors (org-scribe-project-structure)))
                (manuscript-file (plist-get struct :manuscript-file))
                (live-buf (find-buffer-visiting manuscript-file)))
      (with-current-buffer live-buf
        (let ((inhibit-message t))
          (org-scribe-ews-org-count-words))))))

;;;###autoload (autoload 'org-scribe-planner-update-progress "org-scribe-planner" nil t)
(defun org-scribe-planner-update-progress ()
  "Sync the word count from the manuscript into the active writing plan.
First refreshes WORDCOUNT properties on every heading in the manuscript
buffer (when it is live and `org-context-extended' is available), so the
numbers are always current rather than reflecting the last explicit count.
Reads the total via `org-scribe-planner-wordcount-function' when set
(i.e. when the planner integration is active); otherwise prompts for a
number.  The plan's words outside the manuscript are added (see
`org-scribe-planner--written-total').  Like the automatic sync
\(`org-scribe-planner-record-total'), a counted manuscript of 0 words is
ignored: an empty manuscript says nothing about the words written.
Saves the new count to the plan file, updates the in-memory plan, fires
progress hooks, and re-renders the calendar view."
  (interactive)
  (org-scribe-planner--recount-manuscript-if-live)
  (org-scribe-planner--with-current-plan (plan file)
   (let ((counted (and org-scribe-planner-wordcount-function
                       (funcall org-scribe-planner-wordcount-function))))
    (if (and (numberp counted) (zerop counted))
        (message "The manuscript has no words counted yet; nothing synced.")
    (let ((new-count (org-scribe-planner--written-total
                      plan
                      (or counted
                          (org-scribe-planner--read-non-negative-number
                           "Manuscript word count: "))
                      (org-scribe-planner--outside-base file))))
      (setf (org-scribe-plan-current-words plan) new-count)

      ;; Update the org file
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (when (re-search-forward "^\\*" nil t)
          (org-set-property "CURRENT_WORDS" (number-to-string new-count)))
        (save-buffer))

      ;; Update current plan in memory
      (setq org-scribe-planner--current-plan plan)

      ;; Run hook: nil date signals a cumulative (non-daily) update
      (run-hook-with-args 'org-scribe-planner-after-progress-update-hook plan new-count nil)

      ;; Show updated calendar
      (org-scribe-planner-show-calendar plan file)
      (org-scribe-planner--show-progress-report plan)
      (org-scribe-planner--warn-if-below-start plan))))))

;;;###autoload (autoload 'org-scribe-planner-today "org-scribe-planner" nil t)
(defun org-scribe-planner-today ()
  "Log today's word count to the active plan without extra prompts.
Pre-fills today's date, asks for a word count (using
`org-scribe-planner-wordcount-function' when set) and an optional note,
then saves immediately.  No recalculation prompt is shown.

DAILY_WORD_COUNTS stores the manuscript's *cumulative* total as of each
date (see the `org-scribe-plan-daily-word-counts' slot docstring), so
when `org-scribe-planner-wordcount-function' is set, its return value is
recorded as-is.  When it is not set, the number the user enters is a
delta (\"words written today\"), converted to a cumulative total by
adding it to the previous entry's cumulative total (0 if none)."
  (interactive)
  (org-scribe-planner--with-current-plan (plan file)
    (let* ((today (org-scribe-planner--get-today-date))
           (schedule (org-scribe-planner--generate-day-schedule plan))
           (day-info (cl-find today schedule
                              :key (lambda (d) (plist-get d :date))
                              :test #'string=))
           (is-spare (and day-info (plist-get day-info :is-spare-day)))
           (current-target (if day-info
                               (plist-get day-info :words)
                             (org-scribe-plan-daily-words plan)))
           (counted (and org-scribe-planner-wordcount-function
                         (funcall org-scribe-planner-wordcount-function)))
           ;; The manuscript's count plus the words outside it
           (total (and counted
                       (org-scribe-planner--written-total
                        plan counted (org-scribe-planner--outside-base file))))
           (cumulative
            (if total
                total
              (+ (org-scribe-planner--previous-cumulative-total
                  (org-scribe-plan-daily-word-counts plan) today
                  (org-scribe-plan-preexisting-words plan))
                 (org-scribe-planner--read-non-negative-number
                  (if is-spare
                      (format "Word count for today (%s, spare day): " today)
                    (format "Word count for today (%s): " today))
                  0))))
           (note (read-string "Note (optional): " ""))
           (existing (assoc today (org-scribe-plan-daily-word-counts plan)))
           (entry-data (list :words cumulative
                             :note note
                             :target current-target)))
      (if existing
          (setcdr existing entry-data)
        (push (cons today entry-data) (org-scribe-plan-daily-word-counts plan)))
      (setf (org-scribe-plan-current-words plan) cumulative)
      (org-scribe-planner--save-plan plan file)
      (setq org-scribe-planner--current-plan plan)
      (let ((delta (org-scribe-planner--entry-delta
                    (org-scribe-plan-daily-word-counts plan) today
                    (org-scribe-plan-preexisting-words plan))))
        (message "Logged %d words for %s%s" delta today
                 (if (string-empty-p note) "" (format " — %s" note)))
        (run-hook-with-args 'org-scribe-planner-after-progress-update-hook
                            plan delta today)))))

;;;###autoload (autoload 'org-scribe-planner-add-session-note "org-scribe-planner" nil t)
(defun org-scribe-planner-add-session-note ()
  "Add or update a note for a writing session in the active plan.
The word count for the selected day is pre-filled from whatever the
auto-tracker already recorded; the primary prompt is for the session
annotation (what you wrote, scene finished, etc.).  You can also adjust
the word count when auto-tracking was unavailable — for example when
writing on paper or in an external file.

The prompt shows and edits this day's net delta (words written that
day) for a natural entry experience, even though DAILY_WORD_COUNTS
stores cumulative totals internally (see the
`org-scribe-plan-daily-word-counts' slot docstring): the edited delta is
converted back to a cumulative total — this date's previous cumulative
total plus the (possibly edited) delta — before being saved.  Editing an
earlier day's delta this way naturally shifts the derived deltas of
every later date that still has its own absolute cumulative total."
  (interactive)
  (org-scribe-planner--with-current-plan (plan file)
    (let* ((today (org-scribe-planner--get-today-date))
           (schedule (org-scribe-planner--generate-day-schedule plan))
           (dates (mapcar (lambda (day) (plist-get day :date)) schedule))
           (date (completing-read
                  (format "Date [%s]: " today)
                  dates nil nil nil nil today))
           ;; Any text used to be accepted and saved as a ledger key
           ;; ("next tuesday"), which later broke the dashboards that
           ;; parse every key as a date.
           (_ (condition-case err
                  (org-scribe-planner--validate-date date)
                (error (user-error "%s" (error-message-string err)))))
           (daily-counts (org-scribe-plan-daily-word-counts plan))
           (existing (assoc date daily-counts))
           (existing-data (when existing (cdr existing)))
           (current-delta (or (org-scribe-planner--entry-delta daily-counts date
                             (org-scribe-plan-preexisting-words plan)) 0))
           (current-note (or (and existing-data
                                  (plist-get existing-data :note))
                             ""))
           (day-info (cl-find date schedule
                              :key (lambda (d) (plist-get d :date))
                              :test #'string=))
           (day-target (when day-info (plist-get day-info :words)))
           (note (read-string
                  (if (string-empty-p current-note)
                      (format "Note for %s (%d words tracked): " date current-delta)
                    (format "Note for %s (%d words tracked) [%s]: "
                            date current-delta current-note))
                  current-note))
           (new-delta (org-scribe-planner--read-non-negative-number
                       (format "Word count [%d]: " current-delta)
                       current-delta))
           (new-cumulative (+ (org-scribe-planner--previous-cumulative-total
                               daily-counts date
                               (org-scribe-plan-preexisting-words plan))
                              new-delta))
           (entry-data (list :words new-cumulative
                             :note note
                             :target (or day-target
                                         (org-scribe-plan-daily-words plan)))))
      (if existing
          (setcdr existing entry-data)
        (push (cons date entry-data) (org-scribe-plan-daily-word-counts plan)))
      (org-scribe-planner--save-plan plan file)
      (setq org-scribe-planner--current-plan plan)
      (message "Session saved for %s — %d words%s"
               date new-delta
               (if (string-empty-p note) "" (format ": %s" note)))
      (run-hook-with-args 'org-scribe-planner-after-progress-update-hook
                          plan new-delta date)
      (org-scribe-planner-show-calendar plan file))))

(define-obsolete-function-alias
  'org-scribe-planner-update-daily-word-count
  'org-scribe-planner-add-session-note
  "0.5.0"
  "Use `org-scribe-planner-add-session-note' instead.")

(defun org-scribe-planner--show-progress-report (plan)
  "Display a progress report for PLAN: words written, and where it stands.
Expected words run through the end of today: today's goal counts already
\(see `org-scribe-planner--status')."
  (let* ((total (org-scribe-plan-total-words plan))
         (current (org-scribe-plan-current-words plan))
         (percent (* 100.0 (/ (float current) total))))
    (message "Progress: %d/%d words (%.1f%%) | %s"
             current total percent
             (org-scribe-planner--describe-status (org-scribe-planner--status plan)))))

(defun org-scribe-planner--rebase-keeping-end-date (plan as-of)
  "Rebase PLAN on progress from AS-OF and spread the words left, end date kept.
See `org-scribe-planner--rebase-on-progress'.  When weekends have a goal
of their own, it keeps its proportion to the weekday goal: the open
weekday and weekend days share the words left at the old ratio, as
wordplanner's `recalculate_from_progress' does."
  (let* ((old-daily (org-scribe-plan-daily-words plan))
         (ratio (when (and (eq (org-scribe-plan-weekend-mode plan) 'different)
                           (numberp old-daily) (> old-daily 0))
                  (/ (float (or (org-scribe-plan-weekend-daily plan) 0)) old-daily))))
    (org-scribe-planner--rebase-on-progress plan as-of)
    (when ratio
      ;; Solve with weekends at 0, then share the open words between both
      (setf (org-scribe-plan-weekend-daily plan) 0)
      (let* ((schedule (org-scribe-planner--build-schedule plan))
             (weekday-weight (plist-get schedule :adjustable-weight))
             (spread (* (plist-get schedule :level) weekday-weight))
             (open-weekend-days
              (cl-count-if (lambda (row)
                             (let ((date (car row)) (info (cdr row)))
                               (and (not (string< date as-of))
                                    (not (plist-get info :locked))
                                    (not (plist-get info :custom))
                                    (not (plist-get info :rest))
                                    (org-scribe-planner--weekend-p plan date))))
                           (plist-get schedule :rows)))
             (denominator (+ weekday-weight (* ratio open-weekend-days))))
        (setf (org-scribe-plan-weekend-daily plan)
              (if (> denominator 0)
                  (round (/ (* ratio spread) denominator))
                0))))
    (org-scribe-planner--sync-daily plan)))

;;; Moving the start date

(defun org-scribe-planner--days-from (from to)
  "Return the number of days from date FROM to date TO (negative when earlier)."
  (round (/ (float-time (time-subtract (org-scribe-planner--parse-date to)
                                       (org-scribe-planner--parse-date from)))
            86400)))

(defun org-scribe-planner--movable-dated-days (plan)
  "Return PLAN's spare days and day goals from today on, as (DATE . LABEL)."
  (let ((today (org-scribe-planner--get-today-date)))
    (sort (append
           (mapcar (lambda (date) (cons date "spare day"))
                   (cl-remove-if (lambda (date) (string< date today))
                                 (org-scribe-plan-spare-days plan)))
           (mapcar (lambda (goal) (cons (car goal) (format "day goal of %d words" (cdr goal))))
                   (cl-remove-if (lambda (goal) (string< (car goal) today))
                                 (org-scribe-plan-custom-goals plan))))
          (lambda (a b) (string< (car a) (car b))))))

(defun org-scribe-planner--move-start-date (plan new-start &optional shift)
  "Move PLAN to start on NEW-START, as wordplanner's `move_start_date' does.
- The number of writing days is kept, so the end date moves too.
- Spare days and day goals stay on their dates; outside the new window
  they are ignored.  With SHIFT, the ones from today on move by the same
  number of days; past ones are history, and a shifted day goal never
  replaces a past one.
- Recorded words never move.  Locked goals stay on their dates: inside
  the new window they keep their goals, outside it they are ignored.
- The pre-existing words follow the plan.
- A plan that has started (locked days or words written) is rebased (see
  `org-scribe-planner--rebase-on-progress'), so its open days cover the
  words actually left; words outside the new window still count."
  (let ((offset (org-scribe-planner--days-from (org-scribe-plan-start-date plan) new-start))
        (today (org-scribe-planner--get-today-date))
        ;; Decided before the move locks anything: only a plan that had
        ;; started is rebased (wordplanner's decision 10)
        (started (or (org-scribe-plan-locked-goals plan)
                     (> (org-scribe-plan-current-words plan)
                        (or (org-scribe-plan-preexisting-words plan) 0)))))
    (when (and shift (/= offset 0))
      (setf (org-scribe-plan-spare-days plan)
            (delete-dups (mapcar (lambda (date)
                                   (if (string< date today)
                                       date
                                     (org-scribe-planner--add-days date offset)))
                                 (org-scribe-plan-spare-days plan))))
      (let* ((goals (org-scribe-plan-custom-goals plan))
             (past (cl-remove-if-not (lambda (goal) (string< (car goal) today)) goals))
             (moved (mapcar (lambda (goal)
                              (cons (org-scribe-planner--add-days (car goal) offset) (cdr goal)))
                            (cl-remove-if (lambda (goal) (string< (car goal) today)) goals))))
        ;; A past goal is history: a shifted goal never replaces it
        (setf (org-scribe-plan-custom-goals plan)
              (append (cl-remove-if (lambda (goal) (assoc (car goal) past)) moved)
                      past))))
    (setf (org-scribe-plan-start-date plan) new-start)
    (org-scribe-planner--sync-window plan)
    ;; Locked days from the new start on are history: the window grows to
    ;; hold them (moving the start earlier could end the plan before
    ;; them), and the past days that join it are locked as they are
    (let ((as-of (org-scribe-planner--default-recalculation-date plan)))
      (org-scribe-planner--lock-past-goals plan as-of)
      (while (< (org-scribe-planner--writing-days plan)
                (org-scribe-planner--min-writing-days plan))
        (org-scribe-planner--set-writing-days
         plan (org-scribe-planner--min-writing-days plan))
        (org-scribe-planner--lock-past-goals plan as-of)))
    (when started
      (org-scribe-planner--rebase-on-progress
       plan (org-scribe-planner--default-recalculation-date plan)))
    (org-scribe-planner--sync-daily plan)
    offset))

(defun org-scribe-planner--in-window-p (plan date)
  "Return non-nil when DATE falls inside PLAN's start and end dates."
  (and (org-scribe-plan-end-date plan)
       (not (string< date (org-scribe-plan-start-date plan)))
       (not (string< (org-scribe-plan-end-date plan) date))))

(defun org-scribe-planner--start-move-warnings (before moved)
  "Return what to know before a start-date move is applied, as strings.
BEFORE is the plan before the move, MOVED a copy with the move applied."
  (let* ((today (org-scribe-planner--get-today-date))
         (written (org-scribe-planner--words-by-date moved))
         (outside nil)
         (warnings nil))
    (maphash (lambda (date words)
               (when (and (> words 0) (not (org-scribe-planner--in-window-p moved date)))
                 (push (cons date words) outside)))
             written)
    (when outside
      (push (format "%d day(s) with words written (%d words) are outside the new plan. Recorded words never move: they still count as written, but not toward any day's goal."
                    (length outside) (apply #'+ (mapcar #'cdr outside)))
            warnings))
    (let ((joining (cl-count-if
                    (lambda (day)
                      (let ((date (plist-get day :date)))
                        (and (string< date today)
                             (not (org-scribe-planner--in-window-p before date)))))
                    (org-scribe-planner--generate-day-schedule moved))))
      (when (> joining 0)
        (push (format "%d day(s) before today join the plan. They are past, so their goals are locked as they are now."
                      joining)
              warnings)))
    (when (cl-some (lambda (lock)
                     (and (org-scribe-planner--in-window-p before (car lock))
                          (not (org-scribe-planner--in-window-p moved (car lock)))))
                   (org-scribe-plan-locked-goals before))
      (push "Some past (locked) days are now outside the plan; their goals no longer count."
            warnings))
    (when (or (org-scribe-plan-locked-goals moved)
              (> (org-scribe-plan-current-words moved)
                 (or (org-scribe-plan-preexisting-words moved) 0)))
      (push (format "The plan has started: the open days are recalculated to cover the %d words left."
                    (max 0 (- (org-scribe-plan-total-words moved)
                              (org-scribe-plan-current-words moved))))
            warnings))
    (nreverse warnings)))

(defun org-scribe-planner--recalculation-problem (plan as-of)
  "Return why PLAN can't be recalculated from AS-OF, as a message, or nil.
The goal may already be reached, the plan may be over, or no writing day
may be left from AS-OF on."
  (let ((remaining (- (org-scribe-plan-total-words plan)
                      (org-scribe-plan-current-words plan)))
        (end-date (org-scribe-plan-end-date plan)))
    (cond
     ((<= remaining 0)
      (format "Goal already reached: %d of %d words. Plan unchanged."
              (org-scribe-plan-current-words plan) (org-scribe-plan-total-words plan)))
     ((or (null end-date) (string< end-date as-of))
      (format "The plan ended on %s; %d words left. Use the end-date adjustment to extend it."
              end-date remaining))
     ((zerop (cl-count-if (lambda (day)
                            (and (not (plist-get day :is-spare-day))
                                 (not (string< (plist-get day :date) as-of))))
                          (org-scribe-planner--generate-day-schedule plan)))
      (format "No writing days left from %s; %d words left." as-of remaining)))))

(defun org-scribe-planner-recalculate-remaining-days (plan file)
  "Spread the words PLAN actually has left over its remaining writing days.
Keeps the end date and the total goal; changes the daily target.  Days
before the recalculation date (today, or tomorrow if today already has a
word count) are locked with the goals they had, and the open days cover
the words really left (see `org-scribe-planner--rebase-on-progress').
FILE is the path where the plan is saved."
  (let* ((as-of (org-scribe-planner--default-recalculation-date plan))
         (problem (org-scribe-planner--recalculation-problem plan as-of)))
    (if problem
        (message "%s" problem)
      (let ((old-daily (org-scribe-plan-daily-words plan))
            (remaining (- (org-scribe-plan-total-words plan)
                          (org-scribe-plan-current-words plan))))
        (org-scribe-planner--rebase-keeping-end-date plan as-of)
        (org-scribe-planner--save-plan plan file)
        (org-scribe-planner--refresh-agenda-if-synced plan file)
        (org-scribe-planner-show-calendar plan file)
        (message "Daily target %d → %d words from %s: %d words left, end date %s unchanged"
                 old-daily (org-scribe-plan-daily-words plan) as-of remaining
                 (org-scribe-plan-end-date plan))))))

(defun org-scribe-planner-recalculate-end-date (plan file)
  "Move PLAN's end date so the words left fit at its current daily target.
Keeps the daily target and the total goal: finishes early when ahead,
extends the deadline when behind.  Days before the recalculation date
\(today, or tomorrow when today already has a word count) are locked and
the plan is rebased on the words actually written (see
`org-scribe-planner--rebase-on-progress'); then it gets the fewest
writing days that fit the rest, spare days skipped.  FILE is the path
where the plan is saved."
  (let ((remaining-words (- (org-scribe-plan-total-words plan)
                            (org-scribe-plan-current-words plan)))
        (daily-words (org-scribe-plan-daily-words plan))
        (old-end-date (org-scribe-plan-end-date plan)))
    (cond
     ((<= remaining-words 0)
      (message "Goal already reached: %d of %d words. End date unchanged."
               (org-scribe-plan-current-words plan) (org-scribe-plan-total-words plan)))
     ((not (and (numberp daily-words) (> daily-words 0)))
      (message "The plan has no daily target to keep. Use the recalculate command instead."))
     (t
      (org-scribe-planner--rebase-on-progress
       plan (org-scribe-planner--default-recalculation-date plan))
      (org-scribe-planner--calculate-time plan daily-words)
      (let* ((new-end-date (org-scribe-plan-end-date plan))
             (shift (1- (org-scribe-planner--days-between old-end-date new-end-date))))
        (org-scribe-planner--save-plan plan file)
        (org-scribe-planner--refresh-agenda-if-synced plan file)
        (org-scribe-planner-show-calendar plan file)
        (message "New end date %s (was %s): %s, keeping %d words/day for the %d words left"
                 new-end-date old-end-date
                 (cond ((< shift 0) (format "finishing %d day%s early" (- shift)
                                            (if (= shift -1) "" "s")))
                       ((> shift 0) (format "extended by %d day%s" shift
                                            (if (= shift 1) "" "s")))
                       (t "unchanged"))
                 daily-words remaining-words))))))

;;; Changing a plan, with previews

(defun org-scribe-planner--copy-plan (plan)
  "Return a copy of PLAN that can be changed without touching PLAN.
List slots are copied deeply; the heading marker is shared."
  (let ((copy (copy-org-scribe-plan plan)))
    (setf (org-scribe-plan-spare-days copy) (copy-tree (org-scribe-plan-spare-days plan))
          (org-scribe-plan-locked-goals copy) (copy-tree (org-scribe-plan-locked-goals plan))
          (org-scribe-plan-custom-goals copy) (copy-tree (org-scribe-plan-custom-goals plan))
          (org-scribe-plan-outside-files copy) (copy-tree (org-scribe-plan-outside-files plan))
          (org-scribe-plan-pattern-config copy)
          (copy-tree (org-scribe-plan-pattern-config plan))
          (org-scribe-plan-weekend-days copy)
          (copy-sequence (org-scribe-plan-weekend-days plan))
          (org-scribe-plan-daily-word-counts copy)
          (copy-tree (org-scribe-plan-daily-word-counts plan)))
    copy))

(defun org-scribe-planner--describe-outcome (plan)
  "Describe PLAN's shape in one line: end date, daily target and total."
  (format "ends %s, %d words/day, total %d words"
          (or (org-scribe-plan-end-date plan) "-")
          (or (org-scribe-plan-daily-words plan) 0)
          (or (org-scribe-plan-total-words plan) 0)))

(defun org-scribe-planner--choose-change (plan question options &optional before describe)
  "Preview each of OPTIONS on a copy of PLAN, ask QUESTION, apply the choice.
OPTIONS is a list of (LABEL . FUNCTION); FUNCTION changes the plan it is
given.  Each candidate shows the outcome of its option, as DESCRIBE (a
function of a plan; default `org-scribe-planner--describe-outcome') puts
it; an option that fails on its preview is offered with the reason and
can't be chosen.  The first option is the default.  BEFORE, a plan copy
taken before the change being absorbed, is shown as the starting point.
Returns the chosen LABEL."
  (let* ((describe (or describe #'org-scribe-planner--describe-outcome))
         (previews
          (mapcar (lambda (option)
                    (let ((copy (org-scribe-planner--copy-plan plan)))
                      (condition-case err
                          (progn (funcall (cdr option) copy)
                                 (list option (funcall describe copy) t))
                        (error (list option (format "not possible: %s"
                                                    (error-message-string err))
                                     nil)))))
                  options))
         (candidates (mapcar (lambda (preview)
                               (cons (format "%s → %s" (car (nth 0 preview)) (nth 1 preview))
                                     preview))
                             previews))
         (possible (cl-remove-if-not (lambda (c) (nth 2 (cdr c))) candidates)))
    (unless possible
      (user-error "No option is possible: %s" (nth 1 (cdr (car candidates)))))
    (let* ((prompt (if before
                       (format "%s (now: %s) "
                               question (funcall describe before))
                     (format "%s " question)))
           (answer (completing-read prompt (mapcar #'car candidates)
                                    (lambda (c) (nth 2 (cdr (if (consp c) c
                                                                (assoc c candidates)))))
                                    t nil nil (car (car possible))))
           (option (car (cdr (assoc answer candidates)))))
      (funcall (cdr option) plan)
      (message "Plan %s" (funcall describe plan))
      (car option))))

(defconst org-scribe-planner--adjustments '(keep-end move-end change-total)
  "The three ways a plan can absorb a change to its days (decision 1).")

(defun org-scribe-planner--apply-adjustment (plan before adjustment)
  "Rebalance PLAN after a change, in one of three ways.
BEFORE is a copy of PLAN taken before the change.  ADJUSTMENT is one of
`org-scribe-planner--adjustments':
  `keep-end'     keep the end date and the total; the daily target changes;
  `move-end'     keep the daily target and the total; the end date moves;
  `change-total' keep the end date and the daily target; the total changes.
If no open day is left (every day past or spare), the total becomes what
the fixed days add up to."
  (let ((old-end (org-scribe-plan-end-date before))
        (old-daily (org-scribe-plan-daily-words before)))
    (pcase adjustment
      ('keep-end
       (setf (org-scribe-plan-total-words plan) (org-scribe-plan-total-words before))
       (org-scribe-planner--keep-end-date plan old-end))
      ('move-end
       (setf (org-scribe-plan-total-words plan) (org-scribe-plan-total-words before))
       (org-scribe-planner--calculate-time plan old-daily))
      ('change-total
       (org-scribe-planner--keep-end-date plan old-end)
       (setf (org-scribe-plan-total-words plan)
             (round (org-scribe-planner--total-for-daily plan old-daily)))
       (org-scribe-planner--sync-daily plan))
      (_ (error "Unknown plan adjustment: %s" adjustment)))
    (when (zerop (plist-get (org-scribe-planner--build-schedule plan) :adjustable-weight))
      (setf (org-scribe-plan-total-words plan)
            (round (org-scribe-planner--total-for-daily plan 0))))))

(defun org-scribe-planner--schedule-goals (plan)
  "Return PLAN's (DATE . GOAL) pairs, to tell whether a change moved any goal."
  (mapcar (lambda (day) (cons (plist-get day :date) (plist-get day :words)))
          (org-scribe-planner--generate-day-schedule plan)))

(defun org-scribe-planner--offer-adjustment (plan before &optional question)
  "Ask how PLAN absorbs a change to its days, previewing each way; apply it.
BEFORE is a copy of PLAN taken before the change (spare days added or
removed, and later weekend or day-goal changes).  Does nothing when the
change didn't move any goal.  The three ways are those of
`org-scribe-planner--apply-adjustment'; moving the end date isn't offered
when the daily target was 0.  Keeping the end date is the default.
QUESTION replaces the default question.  Returns the chosen adjustment,
or nil."
  (unless (equal (org-scribe-planner--schedule-goals plan)
                 (org-scribe-planner--schedule-goals before))
    (let* ((old-daily (org-scribe-plan-daily-words before))
           (labels `((keep-end . "Keep the end date (change the daily target)")
                     (move-end . ,(format "Move the end date (keep %d words/day)" old-daily))
                     (change-total . "Keep the end date and the daily target (change the total)")))
           (options (delq nil
                          (mapcar (lambda (adjustment)
                                    (unless (and (eq adjustment 'move-end)
                                                 (not (> (or old-daily 0) 0)))
                                      (cons (cdr (assq adjustment labels))
                                            (lambda (p)
                                              (org-scribe-planner--apply-adjustment
                                               p before adjustment)))))
                                  org-scribe-planner--adjustments)))
           (chosen (org-scribe-planner--choose-change
                    plan (or question "This changes the plan's days. How should it adjust?")
                    options before)))
      (car (rassoc chosen labels)))))

(defun org-scribe-planner--refresh-agenda-if-synced (plan file)
  "Regenerate PLAN's agenda entries in FILE when it has been synced before.
A plan synced with `org-scribe-planner-sync-agenda' has an \"** Agenda
Entries\" subtree; after a change its TODOs would show old goals."
  (when (and file (file-exists-p file)
             (with-temp-buffer
               (insert-file-contents file)
               (goto-char (point-min))
               (re-search-forward "^\\*\\* Agenda Entries" nil t)))
    (org-scribe-planner--add-agenda-entries plan file)))

(defun org-scribe-planner--read-writing-days (plan prompt)
  "Read a plan length with PROMPT; return it as PLAN's writing days.
The answer is a number of calendar days from the start date, or a start
and end date (which also moves PLAN's start date); spare days inside are
taken out.  Refuses fewer days than PLAN's locked (past) writing days."
  (let* ((days-result (org-scribe-planner--read-days prompt (org-scribe-plan-days plan)))
         (writing-days
          (if (listp days-result)
              (progn
                (setf (org-scribe-plan-start-date plan) (plist-get days-result :start-date))
                (org-scribe-planner--writing-days-through
                 plan (plist-get days-result :end-date)))
            (- days-result (org-scribe-planner--spare-days-in-window plan days-result))))
         (min-days (max 1 (org-scribe-planner--min-writing-days plan))))
    (when (< writing-days min-days)
      (user-error "The plan already has %d past writing days; it needs at least %d"
                  (org-scribe-planner--min-writing-days plan) min-days))
    writing-days))

;;; Recalculating from progress, with previews

(defun org-scribe-planner--describe-shift (old-end new-end)
  "Describe moving an end date from OLD-END to NEW-END in a few words."
  (let ((shift (1- (org-scribe-planner--days-between old-end new-end))))
    (cond ((< shift 0) (format "finishing %d day%s early" (- shift) (if (= shift -1) "" "s")))
          ((> shift 0) (format "extended by %d day%s" shift (if (= shift 1) "" "s")))
          (t "unchanged"))))

(defun org-scribe-planner--describe-remaining-days-change (plan preview as-of)
  "Describe what keeping PLAN's end date does, as PREVIEW shows, from AS-OF."
  (let* ((old-daily (org-scribe-plan-daily-words plan))
         (new-daily (org-scribe-plan-daily-words preview))
         (change (- new-daily old-daily))
         (open (cl-remove-if (lambda (day) (string< (plist-get day :date) as-of))
                             (org-scribe-planner--generate-day-schedule preview)))
         (open-days (cl-count-if-not (lambda (day) (plist-get day :is-spare-day)) open))
         ;; The goals the target is worked out from, so the two always agree
         (words-left (apply #'+ (mapcar (lambda (day) (plist-get day :words)) open)))
         (newly-locked (- (length (org-scribe-plan-locked-goals preview))
                          (length (org-scribe-plan-locked-goals plan)))))
    (format "From %s: %d words left over %d writing days.\nDaily target %d → %d words (%s%d, %s%.1f%%)%s. End date %s unchanged."
            as-of words-left open-days old-daily new-daily
            (if (>= change 0) "+" "") change
            (if (>= change 0) "+" "")
            (if (> old-daily 0) (* 100.0 (/ (float change) old-daily)) 0.0)
            (if (> newly-locked 0)
                (format ". %d more day%s locked, through %s" newly-locked
                        (if (= newly-locked 1) "" "s")
                        (org-scribe-planner--add-days as-of -1))
              "")
            (org-scribe-plan-end-date preview))))

;;;###autoload (autoload 'org-scribe-planner-adjust-remaining-plan "org-scribe-planner" nil t)
(defun org-scribe-planner-adjust-remaining-plan ()
  "Spread the words you actually have left over the remaining writing days.
Keeps your end date and total word goal; changes the daily target.  Past
days keep their goals; the change starts today, or tomorrow if you have
already written today.  Shows what changes and asks before applying it.
The counterpart of `org-scribe-planner-adjust-end-date'."
  (interactive)
  (org-scribe-planner--with-current-plan (plan file)
    (let* ((as-of (org-scribe-planner--default-recalculation-date plan))
           (problem (org-scribe-planner--recalculation-problem plan as-of)))
      (if problem
          (message "%s" problem)
        (let ((preview (org-scribe-planner--copy-plan plan)))
          (org-scribe-planner--rebase-keeping-end-date preview as-of)
          (if (y-or-n-p (concat (org-scribe-planner--describe-remaining-days-change
                                 plan preview as-of)
                                "\nApply? "))
              (org-scribe-planner-recalculate-remaining-days plan file)
            (message "Plan left unchanged.")))))))

;;;###autoload (autoload 'org-scribe-planner-adjust-end-date "org-scribe-planner" nil t)
(defun org-scribe-planner-adjust-end-date ()
  "Move the end date so the words left fit at the current daily target.
Finishes early when you are ahead and extends the deadline when you are
behind, keeping your daily target and total word goal.  Past days keep
their goals.  Shows the new end date and asks before applying it.  The
counterpart of `org-scribe-planner-adjust-remaining-plan', which keeps
the end date and changes the daily target instead."
  (interactive)
  (org-scribe-planner--with-current-plan (plan file)
    (let ((remaining (- (org-scribe-plan-total-words plan)
                        (org-scribe-plan-current-words plan)))
          (daily (org-scribe-plan-daily-words plan)))
      (cond
       ((<= remaining 0)
        (message "Goal already reached: %d of %d words. End date unchanged."
                 (org-scribe-plan-current-words plan) (org-scribe-plan-total-words plan)))
       ((not (and (numberp daily) (> daily 0)))
        (message "The plan has no daily target to keep. Use the recalculate command instead."))
       (t
        (let ((preview (org-scribe-planner--copy-plan plan)))
          (org-scribe-planner--rebase-on-progress
           preview (org-scribe-planner--default-recalculation-date plan))
          (org-scribe-planner--calculate-time preview daily)
          (if (y-or-n-p
               (format "Daily target %d words kept. Writing days %d → %d.\nEnd date %s → %s: %s. Apply? "
                       daily
                       (org-scribe-planner--writing-days plan)
                       (org-scribe-planner--writing-days preview)
                       (org-scribe-plan-end-date plan) (org-scribe-plan-end-date preview)
                       (org-scribe-planner--describe-shift
                        (org-scribe-plan-end-date plan) (org-scribe-plan-end-date preview))))
              (org-scribe-planner-recalculate-end-date plan file)
            (message "Plan left unchanged."))))))))

;;;###autoload (autoload 'org-scribe-planner-check-progress "org-scribe-planner" nil t)
(defun org-scribe-planner-check-progress ()
  "Show where the active plan stands; when ahead or behind, offer to adjust it.
Ahead, you can reduce the daily target (keeping the end date) or finish
early (keeping the daily target); behind, increase the daily target or
extend the deadline.  Every option shows its outcome first, and keeping
the plan as it is is the default.  Expected words run through yesterday:
today's goal isn't due until the day is over."
  (interactive)
  (org-scribe-planner--with-current-plan (plan file)
    (let ((status (org-scribe-planner--status plan 'yesterday))
          (stale (ignore-errors (org-scribe-planner--rebase-stale plan))))
      (cond
       ((null status)
        (message "The plan has no schedule yet."))
       ((eq (plist-get status :status) 'on-track)
        (message "On track: %d words written, as expected by the end of yesterday.%s"
                 (plist-get status :actual)
                 (if stale
                     (format " Out of date: %s; adjust the remaining days to count them."
                             (org-scribe-planner--describe-stale stale))
                   "")))
       (t
        (let* ((ahead (eq (plist-get status :status) 'ahead))
               (as-of (org-scribe-planner--default-recalculation-date plan))
               (chosen
                (org-scribe-planner--choose-change
                 plan
                 (format "%s (expected %d, written %d)%s. Adjust the plan?"
                         (org-scribe-planner--describe-status status)
                         (plist-get status :expected) (plist-get status :actual)
                         (if stale
                             (format ". Out of date: %s"
                                     (org-scribe-planner--describe-stale stale))
                           ""))
                 `(("Keep the plan as it is" . ignore)
                   (,(if ahead "Reduce the daily target (keep the end date)"
                       "Increase the daily target (keep the end date)")
                    . ,(lambda (p)
                         (let ((problem (org-scribe-planner--recalculation-problem p as-of)))
                           (when problem (user-error "%s" problem)))
                         (org-scribe-planner--rebase-keeping-end-date p as-of)))
                   (,(if ahead "Finish early (keep the daily target)"
                       "Extend the deadline (keep the daily target)")
                    . ,(lambda (p)
                         (org-scribe-planner--rebase-on-progress p as-of)
                         (org-scribe-planner--calculate-time
                          p (org-scribe-plan-daily-words p))))))))
          (unless (string= chosen "Keep the plan as it is")
            (org-scribe-planner--save-plan plan file)
            (org-scribe-planner--refresh-agenda-if-synced plan file)
            (setq org-scribe-planner--current-plan plan)
            (org-scribe-planner-show-calendar plan file))))))))

(defconst org-scribe-planner--day-names
  '(("Sunday" . 0) ("Monday" . 1) ("Tuesday" . 2) ("Wednesday" . 3)
    ("Thursday" . 4) ("Friday" . 5) ("Saturday" . 6))
  "Day names and their numbers, as `calendar-day-of-week' gives them.")

(defun org-scribe-planner--read-pattern ()
  "Ask for a writing pattern and its intensity; return (PATTERN . CONFIG)."
  (let* ((choice (org-scribe-planner--choose
                  "Writing pattern: "
                  (mapcar #'cdr org-scribe-planner--patterns)))
         (pattern (car (rassoc choice org-scribe-planner--patterns))))
    (if (eq pattern 'flat)
        (cons 'flat nil)
      (let* ((intensity (intern (completing-read
                                 "How much variation? " '("moderate" "mild" "extreme")
                                 nil t nil nil "moderate")))
             (config (copy-sequence
                      (cdr (assq intensity (cdr (assq pattern
                                                      org-scribe-planner--pattern-intensities)))))))
        ;; The random pattern keeps its seed, so its goals stay the same
        (when (eq pattern 'random)
          (setq config (append config (list :seed (1+ (random 999999))))))
        (cons pattern config)))))

(defun org-scribe-planner--read-weekend-days (plan)
  "Ask which days count as the weekend in PLAN; return their numbers."
  (let* ((current (mapcar (lambda (n) (car (rassq n org-scribe-planner--day-names)))
                          (or (org-scribe-plan-weekend-days plan) '(6 0))))
         (names (completing-read-multiple
                 (format "Weekend days (default %s): " (string-join current ","))
                 (mapcar #'car org-scribe-planner--day-names) nil t nil nil
                 (string-join current ","))))
    (or (delq nil (mapcar (lambda (name) (cdr (assoc name org-scribe-planner--day-names)))
                          names))
        '(6 0))))

(defun org-scribe-planner--custom-goal-problem (plan date)
  "Return why DATE can't get a day goal in PLAN, as a message, or nil.
Past days are locked; days outside the plan have no goal; spare days and
weekends off are rest days by rule."
  (let ((start (org-scribe-plan-start-date plan))
        (end (org-scribe-plan-end-date plan)))
    (cond
     ((string< date (org-scribe-planner--get-today-date))
      "Days before today are locked: their goals can't change.")
     ((or (null end) (string< date start) (string< end date))
      (format "%s is outside the plan (%s to %s)." date start end))
     ((member date (org-scribe-plan-spare-days plan))
      (format "%s is a spare day. Remove it from the spare days first." date))
     ((and (eq (org-scribe-plan-weekend-mode plan) 'rest)
           (org-scribe-planner--weekend-p plan date))
      (format "%s falls on a weekend, and weekends are days off. Change the weekend setting first."
              date)))))

(defconst org-scribe-planner--day-goal-question
  "This changes the day's goal. How should the rest of the plan adjust?"
  "The adjustment question asked after a day goal is set or removed.")

(defun org-scribe-planner--change-total (plan total)
  "Change PLAN's total to TOTAL, asking which of the days or daily target to keep.
Each option is previewed (`org-scribe-planner--choose-change').  Return
the chosen label."
  (org-scribe-planner--choose-change
   plan (format "Total %d words: keep" total)
   `(("Writing days (the daily target changes)"
      . ,(lambda (p)
           (setf (org-scribe-plan-total-words p) total)
           (org-scribe-planner--sync-daily p)))
     (,(format "Daily target (the end date moves)")
      . ,(lambda (p)
           (let ((daily (org-scribe-plan-daily-words p)))
             (setf (org-scribe-plan-total-words p) total)
             (org-scribe-planner--calculate-time p daily)))))))

(defun org-scribe-planner-set-total (total)
  "Change the active plan's total to TOTAL, previewing and saving it.
This is the entry point for org-scribe, which offers it when the
project's own word target changes.  Return non-nil when a change was
applied."
  (let ((done nil))
    (org-scribe-planner--with-current-plan (plan file)
      (let ((backup (org-scribe-planner--copy-plan plan))
            (finished nil))
        (unwind-protect
            (progn
              (org-scribe-planner--change-total plan total)
              (org-scribe-planner--save-plan plan file)
              (org-scribe-planner--refresh-agenda-if-synced plan file)
              (setq org-scribe-planner--current-plan plan)
              (setq finished t done t))
          (unless finished
            (setq org-scribe-planner--current-plan backup)))))
    done))

(defun org-scribe-planner-set-title (title)
  "Rename the active plan to TITLE and save it.
This is the entry point for org-scribe, which offers it when the
project's title changes.  Only the plan's title (the level-1 heading
of its file) changes.  Return non-nil when the plan was renamed."
  (let ((done nil))
    (org-scribe-planner--with-current-plan (plan file)
      (let ((old (org-scribe-plan-title plan)))
        (unwind-protect
            (progn
              (setf (org-scribe-plan-title plan) title)
              (org-scribe-planner--save-plan plan file)
              (setq org-scribe-planner--current-plan plan)
              (setq done t))
          (unless done
            (setf (org-scribe-plan-title plan) old)))))
    done))

(defvar org-scribe--word-target-offer-planner)

(defun org-scribe-planner--offer-project-total (plan)
  "Offer to give the project the total PLAN now has.
Asked only inside an org-scribe project that records a word target
different from PLAN's total; a project with no target is not asked
\(nothing is migrated).  Answering yes rescales the project's parts too,
without asking the planner back."
  (let ((project-target (org-scribe-planner--project-word-target))
        (total (org-scribe-plan-total-words plan)))
    (when (and project-target total
               (fboundp 'org-scribe-set-word-target)
               (/= project-target total)
               (y-or-n-p (format "The project's target is %d words. Change it to %d and rescale its parts too? "
                                 project-target total)))
      (condition-case err
          (let ((org-scribe--word-target-offer-planner nil))
            (org-scribe-set-word-target (round total) 'keep t))
        (user-error (message "%s" (cadr err)))))))

;;;; "What changed?": one handler per option

(defun org-scribe-planner--change-total-words (plan _file)
  "Ask for PLAN's new total words and what to keep."
  (let ((total (org-scribe-planner--read-positive-number
                "New total words: " (org-scribe-plan-total-words plan))))
    (org-scribe-planner--change-total plan total)
    (org-scribe-planner--offer-project-total plan)))

(defun org-scribe-planner--change-daily-words (plan _file)
  "Ask for PLAN's new daily words and whether the total or the length moves."
  (let ((daily (org-scribe-planner--read-positive-number
                "New daily words: " (org-scribe-plan-daily-words plan))))
    (org-scribe-planner--choose-change
     plan (format "%d words/day: keep" daily)
     `(("Total (the end date moves)"
        . ,(lambda (p) (org-scribe-planner--calculate-time p daily)))
       ("Writing days (the total changes)"
        . ,(lambda (p) (org-scribe-planner--set-total-keeping-daily p daily)))))))

(defun org-scribe-planner--change-days-available (plan _file)
  "Ask for PLAN's new number of writing days and whether the total or daily moves."
  (let* ((target (org-scribe-planner--copy-plan plan))
         (writing-days (org-scribe-planner--read-writing-days
                        target "New days available: "))
         (start-date (org-scribe-plan-start-date target)))
    (org-scribe-planner--choose-change
     plan (format "%d writing days: keep" writing-days)
     `(("Total (the daily target changes)"
        . ,(lambda (p)
             (setf (org-scribe-plan-start-date p) start-date)
             (org-scribe-planner--set-writing-days p writing-days)))
       ("Daily target (the total changes)"
        . ,(lambda (p)
             (let ((daily (org-scribe-plan-daily-words p)))
               (setf (org-scribe-plan-start-date p) start-date)
               (org-scribe-planner--set-writing-days p writing-days)
               (org-scribe-planner--set-total-keeping-daily p daily))))))))

(defun org-scribe-planner--change-two-values (plan _file)
  "Ask for two of PLAN's total, daily words and days, and calculate the third."
  (let* ((third (org-scribe-planner--choose
                 "Calculate: "
                 '("Total words (from daily words × days)"
                   "Daily words (from total ÷ days)"
                   "Days (from total ÷ daily words)")))
         (target (org-scribe-planner--copy-plan plan))
         (change
          (pcase third
            ("Total words (from daily words × days)"
             (let* ((daily (org-scribe-planner--read-positive-number
                            "Daily words: " (org-scribe-plan-daily-words plan)))
                    (writing-days (org-scribe-planner--read-writing-days
                                   target "Days available: "))
                    (start-date (org-scribe-plan-start-date target)))
               (lambda (p)
                 (setf (org-scribe-plan-start-date p) start-date)
                 (org-scribe-planner--set-writing-days p writing-days)
                 (org-scribe-planner--set-total-keeping-daily p daily))))
            ("Daily words (from total ÷ days)"
             (let* ((total (org-scribe-planner--read-positive-number
                            "Total words: " (org-scribe-plan-total-words plan)))
                    (writing-days (org-scribe-planner--read-writing-days
                                   target "Days available: "))
                    (start-date (org-scribe-plan-start-date target)))
               (lambda (p)
                 (setf (org-scribe-plan-start-date p) start-date
                       (org-scribe-plan-total-words p) total)
                 (org-scribe-planner--set-writing-days p writing-days))))
            (_
             (let ((total (org-scribe-planner--read-positive-number
                           "Total words: " (org-scribe-plan-total-words plan)))
                   (daily (org-scribe-planner--read-positive-number
                           "Daily words: " (org-scribe-plan-daily-words plan))))
               (lambda (p)
                 (setf (org-scribe-plan-total-words p) total)
                 (org-scribe-planner--calculate-time p daily)))))))
    (org-scribe-planner--choose-change
     plan "Apply?" `(("Apply" . ,change)))))

(defun org-scribe-planner--change-start-date (plan _file)
  "Ask for PLAN's new start date and what happens to its dated days."
  (let* ((new-start (org-scribe-planner--read-date
                     "New start date (YYYY-MM-DD)" (org-scribe-planner--get-today-date)))
         (offset (org-scribe-planner--days-from (org-scribe-plan-start-date plan) new-start))
         (before (org-scribe-planner--copy-plan plan))
         (movable (org-scribe-planner--movable-dated-days plan))
         (direction (if (> offset 0) "later" "earlier"))
         (describe (lambda (p) (format "%s to %s, %d words/day"
                                       (org-scribe-plan-start-date p)
                                       (org-scribe-plan-end-date p)
                                       (org-scribe-plan-daily-words p)))))
    (when (= offset 0)
      (user-error "%s is already the start date" new-start))
    (if (null movable)
        (org-scribe-planner--choose-change
         plan (format "Move the start %d day(s) %s?" (abs offset) direction)
         `(("Move the start date" . ,(lambda (p) (org-scribe-planner--move-start-date p new-start))))
         before describe)
      ;; Which spare days and day goals would end up outside, if kept
      (let* ((kept (org-scribe-planner--copy-plan plan))
             (outside (progn (org-scribe-planner--move-start-date kept new-start)
                             (cl-remove-if (lambda (item)
                                             (org-scribe-planner--in-window-p kept (car item)))
                                           movable))))
        (org-scribe-planner--choose-change
         plan
         (format "Moving the start %d day(s) %s. %s Your spare days and day goals from today on:"
                 (abs offset) direction
                 (if outside
                     (format "Kept on their dates, %d would fall outside the plan (%s)."
                             (length outside)
                             (mapconcat (lambda (item) (format "%s %s" (car item) (cdr item)))
                                        outside ", "))
                   "Kept on their dates, all stay inside the plan."))
         `(("Keep them on their dates"
            . ,(lambda (p) (org-scribe-planner--move-start-date p new-start)))
           (,(format "Shift them all %d day(s) %s%s" (abs offset) direction
                     (if (zerop (mod offset 7)) "" " (they land on other weekdays)"))
            . ,(lambda (p) (org-scribe-planner--move-start-date p new-start t))))
         before describe)))
    (let ((warnings (org-scribe-planner--start-move-warnings before plan)))
      (when (and warnings
                 (not (y-or-n-p (format "%s\nApply the new start date? "
                                        (string-join warnings "\n")))))
        (user-error "Start date not changed")))))

(defun org-scribe-planner--change-outside-words (plan file)
  "Change the words PLAN, saved in FILE, records outside the manuscript."
  (let* ((base (org-scribe-planner--outside-base file))
         (target (org-scribe-planner--copy-plan plan))
         (typed (or (org-scribe-plan-outside-words plan) 0))
         (files (org-scribe-plan-outside-files plan))
         (action (org-scribe-planner--choose
                  (format "Words outside the manuscript (%s): "
                          (org-scribe-planner--describe-outside plan base))
                  (append (list (format "Set the typed number (now %d)" typed)
                                "Add a file")
                          (when files '("Stop counting a file"))))))
    (cond
     ((string-prefix-p "Set" action)
      (setf (org-scribe-plan-outside-words target)
            (org-scribe-planner--read-non-negative-number
             "Words written elsewhere, not in the manuscript: " typed)))
     ((string= action "Add a file")
      (unless (org-scribe-planner--add-outside-file target base)
        (user-error "No file added")))
     (t
      (let ((path (org-scribe-planner--choose "Stop counting: "
                                              (mapcar (lambda (e) (plist-get e :path)) files))))
        (setf (org-scribe-plan-outside-files target)
              (cl-remove path (org-scribe-plan-outside-files target)
                         :key (lambda (e) (plist-get e :path)) :test #'equal)))))
    ;; Recorded only: the ledger never counts these words
    (setf (org-scribe-plan-outside-words plan) (org-scribe-plan-outside-words target)
          (org-scribe-plan-outside-files plan) (copy-tree (org-scribe-plan-outside-files target)))
    (message "Words outside the manuscript: %s (shown, not counted)"
             (org-scribe-planner--describe-outside plan base))))

(defun org-scribe-planner--change-pattern (plan _file)
  "Ask for PLAN's writing pattern."
  (let ((choice (org-scribe-planner--read-pattern)))
    (setf (org-scribe-plan-pattern plan) (car choice))
    (setf (org-scribe-plan-pattern-config plan) (cdr choice))
    (org-scribe-planner--sync-daily plan)
    (message "Pattern: %s. Past days keep their goals."
             (org-scribe-planner--describe-pattern plan))))

(defun org-scribe-planner--change-weekends (plan _file)
  "Ask how PLAN treats weekends, then how the plan adjusts."
  (let* ((before (org-scribe-planner--copy-plan plan))
         (mode (org-scribe-planner--choose
                "Weekends: "
                '("Same as other days" "A goal of their own" "No writing"))))
    (pcase mode
      ("Same as other days"
       (setf (org-scribe-plan-weekend-mode plan) 'same))
      ("A goal of their own"
       (setf (org-scribe-plan-weekend-mode plan) 'different)
       (setf (org-scribe-plan-weekend-daily plan)
             (org-scribe-planner--read-non-negative-number
              "Weekend goal (words/day): "
              (if (eq (org-scribe-plan-weekend-mode before) 'different)
                  (org-scribe-plan-weekend-daily before)
                (round (* 1.5 (org-scribe-plan-daily-words plan)))))))
      (_ (setf (org-scribe-plan-weekend-mode plan) 'rest)))
    (unless (eq (org-scribe-plan-weekend-mode plan) 'same)
      (setf (org-scribe-plan-weekend-days plan)
            (org-scribe-planner--read-weekend-days plan)))
    (org-scribe-planner--offer-adjustment
     plan before "This changes the weekends. How should the plan adjust?")))

(defun org-scribe-planner--change-day-goal-set (plan _file)
  "Ask for a date and its goal in PLAN."
  (let* ((date (org-scribe-planner--read-date "Date (YYYY-MM-DD)"
                                              (org-scribe-planner--get-today-date)))
         (problem (org-scribe-planner--custom-goal-problem plan date)))
    (when problem (user-error "%s" problem))
    (let ((goal (org-scribe-planner--read-non-negative-number
                 (format "Goal for %s (0 makes it a rest day): " date)
                 (or (cdr (assoc date (org-scribe-plan-custom-goals plan)))
                     (plist-get (cl-find date (org-scribe-planner--generate-day-schedule plan)
                                         :key (lambda (d) (plist-get d :date))
                                         :test #'string=)
                                :words))))
          (before (org-scribe-planner--copy-plan plan)))
      (setf (org-scribe-plan-custom-goals plan)
            (cons (cons date goal)
                  (assoc-delete-all date (copy-tree (org-scribe-plan-custom-goals plan)))))
      (org-scribe-planner--offer-adjustment
       plan before org-scribe-planner--day-goal-question))))

(defun org-scribe-planner--change-day-goal-remove (plan _file)
  "Remove one of PLAN's day goals from today on."
  (let ((dates (cl-remove-if (lambda (date) (string< date (org-scribe-planner--get-today-date)))
                             (mapcar #'car (org-scribe-plan-custom-goals plan)))))
    (if (null dates)
        (message "No day goals from today on to remove")
      (let ((date (org-scribe-planner--choose "Remove the goal of: " (sort dates #'string<)))
            (before (org-scribe-planner--copy-plan plan)))
        (setf (org-scribe-plan-custom-goals plan)
              (assoc-delete-all date (copy-tree (org-scribe-plan-custom-goals plan))))
        (org-scribe-planner--offer-adjustment
         plan before org-scribe-planner--day-goal-question)))))

(defun org-scribe-planner--change-day-goals-clear (plan _file)
  "Remove all of PLAN's day goals from today on."
  (let* ((today (org-scribe-planner--get-today-date))
         (clearable (cl-remove-if (lambda (goal) (string< (car goal) today))
                                  (org-scribe-plan-custom-goals plan))))
    (if (null clearable)
        (message "No day goals from today on to clear")
      (when (y-or-n-p (format "Clear %d day goals from today on? " (length clearable)))
        (let ((before (org-scribe-planner--copy-plan plan)))
          (setf (org-scribe-plan-custom-goals plan)
                (cl-remove-if-not (lambda (goal) (string< (car goal) today))
                                  (org-scribe-plan-custom-goals plan)))
          (org-scribe-planner--offer-adjustment
           plan before org-scribe-planner--day-goal-question))))))

(defun org-scribe-planner--change-spare-days-add (plan _file)
  "Add or modify PLAN's spare days, then ask how the plan adjusts."
  (let ((before (org-scribe-planner--copy-plan plan)))
    (org-scribe-planner--configure-spare-days plan)
    (org-scribe-planner--offer-adjustment plan before)))

(defun org-scribe-planner--change-spare-days-remove (plan _file)
  "Remove one of PLAN's spare days, then ask how the plan adjusts."
  (if (null (org-scribe-plan-spare-days plan))
      (message "No spare days to remove")
    (let ((before (org-scribe-planner--copy-plan plan))
          (date (org-scribe-planner--choose "Select date to remove: "
                                            (sort (copy-sequence (org-scribe-plan-spare-days plan))
                                                  #'string<))))
      (setf (org-scribe-plan-spare-days plan)
            (remove date (org-scribe-plan-spare-days plan)))
      (org-scribe-planner--offer-adjustment plan before))))

(defun org-scribe-planner--change-spare-days-clear (plan _file)
  "Remove all of PLAN's spare days, then ask how the plan adjusts."
  (let ((spare-days (org-scribe-plan-spare-days plan)))
    (if (null spare-days)
        (message "No spare days to clear")
      (when (y-or-n-p (format "Remove all %d spare days? " (length spare-days)))
        (let ((before (org-scribe-planner--copy-plan plan)))
          (setf (org-scribe-plan-spare-days plan) nil)
          (org-scribe-planner--offer-adjustment plan before))))))

(defconst org-scribe-planner--change-options
  '(("Total words" . org-scribe-planner--change-total-words)
    ("Daily words" . org-scribe-planner--change-daily-words)
    ("Days available" . org-scribe-planner--change-days-available)
    ("Two values (third calculated)" . org-scribe-planner--change-two-values)
    ("Start date" . org-scribe-planner--change-start-date)
    ("Words outside the manuscript" . org-scribe-planner--change-outside-words)
    ("Writing pattern" . org-scribe-planner--change-pattern)
    ("Weekends" . org-scribe-planner--change-weekends)
    ("Day goal: set" . org-scribe-planner--change-day-goal-set)
    ("Day goal: remove" . org-scribe-planner--change-day-goal-remove)
    ("Day goals: clear from today on" . org-scribe-planner--change-day-goals-clear)
    ("Add/modify spare days" . org-scribe-planner--change-spare-days-add)
    ("Remove spare days" . org-scribe-planner--change-spare-days-remove)
    ("Clear all spare days" . org-scribe-planner--change-spare-days-clear))
  "The options of \"What changed?\" (`org-scribe-planner-recalculate').
Each is (LABEL . HANDLER); HANDLER is called with the plan and its file,
changes the plan in place, and signals (or the user quits) to leave it
unchanged.")

;;;###autoload (autoload 'org-scribe-planner-recalculate "org-scribe-planner" nil t)
(defun org-scribe-planner-recalculate ()
  "Change the active writing plan, previewing the result before applying it.
A new total, daily target or length asks which of the other two to keep;
\"Two values\" sets two and calculates the third.  Adding or removing
spare days asks how the plan absorbs them: keep the end date (the daily
target changes), move the end date (keep the daily target), or keep both
\(the total changes).  Every option shows its outcome first.  Past days
keep their goals throughout.  The options are
`org-scribe-planner--change-options'."
  (interactive)
  (org-scribe-planner--with-current-plan (plan file)
    ;; If the change doesn't finish (no option possible, C-g), the plan in
    ;; memory goes back to how it was: nothing half-changed stays active
    (let ((backup (org-scribe-planner--copy-plan plan))
          (finished nil))
      (unwind-protect
          (let* ((choice (org-scribe-planner--choose
                          "What changed? "
                          (mapcar #'car org-scribe-planner--change-options)))
                 (handler (cdr (assoc choice org-scribe-planner--change-options))))
            (when handler
              (funcall handler plan file))
            (org-scribe-planner--save-plan plan file)
            (org-scribe-planner--refresh-agenda-if-synced plan file)
            (setq org-scribe-planner--current-plan plan)
            (setq finished t)
            (org-scribe-planner-show-calendar plan file))
        (unless finished
          (setq org-scribe-planner--current-plan backup))))))

(provide 'org-scribe-planner-edit)

;;; org-scribe-planner-edit.el ends here
