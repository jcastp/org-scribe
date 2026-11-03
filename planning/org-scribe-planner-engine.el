;;; org-scribe-planner-engine.el --- Plan model and schedule engine for org-scribe-planner -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Javier Castilla
;; URL: https://codeberg.org/jcastp/org-scribe

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The plan structure, the customization options, the schedule engine,
;; dates, recalculation bookkeeping (rebase, out of date), where the plan
;; stands, and the progress analytics.  Nothing here reads or writes
;; files or draws buffers.
;;
;; Part of the writing planner; load it through `org-scribe-planner',
;; which requires every part in order.

;;; Code:

(require 'org)
(require 'org-element)
(require 'calendar)
(require 'cl-lib)

(declare-function org-scribe--count-words-region "counting/org-scribe-wordcount")
(declare-function org-scribe--project-marker-get "core/org-scribe-core")
(declare-function org-scribe-plan "org-scribe-planner")
(defvar org-scribe-planner--day-names)
(declare-function org-scribe-planner--get-current-plan "org-scribe-planner-io")
(declare-function org-scribe-project-root "core/org-scribe-core")
(declare-function org-scribe-project-structure "core/org-scribe-core")
(declare-function org-scribe-project-word-target "core/org-scribe-core")

;;; Customization

(defgroup org-scribe-planner nil
  "Writing planning and tracking tools for Org-mode."
  :group 'org-scribe
  :prefix "org-scribe-planner-")

(defcustom org-scribe-planner-directory (expand-file-name "writing-projects/" org-directory)
  "Directory where writing project files are stored."
  :type 'directory
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-calendar-buffer "*Writing Plan Calendar*"
  "Name of the buffer for calendar visualization."
  :type 'string
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-auto-track-daily t
  "When non-nil, automatically populate DAILY_WORD_COUNTS on every manuscript save.
Each save records the manuscript's cumulative word total for today via
`org-scribe-planner-record-total'; the net daily delta is derived
automatically from the previous entry (see the
`org-scribe-plan-daily-word-counts' slot docstring) — there is no
separate baseline to seed or roll forward.  Set to nil to keep the old
behaviour where only CURRENT_WORDS is updated automatically and daily
entries must be entered by hand with `org-scribe-planner-today'."
  :type 'boolean
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-presets
  '(("Novel draft" :description "A full novel draft" :total 80000 :days 60
     :pattern valley :weekend-mode same :project-types (novel))
    ("NaNoWriMo" :description "50,000 words in 30 days" :total 50000 :days 30
     :pattern flat :weekend-mode different :project-types (novel))
    ("Novella" :description "A novella, 20,000-40,000 words" :total 30000 :days 30
     :pattern mountain :weekend-mode different :project-types (novel short-story))
    ("Short story" :description "A short story in a week" :total 5000 :days 7
     :pattern rising :weekend-mode different :project-types (short-story))
    ("1,000 words a day" :description "A daily habit for a month" :total 30000 :daily 1000
     :pattern flat :weekend-mode same)
    ("500 words a day" :description "A gentle daily habit for a month" :total 15000 :daily 500
     :pattern flat :weekend-mode same))
  "Preset plans offered when a new plan is created.
Each entry is (NAME . PLIST).  The PLIST keys:
  :description    one line shown next to the name
  :total          the total goal, in words
  :days           the days available (calendar days), or
  :daily          the words per day: give one of the two
  :pattern        a suggested writing pattern (one of
                  `org-scribe-planner--patterns')
  :weekend-mode   a suggested weekend setting: `same', `different' or `rest'
  :project-types  the org-scribe project types (`novel', `short-story') the
                  preset suits; the first is its main one.  A project's
                  type gets, as the default answer, the first preset made
                  mainly for it, or else the first that suits it
The values can all be changed when the plan is created.  Adapted from
wordplanner's templates."
  :type '(alist :key-type string :value-type plist)
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-default-pattern 'flat
  "Writing pattern offered for new plans made from scratch.
One of `flat', `valley', `mountain', `rising', `declining' or `random'.
When it isn't `flat', the new-plan wizard asks whether to use it."
  :type '(choice (const flat) (const valley) (const mountain) (const rising)
                 (const declining) (const random))
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-default-pattern-intensity 'moderate
  "How much the default pattern varies: `mild', `moderate' or `extreme'."
  :type '(choice (const mild) (const moderate) (const extreme))
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-default-weekend-mode 'same
  "Weekend setting offered for new plans made from scratch.
`same' as other days, a `different' goal of their own, or `rest' (no
writing).  When it isn't `same', the new-plan wizard asks whether to use it."
  :type '(choice (const same) (const different) (const rest))
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-default-weekend-days '(6 0)
  "Days of the week new plans treat as the weekend, 0 for Sunday to 6 for Saturday."
  :type '(repeat integer)
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-default-weekend-daily nil
  "Weekend goal offered when weekends have a goal of their own, in words.
nil offers 1.5 times the plan's daily target."
  :type '(choice (const :tag "1.5 × the daily target" nil) integer)
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-week-start-day 1
  "The day the planner's weeks start on: 1 for Monday, 0 for Sunday.
Any day of the week works, 0 (Sunday) to 6 (Saturday).  Used by the plan
calendar's weeks, the weekly summary, the month grid and the heatmap."
  :type '(choice (const :tag "Monday" 1) (const :tag "Sunday" 0)
                 (integer :tag "Other day (0 = Sunday … 6 = Saturday)"))
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-streak-grace-days 2
  "How many days, today included, the last written day may lie behind.
A value of 2 means the streak survives while the last written day is
yesterday, which accommodates logging words the morning after you wrote
them; each day more tolerates one more missed writing day.  Set to 1 to
count the streak only once today's words are in."
  :type 'integer
  :group 'org-scribe-planner)

;;; Current Plan Tracking

(defvar org-scribe-planner--current-plan nil
  "The currently active writing plan.
This is set when creating a new plan or loading an existing one.")

(defvar org-scribe-planner--current-plan-file nil
  "File path of the currently active writing plan.
This is set when creating a new plan or loading an existing one.")

;;; Integration Hooks and Pluggable Function Variables

(defvar org-scribe-planner-after-progress-update-hook nil
  "Hook run after a word count update is saved.
Each function is called with three arguments: PLAN (the current
`org-scribe-plan' struct), NEW-COUNT (the updated cumulative word
count or per-day count just saved), and DATE (YYYY-MM-DD string for
the day being updated, or nil for a cumulative update).")

(defvar org-scribe-planner-after-plan-load-hook nil
  "Hook run after a plan is loaded and set as current.
Each function is called with two arguments: PLAN (the loaded
`org-scribe-plan' struct) and FILE-PATH (absolute path to the .org
file).")

(defvar org-scribe-planner-wordcount-function nil
  "Optional function to retrieve the current project word count.
When non-nil, `org-scribe-planner-update-progress' calls this
function with no arguments and uses its return value (an integer)
as the new cumulative word count, skipping the manual prompt.
The function may return nil to fall back to the normal prompt.")

(defvar org-scribe-planner-project-root-function nil
  "Optional function returning the current writing project root.
When non-nil, commands such as `org-scribe-planner-load-plan' can
use this to suggest or auto-load the project's associated plan file.
The function takes no arguments and returns a directory path string
or nil if no project is active.")

;;; Data Structures

(cl-defstruct org-scribe-plan
  "Structure representing a writing plan."
  (title nil :type string)
  (total-words nil :type (or null number))
  (daily-words nil :type (or null number))
  (days nil :type (or null number))
  (start-date nil :type string)  ; Format: "YYYY-MM-DD"
  (end-date nil :type string)
  (spare-days nil :type list)    ; List of dates in "YYYY-MM-DD" format
  (current-words 0 :type number)
  ;; Words in the manuscript before the plan started (PREEXISTING_WORDS).
  ;; TOTAL-WORDS is the full goal, these included; the schedule covers the
  ;; rest.  The first ledger entry's delta is measured against them.
  (preexisting-words 0 :type number)
  ;; Writing days (WRITING_DAYS): the plan's length.  END-DATE and DAYS
  ;; (calendar days, spare days included) are derived from it.
  (writing-days nil :type (or null number))
  ;; Alist of (DATE . GOAL): goals frozen once a day is past (LOCKED_GOALS)
  (locked-goals nil :type list)
  ;; Words added to the open days by a recalculation from progress, so they
  ;; cover what is actually left (negative when ahead) (SCHEDULE_ADJUSTMENT)
  (schedule-adjustment 0 :type number)
  ;; The first day of the last recalculation from progress (REBASED_FROM):
  ;; the plan before it is measured as it stood, without the adjustment,
  ;; and from the end of the day before it on the recalculated plan.  Nil
  ;; when the plan was never rebased (see `org-scribe-planner--rebase-date')
  (rebased-from nil :type (or null string))
  ;; What the last recalculation saw (REBASED_ON, REBASED_WORDS): the day it
  ;; ran, and the manuscript's total as of the day before REBASED_FROM.  The
  ;; day it ran may still be written on; the two let the plan tell when the
  ;; recalculation went out of date (see `org-scribe-planner--rebase-stale').
  ;; Nil for a plan recalculated before they were kept.
  (rebased-on nil :type (or null string))
  (rebased-words nil :type (or null number))
  ;; Writing pattern (PATTERN, PATTERN_CONFIG): a symbol from
  ;; `org-scribe-planner--patterns', and a plist of its settings
  (pattern 'flat)
  (pattern-config nil :type list)
  ;; Weekends (WEEKEND_MODE, WEEKEND_DAYS, WEEKEND_DAILY): `same' as other
  ;; days, a `different' goal (WEEKEND-DAILY), or `rest'; WEEKEND-DAYS are
  ;; days of the week, 0 for Sunday to 6 for Saturday
  (weekend-mode 'same)
  (weekend-days '(6 0) :type list)
  (weekend-daily 0 :type number)
  ;; Alist of (DATE . GOAL): goals set for single days (CUSTOM_GOALS)
  (custom-goals nil :type list)
  ;; Words that count toward the goal but aren't in the manuscript: a typed
  ;; number (OUTSIDE_WORDS), and files counted (OUTSIDE_FILES), each a plist
  ;; (:path P :live t) counted on every sync, or (:path P :count N) counted
  ;; once.  Kept and shown, never counted (see
  ;; `org-scribe-planner--written-total').
  (outside-words 0 :type number)
  (outside-files nil :type list)
  ;; What the ledger holds (WORDS_BASIS): nil for a plan saved before the
  ;; ledger stopped counting outside words, which may include them;
  ;; `manuscript' once it holds the manuscript's count only.
  (words-basis nil)
  (org-heading-marker nil)       ; Marker to org heading
  ;; Alist of (date . plist), plist has :words :note :target.  :words holds
  ;; the manuscript's CUMULATIVE word total as of that date, not that day's
  ;; delta — see `org-scribe-planner--entry-delta'.
  (daily-word-counts nil :type list))

;;; Core Calculation Functions

(defun org-scribe-planner--spare-days-in-window (plan days)
  "Return how many of PLAN's spare days fall within its first DAYS calendar days.
The window runs from PLAN's start-date to start-date + DAYS - 1, the same
span `org-scribe-planner--calculate-dates' gives the plan.  Spare days
outside it don't shorten the plan.  Without a start-date there is no
window yet, so every spare day counts."
  (let ((spare-days (delete-dups (copy-sequence (org-scribe-plan-spare-days plan))))
        (start (org-scribe-plan-start-date plan)))
    (if (not start)
        (length spare-days)
      (let ((end (org-scribe-planner--add-days start (1- days))))
        (cl-count-if (lambda (date)
                       (and (not (string< date start))
                            (not (string< end date))))
                     spare-days)))))

;;; Schedule engine
;;
;; One pass decides every day's goal, as wordplanner's `Plan.build_schedule'
;; does.  Days are either FIXED (a locked past goal, or 0 on a spare day) or
;; ADJUSTABLE; the adjustable days share the words left after the fixed
;; ones, weighted by the pattern (flat for now: every weight is 1), so the
;; schedule always adds up to the words to write plus the schedule
;; adjustment.  The plan stores its number of WRITING days; the end date
;; and the calendar-day count are derived from it.  The daily target is
;; derived too: the goal of an ordinary open day.  It is an input only to
;; `org-scribe-planner--calculate-time' and
;; `org-scribe-planner--total-for-daily'.

(defconst org-scribe-planner--max-days 36500
  "Upper limit for a plan's writing days (100 years).
It also bounds the search in `org-scribe-planner--calculate-time'.")

(defun org-scribe-planner--writing-days (plan)
  "Return PLAN's number of writing days, or nil if it can't be known yet.
Uses the WRITING-DAYS slot when set.  Plans that don't have it (older
files, plans built by hand) derive it from START-DATE and END-DATE, or
from DAYS, by taking out the spare days inside that window."
  (let ((stored (org-scribe-plan-writing-days plan))
        (start (org-scribe-plan-start-date plan))
        (end (org-scribe-plan-end-date plan))
        (days (org-scribe-plan-days plan)))
    (cond
     ((numberp stored) stored)
     ((and start end)
      (let ((span (org-scribe-planner--days-between start end)))
        (max 0 (- span (org-scribe-planner--spare-days-in-window plan span)))))
     ((numberp days)
      (max 0 (- days (org-scribe-planner--spare-days-in-window plan days))))
     (t nil))))

(defconst org-scribe-planner--patterns
  '((flat . "Steady: the same goal every day")
    (valley . "Valley: start high, ease in the middle, finish high")
    (mountain . "Mountain: start easy, peak in the middle, ease at the end")
    (rising . "Rising: goals grow over time")
    (declining . "Declining: goals shrink over time")
    (random . "Random: varied goals within a range"))
  "Writing patterns: how the words are spread over the writing days.")

(defconst org-scribe-planner--pattern-intensities
  '((valley (mild :peak 1.2 :valley 0.8) (moderate :peak 1.5 :valley 0.7)
            (extreme :peak 2.0 :valley 0.5))
    (mountain (mild :peak 1.2 :valley 0.8) (moderate :peak 1.5 :valley 0.7)
              (extreme :peak 2.0 :valley 0.5))
    (rising (mild :start 0.8 :end 1.2) (moderate :start 0.5 :end 1.5)
            (extreme :start 0.3 :end 2.0))
    (declining (mild :start 1.2 :end 0.8) (moderate :start 1.5 :end 0.5)
               (extreme :start 2.0 :end 0.3))
    (random (mild :min 0.8 :max 1.2) (moderate :min 0.5 :max 1.5)
            (extreme :min 0.3 :max 2.0)))
  "Each pattern's settings for a mild, moderate (default) or extreme variation.")

(defun org-scribe-planner--pattern-setting (plan key)
  "Return PLAN's pattern setting KEY, or the moderate default for its pattern."
  (or (plist-get (org-scribe-plan-pattern-config plan) key)
      (plist-get (cdr (assq 'moderate (cdr (assq (org-scribe-plan-pattern plan)
                                                  org-scribe-planner--pattern-intensities))))
                 key)))

(defun org-scribe-planner--random-weights (seed count low high)
  "Return COUNT numbers in [LOW, HIGH] from a generator seeded with SEED.
A linear congruential generator of its own, so the global `random' state
is never touched and the same seed always gives the same goals."
  (let ((state seed) (result nil))
    (dotimes (_ count)
      (setq state (mod (+ (* state 1103515245) 12345) 2147483648))
      (push (+ low (* (- high low) (/ (float state) 2147483648))) result))
    (nreverse result)))

(defun org-scribe-planner--pattern-weights (plan positions)
  "Return the relative weight of each of POSITIONS writing days of PLAN.
The weights follow PLAN's pattern (see `org-scribe-planner--patterns')
and average 1, so the daily target is the goal of a day of weight 1:
  valley    low + (high - low) × (cos(2πp/n) + 1) / 2
  mountain  low + (high - low) × sin(πp/n)
  rising,
  declining a straight line from the start to the end multiplier
  random    uniform in [min, max], from the plan's stored seed
for position p of n."
  (let ((n positions)
        (pattern (or (org-scribe-plan-pattern plan) 'flat))
        (setting (lambda (key) (org-scribe-planner--pattern-setting plan key))))
    (if (<= n 0)
        nil
      (let* ((raw
              (pcase pattern
                ('valley
                 (let ((high (funcall setting :peak)) (low (funcall setting :valley)))
                   (mapcar (lambda (p) (+ low (* (- high low)
                                                 (/ (+ (cos (/ (* p 2 float-pi) n)) 1) 2))))
                           (number-sequence 0 (1- n)))))
                ('mountain
                 (let ((high (funcall setting :peak)) (low (funcall setting :valley)))
                   (mapcar (lambda (p) (+ low (* (- high low) (sin (/ (* p float-pi) n)))))
                           (number-sequence 0 (1- n)))))
                ((or 'rising 'declining)
                 (let ((from (funcall setting :start)) (to (funcall setting :end)))
                   (mapcar (lambda (p) (+ from (* (- to from)
                                                  (if (> n 1) (/ (float p) (1- n)) 0))))
                           (number-sequence 0 (1- n)))))
                ('random
                 (org-scribe-planner--random-weights
                  (or (funcall setting :seed) 1) n
                  (funcall setting :min) (funcall setting :max)))
                (_ (make-list n 1.0))))
             (sum (apply #'+ raw)))
        (if (<= sum 0)
            (make-list n 1.0)
          (mapcar (lambda (w) (/ (* w n) sum)) raw))))))

(defun org-scribe-planner--weekday (date)
  "Return DATE's day of the week, 0 for Sunday to 6 for Saturday."
  (let ((parts (org-scribe-planner--parse-date-parts date)))
    (calendar-day-of-week (list (nth 1 parts) (nth 2 parts) (nth 0 parts)))))

(defun org-scribe-planner--weekend-p (plan date)
  "Return non-nil when DATE falls on one of PLAN's weekend days."
  (memq (org-scribe-planner--weekday date)
        (or (org-scribe-plan-weekend-days plan) '(6 0))))

(defvar org-scribe-planner--engine-cache nil
  "One-entry cache for `org-scribe-planner--build-schedule': (KEY . RESULT).
KEY holds every input of the schedule, so plain slot changes need no
invalidation.")

(defun org-scribe-planner--effective-total (plan writing-days)
  "Return PLAN's total goal; for a plan without one, what its daily target gives.
WRITING-DAYS is PLAN's number of writing days."
  (or (org-scribe-plan-total-words plan)
      (+ (or (org-scribe-plan-preexisting-words plan) 0)
         (* (or (org-scribe-plan-daily-words plan) 0) (or writing-days 0)))))

(defun org-scribe-planner--build-schedule (plan)
  "Decide the goal of every day of PLAN in one pass.
Days are fixed or adjustable.  In order of precedence, a day is fixed at:
its locked goal (a past day); 0 when it is a rest day (a spare date, or a
weekend day when weekends are off); its custom day goal; or the weekend
goal when weekends have a goal of their own.  The adjustable days share
the words left after the fixed ones, weighted by the pattern (see
`org-scribe-planner--pattern-weights'); pattern positions skip rest
days, so the shape doesn't shift when one is added.

Returns a plist:
  :rows              list of (DATE :goal G :spare BOOL :rest BOOL :locked BOOL
                     :custom BOOL), in date order; G is exact (a float on
                     adjustable days)
  :level             goal of an adjustable day of weight 1
  :adjustable-weight sum of the adjustable days' weights
  :fixed-words       sum of the fixed days' goals
  :end-date          the last day, or nil
Without a start date there are no dates yet: :rows is nil and the plan's
writing days are all adjustable."
  (let* ((start (org-scribe-plan-start-date plan))
         (writing-days (org-scribe-planner--writing-days plan))
         (spare (org-scribe-plan-spare-days plan))
         (locked (org-scribe-plan-locked-goals plan))
         (custom (org-scribe-plan-custom-goals plan))
         (weekend-mode (or (org-scribe-plan-weekend-mode plan) 'same))
         (weekend-daily (or (org-scribe-plan-weekend-daily plan) 0))
         (preexisting (or (org-scribe-plan-preexisting-words plan) 0))
         (adjustment (or (org-scribe-plan-schedule-adjustment plan) 0))
         (total (org-scribe-planner--effective-total plan writing-days))
         ;; Copies: a caller changing a list in place (e.g. `delete' on the
         ;; spare days) must not change the cached key with it
         (key (list start writing-days (copy-tree spare) (copy-tree locked)
                    (copy-tree custom) weekend-mode
                    (copy-sequence (org-scribe-plan-weekend-days plan)) weekend-daily
                    (org-scribe-plan-pattern plan)
                    (copy-tree (org-scribe-plan-pattern-config plan))
                    preexisting adjustment total)))
    (if (equal key (car org-scribe-planner--engine-cache))
        (cdr org-scribe-planner--engine-cache)
      (let ((rows nil)
            (fixed-words 0)
            (adjustable-weight 0)
            (end-date nil))
        (if (and start writing-days (> writing-days 0))
            (let* ((end (org-scribe-planner--end-after-working-days
                         start writing-days spare))
                   (dates (org-scribe-planner--generate-date-range start end))
                   (rest-p (lambda (date)
                             (or (member date spare)
                                 (and (eq weekend-mode 'rest)
                                      (org-scribe-planner--weekend-p plan date)))))
                   ;; Pattern positions: every day that isn't a rest day
                   (positions (cl-remove-if rest-p dates))
                   ;; A hash table: an alist lookup per day is quadratic in
                   ;; the plan's length
                   (weights (let ((table (make-hash-table :test #'equal
                                                          :size (length positions))))
                              (cl-mapc (lambda (date weight) (puthash date weight table))
                                       positions
                                       (org-scribe-planner--pattern-weights
                                        plan (length positions)))
                              table)))
              (setq end-date end)
              (dolist (date dates)
                (let* ((lock (assoc date locked))
                       (is-spare (and (member date spare) t))
                       (is-rest (and (funcall rest-p date) t))
                       (goal (assoc date custom))
                       (flags (list :spare is-spare :rest is-rest)))
                  (cond
                   (lock
                    (setq fixed-words (+ fixed-words (cdr lock)))
                    (push (append (list date :goal (cdr lock) :locked t) flags) rows))
                   (is-rest
                    (push (append (list date :goal 0) flags) rows))
                   (goal
                    (setq fixed-words (+ fixed-words (cdr goal)))
                    (push (append (list date :goal (cdr goal) :custom t) flags) rows))
                   ((and (eq weekend-mode 'different)
                         (org-scribe-planner--weekend-p plan date))
                    (setq fixed-words (+ fixed-words weekend-daily))
                    (push (append (list date :goal weekend-daily) flags) rows))
                   (t
                    (let ((weight (gethash date weights)))
                      (setq adjustable-weight (+ adjustable-weight weight))
                      (push (append (list date :goal weight :weight weight) flags)
                            rows))))))
              (setq rows (nreverse rows)))
          ;; No dates yet: abstract writing days, all adjustable
          (setq adjustable-weight
                (apply #'+ (org-scribe-planner--pattern-weights plan (or writing-days 0)))))
        (let* ((target (- (+ (- total preexisting) adjustment) fixed-words))
               (level (if (> adjustable-weight 0)
                          (max 0.0 (/ (float target) adjustable-weight))
                        0.0)))
          ;; Adjustable rows held their weight; turn it into the goal
          (dolist (row rows)
            (let ((weight (plist-get (cdr row) :weight)))
              (when weight
                (setf (cdr row) (list :goal (* level weight) :adjustable t
                                      :spare nil :rest nil)))))
          (let ((result (list :rows rows
                              :level level
                              :adjustable-weight adjustable-weight
                              :fixed-words fixed-words
                              :end-date end-date)))
            (setq org-scribe-planner--engine-cache (cons key result))
            result))))))

(defun org-scribe-planner--total-for-daily (plan daily)
  "Return the total goal that gives PLAN's ordinary open days a goal of DAILY.
Fixed days keep their goals.  Used to keep the daily target when something
else changes."
  (let ((schedule (org-scribe-planner--build-schedule plan)))
    (- (+ (or (org-scribe-plan-preexisting-words plan) 0)
          (plist-get schedule :fixed-words)
          (* daily (plist-get schedule :adjustable-weight)))
       (or (org-scribe-plan-schedule-adjustment plan) 0))))

(defun org-scribe-planner--sync-daily (plan)
  "Set PLAN's daily target to the goal of an ordinary open day, rounded.
Leaves it alone when no adjustable day is left."
  (let ((schedule (org-scribe-planner--build-schedule plan)))
    (when (> (plist-get schedule :adjustable-weight) 0)
      (setf (org-scribe-plan-daily-words plan)
            (round (plist-get schedule :level))))))

(defun org-scribe-planner--sync-window (plan)
  "Store PLAN's writing days and set its END-DATE and DAYS from them.
With a start date, END-DATE is the day the last writing day falls on and
DAYS the calendar days from START-DATE to it; without one, DAYS is the
writing days plus every spare day."
  (let ((writing-days (org-scribe-planner--writing-days plan)))
    (setf (org-scribe-plan-writing-days plan) writing-days)
    (when writing-days
      (if (org-scribe-plan-start-date plan)
          (let ((end (plist-get (org-scribe-planner--build-schedule plan) :end-date)))
            (setf (org-scribe-plan-end-date plan) end)
            (setf (org-scribe-plan-days plan)
                  (if end
                      (org-scribe-planner--days-between
                       (org-scribe-plan-start-date plan) end)
                    0)))
        (setf (org-scribe-plan-days plan)
              (+ writing-days
                 (length (delete-dups (copy-sequence (org-scribe-plan-spare-days plan))))))))))

(defun org-scribe-planner--set-writing-days (plan writing-days)
  "Give PLAN WRITING-DAYS writing days, then refresh its window and daily target."
  (setf (org-scribe-plan-writing-days plan) writing-days)
  (org-scribe-planner--sync-window plan)
  (org-scribe-planner--sync-daily plan))

(defun org-scribe-planner--min-writing-days (plan)
  "Return the fewest writing days PLAN can have without dropping locked days.
The writing days from the start date through the last locked day on or
after it: past days are history, so the plan can't end before them.
Locked days are normally every day from the start to yesterday, so this
is usually the number of locked days; after the start date moves, a gap
between them counts too."
  (let* ((start (org-scribe-plan-start-date plan))
         (last-locked (when start
                        (car (last (sort (cl-remove-if (lambda (date) (string< date start))
                                                       (mapcar #'car (org-scribe-plan-locked-goals plan)))
                                         #'string<))))))
    (if last-locked
        (org-scribe-planner--writing-days-through plan last-locked)
      0)))

(defun org-scribe-planner--calculate-time (plan daily)
  "Give PLAN the fewest writing days that fit its total at DAILY words a day.
Searches by doubling and halving, since the schedule only holds more
words as days are added; then the daily target is lowered slightly so
the schedule adds up to exactly the total.  Never goes below the locked
\(past) writing days.  With DAILY 0 the fixed days have to carry the
total; if they can't, signals an error and leaves PLAN unchanged."
  (let* ((total (org-scribe-plan-total-words plan))
         ;; At least one writing day: a plan far ahead could "fit" in none
         (min-days (max 1 (org-scribe-planner--min-writing-days plan)))
         (original (org-scribe-plan-writing-days plan))
         (to-write (max 0 (- total (or (org-scribe-plan-preexisting-words plan) 0))))
         (fits (lambda (days)
                 (setf (org-scribe-plan-writing-days plan) days)
                 (>= (org-scribe-planner--total-for-daily plan daily) (- total 1e-6))))
         (estimate (if (> daily 0)
                       (ceiling (/ (float to-write) daily))
                     (or original 1)))
         (guess (min org-scribe-planner--max-days (max min-days estimate 1)))
         low high)
    ;; With no daily pace, more days only add words through a recurring
    ;; fixed goal (a weekend goal); without one, if the days now can't
    ;; carry the total, no number of days can -- don't search to the limit
    (when (and (<= daily 0)
               (not (funcall fits guess))
               (not (and (eq (org-scribe-plan-weekend-mode plan) 'different)
                         (> (or (org-scribe-plan-weekend-daily plan) 0) 0))))
      (setf (org-scribe-plan-writing-days plan) original)
      (error "With a daily target of 0, the plan's days never reach the %d-word goal"
             total))
    (if (funcall fits guess)
        (setq low (1- min-days) high guess)
      (setq low guess high (min org-scribe-planner--max-days (* 2 guess)))
      (while (and (not (funcall fits high)) (< high org-scribe-planner--max-days))
        (setq low high high (min org-scribe-planner--max-days (* 2 high)))))
    (while (> (- high low) 1)
      (let ((middle (/ (+ low high) 2)))
        (if (funcall fits middle) (setq high middle) (setq low middle))))
    (let ((days (max min-days high)))
      ;; Patterns re-spread their weights with the length: check below too
      (while (and (> days min-days) (funcall fits (1- days)))
        (setq days (1- days)))
      (when (and (= daily 0) (not (funcall fits days)))
        (setf (org-scribe-plan-writing-days plan) original)
        (error "With a daily target of 0, the plan's days never reach the %d-word goal"
               total))
      (org-scribe-planner--set-writing-days plan days))))

(defun org-scribe-planner--writing-days-through (plan end-date)
  "Return PLAN's writing days from its start date through END-DATE, inclusive."
  (let ((start (org-scribe-plan-start-date plan))
        (spare (org-scribe-plan-spare-days plan)))
    (if (or (not start) (string< end-date start))
        0
      (cl-count-if-not (lambda (date) (member date spare))
                       (org-scribe-planner--generate-date-range start end-date)))))

(defun org-scribe-planner--keep-end-date (plan end-date)
  "Give PLAN the writing days that end it on END-DATE, keeping its total.
Used after spare days change: the words are spread over the writing days
left before END-DATE.  Never goes below the locked (past) writing days."
  (org-scribe-planner--set-writing-days
   plan (max 1 (org-scribe-planner--min-writing-days plan)
             (org-scribe-planner--writing-days-through plan end-date))))

(defun org-scribe-planner--calculate-missing-variable (plan)
  "Calculate the missing variable in PLAN.
Given 2 of 3 variables (total-words, daily-words, days), calculate the third.
Updates PLAN in place and returns the calculated variable as a symbol.

DAYS is calendar days from the start date, spare days included; the plan
gets the writing days left once the spare days inside that window are
taken out (see `org-scribe-planner--spare-days-in-window').  TOTAL-WORDS
is the full goal; the schedule covers what is left after the plan's
pre-existing words.  The work is done by the schedule engine
\(`org-scribe-planner--build-schedule'): the daily target is always the
goal of an ordinary open day, so the schedule adds up to the total."
  (let* ((total-words (org-scribe-plan-total-words plan))
         (daily-words (org-scribe-plan-daily-words plan))
         (days (org-scribe-plan-days plan))
         (set-count (cl-count-if #'numberp (list total-words daily-words days)))
         (missing-var nil))
    (when (< set-count 2)
      (error "At least 2 variables must be set to calculate a plan. Found: total-words=%s, daily-words=%s, days=%s"
             total-words daily-words days))
    (when (numberp days)
      ;; DAYS is what the caller set: derive the writing days from it
      (let ((working-days (- days (org-scribe-planner--spare-days-in-window plan days))))
        (when (<= working-days 0)
          (error "Cannot calculate plan: not enough working days. Total days: %d, spare days: %d. Please reduce spare days or increase total days"
                 days (- days working-days)))
        (setf (org-scribe-plan-writing-days plan) working-days)))
    (cond
     ((not (numberp total-words))
      (setf (org-scribe-plan-total-words plan)
            (round (org-scribe-planner--total-for-daily plan daily-words)))
      (setq missing-var 'total-words))
     ((not (numberp days))
      (when (<= daily-words 0)
        (error "Cannot calculate plan: daily words (%d) must be greater than 0. Please enter a positive number" daily-words))
      (org-scribe-planner--calculate-time plan daily-words)
      (setq missing-var 'days))
     ((not (numberp daily-words))
      (setq missing-var 'daily-words))
     ;; All three set: the daily target follows the total and the days
     ((/= daily-words (round (plist-get (org-scribe-planner--build-schedule plan) :level)))
      (message "Warning: Values are inconsistent. Recalculating daily-words.")
      (setq missing-var 'daily-words)))
    (org-scribe-planner--sync-window plan)
    (org-scribe-planner--sync-daily plan)
    missing-var))

(defun org-scribe-planner--calculate-dates (plan)
  "Set PLAN's start date (today when unset) and derive its end date.
The end date is where PLAN's writing days end, spare days skipped (see
`org-scribe-planner--sync-window')."
  (unless (org-scribe-planner--writing-days plan)
    (error "Cannot calculate dates: days value is not set. Please ensure the plan has been properly calculated"))
  (unless (org-scribe-plan-start-date plan)
    (setf (org-scribe-plan-start-date plan)
          (org-scribe-planner--get-today-date)))
  (org-scribe-planner--sync-window plan))

(defun org-scribe-planner--parse-date (date-string)
  "Parse DATE-STRING in YYYY-MM-DD format to Emacs time.
Anchored at local noon rather than midnight so that adding whole-day
increments (86400 seconds) with `time-add' never crosses a DST
transition boundary (which always occurs in the small hours, never
near noon), and never duplicates or skips a calendar day."
  (apply #'encode-time (parse-time-string (concat date-string " 12:00:00"))))

(defun org-scribe-planner--parse-date-parts (date-string)
  "Return a list (YEAR MONTH DAY) of integers from DATE-STRING in YYYY-MM-DD format."
  (mapcar #'string-to-number (split-string date-string "-")))

(defun org-scribe-planner--get-today-date ()
  "Get today's date in YYYY-MM-DD format."
  (format-time-string "%Y-%m-%d"))

(defun org-scribe-planner--date-to-string (time)
  "Convert Emacs TIME to YYYY-MM-DD string."
  (format-time-string "%Y-%m-%d" time))

(defun org-scribe-planner--add-days (date-string days)
  "Add DAYS to DATE-STRING and return new date string in YYYY-MM-DD format."
  (let* ((date-time (org-scribe-planner--parse-date date-string))
         (new-time (time-add date-time (days-to-time days))))
    (format-time-string "%Y-%m-%d" new-time)))

(defun org-scribe-planner--end-after-working-days (from working-days spare-days)
  "Return the date on which WORKING-DAYS working days, counted from FROM, end.
FROM is a YYYY-MM-DD string and counts as the first day; dates in
SPARE-DAYS are skipped.  With WORKING-DAYS below 1, returns FROM."
  (let ((date from)
        (left (max 1 working-days)))
    (while (progn
             (unless (member date spare-days)
               (setq left (1- left)))
             (> left 0))
      (setq date (org-scribe-planner--add-days date 1)))
    date))

(defun org-scribe-planner--days-between (start-date-string end-date-string)
  "Calculate the number of days between START-DATE-STRING and END-DATE-STRING."
  (let* ((start-time (org-scribe-planner--parse-date start-date-string))
         (end-time (org-scribe-planner--parse-date end-date-string))
         (diff (time-subtract end-time start-time))
         (days (/ (float-time diff) 86400)))
    (round (1+ days))))  ; Add 1 to include both start and end dates

(defun org-scribe-planner--validate-date-format (date-string)
  "Check if DATE-STRING matches YYYY-MM-DD format.
Returns t if valid format, nil otherwise."
  (and (stringp date-string)
       (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" date-string)))

(defun org-scribe-planner--validate-date (date-string)
  "Validate DATE-STRING is in YYYY-MM-DD format and represents a valid date.
Returns t if valid, signals an error otherwise."
  ;; Check format first
  (unless (org-scribe-planner--validate-date-format date-string)
    (error "Invalid date format: '%s'. Expected YYYY-MM-DD (e.g., 2024-11-19)" date-string))

  ;; Parse date components
  (let* ((parts (org-scribe-planner--parse-date-parts date-string))
         (year (nth 0 parts))
         (month (nth 1 parts))
         (day (nth 2 parts)))

    ;; Validate month
    (unless (and (>= month 1) (<= month 12))
      (error "Invalid month in date '%s': month must be between 1 and 12" date-string))

    ;; Validate day using calendar functions
    ;; calendar-last-day-of-month expects (month year) format
    (let ((max-day (calendar-last-day-of-month month year)))
      (unless (and (>= day 1) (<= day max-day))
        (error "Invalid day in date '%s': day must be between 1 and %d for month %d"
               date-string max-day month)))

    ;; Additional validation: try to parse it
    (condition-case nil
        (progn
          (org-scribe-planner--parse-date date-string)
          t)
      (error
       (error "Invalid date: '%s'. Please check the date is correct" date-string)))))

(defun org-scribe-planner--read-date (prompt &optional default allow-empty)
  "Read a date from user with PROMPT, validating YYYY-MM-DD format.
If DEFAULT is provided, show it in prompt and use if user enters empty string.
If ALLOW-EMPTY is non-nil, empty input returns nil without error.
Returns validated date string or nil (if ALLOW-EMPTY and user entered nothing)."
  (let ((date-input nil)
        (valid nil))
    (while (not valid)
      (setq date-input
            (read-string
             (if default
                 (format "%s (default: %s): " prompt default)
               (format "%s: " prompt))))

      (cond
       ;; Empty input
       ((string-empty-p date-input)
        (cond
         ;; Use default if provided
         (default
          (setq date-input default)
          (setq valid t))
         ;; Allow empty if specified
         (allow-empty
          (setq date-input nil)
          (setq valid t))
         ;; Otherwise, require input
         (t
          (message "Date is required. Please enter a date in YYYY-MM-DD format")
          (sit-for 1))))

       ;; Validate the input
       (t
        (condition-case err
            (progn
              (org-scribe-planner--validate-date date-input)
              (setq valid t))
          (error
           (message "%s" (error-message-string err))
           (sit-for 1.5))))))

    date-input))

;;; Helper Macros

(defmacro org-scribe-planner--with-current-plan (varlist &rest body)
  "Execute BODY with variables from VARLIST bound to current plan and file.
VARLIST should be (PLAN-VAR FILE-VAR).
Days that are now past are locked first (see
`org-scribe-planner--lock-past-goals'), so nothing BODY changes can alter
their goals.  If no current plan is available, display a message and
return nil."
  (declare (indent 1))
  (let ((plan-var (car varlist))
        (file-var (cadr varlist))
        (current-sym (gensym "current")))
    `(let ((,current-sym (org-scribe-planner--get-current-plan t)))
       (if ,current-sym
           (let ((,plan-var (car ,current-sym))
                 (,file-var (cdr ,current-sym)))
             (org-scribe-planner--lock-past-goals ,plan-var)
             ,@body)
         (message "No current plan available")
         nil))))

;;; Project total word target

(defun org-scribe-planner--project-word-target ()
  "Return the current project's total word target, or nil.
The planner is usable without org-scribe, so this is nil there."
  (and (fboundp 'org-scribe-project-word-target)
       (ignore-errors (org-scribe-project-word-target))))

;;; Input Validation Helpers

(defun org-scribe-planner--choose (prompt choices &optional default)
  "Read one of CHOICES with PROMPT, never returning an empty answer.
RET on an empty input picks DEFAULT when there is one, and otherwise asks
again.  A plain `completing-read' with REQUIRE-MATCH returns \"\" for an
empty input, which every caller here then failed on -- most with a raw
\"Invalid choice: \" error that threw away the answers typed so far."
  (let ((choice ""))
    (while (string-empty-p choice)
      (setq choice (completing-read prompt choices nil t nil nil default)))
    choice))

(defun org-scribe-planner--read-positive-number (prompt &optional default)
  "Read a positive number from the user with PROMPT.
Optional DEFAULT provides a default value.
Ensures the number is greater than zero."
  (let ((number nil)
        (valid nil))
    (while (not valid)
      (setq number (read-number prompt default))
      (if (<= number 0)
          (message "Please enter a positive number greater than zero. You entered: %d" number)
        (setq valid t)))
    number))

(defun org-scribe-planner--read-non-negative-number (prompt &optional default)
  "Read a non-negative number from the user with PROMPT.
Optional DEFAULT provides a default value.
Ensures the number is greater than or equal to zero."
  (let ((number nil)
        (valid nil))
    (while (not valid)
      (setq number (read-number prompt default))
      (if (< number 0)
          (message "Please enter a non-negative number (0 or greater). You entered: %d" number)
        (setq valid t)))
    number))

(defun org-scribe-planner--read-days (prompt &optional default)
  "Read number of days from user, either directly or by entering start/end dates.
PROMPT is shown to the user when entering days as a number.
Optional DEFAULT provides a default value when entering days directly.
Returns either:
  - An integer (when entering days directly)
  - A plist with :days, :start-date, :end-date (when entering dates)"
  (let ((method (org-scribe-planner--choose
                 "How do you want to enter the days? "
                 '("Enter number of days"
                   "Enter start and end dates")
                 "Enter number of days")))
    (cond
     ((string-match-p "number of days" method)
      (org-scribe-planner--read-positive-number prompt default))

     ((string-match-p "start and end dates" method)
      (let ((start-date (org-scribe-planner--read-date "Start date (YYYY-MM-DD)"))
            (end-date (org-scribe-planner--read-date "End date (YYYY-MM-DD)")))
        ;; Validate that end date is after or equal to start date
        (let ((days (org-scribe-planner--days-between start-date end-date)))
          (if (<= days 0)
              (error "End date must be on or after start date. Start: %s, End: %s results in %d days"
                     start-date end-date days)
            (message "Calculated %d days from %s to %s" days start-date end-date)
            (list :days days :start-date start-date :end-date end-date)))))

     (t
      (error "Invalid choice: %s" method)))))

;;; Schedule Generation

(defvar org-scribe-planner--schedule-cache nil
  "One-entry memo cache for `org-scribe-planner--generate-day-schedule'.
A cons (ENGINE-RESULT . SCHEDULE): the schedule is rebuilt whenever
`org-scribe-planner--build-schedule' returns a different result.  Set to
nil when no result is cached.")

(defun org-scribe-planner--round-half-up (number)
  "Round NUMBER to the nearest integer, halves upward.
Unlike `round' (halves to even), shifting NUMBER by a whole number shifts
the result by the same amount, which keeps whole-word goals exact."
  (floor (+ number 0.5)))

(defun org-scribe-planner--generate-day-schedule (plan)
  "Generate a list of the days of PLAN with their goals.
Returns a list of plists with :date, :words, :cumulative, :is-spare-day,
:spare, :locked and :custom, one per calendar day from the start date to
the end date.  :is-spare-day marks a rest day: any day whose goal is 0 (a
spare date, a weekend day when weekends are off, a day goal of 0, a
locked 0); everything that skips rest days reads it.  :spare marks the
spare dates themselves, :custom a day with its own goal.

The goals come from the schedule engine (`org-scribe-planner--build-schedule'),
rounded to whole words through their running total, so they add up exactly:
10,000 words over 7 days gives 1,429 four times and 1,428 three times.
:cumulative is where the manuscript should stand at the end of that day:
PLAN's pre-existing words plus the goals up to it, measured from the
baseline of the last recalculation from progress (minus the schedule
adjustment), so it reaches the total on the end date.

The result is memoised on the engine's own result."
  (unless (org-scribe-plan-start-date plan)
    (error "Cannot generate schedule: the plan has no start date. Please calculate dates first"))
  (let ((engine (org-scribe-planner--build-schedule plan)))
    (unless (plist-get engine :rows)
      (error "Cannot generate schedule: the plan has no writing days. Please calculate dates first"))
    (if (eq (car org-scribe-planner--schedule-cache) engine)
        (cdr org-scribe-planner--schedule-cache)
      (let* ((base (- (or (org-scribe-plan-preexisting-words plan) 0)
                      (or (org-scribe-plan-schedule-adjustment plan) 0)))
             (exact base)
             (previous (org-scribe-planner--round-half-up base))
             (schedule nil))
        (dolist (row (plist-get engine :rows))
          (setq exact (+ exact (plist-get (cdr row) :goal)))
          (let* ((cumulative (org-scribe-planner--round-half-up exact))
                 (words (- cumulative previous)))
            (push (list :date (car row)
                        :words words
                        :cumulative cumulative
                        :is-spare-day (zerop words)
                        :spare (plist-get (cdr row) :spare)
                        :locked (plist-get (cdr row) :locked)
                        :custom (plist-get (cdr row) :custom))
                  schedule)
            (setq previous cumulative)))
        (let ((result (nreverse schedule)))
          (setq org-scribe-planner--schedule-cache (cons engine result))
          result)))))

;;; Before and after a recalculation from progress
;;
;; A recalculation from progress (`org-scribe-planner--rebase-on-progress')
;; points the open days at the words really left by shifting the whole
;; schedule by the schedule adjustment.  That shift is right from the
;; recalculation on, and wrong before it: the days before were planned
;; without it.  The plan therefore has two halves:
;;   - days before the rebase date: as they stood, :cumulative plus the
;;     adjustment (pre-existing words plus the goals so far);
;;   - from the rebase date on: :cumulative.
;; At the end of the day before the rebase date the plan steps from one to
;; the other: the words written above (or below) the plan when it was
;; recalculated.  Only the last recalculation shows a step; earlier ones
;; are folded into the locked goals.

(defun org-scribe-planner--rebase-date (plan)
  "Return the first day of PLAN's last recalculation from progress, or nil.
Nil when PLAN has no schedule adjustment (nothing to measure apart) or
no recorded date (REBASED_FROM)."
  (unless (zerop (or (org-scribe-plan-schedule-adjustment plan) 0))
    (org-scribe-plan-rebased-from plan)))

(defun org-scribe-planner--before-rebase-p (plan date)
  "Return non-nil when DATE comes before PLAN's last rebase date."
  (when-let* ((rebased (org-scribe-planner--rebase-date plan)))
    (string< date rebased)))

(defun org-scribe-planner--planned-total (plan day)
  "Return where PLAN's manuscript should stand at the end of DAY, as planned.
DAY is a plist from `org-scribe-planner--generate-day-schedule'.  A day
before the last recalculation from progress is the plan as it stood,
without the schedule adjustment; from the recalculation on it is the
rebased :cumulative.  With the adjustment on every day, the first days of
a rebased plan read a negative total (behind), or one already above the
words due (ahead).  This is the per-day figure of the plan table, the
calendar and the charts; where the plan stands is
`org-scribe-planner--expected-through'."
  (if (org-scribe-planner--before-rebase-p plan (plist-get day :date))
      (+ (plist-get day :cumulative)
         (or (org-scribe-plan-schedule-adjustment plan) 0))
    (plist-get day :cumulative)))

(defun org-scribe-planner--rebase-step (plan)
  "Return PLAN's step at its last recalculation, or nil when there is none.
A plist: :date, the rebase date; :from, where the plan stood at the end
of the day before (as it stood); :to, where the recalculated plan starts
from.  Nil without an adjustment or when the date falls after the plan."
  (when-let* ((rebased (org-scribe-planner--rebase-date plan)))
    (let ((end (org-scribe-plan-end-date plan))
          (before (org-scribe-planner--add-days rebased -1)))
      (unless (and end (string< end rebased))
        (let ((to (org-scribe-planner--rebased-through plan before)))
          (list :date rebased
                :from (+ to (or (org-scribe-plan-schedule-adjustment plan) 0))
                :to to))))))

(defun org-scribe-planner--rebased-through (plan date)
  "Return where PLAN's manuscript should stand at the end of DATE, rebased.
The :cumulative of the last plan day on or before DATE, or the rebased
starting point (pre-existing words minus the schedule adjustment) before
the plan."
  (let ((expected (- (or (org-scribe-plan-preexisting-words plan) 0)
                     (or (org-scribe-plan-schedule-adjustment plan) 0))))
    (dolist (day (org-scribe-planner--generate-day-schedule plan))
      (unless (string< date (plist-get day :date))
        (setq expected (plist-get day :cumulative))))
    expected))

(defun org-scribe-planner--measure-offset (plan date)
  "Return what to add to the rebased plan to measure the end of DATE.
The schedule adjustment while the end of DATE comes before PLAN's last
recalculation (the plan as it stood then), 0 from the end of the day
before it on: from then the plan follows the words actually written."
  (if (org-scribe-planner--before-rebase-p plan (org-scribe-planner--add-days date 1))
      (or (org-scribe-plan-schedule-adjustment plan) 0)
    0))

(defun org-scribe-planner--expected-through (plan date)
  "Return where PLAN's manuscript should stand at the end of DATE.
That is the :cumulative of the last plan day on or before DATE, or the
plan's starting point when DATE is before the plan, measured on the plan
in force at the end of DATE (see `org-scribe-planner--measure-offset'):
before the last recalculation from progress, the plan as it stood (the
pre-existing words before the plan starts); from the end of the day
before it on, the recalculated plan, so recalculating puts the plan on
track again."
  (+ (org-scribe-planner--rebased-through plan date)
     (org-scribe-planner--measure-offset plan date)))

(defun org-scribe-planner--planned-through (plan date)
  "Return where PLAN's manuscript should stand at the end of DATE, as planned.
Like `org-scribe-planner--planned-total' for the last plan day on or
before DATE; before the plan, the pre-existing words."
  (let ((planned (or (org-scribe-plan-preexisting-words plan) 0)))
    (dolist (day (org-scribe-planner--generate-day-schedule plan))
      (unless (string< date (plist-get day :date))
        (setq planned (org-scribe-planner--planned-total plan day))))
    planned))

;;; A recalculation that went out of date
;;
;; A recalculation from progress on a day that is still being written
;; credits that day with the words it has at that moment.  Words written
;; afterwards are not in the plan line; they only show up as being ahead.
;; The recalculation is then stale until it is run again.  Nothing is done
;; behind the writer's back: the report says so, `check-progress' says so,
;; and the first save of a new day offers to run it again.

(defun org-scribe-planner--written-through (plan date)
  "Return the manuscript total PLAN's ledger records as of the end of DATE.
The pre-existing words when no entry falls on or before DATE."
  (let ((total (or (org-scribe-plan-preexisting-words plan) 0)))
    (dolist (entry (org-scribe-planner--sorted-daily-counts
                    (org-scribe-plan-daily-word-counts plan)))
      (when (and (not (string< date (car entry)))
                 (numberp (plist-get (cdr entry) :words)))
        (setq total (plist-get (cdr entry) :words))))
    total))

(defun org-scribe-planner--rebase-stale (plan)
  "Return how PLAN's last recalculation went out of date, or nil.
A plist: :date, the last day the recalculation counted; :anchored, the
words it counted through that day; :written, the words recorded through
it now.  Nil when PLAN was never recalculated, when the recalculation ran
after that day was over, and when nothing changed since.

The recalculation credited that day with the words it had, so it is out
of date when words were written on it afterwards.  A recalculation made
before 2026-11-03 credited a day still being written with at least its
goal: its rebased line through the day is above the words it counted, and
once the day is over it is out of date when the day ended below that
line.  A plan recalculated before the counts were kept assumes the day was
open and was anchored at its rebased total."
  (when-let* ((rebased (org-scribe-plan-rebased-from plan)))
    (let* ((day (org-scribe-planner--add-days rebased -1))
           (ran-on (or (org-scribe-plan-rebased-on plan) day))
           (anchored (or (org-scribe-plan-rebased-words plan)
                         (org-scribe-planner--rebased-through plan day)))
           (written (org-scribe-planner--written-through plan day))
           (credited (org-scribe-planner--rebased-through plan day)))
      (when (and (not (string< day ran-on))
                 (or (/= written anchored)
                     ;; Credited with its goal (the old rule), ended below it
                     (and (org-scribe-plan-rebased-words plan)
                          (> credited (max anchored written))
                          (string< day (org-scribe-planner--get-today-date)))))
        (list :date day :anchored anchored :written written)))))

(defun org-scribe-planner--describe-stale (stale)
  "Describe STALE (from `org-scribe-planner--rebase-stale') in one sentence."
  (let ((gap (- (plist-get stale :written) (plist-get stale :anchored))))
    (format "Recalculated at %d words through %s; %d written there now (%s%d)"
            (plist-get stale :anchored) (plist-get stale :date)
            (plist-get stale :written) (if (>= gap 0) "+" "") gap)))

;;; Where the plan stands

(defun org-scribe-planner--writing-day-position (plan words &optional start)
  "Return how many writing days of PLAN's schedule WORDS cover, as a fraction.
WORDS is a manuscript total.  Follows each day's own goal from START,
the plan's starting point (default: pre-existing words, on the baseline
of the last recalculation), so a recalculation or a pattern doesn't
distort it.  Days without a goal take no position.  Words beyond the end
count at the plan's daily target."
  (let ((cumulative (or start
                        (- (or (org-scribe-plan-preexisting-words plan) 0)
                           (or (org-scribe-plan-schedule-adjustment plan) 0))))
        (position 0.0)
        (result nil))
    (when (> words cumulative)
      (dolist (day (org-scribe-planner--generate-day-schedule plan))
        (let ((goal (plist-get day :words)))
          (when (and (null result) (> goal 0))
            (if (>= (+ cumulative goal) words)
                (setq result (+ position (/ (float (- words cumulative)) goal)))
              (setq cumulative (+ cumulative goal)
                    position (1+ position)))))))
    (cond
     (result result)
     ((<= words cumulative) position)
     (t (+ position (/ (float (- words cumulative))
                       (max 1 (or (org-scribe-plan-daily-words plan) 1))))))))

(defun org-scribe-planner--status (plan &optional through)
  "Return where PLAN stands, or nil when it has no schedule yet.
THROUGH is when the plan's words are due: `today' (the default), where
the manuscript should stand at the end of today, so today's goal counts
already; or `yesterday', the goals of the days that are over (what
`org-scribe-planner-check-progress' offers adjustments on: an unwritten
morning is not behind).  Today, or yesterday, is measured on the plan in
force then: before the last recalculation from progress the plan as it
stood, so a day recalculated after writing keeps its old goal.
A plist:
  :status     `ahead', `on-track' or `behind'
  :expected   where the manuscript should stand by THROUGH
  :actual     the manuscript's count, today's words included
  :difference :actual minus :expected
  :percent    :difference as a percentage of the words the plan expected
              to be written by then (0 before the plan has expected any)
  :days       writing days ahead (positive) or behind (negative),
              measured along the schedule from the same starting point
              as :expected (see `org-scribe-planner--writing-day-position')"
  (when (and (org-scribe-plan-start-date plan)
             (plist-get (org-scribe-planner--build-schedule plan) :rows))
    (let* ((today (org-scribe-planner--get-today-date))
           (date (if (eq through 'yesterday)
                     (org-scribe-planner--add-days today -1)
                   today))
           ;; The plan in force today: as it stood before the last
           ;; recalculation, the recalculated one from its first day on
           (offset (if (org-scribe-planner--before-rebase-p plan today)
                       (or (org-scribe-plan-schedule-adjustment plan) 0)
                     0))
           (expected (+ (org-scribe-planner--rebased-through plan date) offset))
           (actual (org-scribe-plan-current-words plan))
           (difference (- actual expected))
           ;; The plan's starting point, on the same plan as EXPECTED
           (baseline (+ (- (or (org-scribe-plan-preexisting-words plan) 0)
                           (or (org-scribe-plan-schedule-adjustment plan) 0))
                        offset))
           (expected-written (- expected baseline)))
      (list :status (cond ((> difference 0) 'ahead)
                          ((= difference 0) 'on-track)
                          (t 'behind))
            :expected expected
            :actual actual
            :difference difference
            :percent (if (> expected-written 0)
                         (* 100.0 (/ (float difference) expected-written))
                       0.0)
            :days (- (org-scribe-planner--writing-day-position plan actual baseline)
                     (org-scribe-planner--writing-day-position plan expected baseline))))))

(defun org-scribe-planner--describe-status (status)
  "Describe STATUS (from `org-scribe-planner--status') in a few words."
  (pcase (plist-get status :status)
    ('on-track "On track")
    (state (format "%s: %s%d words (%s%.1f%%, %s%.1f writing days)"
                   (if (eq state 'ahead) "Ahead" "Behind")
                   (if (eq state 'ahead) "+" "")
                   (plist-get status :difference)
                   (if (eq state 'ahead) "+" "")
                   (plist-get status :percent)
                   (if (eq state 'ahead) "+" "")
                   (plist-get status :days)))))

;;; Analytics on the schedule

(defun org-scribe-planner--words-by-date (plan)
  "Return a hash table of the net words PLAN's ledger records on each date.
The first entry is measured against the plan's pre-existing words."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (delta (org-scribe-planner--daily-deltas
                    (org-scribe-plan-daily-word-counts plan)
                    (org-scribe-plan-preexisting-words plan)))
      (puthash (car delta) (cdr delta) table))
    table))

(defvar org-scribe-planner--goal-table-cache nil
  "One-entry cache for `org-scribe-planner--day-goal': (SCHEDULE . TABLE).")

(defun org-scribe-planner--day-goal (plan date)
  "Return PLAN's goal for DATE, or nil when DATE is outside the plan.
Looks the date up in a table cached on the schedule, so asking for every
day of a plan stays linear."
  (when (and (org-scribe-plan-start-date plan)
             (plist-get (org-scribe-planner--build-schedule plan) :rows))
    (let ((schedule (org-scribe-planner--generate-day-schedule plan)))
      (unless (eq (car org-scribe-planner--goal-table-cache) schedule)
        (let ((table (make-hash-table :test #'equal :size (length schedule))))
          (dolist (day schedule)
            (puthash (plist-get day :date) (plist-get day :words) table))
          (setq org-scribe-planner--goal-table-cache (cons schedule table))))
      (gethash date (cdr org-scribe-planner--goal-table-cache)))))

(defun org-scribe-planner--elapsed-daily-words (plan)
  "Return PLAN's elapsed writing days as (DATE . WORDS), in date order.
The days the writing pace is measured over, as in wordplanner: every
plan day up to yesterday that isn't a rest day (missed days included,
with 0 words), plus every day with words written — today once you've
written, rest days you wrote on, days outside the plan."
  (let* ((today (org-scribe-planner--get-today-date))
         (written (org-scribe-planner--words-by-date plan))
         (days nil))
    (when (and (org-scribe-plan-start-date plan)
               (plist-get (org-scribe-planner--build-schedule plan) :rows))
      (dolist (day (org-scribe-planner--generate-day-schedule plan))
        (when (and (string< (plist-get day :date) today)
                   (not (plist-get day :is-spare-day)))
          (push (plist-get day :date) days))))
    (maphash (lambda (date words) (when (> words 0) (push date days))) written)
    (mapcar (lambda (date) (cons date (gethash date written 0)))
            (sort (delete-dups days) #'string<))))

(defun org-scribe-planner--average-pace (plan)
  "Return PLAN's words written in the plan per elapsed writing day.
Pre-existing words don't count; missed days do, rest days don't (see
`org-scribe-planner--elapsed-daily-words')."
  (let ((days (length (org-scribe-planner--elapsed-daily-words plan))))
    (if (> days 0)
        (/ (float (- (org-scribe-plan-current-words plan)
                     (or (org-scribe-plan-preexisting-words plan) 0)))
           days)
      0.0)))

(defun org-scribe-planner--day-status (plan date &optional written)
  "Return the status of DATE in PLAN, as a symbol.
WRITTEN is the table from `org-scribe-planner--words-by-date' (built when
nil).  The statuses:
  `no-goal'  outside the plan
  `rest'     a rest day (goal 0) with nothing written; writing on one
             is `exceeded'
  `future'   after today, or today below its goal (today isn't over:
             it isn't due, and the words so far can still grow)
  `exceeded' more than the day's goal
  `met'      the day's goal
  `partial'  75% of the goal or more
  `missed'   less than 75%, nothing, or fewer words than the day before
             (a net cut)"
  (let* ((goal (org-scribe-planner--day-goal plan date))
         (written (or written (org-scribe-planner--words-by-date plan)))
         (words (gethash date written))
         (today (org-scribe-planner--get-today-date)))
    (cond
     ((null goal) 'no-goal)
     ((zerop goal) (if (and words (> words 0)) 'exceeded 'rest))
     ((string< today date) 'future)
     ((and (string= date today) (< (or words 0) goal)) 'future)
     ((> (or words 0) goal) 'exceeded)
     ((= (or words 0) goal) 'met)
     ((>= (or words 0) (* 0.75 goal)) 'partial)
     (t 'missed))))

(defun org-scribe-planner--rest-after-end-p (plan date)
  "Return non-nil when DATE, after PLAN's end, would be a rest day.
Spare dates, and weekend days when weekends are off."
  (or (member date (org-scribe-plan-spare-days plan))
      (and (eq (org-scribe-plan-weekend-mode plan) 'rest)
           (org-scribe-planner--weekend-p plan date))))

(defun org-scribe-planner--completion-date (plan pace)
  "Return the date PLAN's goal is reached writing PACE words a writing day.
Spends PACE on each coming writing day, from today (tomorrow when today
is already logged), skipping rest days: the plan's own, then spare days
and weekends off after its end.  Today's date when the goal is already
reached; nil without a positive PACE."
  (let ((remaining (- (org-scribe-plan-total-words plan)
                      (org-scribe-plan-current-words plan))))
    (cond
     ((<= remaining 0) (org-scribe-planner--get-today-date))
     ((not (and pace (> pace 0))) nil)
     (t
      (let* ((end (org-scribe-plan-end-date plan))
             (rest (make-hash-table :test #'equal))
             (date (org-scribe-planner--default-recalculation-date plan))
             (found nil)
             (count 0))
        (dolist (day (org-scribe-planner--generate-day-schedule plan))
          (when (plist-get day :is-spare-day)
            (puthash (plist-get day :date) t rest)))
        (while (and (not found) (< count org-scribe-planner--max-days))
          (unless (if (and end (not (string< end date)))
                      (gethash date rest)
                    (org-scribe-planner--rest-after-end-p plan date))
            (setq remaining (- remaining pace))
            (when (<= remaining 1e-9) (setq found date)))
          (setq date (org-scribe-planner--add-days date 1)
                count (1+ count)))
        found)))))

;;; Progress analytics

;; Figures computed from a plan for the dashboards and the companion's
;; progress page; neither draws anything.

(defun org-scribe-planner--get-today-target (plan)
  "Get the target word count for today from PLAN.
Returns nil if today is not in the plan or is a spare day."
  (let* ((today (org-scribe-planner--get-today-date))
         (schedule (org-scribe-planner--generate-day-schedule plan))
         (today-entry (cl-find today schedule
                              :key (lambda (day) (plist-get day :date))
                              :test 'string=)))
    (when today-entry
      (if (plist-get today-entry :is-spare-day)
          0
        (plist-get today-entry :words)))))

(defun org-scribe-planner--get-today-actual (plan)
  "Get the actual word count written today from PLAN.
Returns nil if no data has been logged for today."
  (let ((today (org-scribe-planner--get-today-date))
        (daily-counts (org-scribe-plan-daily-word-counts plan)))
    (org-scribe-planner--entry-delta daily-counts today (org-scribe-plan-preexisting-words plan))))

(defun org-scribe-planner--calculate-current-streak (plan)
  "Calculate current and longest writing streaks for PLAN.
Returns plist with :current :longest and :last-streak-date.  Only the
plan's writing days up to today count; rest days neither extend nor
break a streak."
  (let* ((daily-counts (org-scribe-plan-daily-word-counts plan))
         (schedule (org-scribe-planner--generate-day-schedule plan))
         (today (org-scribe-planner--get-today-date))
         (current-streak 0)
         (longest-streak 0)
         (temp-streak 0)
         (streak-at-last 0)
         (last-streak-date nil))

    ;; Sort daily counts by date
    (setq daily-counts (sort (copy-sequence daily-counts)
                            (lambda (a b) (string< (car a) (car b)))))

    ;; Calculate streaks
    (dolist (day schedule)
      (let* ((date (plist-get day :date))
             (is-spare (plist-get day :is-spare-day))
             (delta (org-scribe-planner--entry-delta daily-counts date (org-scribe-plan-preexisting-words plan)))
             (has-words (and delta (> delta 0))))

        ;; Only count working days (not spare days), up to today: a day
        ;; to come has no words yet and must not break the streak.  Nor
        ;; does today before anything is written: the day isn't over.
        (unless (or is-spare
                    (string< today date)
                    (and (string= date today) (not has-words)))
          (if has-words
              (progn
                (setq temp-streak (1+ temp-streak))
                (setq last-streak-date date)
                (setq streak-at-last temp-streak)
                (when (> temp-streak longest-streak)
                  (setq longest-streak temp-streak)))
            ;; Break in streak
            (setq temp-streak 0)))))

    ;; Current streak is the run ending on the last written day, kept
    ;; while that day is within the grace window: the missed days after
    ;; it have zeroed TEMP-STREAK already, so it can't be read here.
    (setq current-streak
          (if (and last-streak-date
                  (or (string= last-streak-date today)
                      (< (org-scribe-planner--days-between last-streak-date today)
                         (1+ org-scribe-planner-streak-grace-days))))
              streak-at-last
            0))

    (list :current current-streak
          :longest longest-streak
          :last-date last-streak-date)))

(defun org-scribe-planner--calculate-velocity (plan)
  "Calculate velocity metrics for PLAN.
Returns a plist:
  :average        words written in the plan per elapsed writing day
                  (missed days count, rest days don't; see
                  `org-scribe-planner--average-pace')
  :recent         the same over the last 7 elapsed writing days
  :trend          \"accelerating\", \"slowing\", \"steady\" or \"no data\"
  :eta            when the goal is reached at the :average pace
  :projected-date when it is reached at the :recent pace
  :days-logged    days with words written
  :elapsed-days   elapsed writing days
The two dates skip rest days (see `org-scribe-planner--completion-date')."
  (let* ((elapsed (org-scribe-planner--elapsed-daily-words plan))
         (average-velocity (org-scribe-planner--average-pace plan))
         (recent-words (mapcar #'cdr (last elapsed (min 7 (length elapsed)))))
         (recent-velocity (if recent-words
                              (/ (float (apply #'+ recent-words)) (length recent-words))
                            0))
         (trend (cond
                 ((= average-velocity 0) "no data")
                 ((> recent-velocity (* 1.1 average-velocity)) "accelerating")
                 ((< recent-velocity (* 0.9 average-velocity)) "slowing")
                 (t "steady"))))
    (list :average average-velocity
          :recent recent-velocity
          :trend trend
          :eta (org-scribe-planner--completion-date plan average-velocity)
          :projected-date (org-scribe-planner--completion-date plan recent-velocity)
          :days-logged (cl-count-if (lambda (day) (> (cdr day) 0)) elapsed)
          :elapsed-days (length elapsed))))

(defun org-scribe-planner--calculate-schedule-position (plan)
  "Calculate if PLAN is ahead or behind schedule.
Returns plist with :status :days-ahead :words-ahead :current-words :percentage :days-elapsed :days-remaining.
:expected-by-today is where the manuscript should stand at the end of
today, as `org-scribe-planner--status' measures it: today's goal counts
already.  :days-elapsed counts the days before today and :days-remaining
the working days from today on."
  (let* ((schedule (org-scribe-planner--generate-day-schedule plan))
         (today (org-scribe-planner--get-today-date))
         ;; current-words is kept in sync with the latest cumulative ledger
         ;; entry by every write path, so it IS the total written so far.
         (cumulative-actual (org-scribe-plan-current-words plan))
         (status (org-scribe-planner--status plan))
         ;; Where the manuscript should stand at the end of today
         (expected-by-today (or (plist-get status :expected) 0))
         (expected-days 0)
         (days-elapsed 0)
         (days-remaining 0))

    ;; Calculate expected words and days elapsed, up to yesterday
    (dolist (day schedule)
      (let* ((date (plist-get day :date))
             (is-spare (plist-get day :is-spare-day)))

        (when (string< date today)
          ;; Count all days elapsed (including spare days)
          (setq days-elapsed (1+ days-elapsed))

          (unless is-spare
            (setq expected-days (1+ expected-days))))))

    ;; Calculate remaining days (from today onwards, excluding spare days)
    (dolist (day schedule)
      (let* ((date (plist-get day :date))
             (is-spare (plist-get day :is-spare-day)))
        (when (and (not (string< date today))
                  (not is-spare))
          (setq days-remaining (1+ days-remaining)))))

    (let* ((total-words (org-scribe-plan-total-words plan))
           (percentage (if (> total-words 0)
                          (/ (* 100.0 cumulative-actual) total-words)
                        0))
           (words-ahead (- cumulative-actual expected-by-today))
           ;; Writing days ahead along the schedule, not words ÷ the
           ;; current daily target (which changes with every recalculation)
           (days-ahead (or (plist-get status :days) 0))
           (status (cond
                   ((>= words-ahead 0) 'ahead)
                   (t 'behind))))

      ;; :status stays two-valued for the dashboards; :on-track is exact
      (list :status status
            :on-track (= words-ahead 0)
            :days-ahead days-ahead
            :words-ahead words-ahead
            :cumulative-actual cumulative-actual
            :current-words cumulative-actual
            :expected-by-today expected-by-today
            :percentage percentage
            :days-elapsed days-elapsed
            :days-remaining days-remaining))))

(defun org-scribe-planner--moving-average (values window)
  "Calculate WINDOW-day moving average of VALUES.
VALUES is a list of numbers, WINDOW is the number of days to average over."
  (let ((result nil))
    (dotimes (i (length values))
      (let* ((start (max 0 (- i (1- window))))
             (window-values (cl-subseq values start (1+ i)))
             (avg (/ (float (apply #'+ window-values)) (length window-values))))
        (push avg result)))
    (nreverse result)))

(defun org-scribe-planner--calculate-day-of-week-stats (plan)
  "Calculate day-of-week performance statistics for PLAN.
Returns an alist with keys 0-6 (Sunday-Saturday) and values as plists
containing :day-name :total-words :count :average.  Counts every elapsed
writing day (see `org-scribe-planner--elapsed-daily-words'), so a missed
day lowers its weekday's average; rest days aren't counted."
  (let ((dow-data (make-vector 7 nil)))
    (dotimes (i 7)
      (aset dow-data i (list :day-name (calendar-day-name i nil t)
                             :total-words 0
                             :count 0
                             :average 0.0)))
    (dolist (day (org-scribe-planner--elapsed-daily-words plan))
      (let ((dow-entry (aref dow-data (org-scribe-planner--weekday (car day)))))
        (plist-put dow-entry :total-words (+ (plist-get dow-entry :total-words) (cdr day)))
        (plist-put dow-entry :count (1+ (plist-get dow-entry :count)))))
    (dotimes (i 7)
      (let ((dow-entry (aref dow-data i)))
        (when (> (plist-get dow-entry :count) 0)
          (plist-put dow-entry :average
                     (/ (float (plist-get dow-entry :total-words))
                        (plist-get dow-entry :count))))))
    (let ((result nil))
      (dotimes (i 7)
        (push (cons i (aref dow-data i)) result))
      (nreverse result))))

(defun org-scribe-planner--plan-day-statuses (plan)
  "Return the statuses of PLAN's days that are due, as (DATE . STATUS).
Days that are due have a goal and are over, or are today with words
written: their status is `exceeded', `met', `partial' or `missed' (see
`org-scribe-planner--day-status')."
  (let ((written (org-scribe-planner--words-by-date plan))
        (result nil))
    (dolist (day (org-scribe-planner--generate-day-schedule plan))
      (let ((status (org-scribe-planner--day-status plan (plist-get day :date) written)))
        (when (memq status '(exceeded met partial missed))
          (push (cons (plist-get day :date) status) result))))
    (nreverse result)))

(defun org-scribe-planner--calculate-consistency-score (plan)
  "Calculate consistency score for PLAN.
Returns a plist with :score (0-100), :days-logged, :working-days, :rate:
the share of the plan's writing days so far on which some words were
written.  The days are those of `org-scribe-planner--elapsed-daily-words',
as for the velocity: today counts once it has words.  Counting only the
days that are due left today out until its goal was met, so this read
\"4 / 7\" beside the velocity's \"5 of 8\" on the same day."
  (let* ((elapsed (org-scribe-planner--elapsed-daily-words plan))
         (working-days (length elapsed))
         (days-logged (cl-count-if (lambda (day) (> (cdr day) 0)) elapsed))
         (rate (if (> working-days 0)
                   (/ (* 100.0 days-logged) working-days)
                 0)))
    (list :score (round rate)
          :days-logged days-logged
          :working-days working-days
          :rate rate)))

(defun org-scribe-planner--calculate-target-achievement-rate (plan)
  "Calculate target achievement rate for PLAN.
Returns a plist with :rate, :days-met, :days-partial, :days-missed,
:total-days, over the plan's writing days so far (see
`org-scribe-planner--plan-day-statuses'): met counts the days at or over
their goal, partial those at 75% or more, missed the rest — days with no
words and net cuts included."
  (let* ((statuses (mapcar #'cdr (org-scribe-planner--plan-day-statuses plan)))
         (days-met (cl-count-if (lambda (s) (memq s '(exceeded met))) statuses))
         (days-partial (cl-count 'partial statuses))
         (days-missed (cl-count 'missed statuses))
         (total-days (length statuses)))
    (list :rate (if (> total-days 0) (/ (* 100.0 days-met) total-days) 0)
          :days-met days-met
          :days-partial days-partial
          :days-missed days-missed
          :total-days total-days)))

(defun org-scribe-planner--calculate-efficiency-ratio (plan)
  "Calculate efficiency ratio for PLAN.
Returns actual velocity / planned velocity as a percentage."
  (let* ((velocity (org-scribe-planner--calculate-velocity plan))
         (avg-velocity (plist-get velocity :average))
         (planned-velocity (org-scribe-plan-daily-words plan)))

    (if (> planned-velocity 0)
        (* 100.0 (/ avg-velocity planned-velocity))
      0)))

(defun org-scribe-planner--calculate-multi-window-velocity (plan)
  "Calculate velocity across multiple time windows for PLAN.
Returns plist with :7-day :14-day :30-day :overall velocities and trend info."
  (let* ((word-counts (mapcar #'cdr (org-scribe-planner--elapsed-daily-words plan)))
         (total-days (length word-counts)))

    (when (> total-days 0)
      (let* ((overall-avg (/ (float (apply #'+ word-counts)) total-days))
             ;; Calculate 7-day average
             (last-7 (last word-counts (min 7 total-days)))
             (avg-7 (if last-7
                       (/ (float (apply #'+ last-7)) (length last-7))
                     0))
             ;; Calculate 14-day average
             (last-14 (last word-counts (min 14 total-days)))
             (avg-14 (if last-14
                        (/ (float (apply #'+ last-14)) (length last-14))
                      0))
             ;; Calculate 30-day average
             (last-30 (last word-counts (min 30 total-days)))
             (avg-30 (if last-30
                        (/ (float (apply #'+ last-30)) (length last-30))
                      0))
             ;; Determine trend (compare recent to overall)
             (trend-7 (cond
                      ((= overall-avg 0) 'unknown)
                      ((> avg-7 (* 1.1 overall-avg)) 'accelerating)
                      ((< avg-7 (* 0.9 overall-avg)) 'decelerating)
                      (t 'steady)))
             ;; Calculate velocity change rate (7-day vs 14-day)
             (velocity-change (if (and (> avg-14 0) (> total-days 7))
                                (/ (- avg-7 avg-14) avg-14)
                              0)))

        (list :7-day avg-7
              :14-day avg-14
              :30-day avg-30
              :overall overall-avg
              :total-days total-days
              :trend-7 trend-7
              :velocity-change velocity-change
              :acceleration (> velocity-change 0.05)
              :deceleration (< velocity-change -0.05))))))

(defun org-scribe-planner--calculate-required-velocity (plan)
  "Calculate required velocity to complete PLAN on time.
Returns plist with :required-velocity :current-velocity :feasible :adjustment-needed."
  (let* ((total-words (org-scribe-plan-total-words plan))
         (current-words (org-scribe-plan-current-words plan))
         (remaining-words (- total-words current-words))
         (schedule (org-scribe-planner--generate-day-schedule plan))
         (today (org-scribe-planner--get-today-date))
         (remaining-days 0))

    ;; Count remaining working days from today onwards
    (dolist (day schedule)
      (let ((date (plist-get day :date))
            (is-spare (plist-get day :is-spare-day)))
        (when (and (not (string< date today))
                  (not is-spare))
          (setq remaining-days (1+ remaining-days)))))

    (let* ((required-velocity (if (> remaining-days 0)
                                 (/ (float remaining-words) remaining-days)
                               0))
           (velocity (org-scribe-planner--calculate-velocity plan))
           (current-velocity (plist-get velocity :recent))
           (planned-velocity (org-scribe-plan-daily-words plan))
           (feasible (or (<= required-velocity (* 1.5 planned-velocity))
                        (<= required-velocity current-velocity)))
           (adjustment-needed (- required-velocity current-velocity)))

      (list :required-velocity required-velocity
            :current-velocity current-velocity
            :planned-velocity planned-velocity
            :remaining-words remaining-words
            :remaining-days remaining-days
            :feasible feasible
            :adjustment-needed adjustment-needed
            :adjustment-percent (if (> current-velocity 0)
                                   (* 100 (/ adjustment-needed current-velocity))
                                 0)))))

(defun org-scribe-planner--calculate-momentum-score (plan)
  "Calculate momentum score for PLAN based on recent velocity trends.
Returns a score from 0-100 indicating writing momentum."
  (let* ((multi-vel (org-scribe-planner--calculate-multi-window-velocity plan))
         (avg-7 (plist-get multi-vel :7-day))
         (avg-14 (plist-get multi-vel :14-day))
         (overall (plist-get multi-vel :overall))
         (planned (org-scribe-plan-daily-words plan)))

    (when (and avg-7 avg-14 overall)
      (let* (;; Base score: how well are we doing vs plan?
             (performance-score (if (> planned 0)
                                   (min 100 (* 100 (/ avg-7 planned)))
                                 50))
             ;; Trend bonus: are we accelerating?
             (trend-bonus (cond
                          ((> avg-7 (* 1.2 overall)) 20)
                          ((> avg-7 (* 1.1 overall)) 10)
                          ((< avg-7 (* 0.8 overall)) -20)
                          ((< avg-7 (* 0.9 overall)) -10)
                          (t 0)))
             ;; Consistency bonus: 7-day close to 14-day?
             (consistency-bonus (if (and (> avg-14 0)
                                        (< (abs (- avg-7 avg-14)) (* 0.2 avg-14)))
                                   10
                                 0))
             (total-score (+ performance-score trend-bonus consistency-bonus)))

        (max 0 (min 100 (round total-score)))))))

;;; Words outside the manuscript

(defun org-scribe-planner--count-file-words (file)
  "Return the number of words in FILE, or nil when it can't be read.
A directory is not a file: `file-readable-p' is true for one, and the
file picker's default answer is the project directory, so accepting it
used to signal \"Is a directory\" and abort the whole new-plan wizard,
losing everything already typed.
Org files are counted like the manuscript — without comments, drawers,
headings, properties, tables or blocks (`org-scribe--count-words-region',
which uses `org-context-count-words') — other files by plain words."
  (when (and (file-readable-p file) (not (file-directory-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (if (and (string-match-p "\\.org\\'" file)
               (fboundp 'org-scribe--count-words-region))
          (progn (delay-mode-hooks (org-mode))
                 (org-scribe--count-words-region (point-min) (point-max)))
        (count-words (point-min) (point-max))))))

(defun org-scribe-planner--outside-base (&optional plan-file)
  "Return the directory outside files are relative to: PLAN-FILE's.
Falls back to the active plan's file, then the project root, then
`default-directory'."
  (file-name-as-directory
   (expand-file-name
    (or (and plan-file (file-name-directory plan-file))
        (and org-scribe-planner--current-plan-file
             (file-name-directory org-scribe-planner--current-plan-file))
        (ignore-errors (org-scribe-project-root))
        default-directory))))

(defun org-scribe-planner--outside-file-words (entry base)
  "Return the words OUTSIDE-FILES ENTRY counts: recounted when live.
BASE is the directory its path is relative to."
  (if (plist-get entry :live)
      (or (org-scribe-planner--count-file-words
           (expand-file-name (plist-get entry :path) base))
          0)
    (or (plist-get entry :count) 0)))

(defun org-scribe-planner--outside-total (plan &optional base)
  "Return PLAN's words outside the manuscript: typed, plus its files.
Files counted on every sync are counted now.  BASE is the directory
their paths are relative to (see `org-scribe-planner--outside-base')."
  (let ((base (or base (org-scribe-planner--outside-base))))
    (+ (or (org-scribe-plan-outside-words plan) 0)
       (apply #'+ (mapcar (lambda (entry) (org-scribe-planner--outside-file-words entry base))
                          (org-scribe-plan-outside-files plan))))))

(defun org-scribe-planner--written-total (_plan manuscript &optional _base)
  "Return the written total for a MANUSCRIPT count: the count itself.
This is what the ledger (DAILY_WORD_COUNTS) and CURRENT_WORDS hold.
Words outside the manuscript are reported by
`org-scribe-planner--outside-total', never added.  The arguments after
MANUSCRIPT are kept so every caller goes through this one seam."
  manuscript)

(defun org-scribe-planner--warn-if-below-start (plan)
  "Warn when PLAN's written total is below its pre-existing words."
  (let ((preexisting (or (org-scribe-plan-preexisting-words plan) 0)))
    (when (< (org-scribe-plan-current-words plan) preexisting)
      (message "The plan counts %d words, fewer than the %d it started with."
               (org-scribe-plan-current-words plan) preexisting))))

(defun org-scribe-planner--convert-to-manuscript-basis (plan base)
  "Convert PLAN, saved when outside words were counted, to manuscript-only.
Does nothing when `words-basis' is already `manuscript'.  Otherwise the
outside words (see `org-scribe-planner--outside-total', with BASE) are
subtracted from every ledger entry, CURRENT_WORDS and PREEXISTING_WORDS,
so no day shows a jump.  Return the number of words shifted, or nil when
nothing moved.  Approximation: a live outside file is subtracted at its
current size, not the size it had on each past day."
  (unless (eq (org-scribe-plan-words-basis plan) 'manuscript)
    (let ((outside (org-scribe-planner--outside-total plan base)))
      (when (> outside 0)
        (org-scribe-planner--shift-written plan (- outside) t))
      (setf (org-scribe-plan-words-basis plan) 'manuscript)
      (and (> outside 0) outside))))

(defun org-scribe-planner--outside-line (plan &optional base)
  "Return the line saying PLAN's outside words are not counted, or nil if none."
  (let ((outside (org-scribe-planner--outside-total plan base)))
    (when (> outside 0)
      (format "Outside the manuscript: %d words (not counted)" outside))))

(defun org-scribe-planner--manuscript-file ()
  "Return the current project's manuscript file, or nil."
  (when (fboundp 'org-scribe-project-structure)
    (plist-get (ignore-errors (org-scribe-project-structure)) :manuscript-file)))

(defun org-scribe-planner--under-directory-p (file directory)
  "Return non-nil when FILE's path lies inside DIRECTORY's, by name."
  (string-prefix-p (file-name-as-directory (expand-file-name directory))
                   (expand-file-name file)))

(defun org-scribe-planner--outside-path (file base)
  "Return how FILE is stored: relative to BASE when inside it, else abbreviated."
  (let ((file (expand-file-name file)))
    (if (org-scribe-planner--under-directory-p file base)
        (file-relative-name file base)
      (abbreviate-file-name file))))

(defun org-scribe-planner--add-outside-file (plan base)
  "Ask for a file outside the manuscript and add it to PLAN's outside files.
Refuses the manuscript itself, a file already there and unreadable
files.  Asks whether to count it on every sync (a draft you keep adding
to; the default for files inside the project) or once (the default for
files elsewhere).  Returns the entry added, or nil."
  (let* ((file (expand-file-name (read-file-name "File to count: " base nil t)))
         (manuscript (org-scribe-planner--manuscript-file))
         (path (org-scribe-planner--outside-path file base))
         (count (org-scribe-planner--count-file-words file)))
    (cond
     ((and manuscript (file-equal-p file manuscript))
      (message "That's the manuscript: its words are already counted.")
      nil)
     ((cl-find path (org-scribe-plan-outside-files plan)
               :key (lambda (e) (plist-get e :path)) :test #'equal)
      (message "%s is already counted." path)
      nil)
     ((null count)
      (message "Can't read %s." file)
      nil)
     (t
      (let* ((inside (org-scribe-planner--under-directory-p file base))
             (live-label "On every sync (a draft you keep adding to)")
             (once-label (format "Once: %d words now" count))
             (choice (completing-read (format "Count %s (%d words): " path count)
                                      (list live-label once-label) nil t nil nil
                                      (if inside live-label once-label)))
             (entry (if (string= choice live-label)
                        (list :path path :live t)
                      (list :path path :count count))))
        (setf (org-scribe-plan-outside-files plan)
              (append (org-scribe-plan-outside-files plan) (list entry)))
        entry)))))

(defun org-scribe-planner--describe-outside (plan &optional base)
  "Describe PLAN's words outside the manuscript in a few words."
  (let ((typed (or (org-scribe-plan-outside-words plan) 0))
        (files (org-scribe-plan-outside-files plan)))
    (if (and (zerop typed) (null files))
        "none"
      (string-join
       (delq nil
             (cons (unless (zerop typed) (format "%d typed" typed))
                   (mapcar (lambda (entry)
                             (format "%s: %d%s" (plist-get entry :path)
                                     (org-scribe-planner--outside-file-words
                                      entry (or base (org-scribe-planner--outside-base)))
                                     (if (plist-get entry :live) " (every sync)" " (counted once)")))
                           files)))
       ", "))))

(defun org-scribe-planner--rebase-outside-paths (plan old-base new-base)
  "Re-anchor PLAN's outside file paths from OLD-BASE to NEW-BASE."
  (unless (equal (expand-file-name old-base) (expand-file-name new-base))
    (setf (org-scribe-plan-outside-files plan)
          (mapcar (lambda (entry)
                    (plist-put (copy-sequence entry) :path
                               (org-scribe-planner--outside-path
                                (expand-file-name (plist-get entry :path) old-base)
                                (file-name-as-directory (expand-file-name new-base)))))
                  (org-scribe-plan-outside-files plan)))))

(defun org-scribe-planner--shift-written (plan difference &optional starting-words)
  "Add DIFFERENCE to every word count PLAN has recorded.
Every ledger entry and CURRENT_WORDS move by the same amount, so no day
shows a jump.  With STARTING-WORDS, the pre-existing words move too:
the words were written before the plan, so they aren't progress."
  (dolist (entry (org-scribe-plan-daily-word-counts plan))
    (when (numberp (plist-get (cdr entry) :words))
      (setf (cdr entry) (plist-put (copy-sequence (cdr entry)) :words
                                   (+ (plist-get (cdr entry) :words) difference)))))
  (setf (org-scribe-plan-current-words plan)
        (+ (org-scribe-plan-current-words plan) difference))
  (when starting-words
    (setf (org-scribe-plan-preexisting-words plan)
          (+ (or (org-scribe-plan-preexisting-words plan) 0) difference))))

;;; Locked past goals and recalculation from progress

(defun org-scribe-planner--lock-past-goals (plan &optional as-of)
  "Freeze the goal of every day of PLAN before AS-OF (default: today).
Once locked (LOCKED_GOALS), a day's goal never changes again, whatever
is changed later: total, daily target, days, spare days or a
recalculation.  Today isn't locked until it's over.  Days already locked
keep their goal.  Returns the number of days newly locked."
  (let ((as-of (or as-of (org-scribe-planner--get-today-date)))
        (newly nil))
    (when (and (org-scribe-plan-start-date plan)
               (plist-get (org-scribe-planner--build-schedule plan) :rows))
      (dolist (day (org-scribe-planner--generate-day-schedule plan))
        (let ((date (plist-get day :date)))
          (when (and (string< date as-of)
                     (not (assoc date (org-scribe-plan-locked-goals plan))))
            (push (cons date (plist-get day :words)) newly))))
      (when newly
        (setf (org-scribe-plan-locked-goals plan)
              (sort (append (copy-sequence (org-scribe-plan-locked-goals plan)) newly)
                    (lambda (a b) (string< (car a) (car b)))))))
    (length newly)))

(defun org-scribe-planner--default-recalculation-date (plan)
  "Return the first day a recalculation from progress may change in PLAN.
Today, or tomorrow when words have been written today (a net change,
cuts included): the day has started, so it is locked with the past.
An entry for today with no change is not a start: a count synced on
save records one before any writing."
  (let* ((today (org-scribe-planner--get-today-date))
         (delta (org-scribe-planner--entry-delta
                 (org-scribe-plan-daily-word-counts plan) today
                 (or (org-scribe-plan-preexisting-words plan) 0))))
    (if (and delta (/= delta 0))
        (org-scribe-planner--add-days today 1)
      today)))

(defun org-scribe-planner--rebase-on-progress (plan as-of)
  "Lock PLAN's days before AS-OF and point its open days at the words left.
Locked days keep their planned goals, so without an adjustment the open
days would cover the total minus the goals of the past, not what is
really left.  The schedule adjustment bridges the two: it is the locked
goals minus the words credited to them minus the words written elsewhere.

Every locked day is credited with what was written on it, today
included when AS-OF is tomorrow: the open days cover exactly the words
left.  Words written today after the recalculation make it out of date
\(see `org-scribe-planner--rebase-stale').  Words not attributed to a
locked day (other days, or a count synced without a daily entry) count as
written elsewhere.  Returns the number of days newly locked."
  (let* ((newly (org-scribe-planner--lock-past-goals plan as-of))
         (today (org-scribe-planner--get-today-date))
         (preexisting (or (org-scribe-plan-preexisting-words plan) 0))
         (deltas (org-scribe-planner--daily-deltas
                  (org-scribe-plan-daily-word-counts plan) preexisting))
         (written-total (- (org-scribe-plan-current-words plan) preexisting))
         (locked-words 0)
         (written-on-locked 0))
    (dolist (day (org-scribe-planner--generate-day-schedule plan))
      (when (plist-get day :locked)
        (setq locked-words (+ locked-words (plist-get day :words))
              written-on-locked (+ written-on-locked
                                   (or (cdr (assoc (plist-get day :date) deltas)) 0)))))
    ;; Locked goals minus the words credited to them (WRITTEN-ON-LOCKED)
    ;; minus the words written elsewhere
    (setf (org-scribe-plan-schedule-adjustment plan)
          (- locked-words written-on-locked (- written-total written-on-locked)))
    (setf (org-scribe-plan-rebased-from plan) as-of
          (org-scribe-plan-rebased-on plan) today
          (org-scribe-plan-rebased-words plan)
          (org-scribe-planner--written-through plan (org-scribe-planner--add-days as-of -1)))
    newly))

;;; Date ranges and weekends

(defun org-scribe-planner--generate-date-range (start-date end-date)
  "Generate a list of dates from START-DATE to END-DATE, both included.
Steps by date strings, not by adding seconds to a time: after a change
to summer time, noon plus 24 hours is 13:00, which compared as later
than END-DATE's noon and dropped the last day of any range crossing it."
  (let ((date start-date)
        (dates nil))
    (while (not (string< end-date date))
      (push date dates)
      (setq date (org-scribe-planner--add-days date 1)))
    (nreverse dates)))

(defun org-scribe-planner--get-weekends (plan)
  "Get all weekend dates (Saturday and Sunday) for PLAN, sorted chronologically."
  (sort (append (org-scribe-planner--get-day-of-week plan 6)
                (org-scribe-planner--get-day-of-week plan 0))
        #'string<))

(defun org-scribe-planner--get-day-of-week (plan day-number)
  "Get all dates in PLAN that fall on DAY-NUMBER (0=Sunday, 6=Saturday)."
  (unless (and (org-scribe-plan-start-date plan) (org-scribe-plan-days plan))
    (error "Start date and days must be set first"))

  (let ((current (org-scribe-planner--parse-date (org-scribe-plan-start-date plan)))
        (days-remaining (org-scribe-plan-days plan))
        (dates nil))

    (dotimes (_ days-remaining)
      (when (= (string-to-number (format-time-string "%w" current)) day-number)
        (push (org-scribe-planner--date-to-string current) dates))
      (setq current (time-add current (days-to-time 1))))

    (nreverse dates)))

;;; Helper Functions for Daily Word Counts

(defun org-scribe-planner--get-entry-words (entry)
  "Extract the raw stored :words value from daily-word-count ENTRY.
ENTRY must be in new format: (date . (:words N :note \"...\" :target M)).
N is the manuscript's CUMULATIVE word total as of that date, not that
day's own delta — for the actual net words written on a given date, use
`org-scribe-planner--entry-delta' instead."
  (plist-get (cdr entry) :words))

(defun org-scribe-planner--get-entry-note (entry)
  "Extract note from daily-word-count ENTRY.
ENTRY must be in new format: (date . (:words N :note \"...\" :target M))."
  (or (plist-get (cdr entry) :note) ""))

(defun org-scribe-planner--counts-with-words (daily-counts)
  "Return only entries from DAILY-COUNTS that have a numeric word count.
Filters out note-only entries, which carry a note string but no :words field."
  (cl-remove-if-not
   (lambda (entry)
     (numberp (org-scribe-planner--get-entry-words entry)))
   daily-counts))

(defun org-scribe-planner--sorted-daily-counts (daily-counts)
  "Return DAILY-COUNTS entries with a numeric :words value, sorted by date.
Ascending chronological order.  Note-only entries (no :words) are
excluded, same as `org-scribe-planner--counts-with-words'."
  (sort (copy-sequence (org-scribe-planner--counts-with-words daily-counts))
        (lambda (a b) (string< (car a) (car b)))))

(defun org-scribe-planner--previous-cumulative-total (daily-counts date &optional baseline)
  "Return the cumulative total recorded strictly before DATE in DAILY-COUNTS.
Looks only at entries with a numeric :words value (real cumulative data
points).  Returns BASELINE (default 0) when DATE is the earliest entry or
no earlier entry exists — the plan's starting count, i.e. its
`org-scribe-plan-preexisting-words'."
  (let* ((sorted (org-scribe-planner--sorted-daily-counts daily-counts))
         (before (seq-take-while (lambda (e) (string< (car e) date)) sorted)))
    (if before
        (plist-get (cdr (car (last before))) :words)
      (or baseline 0))))

(defun org-scribe-planner--daily-deltas (daily-counts &optional baseline)
  "Return DAILY-COUNTS as a chronological list of (DATE . DELTA).
DAILY-COUNTS entries with a numeric :words value hold the manuscript's
cumulative total as of that date (see the `org-scribe-plan-daily-word-counts'
slot docstring).  DELTA is each entry's cumulative total minus the
previous entry's — the actual net words written on that date.  The
earliest entry is measured against BASELINE (default 0): pass the plan's
`org-scribe-plan-preexisting-words', so words written before the plan
don't count as the first day's writing."
  (let ((previous (or baseline 0))
        (result nil))
    (dolist (entry (org-scribe-planner--sorted-daily-counts daily-counts))
      (let ((total (plist-get (cdr entry) :words)))
        (push (cons (car entry) (- total previous)) result)
        (setq previous total)))
    (nreverse result)))

(defun org-scribe-planner--entry-delta (daily-counts date &optional baseline)
  "Return the net words written on DATE, derived from cumulative DAILY-COUNTS.
BASELINE is the count before the earliest entry, as for
`org-scribe-planner--daily-deltas'.  Returns nil when DATE has no
recorded cumulative total (no entry, or a note-only entry)."
  (cdr (assoc date (org-scribe-planner--daily-deltas daily-counts baseline))))

(defun org-scribe-planner--add-spare-day-note (plan date note)
  "Add or update a note for a spare day in PLAN.
DATE should be in YYYY-MM-DD format.
NOTE is the text to associate with this spare day.
Creates an entry in daily-word-counts with only the note (no word count).
This allows tracking notes for spare days without affecting cumulative word counts."
  (let* ((daily-counts (org-scribe-plan-daily-word-counts plan))
         (existing-entry (assoc date daily-counts)))
    (if existing-entry
        ;; Update existing entry to add/replace note (preserve :words if it exists)
        (let ((existing-words (plist-get (cdr existing-entry) :words)))
          ;; Only preserve :words if it was explicitly set (not for note-only entries)
          (if (numberp existing-words)
              (setcdr existing-entry (list :words existing-words :note note))
            ;; Note-only entry, don't set :words
            (setcdr existing-entry (list :note note))))
      ;; Create new note-only entry for spare day (no :words field)
      (push (cons date (list :note note))
            (org-scribe-plan-daily-word-counts plan)))))

(defun org-scribe-planner--remove-spare-day-note (plan date)
  "Remove the note-only entry for DATE from PLAN's daily-word-counts.
Only removes the entry when it carries no actual word count — i.e. it was
created solely by `org-scribe-planner--add-spare-day-note'.  Entries that
also contain a :words value (words actually written on that day) are left
intact so real progress data is never discarded."
  (let* ((daily-counts (org-scribe-plan-daily-word-counts plan))
         (entry (assoc date daily-counts)))
    (when (and entry (not (numberp (plist-get (cdr entry) :words))))
      (setf (org-scribe-plan-daily-word-counts plan)
            (assoc-delete-all date daily-counts)))))

;;; A total for a daily target

(defun org-scribe-planner--set-total-keeping-daily (plan daily)
  "Set PLAN's total to what DAILY words a day give over its writing days."
  (setf (org-scribe-plan-total-words plan)
        (round (org-scribe-planner--total-for-daily plan daily)))
  (org-scribe-planner--sync-daily plan))

;;; Describing weekends and patterns

(defun org-scribe-planner--describe-weekends (plan)
  "Describe PLAN's weekend setting in a few words."
  (let ((days (mapconcat (lambda (n) (car (rassq n org-scribe-planner--day-names)))
                         (or (org-scribe-plan-weekend-days plan) '(6 0)) ", ")))
    (pcase (org-scribe-plan-weekend-mode plan)
      ('different (format "%d words/day on %s" (org-scribe-plan-weekend-daily plan) days))
      ('rest (format "no writing on %s" days))
      (_ "same as other days"))))

(defun org-scribe-planner--describe-pattern (plan)
  "Describe PLAN's writing pattern in a few words."
  (let ((pattern (or (org-scribe-plan-pattern plan) 'flat)))
    (if (eq pattern 'flat)
        "steady"
      (let ((intensity (car (cl-find-if
                             (lambda (preset)
                               (cl-every (lambda (key)
                                           (equal (plist-get (cdr preset) key)
                                                  (plist-get (org-scribe-plan-pattern-config plan) key)))
                                         '(:peak :valley :start :end :min :max)))
                             (cdr (assq pattern org-scribe-planner--pattern-intensities))))))
        (format "%s%s" pattern (if intensity (format " (%s)" intensity) ""))))))

;;; Project title lookup

(defun org-scribe-planner--project-title (root)
  "Return the project title recorded in ROOT/.org-scribe-project, or nil.
The \"# Title:\" line wins over \"# Writing project:\" (the name at
creation), which a title change leaves alone."
  (or (org-scribe--project-marker-get root "Title")
      (org-scribe--project-marker-get root "Writing project")))

(provide 'org-scribe-planner-engine)

;;; org-scribe-planner-engine.el ends here
