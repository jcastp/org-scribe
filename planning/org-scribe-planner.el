;;; org-scribe-planner.el --- Writing planning tool for Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Javier Castilla
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: org, writing, planning, productivity
;; URL: https://codeberg.org/jcastp/org-scribe

;;; Commentary:

;; A comprehensive writing planning tool inspired by pacemaker.press that helps
;; writers calculate, visualize, and manage their writing goals through an
;; interactive interface integrated with Org-mode.
;;
;; Features:
;; - Calculate writing schedules given 2 of 3 variables (total words, daily words, days)
;; - Beautiful calendar visualizations with day-by-day breakdown
;; - Flexible spare days management (holidays, weekends, breaks)
;; - Plan modification and recalculation
;; - Save/load functionality using Org-mode properties
;; - Weekly summaries and milestone tracking
;; - Integration with org-agenda

;;; Code:

(require 'org)
(require 'org-element)
(require 'calendar)
(require 'cl-lib)

;; Dashboard functions will be loaded at the end of this file
(declare-function org-scribe-planner-show-progress-dashboard
                  "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-progress-dashboard-svg
                  "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-burndown
                  "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-burndown-gnuplot
                  "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-burndown-ascii
                  "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-cumulative-progress
                  "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-velocity
                  "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-velocity-chart
                  "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-velocity-trends
                  "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-performance-analytics
                  "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-multi-metric-dashboard
                  "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-split-dashboards
                  "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-heatmap
                  "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-dashboards-menu
                  "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-dashboard-mode
                  "org-scribe-planner-dashboards")

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
  "Number of days without a new entry before a writing streak is broken.
A value of 2 means the streak survives if the last logged entry was
yesterday or the day before today, which accommodates logging words
the morning after you wrote them.  Set to 1 to require an entry
every day without any tolerance."
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
  ;; once.  The written total is the manuscript's count plus these.
  (outside-words 0 :type number)
  (outside-files nil :type list)
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

(defun org-scribe-planner--anchor-expected (anchor day expected)
  "Return the running expected total to use before adding DAY's goal.
ANCHOR is a two-element list the caller keeps across days: its car is
non-nil once a ledger entry has anchored the series on an actual total,
its cadr once today has been reached.  On the first day from today on,
a series no entry has anchored yet switches to the schedule's own
figure, which carries the adjustment of the last recalculation from
progress; otherwise EXPECTED is returned unchanged."
  (if (or (cadr anchor)
          (string< (plist-get day :date) (org-scribe-planner--get-today-date)))
      expected
    (setcar (cdr anchor) t)
    (if (car anchor)
        expected
      (- (plist-get day :cumulative) (plist-get day :words)))))

(defun org-scribe-planner--expected-through (plan date)
  "Return where PLAN's manuscript should stand at the end of DATE.
That is the :cumulative of the last plan day on or before DATE, or the
plan's starting point (pre-existing words, minus the schedule adjustment)
when DATE is before the plan."
  (let ((expected (- (or (org-scribe-plan-preexisting-words plan) 0)
                     (or (org-scribe-plan-schedule-adjustment plan) 0))))
    (dolist (day (org-scribe-planner--generate-day-schedule plan))
      (unless (string< date (plist-get day :date))
        (setq expected (plist-get day :cumulative))))
    expected))

;;; Where the plan stands

(defun org-scribe-planner--writing-day-position (plan words)
  "Return how many writing days of PLAN's schedule WORDS cover, as a fraction.
WORDS is a manuscript total.  Follows each day's own goal, from the
plan's starting point (pre-existing words, on the baseline of the last
recalculation), so a recalculation or a pattern doesn't distort it.
Days without a goal take no position.  Words beyond the end count at the
plan's daily target."
  (let ((cumulative (- (or (org-scribe-plan-preexisting-words plan) 0)
                       (or (org-scribe-plan-schedule-adjustment plan) 0)))
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

(defun org-scribe-planner--status (plan)
  "Return where PLAN stands, or nil when it has no schedule yet.
A plist:
  :status     `ahead', `on-track' or `behind'
  :expected   where the manuscript should stand at the end of yesterday
              (today's goal isn't due until the day is over)
  :actual     the manuscript's count, today's words included
  :difference :actual minus :expected
  :percent    :difference as a percentage of the words the plan expected
              to be written by then (0 before the plan has expected any)
  :days       writing days ahead (positive) or behind (negative),
              measured along the schedule (see
              `org-scribe-planner--writing-day-position')"
  (when (and (org-scribe-plan-start-date plan)
             (plist-get (org-scribe-planner--build-schedule plan) :rows))
    (let* ((yesterday (org-scribe-planner--add-days (org-scribe-planner--get-today-date) -1))
           (expected (org-scribe-planner--expected-through plan yesterday))
           (actual (org-scribe-plan-current-words plan))
           (difference (- actual expected))
           (baseline (- (or (org-scribe-plan-preexisting-words plan) 0)
                        (or (org-scribe-plan-schedule-adjustment plan) 0)))
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
            :days (- (org-scribe-planner--writing-day-position plan actual)
                     (org-scribe-planner--writing-day-position plan expected))))))

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

(defun org-scribe-planner--written-total (plan manuscript &optional base)
  "Return PLAN's written total for a MANUSCRIPT count: manuscript + outside words.
This is what the ledger (DAILY_WORD_COUNTS) and CURRENT_WORDS hold."
  (+ manuscript (org-scribe-planner--outside-total plan base)))

(defun org-scribe-planner--warn-if-below-start (plan)
  "Warn when PLAN's written total is below its pre-existing words.
That is the sign of words the plan was told about but can't see: words
written outside the manuscript that aren't recorded as such."
  (let ((preexisting (or (org-scribe-plan-preexisting-words plan) 0)))
    (when (< (org-scribe-plan-current-words plan) preexisting)
      (message "The plan counts %d words, fewer than the %d it started with. If some are outside the manuscript, add them: What changed? → Words outside the manuscript."
               (org-scribe-plan-current-words plan) preexisting))))

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

(defun org-scribe-planner--read-outside-words (plan base)
  "Ask about PLAN's words outside the manuscript: a typed number, then files.
BASE is the directory file paths are stored relative to."
  (setf (org-scribe-plan-outside-words plan)
        (org-scribe-planner--read-non-negative-number
         "Words written elsewhere, not in the manuscript (0 if none): "
         (or (org-scribe-plan-outside-words plan) 0)))
  (while (y-or-n-p (if (org-scribe-plan-outside-files plan)
                       "Count another file? "
                     "Count the words of files outside the manuscript? "))
    (org-scribe-planner--add-outside-file plan base)))

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

;;; Weeks and views

(defun org-scribe-planner--week-start (date)
  "Return the first day of the week DATE falls in.
Weeks start on `org-scribe-planner-week-start-day'."
  (org-scribe-planner--add-days
   date (- (mod (- (org-scribe-planner--weekday date) org-scribe-planner-week-start-day) 7))))

(defun org-scribe-planner--week-day-names ()
  "Return the two-letter day names in week order, from the first day of the week."
  (mapcar (lambda (i) (substring (calendar-day-name (mod (+ i org-scribe-planner-week-start-day) 7)
                                                     nil t)
                                 0 2))
          (number-sequence 0 6)))

(defun org-scribe-planner--manuscript-total-through (plan date)
  "Return PLAN's manuscript total at the end of DATE.
The last ledger entry on or before DATE, or the pre-existing words before
the first one."
  (let ((total (or (org-scribe-plan-preexisting-words plan) 0)))
    (dolist (entry (org-scribe-planner--sorted-daily-counts
                    (org-scribe-plan-daily-word-counts plan)))
      (unless (string< date (car entry))
        (setq total (plist-get (cdr entry) :words))))
    total))

(defun org-scribe-planner--weekly-summary (plan)
  "Return PLAN's days grouped in calendar weeks, with what was written.
Weeks start on `org-scribe-planner-week-start-day'; the first and last
may be partial.  One plist per week:
  :number :start :end    week number and its first and last plan day
  :writing-days          days with a goal; :rest-days days without
  :goals                 the goals of the writing days
  :goal                  the week's goal
  :started :ended        whether the week has begun / is over
and, for started weeks:
  :written               words written that week (net)
  :days-written          days with words written
  :written-total         the manuscript total at the end of the week
  :planned-total         where it should stand (see below)
  :difference            :written-total minus :planned-total
For the current week :planned-total runs through yesterday, like the
plan's status: today isn't expected until it's over.  A finished week's
leaves out the schedule adjustment of a later recalculation from
progress, so it stays what it was.  As in wordplanner's
`weekly_summary'."
  (let* ((today (org-scribe-planner--get-today-date))
         (yesterday (org-scribe-planner--add-days today -1))
         (written (org-scribe-planner--words-by-date plan))
         (weeks nil)
         (current nil))
    (dolist (day (org-scribe-planner--generate-day-schedule plan))
      (let ((start (org-scribe-planner--week-start (plist-get day :date))))
        (if (and current (string= (car current) start))
            (push day (cdr current))
          (when current (push current weeks))
          (setq current (list start day)))))
    (when current (push current weeks))
    (let ((number 0))
      (mapcar
       (lambda (week)
         (let* ((days (reverse (cdr week)))
                (start (plist-get (car days) :date))
                (end (plist-get (car (last days)) :date))
                (goals (mapcar (lambda (d) (plist-get d :words)) days))
                (started (not (string< today start)))
                (ended (string< end today))
                (result (list :number (setq number (1+ number))
                              :start start :end end
                              :writing-days (cl-count-if #'cl-plusp goals)
                              :rest-days (cl-count 0 goals)
                              :goals (cl-remove-if-not #'cl-plusp goals)
                              :goal (apply #'+ goals)
                              :started started :ended ended
                              :planned-total (org-scribe-planner--expected-through plan end))))
           (when started
             (let* ((before (org-scribe-planner--add-days start -1))
                    (written-total (org-scribe-planner--manuscript-total-through plan end))
                    ;; A finished week is history: it is measured against
                    ;; the plan as it stood, without the adjustment of a
                    ;; later recalculation from progress.  With it, a week
                    ;; that fell behind read as thousands of words ahead
                    ;; once "adjust remaining" had rebased the plan.
                    (planned (if ended
                                 (+ (org-scribe-planner--expected-through plan end)
                                    (or (org-scribe-plan-schedule-adjustment plan) 0))
                               (org-scribe-planner--expected-through plan yesterday))))
               (setq result
                     (append result
                             (list :written (- written-total
                                               (org-scribe-planner--manuscript-total-through
                                                plan before))
                                   :days-written (cl-count-if
                                                  (lambda (d) (> (gethash (plist-get d :date) written 0) 0))
                                                  days)
                                   :written-total written-total
                                   :difference (- written-total planned))))
               (setq result (plist-put result :planned-total planned))))
           result))
       (nreverse weeks)))))

(defun org-scribe-planner--format-week-written (week)
  "Describe WEEK's words written: the count, ✓ ~ ✗, and the share of its goal.
The marks mean what they mean for a day (see
`org-scribe-planner--day-status'): ~ is a partial week, 75% of its goal
or more; below that a finished week is ✗."
  (if (not (plist-get week :started))
      ""
    (let* ((written (plist-get week :written))
           (goal (plist-get week :goal))
           (mark (cond ((zerop goal) "")
                       ((>= written goal) " ✓")
                       ((not (plist-get week :ended)) "")
                       ((>= written (* 0.75 goal)) " ~")
                       (t " ✗"))))
      (format "%d%s%s" written mark
              (if (> goal 0) (format " %d%%" (round (* 100.0 (/ (float written) goal)))) "")))))

;;;###autoload
(defun org-scribe-planner-show-weekly-summary ()
  "Show the active plan week by week, with what was written.
For each calendar week: days written of the writing days, rest days, the
daily goal (a range when the goals vary), the week's goal, the words
written (✓ goal met, ~ part of it, ✗ none) and their share of the goal,
where the manuscript should stand and where it stands, and the
difference.  The current week's difference counts goals through
yesterday.  Weeks start on `org-scribe-planner-week-start-day'."
  (interactive)
  (org-scribe-planner--with-current-plan (plan _)
    (let ((weeks (org-scribe-planner--weekly-summary plan)))
      (with-current-buffer (get-buffer-create "*Writing Plan Weekly*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (org-mode)
          (insert (format "* Weekly summary: %s\n\n" (org-scribe-plan-title plan)))
          (insert (propertize "✓ met  ~ partial (75%+)  ✗ missed\n\n" 'face 'shadow))
          (insert "| Week | Dates | Days written | Rest | Daily goal | Week goal | Written | Planned total | Actual total | +/- |\n|-\n")
          (dolist (week weeks)
            (let ((goals (plist-get week :goals)))
              (insert (format "| %d | %s – %s | %s | %s | %s | %d | %s | %d | %s | %s |\n"
                              (plist-get week :number)
                              (substring (plist-get week :start) 5)
                              (substring (plist-get week :end) 5)
                              (if (plist-get week :started)
                                  (format "%d/%d" (plist-get week :days-written)
                                          (plist-get week :writing-days))
                                (format "-/%d" (plist-get week :writing-days)))
                              (if (> (plist-get week :rest-days) 0) (plist-get week :rest-days) "-")
                              (cond ((null goals) "-")
                                    ((= (apply #'min goals) (apply #'max goals)) (car goals))
                                    (t (format "%d–%d" (apply #'min goals) (apply #'max goals))))
                              (plist-get week :goal)
                              (org-scribe-planner--format-week-written week)
                              (plist-get week :planned-total)
                              (if (plist-get week :started) (plist-get week :written-total) "")
                              (let ((difference (plist-get week :difference)))
                                (cond ((null difference) "")
                                      ((> difference 0) (format "+%d" difference))
                                      (t (number-to-string difference))))))))
          (goto-char (point-min))
          (forward-line 2)
          (org-table-align)
          (goto-char (point-min))
          (setq buffer-read-only t)
          (local-set-key (kbd "q") #'quit-window)
          (display-buffer (current-buffer)))))))

(defconst org-scribe-planner--month-cell-width 9
  "Width of a day's cell in the month grid.")

(defun org-scribe-planner--month-cell (plan date written today)
  "Return the four lines of DATE's cell in PLAN's month grid, as a list.
WRITTEN is the table from `org-scribe-planner--words-by-date'.  The lines:
the day of the month (today in brackets), its goal (or \"rest\"), the
result (✓ met or exceeded, with the words when over; ~ partial, with the
words; ✗ missed), and the manuscript's share of the goal by the end of
the day, for days already logged."
  (let* ((goal (org-scribe-planner--day-goal plan date))
         (status (org-scribe-planner--day-status plan date written))
         (words (gethash date written))
         (number (string-to-number (substring date 8)))
         (face (pcase status
                 ((or 'met 'exceeded) 'org-done)
                 ('partial 'org-scheduled)
                 ('missed 'org-warning)
                 ('rest 'org-agenda-dimmed-todo-face)
                 (_ 'default))))
    (list (if (string= date today)
              (propertize (format "[%d]" number) 'face '(:weight bold :inverse-video t))
            (format "%d" number))
          (cond ((null goal) "")
                ((zerop goal) (propertize "rest" 'face 'org-agenda-dimmed-todo-face))
                (t (format "%dw" goal)))
          (propertize (pcase status
                        ('exceeded (format "✓%d" words))
                        ('met "✓")
                        ('partial (format "~%d" words))
                        ('missed (if (and words (> words 0)) (format "✗%d" words) "✗"))
                        ;; Today, still under way
                        ((guard (and words (string= date today))) (format "%d…" words))
                        (_ ""))
                      'face face)
          (if (and goal words (not (string< today date)))
              (propertize (format "%d%%" (round (* 100.0 (/ (float (org-scribe-planner--manuscript-total-through plan date))
                                                         (org-scribe-plan-total-words plan)))))
                          'face 'shadow)
            ""))))

;;;###autoload
(defun org-scribe-planner-show-month-calendar ()
  "Show the active plan as a month grid.
One grid per month, weeks starting on `org-scribe-planner-week-start-day'.
Each day shows its goal, its result (✓ met, ~ partial, ✗ missed) and the
manuscript's share of the goal by the end of the day; today is in
brackets."
  (interactive)
  (org-scribe-planner--with-current-plan (plan _)
    (let* ((today (org-scribe-planner--get-today-date))
           (written (org-scribe-planner--words-by-date plan))
           (width org-scribe-planner--month-cell-width)
           (pad (lambda (text) (concat text (make-string (max 0 (- width (string-width text))) ?\s))))
           (month (concat (substring (org-scribe-plan-start-date plan) 0 8) "01"))
           (last-month (substring (org-scribe-plan-end-date plan) 0 7)))
      (with-current-buffer (get-buffer-create "*Writing Plan Months*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize (format "Writing Plan: %s\n" (org-scribe-plan-title plan))
                              'face 'org-level-1))
          (insert (propertize "✓ met  ~ partial (75%+)  ✗ missed  [n] today\n\n" 'face 'shadow))
          (while (not (string< last-month (substring month 0 7)))
            (let* ((parts (org-scribe-planner--parse-date-parts month))
                   (month-end (format "%04d-%02d-%02d" (nth 0 parts) (nth 1 parts)
                                      (calendar-last-day-of-month (nth 1 parts) (nth 0 parts))))
                   (day (org-scribe-planner--week-start month)))
              (insert (propertize (format "%s %d\n" (calendar-month-name (nth 1 parts)) (nth 0 parts))
                                  'face 'org-level-2))
              (insert (string-trim-right
                       (mapconcat (lambda (name) (funcall pad name))
                                  (org-scribe-planner--week-day-names) ""))
                      "\n")
              (while (not (string< month-end day))
                (let ((cells (mapcar (lambda (offset)
                                       (let ((date (org-scribe-planner--add-days day offset)))
                                         (if (and (not (string< date month))
                                                  (not (string< month-end date)))
                                             (org-scribe-planner--month-cell plan date written today)
                                           (list "" "" "" ""))))
                                     (number-sequence 0 6))))
                  (dotimes (line 4)
                    (insert (string-trim-right
                             (mapconcat (lambda (cell) (funcall pad (nth line cell))) cells ""))
                            "\n"))
                  (insert "\n")
                  (setq day (org-scribe-planner--add-days day 7))))
              (setq month (format "%04d-%02d-01"
                                  (if (= (nth 1 parts) 12) (1+ (nth 0 parts)) (nth 0 parts))
                                  (1+ (mod (nth 1 parts) 12))))))
          (goto-char (point-min))
          (special-mode)
          (display-buffer (current-buffer)))))))

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
Today, or tomorrow when today already has a word count: the day has
started, so it is locked with the past."
  (let ((today (org-scribe-planner--get-today-date)))
    (if (numberp (org-scribe-planner--get-entry-words
                  (assoc today (org-scribe-plan-daily-word-counts plan))))
        (org-scribe-planner--add-days today 1)
      today)))

(defun org-scribe-planner--rebase-on-progress (plan as-of)
  "Lock PLAN's days before AS-OF and point its open days at the words left.
Locked days keep their planned goals, so without an adjustment the open
days would cover the total minus the goals of the past, not what is
really left.  The schedule adjustment bridges the two: it is the locked
goals minus the words credited to them minus the words written elsewhere.

A locked day before today is credited with what was written on it; a
locked day from today on (AS-OF can be tomorrow) isn't over, so it is
credited with at least its goal.  Words not attributed to a locked day
\(other days, or a count synced without a daily entry) count as written
elsewhere.  Returns the number of days newly locked."
  (let* ((newly (org-scribe-planner--lock-past-goals plan as-of))
         (today (org-scribe-planner--get-today-date))
         (preexisting (or (org-scribe-plan-preexisting-words plan) 0))
         (deltas (org-scribe-planner--daily-deltas
                  (org-scribe-plan-daily-word-counts plan) preexisting))
         (written-total (- (org-scribe-plan-current-words plan) preexisting))
         (locked-words 0)
         (credited 0)
         (written-on-locked 0))
    (dolist (day (org-scribe-planner--generate-day-schedule plan))
      (when (plist-get day :locked)
        (let ((goal (plist-get day :words))
              (written (or (cdr (assoc (plist-get day :date) deltas)) 0)))
          (setq locked-words (+ locked-words goal)
                written-on-locked (+ written-on-locked written)
                credited (+ credited (if (string< (plist-get day :date) today)
                                         written
                                       (max goal written)))))))
    (setf (org-scribe-plan-schedule-adjustment plan)
          (- locked-words credited (- written-total written-on-locked)))
    newly))

;;; New plans: presets and defaults

(defun org-scribe-planner--describe-preset (name preset)
  "Describe PRESET, called NAME, in one line for the preset question."
  (format "%s — %s (%d words %s%s)"
          name (plist-get preset :description) (plist-get preset :total)
          (if (plist-get preset :days)
              (format "in %d days" (plist-get preset :days))
            (format "at %d a day" (plist-get preset :daily)))
          (let ((pattern (plist-get preset :pattern)))
            (if (memq pattern '(nil flat)) "" (format ", %s" pattern)))))

(defun org-scribe-planner--read-preset ()
  "Ask whether to start a new plan from a preset; return (NAME . PLIST) or nil.
Offers `org-scribe-planner-presets' after \"From scratch\".  Inside an
org-scribe project, the first preset made for its type (novel or short
story) is the default answer."
  (when org-scribe-planner-presets
    (let* ((type (ignore-errors (org-scribe-project-type)))
           (labels (mapcar (lambda (preset)
                             (cons (org-scribe-planner--describe-preset (car preset) (cdr preset))
                                   preset))
                           org-scribe-planner-presets))
           ;; The first type a preset lists is its main one: prefer a preset
           ;; made mainly for this type, then any that also suits it
           (suggested (or (cl-find-if (lambda (label)
                                        (eq type (car (plist-get (cddr label) :project-types))))
                                      labels)
                          (cl-find-if (lambda (label)
                                        (memq type (plist-get (cddr label) :project-types)))
                                      labels)))
           (answer (completing-read "Start from: " (cons "From scratch" (mapcar #'car labels))
                                    nil t nil nil
                                    (if suggested (car suggested) "From scratch"))))
      (cdr (assoc answer labels)))))

(defun org-scribe-planner--pattern-config-for (pattern intensity)
  "Return the settings of PATTERN at INTENSITY; random gets a seed of its own."
  (let ((config (copy-sequence
                 (cdr (assq intensity (cdr (assq pattern org-scribe-planner--pattern-intensities)))))))
    (if (eq pattern 'random)
        (append config (list :seed (1+ (random 999999))))
      config)))

(defun org-scribe-planner--offer-pattern (plan pattern intensity why)
  "Ask whether PLAN uses PATTERN at INTENSITY; apply it on yes.
WHY says where the suggestion comes from.  Does nothing for `flat'."
  (when (and pattern (not (eq pattern 'flat))
             (y-or-n-p (format "Use the %s %s pattern (%s)? " why pattern
                               (cdr (assq pattern org-scribe-planner--patterns)))))
    (setf (org-scribe-plan-pattern plan) pattern)
    (setf (org-scribe-plan-pattern-config plan)
          (org-scribe-planner--pattern-config-for pattern intensity))
    t))

(defun org-scribe-planner--offer-weekends (plan mode why)
  "Ask whether PLAN's weekends follow MODE; apply it on yes.
WHY says where the suggestion comes from.  A goal of their own is asked
for, offering `org-scribe-planner-default-weekend-daily' or 1.5 times the
daily target.  Does nothing for `same'."
  (when (and mode (not (eq mode 'same))
             (y-or-n-p (format "%s weekends: %s? " why
                               (if (eq mode 'rest) "no writing" "a goal of their own"))))
    (setf (org-scribe-plan-weekend-mode plan) mode)
    (setf (org-scribe-plan-weekend-days plan) (copy-sequence org-scribe-planner-default-weekend-days))
    (when (eq mode 'different)
      (setf (org-scribe-plan-weekend-daily plan)
            (org-scribe-planner--read-non-negative-number
             "Weekend goal (words/day): "
             (or org-scribe-planner-default-weekend-daily
                 (round (* 1.5 (org-scribe-plan-daily-words plan)))))))
    t))

(defun org-scribe-planner--wizard-rebalance (plan calculated daily end-date)
  "Recalculate the value the new-plan wizard worked out, keeping the other two.
CALCULATED is what `org-scribe-planner--calculate-missing-variable'
returned: `days' (keep the total and DAILY, the entered daily target),
`total-words' (keep the days, ending on END-DATE, and DAILY) or anything
else (keep the total and END-DATE)."
  (pcase calculated
    ('days (org-scribe-planner--calculate-time plan daily))
    ('total-words
     (org-scribe-planner--keep-end-date plan end-date)
     (org-scribe-planner--set-total-keeping-daily plan daily))
    (_ (org-scribe-planner--keep-end-date plan end-date))))

;;; Interactive Commands

;;;###autoload
(defun org-scribe-planner-new-plan (&optional title save-path)
  "Create a new writing plan interactively.
TITLE and SAVE-PATH, when supplied, are used directly instead of
prompting for them — the interactive spec supplies both automatically
when invoked (via `M-x' or a keybinding) inside an org-scribe project
that has not yet been asked, so no prompt appears in that case either."
  (interactive
   (let* ((root (ignore-errors (org-scribe-project-root)))
          (in-project (and root
                           (file-exists-p (expand-file-name ".org-scribe-project" root)))))
     (list (and in-project (org-scribe-planner--project-title root))
           (and in-project (expand-file-name "plan.org" root)))))
  (let ((plan (make-org-scribe-plan))
        (preset nil))

    ;; Get plan title
    (setf (org-scribe-plan-title plan)
          (or title (read-string "Project title: ")))

    ;; A preset, or from scratch
    (setq preset (org-scribe-planner--read-preset))
    (if preset
        (let ((settings (cdr preset)))
          (setf (org-scribe-plan-total-words plan)
                (org-scribe-planner--read-positive-number
                 "Total words to write: " (plist-get settings :total)))
          (if (plist-get settings :days)
              (let ((days-result (org-scribe-planner--read-days
                                  "Days available: " (plist-get settings :days))))
                (if (listp days-result)
                    (setf (org-scribe-plan-days plan) (plist-get days-result :days)
                          (org-scribe-plan-start-date plan) (plist-get days-result :start-date)
                          (org-scribe-plan-end-date plan) (plist-get days-result :end-date))
                  (setf (org-scribe-plan-days plan) days-result)))
            (setf (org-scribe-plan-daily-words plan)
                  (org-scribe-planner--read-positive-number
                   "Words per day you can write: " (plist-get settings :daily)))))
    ;; Ask which variables to set and read them directly into the plan
    (let ((var-choice (org-scribe-planner--choose
                       "What do you know? "
                       '("Total words + Days → Calculate daily words"
                         "Total words + Daily words → Calculate days needed"
                         "Daily words + Days → Calculate total words")
                       "Total words + Days → Calculate daily words")))

      (cond
       ;; Total words + Days → Calculate daily words
       ((string-match-p "Calculate daily words" var-choice)
        (setf (org-scribe-plan-total-words plan)
              (org-scribe-planner--read-positive-number "Total words to write: "))
        (let ((days-result (org-scribe-planner--read-days "Days available: ")))
          (if (listp days-result)
              ;; User entered dates - extract days and set start/end dates
              (progn
                (setf (org-scribe-plan-days plan) (plist-get days-result :days))
                (setf (org-scribe-plan-start-date plan) (plist-get days-result :start-date))
                (setf (org-scribe-plan-end-date plan) (plist-get days-result :end-date)))
            ;; User entered a number - just set days
            (setf (org-scribe-plan-days plan) days-result))))

       ;; Total words + Daily words → Calculate days needed
       ((string-match-p "Calculate days needed" var-choice)
        (setf (org-scribe-plan-total-words plan)
              (org-scribe-planner--read-positive-number "Total words to write: "))
        (setf (org-scribe-plan-daily-words plan)
              (org-scribe-planner--read-positive-number "Words per day you can write: ")))

       ;; Daily words + Days → Calculate total words
       ((string-match-p "Calculate total words" var-choice)
        (setf (org-scribe-plan-daily-words plan)
              (org-scribe-planner--read-positive-number "Words per day you can write: "))
        (let ((days-result (org-scribe-planner--read-days "Days available: ")))
          (if (listp days-result)
              ;; User entered dates - extract days and set start/end dates
              (progn
                (setf (org-scribe-plan-days plan) (plist-get days-result :days))
                (setf (org-scribe-plan-start-date plan) (plist-get days-result :start-date))
                (setf (org-scribe-plan-end-date plan) (plist-get days-result :end-date)))
            ;; User entered a number - just set days
            (setf (org-scribe-plan-days plan) days-result))))

       ;; Fallback if nothing matched (shouldn't happen, but just in case)
       (t
        (error "Invalid choice: %s" var-choice)))))

    ;; Words already written: the plan's pre-existing words.  Inside a
    ;; project they are the manuscript's count — what every sync reads —
    ;; plus any words written outside it; elsewhere, a number.
    (let* ((counted (and org-scribe-planner-wordcount-function
                         (ignore-errors (funcall org-scribe-planner-wordcount-function))))
           (base (org-scribe-planner--outside-base save-path))
           (preexisting
            (if (numberp counted)
                (progn
                  (message "The manuscript has %d words" counted)
                  (org-scribe-planner--read-outside-words plan base)
                  (org-scribe-planner--written-total plan counted base))
              (org-scribe-planner--read-non-negative-number
               "Words already written (before this plan): " 0)))
           (total (org-scribe-plan-total-words plan)))
      (when (and (numberp total) (>= preexisting total))
        (user-error "%d words are already written, which reaches the %d-word goal"
                    preexisting total))
      (setf (org-scribe-plan-preexisting-words plan) preexisting)
      (setf (org-scribe-plan-current-words plan) preexisting))

    ;; Ask about start date only if not already set (from date entry method)
    (unless (org-scribe-plan-start-date plan)
      (setf (org-scribe-plan-start-date plan)
            (org-scribe-planner--read-date "Start date (YYYY-MM-DD)"
                                          (org-scribe-planner--get-today-date))))

    ;; Calculate missing variable and dates FIRST (before asking about spare days)
    (condition-case err
        (progn
          ;; Initial calculation without spare days
          ;; Remember which variable we calculated so we can recalculate it if spare days are added
          (let* ((typed-daily (org-scribe-plan-daily-words plan))
                 (calculated-var (org-scribe-planner--calculate-missing-variable plan))
                 (initial-value (when calculated-var
                                  (cl-getf (list 'total-words (org-scribe-plan-total-words plan)
                                                'daily-words (org-scribe-plan-daily-words plan)
                                                'days (org-scribe-plan-days plan))
                                          calculated-var))))
            (when calculated-var
              (message "Calculated %s: %s" calculated-var initial-value))

            ;; Calculate dates (now we have start-date and all variables)
            (org-scribe-planner--calculate-dates plan)

            ;; The pattern and weekends: the preset's suggestions, or the
            ;; defaults for plans made from scratch
            ;; Rebalancing keeps the daily target as typed, when one was
            (let ((entered-daily (or typed-daily (org-scribe-plan-daily-words plan)))
                  (end-date (org-scribe-plan-end-date plan))
                  (settings (cdr preset)))
              (org-scribe-planner--offer-pattern
               plan
               (if preset (plist-get settings :pattern) org-scribe-planner-default-pattern)
               (if preset 'moderate org-scribe-planner-default-pattern-intensity)
               (if preset "suggested" "default"))
              (when (org-scribe-planner--offer-weekends
                     plan
                     (if preset (plist-get settings :weekend-mode)
                       org-scribe-planner-default-weekend-mode)
                     (if preset "Suggested" "Default"))
                (org-scribe-planner--wizard-rebalance plan calculated-var entered-daily end-date))
              (org-scribe-planner--sync-daily plan)

              ;; Spare days: the same rebalance keeps the two values given
              (when (y-or-n-p "Do you want to set spare/break days? ")
                (setq end-date (org-scribe-plan-end-date plan))
                (org-scribe-planner--configure-spare-days plan)
                (when (org-scribe-plan-spare-days plan)
                  (message "Added %d spare day(s), recalculating..."
                           (length (org-scribe-plan-spare-days plan)))
                  (org-scribe-planner--wizard-rebalance plan calculated-var entered-daily end-date)
                  (message "Recalculated: %d writing days ending %s, %d words/day, %d words"
                           (org-scribe-plan-writing-days plan) (org-scribe-plan-end-date plan)
                           (org-scribe-plan-daily-words plan) (org-scribe-plan-total-words plan))))))

          ;; A plan started in the past: its past days keep these goals
          (org-scribe-planner--lock-past-goals plan)

          ;; Ask user for save location
          (let* ((default-filename (concat (downcase (replace-regexp-in-string
                                                     "[^[:alnum:]]" "-"
                                                     (org-scribe-plan-title plan)))
                                          ".org"))
                 (default-filepath (expand-file-name default-filename org-scribe-planner-directory))
                 (save-location (or save-path
                                    (read-file-name "Save plan to: "
                                                    org-scribe-planner-directory
                                                    default-filepath
                                                    nil
                                                    default-filename)))
                 (save-dir (file-name-directory save-location)))

            ;; Outside files were stored relative to where the plan was
            ;; expected; re-anchor them to where it is saved
            (org-scribe-planner--rebase-outside-paths
             plan (org-scribe-planner--outside-base save-path) save-dir)

            ;; Ensure the directory exists
            (if (file-exists-p save-dir)
                ;; Directory exists, save the plan
                (org-scribe-planner--save-plan plan save-location)
              ;; Directory doesn't exist, ask to create it
              (if (y-or-n-p (format "Directory %s does not exist. Create it? " save-dir))
                  (progn
                    (make-directory save-dir t)
                    (org-scribe-planner--save-plan plan save-location))
                ;; User declined to create directory
                (error "Cannot save plan: directory does not exist")))

            ;; Set as current active plan
            (setq org-scribe-planner--current-plan plan)
            (setq org-scribe-planner--current-plan-file save-location)

            ;; Run hook
            (run-hook-with-args 'org-scribe-planner-after-plan-load-hook plan save-location)

            ;; Display the plan
            (org-scribe-planner-show-calendar plan save-location)))
      (error
       (message "Error creating plan: %s" (error-message-string err))))))

;;;###autoload
(defun org-scribe-planner-onboard-existing-project ()
  "Create a writing plan for a novel already in progress.
Reads the current manuscript word count, asks for your target and
timeline, then builds a plan that starts today (or the date you give)
with the existing words recorded as its pre-existing words
\(PREEXISTING_WORDS): they count toward the goal and completion, but
not as any one day's writing, so auto-tracking records only net-new
writing going forward.

Skips the normal new-plan wizard; the daily target is calculated from
the *remaining* words over the *remaining* working days, not the total."
  (interactive)
  (let* ((today (org-scribe-planner--get-today-date))
         ;; 1. The manuscript's word count: counted when it can be (an
         ;; empty manuscript counts 0), asked otherwise
         (manuscript-words
          (let ((auto (and org-scribe-planner-wordcount-function
                           (ignore-errors
                             (funcall org-scribe-planner-wordcount-function)))))
            (if (numberp auto)
                (progn (message "The manuscript has %d words" auto) auto)
              (org-scribe-planner--read-non-negative-number
               "Manuscript word count (0 if it's empty): "))))
         (plan (make-org-scribe-plan))
         ;; Outside files are stored relative to the plan's directory: the
         ;; project root, where the plan is saved by default
         (base (file-name-as-directory
                (expand-file-name (or (and (featurep 'org-scribe)
                                           (ignore-errors (org-scribe-project-root)))
                                      default-directory))))
         ;; 1b. Words written elsewhere count too
         (current-words (progn
                          (org-scribe-planner--read-outside-words plan base)
                          (org-scribe-planner--written-total plan manuscript-words base))))

    ;; 2. Core parameters
    (setf (org-scribe-plan-title plan)
          (read-string "Project title: "))
    (setf (org-scribe-plan-total-words plan)
          (org-scribe-planner--read-positive-number
           (format "Total word target (you have %d so far): " current-words)))

    (when (<= (org-scribe-plan-total-words plan) current-words)
      (error "Target (%d) must be greater than words already written (%d)"
             (org-scribe-plan-total-words plan) current-words))

    ;; The plan starts today by default: the words already written are
    ;; its pre-existing words, not part of its schedule
    (setf (org-scribe-plan-start-date plan)
          (org-scribe-planner--read-date "Plan start date (YYYY-MM-DD)" today))
    (setf (org-scribe-plan-preexisting-words plan) current-words)

    ;; 3. Pace: end date or daily words
    (let ((how (completing-read
                "Set the pace by: "
                '("End date → calculate daily words"
                  "Daily words → calculate end date")
                nil t nil nil "End date → calculate daily words")))
      ;; Compare the start of the choice, case-sensitively: the other
      ;; choice also contains "end date", which `string-match-p' matches
      ;; under the default `case-fold-search'
      (if (string-prefix-p "End date" how)
          (setf (org-scribe-plan-end-date plan)
                (org-scribe-planner--read-date
                 "Target end date (YYYY-MM-DD)"
                 (org-scribe-planner--add-days today 90)))
        (setf (org-scribe-plan-daily-words plan)
              (org-scribe-planner--read-positive-number
               "Words per day (for the remaining period): "))))

    ;; 4. Optional spare days
    (when (y-or-n-p "Set spare/break days (weekends, holidays)? ")
      (org-scribe-planner--configure-spare-days plan))

    ;; 5. Calculate the plan with the schedule engine.  The existing
    ;;    words are its pre-existing words, so the daily target covers only
    ;;    what is left.  If the plan starts before today, the days before
    ;;    today are locked with no goal: the words left are written from
    ;;    today on.
    (condition-case err
        (let* ((remaining-words (- (org-scribe-plan-total-words plan) current-words))
               (start-date (org-scribe-plan-start-date plan)))
          (setf (org-scribe-plan-current-words plan) current-words)
          (when (string< start-date today)
            (setf (org-scribe-plan-locked-goals plan)
                  (mapcar (lambda (date) (cons date 0))
                          (org-scribe-planner--generate-date-range
                           start-date (org-scribe-planner--add-days today -1)))))
          (if (org-scribe-plan-end-date plan)
              ;; End date given: the daily target follows
              (org-scribe-planner--set-writing-days
               plan (org-scribe-planner--writing-days-through
                     plan (org-scribe-plan-end-date plan)))
            ;; Daily words given: the plan gets the days it needs
            (org-scribe-planner--calculate-time plan (org-scribe-plan-daily-words plan)))

          (let ((future-working-days (- (org-scribe-planner--writing-days plan)
                                        (org-scribe-planner--min-writing-days plan))))
            (when (<= future-working-days 0)
              (error "No future working days in the plan — adjust your end date"))

          ;; 6. The existing words are the plan's pre-existing words (set
          ;; above), not a ledger entry: the first sync's delta is measured
          ;; against them, so only net-new writing counts as a day's words.

          ;; 7. Save
          (let* ((default-filename
                  (concat (downcase (replace-regexp-in-string
                                     "[^[:alnum:]]" "-"
                                     (org-scribe-plan-title plan)))
                          ".org"))
                 (default-dir (or (and (featurep 'org-scribe)
                                       (ignore-errors (org-scribe-project-root)))
                                  org-scribe-planner-directory))
                 (default-filepath (expand-file-name default-filename default-dir))
                 (save-location (read-file-name "Save plan to: "
                                               (file-name-directory default-filepath)
                                               default-filepath nil
                                               default-filename)))
            (make-directory (file-name-directory save-location) t)
            (org-scribe-planner--rebase-outside-paths
             plan base (file-name-directory (expand-file-name save-location)))
            (org-scribe-planner--save-plan plan save-location)
            (setq org-scribe-planner--current-plan plan)
            (setq org-scribe-planner--current-plan-file save-location)
            (run-hook-with-args 'org-scribe-planner-after-plan-load-hook
                                plan save-location)
            (message "Plan ready — %d written, %d to go at %d words/day over %d days."
                     current-words remaining-words
                     (org-scribe-plan-daily-words plan)
                     future-working-days)
            (org-scribe-planner-show-calendar plan save-location))))
      (error
       (message "Error creating plan: %s" (error-message-string err))))))

(defun org-scribe-planner--configure-spare-days (plan)
  "Interactively configure spare days for PLAN."
  (let ((spare-days (or (org-scribe-plan-spare-days plan) nil))
        (continue t))

    (while continue
      (let ((method (org-scribe-planner--choose
                     "Configure spare days: "
                     '("Add: Specific date"
                       "Add: Date range"
                       "Add: All weekends"
                       "Add: All Saturdays"
                       "Add: All Sundays"
                       "Remove: Specific date"
                       "Remove: All spare days"
                       "List current spare days"
                       "Done")
                     "Done")))
        (cond
         ((equal method "Add: Specific date")
          (let ((date (org-scribe-planner--read-date "Enter date (YYYY-MM-DD)" nil t)))
            (when date
              (push date spare-days)
              (let ((note (read-string (format "Note for %s (leave empty for default): " date))))
                (when (and note (not (string-empty-p (string-trim note))))
                  ;; Add entry to daily-word-counts with note
                  (org-scribe-planner--add-spare-day-note plan date note)))
              (message "Added %s as spare day" date))))

         ((equal method "Add: Date range")
          (let* ((start (org-scribe-planner--read-date "Start date (YYYY-MM-DD)" nil t))
                 (end (when start
                        (org-scribe-planner--read-date "End date (YYYY-MM-DD)" nil t))))
            (when (and start end)
              (let ((date-range (org-scribe-planner--generate-date-range start end)))
                (setq spare-days (append spare-days date-range))
                ;; Ask if user wants to add a note for all dates in range
                (let ((note (read-string (format "Note for all dates in range (leave empty for default): "))))
                  (when (and note (not (string-empty-p (string-trim note))))
                    (dolist (date date-range)
                      (org-scribe-planner--add-spare-day-note plan date note))))
                (message "Added dates from %s to %s" start end)))))

         ((equal method "Add: All weekends")
          (let ((weekend-dates (org-scribe-planner--get-weekends plan)))
            (setq spare-days (append spare-days weekend-dates))
            (let ((note (read-string "Note for all weekends (leave empty for default): ")))
              (when (and note (not (string-empty-p (string-trim note))))
                (dolist (date weekend-dates)
                  (org-scribe-planner--add-spare-day-note plan date note))))
            (message "Added all weekends")))

         ((equal method "Add: All Saturdays")
          (let ((saturday-dates (org-scribe-planner--get-day-of-week plan 6)))
            (setq spare-days (append spare-days saturday-dates))
            (let ((note (read-string "Note for all Saturdays (leave empty for default): ")))
              (when (and note (not (string-empty-p (string-trim note))))
                (dolist (date saturday-dates)
                  (org-scribe-planner--add-spare-day-note plan date note))))
            (message "Added all Saturdays")))

         ((equal method "Add: All Sundays")
          (let ((sunday-dates (org-scribe-planner--get-day-of-week plan 0)))
            (setq spare-days (append spare-days sunday-dates))
            (let ((note (read-string "Note for all Sundays (leave empty for default): ")))
              (when (and note (not (string-empty-p (string-trim note))))
                (dolist (date sunday-dates)
                  (org-scribe-planner--add-spare-day-note plan date note))))
            (message "Added all Sundays")))

         ((equal method "Remove: Specific date")
          (if (null spare-days)
              (message "No spare days to remove")
            (let ((date (org-scribe-planner--choose "Select date to remove: "
                                                    (sort (copy-sequence spare-days) #'string<))))
              (when date
                (setq spare-days (delete date spare-days))
                (org-scribe-planner--remove-spare-day-note plan date)
                (message "Removed %s from spare days" date)))))

         ((equal method "Remove: All spare days")
          (if (null spare-days)
              (message "No spare days to remove")
            (when (y-or-n-p (format "Remove all %d spare days? " (length spare-days)))
              (dolist (date spare-days)
                (org-scribe-planner--remove-spare-day-note plan date))
              (setq spare-days nil)
              (message "Removed all spare days"))))

         ((equal method "List current spare days")
          (if (null spare-days)
              (message "No spare days configured")
            (let ((sorted-days (sort (copy-sequence spare-days) #'string<)))
              (message "Current spare days (%d): %s"
                      (length sorted-days)
                      (mapconcat #'identity sorted-days ", ")))))

         ((equal method "Done")
          (setq continue nil)))))

    (setf (org-scribe-plan-spare-days plan) (delete-dups spare-days))))

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
          ;; Skip the separator line (|------+-----+....|)
          (forward-line 1)
          ;; Now we should be at the first data row - skip separator
          (when (looking-at "^|[-+]+|")
            (forward-line 1))
          ;; Parse table rows
          (while (looking-at "^|[[:space:]]+\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[[:space:]]+|[^|]*|[^|]*|[^|]*|[^|]*|[[:space:]]*\\([^|]+?\\)[[:space:]]*|")
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
            (forward-line 1))))
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
        (org-set-property "TOTAL_WORDS" (number-to-string (org-scribe-plan-total-words plan)))
        (org-set-property "DAILY_WORDS" (number-to-string (org-scribe-plan-daily-words plan)))
        (org-set-property "DAYS" (number-to-string (org-scribe-plan-days plan)))
        (org-set-property "START_DATE" (org-scribe-plan-start-date plan))
        (org-set-property "END_DATE" (org-scribe-plan-end-date plan))
        (org-set-property "CURRENT_WORDS" (number-to-string (org-scribe-plan-current-words plan)))
        (org-set-property "PREEXISTING_WORDS"
                          (number-to-string (or (org-scribe-plan-preexisting-words plan) 0)))
        (when (org-scribe-planner--writing-days plan)
          (org-set-property "WRITING_DAYS"
                            (number-to-string (org-scribe-planner--writing-days plan))))
        (when (org-scribe-plan-locked-goals plan)
          (org-set-property "LOCKED_GOALS"
                            (mapconcat (lambda (lock) (format "%s:%d" (car lock) (cdr lock)))
                                       (org-scribe-plan-locked-goals plan) ",")))
        (unless (zerop (or (org-scribe-plan-schedule-adjustment plan) 0))
          (org-set-property "SCHEDULE_ADJUSTMENT"
                            (number-to-string (org-scribe-plan-schedule-adjustment plan))))
        (unless (memq (org-scribe-plan-pattern plan) '(nil flat))
          (org-set-property "PATTERN" (symbol-name (org-scribe-plan-pattern plan)))
          (when (org-scribe-plan-pattern-config plan)
            (org-set-property "PATTERN_CONFIG"
                              (org-scribe-planner--format-pattern-config
                               (org-scribe-plan-pattern-config plan)))))
        (unless (memq (org-scribe-plan-weekend-mode plan) '(nil same))
          (org-set-property "WEEKEND_MODE" (symbol-name (org-scribe-plan-weekend-mode plan)))
          (org-set-property "WEEKEND_DAYS"
                            (mapconcat #'number-to-string
                                       (or (org-scribe-plan-weekend-days plan) '(6 0)) ","))
          (when (eq (org-scribe-plan-weekend-mode plan) 'different)
            (org-set-property "WEEKEND_DAILY"
                              (number-to-string (or (org-scribe-plan-weekend-daily plan) 0)))))
        (unless (zerop (or (org-scribe-plan-outside-words plan) 0))
          (org-set-property "OUTSIDE_WORDS"
                            (number-to-string (org-scribe-plan-outside-words plan))))
        (when (org-scribe-plan-outside-files plan)
          (org-set-property "OUTSIDE_FILES"
                            (prin1-to-string (org-scribe-plan-outside-files plan))))
        (when (org-scribe-plan-custom-goals plan)
          (org-set-property "CUSTOM_GOALS"
                            (mapconcat (lambda (goal) (format "%s:%d" (car goal) (cdr goal)))
                                       (sort (copy-sequence (org-scribe-plan-custom-goals plan))
                                             (lambda (a b) (string< (car a) (car b))))
                                       ",")))

        (when (org-scribe-plan-spare-days plan)
          (org-set-property "SPARE_DAYS" (mapconcat #'identity (org-scribe-plan-spare-days plan) ",")))

        (when (org-scribe-plan-daily-word-counts plan)
          (let ((entries-with-words
                 (org-scribe-planner--counts-with-words
                  (org-scribe-plan-daily-word-counts plan))))
            (when entries-with-words
              (org-set-property "DAILY_WORD_COUNTS"
                               (mapconcat #'org-scribe-planner--format-daily-count-entry
                                          entries-with-words
                                          ",")))))

        ;; Add schedule as content
        (goto-char (point-max))
        (insert "\n** Schedule\n\n")
        (let ((schedule (org-scribe-planner--generate-day-schedule plan))
              (daily-counts (org-scribe-plan-daily-word-counts plan))
              (cumulative-actual 0)
              ;; The manuscript starts at its pre-existing words.  The
              ;; schedule adjustment of a recalculation from progress is
              ;; applied from today on (see
              ;; `org-scribe-planner--anchor-expected'): applied from the
              ;; start, it showed a negative expected total for the past
              ;; days before the first entry.
              (expected-total (or (org-scribe-plan-preexisting-words plan) 0))
              (anchor (list nil nil)))
          (insert "| Date | Target | Cumulative | Actual | Progress % | Notes |\n")
          (insert "|------+--------+------------+--------+------------+-------|\n")
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

              ;; Calculate cumulative the same way as in the report
              (setq expected-total
                    (org-scribe-planner--anchor-expected anchor day expected-total))
              (if actual
                  ;; Has actual data - the entry's own cumulative total IS
                  ;; the cumulative-to-date; expected matches it
                  (progn
                    (setq cumulative-actual (plist-get daily-data :words))
                    (setq expected-total cumulative-actual)
                    (setcar anchor t))
                ;; No actual data - add daily target to expected (skip spare days)
                (unless is-spare
                  (setq expected-total (+ expected-total target))))

              (insert (format "| %s | %s | %d | %s | %s | %s |\n"
                             date
                             (if is-spare "REST" (number-to-string target))
                             expected-total
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

(defun org-scribe-planner--migrate-existing-manuscript-entry (plan)
  "Turn PLAN's \"Existing manuscript\" ledger entry into pre-existing words.
Onboarding used to record the manuscript's count as the first entry of
DAILY_WORD_COUNTS, with that note, so it counted as one day's writing.
When the earliest entry with a word count carries that note, its count
becomes `org-scribe-plan-preexisting-words' and the entry is removed.
Returns non-nil when PLAN was changed."
  (let* ((daily-counts (org-scribe-plan-daily-word-counts plan))
         (first (car (org-scribe-planner--sorted-daily-counts daily-counts))))
    (when (and first
               (equal (org-scribe-planner--get-entry-note first) "Existing manuscript"))
      (setf (org-scribe-plan-preexisting-words plan)
            (org-scribe-planner--get-entry-words first))
      (setf (org-scribe-plan-daily-word-counts plan)
            (assoc-delete-all (car first) daily-counts))
      t)))

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

(defun org-scribe-planner--migrate-recalculated-daily (plan)
  "Keep the daily target of a PLAN saved before the schedule engine.
Old recalculations changed DAILY_WORDS and left TOTAL_WORDS alone, so the
two may not match any more.  The engine derives the daily target from
the total, which would undo the recalculation; instead, set the schedule
adjustment so the open days keep the stored daily target.  A difference
smaller than one word per writing day is the old rounding up, not a
recalculation, and is left alone.  Returns non-nil when PLAN was changed."
  (let ((total (org-scribe-plan-total-words plan))
        (daily (org-scribe-plan-daily-words plan))
        (writing-days (org-scribe-planner--writing-days plan)))
    (when (and (numberp total) (numberp daily) writing-days (> writing-days 0))
      (let ((difference (- (+ (* daily writing-days)
                              (or (org-scribe-plan-preexisting-words plan) 0))
                           total)))
        (when (or (< difference 0) (>= difference writing-days))
          (setf (org-scribe-plan-schedule-adjustment plan) difference)
          t)))))

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
      (setf (org-scribe-plan-total-words plan)
            (string-to-number (or (org-entry-get nil "TOTAL_WORDS") "0")))
      (setf (org-scribe-plan-daily-words plan)
            (string-to-number (or (org-entry-get nil "DAILY_WORDS") "0")))
      (setf (org-scribe-plan-days plan)
            (string-to-number (or (org-entry-get nil "DAYS") "0")))
      (setf (org-scribe-plan-start-date plan)
            (org-entry-get nil "START_DATE"))
      (setf (org-scribe-plan-end-date plan)
            (org-entry-get nil "END_DATE"))
      (setf (org-scribe-plan-current-words plan)
            (string-to-number (or (org-entry-get nil "CURRENT_WORDS") "0")))
      (setf (org-scribe-plan-preexisting-words plan)
            (string-to-number (or (org-entry-get nil "PREEXISTING_WORDS") "0")))
      (let ((writing-days (org-entry-get nil "WRITING_DAYS")))
        (when writing-days
          (setf (org-scribe-plan-writing-days plan) (string-to-number writing-days))))
      (let ((locked (org-entry-get nil "LOCKED_GOALS")))
        (when locked
          (setf (org-scribe-plan-locked-goals plan)
                (mapcar (lambda (item)
                          (let ((parts (split-string item ":")))
                            (cons (car parts) (string-to-number (cadr parts)))))
                        (split-string locked "," t " ")))))
      (setf (org-scribe-plan-schedule-adjustment plan)
            (string-to-number (or (org-entry-get nil "SCHEDULE_ADJUSTMENT") "0")))
      (let ((pattern (org-entry-get nil "PATTERN")))
        (when pattern
          (setf (org-scribe-plan-pattern plan) (intern pattern))
          (setf (org-scribe-plan-pattern-config plan)
                (org-scribe-planner--parse-pattern-config
                 (org-entry-get nil "PATTERN_CONFIG")))))
      (let ((mode (org-entry-get nil "WEEKEND_MODE")))
        (when mode
          (setf (org-scribe-plan-weekend-mode plan) (intern mode))
          (let ((days (org-entry-get nil "WEEKEND_DAYS")))
            (when days
              (setf (org-scribe-plan-weekend-days plan)
                    (mapcar #'string-to-number (split-string days "," t " ")))))
          (setf (org-scribe-plan-weekend-daily plan)
                (string-to-number (or (org-entry-get nil "WEEKEND_DAILY") "0")))))
      (setf (org-scribe-plan-outside-words plan)
            (string-to-number (or (org-entry-get nil "OUTSIDE_WORDS") "0")))
      (let ((files (org-entry-get nil "OUTSIDE_FILES")))
        (when files
          ;; Data only: a list of plists, read without evaluating
          (setf (org-scribe-plan-outside-files plan)
                (condition-case nil
                    (let ((value (car (read-from-string files))))
                      (when (and (listp value) (cl-every #'listp value)) value))
                  (error nil)))))
      (let ((goals (org-entry-get nil "CUSTOM_GOALS")))
        (when goals
          (setf (org-scribe-plan-custom-goals plan)
                (mapcar (lambda (item)
                          (let ((parts (split-string item ":")))
                            (cons (car parts) (string-to-number (cadr parts)))))
                        (split-string goals "," t " ")))))

      (let ((spare-days-str (org-entry-get nil "SPARE_DAYS")))
        (when spare-days-str
          (setf (org-scribe-plan-spare-days plan)
                (split-string spare-days-str "," t " "))))

      (let ((daily-counts-str (org-entry-get nil "DAILY_WORD_COUNTS")))
        (when daily-counts-str
          (let ((parsed-counts
                 (mapcar (lambda (entry-str)
                          (let* ((parts (split-string entry-str ":" nil " "))  ; Don't skip empty parts for :: syntax
                                 (date (nth 0 parts))
                                 (word-count (string-to-number (nth 1 parts)))
                                 (note-or-empty (nth 2 parts))
                                 (target-str (nth 3 parts))
                                 ;; If we have 4 parts, it's new format: date:words:note:target or date:words::target
                                 ;; If we have 3 parts, it's old format: date:words:note
                                 ;; If we have 2 parts, it's old format: date:words
                                 (has-target (and target-str (not (string-empty-p target-str))))
                                 (note (org-scribe-planner--unescape-note-from-property
                                        (if (and note-or-empty (not (string-empty-p note-or-empty)))
                                            note-or-empty
                                          "")))
                                 (target (when has-target (string-to-number target-str))))
                            (if has-target
                                ;; New format with target
                                (cons date (list :words word-count :note note :target target))
                              ;; Old format - convert to new format (no target stored)
                              (cons date (list :words word-count :note note)))))
                        (org-scribe-planner--split-daily-counts-str daily-counts-str))))
            (setf (org-scribe-plan-daily-word-counts plan) parsed-counts))))

      ;; Plans onboarded before PREEXISTING_WORDS existed stored the
      ;; manuscript as a first ledger entry noted "Existing manuscript".
      ;; Move it to the pre-existing words, so it no longer counts as a
      ;; day's writing.  The next save writes the new property.
      (unless (org-entry-get nil "PREEXISTING_WORDS")
        (org-scribe-planner--migrate-existing-manuscript-entry plan))

      ;; Plans saved before the schedule engine: keep a daily target that
      ;; an old recalculation set apart from the total (see
      ;; `org-scribe-planner--migrate-recalculated-daily')
      (unless (org-entry-get nil "WRITING_DAYS")
        (org-scribe-planner--migrate-recalculated-daily plan))
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
                             (mapconcat #'org-scribe-planner--format-daily-count-entry
                                        (org-scribe-planner--counts-with-words
                                         (org-scribe-plan-daily-word-counts plan))
                                        ","))
            (save-buffer)
            (message "Merged %d note(s) from schedule table into plan properties"
                     (length table-notes)))))

      plan)))

;;;###autoload
(defun org-scribe-planner-load-plan (&optional file)
  "Load an existing writing plan and set it as the active plan.
FILE, when supplied, is loaded directly with no file-picker prompt —
the interactive spec supplies it automatically when invoked (via `M-x'
or a keybinding) inside an org-scribe project that already has a valid
saved plan file."
  (interactive
   (let* ((root (ignore-errors (org-scribe-project-root)))
          (in-project (and root
                           (file-exists-p (expand-file-name ".org-scribe-project" root))))
          (plan-file (and in-project (org-scribe-planner--find-plan-file root))))
     (list (and plan-file (org-scribe-planner--plan-file-valid-p plan-file) plan-file))))
  (let* ((start-dir (when org-scribe-planner-project-root-function
                      (funcall org-scribe-planner-project-root-function)))
         (file (or file (org-scribe-planner--select-plan-file "Select plan file: " start-dir))))
    (when file
      (let ((plan (org-scribe-planner--load-plan file)))
        (org-scribe-planner--lock-past-goals plan)
        ;; Set as current active plan
        (setq org-scribe-planner--current-plan plan)
        (setq org-scribe-planner--current-plan-file file)
        ;; Run hook
        (run-hook-with-args 'org-scribe-planner-after-plan-load-hook plan file)
        (org-scribe-planner-show-calendar plan file)))))

;;; Calendar Visualization

(defun org-scribe-planner-show-calendar (plan &optional filepath)
  "Display a calendar visualization for PLAN.
Optional FILEPATH shows the location of the plan file."
  (let ((buffer (get-buffer-create org-scribe-planner-calendar-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-scribe-planner-calendar-mode)

        ;; Header
        (insert (propertize (format "Writing Plan: %s\n" (org-scribe-plan-title plan))
                           'face 'org-level-1))
        (when filepath
          (insert (propertize (format "Location: %s\n" filepath)
                             'face 'org-document-info)))
        ;; Show if this is the active plan
        (when (and org-scribe-planner--current-plan-file
                   filepath
                   (string= org-scribe-planner--current-plan-file filepath))
          (insert (propertize "[ACTIVE PLAN]\n" 'face 'org-done)))
        (insert (make-string 80 ?=) "\n")
        (insert "Commands:\n")
        (insert "  [q] quit  [G] refresh view  [u] sync word count  [d] daily word count\n")
        (insert "  [r] recalculate plan  [a] adjust remaining days  [e] adjust end date  [c] check progress\n  [w] weekly summary  [M] month grid\n")
        (insert "  [D] dashboards menu  [p] progress  [b] burndown  [g] cumulative\n")
        (insert "  [v] velocity  [h] heatmap\n")
	(insert (make-string 80 ?=) "\n\n")

        ;; Summary
        (insert (propertize "Summary:\n" 'face 'org-level-2))
        (insert (format "  Total Words:   %d\n" (org-scribe-plan-total-words plan)))
        (insert (format "  Daily Words:   %d%s\n" (org-scribe-plan-daily-words plan)
                        (if (memq (org-scribe-plan-pattern plan) '(nil flat))
                            ""
                          (format " (average, %s pattern)"
                                  (org-scribe-planner--describe-pattern plan)))))
        (unless (memq (org-scribe-plan-weekend-mode plan) '(nil same))
          (insert (format "  Weekends:      %s\n" (org-scribe-planner--describe-weekends plan))))
        (insert (format "  Total Days:    %d\n" (org-scribe-plan-days plan)))
        (insert (format "  Working Days:  %d\n"
                        (- (org-scribe-plan-days plan)
                           (org-scribe-planner--spare-days-in-window
                            plan (org-scribe-plan-days plan)))))
        (insert (format "  Start Date:    %s\n" (org-scribe-plan-start-date plan)))
        (insert (format "  End Date:      %s\n" (org-scribe-plan-end-date plan)))
        (when (> (or (org-scribe-plan-preexisting-words plan) 0) 0)
          (insert (format "  Written Before: %d (pre-existing, part of the total)\n"
                          (org-scribe-plan-preexisting-words plan))))
        (unless (and (zerop (or (org-scribe-plan-outside-words plan) 0))
                     (null (org-scribe-plan-outside-files plan)))
          (insert (format "  Outside:       %s (counted with the manuscript)\n"
                          (org-scribe-planner--describe-outside
                           plan (org-scribe-planner--outside-base filepath)))))
        (when-let* ((status (ignore-errors (org-scribe-planner--status plan))))
          (insert (format "  Status:        %s\n"
                          (org-scribe-planner--describe-status status))))
        (insert (format "  Current Words: %d (%.1f%%)\n\n"
                       (org-scribe-plan-current-words plan)
                       (* 100 (/ (float (org-scribe-plan-current-words plan))
                                (org-scribe-plan-total-words plan)))))

        ;; Day-by-day schedule
        (insert (propertize "Daily Schedule:\n" 'face 'org-level-2))
        (insert (make-string 80 ?-) "\n")

        (let ((schedule (org-scribe-planner--generate-day-schedule plan))
              (daily-counts (org-scribe-plan-daily-word-counts plan))
              (written-table (org-scribe-planner--words-by-date plan))
              (week-num 1)
              (week-words 0)
              (prev-week-start nil)
              (cumulative-actual 0)
              ;; The manuscript starts at its pre-existing words.  The
              ;; schedule adjustment of a recalculation from progress is
              ;; applied from today on (see
              ;; `org-scribe-planner--anchor-expected'): applied from the
              ;; start, it showed a negative expected total for the past
              ;; days before the first entry.
              (expected-total (or (org-scribe-plan-preexisting-words plan) 0))
              (anchor (list nil nil)))

          (dolist (day schedule)
            (let* ((date (plist-get day :date))
                   (current-daily-words (plist-get day :words))
                   (cumulative (plist-get day :cumulative))
                   (is-spare (plist-get day :is-spare-day))
                   (daily-entry (assoc date daily-counts))
                   (daily-data (when daily-entry (cdr daily-entry)))
                   ;; Use stored target for display if available, otherwise use current plan's daily-words
                   (stored-target (when daily-data
                                   (plist-get daily-data :target)))
                   (display-target (or stored-target current-daily-words))
                   ;; This day's net delta, derived from the cumulative
                   ;; total stored in daily-data
                   (actual (when daily-data
                            (org-scribe-planner--entry-delta daily-counts date
                             (org-scribe-plan-preexisting-words plan))))
                   (note (when daily-data
                          (or (plist-get daily-data :note) "")))
                   ;; Parse date to get day of week (0=Sunday, 1=Monday, ..., 6=Saturday)
                   (date-parts (org-scribe-planner--parse-date-parts date))
                   (year (nth 0 date-parts))
                   (month (nth 1 date-parts))
                   (day-num (nth 2 date-parts))
                   (day-of-week (calendar-day-of-week (list month day-num year)))
                   (day-name (calendar-day-name (list month day-num year)))
                   ;; A week starts on `org-scribe-planner-week-start-day'
                   (is-monday (= day-of-week org-scribe-planner-week-start-day))
                   (percentage (if (and actual (not is-spare) (> display-target 0))
                                  (format "[%.1f%%]" (* 100.0 (/ (float actual) display-target)))
                                ""))
                   (face (if is-spare 'org-agenda-dimmed-todo-face 'default)))

              ;; Update cumulative actual word count and expected total.
              ;; daily-data's :words IS already the cumulative total as of
              ;; this date, so no summation — just carry it forward.
              (setq expected-total
                    (org-scribe-planner--anchor-expected anchor day expected-total))
              (if actual
                  (progn
                    (setq cumulative-actual (plist-get daily-data :words))
                    (setq expected-total cumulative-actual)
                    (setcar anchor t))
                ;; No actual data - add daily target to expected (skip spare days)
                (unless is-spare
                  (setq expected-total (+ expected-total current-daily-words))))

              ;; Week header (on the first day of the week)
              (when (or is-monday (null prev-week-start))
                (insert (propertize (format "\nWeek %d:\n" week-num) 'face 'org-level-3))
                ;; Add column headers
                (insert (propertize
                        (format "  %-25s  %-13s  %-15s  %-15s  %-23s  %s\n"
                               "Date (Day)"
                               "Daily Target"
                               "Expected Total"
                               "Actual Total"
                               "Daily Actual"
                               "Notes")
                        'face 'bold))
                (insert (propertize
                        (format "  %s  %s  %s  %s  %s  %s\n"
                               (make-string 25 ?-)
                               (make-string 13 ?-)
                               (make-string 15 ?-)
                               (make-string 15 ?-)
                               (make-string 23 ?-)
                               (make-string 30 ?-))
                        'face 'org-level-4))
                (setq week-num (1+ week-num)
                      week-words 0
                      prev-week-start date))

              ;; Day entry with columnar formatting
              (let* ((date-col (format "%s (%-9s)" date day-name))
                     (target-col (if is-spare "REST" (format "%d words" display-target)))
                     (expected-col (format "%d words" expected-total))
                     (cumulative-actual-col (if (> cumulative-actual 0)
                                               (format "%d words" cumulative-actual)
                                             ""))
                     ;; ✓ met, ~ partial (75%+), ✗ missed (see
                     ;; `org-scribe-planner--day-status')
                     (day-status (org-scribe-planner--day-status plan date written-table))
                     (mark (pcase day-status
                             ((or 'met 'exceeded) " ✓")
                             ('partial " ~")
                             ('missed " ✗")
                             (_ "")))
                     (daily-actual-col (cond
                                        (actual (format "%d words %s%s" actual percentage mark))
                                        ((eq day-status 'missed) "0 ✗")
                                        (t "")))
                     (note-col (cond
                                ;; If there's a custom note (even for spare days), show it
                                ((and note (not (string-empty-p note))) note)
                                ;; Otherwise, if it's a spare day, show default message
                                (is-spare (if (plist-get day :spare) "(spare day)" "(rest day)"))
                                ((plist-get day :custom) "(day goal)")
                                ;; No note and not spare day
                                (t ""))))
                (insert (propertize
                        (format "  %-25s  %-13s  %-15s  %-15s  %-23s  %s\n"
                               date-col
                               target-col
                               expected-col
                               cumulative-actual-col
                               daily-actual-col
                               note-col)
                        'face face)))

              ;; Add actual words written to week total (not targets)
              (when (numberp actual)
                (setq week-words (+ week-words actual)))

              ;; Week summary (show at end of week or end of schedule)
              (let ((next-day (cadr (member day schedule))))
                (when (or (null next-day)  ; last day
                         (let* ((next-date (plist-get next-day :date))
                                (next-parts (org-scribe-planner--parse-date-parts next-date))
                                (next-dow (calendar-day-of-week (list (nth 1 next-parts) (nth 2 next-parts) (nth 0 next-parts)))))
                           (= next-dow org-scribe-planner-week-start-day)))  ; next week begins
                  (insert (propertize (format "  Week total: %d words\n" week-words)
                                     'face 'org-level-4)))))))

        (insert "\n" (make-string 80 ?=) "\n")
        (insert "\nCommands:\n")
        (insert "  [q] quit  [G] refresh view  [u] sync word count  [d] daily word count\n")
        (insert "  [r] recalculate plan  [a] adjust remaining days  [e] adjust end date  [c] check progress\n  [w] weekly summary  [M] month grid\n")
        (insert "  [D] dashboards menu  [p] progress  [b] burndown  [g] cumulative\n")
        (insert "  [v] velocity  [h] heatmap\n"))

      (goto-char (point-min))
      (display-buffer buffer))))

(defvar org-scribe-planner-calendar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "G") #'org-scribe-planner-refresh-calendar)
    (define-key map (kbd "r") #'org-scribe-planner-recalculate)
    (define-key map (kbd "u") #'org-scribe-planner-update-progress)
    (define-key map (kbd "d") #'org-scribe-planner-add-session-note)
    (define-key map (kbd "a") #'org-scribe-planner-adjust-remaining-plan)
    (define-key map (kbd "e") #'org-scribe-planner-adjust-end-date)
    (define-key map (kbd "c") #'org-scribe-planner-check-progress)
    (define-key map (kbd "w") #'org-scribe-planner-show-weekly-summary)
    (define-key map (kbd "M") #'org-scribe-planner-show-month-calendar)
    (define-key map (kbd "D") #'org-scribe-planner-dashboards-menu)
    (define-key map (kbd "m") #'org-scribe-planner-show-multi-metric-dashboard)
    (define-key map (kbd "p") #'org-scribe-planner-show-progress-dashboard)
    (define-key map (kbd "b") #'org-scribe-planner-show-burndown)
    (define-key map (kbd "g") #'org-scribe-planner-show-cumulative-progress)
    (define-key map (kbd "v") #'org-scribe-planner-show-velocity)
    (define-key map (kbd "V") #'org-scribe-planner-show-velocity-chart)
    (define-key map (kbd "t") #'org-scribe-planner-show-velocity-trends)
    (define-key map (kbd "P") #'org-scribe-planner-show-performance-analytics)
    (define-key map (kbd "s") #'org-scribe-planner-show-split-dashboards)
    (define-key map (kbd "h") #'org-scribe-planner-show-heatmap)
    map)
  "Keymap for `org-scribe-planner-calendar-mode'.")

(defun org-scribe-planner-refresh-calendar ()
  "Re-render the plan calendar from the current in-memory plan.
Unlike `org-scribe-planner-update-progress', this does no file I/O and
fires no hooks — it just redraws the view with whatever data is already
loaded."
  (interactive)
  (org-scribe-planner--with-current-plan (plan file)
    (org-scribe-planner-show-calendar plan file)))

(define-derived-mode org-scribe-planner-calendar-mode special-mode "Writing-Plan"
  "Major mode for displaying writing plan calendars."
  (setq truncate-lines t))

;;; Org-agenda Integration

(defcustom org-scribe-planner-sync-to-agenda t
  "Whether to automatically sync writing plans to org-agenda."
  :type 'boolean
  :group 'org-scribe-planner)

(defun org-scribe-planner--add-agenda-entries (plan filepath)
  "Add scheduled agenda entries for PLAN to FILEPATH.
Replaces any previously generated entries under \"** Agenda Entries\"
rather than appending, so calling this repeatedly (e.g. re-running
`org-scribe-planner-sync-agenda') regenerates the schedule instead of
duplicating hundreds of TODOs."
  (when org-scribe-planner-sync-to-agenda
    (with-current-buffer (find-file-noselect filepath)
      (goto-char (point-min))

      ;; Add (or reset) the agenda entries heading
      (if (re-search-forward "^\\*\\* Agenda Entries" nil t)
          (let ((subtree-start (line-beginning-position))
                (subtree-end (progn (org-end-of-subtree t t) (point))))
            (delete-region subtree-start subtree-end)
            (goto-char subtree-start)
            (insert "** Agenda Entries\n")
            (insert ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n\n"))
        (goto-char (point-max))
        (insert "\n** Agenda Entries\n")
        (insert ":PROPERTIES:\n:VISIBILITY: folded\n:END:\n\n"))

      (goto-char (point-max))

      ;; Generate scheduled entries for each day
      (let ((schedule (org-scribe-planner--generate-day-schedule plan)))
        (dolist (day schedule)
          ;; Rest days get no entry: spare days, and days with no goal
          (unless (or (plist-get day :is-spare-day) (zerop (plist-get day :words)))
            (let ((date (plist-get day :date))
                  (words (plist-get day :words))
                  (cumulative (plist-get day :cumulative)))
              (insert (format "*** TODO Write %d words\n" words))
              (insert (format "SCHEDULED: <%s>\n" date))
              (insert (format ":PROPERTIES:\n"))
              (insert (format ":TARGET_WORDS: %d\n" words))
              (insert (format ":CUMULATIVE: %d\n" cumulative))
              (insert (format ":END:\n\n"))))))

      (save-buffer)
      (message "Agenda entries synced"))))

(defun org-scribe-planner--update-agenda-file-list (filepath)
  "Add FILEPATH to org-agenda-files if not already present.
No-ops (with a message) when `org-agenda-files' is set to a string — Org's
\"file of files\" indirection — since `member' requires a list and this
function does not attempt to edit an external file list."
  (require 'org-agenda)
  (cond
   ((not (listp org-agenda-files))
    (message "org-agenda-files points to a file (%s); add %s to it manually"
             org-agenda-files filepath))
   ((member filepath org-agenda-files) nil)
   (t
    (customize-save-variable 'org-agenda-files
                            (cons filepath org-agenda-files))
    (message "Added %s to org-agenda-files" filepath))))

;;;###autoload
(defun org-scribe-planner-sync-agenda ()
  "Sync active writing plan to org-agenda."
  (interactive)
  (org-scribe-planner--with-current-plan (plan file)
    (org-scribe-planner--add-agenda-entries plan file)
    (org-scribe-planner--update-agenda-file-list file)))

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

(defun org-scribe-planner--get-entry-target (entry)
  "Extract target from daily-word-count ENTRY.
ENTRY must be in new format: (date . (:words N :note \"...\" :target M)).
Returns nil if no target is stored."
  (plist-get (cdr entry) :target))

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

;;;###autoload
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


;;;###autoload
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

;;;###autoload
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
Expected words run through yesterday: today's goal isn't due until the
day is over, while today's words already count (see
`org-scribe-planner--status')."
  (let* ((total (org-scribe-plan-total-words plan))
         (current (org-scribe-plan-current-words plan))
         (percent (* 100.0 (/ (float current) total))))
    (message "Progress: %d/%d words (%.1f%%) | %s"
             current total percent
             (org-scribe-planner--describe-status (org-scribe-planner--status plan)))))

(defun org-scribe-planner--compute-recalculation-data (plan)
  "Compute the common recalculation values for PLAN.
Returns a plist with:
  :cumulative-actual — total words written so far
  :remaining-words   — total-words minus cumulative-actual
  :remaining-days    — working days in the schedule that have no actual entry"
  (let* ((daily-counts (org-scribe-plan-daily-word-counts plan))
         ;; current-words is kept in sync with the latest cumulative ledger
         ;; entry by every write path, so it IS the total written so far —
         ;; no need to re-derive it by summing daily-counts.
         (cumulative-actual (org-scribe-plan-current-words plan))
         (remaining-words (- (org-scribe-plan-total-words plan) cumulative-actual))
         (schedule (org-scribe-planner--generate-day-schedule plan))
         (remaining-days 0))
    (let ((today (org-scribe-planner--get-today-date)))
      (dolist (day schedule)
        (let ((date (plist-get day :date))
              (is-spare (plist-get day :is-spare-day)))
          ;; Only count today and future days; past empty days are unlogged history,
          ;; not future obligations.  Check for actual :words data, not mere
          ;; presence of a daily-counts entry — a note-only entry (e.g. from
          ;; `org-scribe-planner--add-spare-day-note' on a future day) carries
          ;; no word count and must still count as a remaining working day,
          ;; otherwise annotating a future day silently shrinks the pool of
          ;; remaining days and inflates the recalculated daily target (H8).
          (when (and (not (org-scribe-planner--get-entry-words (assoc date daily-counts)))
                     (not is-spare)
                     (not (string< date today)))
            (setq remaining-days (1+ remaining-days))))))
    (list :cumulative-actual cumulative-actual
          :remaining-words   remaining-words
          :remaining-days    remaining-days)))

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

(defun org-scribe-planner--set-total-keeping-daily (plan daily)
  "Set PLAN's total to what DAILY words a day give over its writing days."
  (setf (org-scribe-plan-total-words plan)
        (round (org-scribe-planner--total-for-daily plan daily)))
  (org-scribe-planner--sync-daily plan))

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
         (open-days (cl-count-if (lambda (day)
                                   (and (not (plist-get day :is-spare-day))
                                        (not (string< (plist-get day :date) as-of))))
                                 (org-scribe-planner--generate-day-schedule preview)))
         (newly-locked (- (length (org-scribe-plan-locked-goals preview))
                          (length (org-scribe-plan-locked-goals plan)))))
    (format "From %s: %d words left over %d writing days.\nDaily target %d → %d words (%s%d, %s%.1f%%)%s. End date %s unchanged."
            as-of
            (- (org-scribe-plan-total-words plan) (org-scribe-plan-current-words plan))
            open-days old-daily new-daily
            (if (>= change 0) "+" "") change
            (if (>= change 0) "+" "")
            (if (> old-daily 0) (* 100.0 (/ (float change) old-daily)) 0.0)
            (if (> newly-locked 0) (format ". Past days locked: %d" newly-locked) "")
            (org-scribe-plan-end-date preview))))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun org-scribe-planner-check-progress ()
  "Show where the active plan stands; when ahead or behind, offer to adjust it.
Ahead, you can reduce the daily target (keeping the end date) or finish
early (keeping the daily target); behind, increase the daily target or
extend the deadline.  Every option shows its outcome first, and keeping
the plan as it is is the default.  Expected words run through yesterday:
today's goal isn't due until the day is over."
  (interactive)
  (org-scribe-planner--with-current-plan (plan file)
    (let ((status (org-scribe-planner--status plan)))
      (cond
       ((null status)
        (message "The plan has no schedule yet."))
       ((eq (plist-get status :status) 'on-track)
        (message "On track: %d words written, as expected by the end of yesterday."
                 (plist-get status :actual)))
       (t
        (let* ((ahead (eq (plist-get status :status) 'ahead))
               (as-of (org-scribe-planner--default-recalculation-date plan))
               (chosen
                (org-scribe-planner--choose-change
                 plan
                 (format "%s (expected %d, written %d). Adjust the plan?"
                         (org-scribe-planner--describe-status status)
                         (plist-get status :expected) (plist-get status :actual))
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

;;;###autoload
(defun org-scribe-planner-recalculate ()
  "Change the active writing plan, previewing the result before applying it.
A new total, daily target or length asks which of the other two to keep;
\"Two values\" sets two and calculates the third.  Adding or removing
spare days asks how the plan absorbs them: keep the end date (the daily
target changes), move the end date (keep the daily target), or keep both
\(the total changes).  Every option shows its outcome first.  Past days
keep their goals throughout."
  (interactive)
  (org-scribe-planner--with-current-plan (plan file)
    ;; If the change doesn't finish (no option possible, C-g), the plan in
    ;; memory goes back to how it was: nothing half-changed stays active
    (let ((backup (org-scribe-planner--copy-plan plan))
          (finished nil))
     (unwind-protect
      (progn
    (let ((choice (org-scribe-planner--choose
                   "What changed? "
                   '("Total words"
                     "Daily words"
                     "Days available"
                     "Two values (third calculated)"
                     "Start date"
                     "Words outside the manuscript"
                     "Writing pattern"
                     "Weekends"
                     "Day goal: set"
                     "Day goal: remove"
                     "Day goals: clear from today on"
                     "Add/modify spare days"
                     "Remove spare days"
                     "Clear all spare days"))))
      (pcase choice
        ("Total words"
         (let ((total (org-scribe-planner--read-positive-number
                       "New total words: " (org-scribe-plan-total-words plan))))
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
                      (org-scribe-planner--calculate-time p daily))))))))

        ("Daily words"
         (let ((daily (org-scribe-planner--read-positive-number
                       "New daily words: " (org-scribe-plan-daily-words plan))))
           (org-scribe-planner--choose-change
            plan (format "%d words/day: keep" daily)
            `(("Total (the end date moves)"
               . ,(lambda (p) (org-scribe-planner--calculate-time p daily)))
              ("Writing days (the total changes)"
               . ,(lambda (p) (org-scribe-planner--set-total-keeping-daily p daily)))))))

        ("Days available"
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

        ("Two values (third calculated)"
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

        ("Start date"
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

        ("Words outside the manuscript"
         (let* ((base (org-scribe-planner--outside-base file))
                (old-total (org-scribe-planner--outside-total plan base))
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
           (let* ((difference (- (org-scribe-planner--outside-total target base) old-total))
                  (new-typed (org-scribe-plan-outside-words target))
                  (new-files (org-scribe-plan-outside-files target))
                  (configure (lambda (p)
                               (setf (org-scribe-plan-outside-words p) new-typed)
                               (setf (org-scribe-plan-outside-files p) (copy-tree new-files)))))
             (if (zerop difference)
                 (progn (funcall configure plan)
                        (message "Words outside the manuscript: %s (the total is unchanged)"
                                 (org-scribe-planner--describe-outside plan base)))
               ;; The record moves by the difference; are these words among
               ;; the starting words already declared, or new to the plan?
               (org-scribe-planner--choose-change
                plan
                (format "%s%d words outside the manuscript. These words:"
                        (if (> difference 0) "+" "") difference)
                `(("Are among the starting words already declared (correct the record)"
                   . ,(lambda (p) (funcall configure p)
                        (org-scribe-planner--shift-written p difference)))
                  ("Are new to the plan, written before it (change the starting words too)"
                   . ,(lambda (p) (funcall configure p)
                        (org-scribe-planner--shift-written p difference t))))
                (org-scribe-planner--copy-plan plan)
                (lambda (p)
                  (format "written %d, %d at the start; %s"
                          (org-scribe-plan-current-words p)
                          (org-scribe-plan-preexisting-words p)
                          (org-scribe-planner--describe-status (org-scribe-planner--status p)))))))))

        ("Writing pattern"
         (let ((choice (org-scribe-planner--read-pattern)))
           (setf (org-scribe-plan-pattern plan) (car choice))
           (setf (org-scribe-plan-pattern-config plan) (cdr choice))
           (org-scribe-planner--sync-daily plan)
           (message "Pattern: %s. Past days keep their goals."
                    (org-scribe-planner--describe-pattern plan))))

        ("Weekends"
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

        ("Day goal: set"
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

        ("Day goal: remove"
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

        ("Day goals: clear from today on"
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

        ("Add/modify spare days"
         (let ((before (org-scribe-planner--copy-plan plan)))
           (org-scribe-planner--configure-spare-days plan)
           (org-scribe-planner--offer-adjustment plan before)))

        ("Remove spare days"
         (if (null (org-scribe-plan-spare-days plan))
             (message "No spare days to remove")
           (let ((before (org-scribe-planner--copy-plan plan))
                 (date (org-scribe-planner--choose "Select date to remove: "
                                                   (sort (copy-sequence (org-scribe-plan-spare-days plan))
                                                         #'string<))))
             (setf (org-scribe-plan-spare-days plan)
                   (remove date (org-scribe-plan-spare-days plan)))
             (org-scribe-planner--offer-adjustment plan before))))

        ("Clear all spare days"
         (let ((spare-days (org-scribe-plan-spare-days plan)))
           (if (null spare-days)
               (message "No spare days to clear")
             (when (y-or-n-p (format "Remove all %d spare days? " (length spare-days)))
               (let ((before (org-scribe-planner--copy-plan plan)))
                 (setf (org-scribe-plan-spare-days plan) nil)
                 (org-scribe-planner--offer-adjustment plan before)))))))

      (org-scribe-planner--save-plan plan file)
      (org-scribe-planner--refresh-agenda-if-synced plan file)
      (setq org-scribe-planner--current-plan plan)
      (setq finished t)
      (org-scribe-planner-show-calendar plan file)))
      (unless finished
        (setq org-scribe-planner--current-plan backup))))))

;;; Milestone Tracking

(defun org-scribe-planner--get-enhanced-milestones (plan)
  "Calculate enhanced milestone information for PLAN.
Returns a list of plists with :percent, :words, :reached, :date, and :expected.
Tracks actual progress and calculates expected dates for unreached milestones."
  (let* ((total-words (org-scribe-plan-total-words plan))
         (daily-counts (org-scribe-plan-daily-word-counts plan))
         (schedule (org-scribe-planner--generate-day-schedule plan))
         (milestone-percentages '(25 50 75 100))
         (today (org-scribe-planner--get-today-date))
         ;; The manuscript starts at its pre-existing words; a milestone
         ;; they already reach has no date (reached before the plan)
         (cumulative-actual (or (org-scribe-plan-preexisting-words plan) 0))
         (actual-by-date nil)
         (results nil))

    ;; Build cumulative actual progress by date.  daily-data's :words IS
    ;; already the cumulative total as of that date, so no summation here —
    ;; just carry the latest known value forward.
    (dolist (day schedule)
      (let* ((date (plist-get day :date))
             (daily-entry (assoc date daily-counts)))
        (when daily-entry
          (let ((actual (org-scribe-planner--get-entry-words daily-entry)))
            ;; Only count entries with actual word counts (not note-only entries)
            (when (numberp actual)
              (setq cumulative-actual actual)
              (push (cons date cumulative-actual) actual-by-date))))))

    ;; Reverse to get chronological order
    (setq actual-by-date (nreverse actual-by-date))

    ;; Process each milestone
    (dolist (percent milestone-percentages)
      (let* ((milestone-words (/ (* percent total-words) 100))
             (reached (>= cumulative-actual milestone-words))
             (actual-date nil)
             (expected-date nil))

        (if reached
            ;; Find date when milestone was reached
            (progn
              (dolist (entry actual-by-date)
                (when (and (not actual-date)
                          (>= (cdr entry) milestone-words))
                  (setq actual-date (car entry)))))

          ;; Calculate expected date for unreached milestone
          (let ((remaining-words (- milestone-words cumulative-actual))
                (projection-total cumulative-actual))

            (dolist (day schedule)
              (let* ((date (plist-get day :date))
                     (is-spare (plist-get day :is-spare-day))
                     (has-actual (assoc date daily-counts))
                     (current-daily (plist-get day :words)))

                ;; Project forward using days without actuals.  Today may
                ;; already hold a partial entry; what is left of its goal
                ;; still counts, or a plan on schedule never reaches 100%.
                (when (and (or (not has-actual) (equal date today))
                          (not is-spare)
                          (not expected-date))
                  (setq projection-total
                        (+ projection-total
                           (if has-actual
                               (max 0 (- current-daily
                                         (or (org-scribe-planner--entry-delta
                                              daily-counts date
                                              (org-scribe-plan-preexisting-words plan))
                                             0)))
                             current-daily)))

                  (when (>= projection-total milestone-words)
                    (setq expected-date date)))))))

        ;; Add milestone to results
        (push (list :percent percent
                   :words milestone-words
                   :reached reached
                   :date actual-date
                   :expected expected-date)
              results)))

    ;; Return in ascending order (25%, 50%, 75%, 100%)
    (nreverse results)))

;;;###autoload
(defun org-scribe-planner-show-milestones ()
  "Show milestone dates for the active writing plan with actual and expected dates."
  (interactive)
  (org-scribe-planner--with-current-plan (plan _)
    (let ((milestones (org-scribe-planner--get-enhanced-milestones plan)))
      (with-current-buffer (get-buffer-create "*Writing Plan Milestones*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize (format "Milestones for: %s\n\n"
                                     (org-scribe-plan-title plan))
                             'face 'org-level-1))

          (dolist (milestone milestones)
            (let ((percent (plist-get milestone :percent))
                  (words (plist-get milestone :words))
                  (reached (plist-get milestone :reached))
                  (date (plist-get milestone :date))
                  (expected (plist-get milestone :expected)))

              (cond
               ;; Milestone reached - show actual date
               (reached
                (insert (propertize
                        (format "%3d%% - %s (%d words) - Reached\n"
                               percent
                               (or date "before the plan")
                               words)
                        'face 'org-done)))

               ;; Milestone not reached with expected date
               (expected
                (insert (format "%3d%% - Not reached (%d words) - expected on %s\n"
                               percent
                               words
                               expected)))

               ;; Milestone not reached, no expected date (past end date or insufficient days)
               (t
                (insert (propertize
                        (format "%3d%% - Not reached (%d words) - cannot be reached by end date\n"
                               percent
                               words)
                        'face 'org-warning))))))

          (insert "\n")
          (insert (propertize "Press 'q' to close | 'r' to refresh | 'c' to calendar view\n"
                            'face 'shadow))
          (goto-char (point-min))
          (require 'org-scribe-planner-dashboards)
          (org-scribe-planner-dashboard-mode)
          (display-buffer (current-buffer)))))))

;;; Active Plan Management

;;;###autoload
(defun org-scribe-planner-show-current-plan ()
  "Display the calendar for the currently active plan.
If no plan is active, prompt to load one."
  (interactive)
  (org-scribe-planner--with-current-plan (plan file)
    (org-scribe-planner-show-calendar plan file)))

;;;###autoload
(defun org-scribe-planner-current-plan-info ()
  "Show information about the currently active plan."
  (interactive)
  (if (and org-scribe-planner--current-plan
           org-scribe-planner--current-plan-file)
      (message "Active plan: %s (%s)"
               (org-scribe-plan-title org-scribe-planner--current-plan)
               org-scribe-planner--current-plan-file)
    (message "No active plan. Use `org-scribe-planner-load-plan' to load one.")))

;;; org-scribe Integration

;; These features activate automatically when org-scribe is present.
;; Set any defcustom below to nil before loading the planner to disable
;; the corresponding feature.

(declare-function org-scribe-project-root "core/org-scribe-core")
(declare-function org-scribe-project-structure "core/org-scribe-core")
(declare-function org-scribe-project-type "core/org-scribe-core")
(declare-function org-scribe--count-words-region "counting/org-scribe-wordcount")
(declare-function org-scribe--project-marker-get "core/org-scribe-core")
(declare-function org-scribe--project-marker-set "core/org-scribe-core")
(declare-function org-scribe-planner-gate "core/org-scribe-core")
(declare-function org-scribe-ews-org-count-words "counting/org-scribe-wordcount")
(declare-function org-scribe-create-novel-project "templates/org-scribe-project")
(declare-function org-scribe-create-short-story-project "templates/org-scribe-project")

(defcustom org-scribe-planner-auto-load-plan t
  "When non-nil, find and load the project plan on entering an org-scribe project.
The plan file is located via `org-scribe-planner--find-plan-file'."
  :type 'boolean
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-auto-push-wordcount t
  "When non-nil, push the manuscript word count to the active plan automatically.
Activated after each call to `org-scribe-ews-org-count-words'."
  :type 'boolean
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-offer-plan-on-create t
  "When non-nil, offer to create a writing plan after a new org-scribe project is created."
  :type 'boolean
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-show-mode-line t
  "When non-nil, show today's word count status in the mode line when a plan is active."
  :type 'boolean
  :group 'org-scribe-planner)

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
     ((let* ((title (org-scribe--project-marker-get root "Writing project"))
             (slug (when title
                     (downcase (replace-regexp-in-string "[^[:alnum:]]" "-" title))))
             (candidate-2 (when slug
                            (expand-file-name (concat slug "-plan.org") root))))
        (when (and candidate-2 (file-exists-p candidate-2))
          candidate-2)))
     ;; 3. Any single .org file in ROOT with a TOTAL_WORDS property
     ((cl-find-if
       (lambda (f)
         (with-temp-buffer
           (insert-file-contents f)
           (goto-char (point-min))
           (re-search-forward "^:TOTAL_WORDS:" nil t)))
       (directory-files root t "\\.org$" t))))))

(defun org-scribe-planner--auto-load-plan ()
  "Try to auto-load the plan associated with the current org-scribe project.
Silently does nothing if the project has not opted into the planner
\(see `org-scribe-planner-gate'), no plan candidate is found, or a plan
is already active."
  (when (and (featurep 'org-scribe)
             (not org-scribe-planner--current-plan))
    (when-let* ((root (org-scribe-project-root))
                (_ (eq (org-scribe-planner-gate root) 'yes))
                (plan-file (org-scribe-planner--find-plan-file root))
                (plan (ignore-errors (org-scribe-planner--load-plan plan-file))))
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

;;; Word-count wiring

(defun org-scribe-planner--parent-has-wordcount-p ()
  "Return non-nil if the immediate parent heading of point has a WORDCOUNT property.
Used to avoid double-counting when WORDCOUNT is set on both parent and child headings
\(e.g. act → chapter → scene, where parent totals aggregate their children)."
  (save-excursion
    (when (org-up-heading-safe)
      (org-entry-get nil "WORDCOUNT"))))

(defun org-scribe-planner--sum-wordcounts (novel-file)
  "Sum WORDCOUNT properties in NOVEL-FILE, counting only the highest level in each subtree.
A heading is included only when its immediate parent heading does not already carry a
WORDCOUNT property.  This prevents double-counting in hierarchical manuscripts where
WORDCOUNT on a chapter or act is the aggregate of its scene children.
Prefers a live buffer so unsaved changes are included.  When summing a
live buffer, widens first (`org-with-wide-buffer') so an active
narrowing — e.g. org-scribe's focus mode, which narrows to a single
subtree — does not collapse the \"manuscript total\" down to whatever
subtree happens to be visible."
  (when (and novel-file (file-readable-p novel-file))
    (let ((total 0)
          (live-buffer (find-buffer-visiting novel-file)))
      (if live-buffer
          (with-current-buffer live-buffer
            (org-with-wide-buffer
             (org-map-entries
              (lambda ()
                (let ((wc (org-entry-get nil "WORDCOUNT")))
                  (when (and wc (not (org-scribe-planner--parent-has-wordcount-p)))
                    (setq total (+ total (string-to-number wc)))))))))
        (with-temp-buffer
          (insert-file-contents novel-file)
          (org-mode)
          (org-map-entries
           (lambda ()
             (let ((wc (org-entry-get nil "WORDCOUNT")))
               (when (and wc (not (org-scribe-planner--parent-has-wordcount-p)))
                 (setq total (+ total (string-to-number wc)))))))))
      total)))

(defun org-scribe-planner--wordcount-from-manuscript ()
  "Return the total word count for the current org-scribe project manuscript.
Returns nil when no count is available, causing the planner to prompt instead."
  (when (featurep 'org-scribe)
    (when-let* ((struct (ignore-errors (org-scribe-project-structure)))
                (manuscript-file (plist-get struct :manuscript-file)))
      (org-scribe-planner--sum-wordcounts manuscript-file))))

(defun org-scribe-planner--plan-file-in-current-project-p (plan-file)
  "Return non-nil if PLAN-FILE lives under the current project's root.
Used to guard against pushing word counts from the manuscript of one
project into the plan of another (M9): `org-scribe-planner--wordcount-from-manuscript'
always reads the *current* project's manuscript (via `default-directory'),
but `org-scribe-planner--current-plan-file' can still point at whatever
project's plan was active last — the auto-load hook only runs when no plan
is active at all, so it never swaps plans when switching between two
projects that both already have one loaded."
  (when-let* ((root (ignore-errors (org-scribe-project-root))))
    (file-in-directory-p plan-file root)))

(defun org-scribe-planner--ensure-plan-for-current-project ()
  "Drop the active plan if it does not belong to the current project.
When `org-scribe-planner--current-plan-file' is set but is not under the
current project root, clears it and tries to auto-load the plan that
actually belongs to this project instead.  No-op when there is no active
plan, project detection is unavailable (`org-scribe-project-root' not
loaded — the planner is usable standalone, without org-scribe), or project
detection is inconclusive."
  (when (and org-scribe-planner--current-plan-file
             (fboundp 'org-scribe-project-root)
             (ignore-errors (org-scribe-project-root))
             (not (org-scribe-planner--plan-file-in-current-project-p
                   org-scribe-planner--current-plan-file)))
    (setq org-scribe-planner--current-plan nil)
    (setq org-scribe-planner--current-plan-file nil)
    (org-scribe-planner--auto-load-plan)))

;;;###autoload
(defun org-scribe-planner-record-total (total)
  "Record TOTAL as today's cumulative manuscript word count in the active plan.
Upserts today's DAILY_WORD_COUNTS entry with TOTAL (preserving any
existing note), saves the plan, and fires
`org-scribe-planner-after-progress-update-hook' with today's derived net
delta.  Idempotent: calling repeatedly with the same TOTAL on the same
day reproduces the same entry and delta — there is no separate baseline
to roll forward, unlike the old SYNC_DATE/SYNC_WORDS mechanism.

No-op when `org-scribe-planner-auto-push-wordcount' or
`org-scribe-planner-auto-track-daily' is nil, when there is no active
plan for the current project, when the active plan belongs to a
different project than the current manuscript and no matching plan can
be found for this project (M9 — prevents cross-project data corruption
instead of silently recording into the wrong plan), or when TOTAL is
not positive."
  (when (and org-scribe-planner-auto-push-wordcount
             org-scribe-planner-auto-track-daily)
    (unless (and org-scribe-planner--current-plan
                 org-scribe-planner--current-plan-file)
      (org-scribe-planner--auto-load-plan))
    (org-scribe-planner--ensure-plan-for-current-project)
    (when (and org-scribe-planner--current-plan
               org-scribe-planner--current-plan-file
               (> total 0))
      (let* ((plan org-scribe-planner--current-plan)
             (file org-scribe-planner--current-plan-file)
             (today (org-scribe-planner--get-today-date))
             (existing (assoc today (org-scribe-plan-daily-word-counts plan)))
             (existing-note (when existing
                              (org-scribe-planner--get-entry-note existing)))
             ;; The manuscript's count plus the words outside it
             (written (org-scribe-planner--written-total
                       plan total (org-scribe-planner--outside-base file)))
             (entry-data (list :words written :note (or existing-note ""))))
        (org-scribe-planner--lock-past-goals plan)
        ;; Upsert today's entry, preserving any hand-written note
        (if existing
            (setcdr existing entry-data)
          (setf (org-scribe-plan-daily-word-counts plan)
                (cons (cons today entry-data)
                      (org-scribe-plan-daily-word-counts plan))))
        (setf (org-scribe-plan-current-words plan) written)
        (org-scribe-planner--save-plan plan file)
        (setq org-scribe-planner--current-plan plan)
        (org-scribe-planner--warn-if-below-start plan)
        (run-hook-with-args 'org-scribe-planner-after-progress-update-hook
                            plan
                            (org-scribe-planner--entry-delta
                             (org-scribe-plan-daily-word-counts plan) today
                             (org-scribe-plan-preexisting-words plan))
                            today)))))

;;; Mode-line status

(defun org-scribe-planner--mode-line ()
  "Return a mode-line string showing today's target/actual, or nil.
In practice a plan is only ever loaded when the project's gate is
\\='yes (see `org-scribe-planner-gate'), but the gate is checked
explicitly here too so the mode-line contribution stays off even if
some other path ever loads a plan into a gated-off project.
The target is today's own goal from the schedule: on a spare day the
string reads \"[W:actual rest]\", and outside the plan just \"[W:actual]\"."
  (when-let* ((plan org-scribe-planner--current-plan)
              (_ (eq (ignore-errors (org-scribe-planner-gate)) 'yes))
              (today (org-scribe-planner--get-today-date))
              (actual (org-scribe-planner--entry-delta
                       (org-scribe-plan-daily-word-counts plan) today
                       (org-scribe-plan-preexisting-words plan))))
    (let ((day (cl-find today
                        (ignore-errors (org-scribe-planner--generate-day-schedule plan))
                        :key (lambda (d) (plist-get d :date))
                        :test #'string=)))
      (cond
       ((null day) (format " [W:%d]" actual))
       ((plist-get day :is-spare-day) (format " [W:%d rest]" actual))
       (t (format " [W:%d/%d]" actual (plist-get day :words)))))))

;;; Project title lookup

(defun org-scribe-planner--project-title (root)
  "Return the project title recorded in ROOT/.org-scribe-project, or nil."
  (org-scribe--project-marker-get root "Writing project"))

;;; Offer plan creation on project creation

(defun org-scribe-planner--offer-plan-on-create (base-dir title &rest _)
  "Offer to create a writing plan for the project just created under BASE-DIR.
BASE-DIR and TITLE are the first two arguments of
`org-scribe-create-novel-project' / `org-scribe-create-short-story-project'
\(this function is installed as :after advice on both, so it receives
their exact arguments).  BASE-DIR is the *parent* directory the creation
command was given, not the new project's own directory — mirror the
`(expand-file-name title base-dir)' computation those functions use
internally to get the actual project directory, so that
`org-scribe-planner-new-plan' is called with an explicit TITLE and
SAVE-PATH of <project>/plan.org instead of falling back to generic
prompts and `org-scribe-planner-directory'.

Also records the answer in the project's marker file as \"# Planner:
yes\" or \"# Planner: no\" (see `org-scribe-planner-gate'), so a decline
is remembered — the project is never asked again, and every other
gate-aware feature (auto-load, mode line, health report) stays off
until the author explicitly enables it later."
  (let ((project-dir (expand-file-name title base-dir)))
    (if (yes-or-no-p (format "Create a writing plan for \"%s\" with org-scribe-planner? "
                             title))
        (progn
          (org-scribe--project-marker-set project-dir "Planner" "yes")
          (org-scribe-planner-new-plan title (expand-file-name "plan.org" project-dir)))
      (org-scribe--project-marker-set project-dir "Planner" "no"))))

;;; Wire up all features (only when org-scribe is present)

(with-eval-after-load 'org-scribe
  ;; Plan-path persistence — always active.  `org-scribe-planner-new-plan'
  ;; and `org-scribe-planner-load-plan' both run this hook themselves after
  ;; activating a plan, so a single hook covers every path (manual new/load,
  ;; the org-scribe-plan dispatcher, and auto-load) with no extra advice.
  (add-hook 'org-scribe-planner-after-plan-load-hook #'org-scribe-planner--save-plan-path)

  ;; Word-count and project-root wiring — always active.
  (setq org-scribe-planner-wordcount-function #'org-scribe-planner--wordcount-from-manuscript)
  (setq org-scribe-planner-project-root-function #'org-scribe-project-root)

  ;; Auto-load plan when opening a project file.
  (when org-scribe-planner-auto-load-plan
    (add-hook 'org-scribe-mode-hook #'org-scribe-planner--auto-load-plan))

  ;; Note: pushing the word count to the active plan after each count is no
  ;; longer advice-based — counting/org-scribe-wordcount.el calls
  ;; `org-scribe-planner-record-total' directly at the end of
  ;; `org-scribe-ews-org-count-words' (guarded on that function existing),
  ;; and `org-scribe-planner-auto-push-wordcount' is checked inside
  ;; `org-scribe-planner-record-total' itself.

  ;; Offer to create a plan when a new project is created.
  (when org-scribe-planner-offer-plan-on-create
    (advice-add 'org-scribe-create-novel-project :after #'org-scribe-planner--offer-plan-on-create)
    (advice-add 'org-scribe-create-short-story-project :after #'org-scribe-planner--offer-plan-on-create))

  ;; Mode-line status indicator.
  (when org-scribe-planner-show-mode-line
    (add-to-list 'mode-line-misc-info '(:eval (org-scribe-planner--mode-line)))))

;;; Unified project plan entry point

;;;###autoload
(defun org-scribe-plan ()
  "Open the writing plan for the current org-scribe project.
If a plan is already active, show its calendar.  If the project has
declined the planner (see `org-scribe-planner-gate'), offer to enable
it before doing anything else.  If a plan file exists for the current
project but is not yet loaded, load it silently first.  If no plan
file exists, offer to create one.  Falls back to
`org-scribe-planner-show-current-plan' when not inside an org-scribe project."
  (interactive)
  (let* ((root (ignore-errors (org-scribe-project-root)))
         (in-project (and root
                          (file-exists-p
                           (expand-file-name ".org-scribe-project" root)))))
    (cond
     ;; Plan already active — show it
     (org-scribe-planner--current-plan
      (org-scribe-planner--lock-past-goals org-scribe-planner--current-plan)
      (org-scribe-planner-show-calendar org-scribe-planner--current-plan
                                        org-scribe-planner--current-plan-file))
     ;; In a project that has declined the planner — offer to enable first
     ((and in-project (eq (org-scribe-planner-gate root) 'no))
      (if (yes-or-no-p "The planner is disabled for this project.  Enable it? ")
          (progn
            (org-scribe--project-marker-set root "Planner" "yes")
            (org-scribe-plan))
        (message "org-scribe-planner: staying disabled for this project.")))
     ;; In project with a valid saved plan file — load silently, then show
     ((and in-project
           (org-scribe-planner--plan-file-valid-p
            (org-scribe-planner--find-plan-file root)))
      (let* ((plan-file (org-scribe-planner--find-plan-file root))
             (plan (org-scribe-planner--load-plan plan-file)))
        (org-scribe-planner--lock-past-goals plan)
        (setq org-scribe-planner--current-plan plan)
        (setq org-scribe-planner--current-plan-file plan-file)
        (run-hook-with-args 'org-scribe-planner-after-plan-load-hook plan plan-file)
        (org-scribe-planner-show-calendar plan plan-file)))
     ;; In project but no valid plan — offer to create one
     (in-project
      (when (yes-or-no-p "No writing plan found for this project.  Create one? ")
        (org-scribe--project-marker-set root "Planner" "yes")
        (org-scribe-planner-new-plan (org-scribe-planner--project-title root)
                                     (expand-file-name "plan.org" root))))
     ;; Not in an org-scribe project — standard show-or-load flow
     (t
      (org-scribe-planner-show-current-plan)))))

;;; Per-project enable/disable

;;;###autoload
(defun org-scribe-planner-enable-for-project ()
  "Enable the writing planner for the current org-scribe project.
Sets \"# Planner: yes\" in .org-scribe-project.  With the gate open,
auto-load, the mode line, and the health report's Writing Plan section
all resume for this project.  Errors if not inside an org-scribe project."
  (interactive)
  (let ((root (org-scribe-project-root)))
    (unless (file-exists-p (expand-file-name ".org-scribe-project" root))
      (user-error "Not inside an org-scribe project"))
    (org-scribe--project-marker-set root "Planner" "yes")
    (message "org-scribe-planner: enabled for this project.")))

;;;###autoload
(defun org-scribe-planner-disable-for-project ()
  "Disable the writing planner for the current org-scribe project.
Sets \"# Planner: no\" in .org-scribe-project.  Does not unload an
already-active plan from memory, but auto-load, the mode line, and the
health report's Writing Plan section all stop for this project.
Errors if not inside an org-scribe project."
  (interactive)
  (let ((root (org-scribe-project-root)))
    (unless (file-exists-p (expand-file-name ".org-scribe-project" root))
      (user-error "Not inside an org-scribe project"))
    (org-scribe--project-marker-set root "Planner" "no")
    (message "org-scribe-planner: disabled for this project.")))

;;; Planner hydra submenu

(declare-function hydra-org-scribe/body "ui/org-scribe-hydra")

(when (require 'hydra nil t)
  ;;;###autoload (autoload 'hydra-org-scribe-planner/body "planning/org-scribe-planner" nil t)
  (defhydra hydra-org-scribe-planner (:color blue :hint nil)
    "
  org-scribe-planner
  ──────────────────────────────────────────────────
  _p_: Open project plan (load or create)
  _n_: New plan (fresh start)
  _o_: Onboard existing project
  _l_: Load plan
  _u_: Sync word count (from manuscript)
  _d_: Add session note
  _r_: Recalculate plan
  _a_: Adjust remaining days (keep end date)
  _e_: Adjust end date (keep daily words)
  _c_: Check progress (adjust if ahead/behind)
  _m_: Milestones
  _w_: Weekly summary
  _M_: Month grid
  _D_: Dashboards menu
  _E_: Enable for project
  _X_: Disable for project
  _q_: Back to main menu
  "
    ("p" org-scribe-plan "open project plan")
    ("n" org-scribe-planner-new-plan "new plan")
    ("o" org-scribe-planner-onboard-existing-project "onboard existing")
    ("l" org-scribe-planner-load-plan "load plan")
    ("u" org-scribe-planner-update-progress "sync word count")
    ("d" org-scribe-planner-add-session-note "session note")
    ("r" org-scribe-planner-recalculate "recalculate")
    ("a" org-scribe-planner-adjust-remaining-plan "adjust remaining")
    ("e" org-scribe-planner-adjust-end-date "adjust end date")
    ("c" org-scribe-planner-check-progress "check progress")
    ("m" org-scribe-planner-show-milestones "milestones")
    ("w" org-scribe-planner-show-weekly-summary "weekly summary")
    ("M" org-scribe-planner-show-month-calendar "month grid")
    ("D" org-scribe-planner-dashboards-menu "dashboards")
    ("E" org-scribe-planner-enable-for-project "enable for project")
    ("X" org-scribe-planner-disable-for-project "disable for project")
    ("q" hydra-org-scribe/body "back")))

;;; Provide

(provide 'org-scribe-planner)

;;; org-scribe-planner.el ends here
