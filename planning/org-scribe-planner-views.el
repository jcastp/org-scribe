;;; org-scribe-planner-views.el --- Calendar, weeks and agenda for org-scribe-planner -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Javier Castilla
;; URL: https://codeberg.org/jcastp/org-scribe

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The plan calendar buffer and its mode, the weekly summary and month
;; grid, milestones, and the org-agenda entries.
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

(declare-function org-scribe-planner-add-session-note "org-scribe-planner-edit")
(declare-function org-scribe-planner-adjust-end-date "org-scribe-planner-edit")
(declare-function org-scribe-planner-adjust-remaining-plan "org-scribe-planner-edit")
(declare-function org-scribe-planner-check-progress "org-scribe-planner-edit")
(declare-function org-scribe-planner-dashboard-mode "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-dashboards-menu "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-recalculate "org-scribe-planner-edit")
(declare-function org-scribe-planner-show-burndown "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-cumulative-progress "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-heatmap "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-multi-metric-dashboard "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-performance-analytics "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-progress-dashboard "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-split-dashboards "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-velocity "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-velocity-chart "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-show-velocity-trends "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-update-progress "org-scribe-planner-edit")

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
For the current week :planned-total runs through yesterday, as
`org-scribe-planner-check-progress' measures (the plan's status counts
today's goal too).  A finished week's
leaves out the schedule adjustment of a later recalculation from
progress, so it stays what it was (see
`org-scribe-planner--planned-total').  As in wordplanner's
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
                                 (org-scribe-planner--planned-through plan end)
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

;;;###autoload (autoload 'org-scribe-planner-show-weekly-summary "org-scribe-planner" nil t)
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
          ;; Find the table by search, not a line count: the legend above
          ;; it moved it down, and `org-table-align' off the table did
          ;; nothing, leaving it ragged.
          (goto-char (point-min))
          (re-search-forward "^|")
          (org-table-align)
          (goto-char (point-min))
          (setq buffer-read-only t)
          ;; Copy the keymap first: the buffer is in `org-mode', whose map
          ;; `local-set-key' would otherwise modify for every Org buffer,
          ;; turning `q' into `quit-window' in the manuscript.
          (use-local-map (copy-keymap (or (current-local-map) (make-sparse-keymap))))
          (local-set-key (kbd "q") #'quit-window)
          (pop-to-buffer (current-buffer)))))))

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

;;;###autoload (autoload 'org-scribe-planner-show-month-calendar "org-scribe-planner" nil t)
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
          (pop-to-buffer (current-buffer)))))))

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
        (when-let* ((line (org-scribe-planner--outside-line
                           plan (org-scribe-planner--outside-base filepath))))
          (insert (format "  %s\n" line))
          (insert (format "                 (%s)\n"
                          (org-scribe-planner--describe-outside
                           plan (org-scribe-planner--outside-base filepath)))))
        (when-let* ((status (ignore-errors (org-scribe-planner--status plan))))
          (insert (format "  Status:        %s\n"
                          (org-scribe-planner--describe-status status))))
        (when-let* ((stale (ignore-errors (org-scribe-planner--rebase-stale plan))))
          (insert (format "  Out of date:   %s\n                 [a] adjust remaining days to count them\n"
                          (org-scribe-planner--describe-stale stale))))
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
              (cumulative-actual 0))

          (dolist (day schedule)
            (let* ((date (plist-get day :date))
                   (current-daily-words (plist-get day :words))
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

              ;; daily-data's :words IS already the cumulative total as of
              ;; this date, so no summation — just carry it forward.
              (when actual
                (setq cumulative-actual (plist-get daily-data :words)))

              ;; Week header (on the first day of the week)
              (when (or is-monday (null prev-week-start))
                (insert (propertize (format "\nWeek %d:\n" week-num) 'face 'org-level-3))
                ;; Add column headers
                (insert (propertize
                        (format "  %-25s  %-13s  %-15s  %-15s  %-23s  %s\n"
                               "Date (Day)"
                               "Daily Target"
                               "Planned Total"
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
                     ;; The plan's own figure, not a projection from the
                     ;; last entry: that one fell after a missed day.
                     (expected-col (format "%d words"
                                           (org-scribe-planner--planned-total plan day)))
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
      ;; Select it, so its keys ([q] quit, ...) work at once.
      (pop-to-buffer buffer))))

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
                  (cumulative (org-scribe-planner--planned-total plan day)))
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

;;;###autoload (autoload 'org-scribe-planner-sync-agenda "org-scribe-planner" nil t)
(defun org-scribe-planner-sync-agenda ()
  "Sync active writing plan to org-agenda."
  (interactive)
  (org-scribe-planner--with-current-plan (plan file)
    (org-scribe-planner--add-agenda-entries plan file)
    (org-scribe-planner--update-agenda-file-list file)))

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
          (let ((projection-total cumulative-actual))

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

;;;###autoload (autoload 'org-scribe-planner-show-milestones "org-scribe-planner" nil t)
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
          (pop-to-buffer (current-buffer)))))))

(provide 'org-scribe-planner-views)

;;; org-scribe-planner-views.el ends here
