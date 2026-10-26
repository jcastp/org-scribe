;;; test-planner-views.el --- Tests for the weekly summary, month grid and week start -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the planner's views by week and month: the first day of the
;; week (`org-scribe-planner-week-start-day'), the weekly summary, the
;; month grid, the heatmap's columns and the plan calendar's weeks, and
;; today's status while it is under way.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-scribe-planner)
(require 'org-scribe-planner-dashboards)

;;; Helpers

(defmacro test-views--on (today &rest body)
  "Run BODY with today stubbed to TODAY and windows not displayed."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'org-scribe-planner--get-today-date) (lambda () ,today))
             ((symbol-function 'display-buffer) #'ignore))
     ,@body))

(defun test-views--plan ()
  "14,000 words over 14 days from Sunday Nov 1, 2026, weekends off.
Written: 1,400 on Nov 2, 1,500 on Nov 3, nothing on Nov 4, 900 on Nov 5,
1,400 on Nov 6, and 500 so far on Monday Nov 9."
  (let ((plan (make-org-scribe-plan :title "Demo" :total-words 14000 :days 14
                                    :start-date "2026-11-01" :weekend-mode 'rest)))
    (org-scribe-planner--calculate-missing-variable plan)
    (setf (org-scribe-plan-daily-word-counts plan)
          (mapcar (lambda (e) (cons (car e) (list :words (cdr e) :note "")))
                  '(("2026-11-02" . 1400) ("2026-11-03" . 2900) ("2026-11-05" . 3800)
                    ("2026-11-06" . 5200) ("2026-11-09" . 5700))))
    (setf (org-scribe-plan-current-words plan) 5700)
    plan))

(defmacro test-views--with-current (plan &rest body)
  "Run BODY with PLAN as the active plan."
  (declare (indent 1))
  `(let ((org-scribe-planner--current-plan ,plan)
         (org-scribe-planner--current-plan-file "/tmp/unused-plan.org"))
     ,@body))

;;; Week start

(ert-deftest test-planner-views-week-start ()
  "Weeks start on Monday by default, or on the chosen day."
  (should (string= (org-scribe-planner--week-start "2026-11-05") "2026-11-02"))
  (should (string= (org-scribe-planner--week-start "2026-11-01") "2026-10-26"))
  (let ((org-scribe-planner-week-start-day 0))
    (should (string= (org-scribe-planner--week-start "2026-11-05") "2026-11-01"))
    (should (string= (org-scribe-planner--week-start "2026-11-01") "2026-11-01"))
    (should (equal (car (org-scribe-planner--week-day-names)) "Su"))))

(ert-deftest test-planner-views-manuscript-total-through ()
  "The manuscript total at the end of a day: the last entry on or before it."
  (let ((plan (test-views--plan)))
    (should (= (org-scribe-planner--manuscript-total-through plan "2026-11-01") 0))
    (should (= (org-scribe-planner--manuscript-total-through plan "2026-11-04") 2900))
    (should (= (org-scribe-planner--manuscript-total-through plan "2026-11-30") 5700))))

;;; Today under way

(ert-deftest test-planner-views-today-below-goal-is-not-due ()
  "Today below its goal is still under way, not missed."
  (test-views--on "2026-11-09"
    (let ((plan (test-views--plan)))
      (should (eq (org-scribe-planner--day-status plan "2026-11-09") 'future))
      ;; At its goal it counts
      (setf (org-scribe-plan-daily-word-counts plan)
            (cons (cons "2026-11-09" (list :words 6600 :note ""))
                  (assoc-delete-all "2026-11-09" (org-scribe-plan-daily-word-counts plan))))
      (should (eq (org-scribe-planner--day-status plan "2026-11-09") 'met)))))

;;; Weekly summary

(ert-deftest test-planner-views-weekly-summary ()
  "Calendar weeks from Monday; the current week is measured through yesterday."
  (test-views--on "2026-11-09"
    (let ((weeks (org-scribe-planner--weekly-summary (test-views--plan))))
      (should (= (length weeks) 3))
      ;; Week 1 is Sunday Nov 1 alone, a rest day
      (should (equal (list (plist-get (nth 0 weeks) :start) (plist-get (nth 0 weeks) :end))
                     '("2026-11-01" "2026-11-01")))
      (should (= (plist-get (nth 0 weeks) :rest-days) 1))
      (let ((week (nth 1 weeks)))
        (should (equal (list (plist-get week :start) (plist-get week :end))
                       '("2026-11-02" "2026-11-08")))
        (should (= (plist-get week :writing-days) 5))
        (should (= (plist-get week :days-written) 4))
        (should (= (plist-get week :goal) 7000))
        (should (= (plist-get week :written) 5200))
        (should (= (plist-get week :planned-total) 7000))
        (should (= (plist-get week :difference) -1800))
        ;; 74% of the goal: under the 75% a partial day needs, so missed.
        (should (string= (org-scribe-planner--format-week-written week) "5200 ✗ 74%")))
      (let ((week (nth 2 weeks)))
        (should (plist-get week :started))
        (should-not (plist-get week :ended))
        ;; Planned through yesterday, Sunday Nov 8: 7,000
        (should (= (plist-get week :planned-total) 7000))
        (should (= (plist-get week :written-total) 5700))
        (should (= (plist-get week :difference) -1300))
        ;; Under way: no mark yet
        (should (string= (org-scribe-planner--format-week-written week) "500 7%"))))))

(ert-deftest test-planner-views-weekly-summary-sunday-weeks ()
  "With weeks from Sunday, Nov 1-7 and Nov 8-14."
  (test-views--on "2026-11-09"
    (let* ((org-scribe-planner-week-start-day 0)
           (weeks (org-scribe-planner--weekly-summary (test-views--plan))))
      (should (= (length weeks) 2))
      (should (equal (mapcar (lambda (w) (plist-get w :start)) weeks)
                     '("2026-11-01" "2026-11-08"))))))

(ert-deftest test-planner-views-week-marks-match-day-marks ()
  "A finished week is ~ from 75% of its goal, ✗ below, as a day is."
  (should (string= (org-scribe-planner--format-week-written
                    '(:started t :ended t :written 800 :goal 1000))
                   "800 ~ 80%"))
  (should (string= (org-scribe-planner--format-week-written
                    '(:started t :ended t :written 560 :goal 1000))
                   "560 ✗ 56%")))

(ert-deftest test-planner-views-planned-total-after-a-rebase ()
  "A recalculation from progress shifts the planned total from today on;
a past day is measured against the plan as it stood.  With the adjustment
applied from the start, the first day of a plan read a negative total."
  (let ((plan (make-org-scribe-plan :title "T" :schedule-adjustment 5000)))
    (cl-letf (((symbol-function 'org-scribe-planner--get-today-date)
               (lambda () "2026-11-10")))
      (should (= 1120 (org-scribe-planner--planned-total
                       plan '(:date "2026-11-01" :words 1000 :cumulative -3880))))
      (should (= 6000 (org-scribe-planner--planned-total
                       plan '(:date "2026-11-10" :words 1000 :cumulative 6000)))))))

(defun test-views--column (rows header)
  "Return the cells under HEADER in the Org table ROWS (lists of cells)."
  (let ((index (cl-position header (car rows) :test #'string=)))
    (mapcar (lambda (row) (nth index row)) (cdr rows))))

(ert-deftest test-planner-views-schedule-table-splits-planned-and-actual ()
  "The plan file's schedule shows the planned and the actual totals apart.
One \"Cumulative\" column mixed them: it fell after a missed day and ended
short of the plan's total."
  (test-views--on "2026-11-09"
    (let ((plan (test-views--plan))
          (file (make-temp-file "test-views-plan-" nil ".org")))
      (unwind-protect
          (progn
            (org-scribe-planner--save-plan plan file)
            (with-current-buffer (find-file-noselect file)
              (goto-char (point-min))
              (re-search-forward "^\\*\\* Schedule")
              (re-search-forward "^|")
              (let* ((rows (seq-remove (lambda (row) (eq row 'hline))
                                       (org-table-to-lisp)))
                     (dates (test-views--column rows "Date"))
                     (planned (mapcar #'string-to-number
                                      (test-views--column rows "Planned total")))
                     (actual (test-views--column rows "Actual total")))
                (should (equal planned (sort (copy-sequence planned) #'<=)))
                (should (= 14000 (car (last planned))))
                (should (string= "" (nth (cl-position "2026-11-04" dates :test #'string=) actual)))
                (should (string= "3800" (nth (cl-position "2026-11-05" dates :test #'string=) actual))))
              (kill-buffer)))
        (delete-file file)))))

(ert-deftest test-planner-views-calendar-planned-total-never-falls ()
  "The calendar's planned total is the plan's, not a projection from the
last entry, so a missed day does not make it fall."
  (test-views--on "2026-11-09"
    (let ((plan (test-views--plan)))
      (org-scribe-planner-show-calendar plan)
      (with-current-buffer org-scribe-planner-calendar-buffer
        (let ((text (buffer-string))
              (totals nil))
          (should (string-match-p "Planned Total" text))
          (should-not (string-match-p "Expected Total" text))
          (with-temp-buffer
            (insert text)
            (goto-char (point-min))
            (while (re-search-forward
                    "^  [0-9-]+ ([A-Za-z ]+) +\\(?:REST\\|[0-9]+ words\\) +\\([0-9]+\\) words" nil t)
              (push (string-to-number (match-string 1)) totals)))
          (setq totals (nreverse totals))
          (should (= 14 (length totals)))
          (should (equal totals (sort (copy-sequence totals) #'<=)))
          (should (= 14000 (car (last totals)))))))))

(ert-deftest test-planner-views-weekly-summary-ended-week-ignores-rebase ()
  "A finished week keeps its planned total after a recalculation from
progress; only the current and future weeks follow the rebased plan."
  (test-views--on "2026-11-09"
    (let* ((plan (test-views--plan))
           (before (progn
                     ;; As before any real rebase: past days keep their goals.
                     (org-scribe-planner--lock-past-goals plan)
                     (org-scribe-planner--weekly-summary plan))))
      (setf (org-scribe-plan-schedule-adjustment plan) 3000)
      (let ((after (org-scribe-planner--weekly-summary plan)))
        ;; Week 2 (Nov 2-8) is over: unchanged by the adjustment.
        (should (= (plist-get (nth 1 before) :planned-total)
                   (plist-get (nth 1 after) :planned-total)))
        ;; Week 3 is under way: it follows the rebased plan (through
        ;; yesterday, all locked days, so shifted by the adjustment).
        (should (= (- (plist-get (nth 2 before) :planned-total) 3000)
                   (plist-get (nth 2 after) :planned-total)))))))

(ert-deftest test-planner-views-goal-free-week-has-no-mark ()
  "A week with no goal gets no ✓ ~ ✗."
  (should (string= (org-scribe-planner--format-week-written
                    '(:started t :ended t :written 0 :goal 0))
                   "0")))

(ert-deftest test-planner-views-weekly-summary-buffer ()
  "The weekly summary is an aligned Org table."
  (test-views--on "2026-11-09"
    (test-views--with-current (test-views--plan)
      (org-scribe-planner-show-weekly-summary)
      (with-current-buffer "*Writing Plan Weekly*"
        (let ((text (buffer-string)))
          (should (string-match-p "| Week | Dates +| Days written |" text))
          (should (string-match-p "| +2 | 11-02 – 11-08 | 4/5 +|" text))
          (should (string-match-p "-1800 |" text))
          ;; Aligned: a full separator rule, and every row the same width.
          (should (string-match-p "^|-+\\+-" text))
          (let ((widths (delete-dups
                         (mapcar #'string-width
                                 (seq-filter (lambda (line) (string-prefix-p "|" line))
                                             (split-string text "\n"))))))
            (should (= 1 (length widths)))))))))

;;; Month grid

(ert-deftest test-planner-views-month-grid ()
  "Each day shows its goal, its result and the share of the goal; today in brackets."
  (test-views--on "2026-11-09"
    (test-views--with-current (test-views--plan)
      (org-scribe-planner-show-month-calendar)
      (with-current-buffer "*Writing Plan Months*"
        (let ((text (buffer-string)))
          (should (string-match-p "^November 2026$" text))
          (should (string-match-p "^Mo +Tu +We +Th +Fr +Sa +Su$" text))
          (should (string-match-p "\\[9\\]" text))
          (should (string-match-p "1400w +1400w +1400w +1400w +1400w +rest +rest" text))
          ;; Nov 2 met, Nov 3 over, Nov 4 missed, Nov 5 under 75%, Nov 6 met
          (should (string-match-p "^✓ +✓1500 +✗ +✗900 +✓$" text))
          ;; Today's words so far
          (should (string-match-p "^500…$" text))
          (should (string-match-p "^10% +21% +27% +37%$" text)))))))

(ert-deftest test-planner-views-month-grid-sunday-first ()
  "With weeks from Sunday the grid's first column is Sunday."
  (test-views--on "2026-11-09"
    (let ((org-scribe-planner-week-start-day 0))
      (test-views--with-current (test-views--plan)
        (org-scribe-planner-show-month-calendar)
        (with-current-buffer "*Writing Plan Months*"
          (should (string-match-p "^Su +Mo +Tu" (buffer-string)))
          ;; Sunday Nov 1 opens the first week
          (should (string-match-p "^1 +2 +3 +4 +5 +6 +7$" (buffer-string))))))))

;;; Heatmap columns and calendar weeks

(ert-deftest test-planner-views-heatmap-columns ()
  "The heatmap puts each day under its own weekday.
Regression: the header started on Monday but the padding counted from
Sunday, so every day sat one column to the right."
  (test-views--on "2026-11-09"
    (test-views--with-current (test-views--plan)
      (org-scribe-planner-show-heatmap)
      (with-current-buffer "*Writing Heatmap*"
        (goto-char (point-min))
        (re-search-forward "^  Mo Tu We Th Fr Sa Su\n")
        ;; Sunday Nov 1 is the seventh column: 6 empty cells of 3 characters
        (should (looking-at (concat (make-string 18 ?\s) " · ")))))))

(ert-deftest test-planner-views-calendar-weeks-follow-week-start ()
  "The plan calendar's weeks start on the chosen day."
  (test-views--on "2026-11-09"
    (let ((org-scribe-planner-week-start-day 0)
          (plan (test-views--plan)))
      (org-scribe-planner-show-calendar plan)
      (with-current-buffer org-scribe-planner-calendar-buffer
        (let ((text (buffer-string)))
          ;; Week 1 begins on Sunday Nov 1, week 2 on Sunday Nov 8
          (should (string-match-p "Week 1:\n.*\n.*\n  2026-11-01" text))
          (should (string-match-p "Week 2:\n.*\n.*\n  2026-11-08" text)))))))

;;; Views select their window

(ert-deftest test-planner-views-open-in-the-selected-window ()
  "Every view selects its window, so its q (and other keys) work at once.
They used `display-buffer', which left focus in the manuscript."
  (cl-letf (((symbol-function 'org-scribe-planner--get-today-date) (lambda () "2026-11-09"))
            ((symbol-function 'org-scribe-planner--gnuplot-available-p) (lambda () nil)))
    (save-window-excursion
      (test-views--with-current (test-views--plan)
        (dolist (view '((org-scribe-planner-show-weekly-summary . "*Writing Plan Weekly*")
                        (org-scribe-planner-show-month-calendar . "*Writing Plan Months*")
                        (org-scribe-planner-show-milestones . nil)
                        (org-scribe-planner-show-progress-dashboard . "*Writing Dashboard*")
                        (org-scribe-planner-show-heatmap . "*Writing Heatmap*")))
          (delete-other-windows)
          (with-temp-buffer
            (switch-to-buffer (current-buffer))
            (let ((manuscript (current-buffer)))
              (funcall (car view))
              (should-not (eq (window-buffer (selected-window)) manuscript))
              (when (cdr view)
                (should (string= (cdr view)
                                 (buffer-name (window-buffer (selected-window)))))))))
        (delete-other-windows)
        (org-scribe-planner-show-calendar (test-views--plan))
        (should (eq (window-buffer (selected-window))
                    (get-buffer org-scribe-planner-calendar-buffer)))))))

(ert-deftest test-planner-views-split-dashboards-keep-their-grid ()
  "The split view still puts four different dashboards in its four windows."
  (cl-letf (((symbol-function 'org-scribe-planner--get-today-date) (lambda () "2026-11-09"))
            ((symbol-function 'org-scribe-planner--gnuplot-available-p) (lambda () nil))
            ((symbol-function 'message) #'ignore))
    (save-window-excursion
      (test-views--with-current (test-views--plan)
        (org-scribe-planner-show-split-dashboards)
        (let ((buffers (mapcar #'window-buffer (window-list))))
          (should (= 4 (length buffers)))
          (should (= 4 (length (delete-dups (copy-sequence buffers))))))))))

(provide 'test-planner-views)
;;; test-planner-views.el ends here
