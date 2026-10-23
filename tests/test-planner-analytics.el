;;; test-planner-analytics.el --- Tests for analytics on the schedule -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the analytics that follow each day's own goal: the elapsed
;; writing days (missed days count, rest days don't), the average pace,
;; the shared day status (met, partial at 75%, missed), the completion
;; estimate that skips rest days, and the dashboards built on them.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-scribe-planner)
(require 'org-scribe-planner-dashboards)

;;; Helpers

(defvar test-analytics--today "2026-11-09"
  "The date `org-scribe-planner--get-today-date' returns in these tests.")

(defmacro test-analytics--on (today &rest body)
  "Run BODY with today's date stubbed to TODAY."
  (declare (indent 1))
  `(let ((test-analytics--today ,today))
     (cl-letf (((symbol-function 'org-scribe-planner--get-today-date)
                (lambda () test-analytics--today)))
       ,@body)))

(defun test-analytics--plan (ledger &rest args)
  "14,000 words over 14 days from Sunday Nov 1, 2026, with ARGS.
LEDGER is a list of (DATE . MANUSCRIPT-TOTAL)."
  (let ((plan (apply #'make-org-scribe-plan
                     (append args (list :title "P" :total-words 14000 :days 14
                                        :start-date "2026-11-01")))))
    (org-scribe-planner--calculate-missing-variable plan)
    (setf (org-scribe-plan-daily-word-counts plan)
          (mapcar (lambda (entry) (cons (car entry) (list :words (cdr entry) :note "")))
                  ledger))
    (setf (org-scribe-plan-current-words plan) (if ledger (cdr (car (last ledger))) 0))
    plan))

(defun test-analytics--week ()
  "Nov 1-7: 1,000 words a day except nothing on Nov 3 and 700 on Nov 5."
  (test-analytics--plan '(("2026-11-01" . 1000) ("2026-11-02" . 2000)
                          ("2026-11-04" . 3000) ("2026-11-05" . 3700)
                          ("2026-11-06" . 4700) ("2026-11-07" . 5700))))

;;; Elapsed writing days and pace

(ert-deftest test-planner-analytics-elapsed-days-include-missed ()
  "Missed writing days count, with 0 words."
  (test-analytics--on "2026-11-08"
    (let ((days (org-scribe-planner--elapsed-daily-words (test-analytics--week))))
      (should (= (length days) 7))
      (should (equal (assoc "2026-11-03" days) '("2026-11-03" . 0))))))

(ert-deftest test-planner-analytics-elapsed-days-skip-rest-days ()
  "Rest days don't count, unless you wrote on them; today counts once written."
  (test-analytics--on "2026-11-09"
    (let* ((plan (test-analytics--plan '(("2026-11-02" . 1400) ("2026-11-07" . 1800)
                                         ("2026-11-09" . 2300))
                                       :weekend-mode 'rest))
           (dates (mapcar #'car (org-scribe-planner--elapsed-daily-words plan))))
      ;; Weekdays Nov 2-6, Saturday Nov 7 (written on), and today
      (should (equal dates '("2026-11-02" "2026-11-03" "2026-11-04" "2026-11-05"
                             "2026-11-06" "2026-11-07" "2026-11-09"))))))

(ert-deftest test-planner-analytics-average-pace-counts-missed-days ()
  "5,700 words over 7 writing days, one of them missed: about 814 a day.
Regression: the average divided by the days logged only (6), giving 950."
  (test-analytics--on "2026-11-08"
    (should (< (abs (- (org-scribe-planner--average-pace (test-analytics--week))
                       (/ 5700.0 7)))
               1e-9))))

;;; Day status

(ert-deftest test-planner-analytics-day-status ()
  "Met, partial at 75% or more, missed below or with nothing; future and rest."
  (test-analytics--on "2026-11-08"
    (let ((plan (test-analytics--week)))
      (should (eq (org-scribe-planner--day-status plan "2026-11-01") 'met))
      (should (eq (org-scribe-planner--day-status plan "2026-11-03") 'missed))
      (should (eq (org-scribe-planner--day-status plan "2026-11-05") 'missed))
      (should (eq (org-scribe-planner--day-status plan "2026-11-08") 'future))
      (should (eq (org-scribe-planner--day-status plan "2026-11-10") 'future))
      (should (eq (org-scribe-planner--day-status plan "2026-12-01") 'no-goal)))))

(ert-deftest test-planner-analytics-day-status-partial-and-exceeded ()
  "750 of 1,000 is partial; 1,200 exceeded; a cut is missed."
  (test-analytics--on "2026-11-05"
    (let ((plan (test-analytics--plan '(("2026-11-01" . 750) ("2026-11-02" . 1950)
                                        ("2026-11-03" . 1800)))))
      (should (eq (org-scribe-planner--day-status plan "2026-11-01") 'partial))
      (should (eq (org-scribe-planner--day-status plan "2026-11-02") 'exceeded))
      (should (eq (org-scribe-planner--day-status plan "2026-11-03") 'missed)))))

(ert-deftest test-planner-analytics-day-status-rest-days ()
  "A rest day is rest, or exceeded when written on."
  (test-analytics--on "2026-11-09"
    (let ((plan (test-analytics--plan '(("2026-11-07" . 300)) :weekend-mode 'rest)))
      (should (eq (org-scribe-planner--day-status plan "2026-11-01") 'rest))
      (should (eq (org-scribe-planner--day-status plan "2026-11-07") 'exceeded)))))

;;; Completion estimate

(ert-deftest test-planner-analytics-completion-skips-rest-days ()
  "Weekends off: 2,800 words at 1,400 a day take two weekdays."
  (test-analytics--on "2026-11-12"
    (let ((plan (test-analytics--plan '(("2026-11-11" . 11200)) :weekend-mode 'rest)))
      ;; Thursday Nov 12 and Friday Nov 13
      (should (string= (org-scribe-planner--completion-date plan 1400) "2026-11-13"))
      ;; At 1,000 a day: Thu, Fri, then the weekend is skipped: Mon Nov 16
      (should (string= (org-scribe-planner--completion-date plan 1000) "2026-11-16")))))

(ert-deftest test-planner-analytics-completion-after-end-skips-weekends-off ()
  "Past the plan's end, weekends off are still skipped."
  (test-analytics--on "2026-11-12"
    (let ((plan (test-analytics--plan '(("2026-11-11" . 4000)) :weekend-mode 'rest)))
      ;; 10,000 left at 1,000 a day: 10 weekdays from Thu Nov 12 → Wed Nov 25
      (should (string= (org-scribe-planner--completion-date plan 1000) "2026-11-25")))))

(ert-deftest test-planner-analytics-completion-edge-cases ()
  "No pace: no date.  Goal reached: today."
  (test-analytics--on "2026-11-08"
    (should-not (org-scribe-planner--completion-date (test-analytics--week) 0))
    (should (string= (org-scribe-planner--completion-date
                      (test-analytics--plan '(("2026-11-07" . 14500))) 0)
                     "2026-11-08"))))

;;; Dashboard calculations

(ert-deftest test-planner-analytics-velocity ()
  "Average and recent pace over writing days, and both estimates."
  (test-analytics--on "2026-11-08"
    (let ((velocity (org-scribe-planner--calculate-velocity (test-analytics--week))))
      (should (< (abs (- (plist-get velocity :average) (/ 5700.0 7))) 1e-9))
      (should (< (abs (- (plist-get velocity :recent) (/ 5700.0 7))) 1e-9))
      (should (= (plist-get velocity :days-logged) 6))
      (should (= (plist-get velocity :elapsed-days) 7))
      (should (stringp (plist-get velocity :eta))))))

(ert-deftest test-planner-analytics-consistency-and-achievement ()
  "Over the 7 days due: written on 6; met 5, missed 2 (nothing; 700)."
  (test-analytics--on "2026-11-08"
    (let* ((plan (test-analytics--week))
           (consistency (org-scribe-planner--calculate-consistency-score plan))
           (achievement (org-scribe-planner--calculate-target-achievement-rate plan)))
      (should (= (plist-get consistency :working-days) 7))
      (should (= (plist-get consistency :days-logged) 6))
      (should (= (plist-get achievement :total-days) 7))
      (should (= (plist-get achievement :days-met) 5))
      (should (= (plist-get achievement :days-partial) 0))
      (should (= (plist-get achievement :days-missed) 2)))))

(ert-deftest test-planner-analytics-day-of-week-counts-missed-days ()
  "A missed Tuesday lowers Tuesday's average to 0."
  (test-analytics--on "2026-11-08"
    (let ((tuesday (cdr (assq 2 (org-scribe-planner--calculate-day-of-week-stats
                                 (test-analytics--week))))))
      (should (= (plist-get tuesday :count) 1))
      (should (= (plist-get tuesday :average) 0.0)))))

;;; Dashboards render

(ert-deftest test-planner-analytics-dashboards-render ()
  "Every dashboard renders for a plan with a pattern, weekends off and a missed day."
  (test-analytics--on "2026-11-12"
    (let* ((plan (test-analytics--plan '(("2026-11-02" . 1200) ("2026-11-03" . 2600)
                                         ("2026-11-05" . 3900) ("2026-11-06" . 5000)
                                         ("2026-11-09" . 6100) ("2026-11-10" . 7000))
                                       :weekend-mode 'rest :pattern 'rising))
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file "/tmp/unused-plan.org"))
      (cl-letf (((symbol-function 'display-buffer) #'ignore)
                ((symbol-function 'org-scribe-planner--gnuplot-available-p) (lambda () nil)))
        (dolist (command '(org-scribe-planner-show-progress-dashboard
                           org-scribe-planner-show-velocity
                           org-scribe-planner-show-velocity-chart
                           org-scribe-planner-show-velocity-trends
                           org-scribe-planner-show-performance-analytics
                           org-scribe-planner-show-multi-metric-dashboard
                           org-scribe-planner-show-heatmap
                           org-scribe-planner-show-burndown-ascii
                           org-scribe-planner-show-milestones))
          (should (equal (list command (condition-case err (progn (funcall command) 'ok)
                                         (error err)))
                         (list command 'ok)))))
      (with-current-buffer "*Writing Heatmap*"
        (should (string-match-p "Still to come" (buffer-string)))
        (should (string-match-p "Rest day" (buffer-string))))
      (with-current-buffer "*Velocity Statistics*"
        (should (string-match-p "Days written: +6 of 8 writing days" (buffer-string)))))))

(ert-deftest test-planner-analytics-burndown-is-drawn-the-right-way-up ()
  "Words remaining fall from the top-left to the bottom-right.
The scale and the renderer both inverted the rows, so the ideal line rose
instead of falling, and a value of 0 landed outside the canvas."
  (should (= 9 (org-scribe-planner--scale-to-height 100 100 10)))
  (should (= 0 (org-scribe-planner--scale-to-height 0 100 10)))
  (should (= 0 (org-scribe-planner--scale-to-height -50 100 10)))
  (let ((rows (split-string
               (with-temp-buffer
                 (org-scribe-planner--draw-ascii-burndown
                  (list (list :ideal 100 :actual nil)
                        (list :ideal 50 :actual nil)
                        (list :ideal 0 :actual nil))
                  3 3 100)
                 (buffer-string))
               "\n" t)))
    (should (equal "│-  │" (nth 0 rows)))
    (should (equal "│ - │" (nth 1 rows)))
    (should (equal "│  -│" (nth 2 rows)))))

(ert-deftest test-planner-analytics-velocity-trends-not-on-track-when-behind ()
  "Velocity trends only says \"On track\" when the recent pace covers the
required one.  A required pace merely within reach of the plan's used to
be shown as \"On track to complete on time!\" beside a late projection."
  (test-analytics--on "2026-11-08"
    (let* ((plan (test-analytics--week))
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file "/tmp/unused-plan.org"))
      (cl-letf (((symbol-function 'display-buffer) #'ignore))
        (org-scribe-planner-show-velocity-trends))
      (with-current-buffer "*Velocity Trend Analysis*"
        (should-not (string-match-p "On track to complete on time" (buffer-string)))
        (should (string-match-p "Within reach, but above your recent pace" (buffer-string)))))))

(ert-deftest test-planner-analytics-cumulative-chart-messages ()
  "A chart that was generated is shown without a \"Failed\" message, and a
failure says so.  A misplaced parenthesis echoed \"Failed to generate\"
after every success, and a real failure said gnuplot was not available."
  (test-analytics--on "2026-11-08"
    (let* ((plan (test-analytics--week))
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file "/tmp/unused-plan.org")
           (messages nil))
      (cl-letf (((symbol-function 'display-buffer) #'ignore)
                ((symbol-function 'insert-image) #'ignore)
                ((symbol-function 'org-scribe-planner--gnuplot-available-p) (lambda () t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
        (cl-letf (((symbol-function 'org-scribe-planner--generate-gnuplot-cumulative)
                   (lambda (&rest _) t)))
          (org-scribe-planner-show-cumulative-progress))
        (should-not (cl-some (lambda (m) (string-match-p "Failed" m)) messages))
        (should (get-buffer "*Cumulative Progress*"))
        (setq messages nil)
        (cl-letf (((symbol-function 'org-scribe-planner--generate-gnuplot-cumulative)
                   (lambda (&rest _) nil)))
          (org-scribe-planner-show-cumulative-progress))
        (should (member "Failed to generate cumulative progress chart" messages))))))

(ert-deftest test-planner-analytics-multi-metric-no-projection-on-day-one ()
  "Only today's partial entry exists: no BEHIND SCHEDULE projection is shown."
  (test-analytics--on "2026-11-01"
    (let* ((plan (test-analytics--plan '(("2026-11-01" . 47))))
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file "/tmp/unused-plan.org"))
      (cl-letf (((symbol-function 'display-buffer) #'ignore))
        (org-scribe-planner-show-multi-metric-dashboard))
      (with-current-buffer "*Multi-Metric Dashboard*"
        (should-not (string-match-p "BEHIND SCHEDULE" (buffer-string)))
        (should (string-match-p "first completed writing day" (buffer-string)))))))

(ert-deftest test-planner-analytics-count-text-pluralizes ()
  "One day, two days, zero days."
  (should (equal "1 day" (org-scribe-planner--count-text 1 "day" "days")))
  (should (equal "0 days" (org-scribe-planner--count-text 0 "day" "days")))
  (should (equal "1 session" (org-scribe-planner--count-text 1 "session" "sessions"))))

(provide 'test-planner-analytics)
;;; test-planner-analytics.el ends here
