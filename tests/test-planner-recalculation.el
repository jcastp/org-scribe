;;; test-planner-recalculation.el --- Tests for plan changes and status -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the commands that change a plan's length (the "What changed?"
;; recalculation, the new-plan wizard, onboarding, adjusting the end date),
;; for the on-track status, and for the mode-line string.  Most behaviours
;; here depend on today's date, so plans are built relative to it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-scribe-planner)
(require 'org-scribe-planner-dashboards)

;;; Helpers

(defun test-recalc--day (offset)
  "Return the date OFFSET days from today, as YYYY-MM-DD."
  (org-scribe-planner--add-days (org-scribe-planner--get-today-date) offset))

(defun test-recalc--answers (&rest values)
  "Return a function that returns VALUES one by one, ignoring its arguments."
  (lambda (&rest _) (pop values)))

(defun test-recalc--choose (&rest prefixes)
  "Return a `completing-read' stub answering with PREFIXES, one per call.
Each answer is the first candidate starting with its prefix, so tests
needn't spell out the previewed outcome that follows an option's label."
  (lambda (_prompt collection &rest _)
    (let ((prefix (pop prefixes)))
      (or (cl-find-if (lambda (c) (string-prefix-p prefix c))
                      (all-completions "" collection))
          prefix))))

(defmacro test-recalc--without-io (&rest body)
  "Run BODY with plan saving, calendar display and load hooks stubbed out."
  (declare (indent 0))
  `(let ((org-scribe-planner-after-plan-load-hook nil)
         (org-scribe-planner--current-plan nil)
         (org-scribe-planner--current-plan-file nil))
     (cl-letf (((symbol-function 'org-scribe-planner--save-plan) #'ignore)
               ((symbol-function 'org-scribe-planner-show-calendar) #'ignore))
       ,@body)))

;;; "What changed?" keeps END_DATE in step with DAYS

(ert-deftest test-planner-recalculate-daily-words-moves-end-date ()
  "A new daily target recomputes DAYS and the end date together.
Regression: DAYS went from 10 to 20 but END_DATE stayed on Nov 10, so the
schedule still had 10 days."
  (test-recalc--without-io
    (let ((plan (make-org-scribe-plan :title "P" :total-words 10000 :daily-words 1000
                                      :days 10 :start-date "2026-11-01"
                                      :end-date "2026-11-10")))
      (setq org-scribe-planner--current-plan plan
            org-scribe-planner--current-plan-file "/tmp/unused-plan.org")
      (cl-letf (((symbol-function 'completing-read)
                 (test-recalc--choose "Daily words" "Total (the end date moves)"))
                ((symbol-function 'org-scribe-planner--read-positive-number)
                 (lambda (&rest _) 500)))
        (org-scribe-planner-recalculate))
      (should (= (org-scribe-plan-days plan) 20))
      (should (string= (org-scribe-plan-end-date plan) "2026-11-20"))
      (should (= (length (org-scribe-planner--generate-day-schedule plan)) 20)))))

(ert-deftest test-planner-recalculate-days-number-moves-end-date ()
  "A new number of days (typed as a number) moves the end date too."
  (test-recalc--without-io
    (let ((plan (make-org-scribe-plan :title "P" :total-words 10000 :daily-words 1000
                                      :days 10 :start-date "2026-11-01"
                                      :end-date "2026-11-10")))
      (setq org-scribe-planner--current-plan plan
            org-scribe-planner--current-plan-file "/tmp/unused-plan.org")
      (cl-letf (((symbol-function 'completing-read)
                 (test-recalc--choose "Days available" "Total (the daily target changes)"))
                ((symbol-function 'org-scribe-planner--read-days)
                 (lambda (&rest _) 20)))
        (org-scribe-planner-recalculate))
      (should (string= (org-scribe-plan-end-date plan) "2026-11-20"))
      (should (= (org-scribe-plan-daily-words plan) 500)))))

;;; New-plan wizard

(ert-deftest test-planner-new-plan-spare-days-move-calculated-end-date ()
  "Total + daily, then spare days: the end date covers the longer plan.
Regression: DAYS grew to include the spare days but END_DATE kept the
date computed before they were added."
  (test-recalc--without-io
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Total words + Daily words → Calculate days needed"))
              ((symbol-function 'org-scribe-planner--read-positive-number)
               (test-recalc--answers 8000 1000))
              ((symbol-function 'org-scribe-planner--read-non-negative-number)
               (lambda (&rest _) 0))
              ((symbol-function 'org-scribe-planner--read-date)
               (lambda (&rest _) "2026-11-01"))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'org-scribe-planner--configure-spare-days)
               (lambda (plan)
                 (setf (org-scribe-plan-spare-days plan) '("2026-11-03" "2026-11-04")))))
      (org-scribe-planner-new-plan "P" "/tmp/unused-plan.org")
      (let ((plan org-scribe-planner--current-plan))
        (should (= (org-scribe-plan-days plan) 10))
        (should (string= (org-scribe-plan-end-date plan) "2026-11-10"))))))

;;; Onboarding

(ert-deftest test-planner-onboard-daily-pace-ignores-past-spare-days ()
  "Onboarding by daily words: only spare days from today on push the end.
Regressions: choosing \"Daily words → calculate end date\" asked for an
end date instead (the choice was matched case-insensitively against
\"End date\"); every spare day lengthened the plan, including ones before
today; and DAYS was measured from today while the plan started earlier."
  (test-recalc--without-io
    (let ((org-scribe-planner-wordcount-function nil)
          (start (test-recalc--day -30)))
      (cl-letf (((symbol-function 'org-scribe-planner--read-positive-number)
                 ;; current manuscript, target, words per day
                 (test-recalc--answers 20000 30000 1000))
                ((symbol-function 'read-string) (lambda (&rest _) "P"))
                ((symbol-function 'org-scribe-planner--read-date)
                 (lambda (&rest _) start))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "Daily words → calculate end date"))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'org-scribe-planner--configure-spare-days)
                 (lambda (plan)
                   (setf (org-scribe-plan-spare-days plan)
                         (list (test-recalc--day -5) (test-recalc--day 1)))))
                ((symbol-function 'read-file-name)
                 (lambda (&rest _) "/tmp/unused-plan.org"))
                ((symbol-function 'make-directory) #'ignore))
        (org-scribe-planner-onboard-existing-project))
      (let ((plan org-scribe-planner--current-plan))
        ;; 10,000 words left at 1,000/day: today + 9 more working days,
        ;; one of them pushed out by tomorrow's spare day
        (should (string= (org-scribe-plan-end-date plan) (test-recalc--day 10)))
        (should (= (org-scribe-plan-days plan)
                   (org-scribe-planner--days-between start (test-recalc--day 10))))
        (should (= (org-scribe-plan-daily-words plan) 1000))))))

;;; Adjust the end date (keep the daily target)

(defun test-recalc--running-plan (&rest args)
  "A plan started 10 days ago at 1,000/day for 30,000 words, ending in 20 days.
ARGS are extra slots for `make-org-scribe-plan', overriding the defaults."
  (apply #'make-org-scribe-plan
         (append args
                 (list :title "P" :total-words 30000 :daily-words 1000
                       :start-date (test-recalc--day -10)
                       :end-date (test-recalc--day 19) :days 30))))

(ert-deftest test-planner-adjust-end-date-finishes-early-when-ahead ()
  "Ahead: the end date comes forward and the daily target stays."
  (test-recalc--without-io
    (let ((plan (test-recalc--running-plan
                 :current-words 25000
                 :daily-word-counts (list (cons (test-recalc--day -1)
                                                (list :words 25000 :note ""))))))
      (org-scribe-planner-recalculate-end-date plan "/tmp/unused-plan.org")
      ;; 5,000 words left at 1,000/day: today and the next 4 days
      (should (string= (org-scribe-plan-end-date plan) (test-recalc--day 4)))
      (should (= (org-scribe-plan-days plan) 15))
      (should (= (org-scribe-plan-daily-words plan) 1000))
      (should (= (org-scribe-plan-total-words plan) 30000)))))

(ert-deftest test-planner-adjust-end-date-extends-when-behind ()
  "Behind: the end date moves out."
  (test-recalc--without-io
    (let ((plan (test-recalc--running-plan
                 :current-words 5000
                 :daily-word-counts (list (cons (test-recalc--day -1)
                                                (list :words 5000 :note ""))))))
      (org-scribe-planner-recalculate-end-date plan "/tmp/unused-plan.org")
      ;; 25,000 words left: 25 working days from today
      (should (string= (org-scribe-plan-end-date plan) (test-recalc--day 24))))))

(ert-deftest test-planner-adjust-end-date-starts-tomorrow-when-today-logged ()
  "Words already logged today: the remaining words start tomorrow."
  (test-recalc--without-io
    (let ((plan (test-recalc--running-plan
                 :current-words 25000
                 :daily-word-counts (list (cons (test-recalc--day 0)
                                                (list :words 25000 :note ""))))))
      (org-scribe-planner-recalculate-end-date plan "/tmp/unused-plan.org")
      (should (string= (org-scribe-plan-end-date plan) (test-recalc--day 5))))))

(ert-deftest test-planner-adjust-end-date-skips-spare-days ()
  "Spare days between today and the new end date push it out."
  (test-recalc--without-io
    (let ((plan (test-recalc--running-plan
                 :current-words 25000
                 :spare-days (list (test-recalc--day 1) (test-recalc--day 2)))))
      (org-scribe-planner-recalculate-end-date plan "/tmp/unused-plan.org")
      (should (string= (org-scribe-plan-end-date plan) (test-recalc--day 6))))))

(ert-deftest test-planner-adjust-end-date-never-before-start ()
  "A plan that hasn't started yet counts from its start date."
  (test-recalc--without-io
    (let ((plan (make-org-scribe-plan :title "P" :total-words 3000 :daily-words 1000
                                      :start-date (test-recalc--day 5)
                                      :end-date (test-recalc--day 14) :days 10
                                      :current-words 0)))
      (org-scribe-planner-recalculate-end-date plan "/tmp/unused-plan.org")
      (should (string= (org-scribe-plan-end-date plan) (test-recalc--day 7)))
      (should (= (org-scribe-plan-days plan) 3)))))

(ert-deftest test-planner-adjust-end-date-goal-met-changes-nothing ()
  "With the goal reached, the plan is left alone."
  (test-recalc--without-io
    (let ((plan (test-recalc--running-plan :current-words 30500))
          (saved nil))
      (cl-letf (((symbol-function 'org-scribe-planner--save-plan)
                 (lambda (&rest _) (setq saved t))))
        (org-scribe-planner-recalculate-end-date plan "/tmp/unused-plan.org"))
      (should (string= (org-scribe-plan-end-date plan) (test-recalc--day 19)))
      (should-not saved))))

;;; On-track status: today isn't expected until it's over

(ert-deftest test-planner-schedule-position-expects-goals-through-yesterday ()
  "Expected words stop at yesterday; today counts as a remaining day.
Regression: today's goal was already expected, so every morning showed
the plan a full day behind."
  (let* ((plan (make-org-scribe-plan :title "P" :total-words 10000 :daily-words 1000
                                     :start-date (test-recalc--day -2)
                                     :end-date (test-recalc--day 7) :days 10
                                     :current-words 2000))
         (position (org-scribe-planner--calculate-schedule-position plan)))
    (should (= (plist-get position :expected-by-today) 2000))
    (should (= (plist-get position :words-ahead) 0))
    (should (eq (plist-get position :status) 'ahead))
    (should (= (plist-get position :days-elapsed) 2))
    ;; Today and the 7 days after it
    (should (= (plist-get position :days-remaining) 8))))

(ert-deftest test-planner-progress-report-expects-goals-through-yesterday ()
  "The progress message isn't behind just because today has started."
  (let ((plan (make-org-scribe-plan :title "P" :total-words 10000 :daily-words 1000
                                    :start-date (test-recalc--day -2)
                                    :end-date (test-recalc--day 7) :days 10
                                    :current-words 2000))
        (shown nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
      (org-scribe-planner--show-progress-report plan))
    ;; Exactly the words expected through yesterday: on track
    (should (string-match-p "| On track" shown))))

;;; Mode line

(defmacro test-recalc--with-mode-line-plan (plan &rest body)
  "Run BODY with PLAN as the current plan and the project gate open."
  (declare (indent 1))
  `(let ((org-scribe-planner--current-plan ,plan))
     (cl-letf (((symbol-function 'org-scribe-planner-gate) (lambda (&rest _) 'yes)))
       ,@body)))

(defun test-recalc--today-ledger ()
  "Ledger with 1,000 words yesterday and 300 more today."
  (list (cons (test-recalc--day -1) (list :words 1000 :note ""))
        (cons (test-recalc--day 0) (list :words 1300 :note ""))))

(ert-deftest test-planner-mode-line-shows-todays-goal ()
  "On a writing day the mode line shows today's words against today's goal."
  (test-recalc--with-mode-line-plan
      (make-org-scribe-plan :total-words 10000 :daily-words 1000
                            :start-date (test-recalc--day -2)
                            :end-date (test-recalc--day 7) :days 10
                            :daily-word-counts (test-recalc--today-ledger))
    (should (string= (org-scribe-planner--mode-line) " [W:300/1000]"))))

(ert-deftest test-planner-mode-line-spare-day-is-rest ()
  "On a spare day there is no target to compare with.
Regression: the mode line showed the plan's daily words as today's target."
  (test-recalc--with-mode-line-plan
      (make-org-scribe-plan :total-words 10000 :daily-words 1000
                            :start-date (test-recalc--day -2)
                            :end-date (test-recalc--day 7) :days 10
                            :spare-days (list (test-recalc--day 0))
                            :daily-word-counts (test-recalc--today-ledger))
    (should (string= (org-scribe-planner--mode-line) " [W:300 rest]"))))

(ert-deftest test-planner-mode-line-outside-plan-shows-words-only ()
  "After the plan's last day, only today's words are shown."
  (test-recalc--with-mode-line-plan
      (make-org-scribe-plan :total-words 10000 :daily-words 1000
                            :start-date (test-recalc--day -10)
                            :end-date (test-recalc--day -1) :days 10
                            :daily-word-counts (test-recalc--today-ledger))
    (should (string= (org-scribe-planner--mode-line) " [W:300]"))))

(provide 'test-planner-recalculation)
;;; test-planner-recalculation.el ends here
