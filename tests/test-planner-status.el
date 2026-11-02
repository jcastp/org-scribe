;;; test-planner-status.el --- Tests for plan status and recalculation previews -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for where a plan stands (`org-scribe-planner--status'): ahead, on
;; track or behind, in words, % and writing days measured along the
;; schedule; for the previews of the two recalculations from progress
;; (`a', `e'); and for `org-scribe-planner-check-progress', which offers
;; the right pair of adjustments when the plan is ahead or behind.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-scribe-planner)
(require 'org-scribe-planner-dashboards)

;;; Helpers

(defvar test-status--today "2026-11-11"
  "The date `org-scribe-planner--get-today-date' returns in these tests.")

(defmacro test-status--on (today &rest body)
  "Run BODY with today stubbed to TODAY, and saving and display stubbed."
  (declare (indent 1))
  `(let ((test-status--today ,today))
     (cl-letf (((symbol-function 'org-scribe-planner--get-today-date)
                (lambda () test-status--today))
               ((symbol-function 'org-scribe-planner--save-plan) #'ignore)
               ((symbol-function 'org-scribe-planner--refresh-agenda-if-synced) #'ignore)
               ((symbol-function 'org-scribe-planner-show-calendar) #'ignore))
       ,@body)))

(defun test-status--plan (written &optional date)
  "30,000 words over 30 days from Nov 1, 2026, with WRITTEN logged on DATE.
DATE defaults to Nov 10."
  (let ((plan (make-org-scribe-plan :title "P" :total-words 30000 :days 30
                                    :start-date "2026-11-01")))
    (org-scribe-planner--calculate-missing-variable plan)
    (setf (org-scribe-plan-daily-word-counts plan)
          (list (cons (or date "2026-11-10") (list :words written :note ""))))
    (setf (org-scribe-plan-current-words plan) written)
    plan))

(defmacro test-status--with-current (plan &rest body)
  "Run BODY with PLAN as the active plan."
  (declare (indent 1))
  `(let ((org-scribe-planner--current-plan ,plan)
         (org-scribe-planner--current-plan-file "/tmp/unused-plan.org"))
     ,@body))

;;; Status

(ert-deftest test-planner-status-on-track-is-exact ()
  "Exactly the words expected by the end of today: on track, not ahead."
  (test-status--on "2026-11-10"
    (let ((status (org-scribe-planner--status (test-status--plan 10000))))
      (should (eq (plist-get status :status) 'on-track))
      (should (= (plist-get status :difference) 0))
      (should (= (plist-get status :days) 0))
      (should (string= (org-scribe-planner--describe-status status) "On track")))))

(ert-deftest test-planner-status-ahead-in-words-percent-and-days ()
  "2,000 words ahead of 10,000 expected: +20%, two writing days."
  (test-status--on "2026-11-10"
    (let ((status (org-scribe-planner--status (test-status--plan 12000))))
      (should (eq (plist-get status :status) 'ahead))
      (should (= (plist-get status :difference) 2000))
      (should (= (plist-get status :percent) 20.0))
      (should (= (plist-get status :days) 2.0))
      (should (string= (org-scribe-planner--describe-status status)
                       "Ahead: +2000 words (+20.0%, +2.0 writing days)")))))

(ert-deftest test-planner-status-behind ()
  "500 words short: behind by 5% and half a writing day."
  (test-status--on "2026-11-10"
    (let ((status (org-scribe-planner--status (test-status--plan 9500))))
      (should (eq (plist-get status :status) 'behind))
      (should (= (plist-get status :percent) -5.0))
      (should (= (plist-get status :days) -0.5))
      (should (string= (org-scribe-planner--describe-status status)
                       "Behind: -500 words (-5.0%, -0.5 writing days)")))))

(ert-deftest test-planner-status-days-follow-each-days-goal ()
  "Days behind are measured along the schedule, day by day.
After a recalculation the past days keep 1,000 and the open days get
1,250; dividing the shortfall by the daily target would say 2.2 days."
  (test-status--on "2026-11-11"
    (let ((plan (test-status--plan 5000)))
      (org-scribe-planner-recalculate-remaining-days plan "/tmp/unused-plan.org")
      (should (= (org-scribe-plan-daily-words plan) 1250))
      ;; A day later with only 4,750 words (a cut of 250)
      (setq test-status--today "2026-11-12")
      (setf (org-scribe-plan-daily-word-counts plan)
            (list (cons "2026-11-11" (list :words 4750 :note ""))))
      (setf (org-scribe-plan-current-words plan) 4750)
      (let ((status (org-scribe-planner--status plan)))
        ;; Due by the end of Nov 12: 5,000 + 2 × 1,250
        (should (= (plist-get status :difference) -2750))
        (should (= (plist-get status :days) -2.25))))))

(ert-deftest test-planner-status-before-plan-starts ()
  "Before the plan starts nothing is expected: on track, 0%."
  (test-status--on "2026-10-20"
    (let ((status (org-scribe-planner--status (test-status--plan 0 "2026-10-19"))))
      (should (eq (plist-get status :status) 'on-track))
      (should (= (plist-get status :percent) 0.0)))))

(ert-deftest test-planner-status-nil-without-schedule ()
  "A plan with no dates yet has no status."
  (should-not (org-scribe-planner--status (make-org-scribe-plan :total-words 1000))))

(ert-deftest test-planner-status-schedule-position-uses-schedule-days ()
  "The dashboards' position reports schedule days and the exact state."
  (test-status--on "2026-11-10"
    (let ((on-track (org-scribe-planner--calculate-schedule-position (test-status--plan 10000)))
          (ahead (org-scribe-planner--calculate-schedule-position (test-status--plan 12000))))
      (should (plist-get on-track :on-track))
      (should (string-prefix-p "ON TRACK" (org-scribe-planner--status-text on-track)))
      (should-not (plist-get ahead :on-track))
      (should (= (plist-get ahead :days-ahead) 2.0))
      (should (string-prefix-p "AHEAD" (org-scribe-planner--status-text ahead))))))

;;; Previews of the two recalculations

(ert-deftest test-planner-status-adjust-remaining-previews-and-applies ()
  "`a' shows the new daily target first and applies it on yes."
  (test-status--on "2026-11-11"
    (let ((plan (test-status--plan 5000))
          (asked nil))
      (test-status--with-current plan
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (prompt) (setq asked prompt) t))
                  ((symbol-function 'message) #'ignore))
          (org-scribe-planner-adjust-remaining-plan)))
      (should (string-match-p "From 2026-11-11: 25000 words left over 20 writing days" asked))
      (should (string-match-p "Daily target 1000 → 1250 words (\\+250, \\+25.0%)" asked))
      (should (= (org-scribe-plan-daily-words plan) 1250)))))

(ert-deftest test-planner-status-adjust-remaining-declined-changes-nothing ()
  "`a' answered no: the plan, past days included, is left as it was."
  (test-status--on "2026-11-11"
    (let ((plan (test-status--plan 5000)))
      (test-status--with-current plan
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil))
                  ((symbol-function 'message) #'ignore))
          (org-scribe-planner-adjust-remaining-plan)))
      (should (= (org-scribe-plan-daily-words plan) 1000))
      (should (= (org-scribe-plan-schedule-adjustment plan) 0)))))

(ert-deftest test-planner-status-adjust-end-date-previews ()
  "`e' shows the new end date first."
  (test-status--on "2026-11-11"
    (let ((plan (test-status--plan 5000))
          (asked nil))
      (test-status--with-current plan
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (prompt) (setq asked prompt) nil))
                  ((symbol-function 'message) #'ignore))
          (org-scribe-planner-adjust-end-date)))
      (should (string-match-p "Writing days 30 → 35" asked))
      (should (string-match-p "End date 2026-11-30 → 2026-12-05: extended by 5 days" asked))
      ;; Declined: unchanged
      (should (string= (org-scribe-plan-end-date plan) "2026-11-30")))))

;;; Check progress

(ert-deftest test-planner-status-check-on-track-asks-nothing ()
  "On track: a message, no question."
  (test-status--on "2026-11-11"
    (let ((shown nil))
      (test-status--with-current (test-status--plan 10000)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) (error "Nothing should be asked")))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
          (org-scribe-planner-check-progress)))
      (should (string-prefix-p "On track" shown)))))

(ert-deftest test-planner-status-check-behind-offers-and-applies ()
  "Behind: keep, increase the daily target or extend the deadline, previewed."
  (test-status--on "2026-11-11"
    (let ((plan (test-status--plan 5000))
          (offered nil)
          (prompt-seen nil))
      (test-status--with-current plan
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt collection &optional predicate &rest _)
                     (setq prompt-seen prompt
                           offered (all-completions "" collection predicate))
                     (cl-find-if (lambda (c) (string-prefix-p "Extend" c)) offered)))
                  ((symbol-function 'message) #'ignore))
          (org-scribe-planner-check-progress)))
      (should (string-match-p "Behind: -5000 words" prompt-seen))
      (should (= (length offered) 3))
      (should (string-prefix-p "Keep the plan as it is" (car offered)))
      (should (cl-some (lambda (c) (string-prefix-p "Increase the daily target" c)) offered))
      (should (member "Extend the deadline (keep the daily target) → ends 2026-12-05, 1000 words/day, total 30000 words"
                      offered))
      (should (string= (org-scribe-plan-end-date plan) "2026-12-05")))))

(ert-deftest test-planner-status-check-ahead-offers-reduce-or-finish-early ()
  "Ahead: the options are to reduce the daily target or finish early."
  (test-status--on "2026-11-11"
    (let ((plan (test-status--plan 15000))
          (offered nil))
      (test-status--with-current plan
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &optional predicate &rest _)
                     (setq offered (all-completions "" collection predicate))
                     (car offered)))
                  ((symbol-function 'message) #'ignore))
          (org-scribe-planner-check-progress)))
      (should (cl-some (lambda (c) (string-prefix-p "Reduce the daily target" c)) offered))
      (should (cl-some (lambda (c) (string-prefix-p "Finish early" c)) offered))
      ;; The default, keeping the plan, changed nothing
      (should (= (org-scribe-plan-daily-words plan) 1000))
      (should (string= (org-scribe-plan-end-date plan) "2026-11-30")))))

;;; A recalculation on a day already written (reported 2026-11-01)

(defun test-status--recalculated-on-day-one ()
  "50,000 words over 30 days from Nov 1, recalculated on Nov 1 at 2,334
words, then written on to 2,564 the same day.  Today is Nov 1."
  (let ((plan (make-org-scribe-plan :title "P" :total-words 50000 :days 30
                                    :start-date "2026-11-01")))
    (org-scribe-planner--calculate-missing-variable plan)
    (setf (org-scribe-plan-daily-word-counts plan)
          (list (cons "2026-11-01" (list :words 2334 :note ""))))
    (setf (org-scribe-plan-current-words plan) 2334)
    (org-scribe-planner-recalculate-remaining-days plan "/tmp/unused-plan.org")
    (setf (org-scribe-plan-daily-word-counts plan)
          (list (cons "2026-11-01" (list :words 2564 :note ""))))
    (setf (org-scribe-plan-current-words plan) 2564)
    plan))

(defun test-status--day (plan date)
  "PLAN's schedule entry for DATE."
  (cl-find date (org-scribe-planner--generate-day-schedule plan)
           :key (lambda (day) (plist-get day :date)) :test #'string=))

(ert-deftest test-planner-status-recalculated-on-day-one-keeps-todays-goal ()
  "Recalculated after writing, today keeps the goal it had: 1,667 due, so
897 ahead, about half a writing day (897 of Nov 2's 1,644).  Regression:
the rebased starting point (667) was taken as due, so +1,897."
  (test-status--on "2026-11-01"
    (let ((plan (test-status--recalculated-on-day-one)))
      (should (= (org-scribe-plan-schedule-adjustment plan) -667))
      (should (string= (org-scribe-plan-rebased-from plan) "2026-11-02"))
      (should (= (org-scribe-plan-daily-words plan) 1644))
      (let ((status (org-scribe-planner--status plan)))
        (should (= (plist-get status :expected) 1667))
        (should (= (plist-get status :difference) 897))
        (should (< (abs (- (plist-get status :days) (/ 897.0 1644))) 1e-9)))
      ;; Against the days that are over (check-progress): nothing was due
      (should (= (plist-get (org-scribe-planner--status plan 'yesterday) :difference)
                 2564)))))

(ert-deftest test-planner-status-recalculated-plan-takes-over-the-next-day ()
  "From the recalculation's first day the recalculated plan counts: on
Nov 2 it expects 2,334 (where it was recalculated from) plus Nov 2's
1,644.  Against yesterday, the 230 written after recalculating."
  (test-status--on "2026-11-01"
    (let ((plan (test-status--recalculated-on-day-one)))
      (setq test-status--today "2026-11-02")
      (let ((status (org-scribe-planner--status plan)))
        (should (= (plist-get status :expected) 3978))
        (should (= (plist-get status :difference) -1414)))
      (should (= (plist-get (org-scribe-planner--status plan 'yesterday) :difference)
                 230)))))

(ert-deftest test-planner-status-planned-totals-step-at-the-recalculation ()
  "The day before the recalculation is planned as it stood (1,667); the
recalculated plan follows (3,978 on Nov 2).  The step between them is
the 667 words written above the plan when it was recalculated."
  (test-status--on "2026-11-01"
    (let ((plan (test-status--recalculated-on-day-one)))
      (should (= 1667 (org-scribe-planner--planned-total
                       plan (test-status--day plan "2026-11-01"))))
      (should (= 3978 (org-scribe-planner--planned-total
                       plan (test-status--day plan "2026-11-02"))))
      (should (= 50000 (org-scribe-planner--planned-total
                        plan (test-status--day plan "2026-11-30"))))
      (should (equal (org-scribe-planner--rebase-step plan)
                     '(:date "2026-11-02" :from 1667 :to 2334)))
      ;; Days after the recalculation stay on the recalculated plan once
      ;; they are past and locked too
      (setq test-status--today "2026-11-05")
      (org-scribe-planner--lock-past-goals plan)
      (should (= 3978 (org-scribe-planner--planned-total
                       plan (test-status--day plan "2026-11-02"))))
      (should (= 1667 (org-scribe-planner--planned-total
                       plan (test-status--day plan "2026-11-01")))))))

(ert-deftest test-planner-status-rebase-date-survives-a-save ()
  "REBASED_FROM is written and read back; a plan saved without it has none."
  (let ((file (make-temp-file "test-status-plan-" nil ".org"))
        (plan (test-status--on "2026-11-01" (test-status--recalculated-on-day-one))))
    (unwind-protect
        (cl-letf (((symbol-function 'org-scribe-planner--get-today-date)
                   (lambda () "2026-11-05")))
          (org-scribe-planner--save-plan plan file)
          (should (string= "2026-11-02"
                           (org-scribe-plan-rebased-from
                            (org-scribe-planner--load-plan file))))
          ;; An older file: no REBASED_FROM
          (with-current-buffer (find-file-noselect file)
            (goto-char (point-min))
            (re-search-forward "^\\*")
            (org-entry-delete nil "REBASED_FROM")
            (save-buffer))
          (should-not (org-scribe-plan-rebased-from
                       (org-scribe-planner--load-plan file))))
      (when-let* ((buffer (get-file-buffer file))) (kill-buffer buffer))
      (delete-file file))))

;;; A recalculation that went out of date (2026-11-02)

(ert-deftest test-planner-status-recalculation-goes-stale-when-the-day-is-written-on ()
  "Recalculated at 2,334, written on to 2,564 the same day: the plan was
anchored at 2,334 words and now sees 2,564."
  (test-status--on "2026-11-01"
    (let ((plan (test-status--recalculated-on-day-one)))
      (should (= (org-scribe-plan-rebased-words plan) 2334))
      (should (string= (org-scribe-plan-rebased-on plan) "2026-11-01"))
      (should (equal (org-scribe-planner--rebase-stale plan)
                     '(:date "2026-11-01" :anchored 2334 :written 2564)))
      (should (string-match-p "2334 words through 2026-11-01; 2564"
                              (org-scribe-planner--describe-stale
                               (org-scribe-planner--rebase-stale plan)))))))

(ert-deftest test-planner-status-recalculation-not-stale-when-nothing-changed ()
  "No words after the recalculation, and none before one: not stale."
  (test-status--on "2026-11-01"
    (let ((plan (test-status--plan 2334 "2026-11-01")))
      ;; Never recalculated
      (should-not (org-scribe-planner--rebase-stale plan))
      (org-scribe-planner-recalculate-remaining-days plan "/tmp/unused-plan.org")
      (should (string= (org-scribe-plan-rebased-from plan) "2026-11-02"))
      (should-not (org-scribe-planner--rebase-stale plan))))
  ;; A recalculation made when the day before was already over stays good
  (test-status--on "2026-11-03"
    (let ((plan (test-status--plan 3000 "2026-11-02")))
      (org-scribe-planner-recalculate-remaining-days plan "/tmp/unused-plan.org")
      (should (string= (org-scribe-plan-rebased-from plan) "2026-11-03"))
      (should-not (org-scribe-planner--rebase-stale plan)))))

(ert-deftest test-planner-status-recalculation-stale-when-the-day-ends-under-its-goal ()
  "Recalculated at 500 words (the day was credited with its 1,667 goal),
the day ends at 500: once it is over, the recalculation is stale."
  (test-status--on "2026-11-01"
    (let ((plan (test-status--plan 500 "2026-11-01")))
      (org-scribe-planner-recalculate-remaining-days plan "/tmp/unused-plan.org")
      ;; Still being written: the goal it was credited with is not missed yet
      (should-not (org-scribe-planner--rebase-stale plan))
      (setq test-status--today "2026-11-02")
      (should (org-scribe-planner--rebase-stale plan)))))

(ert-deftest test-planner-status-recalculation-stale-for-a-plan-saved-before-the-counts ()
  "A plan without REBASED_ON / REBASED_WORDS is checked against its rebased total."
  (test-status--on "2026-11-01"
    (let ((plan (test-status--recalculated-on-day-one)))
      (setf (org-scribe-plan-rebased-on plan) nil
            (org-scribe-plan-rebased-words plan) nil)
      (should (equal (org-scribe-planner--rebase-stale plan)
                     '(:date "2026-11-01" :anchored 2334 :written 2564))))))

(ert-deftest test-planner-status-rebase-counts-survive-a-save ()
  "REBASED_ON and REBASED_WORDS are written and read back."
  (let ((file (make-temp-file "test-status-plan-" nil ".org"))
        (plan (test-status--on "2026-11-01" (test-status--recalculated-on-day-one))))
    (unwind-protect
        (cl-letf (((symbol-function 'org-scribe-planner--get-today-date)
                   (lambda () "2026-11-01")))
          (org-scribe-planner--save-plan plan file)
          (let ((loaded (org-scribe-planner--load-plan file)))
            (should (string= (org-scribe-plan-rebased-on loaded) "2026-11-01"))
            (should (= (org-scribe-plan-rebased-words loaded) 2334))))
      (when-let* ((buffer (get-file-buffer file))) (kill-buffer buffer))
      (delete-file file))))

(ert-deftest test-planner-status-check-progress-names-the-stale-recalculation ()
  "The check-progress question says the recalculation is out of date."
  (test-status--on "2026-11-01"
    (let ((plan (test-status--recalculated-on-day-one))
          (asked nil))
      (let ((org-scribe-planner--current-plan plan)
            (org-scribe-planner--current-plan-file "/tmp/unused-plan.org"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt collection &rest _)
                     (setq asked prompt)
                     (car (all-completions "" collection)))))
          (org-scribe-planner-check-progress))
        (should (string-match-p "Out of date: Recalculated at 2334 words" asked))))))

;;; The offer on the first save of a new day

(defmacro test-status--offering (answers &rest body)
  "Run BODY interactively, `y-or-n-p' answering from ANSWERS; collects prompts
in `test-status--prompts'."
  (declare (indent 1))
  `(let ((noninteractive nil)
         (org-scribe-planner--refresh-offered nil)
         (answers ,answers)
         (test-status--prompts nil))
     (cl-letf (((symbol-function 'y-or-n-p)
                (lambda (prompt) (push prompt test-status--prompts) (pop answers))))
       ,@body)))

(defvar test-status--prompts nil)

(ert-deftest test-planner-status-offer-recalculates-a-stale-plan-when-accepted ()
  "Nov 2, first count of the day: the stale plan is offered a recalculation
from the real 2,564 words; accepting re-anchors it, and it is asked once."
  (test-status--on "2026-11-02"
    (test-status--offering '(t t)
      (let ((plan (let ((test-status--today "2026-11-01"))
                    (test-status--recalculated-on-day-one))))
        (should (org-scribe-planner--offer-refresh plan "/tmp/unused-plan.org"))
        (should (= (length test-status--prompts) 1))
        (should (string-match-p "Recalculated at 2334 words" (car test-status--prompts)))
        (should (= (org-scribe-plan-schedule-adjustment plan) -897))
        (should (string= (org-scribe-plan-rebased-from plan) "2026-11-02"))
        (should-not (org-scribe-planner--rebase-stale plan))
        ;; Once a day
        (setf (org-scribe-plan-rebased-words plan) 0)
        (should-not (org-scribe-planner--offer-refresh plan "/tmp/unused-plan.org"))
        (should (= (length test-status--prompts) 1))))))

(ert-deftest test-planner-status-offer-declined-changes-nothing ()
  "Saying no leaves the plan as it was."
  (test-status--on "2026-11-02"
    (test-status--offering '(nil)
      (let ((plan (let ((test-status--today "2026-11-01"))
                    (test-status--recalculated-on-day-one))))
        (should-not (org-scribe-planner--offer-refresh plan "/tmp/unused-plan.org"))
        (should (= (length test-status--prompts) 1))
        (should (= (org-scribe-plan-schedule-adjustment plan) -667))
        (should (org-scribe-planner--rebase-stale plan))))))

(ert-deftest test-planner-status-offer-silent-without-need ()
  "No offer for a plan never recalculated, an up-to-date one, batch mode, or
with the option off."
  (test-status--on "2026-11-02"
    (test-status--offering '(t t t t)
      (should-not (org-scribe-planner--offer-refresh
                   (test-status--plan 3000 "2026-11-01") "/tmp/unused-plan.org"))
      (let ((plan (let ((test-status--today "2026-11-01"))
                    (test-status--recalculated-on-day-one))))
        (let ((noninteractive t))
          (should-not (org-scribe-planner--offer-refresh plan "/tmp/unused-plan.org")))
        (let ((org-scribe-planner-offer-refresh-on-new-day nil))
          (should-not (org-scribe-planner--offer-refresh plan "/tmp/unused-plan.org"))))
      (should-not test-status--prompts))))

;;; Streak

(ert-deftest test-planner-status-streak-ignores-days-to-come ()
  "Three days written, today not yet: the streak is 3.  Regression: the
plan's future days, with no words, broke it to 0."
  (test-status--on "2026-11-04"
    (let ((plan (test-status--plan 0)))
      (setf (org-scribe-plan-daily-word-counts plan)
            (list (cons "2026-11-01" (list :words 1000 :note ""))
                  (cons "2026-11-02" (list :words 2000 :note ""))
                  (cons "2026-11-03" (list :words 3000 :note ""))))
      (setf (org-scribe-plan-current-words plan) 3000)
      (let ((streak (org-scribe-planner--calculate-current-streak plan)))
        (should (= (plist-get streak :current) 3))
        (should (= (plist-get streak :longest) 3)))
      ;; Written today too
      (push (cons "2026-11-04" (list :words 3500 :note ""))
            (org-scribe-plan-daily-word-counts plan))
      (should (= (plist-get (org-scribe-planner--calculate-current-streak plan) :current) 4)))))

(provide 'test-planner-status)
;;; test-planner-status.el ends here
