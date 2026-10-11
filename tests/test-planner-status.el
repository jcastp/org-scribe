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
  "Exactly the words expected through yesterday: on track, not ahead."
  (test-status--on "2026-11-11"
    (let ((status (org-scribe-planner--status (test-status--plan 10000))))
      (should (eq (plist-get status :status) 'on-track))
      (should (= (plist-get status :difference) 0))
      (should (= (plist-get status :days) 0))
      (should (string= (org-scribe-planner--describe-status status) "On track")))))

(ert-deftest test-planner-status-ahead-in-words-percent-and-days ()
  "2,000 words ahead of 10,000 expected: +20%, two writing days."
  (test-status--on "2026-11-11"
    (let ((status (org-scribe-planner--status (test-status--plan 12000))))
      (should (eq (plist-get status :status) 'ahead))
      (should (= (plist-get status :difference) 2000))
      (should (= (plist-get status :percent) 20.0))
      (should (= (plist-get status :days) 2.0))
      (should (string= (org-scribe-planner--describe-status status)
                       "Ahead: +2000 words (+20.0%, +2.0 writing days)")))))

(ert-deftest test-planner-status-behind ()
  "500 words short: behind by 5% and half a writing day."
  (test-status--on "2026-11-11"
    (let ((status (org-scribe-planner--status (test-status--plan 9500))))
      (should (eq (plist-get status :status) 'behind))
      (should (= (plist-get status :percent) -5.0))
      (should (= (plist-get status :days) -0.5))
      (should (string= (org-scribe-planner--describe-status status)
                       "Behind: -500 words (-5.0%, -0.5 writing days)")))))

(ert-deftest test-planner-status-days-follow-each-days-goal ()
  "Days behind are measured along the schedule, day by day.
After a recalculation the past days keep 1,000 and the open days get
1,250; dividing the shortfall by the daily target would say 1.2 days."
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
        (should (= (plist-get status :difference) -1500))
        (should (= (plist-get status :days) -1.25))))))

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
  (test-status--on "2026-11-11"
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

(provide 'test-planner-status)
;;; test-planner-status.el ends here
