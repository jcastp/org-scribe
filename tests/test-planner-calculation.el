;;; test-calculation.el --- Tests for the core calculation engine -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for org-scribe-planner--calculate-missing-variable,
;; org-scribe-planner--calculate-dates, and date arithmetic helpers.

;;; Code:

(require 'ert)
(require 'org-scribe-planner)

;;; --calculate-missing-variable

(ert-deftest test-planner-calc-daily-words-from-total-and-days ()
  "Calculate daily-words when total-words and days are provided."
  (let ((plan (make-org-scribe-plan :total-words 10000 :days 10)))
    (org-scribe-planner--calculate-missing-variable plan)
    (should (= (org-scribe-plan-daily-words plan) 1000))))

(ert-deftest test-planner-calc-daily-words-rounds-up ()
  "daily-words is ceiled, not floored (1000/3 → 334)."
  (let ((plan (make-org-scribe-plan :total-words 1000 :days 3)))
    (org-scribe-planner--calculate-missing-variable plan)
    (should (= (org-scribe-plan-daily-words plan) 334))))

(ert-deftest test-planner-calc-total-words-from-daily-and-days ()
  "Calculate total-words when daily-words and days are provided."
  (let ((plan (make-org-scribe-plan :daily-words 500 :days 20)))
    (org-scribe-planner--calculate-missing-variable plan)
    (should (= (org-scribe-plan-total-words plan) 10000))))

(ert-deftest test-planner-calc-days-from-total-and-daily ()
  "Calculate days when total-words and daily-words are provided."
  (let ((plan (make-org-scribe-plan :total-words 10000 :daily-words 1000)))
    (org-scribe-planner--calculate-missing-variable plan)
    (should (= (org-scribe-plan-days plan) 10))))

(ert-deftest test-planner-calc-days-rounds-up ()
  "days is ceiled when total-words is not divisible by daily-words."
  (let ((plan (make-org-scribe-plan :total-words 1000 :daily-words 300)))
    (org-scribe-planner--calculate-missing-variable plan)
    ;; 1000/300 = 3.33 → ceiling = 4
    (should (= (org-scribe-plan-days plan) 4))))

(ert-deftest test-planner-calc-spare-days-reduce-working-days ()
  "Spare days reduce the working-day count, increasing daily-words."
  (let ((plan (make-org-scribe-plan :total-words 8000 :days 10
                                    :spare-days '("2024-11-02" "2024-11-03"))))
    (org-scribe-planner--calculate-missing-variable plan)
    ;; 8000 / (10 - 2 spare days) = 1000 words/day
    (should (= (org-scribe-plan-daily-words plan) 1000))))

(ert-deftest test-planner-calc-spare-days-increase-total-days ()
  "When calculating days, spare days are added to the working days total."
  (let ((plan (make-org-scribe-plan :total-words 8000 :daily-words 1000
                                    :spare-days '("2024-11-02" "2024-11-03"))))
    (org-scribe-planner--calculate-missing-variable plan)
    ;; 8000/1000 = 8 working days + 2 spare = 10 total days
    (should (= (org-scribe-plan-days plan) 10))))

(ert-deftest test-planner-calc-spare-days-outside-window-ignored ()
  "Spare days outside the plan's window don't shorten it.
Regression: every entry of SPARE-DAYS was subtracted from the working
days, wherever its date fell, so a spare day before the start or after
the end raised the daily target."
  (let ((plan (make-org-scribe-plan :total-words 8000 :days 10
                                    :start-date "2026-11-01"
                                    :spare-days '("2026-11-03" "2026-11-04"
                                                  "2026-10-20" "2026-12-25"))))
    (org-scribe-planner--calculate-missing-variable plan)
    ;; Only Nov 3 and 4 are inside Nov 1-10: 8000 / 8 working days
    (should (= (org-scribe-plan-daily-words plan) 1000))))

(ert-deftest test-planner-calc-total-ignores-spare-days-outside-window ()
  "The total for a daily target counts only spare days inside the window."
  (let ((plan (make-org-scribe-plan :daily-words 500 :days 10
                                    :start-date "2026-11-01"
                                    :spare-days '("2026-11-05" "2027-01-01"))))
    (org-scribe-planner--calculate-missing-variable plan)
    ;; 9 working days (Nov 5 is spare; Jan 1 is outside)
    (should (= (org-scribe-plan-total-words plan) 4500))))

(ert-deftest test-planner-calc-days-window-grows-over-later-spare-days ()
  "Calculating days: a spare day just past the working days still counts.
8 working days from Nov 1 end on Nov 8; the spare day on Nov 5 pushes the
end to Nov 9, which is also spare, so the plan needs 10 calendar days."
  (let ((plan (make-org-scribe-plan :total-words 8000 :daily-words 1000
                                    :start-date "2026-11-01"
                                    :spare-days '("2026-11-05" "2026-11-09"
                                                  "2026-12-01"))))
    (org-scribe-planner--calculate-missing-variable plan)
    (should (= (org-scribe-plan-days plan) 10))))

(ert-deftest test-planner-spare-days-in-window-without-start-counts-all ()
  "Without a start date there is no window yet, so every spare day counts."
  (let ((plan (make-org-scribe-plan :spare-days '("2026-11-01" "2030-01-01"))))
    (should (= (org-scribe-planner--spare-days-in-window plan 5) 2))))

(ert-deftest test-planner-end-after-working-days-skips-spare-days ()
  "The end date lands after WORKING-DAYS non-spare days, FROM included."
  (should (string= (org-scribe-planner--end-after-working-days
                    "2026-11-01" 3 '("2026-11-02"))
                   "2026-11-04"))
  ;; A spare first day doesn't count
  (should (string= (org-scribe-planner--end-after-working-days
                    "2026-11-01" 1 '("2026-11-01"))
                   "2026-11-02"))
  ;; Below one working day, FROM itself
  (should (string= (org-scribe-planner--end-after-working-days
                    "2026-11-01" 0 nil)
                   "2026-11-01")))

(ert-deftest test-planner-calc-error-when-all-days-are-spare ()
  "Error when spare days consume all available days."
  (let ((plan (make-org-scribe-plan :total-words 10000 :days 2
                                    :spare-days '("2024-11-01" "2024-11-02"))))
    (should-error (org-scribe-planner--calculate-missing-variable plan))))

(ert-deftest test-planner-calc-error-when-fewer-than-two-vars ()
  "Error when fewer than 2 of the 3 variables are provided."
  (let ((plan (make-org-scribe-plan :total-words 10000)))
    (should-error (org-scribe-planner--calculate-missing-variable plan))))

(ert-deftest test-planner-calc-error-when-no-vars ()
  "Error when no variables are provided."
  (let ((plan (make-org-scribe-plan)))
    (should-error (org-scribe-planner--calculate-missing-variable plan))))

;;; Date arithmetic helpers

(ert-deftest test-planner-add-days-basic ()
  "Adding 29 days to November 1st yields November 30th."
  (should (string= (org-scribe-planner--add-days "2024-11-01" 29) "2024-11-30")))

(ert-deftest test-planner-add-days-across-month-boundary ()
  "Adding 1 day to January 31st yields February 1st."
  (should (string= (org-scribe-planner--add-days "2024-01-31" 1) "2024-02-01")))

(ert-deftest test-planner-days-between-same-day ()
  "A range from a date to itself spans exactly 1 day."
  (should (= (org-scribe-planner--days-between "2024-11-01" "2024-11-01") 1)))

(ert-deftest test-planner-days-between-one-month ()
  "November 1st to November 30th spans 30 days (inclusive)."
  (should (= (org-scribe-planner--days-between "2024-11-01" "2024-11-30") 30)))

;;; --calculate-dates

(ert-deftest test-planner-calculate-dates-derives-end-date ()
  "End date is derived from start date + days (days-1 offset for inclusive range)."
  (let ((plan (make-org-scribe-plan :start-date "2024-11-01" :days 30)))
    (org-scribe-planner--calculate-dates plan)
    (should (string= (org-scribe-plan-end-date plan) "2024-11-30"))))

(ert-deftest test-planner-calculate-dates-uses-today-as-default ()
  "When start-date is nil, today is used and end-date is derived."
  (let ((plan (make-org-scribe-plan :days 10)))
    (org-scribe-planner--calculate-dates plan)
    (should (org-scribe-plan-start-date plan))
    (should (org-scribe-planner--validate-date-format (org-scribe-plan-start-date plan)))
    (should (org-scribe-plan-end-date plan))))

(ert-deftest test-planner-calculate-dates-preserves-existing-end-date ()
  "If end-date is already set, it is not overwritten."
  (let ((plan (make-org-scribe-plan :start-date "2024-11-01"
                                    :end-date "2025-01-01"
                                    :days 30)))
    (org-scribe-planner--calculate-dates plan)
    (should (string= (org-scribe-plan-end-date plan) "2025-01-01"))))

(ert-deftest test-planner-calculate-dates-error-without-days ()
  "Error when days is not set."
  (let ((plan (make-org-scribe-plan :start-date "2024-11-01")))
    (should-error (org-scribe-planner--calculate-dates plan))))

;;; test-calculation.el ends here
