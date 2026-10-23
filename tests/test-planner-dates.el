;;; test-date-validation.el --- Tests for date validation functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple tests to verify date validation works correctly

;;; Code:

(require 'ert)
(require 'org-scribe-planner)

;; Test date format validation
(ert-deftest test-planner-validate-date-format-valid ()
  "Test that valid date formats are accepted."
  (should (org-scribe-planner--validate-date-format "2024-11-19"))
  (should (org-scribe-planner--validate-date-format "2025-01-01"))
  (should (org-scribe-planner--validate-date-format "1999-12-31")))

(ert-deftest test-planner-validate-date-format-invalid ()
  "Test that invalid date formats are rejected."
  (should-not (org-scribe-planner--validate-date-format "2024/11/19"))
  (should-not (org-scribe-planner--validate-date-format "11-19-2024"))
  (should-not (org-scribe-planner--validate-date-format "2024-11-9"))
  (should-not (org-scribe-planner--validate-date-format "24-11-19"))
  (should-not (org-scribe-planner--validate-date-format "not-a-date"))
  (should-not (org-scribe-planner--validate-date-format ""))
  (should-not (org-scribe-planner--validate-date-format nil)))

;; Test full date validation (format + validity)
(ert-deftest test-planner-validate-date-valid ()
  "Test that valid dates pass validation."
  (should (org-scribe-planner--validate-date "2024-11-19"))
  (should (org-scribe-planner--validate-date "2024-02-29"))  ; Leap year
  (should (org-scribe-planner--validate-date "2025-01-01"))
  (should (org-scribe-planner--validate-date "2024-12-31")))

(ert-deftest test-planner-validate-date-invalid-format ()
  "Test that invalid formats raise errors."
  (should-error (org-scribe-planner--validate-date "2024/11/19"))
  (should-error (org-scribe-planner--validate-date "11-19-2024"))
  (should-error (org-scribe-planner--validate-date "not-a-date")))

(ert-deftest test-planner-validate-date-invalid-values ()
  "Test that invalid date values raise errors."
  (should-error (org-scribe-planner--validate-date "2024-13-01"))  ; Month > 12
  (should-error (org-scribe-planner--validate-date "2024-00-01"))  ; Month = 0
  (should-error (org-scribe-planner--validate-date "2024-11-32"))  ; Day > 31
  (should-error (org-scribe-planner--validate-date "2024-02-30"))  ; Feb 30th
  (should-error (org-scribe-planner--validate-date "2023-02-29"))) ; Not leap year

;; Test edge cases
(ert-deftest test-planner-validate-date-edge-cases ()
  "Test edge cases in date validation."
  ;; Leap years
  (should (org-scribe-planner--validate-date "2024-02-29"))
  (should (org-scribe-planner--validate-date "2000-02-29"))
  (should-error (org-scribe-planner--validate-date "2100-02-29"))  ; Not a leap year

  ;; Month boundaries
  (should (org-scribe-planner--validate-date "2024-01-31"))
  (should (org-scribe-planner--validate-date "2024-03-31"))
  (should (org-scribe-planner--validate-date "2024-04-30"))
  (should-error (org-scribe-planner--validate-date "2024-04-31"))) ; April only has 30 days

;;; Choice prompts: RET never ends in a raw "Invalid choice"

(ert-deftest test-planner-choose-ret-takes-the-default ()
  "RET on a choice prompt with a default returns the default."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_p _c _pred _req _i _h default &rest _) (or default ""))))
    (should (equal "Done" (org-scribe-planner--choose "Q: " '("Add" "Done") "Done")))))

(ert-deftest test-planner-choose-ret-without-default-asks-again ()
  "RET on a choice prompt without a default asks again instead of returning \"\"."
  (let ((answers (list "" "" "Weekends"))
        (asked 0))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (cl-incf asked) (pop answers))))
      (should (equal "Weekends" (org-scribe-planner--choose "Q: " '("Weekends"))))
      (should (= 3 asked)))))

(ert-deftest test-planner-read-days-ret-enters-a-number ()
  "RET at \"How do you want to enter the days?\" reads a number of days.
It used to fail with \"Invalid choice: \" and abort the wizard -- most
often right after choosing a preset, which had filled in the days."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_p _c _pred _req _i _h default &rest _) (or default "")))
            ((symbol-function 'read-number) (lambda (_p &optional default) default)))
    (should (= 60 (org-scribe-planner--read-days "Days available: " 60)))))

;;; test-date-validation.el ends here
