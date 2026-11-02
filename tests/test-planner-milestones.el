;;; test-milestones.el --- Tests for milestone tracking -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for org-scribe-planner--get-enhanced-milestones.

;;; Code:

(require 'ert)
(require 'org-scribe-planner)

;;; --get-enhanced-milestones

(ert-deftest test-planner-milestones-returns-four-entries ()
  "Milestones list always contains exactly 4 entries (25, 50, 75, 100 %)."
  (let* ((plan (make-org-scribe-plan
                :total-words 4000 :daily-words 1000 :days 4
                :start-date "2030-06-01" :end-date "2030-06-04"
                :daily-word-counts nil))
         (milestones (org-scribe-planner--get-enhanced-milestones plan)))
    (should (= (length milestones) 4))
    (should (= (plist-get (nth 0 milestones) :percent) 25))
    (should (= (plist-get (nth 1 milestones) :percent) 50))
    (should (= (plist-get (nth 2 milestones) :percent) 75))
    (should (= (plist-get (nth 3 milestones) :percent) 100))))

(ert-deftest test-planner-milestones-all-unreached-for-fresh-plan ()
  "A plan with no daily entries has all milestones unreached."
  (let* ((plan (make-org-scribe-plan
                :total-words 4000 :daily-words 1000 :days 4
                :start-date "2030-06-01" :end-date "2030-06-04"
                :daily-word-counts nil))
         (milestones (org-scribe-planner--get-enhanced-milestones plan)))
    (dolist (m milestones)
      (should-not (plist-get m :reached)))))

(ert-deftest test-planner-milestones-unreached-have-expected-dates ()
  "Unreached milestones on a future plan should have projected expected dates."
  (let* ((plan (make-org-scribe-plan
                :total-words 4000 :daily-words 1000 :days 4
                :start-date "2030-06-01" :end-date "2030-06-04"
                :daily-word-counts nil))
         (milestones (org-scribe-planner--get-enhanced-milestones plan)))
    ;; Every milestone should have an expected date since there are enough days
    (dolist (m milestones)
      (should (plist-get m :expected)))))

(ert-deftest test-planner-milestones-25-percent-reached-correctly ()
  "25% milestone is marked reached with correct date when actual progress crosses it."
  (let* ((plan (make-org-scribe-plan
                :total-words 4000 :daily-words 1000 :days 4
                :start-date "2024-11-01" :end-date "2024-11-04"
                :daily-word-counts
                '(("2024-11-01" . (:words 1000 :note "" :target 1000)))))
         (milestones (org-scribe-planner--get-enhanced-milestones plan))
         (m25 (cl-find 25 milestones :key (lambda (m) (plist-get m :percent)))))
    (should (plist-get m25 :reached))
    (should (string= (plist-get m25 :date) "2024-11-01"))))

(ert-deftest test-planner-milestones-50-percent-not-reached-at-25 ()
  "50% milestone is not reached when only 25% of words have been written."
  (let* ((plan (make-org-scribe-plan
                :total-words 4000 :daily-words 1000 :days 4
                :start-date "2024-11-01" :end-date "2024-11-04"
                :daily-word-counts
                '(("2024-11-01" . (:words 1000 :note "" :target 1000)))))
         (milestones (org-scribe-planner--get-enhanced-milestones plan))
         (m50 (cl-find 50 milestones :key (lambda (m) (plist-get m :percent)))))
    (should-not (plist-get m50 :reached))))

(ert-deftest test-planner-milestones-100-percent-reached ()
  "100% milestone is reached when all words are written.
:words holds the CUMULATIVE manuscript total as of each date (Phase 6b),
so day 2's entry is 2000 (total written by then), not another 1000
added on top."
  (let* ((plan (make-org-scribe-plan
                :total-words 2000 :daily-words 1000 :days 2
                :start-date "2024-11-01" :end-date "2024-11-02"
                :daily-word-counts
                '(("2024-11-01" . (:words 1000 :note "" :target 1000))
                  ("2024-11-02" . (:words 2000 :note "" :target 1000)))))
         (milestones (org-scribe-planner--get-enhanced-milestones plan))
         (m100 (cl-find 100 milestones :key (lambda (m) (plist-get m :percent)))))
    (should (plist-get m100 :reached))
    (should (plist-get m100 :date))))

;;; --get-enhanced-milestones

(ert-deftest test-planner-milestones-final-reachable-after-partial-today ()
  "A partial entry for today does not make 100% unreachable.
Four days of 1000 words; 300 written on day one.  The rest of today's
goal plus three more days reaches 4000 on the last day, not never."
  (cl-letf (((symbol-function 'org-scribe-planner--get-today-date)
             (lambda () "2024-11-01")))
    (let* ((plan (make-org-scribe-plan
                  :total-words 4000 :daily-words 1000 :days 4
                  :start-date "2024-11-01" :end-date "2024-11-04"
                  :spare-days nil :current-words 300
                  :daily-word-counts
                  '(("2024-11-01" . (:words 300 :note "" :target 1000)))))
           (ms (org-scribe-planner--get-enhanced-milestones plan))
           (final (car (last ms))))
      (should (= 100 (plist-get final :percent)))
      (should (equal "2024-11-04" (plist-get final :expected))))))

;;; test-milestones.el ends here
