;;; test-data-helpers.el --- Tests for daily-count data helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the daily word count serialization, migration, accessors,
;; and spare day note management functions.

;;; Code:

(require 'ert)
(require 'org-scribe-planner)

;;; --format-daily-count-entry

(ert-deftest test-planner-format-entry-words-only ()
  "Entry with only words serializes as DATE:WORDS."
  (should (string= (org-scribe-planner--format-daily-count-entry
                    '("2024-11-01" . (:words 1500 :note "" :target nil)))
                   "2024-11-01:1500")))

(ert-deftest test-planner-format-entry-words-and-note ()
  "Entry with words and a note serializes as DATE:WORDS:NOTE."
  (should (string= (org-scribe-planner--format-daily-count-entry
                    '("2024-11-01" . (:words 1500 :note "great session" :target nil)))
                   "2024-11-01:1500:great session")))

(ert-deftest test-planner-format-entry-words-and-target ()
  "Entry with words and a target serializes as DATE:WORDS::TARGET."
  (should (string= (org-scribe-planner--format-daily-count-entry
                    '("2024-11-01" . (:words 1500 :note "" :target 2000)))
                   "2024-11-01:1500::2000")))

(ert-deftest test-planner-format-entry-all-fields ()
  "Entry with words, note, and target serializes as DATE:WORDS:NOTE:TARGET."
  (should (string= (org-scribe-planner--format-daily-count-entry
                    '("2024-11-01" . (:words 1500 :note "good day" :target 2000)))
                   "2024-11-01:1500:good day:2000")))

;;; --sorted-daily-counts / --daily-deltas / --entry-delta /
;;; --previous-cumulative-total (Phase 6b: single cumulative ledger)
;;
;; DAILY_WORD_COUNTS entries store the manuscript's CUMULATIVE word total
;; as of each date, not that day's own delta — these are the primitives
;; that derive the actual net words written on a given date.

(ert-deftest test-planner-sorted-daily-counts-orders-chronologically ()
  "Entries come back sorted ascending by date regardless of input order."
  (let ((counts '(("2024-11-03" . (:words 300))
                  ("2024-11-01" . (:words 100))
                  ("2024-11-02" . (:words 200)))))
    (should (equal (mapcar #'car (org-scribe-planner--sorted-daily-counts counts))
                  '("2024-11-01" "2024-11-02" "2024-11-03")))))

(ert-deftest test-planner-sorted-daily-counts-excludes-note-only-entries ()
  "Note-only entries (no numeric :words) are excluded, same as --counts-with-words."
  (let ((counts '(("2024-11-01" . (:words 100))
                  ("2024-11-02" . (:note "rest day")))))
    (should (equal (mapcar #'car (org-scribe-planner--sorted-daily-counts counts))
                  '("2024-11-01")))))

(ert-deftest test-planner-daily-deltas-first-entry-equals-its-own-total ()
  "The earliest entry's delta equals its cumulative total (implicit 0 baseline)."
  (let ((counts '(("2024-11-01" . (:words 500)))))
    (should (equal (org-scribe-planner--daily-deltas counts)
                  '(("2024-11-01" . 500))))))

(ert-deftest test-planner-daily-deltas-derives-net-change-across-entries ()
  "Each subsequent entry's delta is its cumulative total minus the previous one's."
  (let ((counts '(("2024-11-01" . (:words 500))
                  ("2024-11-02" . (:words 800))
                  ("2024-11-04" . (:words 750)))))  ; gap day + a net loss
    (should (equal (org-scribe-planner--daily-deltas counts)
                  '(("2024-11-01" . 500) ("2024-11-02" . 300) ("2024-11-04" . -50))))))

(ert-deftest test-planner-entry-delta-returns-nil-when-no-entry ()
  "entry-delta returns nil for a date with no recorded cumulative total."
  (let ((counts '(("2024-11-01" . (:words 500)))))
    (should (null (org-scribe-planner--entry-delta counts "2024-11-02")))))

(ert-deftest test-planner-entry-delta-matches-daily-deltas ()
  "entry-delta for a given date agrees with --daily-deltas."
  (let ((counts '(("2024-11-01" . (:words 500)) ("2024-11-02" . (:words 900)))))
    (should (= (org-scribe-planner--entry-delta counts "2024-11-02") 400))))

(ert-deftest test-planner-previous-cumulative-total-zero-for-earliest-date ()
  "previous-cumulative-total is 0 when DATE is the earliest (or only) entry."
  (let ((counts '(("2024-11-01" . (:words 500)))))
    (should (= (org-scribe-planner--previous-cumulative-total counts "2024-11-01") 0))))

(ert-deftest test-planner-previous-cumulative-total-finds-prior-entry ()
  "previous-cumulative-total returns the immediately preceding entry's total."
  (let ((counts '(("2024-11-01" . (:words 500)) ("2024-11-02" . (:words 900)))))
    (should (= (org-scribe-planner--previous-cumulative-total counts "2024-11-02") 500))
    ;; A future/unrecorded date still resolves against the latest known entry
    (should (= (org-scribe-planner--previous-cumulative-total counts "2024-11-05") 900))))

;;; --counts-with-words

(ert-deftest test-planner-counts-with-words-keeps-word-entries ()
  "Entries with a numeric :words field are kept."
  (let ((counts '(("2024-11-01" . (:words 1000 :note "" :target nil)))))
    (should (= (length (org-scribe-planner--counts-with-words counts)) 1))))

(ert-deftest test-planner-counts-with-words-filters-note-only-entries ()
  "Note-only entries (no numeric :words) are excluded."
  (let ((counts '(("2024-11-01" . (:words 1000 :note ""))
                  ("2024-11-02" . (:note "rest day")))))  ; note-only, no :words
    (should (= (length (org-scribe-planner--counts-with-words counts)) 1))
    (should (string= (caar (org-scribe-planner--counts-with-words counts))
                     "2024-11-01"))))

;;; Entry accessors

(ert-deftest test-planner-get-entry-words ()
  "Extracts the :words value from an entry."
  (let ((entry '("2024-11-01" . (:words 750 :note "good" :target 1000))))
    (should (= (org-scribe-planner--get-entry-words entry) 750))))

(ert-deftest test-planner-get-entry-note-returns-value ()
  "Extracts the :note value from an entry."
  (let ((entry '("2024-11-01" . (:words 750 :note "good session" :target 1000))))
    (should (string= (org-scribe-planner--get-entry-note entry) "good session"))))

(ert-deftest test-planner-get-entry-note-defaults-to-empty-string ()
  "Returns empty string when :note is absent."
  (let ((entry '("2024-11-01" . (:words 750 :target 1000))))
    (should (string= (org-scribe-planner--get-entry-note entry) ""))))

(ert-deftest test-planner-get-entry-target-returns-value ()
  "Extracts the :target value from an entry."
  (let ((entry '("2024-11-01" . (:words 750 :note "" :target 1000))))
    (should (= (org-scribe-planner--get-entry-target entry) 1000))))

(ert-deftest test-planner-get-entry-target-returns-nil-when-absent ()
  "Returns nil when :target is not stored."
  (let ((entry '("2024-11-01" . (:words 750 :note ""))))
    (should (null (org-scribe-planner--get-entry-target entry)))))

;;; --add-spare-day-note / --remove-spare-day-note

(ert-deftest test-planner-add-spare-day-note-creates-new-entry ()
  "Adding a note to a date with no existing entry creates a note-only entry."
  (let ((plan (make-org-scribe-plan)))
    (org-scribe-planner--add-spare-day-note plan "2024-11-02" "Holiday")
    (let ((entry (assoc "2024-11-02" (org-scribe-plan-daily-word-counts plan))))
      (should entry)
      (should (string= (plist-get (cdr entry) :note) "Holiday"))
      ;; Note-only: no :words field (or nil)
      (should (not (numberp (plist-get (cdr entry) :words)))))))

(ert-deftest test-planner-add-spare-day-note-updates-existing-note-only-entry ()
  "Adding a note over an existing note-only entry replaces the note."
  (let ((plan (make-org-scribe-plan)))
    (org-scribe-planner--add-spare-day-note plan "2024-11-02" "First note")
    (org-scribe-planner--add-spare-day-note plan "2024-11-02" "Updated note")
    (let ((entry (assoc "2024-11-02" (org-scribe-plan-daily-word-counts plan))))
      (should (string= (plist-get (cdr entry) :note) "Updated note")))))

(ert-deftest test-planner-add-spare-day-note-preserves-words-in-existing-entry ()
  "Adding a note to an entry that already has words preserves the word count."
  (let ((plan (make-org-scribe-plan
               :daily-word-counts
               '(("2024-11-02" . (:words 500 :note "" :target 1000))))))
    (org-scribe-planner--add-spare-day-note plan "2024-11-02" "Wrote on rest day")
    (let ((entry (assoc "2024-11-02" (org-scribe-plan-daily-word-counts plan))))
      (should (= (plist-get (cdr entry) :words) 500))
      (should (string= (plist-get (cdr entry) :note) "Wrote on rest day")))))

(ert-deftest test-planner-remove-spare-day-note-removes-note-only-entry ()
  "Removing a note from a note-only entry deletes the entry entirely."
  (let ((plan (make-org-scribe-plan)))
    (org-scribe-planner--add-spare-day-note plan "2024-11-02" "Holiday")
    (org-scribe-planner--remove-spare-day-note plan "2024-11-02")
    (should (null (assoc "2024-11-02" (org-scribe-plan-daily-word-counts plan))))))

(ert-deftest test-planner-remove-spare-day-note-preserves-entry-with-words ()
  "Removing a note does not delete an entry that carries real word counts."
  (let ((plan (make-org-scribe-plan
               :daily-word-counts
               '(("2024-11-02" . (:words 500 :note "Holiday" :target 1000))))))
    (org-scribe-planner--remove-spare-day-note plan "2024-11-02")
    ;; Entry should still exist because it has words
    (should (assoc "2024-11-02" (org-scribe-plan-daily-word-counts plan)))))

;;; test-data-helpers.el ends here
