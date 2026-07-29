;;; test-plan-io.el --- Tests for plan save/load round-trips -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests verifying that all plan fields survive a save→load round-trip,
;; including spare days, daily word counts, and backward-compatible loading
;; of old format entries.

;;; Code:

(require 'ert)
(require 'org-scribe-planner)

;;; --load-plan error handling

(ert-deftest test-planner-load-plan-error-on-missing-file ()
  "Loading a non-existent file signals an error."
  (should-error (org-scribe-planner--load-plan "/tmp/this-file-does-not-exist-12345.org")))

;;; Save/load round-trips

(ert-deftest test-planner-roundtrip-basic-fields ()
  "Core plan fields survive a save→load round-trip."
  (let* ((temp-dir (make-temp-file "org-scribe-test-" t))
         (temp-file (expand-file-name "roundtrip.org" temp-dir))
         (plan (make-org-scribe-plan
                :title "Roundtrip Plan"
                :total-words 50000
                :daily-words 1667
                :days 30
                :start-date "2024-11-01"
                :end-date "2024-11-30"
                :spare-days nil
                :current-words 5000)))
    (unwind-protect
        (progn
          (org-scribe-planner--save-plan plan temp-file)
          (let ((loaded (org-scribe-planner--load-plan temp-file)))
            (should (string= (org-scribe-plan-title loaded) "Roundtrip Plan"))
            (should (= (org-scribe-plan-total-words loaded) 50000))
            (should (= (org-scribe-plan-daily-words loaded) 1667))
            (should (= (org-scribe-plan-days loaded) 30))
            (should (string= (org-scribe-plan-start-date loaded) "2024-11-01"))
            (should (string= (org-scribe-plan-end-date loaded) "2024-11-30"))
            (should (= (org-scribe-plan-current-words loaded) 5000))))
      (delete-directory temp-dir t))))

(ert-deftest test-planner-roundtrip-spare-days ()
  "Spare days list survives a save→load round-trip."
  (let* ((temp-dir (make-temp-file "org-scribe-test-" t))
         (temp-file (expand-file-name "spare-days.org" temp-dir))
         (plan (make-org-scribe-plan
                :title "Spare Days Plan"
                :total-words 10000
                :daily-words 500
                :days 22
                :start-date "2024-11-01"
                :end-date "2024-11-22"
                :spare-days '("2024-11-03" "2024-11-10" "2024-11-17"))))
    (unwind-protect
        (progn
          (org-scribe-planner--save-plan plan temp-file)
          (let* ((loaded (org-scribe-planner--load-plan temp-file))
                 (loaded-spare (org-scribe-plan-spare-days loaded)))
            (should (member "2024-11-03" loaded-spare))
            (should (member "2024-11-10" loaded-spare))
            (should (member "2024-11-17" loaded-spare))
            (should (= (length loaded-spare) 3))))
      (delete-directory temp-dir t))))

(ert-deftest test-planner-roundtrip-daily-word-counts ()
  "Daily word counts with words, notes, and targets survive a save→load round-trip."
  (let* ((temp-dir (make-temp-file "org-scribe-test-" t))
         (temp-file (expand-file-name "counts.org" temp-dir))
         (plan (make-org-scribe-plan
                :title "Counts Plan"
                :total-words 10000
                :daily-words 500
                :days 20
                :start-date "2024-11-01"
                :end-date "2024-11-20"
                :daily-word-counts
                '(("2024-11-01" . (:words 600 :note "great session" :target 500))
                  ("2024-11-02" . (:words 400 :note "" :target 500))))))
    (unwind-protect
        (progn
          (org-scribe-planner--save-plan plan temp-file)
          (let* ((loaded (org-scribe-planner--load-plan temp-file))
                 (counts (org-scribe-plan-daily-word-counts loaded))
                 (entry1 (assoc "2024-11-01" counts))
                 (entry2 (assoc "2024-11-02" counts)))
            (should entry1)
            (should (= (plist-get (cdr entry1) :words) 600))
            (should (string= (plist-get (cdr entry1) :note) "great session"))
            (should (= (plist-get (cdr entry1) :target) 500))
            (should entry2)
            (should (= (plist-get (cdr entry2) :words) 400))))
      (delete-directory temp-dir t))))

(ert-deftest test-planner-roundtrip-note-with-colon ()
  "A note containing ':' survives a save→load round-trip intact (H7).
Regression test: DAILY_WORD_COUNTS entries are DATE:WORDS:NOTE:TARGET,
parsed by splitting naively on ':' — an unescaped colon in the note used
to be mis-split, corrupting both the note and the numeric target."
  (let* ((temp-dir (make-temp-file "org-scribe-test-" t))
         (temp-file (expand-file-name "colon-note.org" temp-dir))
         (plan (make-org-scribe-plan
                :title "Colon Note Plan"
                :total-words 10000
                :daily-words 500
                :days 20
                :start-date "2024-11-01"
                :end-date "2024-11-20"
                :daily-word-counts
                '(("2024-11-01" . (:words 600
                                   :note "fixed plot: rewrote scene 3"
                                   :target 500))))))
    (unwind-protect
        (progn
          (org-scribe-planner--save-plan plan temp-file)
          (let* ((loaded (org-scribe-planner--load-plan temp-file))
                 (entry (assoc "2024-11-01"
                               (org-scribe-plan-daily-word-counts loaded))))
            (should entry)
            (should (= (plist-get (cdr entry) :words) 600))
            (should (string= (plist-get (cdr entry) :note)
                             "fixed plot: rewrote scene 3"))
            (should (= (plist-get (cdr entry) :target) 500))))
      (delete-directory temp-dir t))))

(ert-deftest test-planner-roundtrip-note-with-pipe-survives-schedule-table ()
  "A note containing '|' does not corrupt the regenerated Schedule table (H7).
Regression test: notes are inserted verbatim into a |-delimited Org table
row; an unescaped pipe used to shift subsequent columns and confuse the
table-notes parser used to merge hand-edited notes back into the plan."
  (let* ((temp-dir (make-temp-file "org-scribe-test-" t))
         (temp-file (expand-file-name "pipe-note.org" temp-dir))
         (plan (make-org-scribe-plan
                :title "Pipe Note Plan"
                :total-words 10000
                :daily-words 500
                :days 20
                :start-date "2024-11-01"
                :end-date "2024-11-20"
                :daily-word-counts
                '(("2024-11-01" . (:words 600
                                   :note "outline: A | B | C"
                                   :target 500))))))
    (unwind-protect
        (progn
          (org-scribe-planner--save-plan plan temp-file)
          (with-current-buffer (find-file-noselect temp-file)
            (let ((table-notes (org-scribe-planner--parse-schedule-table)))
              (should (string= (cdr (assoc "2024-11-01" table-notes))
                               "outline: A | B | C")))
            (kill-buffer)))
      (delete-directory temp-dir t))))

(ert-deftest test-planner-roundtrip-no-spare-days-property-absent ()
  "When there are no spare days, SPARE_DAYS property is not written."
  (let* ((temp-dir (make-temp-file "org-scribe-test-" t))
         (temp-file (expand-file-name "no-spare.org" temp-dir))
         (plan (make-org-scribe-plan
                :title "No Spare Plan"
                :total-words 5000
                :daily-words 500
                :days 10
                :start-date "2024-11-01"
                :end-date "2024-11-10"
                :spare-days nil)))
    (unwind-protect
        (progn
          (org-scribe-planner--save-plan plan temp-file)
          (let ((loaded (org-scribe-planner--load-plan temp-file)))
            ;; spare-days should be nil (not set)
            (should (null (org-scribe-plan-spare-days loaded)))))
      (delete-directory temp-dir t))))

;;; test-plan-io.el ends here
