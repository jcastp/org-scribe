;;; test-planner-preexisting.el --- Tests for pre-existing words -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for PREEXISTING_WORDS: words in the manuscript before the plan
;; started.  TOTAL_WORDS is the full goal, these included; the schedule
;; covers what is left; expected words and the ledger's first delta start
;; from them; pace and best day exclude them.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-scribe-planner)
(require 'org-scribe-planner-dashboards)

;;; Helpers

(defun test-pre--day (offset)
  "Return the date OFFSET days from today, as YYYY-MM-DD."
  (org-scribe-planner--add-days (org-scribe-planner--get-today-date) offset))

(defun test-pre--answers (&rest values)
  "Return a function that returns VALUES one by one, ignoring its arguments."
  (lambda (&rest _) (pop values)))

(defmacro test-pre--with-temp-file (file-var &rest body)
  "Bind FILE-VAR to a fresh .org path in a temp directory around BODY."
  (declare (indent 1))
  (let ((dir (make-symbol "dir")))
    `(let* ((,dir (make-temp-file "org-scribe-pre-" t))
            (,file-var (expand-file-name "plan.org" ,dir)))
       (unwind-protect (progn ,@body)
         (delete-directory ,dir t)))))

(defmacro test-pre--without-io (&rest body)
  "Run BODY with plan saving, calendar display and load hooks stubbed out."
  (declare (indent 0))
  `(let ((org-scribe-planner-after-plan-load-hook nil)
         (org-scribe-planner--current-plan nil)
         (org-scribe-planner--current-plan-file nil))
     (cl-letf (((symbol-function 'org-scribe-planner--save-plan) #'ignore)
               ((symbol-function 'org-scribe-planner-show-calendar) #'ignore))
       ,@body)))

(defun test-pre--ledger ()
  "A 20,000-word manuscript, then +1,000 and +800 words on two days."
  (list (cons "2026-11-02" (list :words 21000 :note ""))
        (cons "2026-11-03" (list :words 21800 :note ""))))

;;; The ledger's first delta

(ert-deftest test-planner-pre-first-delta-measured-against-baseline ()
  "The earliest entry's delta is the words written that day, not the manuscript."
  (should (equal (org-scribe-planner--daily-deltas (test-pre--ledger) 20000)
                 '(("2026-11-02" . 1000) ("2026-11-03" . 800))))
  (should (= (org-scribe-planner--entry-delta (test-pre--ledger) "2026-11-02" 20000)
             1000))
  (should (= (org-scribe-planner--previous-cumulative-total
              (test-pre--ledger) "2026-11-02" 20000)
             20000)))

(ert-deftest test-planner-pre-baseline-defaults-to-zero ()
  "Without a baseline the ledger behaves as before."
  (should (= (org-scribe-planner--entry-delta (test-pre--ledger) "2026-11-02")
             21000)))

;;; Calculator: the schedule covers only the words left

(ert-deftest test-planner-pre-calc-daily-covers-words-left ()
  "30,000-word goal, 20,000 written, 10 days: 1,000 words a day."
  (let ((plan (make-org-scribe-plan :total-words 30000 :days 10
                                    :preexisting-words 20000)))
    (org-scribe-planner--calculate-missing-variable plan)
    (should (= (org-scribe-plan-daily-words plan) 1000))))

(ert-deftest test-planner-pre-calc-days-cover-words-left ()
  "30,000-word goal, 20,000 written, 1,000 a day: 10 days."
  (let ((plan (make-org-scribe-plan :total-words 30000 :daily-words 1000
                                    :preexisting-words 20000)))
    (org-scribe-planner--calculate-missing-variable plan)
    (should (= (org-scribe-plan-days plan) 10))))

(ert-deftest test-planner-pre-calc-total-includes-preexisting ()
  "The total for a daily target and days is the full goal."
  (let ((plan (make-org-scribe-plan :daily-words 1000 :days 10
                                    :preexisting-words 20000)))
    (org-scribe-planner--calculate-missing-variable plan)
    (should (= (org-scribe-plan-total-words plan) 30000))))

;;; Schedule and expected words

(ert-deftest test-planner-pre-schedule-cumulative-starts-at-preexisting ()
  ":cumulative is where the manuscript should stand: pre-existing + goals."
  (let* ((plan (make-org-scribe-plan :total-words 23000 :daily-words 1000
                                     :start-date "2026-11-01" :end-date "2026-11-03"
                                     :days 3 :preexisting-words 20000))
         (schedule (org-scribe-planner--generate-day-schedule plan)))
    (should (equal (mapcar (lambda (d) (plist-get d :cumulative)) schedule)
                   '(21000 22000 23000)))))

(ert-deftest test-planner-pre-schedule-cache-sees-preexisting ()
  "Two plans differing only in pre-existing words get different schedules."
  (let ((a (make-org-scribe-plan :daily-words 1000 :start-date "2026-11-01"
                                 :end-date "2026-11-02" :days 2 :preexisting-words 0))
        (b (make-org-scribe-plan :daily-words 1000 :start-date "2026-11-01"
                                 :end-date "2026-11-02" :days 2 :preexisting-words 500)))
    (org-scribe-planner--generate-day-schedule a)
    (should (= (plist-get (car (org-scribe-planner--generate-day-schedule b)) :cumulative)
               1500))))

(ert-deftest test-planner-pre-schedule-position-on-track ()
  "Two days met on top of a 20,000-word manuscript is exactly on track.
Before, expected counted from 0 while CURRENT_WORDS included the
manuscript, so the plan looked 20,000 words ahead."
  (let* ((plan (make-org-scribe-plan :total-words 30000 :daily-words 1000
                                     :start-date (test-pre--day -2)
                                     :end-date (test-pre--day 7) :days 10
                                     :preexisting-words 20000
                                     :current-words 22000))
         (position (org-scribe-planner--calculate-schedule-position plan)))
    (should (= (plist-get position :expected-by-today) 22000))
    (should (= (plist-get position :words-ahead) 0))))

(ert-deftest test-planner-pre-progress-report-on-track ()
  "The progress message measures from the pre-existing words too."
  (let ((plan (make-org-scribe-plan :total-words 30000 :daily-words 1000
                                    :start-date (test-pre--day -2)
                                    :end-date (test-pre--day 7) :days 10
                                    :preexisting-words 20000
                                    :current-words 21500))
        (shown nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
      (org-scribe-planner--show-progress-report plan))
    (should (string-match-p "Behind: -500 words" shown))))

;;; Pace and best day

(ert-deftest test-planner-pre-velocity-excludes-preexisting ()
  "Average pace counts only words written in the plan.
Regression: a 20,000-word manuscript then +1,000 and +800 gave an
average of 7,267 words a day."
  (let ((plan (make-org-scribe-plan :total-words 80000 :daily-words 1000
                                    :start-date "2026-11-02" :end-date "2026-12-31"
                                    :days 60 :preexisting-words 20000
                                    :current-words 21800
                                    :daily-word-counts (test-pre--ledger))))
    (should (= (plist-get (org-scribe-planner--calculate-velocity plan) :average)
               900.0))))

(ert-deftest test-planner-pre-best-day-is-a-real-day ()
  "The best day is the best day's writing, not the manuscript."
  (let* ((plan (make-org-scribe-plan :preexisting-words 20000
                                     :daily-word-counts (test-pre--ledger)))
         (deltas (mapcar #'cdr (org-scribe-planner--daily-deltas
                                (org-scribe-plan-daily-word-counts plan)
                                (org-scribe-plan-preexisting-words plan)))))
    (should (= (apply #'max deltas) 1000))))

;;; Milestones

(ert-deftest test-planner-pre-milestone-reached-before-plan ()
  "Milestones the pre-existing words already reach are reached, with no date."
  (let* ((plan (make-org-scribe-plan :total-words 30000 :daily-words 1000
                                     :start-date "2026-11-01" :end-date "2026-11-10"
                                     :days 10 :preexisting-words 20000
                                     :current-words 20000))
         (milestones (org-scribe-planner--get-enhanced-milestones plan)))
    ;; 25% (7,500) and 50% (15,000) are reached before the plan
    (should (plist-get (nth 0 milestones) :reached))
    (should (plist-get (nth 1 milestones) :reached))
    (should-not (plist-get (nth 1 milestones) :date))
    ;; 75% (22,500) falls on the third day: 20,000 + 3 × 1,000
    (should-not (plist-get (nth 2 milestones) :reached))
    (should (string= (plist-get (nth 2 milestones) :expected) "2026-11-03"))))

;;; Mode line and automatic tracking

(ert-deftest test-planner-pre-mode-line-first-sync-is-todays-words ()
  "The first sync of a plan shows the words written today, not the manuscript."
  (let ((org-scribe-planner--current-plan
         (make-org-scribe-plan :total-words 30000 :daily-words 1000
                               :start-date (test-pre--day 0)
                               :end-date (test-pre--day 9) :days 10
                               :preexisting-words 20000
                               :daily-word-counts
                               (list (cons (test-pre--day 0) (list :words 20400 :note ""))))))
    (cl-letf (((symbol-function 'org-scribe-planner-gate) (lambda (&rest _) 'yes)))
      (should (string= (org-scribe-planner--mode-line) " [W:400/1000]")))))

(ert-deftest test-planner-pre-record-total-reports-net-words ()
  "The progress hook gets the net words of the first sync."
  (let* ((plan (make-org-scribe-plan :total-words 30000 :daily-words 1000
                                     :start-date (test-pre--day 0)
                                     :end-date (test-pre--day 9) :days 10
                                     :preexisting-words 20000 :current-words 20000))
         (org-scribe-planner--current-plan plan)
         (org-scribe-planner--current-plan-file "/tmp/unused-plan.org")
         (org-scribe-planner-auto-push-wordcount t)
         (org-scribe-planner-auto-track-daily t)
         (reported nil)
         (org-scribe-planner-after-progress-update-hook
          (list (lambda (_plan delta _date) (setq reported delta)))))
    (cl-letf (((symbol-function 'org-scribe-planner--ensure-plan-for-current-project) #'ignore)
              ((symbol-function 'org-scribe-planner--save-plan) #'ignore))
      (org-scribe-planner-record-total 20350))
    (should (= reported 350))))

;;; Saving, loading, migrating

(ert-deftest test-planner-pre-roundtrip ()
  "PREEXISTING_WORDS survives a save→load round-trip."
  (test-pre--with-temp-file file
    (org-scribe-planner--save-plan
     (make-org-scribe-plan :title "Pre" :total-words 30000 :daily-words 1000
                           :days 10 :start-date "2026-11-01" :end-date "2026-11-10"
                           :preexisting-words 20000 :current-words 20000)
     file)
    (should (= (org-scribe-plan-preexisting-words (org-scribe-planner--load-plan file))
               20000))))

(ert-deftest test-planner-pre-old-plan-loads-with-zero ()
  "A plan file without PREEXISTING_WORDS loads with 0."
  (test-pre--with-temp-file file
    (with-temp-file file
      (insert "* Old\n:PROPERTIES:\n:TOTAL_WORDS: 10000\n:DAILY_WORDS: 1000\n"
              ":DAYS: 10\n:START_DATE: 2026-11-01\n:END_DATE: 2026-11-10\n"
              ":CURRENT_WORDS: 1000\n:DAILY_WORD_COUNTS: 2026-11-01:1000\n:END:\n"))
    (let ((plan (org-scribe-planner--load-plan file)))
      (should (= (org-scribe-plan-preexisting-words plan) 0))
      (should (= (length (org-scribe-plan-daily-word-counts plan)) 1)))))

(ert-deftest test-planner-pre-migrates-existing-manuscript-entry ()
  "An onboarded plan's \"Existing manuscript\" entry becomes pre-existing words."
  (test-pre--with-temp-file file
    (with-temp-file file
      (insert "* Onboarded\n:PROPERTIES:\n:TOTAL_WORDS: 80000\n:DAILY_WORDS: 1000\n"
              ":DAYS: 60\n:START_DATE: 2026-11-01\n:END_DATE: 2026-12-30\n"
              ":CURRENT_WORDS: 21800\n"
              ":DAILY_WORD_COUNTS: 2026-11-01:20000:Existing manuscript,"
              "2026-11-02:21000,2026-11-03:21800\n:END:\n"))
    (let ((plan (org-scribe-planner--load-plan file)))
      (should (= (org-scribe-plan-preexisting-words plan) 20000))
      (should-not (assoc "2026-11-01" (org-scribe-plan-daily-word-counts plan)))
      (should (= (org-scribe-planner--entry-delta
                  (org-scribe-plan-daily-word-counts plan) "2026-11-02"
                  (org-scribe-plan-preexisting-words plan))
                 1000)))))

(ert-deftest test-planner-pre-no-migration-when-property-present ()
  "With PREEXISTING_WORDS set, a ledger entry with that note is left alone."
  (test-pre--with-temp-file file
    (with-temp-file file
      (insert "* P\n:PROPERTIES:\n:TOTAL_WORDS: 80000\n:DAILY_WORDS: 1000\n"
              ":DAYS: 60\n:START_DATE: 2026-11-01\n:END_DATE: 2026-12-30\n"
              ":CURRENT_WORDS: 20500\n:PREEXISTING_WORDS: 0\n"
              ":DAILY_WORD_COUNTS: 2026-11-01:20500:Existing manuscript\n:END:\n"))
    (let ((plan (org-scribe-planner--load-plan file)))
      (should (= (org-scribe-plan-preexisting-words plan) 0))
      (should (assoc "2026-11-01" (org-scribe-plan-daily-word-counts plan))))))

;;; Creating plans

(ert-deftest test-planner-pre-onboard-starts-today-without-ledger-entry ()
  "Onboarding: the plan starts today and the manuscript is pre-existing words."
  (test-pre--without-io
    (let ((org-scribe-planner-wordcount-function (lambda () 20000))
          (asked-start-default nil))
      (cl-letf (((symbol-function 'org-scribe-planner--read-positive-number)
                 ;; target, words per day
                 (test-pre--answers 30000 1000))
                ;; No words outside the manuscript
                ((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) 0))
                ((symbol-function 'read-string) (lambda (&rest _) "P"))
                ((symbol-function 'org-scribe-planner--read-date)
                 (lambda (_prompt &optional default _allow-empty)
                   (setq asked-start-default default)
                   default))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "Daily words → calculate end date"))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                ((symbol-function 'read-file-name)
                 (lambda (&rest _) "/tmp/unused-plan.org"))
                ((symbol-function 'make-directory) #'ignore))
        (org-scribe-planner-onboard-existing-project))
      (let ((plan org-scribe-planner--current-plan))
        (should (string= asked-start-default (test-pre--day 0)))
        (should (string= (org-scribe-plan-start-date plan) (test-pre--day 0)))
        (should (= (org-scribe-plan-preexisting-words plan) 20000))
        (should (= (org-scribe-plan-current-words plan) 20000))
        (should-not (org-scribe-plan-daily-word-counts plan))
        ;; 10,000 words left at 1,000 a day: today and the next 9 days
        (should (string= (org-scribe-plan-end-date plan) (test-pre--day 9)))))))

(ert-deftest test-planner-pre-new-plan-offers-manuscript-count ()
  "New plan in a project: the manuscript's count is taken as it is, words
elsewhere are asked for (none by default), and the daily target covers
only the words left."
  (test-pre--without-io
    (let ((org-scribe-planner-wordcount-function (lambda () 20000))
          (offered nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Total words + Days → Calculate daily words"))
                ((symbol-function 'org-scribe-planner--read-positive-number)
                 (lambda (&rest _) 30000))
                ((symbol-function 'org-scribe-planner--read-days)
                 (lambda (&rest _) 10))
                ((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (_prompt &optional default) (setq offered default) default))
                ((symbol-function 'org-scribe-planner--read-date)
                 (lambda (&rest _) "2026-11-01"))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (org-scribe-planner-new-plan "P" "/tmp/unused-plan.org"))
      (let ((plan org-scribe-planner--current-plan))
        ;; The only number asked is the words elsewhere, 0 by default
        (should (= offered 0))
        (should (= (org-scribe-plan-preexisting-words plan) 20000))
        (should (= (org-scribe-plan-current-words plan) 20000))
        (should (= (org-scribe-plan-daily-words plan) 1000))))))

(ert-deftest test-planner-pre-new-plan-refuses-goal-already-reached ()
  "Pre-existing words that already reach the goal stop the wizard."
  (test-pre--without-io
    (let ((org-scribe-planner-wordcount-function nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Total words + Days → Calculate daily words"))
                ((symbol-function 'org-scribe-planner--read-positive-number)
                 (lambda (&rest _) 30000))
                ((symbol-function 'org-scribe-planner--read-days)
                 (lambda (&rest _) 10))
                ((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) 30000)))
        (should-error (org-scribe-planner-new-plan "P" "/tmp/unused-plan.org")
                      :type 'user-error)))))

(provide 'test-planner-preexisting)
;;; test-planner-preexisting.el ends here
