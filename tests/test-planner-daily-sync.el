;;; test-planner-daily-sync.el --- Tests for automatic daily word-count tracking -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `org-scribe-planner--sync-daily-from-manuscript' — the function
;; that converts manuscript word counts into DAILY_WORD_COUNTS deltas.

;;; Code:

(require 'ert)
(require 'org-scribe-planner)

;;; Helpers

(defmacro test-daily-sync--with-plan (plan-var file-var total-var today-var &rest body)
  "Run BODY with PLAN-VAR / FILE-VAR bound to a temporary plan.
TOTAL-VAR is let-bound to a mutable cons whose car is the word count
returned by a stubbed `org-scribe-planner-wordcount-function'.
TODAY-VAR is let-bound to a mutable cons whose car is the date string
returned by a stubbed `org-scribe-planner--get-today-date'.
Stubs `org-scribe-planner-auto-load-plan' so it never performs I/O.
Cleans up the temp file unconditionally."
  (declare (indent 4))
  `(let* ((,file-var (make-temp-file "test-daily-sync-" nil ".org"))
          (,plan-var (make-org-scribe-plan
                      :title "Sync Test Plan"
                      :total-words 10000
                      :daily-words 500
                      :days 20
                      :start-date "2026-01-01"
                      :end-date "2026-01-20"
                      :current-words 0))
          (,total-var (cons 1000 nil))   ; mutable word-count holder
          (,today-var (cons "2026-06-28" nil)) ; mutable date holder
          (org-scribe-planner--current-plan ,plan-var)
          (org-scribe-planner--current-plan-file ,file-var)
          (org-scribe-planner-after-progress-update-hook nil)
          (org-scribe-planner-auto-track-daily t)
          (org-scribe-planner-wordcount-function
           (lambda () (car ,total-var))))
     (unwind-protect
         (progn
           (org-scribe-planner--save-plan ,plan-var ,file-var)
           (cl-letf (((symbol-function 'org-scribe-planner--get-today-date)
                      (lambda () (car ,today-var)))
                     ;; Never actually do I/O for auto-load
                     ((symbol-function 'org-scribe-planner--auto-load-plan)
                      (lambda () nil)))
             ,@body))
       (ignore-errors (delete-file ,file-var)))))

;;; Baseline seeding

(ert-deftest test-planner-daily-sync-seeds-baseline-on-first-call ()
  "First call sets sync-date/sync-words and creates a zero-delta entry."
  (test-daily-sync--with-plan plan file total-cell today-cell
    (setcar total-cell 1000)
    (setcar today-cell "2026-06-28")
    (org-scribe-planner--sync-daily-from-manuscript)
    (let ((p org-scribe-planner--current-plan))
      ;; Baseline persisted
      (should (string= (org-scribe-plan-sync-date p) "2026-06-28"))
      (should (= (org-scribe-plan-sync-words p) 1000))
      ;; Delta is 0 (nothing written yet today)
      (let ((entry (assoc "2026-06-28" (org-scribe-plan-daily-word-counts p))))
        (should entry)
        (should (= (plist-get (cdr entry) :words) 0)))
      ;; current-words updated
      (should (= (org-scribe-plan-current-words p) 1000)))))

;;; Delta accumulation within a day

(ert-deftest test-planner-daily-sync-accumulates-delta-same-day ()
  "Subsequent calls on the same day accumulate the net delta."
  (test-daily-sync--with-plan plan file total-cell today-cell
    (setcar total-cell 1000)
    (setcar today-cell "2026-06-28")
    ;; First call — seed baseline
    (org-scribe-planner--sync-daily-from-manuscript)
    ;; Writer adds 500 words
    (setcar total-cell 1500)
    (org-scribe-planner--sync-daily-from-manuscript)
    (let* ((p org-scribe-planner--current-plan)
           (entry (assoc "2026-06-28" (org-scribe-plan-daily-word-counts p))))
      (should (= (plist-get (cdr entry) :words) 500)))
    ;; Writer adds another 300
    (setcar total-cell 1800)
    (org-scribe-planner--sync-daily-from-manuscript)
    (let* ((p org-scribe-planner--current-plan)
           (entry (assoc "2026-06-28" (org-scribe-plan-daily-word-counts p))))
      (should (= (plist-get (cdr entry) :words) 800)))))

;;; Day rollover

(ert-deftest test-planner-daily-sync-rolls-baseline-on-new-day ()
  "On a new calendar day the baseline rolls to the current total."
  (test-daily-sync--with-plan plan file total-cell today-cell
    ;; Day 1: writer writes 600 words
    (setcar total-cell 1000)
    (setcar today-cell "2026-06-28")
    (org-scribe-planner--sync-daily-from-manuscript)
    (setcar total-cell 1600)
    (org-scribe-planner--sync-daily-from-manuscript)
    ;; Day 2: new day, total starts at 1600
    (setcar today-cell "2026-06-29")
    (setcar total-cell 1600)
    (org-scribe-planner--sync-daily-from-manuscript)
    (let* ((p org-scribe-planner--current-plan))
      ;; Baseline rolled to day 2
      (should (string= (org-scribe-plan-sync-date p) "2026-06-29"))
      (should (= (org-scribe-plan-sync-words p) 1600))
      ;; Day-1 entry preserved
      (let ((day1 (assoc "2026-06-28" (org-scribe-plan-daily-word-counts p))))
        (should (= (plist-get (cdr day1) :words) 600)))
      ;; Day-2 entry starts at 0
      (let ((day2 (assoc "2026-06-29" (org-scribe-plan-daily-word-counts p))))
        (should (= (plist-get (cdr day2) :words) 0)))
      ;; Writer adds 400 words on day 2
      (setcar total-cell 2000)
      (org-scribe-planner--sync-daily-from-manuscript)
      (let ((day2 (assoc "2026-06-29"
                         (org-scribe-plan-daily-word-counts
                          org-scribe-planner--current-plan))))
        (should (= (plist-get (cdr day2) :words) 400))))))

;;; Idempotency

(ert-deftest test-planner-daily-sync-idempotent-same-total ()
  "Calling sync multiple times with the same total produces one entry unchanged."
  (test-daily-sync--with-plan plan file total-cell today-cell
    (setcar total-cell 1200)
    (setcar today-cell "2026-06-28")
    (org-scribe-planner--sync-daily-from-manuscript)
    (org-scribe-planner--sync-daily-from-manuscript)
    (org-scribe-planner--sync-daily-from-manuscript)
    (let* ((p org-scribe-planner--current-plan)
           (counts (org-scribe-plan-daily-word-counts p))
           (today-entries (cl-remove-if-not
                           (lambda (e) (string= (car e) "2026-06-28"))
                           counts)))
      (should (= (length today-entries) 1))
      (should (= (plist-get (cdr (car today-entries)) :words) 0)))))

;;; Negative delta (net deletions)

(ert-deftest test-planner-daily-sync-records-negative-delta ()
  "Net deletions produce a negative daily entry — revisions are tracked faithfully."
  (test-daily-sync--with-plan plan file total-cell today-cell
    ;; Seed at 2000
    (setcar total-cell 2000)
    (setcar today-cell "2026-06-28")
    (org-scribe-planner--sync-daily-from-manuscript)
    ;; Heavy revision: net loss of 300 words
    (setcar total-cell 1700)
    (org-scribe-planner--sync-daily-from-manuscript)
    (let* ((entry (assoc "2026-06-28"
                         (org-scribe-plan-daily-word-counts
                          org-scribe-planner--current-plan))))
      (should (= (plist-get (cdr entry) :words) -300)))))

;;; Note preservation

(ert-deftest test-planner-daily-sync-preserves-existing-note ()
  "An existing note on today's entry is kept when the word count updates."
  (test-daily-sync--with-plan plan file total-cell today-cell
    (setcar total-cell 1000)
    (setcar today-cell "2026-06-28")
    ;; Manually add a note for today
    (setf (org-scribe-plan-daily-word-counts org-scribe-planner--current-plan)
          (list (cons "2026-06-28"
                      (list :words 0 :note "Caffeinated sprint" :sync-date "2026-06-28"))))
    ;; Seed baseline so we're not on a rollover
    (setf (org-scribe-plan-sync-date org-scribe-planner--current-plan) "2026-06-28")
    (setf (org-scribe-plan-sync-words org-scribe-planner--current-plan) 1000)
    ;; Now write 400 words
    (setcar total-cell 1400)
    (org-scribe-planner--sync-daily-from-manuscript)
    (let* ((entry (assoc "2026-06-28"
                         (org-scribe-plan-daily-word-counts
                          org-scribe-planner--current-plan))))
      (should (= (plist-get (cdr entry) :words) 400))
      (should (string= (plist-get (cdr entry) :note) "Caffeinated sprint")))))

;;; No-op conditions

(ert-deftest test-planner-daily-sync-noop-when-disabled ()
  "sync is a no-op when `org-scribe-planner-auto-track-daily' is nil."
  (test-daily-sync--with-plan plan file total-cell today-cell
    (setcar total-cell 1000)
    (setcar today-cell "2026-06-28")
    (let ((org-scribe-planner-auto-track-daily nil))
      (org-scribe-planner--sync-daily-from-manuscript))
    (let ((p org-scribe-planner--current-plan))
      (should (null (org-scribe-plan-sync-date p)))
      (should (null (org-scribe-plan-daily-word-counts p))))))

(ert-deftest test-planner-daily-sync-noop-when-zero-total ()
  "sync ignores a zero word count (guards against uninitialized manuscript)."
  (test-daily-sync--with-plan plan file total-cell today-cell
    (setcar total-cell 0)
    (setcar today-cell "2026-06-28")
    (org-scribe-planner--sync-daily-from-manuscript)
    (let ((p org-scribe-planner--current-plan))
      (should (null (org-scribe-plan-sync-date p)))
      (should (null (org-scribe-plan-daily-word-counts p))))))

(ert-deftest test-planner-daily-sync-noop-when-wordcount-nil ()
  "sync is a no-op when the wordcount function returns nil."
  (test-daily-sync--with-plan plan file total-cell today-cell
    (setcar today-cell "2026-06-28")
    (let ((org-scribe-planner-wordcount-function (lambda () nil)))
      (org-scribe-planner--sync-daily-from-manuscript))
    (let ((p org-scribe-planner--current-plan))
      (should (null (org-scribe-plan-sync-date p))))))

;;; Plan created on day 1 (regression: words before first sync were lost)

(ert-deftest test-planner-daily-sync-counts-all-words-when-plan-starts-today ()
  "All words count toward today when a plan is created on the same day.
Regression: new-plan used to seed sync-words = current-words, so any words
already in the manuscript at plan-creation time were used as the baseline and
never credited to today's daily delta."
  (test-daily-sync--with-plan plan file total-cell today-cell
    ;; Simulate what new-plan now does when start-date == today:
    ;; sync-date is set to today, sync-words is 0 (not current-words).
    (setf (org-scribe-plan-sync-date plan) "2026-06-28")
    (setf (org-scribe-plan-sync-words plan) 0)
    (setq org-scribe-planner--current-plan plan)
    (setcar today-cell "2026-06-28")
    ;; Writer had 9 words at plan creation, then added 14 more → total 23.
    (setcar total-cell 23)
    (org-scribe-planner--sync-daily-from-manuscript)
    (let* ((p org-scribe-planner--current-plan)
           (entry (assoc "2026-06-28" (org-scribe-plan-daily-word-counts p))))
      (should entry)
      ;; All 23 words must be credited, not just the 14 added post-creation.
      (should (= (plist-get (cdr entry) :words) 23)))))

;;; Persistence

(ert-deftest test-planner-daily-sync-saves-sync-slots-to-file ()
  "sync-date and sync-words round-trip through save / load."
  (test-daily-sync--with-plan plan file total-cell today-cell
    (setcar total-cell 1500)
    (setcar today-cell "2026-06-28")
    (org-scribe-planner--sync-daily-from-manuscript)
    ;; Load the file back from disk
    (let ((reloaded (org-scribe-planner--load-plan file)))
      (should (string= (org-scribe-plan-sync-date reloaded) "2026-06-28"))
      (should (= (org-scribe-plan-sync-words reloaded) 1500)))))

(ert-deftest test-planner-daily-sync-saves-daily-entry-to-file ()
  "Daily word delta round-trips through save / load."
  (test-daily-sync--with-plan plan file total-cell today-cell
    ;; Seed then write
    (setcar total-cell 1000)
    (setcar today-cell "2026-06-28")
    (org-scribe-planner--sync-daily-from-manuscript)
    (setcar total-cell 1700)
    (org-scribe-planner--sync-daily-from-manuscript)
    (let* ((reloaded (org-scribe-planner--load-plan file))
           (entry (assoc "2026-06-28"
                         (org-scribe-plan-daily-word-counts reloaded))))
      (should entry)
      (should (= (plist-get (cdr entry) :words) 700)))))

;;; Hook fires

(ert-deftest test-planner-daily-sync-fires-after-progress-hook ()
  "after-progress-update-hook is called with (plan delta today)."
  (test-daily-sync--with-plan plan file total-cell today-cell
    (let* ((hook-args nil)
           (org-scribe-planner-after-progress-update-hook
            (list (lambda (p d date) (setq hook-args (list p d date))))))
      ;; Seed then write 800 words
      (setcar total-cell 1000)
      (setcar today-cell "2026-06-28")
      (org-scribe-planner--sync-daily-from-manuscript)
      (setcar total-cell 1800)
      (org-scribe-planner--sync-daily-from-manuscript)
      (should hook-args)
      (should (org-scribe-plan-p (nth 0 hook-args)))
      (should (= (nth 1 hook-args) 800))
      (should (string= (nth 2 hook-args) "2026-06-28")))))

;;; current-words update

(ert-deftest test-planner-daily-sync-updates-current-words ()
  "current-words is set to the latest manuscript total after each sync."
  (test-daily-sync--with-plan plan file total-cell today-cell
    (setcar total-cell 1000)
    (setcar today-cell "2026-06-28")
    (org-scribe-planner--sync-daily-from-manuscript)
    (should (= (org-scribe-plan-current-words org-scribe-planner--current-plan) 1000))
    (setcar total-cell 1600)
    (org-scribe-planner--sync-daily-from-manuscript)
    (should (= (org-scribe-plan-current-words org-scribe-planner--current-plan) 1600))))

(provide 'test-planner-daily-sync)
;;; test-planner-daily-sync.el ends here
