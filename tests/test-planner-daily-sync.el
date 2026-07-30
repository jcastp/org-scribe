;;; test-planner-daily-sync.el --- Tests for automatic daily word-count tracking -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `org-scribe-planner-record-total' — the function that records
;; the manuscript's cumulative word total for today in DAILY_WORD_COUNTS
;; (Phase 6b: single cumulative ledger, no separate sync baseline).  Net
;; daily deltas are derived on demand via `org-scribe-planner--entry-delta',
;; not stored directly.

;;; Code:

(require 'ert)
(require 'org-scribe-planner)

;;; Helpers

(defmacro test-daily-sync--with-plan (plan-var file-var today-var &rest body)
  "Run BODY with PLAN-VAR / FILE-VAR bound to a temporary plan.
TODAY-VAR is let-bound to a mutable cons whose car is the date string
returned by a stubbed `org-scribe-planner--get-today-date'.
Stubs `org-scribe-planner--auto-load-plan' so it never performs I/O.
Cleans up the temp file unconditionally."
  (declare (indent 3))
  `(let* ((,file-var (make-temp-file "test-daily-sync-" nil ".org"))
          (,plan-var (make-org-scribe-plan
                      :title "Sync Test Plan"
                      :total-words 10000
                      :daily-words 500
                      :days 20
                      :start-date "2026-01-01"
                      :end-date "2026-01-20"
                      :current-words 0))
          (,today-var (cons "2026-06-28" nil)) ; mutable date holder
          (org-scribe-planner--current-plan ,plan-var)
          (org-scribe-planner--current-plan-file ,file-var)
          (org-scribe-planner-after-progress-update-hook nil)
          (org-scribe-planner-auto-track-daily t)
          (org-scribe-planner-auto-push-wordcount t))
     (unwind-protect
         (progn
           (org-scribe-planner--save-plan ,plan-var ,file-var)
           (cl-letf (((symbol-function 'org-scribe-planner--get-today-date)
                      (lambda () (car ,today-var)))
                     ;; Never actually do I/O for auto-load
                     ((symbol-function 'org-scribe-planner--auto-load-plan)
                      (lambda () nil))
                     ;; Report the plan's own directory as the current
                     ;; project root, so the M9 cross-project guard in
                     ;; `org-scribe-planner--ensure-plan-for-current-project'
                     ;; sees the active plan as belonging to "this project"
                     ;; and doesn't reset it out from under these tests,
                     ;; which are testing sync logic, not project detection.
                     ((symbol-function 'org-scribe-project-root)
                      (lambda () (file-name-directory ,file-var))))
             ,@body))
       (ignore-errors (delete-file ,file-var)))))

;;; First call records the cumulative total as-is

(ert-deftest test-planner-daily-sync-first-call-records-cumulative-total ()
  "First call creates today's entry holding the raw cumulative total."
  (test-daily-sync--with-plan plan file today-cell
    (setcar today-cell "2026-06-28")
    (org-scribe-planner-record-total 1000)
    (let ((p org-scribe-planner--current-plan))
      (let ((entry (assoc "2026-06-28" (org-scribe-plan-daily-word-counts p))))
        (should entry)
        (should (= (plist-get (cdr entry) :words) 1000))
        ;; Delta for the first-ever entry equals its own cumulative total
        ;; (implicit starting count is 0).
        (should (= (org-scribe-planner--entry-delta
                    (org-scribe-plan-daily-word-counts p) "2026-06-28")
                   1000)))
      (should (= (org-scribe-plan-current-words p) 1000)))))

;;; Repeated calls the same day update the entry in place

(ert-deftest test-planner-daily-sync-same-day-updates-entry-in-place ()
  "Subsequent calls the same day overwrite the cumulative total, not add to it."
  (test-daily-sync--with-plan plan file today-cell
    (setcar today-cell "2026-06-28")
    (org-scribe-planner-record-total 1000)
    (org-scribe-planner-record-total 1500)
    (let* ((p org-scribe-planner--current-plan)
           (daily-counts (org-scribe-plan-daily-word-counts p)))
      (should (= (plist-get (cdr (assoc "2026-06-28" daily-counts)) :words) 1500))
      (should (= (org-scribe-planner--entry-delta daily-counts "2026-06-28") 1500)))
    (org-scribe-planner-record-total 1800)
    (let* ((p org-scribe-planner--current-plan)
           (daily-counts (org-scribe-plan-daily-word-counts p)))
      (should (= (plist-get (cdr (assoc "2026-06-28" daily-counts)) :words) 1800))
      (should (= (org-scribe-planner--entry-delta daily-counts "2026-06-28") 1800)))))

;;; Day rollover

(ert-deftest test-planner-daily-sync-new-day-creates-new-entry-with-correct-delta ()
  "A new calendar day creates its own entry; its delta derives from
yesterday's cumulative total, no separate baseline to roll forward."
  (test-daily-sync--with-plan plan file today-cell
    ;; Day 1: writer ends the day at 1600 cumulative words
    (setcar today-cell "2026-06-28")
    (org-scribe-planner-record-total 1000)
    (org-scribe-planner-record-total 1600)
    ;; Day 2: total stays flat at first (no words written yet)
    (setcar today-cell "2026-06-29")
    (org-scribe-planner-record-total 1600)
    (let* ((p org-scribe-planner--current-plan)
           (daily-counts (org-scribe-plan-daily-word-counts p)))
      ;; Day-1 entry preserved, delta unaffected by day 2
      (should (= (org-scribe-planner--entry-delta daily-counts "2026-06-28") 1600))
      ;; Day-2 entry exists with delta 0 (no change from day 1's cumulative)
      (should (= (plist-get (cdr (assoc "2026-06-29" daily-counts)) :words) 1600))
      (should (= (org-scribe-planner--entry-delta daily-counts "2026-06-29") 0))
      ;; Writer adds 400 words on day 2
      (org-scribe-planner-record-total 2000)
      (let ((daily-counts (org-scribe-plan-daily-word-counts
                           org-scribe-planner--current-plan)))
        (should (= (org-scribe-planner--entry-delta daily-counts "2026-06-29") 400))))))

(ert-deftest test-planner-daily-sync-credits-words-written-before-first-sync-of-day ()
  "A single end-of-day sync must credit all words written that day, not 0.
Regression test for H4, still valid under the cumulative model: yesterday's
cumulative total is the delta baseline regardless of how many times (if
any) today was synced before this call."
  (test-daily-sync--with-plan plan file today-cell
    ;; Day 1 baseline established at 1000 words.
    (setcar today-cell "2026-06-28")
    (org-scribe-planner-record-total 1000)
    ;; Day 2: writer writes 500 words across the day, then syncs once, at
    ;; the end of the session — no earlier sync that day.
    (setcar today-cell "2026-06-29")
    (org-scribe-planner-record-total 1500)
    (let* ((p org-scribe-planner--current-plan)
           (daily-counts (org-scribe-plan-daily-word-counts p)))
      (should (= (org-scribe-planner--entry-delta daily-counts "2026-06-29") 500))
      (should (= (org-scribe-plan-current-words p) 1500)))))

;;; Idempotency

(ert-deftest test-planner-daily-sync-idempotent-same-total ()
  "Calling record-total multiple times with the same total is a stable no-change."
  (test-daily-sync--with-plan plan file today-cell
    (setcar today-cell "2026-06-28")
    (org-scribe-planner-record-total 1200)
    (org-scribe-planner-record-total 1200)
    (org-scribe-planner-record-total 1200)
    (let* ((p org-scribe-planner--current-plan)
           (counts (org-scribe-plan-daily-word-counts p))
           (today-entries (cl-remove-if-not
                           (lambda (e) (string= (car e) "2026-06-28"))
                           counts)))
      (should (= (length today-entries) 1))
      (should (= (plist-get (cdr (car today-entries)) :words) 1200)))))

;;; Negative delta (net deletions)

(ert-deftest test-planner-daily-sync-records-negative-delta ()
  "Net deletions produce a negative delta — revisions are tracked faithfully."
  (test-daily-sync--with-plan plan file today-cell
    ;; Seed at 2000
    (setcar today-cell "2026-06-28")
    (org-scribe-planner-record-total 2000)
    ;; New day; heavy revision drops the manuscript to 1700
    (setcar today-cell "2026-06-29")
    (org-scribe-planner-record-total 1700)
    (let* ((daily-counts (org-scribe-plan-daily-word-counts
                          org-scribe-planner--current-plan)))
      (should (= (org-scribe-planner--entry-delta daily-counts "2026-06-29") -300)))))

;;; Note preservation

(ert-deftest test-planner-daily-sync-preserves-existing-note ()
  "An existing note on today's entry is kept when the word count updates."
  (test-daily-sync--with-plan plan file today-cell
    (setcar today-cell "2026-06-28")
    ;; Manually add a note for today alongside an existing cumulative total
    (setf (org-scribe-plan-daily-word-counts org-scribe-planner--current-plan)
          (list (cons "2026-06-28" (list :words 1000 :note "Caffeinated sprint"))))
    (setf (org-scribe-plan-current-words org-scribe-planner--current-plan) 1000)
    ;; Now the manuscript is at 1400
    (org-scribe-planner-record-total 1400)
    (let* ((entry (assoc "2026-06-28"
                         (org-scribe-plan-daily-word-counts
                          org-scribe-planner--current-plan))))
      (should (= (plist-get (cdr entry) :words) 1400))
      (should (string= (plist-get (cdr entry) :note) "Caffeinated sprint")))))

;;; No-op conditions

(ert-deftest test-planner-daily-sync-noop-when-disabled ()
  "record-total is a no-op when `org-scribe-planner-auto-track-daily' is nil."
  (test-daily-sync--with-plan plan file today-cell
    (setcar today-cell "2026-06-28")
    (let ((org-scribe-planner-auto-track-daily nil))
      (org-scribe-planner-record-total 1000))
    (let ((p org-scribe-planner--current-plan))
      (should (null (org-scribe-plan-daily-word-counts p))))))

(ert-deftest test-planner-daily-sync-noop-when-auto-push-disabled ()
  "record-total is a no-op when `org-scribe-planner-auto-push-wordcount' is nil."
  (test-daily-sync--with-plan plan file today-cell
    (setcar today-cell "2026-06-28")
    (let ((org-scribe-planner-auto-push-wordcount nil))
      (org-scribe-planner-record-total 1000))
    (let ((p org-scribe-planner--current-plan))
      (should (null (org-scribe-plan-daily-word-counts p))))))

(ert-deftest test-planner-daily-sync-noop-when-zero-total ()
  "record-total ignores a zero word count (guards against an uninitialized manuscript)."
  (test-daily-sync--with-plan plan file today-cell
    (setcar today-cell "2026-06-28")
    (org-scribe-planner-record-total 0)
    (let ((p org-scribe-planner--current-plan))
      (should (null (org-scribe-plan-daily-word-counts p))))))

;;; Plan created on day 1

(ert-deftest test-planner-daily-sync-counts-all-words-when-plan-starts-today ()
  "All words count toward today's cumulative entry when a plan is created
the same day, including any words already in the manuscript at creation
time — there is no baseline to under-credit them against."
  (test-daily-sync--with-plan plan file today-cell
    (setcar today-cell "2026-06-28")
    ;; Writer had 9 words at plan creation, then added 14 more → total 23.
    (org-scribe-planner-record-total 23)
    (let* ((p org-scribe-planner--current-plan)
           (entry (assoc "2026-06-28" (org-scribe-plan-daily-word-counts p))))
      (should entry)
      (should (= (plist-get (cdr entry) :words) 23)))))

;;; Persistence

(ert-deftest test-planner-daily-sync-saves-daily-entry-to-file ()
  "The cumulative daily entry round-trips through save / load."
  (test-daily-sync--with-plan plan file today-cell
    (setcar today-cell "2026-06-28")
    (org-scribe-planner-record-total 1000)
    (org-scribe-planner-record-total 1700)
    (let* ((reloaded (org-scribe-planner--load-plan file))
           (entry (assoc "2026-06-28"
                         (org-scribe-plan-daily-word-counts reloaded))))
      (should entry)
      (should (= (plist-get (cdr entry) :words) 1700)))))

;;; Hook fires

(ert-deftest test-planner-daily-sync-fires-after-progress-hook ()
  "after-progress-update-hook is called with (plan delta today)."
  (test-daily-sync--with-plan plan file today-cell
    (let* ((hook-args nil)
           (org-scribe-planner-after-progress-update-hook
            (list (lambda (p d date) (setq hook-args (list p d date))))))
      (setcar today-cell "2026-06-28")
      (org-scribe-planner-record-total 1000)
      (org-scribe-planner-record-total 1800)
      (should hook-args)
      (should (org-scribe-plan-p (nth 0 hook-args)))
      (should (= (nth 1 hook-args) 1800))
      (should (string= (nth 2 hook-args) "2026-06-28")))))

;;; current-words update

(ert-deftest test-planner-daily-sync-updates-current-words ()
  "current-words is set to the latest manuscript total after each call."
  (test-daily-sync--with-plan plan file today-cell
    (setcar today-cell "2026-06-28")
    (org-scribe-planner-record-total 1000)
    (should (= (org-scribe-plan-current-words org-scribe-planner--current-plan) 1000))
    (org-scribe-planner-record-total 1600)
    (should (= (org-scribe-plan-current-words org-scribe-planner--current-plan) 1600))))

;;; Cross-project guard (M9)

(ert-deftest test-planner-daily-sync-skips-when-plan-belongs-to-different-project ()
  "record-total must not push counts into a plan left active from another project.
Regression (M9): switching projects while a different project's plan was
still active silently wrote the new project's manuscript totals into the
old project's plan file, corrupting it (the auto-load hook only fires when
no plan at all is active).  When no plan can be found for the current
project either, syncing must skip entirely rather than fall back to the
mismatched plan."
  (test-daily-sync--with-plan plan file today-cell
    (setcar today-cell "2026-06-28")
    (cl-letf (((symbol-function 'org-scribe-project-root)
               (lambda () "/tmp/some-other-project/"))
              ((symbol-function 'org-scribe-planner--auto-load-plan)
               (lambda () nil)))            ; no plan found for this project
      (org-scribe-planner-record-total 1000))
    ;; The mismatched plan was dropped, and nothing was found to replace
    ;; it, so no data was written anywhere.
    (should (null org-scribe-planner--current-plan))
    (should (null org-scribe-planner--current-plan-file))
    (should (null (org-scribe-plan-daily-word-counts plan)))))

(ert-deftest test-planner-daily-sync-switches-to-correct-plan-for-project ()
  "record-total loads and uses the right project's plan instead of a stale one.
Regression (M9): when a plan file exists for the current project too, the
guard swaps to it and syncs there, instead of either corrupting the old
plan or silently doing nothing."
  (test-daily-sync--with-plan plan file today-cell
    (setcar today-cell "2026-06-28")
    (let* ((other-file (make-temp-file "test-daily-sync-other-" nil ".org"))
           (other-plan (make-org-scribe-plan
                        :title "Other Project Plan"
                        :total-words 5000
                        :daily-words 200
                        :days 25
                        :start-date "2026-01-01"
                        :end-date "2026-01-25"
                        :current-words 0)))
      (unwind-protect
          (progn
            (org-scribe-planner--save-plan other-plan other-file)
            (cl-letf (((symbol-function 'org-scribe-project-root)
                       (lambda () "/tmp/some-other-project/"))
                      ((symbol-function 'org-scribe-planner--auto-load-plan)
                       (lambda ()
                         (setq org-scribe-planner--current-plan other-plan)
                         (setq org-scribe-planner--current-plan-file other-file))))
              (org-scribe-planner-record-total 2000))
            ;; The originally active plan (belonging to a different
            ;; project) was left untouched.
            (should (null (org-scribe-plan-daily-word-counts plan)))
            ;; The correct project's plan received the sync instead.
            (should (eq org-scribe-planner--current-plan other-plan))
            (should (= (org-scribe-plan-current-words other-plan) 2000)))
        (ignore-errors (delete-file other-file))))))

(provide 'test-planner-daily-sync)
;;; test-planner-daily-sync.el ends here
