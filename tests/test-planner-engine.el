;;; test-planner-engine.el --- Tests for the schedule engine -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the schedule engine (`org-scribe-planner--build-schedule' and
;; friends): writing days stored and the end date derived, whole-word
;; goals that add up exactly, locked past goals, the rebase used by the
;; recalculations from progress, and the plan-file properties.  The last
;; section ports wordplanner's invariant tests: seeded random sequences of
;; changes, with today moving forward, checking the schedule after each.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-scribe-planner)

;;; Helpers

(defvar test-engine--today "2026-11-01"
  "The date `org-scribe-planner--get-today-date' returns inside the tests.")

(defmacro test-engine--on (today &rest body)
  "Run BODY with today's date stubbed to TODAY (a YYYY-MM-DD string)."
  (declare (indent 1))
  `(let ((test-engine--today ,today))
     (cl-letf (((symbol-function 'org-scribe-planner--get-today-date)
                (lambda () test-engine--today)))
       ,@body)))

(defmacro test-engine--without-io (&rest body)
  "Run BODY with plan saving and calendar display stubbed out."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'org-scribe-planner--save-plan) #'ignore)
             ((symbol-function 'org-scribe-planner-show-calendar) #'ignore))
     ,@body))

(defun test-engine--plan (&rest args)
  "A plan from Nov 1, 2026: 30,000 words in 30 writing days, then ARGS."
  (let ((plan (apply #'make-org-scribe-plan
                     (append args (list :title "P" :total-words 30000 :days 30
                                        :start-date "2026-11-01")))))
    (org-scribe-planner--calculate-missing-variable plan)
    plan))

(defun test-engine--goals (plan)
  "The whole-word goals of PLAN's days, in order."
  (mapcar (lambda (d) (plist-get d :words))
          (org-scribe-planner--generate-day-schedule plan)))

(defun test-engine--log (plan date total)
  "Record TOTAL as PLAN's manuscript count on DATE."
  (setf (org-scribe-plan-daily-word-counts plan)
        (cons (cons date (list :words total :note ""))
              (assoc-delete-all date (org-scribe-plan-daily-word-counts plan))))
  (setf (org-scribe-plan-current-words plan) total))

;;; Whole-word goals and the derived window

(ert-deftest test-planner-engine-goals-add-up-exactly ()
  "10,000 words over 7 days: whole-word goals that add up to 10,000."
  (let ((plan (make-org-scribe-plan :total-words 10000 :days 7
                                    :start-date "2026-11-01")))
    (org-scribe-planner--calculate-missing-variable plan)
    (let ((goals (test-engine--goals plan)))
      (should (= (apply #'+ goals) 10000))
      (should (cl-every (lambda (g) (memq g '(1428 1429))) goals))
      (should (= (plist-get (car (last (org-scribe-planner--generate-day-schedule plan)))
                            :cumulative)
                 10000)))))

(ert-deftest test-planner-engine-calculate-time-lowers-daily ()
  "10,000 words at 1,500 a day: 7 whole days, the daily target lowered to 1,429.
Regression: the daily target stayed at 1,500 and the plan overshot."
  (let ((plan (make-org-scribe-plan :total-words 10000 :daily-words 1500
                                    :start-date "2026-11-01")))
    (org-scribe-planner--calculate-missing-variable plan)
    (should (= (org-scribe-plan-writing-days plan) 7))
    (should (= (org-scribe-plan-daily-words plan) 1429))
    (should (= (apply #'+ (test-engine--goals plan)) 10000))))

(ert-deftest test-planner-engine-end-date-derived-from-writing-days ()
  "The end date is where the writing days end, spare days skipped."
  (let ((plan (make-org-scribe-plan :total-words 5000 :writing-days 5
                                    :start-date "2026-11-01"
                                    :spare-days '("2026-11-02" "2026-11-04"))))
    (org-scribe-planner--sync-window plan)
    (should (string= (org-scribe-plan-end-date plan) "2026-11-07"))
    (should (= (org-scribe-plan-days plan) 7))
    (should (= (length (org-scribe-planner--generate-day-schedule plan)) 7))))

(ert-deftest test-planner-engine-writing-days-from-old-fields ()
  "A plan without WRITING_DAYS derives them from its dates, spare days out."
  (let ((plan (make-org-scribe-plan :start-date "2026-11-01" :end-date "2026-11-10"
                                    :days 10 :spare-days '("2026-11-05"))))
    (should (= (org-scribe-planner--writing-days plan) 9))))

(ert-deftest test-planner-engine-total-for-daily ()
  "The total for a daily target counts fixed days and pre-existing words."
  (let ((plan (make-org-scribe-plan :total-words 0 :writing-days 10
                                    :start-date "2026-11-01" :preexisting-words 2000
                                    :locked-goals '(("2026-11-01" . 300)))))
    ;; 2,000 + 300 (locked) + 9 open days × 1,000
    (should (= (org-scribe-planner--total-for-daily plan 1000) 11300))))

(ert-deftest test-planner-engine-keep-end-date-spreads-over-fewer-days ()
  "A spare day added before the end date: same end, higher daily target."
  (let ((plan (test-engine--plan)))
    (setf (org-scribe-plan-spare-days plan) '("2026-11-10"))
    (org-scribe-planner--keep-end-date plan "2026-11-30")
    (should (string= (org-scribe-plan-end-date plan) "2026-11-30"))
    (should (= (org-scribe-plan-writing-days plan) 29))
    (should (= (apply #'+ (test-engine--goals plan)) 30000))))

;;; Dates across a change to summer time

(ert-deftest test-planner-engine-date-range-crosses-summer-time ()
  "A range across the last Sunday of March keeps its last day.
Regression: the range stepped by 86,400 seconds from noon; after the
change to summer time that is 13:00, later than the end date's noon, so
the last day was dropped -- every plan crossing it lost its final day."
  (let ((old-zone (getenv "TZ")))
    (unwind-protect
        (progn
          (set-time-zone-rule "CET-1CEST,M3.5.0,M10.5.0/3")
          (let ((range (org-scribe-planner--generate-date-range "2027-03-20" "2027-04-05")))
            (should (= (length range) 17))
            (should (string= (car (last range)) "2027-04-05")))
          (let ((plan (make-org-scribe-plan :total-words 17000 :writing-days 17
                                            :start-date "2027-03-20")))
            (org-scribe-planner--sync-window plan)
            (should (string= (org-scribe-plan-end-date plan) "2027-04-05"))
            (should (= (length (org-scribe-planner--generate-day-schedule plan)) 17))))
      (set-time-zone-rule old-zone))))

;;; Locked past goals

(ert-deftest test-planner-engine-lock-freezes-past-days ()
  "Days before today keep their goals when the total changes."
  (test-engine--on "2026-11-06"
    (let ((plan (test-engine--plan)))
      (should (= (org-scribe-planner--lock-past-goals plan) 5))
      (setf (org-scribe-plan-total-words plan) 45000)
      (org-scribe-planner--sync-daily plan)
      (let ((goals (test-engine--goals plan)))
        ;; Nov 1-5 stay at 1,000; the other 25 days share 40,000
        (should (equal (cl-subseq goals 0 5) '(1000 1000 1000 1000 1000)))
        (should (= (nth 5 goals) 1600))
        (should (= (apply #'+ goals) 45000))))))

(ert-deftest test-planner-engine-lock-is-idempotent ()
  "Locking again locks nothing new and changes nothing."
  (test-engine--on "2026-11-06"
    (let ((plan (test-engine--plan)))
      (org-scribe-planner--lock-past-goals plan)
      (let ((locked (copy-tree (org-scribe-plan-locked-goals plan))))
        (should (= (org-scribe-planner--lock-past-goals plan) 0))
        (should (equal (org-scribe-plan-locked-goals plan) locked))))))

(ert-deftest test-planner-engine-days-never-below-locked ()
  "A daily target that fits in fewer days still keeps the locked days."
  (test-engine--on "2026-11-11"
    (let ((plan (test-engine--plan)))
      (org-scribe-planner--lock-past-goals plan)
      ;; 10 locked days × 1,000; 20,000 left at 20,000 a day: 1 more day
      (org-scribe-planner--calculate-time plan 20000)
      (should (= (org-scribe-plan-writing-days plan) 11)))))

(ert-deftest test-planner-engine-days-available-refuses-fewer-than-locked ()
  "\"Days available\" below the past writing days is refused."
  (test-engine--on "2026-11-11"
    (test-engine--without-io
      (let ((org-scribe-planner--current-plan (test-engine--plan))
            (org-scribe-planner--current-plan-file "/tmp/unused-plan.org"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "Days available"))
                  ((symbol-function 'org-scribe-planner--read-days)
                   (lambda (&rest _) 5)))
          (should-error (org-scribe-planner-recalculate) :type 'user-error))))))

;;; Rebase and the two recalculations

(ert-deftest test-planner-engine-adjust-remaining-locks-past-and-covers-the-rest ()
  "Behind after 10 days: past goals stay, open days cover the words left.
Regression: the new daily target applied to past days too, and the
schedule stopped adding up to the total."
  (test-engine--on "2026-11-11"
    (test-engine--without-io
      (let ((plan (test-engine--plan)))
        (test-engine--log plan "2026-11-10" 5000)
        (org-scribe-planner-recalculate-remaining-days plan "/tmp/unused-plan.org")
        (let ((goals (test-engine--goals plan)))
          (should (equal (cl-subseq goals 0 10) (make-list 10 1000)))
          ;; 25,000 left over the 20 days from today
          (should (= (org-scribe-plan-daily-words plan) 1250))
          (should (string= (org-scribe-plan-end-date plan) "2026-11-30"))
          (should (= (apply #'+ (cl-subseq goals 10)) 25000)))
        ;; On track again: expected through yesterday = what was written
        (should (= (org-scribe-planner--expected-through plan "2026-11-10") 5000))))))

(ert-deftest test-planner-engine-adjust-remaining-starts-tomorrow-when-logged ()
  "Words already logged today: today is locked and the change starts tomorrow."
  (test-engine--on "2026-11-11"
    (test-engine--without-io
      (let ((plan (test-engine--plan)))
        (test-engine--log plan "2026-11-11" 5000)
        (org-scribe-planner-recalculate-remaining-days plan "/tmp/unused-plan.org")
        (should (assoc "2026-11-11" (org-scribe-plan-locked-goals plan)))
        ;; Today is credited with at least its goal (1,000 of the 5,000);
        ;; 25,000 left over the 19 days from tomorrow
        (should (= (apply #'+ (cl-subseq (test-engine--goals plan) 11)) 25000))))))

(ert-deftest test-planner-engine-adjust-remaining-refuses-when-goal-met ()
  "Nothing to recalculate once the goal is reached."
  (test-engine--on "2026-11-11"
    (test-engine--without-io
      (let ((plan (test-engine--plan))
            (shown nil))
        (test-engine--log plan "2026-11-10" 31000)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
          (org-scribe-planner-recalculate-remaining-days plan "/tmp/unused-plan.org"))
        (should (string-match-p "Goal already reached" shown))
        (should-not (org-scribe-plan-locked-goals plan))))))

(ert-deftest test-planner-engine-adjust-end-date-keeps-past-goals ()
  "Moving the end date keeps the past goals and the daily target."
  (test-engine--on "2026-11-11"
    (test-engine--without-io
      (let ((plan (test-engine--plan)))
        (test-engine--log plan "2026-11-10" 25000)
        (org-scribe-planner-recalculate-end-date plan "/tmp/unused-plan.org")
        (should (equal (cl-subseq (test-engine--goals plan) 0 10) (make-list 10 1000)))
        ;; 5,000 left at 1,000 a day: Nov 11-15
        (should (string= (org-scribe-plan-end-date plan) "2026-11-15"))
        (should (= (org-scribe-plan-daily-words plan) 1000))))))

;;; Plan file

(ert-deftest test-planner-engine-roundtrip ()
  "WRITING_DAYS, LOCKED_GOALS and SCHEDULE_ADJUSTMENT survive save→load."
  (let* ((dir (make-temp-file "org-scribe-engine-" t))
         (file (expand-file-name "plan.org" dir)))
    (unwind-protect
        (test-engine--on "2026-11-11"
          (let ((plan (test-engine--plan)))
            (test-engine--log plan "2026-11-10" 5000)
            (org-scribe-planner--rebase-on-progress plan "2026-11-11")
            (org-scribe-planner--save-plan plan file)
            (let ((loaded (org-scribe-planner--load-plan file)))
              (should (= (org-scribe-plan-writing-days loaded) 30))
              (should (equal (org-scribe-plan-locked-goals loaded)
                             (org-scribe-plan-locked-goals plan)))
              (should (= (org-scribe-plan-schedule-adjustment loaded) 5000))
              (should (equal (test-engine--goals loaded) (test-engine--goals plan))))))
      (delete-directory dir t))))

(ert-deftest test-planner-engine-migrates-old-recalculated-daily ()
  "An old file whose recalculation changed DAILY_WORDS keeps that target.
Without the migration the engine would spread the total again and undo
the recalculation."
  (let* ((dir (make-temp-file "org-scribe-engine-" t))
         (file (expand-file-name "plan.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* Old\n:PROPERTIES:\n:TOTAL_WORDS: 30000\n:DAILY_WORDS: 1250\n"
                    ":DAYS: 30\n:START_DATE: 2026-11-01\n:END_DATE: 2026-11-30\n"
                    ":CURRENT_WORDS: 5000\n:END:\n"))
          (let ((plan (org-scribe-planner--load-plan file)))
            (should (= (org-scribe-plan-schedule-adjustment plan) 7500))
            (should (= (org-scribe-plan-daily-words plan) 1250))
            (should (= (plist-get (car (last (org-scribe-planner--generate-day-schedule plan)))
                                  :cumulative)
                       30000))))
      (delete-directory dir t))))

(ert-deftest test-planner-engine-old-rounding-not-migrated ()
  "An old file whose daily target was only rounded up gets no adjustment."
  (let* ((dir (make-temp-file "org-scribe-engine-" t))
         (file (expand-file-name "plan.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* Old\n:PROPERTIES:\n:TOTAL_WORDS: 80000\n:DAILY_WORDS: 728\n"
                    ":DAYS: 110\n:START_DATE: 2026-02-21\n:END_DATE: 2026-06-10\n"
                    ":CURRENT_WORDS: 36\n:END:\n"))
          (let ((plan (org-scribe-planner--load-plan file)))
            (should (= (org-scribe-plan-schedule-adjustment plan) 0))
            (should (= (org-scribe-plan-writing-days plan) 110))
            (should (= (apply #'+ (test-engine--goals plan)) 80000))))
      (delete-directory dir t))))

;;; Onboarding with a past start

(ert-deftest test-planner-engine-onboard-past-start-locks-past-at-zero ()
  "Onboarding with a start date before today: the past days have no goal."
  (test-engine--on "2026-11-11"
    (test-engine--without-io
      (let ((org-scribe-planner-after-plan-load-hook nil)
            (org-scribe-planner--current-plan nil)
            (org-scribe-planner--current-plan-file nil)
            (org-scribe-planner-wordcount-function (lambda () 20000))
            (answers (list 30000)))
        (cl-letf (((symbol-function 'org-scribe-planner--read-positive-number)
                   (lambda (&rest _) (pop answers)))
                  ;; No words outside the manuscript
                  ((symbol-function 'org-scribe-planner--read-non-negative-number)
                   (lambda (&rest _) 0))
                  ((symbol-function 'read-string) (lambda (&rest _) "P"))
                  ((symbol-function 'org-scribe-planner--read-date)
                   (lambda (prompt &rest _)
                     (if (string-match-p "start" prompt) "2026-11-01" "2026-11-20")))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "End date → calculate daily words"))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                  ((symbol-function 'read-file-name) (lambda (&rest _) "/tmp/unused-plan.org"))
                  ((symbol-function 'make-directory) #'ignore))
          (org-scribe-planner-onboard-existing-project))
        (let* ((plan org-scribe-planner--current-plan)
               (goals (test-engine--goals plan)))
          (should (equal (cl-subseq goals 0 10) (make-list 10 0)))
          ;; 10,000 words left over Nov 11-20
          (should (= (org-scribe-plan-daily-words plan) 1000))
          (should (= (apply #'+ goals) 10000)))))))

;;; Invariants under random changes (port of wordplanner's test_invariants)

(defun test-engine--check-invariants (plan before-locked today step)
  "Signal a test failure when PLAN breaks an invariant after STEP.
BEFORE-LOCKED is PLAN's locked goals before the step; TODAY the date."
  (let* ((schedule (org-scribe-planner--generate-day-schedule plan))
         (engine (org-scribe-planner--build-schedule plan))
         (preexisting (org-scribe-plan-preexisting-words plan))
         (adjustment (org-scribe-plan-schedule-adjustment plan))
         (to-write (- (org-scribe-plan-total-words plan) preexisting))
         (exact (apply #'+ (mapcar (lambda (r) (plist-get (cdr r) :goal))
                                   (plist-get engine :rows))))
         (label (format "step %s" step)))
    ;; 1. Locked goals never change or disappear
    (dolist (lock before-locked)
      (should (equal (list label (cdr (assoc (car lock) (org-scribe-plan-locked-goals plan))))
                     (list label (cdr lock)))))
    ;; 2. With open days left to absorb it, the schedule adds up to the
    ;;    words to write plus the adjustment, and the last running total is
    ;;    the goal.  (With no open day, or fixed days already beyond what is
    ;;    left, the open days get 0 and nothing can add up.)
    (when (and (> (plist-get engine :adjustable-weight) 0)
               (>= (+ to-write adjustment) (plist-get engine :fixed-words)))
      (should (equal (list label (< (abs (- exact (+ to-write adjustment))) 1e-6))
                     (list label t)))
      (should (equal (list label (plist-get (car (last schedule)) :cumulative))
                     (list label (org-scribe-plan-total-words plan)))))
    ;; 3. Whole-word goals are never negative
    (dolist (day schedule)
      (should (equal (list label (>= (plist-get day :words) 0)) (list label t))))
    ;; 4. Locked days show their locked goal
    (dolist (day schedule)
      (when (plist-get day :locked)
        (should (equal (list label (plist-get day :words))
                       (list label (cdr (assoc (plist-get day :date)
                                               (org-scribe-plan-locked-goals plan))))))))
    ;; 5. The window: end date, calendar days, writing days, locked minimum
    (should (equal (list label (org-scribe-plan-end-date plan))
                   (list label (plist-get (car (last schedule)) :date))))
    (should (equal (list label (org-scribe-plan-days plan))
                   (list label (length schedule))))
    (should (equal (list label (org-scribe-plan-writing-days plan))
                   ;; Writing days are every day that isn't a spare date
                   (list label (cl-count-if-not (lambda (d) (plist-get d :spare))
                                                schedule))))
    (should (equal (list label (>= (org-scribe-plan-writing-days plan)
                                   (org-scribe-planner--min-writing-days plan)))
                   (list label t)))
    ;; 6. Every plan day before today is locked
    (dolist (day schedule)
      (when (string< (plist-get day :date) today)
        (should (equal (list label (plist-get day :date) (plist-get day :locked))
                       (list label (plist-get day :date) t)))))))

(defun test-engine--restore (plan before)
  "Put every slot of BEFORE back into PLAN: a change that failed is undone.
The \"What changed?\" command does the same when it can't finish."
  (dolist (slot (mapcar #'car (cdr (cl-struct-slot-info 'org-scribe-plan))))
    (setf (cl-struct-slot-value 'org-scribe-plan slot plan)
          (cl-struct-slot-value 'org-scribe-plan slot before))))

(defun test-engine--adjust (plan before adjustment)
  "Apply ADJUSTMENT to PLAN; if it fails, undo the change back to BEFORE."
  (condition-case nil
      (org-scribe-planner--apply-adjustment plan before adjustment)
    (error (test-engine--restore plan before))))

(defun test-engine--random-step (plan rng-state)
  "Apply one random change to PLAN; RNG-STATE is a one-element list (seed).
Returns a description of the change."
  (let* ((next (lambda (n)
                 ;; Linear congruential generator: deterministic per seed
                 (setcar rng-state (mod (+ (* (car rng-state) 1103515245) 12345)
                                        2147483648))
                 (mod (/ (car rng-state) 65536) n)))
         (today (org-scribe-planner--get-today-date))
         (end (org-scribe-plan-end-date plan))
         (op (funcall next 13)))
    (pcase op
      (0 (let ((days (funcall next 4)))
           (setq test-engine--today (org-scribe-planner--add-days today days))
           (format "advance %d day(s)" days)))
      (1 (let ((total (+ (org-scribe-plan-current-words plan) (funcall next 2500))))
           (test-engine--log plan today total)
           (format "log %d on %s" total today)))
      (2 (let ((total (+ 5000 (* 1000 (funcall next 60)))))
           (setf (org-scribe-plan-total-words plan) total)
           (org-scribe-planner--sync-daily plan)
           (format "total %d" total)))
      (3 (let ((daily (+ 200 (* 100 (funcall next 30)))))
           (ignore-errors (org-scribe-planner--calculate-time plan daily))
           (format "daily %d" daily)))
      (4 (let ((days (max (org-scribe-planner--min-writing-days plan)
                          (+ 1 (funcall next 60)))))
           (org-scribe-planner--set-writing-days plan days)
           (format "writing days %d" days)))
      (5 (let ((date (org-scribe-planner--add-days today (funcall next 30)))
               (before (org-scribe-planner--copy-plan plan))
               (adjustment (nth (funcall next 3) org-scribe-planner--adjustments)))
           (unless (member date (org-scribe-plan-spare-days plan))
             (setf (org-scribe-plan-spare-days plan)
                   (cons date (org-scribe-plan-spare-days plan))))
           (ignore end)
           (test-engine--adjust plan before adjustment)
           (format "spare %s, %s" date adjustment)))
      (6 (let ((date (car (org-scribe-plan-spare-days plan)))
               (before (org-scribe-planner--copy-plan plan))
               (adjustment (nth (funcall next 3) org-scribe-planner--adjustments)))
           (setf (org-scribe-plan-spare-days plan)
                 (remove date (org-scribe-plan-spare-days plan)))
           (test-engine--adjust plan before adjustment)
           (format "unspare %s, %s" date adjustment)))
      (7 (org-scribe-planner-recalculate-remaining-days plan "/tmp/unused-plan.org")
         "adjust remaining")
      (8 (let ((pattern (car (nth (funcall next 6) org-scribe-planner--patterns))))
           (setf (org-scribe-plan-pattern plan) pattern)
           (setf (org-scribe-plan-pattern-config plan)
                 (when (eq pattern 'random) (list :seed (1+ (funcall next 1000)))))
           (org-scribe-planner--sync-daily plan)
           (format "pattern %s" pattern)))
      (9 (let ((before (org-scribe-planner--copy-plan plan))
               (mode (nth (funcall next 3) '(same different rest)))
               (adjustment (nth (funcall next 3) org-scribe-planner--adjustments)))
           (setf (org-scribe-plan-weekend-mode plan) mode
                 (org-scribe-plan-weekend-daily plan) (* 100 (funcall next 30)))
           (test-engine--adjust plan before adjustment)
           (format "weekends %s, %s" mode adjustment)))
      (10 (let* ((date (org-scribe-planner--add-days today (funcall next 30)))
                 (goal (* 100 (funcall next 25)))
                 (before (org-scribe-planner--copy-plan plan))
                 (adjustment (nth (funcall next 3) org-scribe-planner--adjustments)))
            (if (org-scribe-planner--custom-goal-problem plan date)
                (format "day goal %s refused" date)
              (setf (org-scribe-plan-custom-goals plan)
                    (cons (cons date goal)
                          (assoc-delete-all date (copy-tree (org-scribe-plan-custom-goals plan)))))
              (test-engine--adjust plan before adjustment)
              (format "day goal %s = %d, %s" date goal adjustment))))
      (11 (let ((new-start (org-scribe-planner--add-days
                            (org-scribe-plan-start-date plan) (- (funcall next 15) 7)))
                (shift (= (funcall next 2) 1)))
            (org-scribe-planner--move-start-date plan new-start shift)
            (format "start %s%s" new-start (if shift ", shifted" ""))))
      (_ (org-scribe-planner-recalculate-end-date plan "/tmp/unused-plan.org")
         "adjust end date"))))

(ert-deftest test-planner-engine-invariants-under-random-changes ()
  "20 seeded sequences of 40 random changes keep every invariant.
Today moves forward along the way; past days are locked before each
change, as `org-scribe-planner--with-current-plan' does.  A failure names
the step; replay a seed by running its sequence alone."
  (dolist (seed (number-sequence 1 20))
    (test-engine--on "2026-11-01"
      (test-engine--without-io
        (cl-letf (((symbol-function 'message) #'ignore))
          (let ((plan (test-engine--plan :preexisting-words (* 500 (mod seed 3)) ))
                (rng (list seed)))
            (dotimes (i 40)
              (org-scribe-planner--lock-past-goals plan)
              (let* ((before (copy-tree (org-scribe-plan-locked-goals plan)))
                     (change (test-engine--random-step plan rng)))
                (org-scribe-planner--lock-past-goals plan)
                (test-engine--check-invariants
                 plan before (org-scribe-planner--get-today-date)
                 (format "seed %d, step %d (%s)" seed i change))))))))))

(provide 'test-planner-engine)
;;; test-planner-engine.el ends here
