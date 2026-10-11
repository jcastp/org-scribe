;;; test-planner-patterns.el --- Tests for patterns, weekends and day goals -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for how the words are spread: writing patterns (flat, valley,
;; mountain, rising, declining, random), weekend modes (same, a goal of
;; their own, rest) on any weekend days, goals for single days, rest days
;; of every kind, and the "What changed?" options that set them.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-scribe-planner)

;;; Helpers

(defvar test-patterns--today "2026-10-25"
  "The date `org-scribe-planner--get-today-date' returns in these tests.")

(defun test-patterns--plan (&rest args)
  "14,000 words over 14 days from Sunday Nov 1, 2026, with ARGS."
  (let ((plan (apply #'make-org-scribe-plan
                     (append args (list :title "P" :total-words 14000 :days 14
                                        :start-date "2026-11-01")))))
    (org-scribe-planner--calculate-missing-variable plan)
    plan))

(defun test-patterns--goals (plan)
  "PLAN's whole-word day goals, in order."
  (mapcar (lambda (d) (plist-get d :words))
          (org-scribe-planner--generate-day-schedule plan)))

(defun test-patterns--weights (pattern n &optional config)
  "The weights of PATTERN over N days, with CONFIG."
  (org-scribe-planner--pattern-weights
   (make-org-scribe-plan :pattern pattern :pattern-config config) n))

(defun test-patterns--choose (&rest prefixes)
  "A `completing-read' stub answering with PREFIXES, one per call."
  (lambda (_prompt collection &optional predicate &rest _)
    (let ((prefix (pop prefixes)))
      (or (cl-find-if (lambda (c) (string-prefix-p prefix c))
                      (all-completions "" collection predicate))
          (error "No candidate starts with %S" prefix)))))

(defmacro test-patterns--run (plan answers &rest stubs)
  "Run \"What changed?\" on PLAN answering ANSWERS, with extra STUBS."
  (declare (indent 2))
  `(let ((org-scribe-planner--current-plan ,plan)
         (org-scribe-planner--current-plan-file "/tmp/unused-plan.org"))
     (cl-letf (((symbol-function 'org-scribe-planner--get-today-date)
                (lambda () test-patterns--today))
               ((symbol-function 'org-scribe-planner--save-plan) #'ignore)
               ((symbol-function 'org-scribe-planner--refresh-agenda-if-synced) #'ignore)
               ((symbol-function 'org-scribe-planner-show-calendar) #'ignore)
               ((symbol-function 'message) #'ignore)
               ((symbol-function 'completing-read) (apply #'test-patterns--choose ,answers))
               ,@stubs)
       (org-scribe-planner-recalculate))))

;;; Pattern weights

(ert-deftest test-planner-patterns-weights-average-one ()
  "Every pattern's weights average 1."
  (dolist (pattern '(flat valley mountain rising declining random))
    (let ((weights (test-patterns--weights pattern 20 (when (eq pattern 'random) '(:seed 7)))))
      (should (= (length weights) 20))
      (should (< (abs (- (/ (apply #'+ weights) 20) 1.0)) 1e-9)))))

(ert-deftest test-planner-patterns-weights-have-their-shape ()
  "Valley dips in the middle, mountain peaks there; rising and declining slope."
  (let ((valley (test-patterns--weights 'valley 20))
        (mountain (test-patterns--weights 'mountain 20))
        (rising (test-patterns--weights 'rising 20))
        (declining (test-patterns--weights 'declining 20)))
    (should (> (nth 0 valley) (nth 10 valley)))
    (should (> (nth 19 valley) (nth 10 valley)))
    (should (< (nth 0 mountain) (nth 10 mountain)))
    (should (< (nth 19 mountain) (nth 10 mountain)))
    (should (equal rising (sort (copy-sequence rising) #'<)))
    (should (equal declining (sort (copy-sequence declining) #'>)))))

(ert-deftest test-planner-patterns-intensity-widens-the-range ()
  "An extreme rising pattern spans more than a mild one."
  (let ((mild (test-patterns--weights 'rising 10 '(:start 0.8 :end 1.2)))
        (extreme (test-patterns--weights 'rising 10 '(:start 0.3 :end 2.0))))
    (should (> (- (car (last extreme)) (car extreme))
               (- (car (last mild)) (car mild))))))

(ert-deftest test-planner-patterns-random-depends-only-on-seed ()
  "The same seed gives the same goals, whatever Emacs's own random state."
  (let ((first (test-patterns--weights 'random 15 '(:seed 42))))
    (random t)
    (should (equal first (test-patterns--weights 'random 15 '(:seed 42))))
    (should-not (equal first (test-patterns--weights 'random 15 '(:seed 43))))))

(ert-deftest test-planner-patterns-schedule-adds-up-with-a-pattern ()
  "A rising plan still adds up to the total, and its goals rise."
  (let* ((plan (test-patterns--plan :pattern 'rising))
         (goals (test-patterns--goals plan)))
    (should (= (apply #'+ goals) 14000))
    (should (< (car goals) (car (last goals))))))

;;; Weekends

(ert-deftest test-planner-patterns-weekends-off ()
  "Weekends as rest days: 0 words, and the weekdays share the total.
Nov 1, 2026 is a Sunday: 14 days hold 4 weekend days and 10 weekdays."
  (let* ((plan (test-patterns--plan :weekend-mode 'rest))
         (schedule (org-scribe-planner--generate-day-schedule plan)))
    (should (string= (org-scribe-plan-end-date plan) "2026-11-14"))
    (dolist (day schedule)
      (if (org-scribe-planner--weekend-p plan (plist-get day :date))
          (progn (should (= (plist-get day :words) 0))
                 (should (plist-get day :is-spare-day))
                 (should-not (plist-get day :spare)))
        (should (= (plist-get day :words) 1400))))
    (should (= (org-scribe-plan-daily-words plan) 1400))))

(ert-deftest test-planner-patterns-weekend-goal-of-their-own ()
  "A weekend goal is fixed; the weekdays share what is left."
  (let ((plan (test-patterns--plan :weekend-mode 'different :weekend-daily 2000)))
    ;; 4 weekend days × 2,000 = 8,000; 6,000 over 10 weekdays
    (should (= (org-scribe-plan-daily-words plan) 600))
    (should (= (apply #'+ (test-patterns--goals plan)) 14000))))

(ert-deftest test-planner-patterns-weekend-days-are-configurable ()
  "Friday and Saturday as the weekend."
  (let* ((plan (test-patterns--plan :weekend-mode 'rest :weekend-days '(5 6)))
         (goals (test-patterns--goals plan)))
    ;; Nov 6 and 7, 2026 are a Friday and a Saturday
    (should (= (nth 5 goals) 0))
    (should (= (nth 6 goals) 0))
    (should (> (nth 7 goals) 0))))

;;; Day goals

(ert-deftest test-planner-patterns-day-goal-is-fixed ()
  "A day goal is kept; the other days share the rest."
  (let ((plan (test-patterns--plan :custom-goals '(("2026-11-05" . 2300)))))
    (let ((goals (test-patterns--goals plan)))
      (should (= (nth 4 goals) 2300))
      (should (= (apply #'+ goals) 14000))
      (should (= (org-scribe-plan-daily-words plan) 900)))))

(ert-deftest test-planner-patterns-day-goal-zero-is-a-rest-day ()
  "A day goal of 0 makes a rest day, though it isn't a spare day."
  (let* ((plan (test-patterns--plan :custom-goals '(("2026-11-05" . 0))))
         (day (nth 4 (org-scribe-planner--generate-day-schedule plan))))
    (should (plist-get day :is-spare-day))
    (should-not (plist-get day :spare))
    (should (plist-get day :custom))))

(ert-deftest test-planner-patterns-day-goal-problems ()
  "Day goals are refused on past days, outside the plan, spare days and weekends off."
  (cl-letf (((symbol-function 'org-scribe-planner--get-today-date) (lambda () "2026-11-03")))
    (let ((plan (test-patterns--plan :spare-days '("2026-11-04") :weekend-mode 'rest)))
      (should (string-match-p "locked" (org-scribe-planner--custom-goal-problem plan "2026-11-02")))
      (should (string-match-p "outside" (org-scribe-planner--custom-goal-problem plan "2026-12-20")))
      (should (string-match-p "spare day" (org-scribe-planner--custom-goal-problem plan "2026-11-04")))
      (should (string-match-p "weekend" (org-scribe-planner--custom-goal-problem plan "2026-11-07")))
      (should-not (org-scribe-planner--custom-goal-problem plan "2026-11-05")))))

;;; Plan file

(ert-deftest test-planner-patterns-roundtrip ()
  "Pattern, weekend settings and day goals survive save→load."
  (let* ((dir (make-temp-file "org-scribe-patterns-" t))
         (file (expand-file-name "plan.org" dir))
         (plan (test-patterns--plan :pattern 'random :pattern-config '(:min 0.5 :max 1.5 :seed 99)
                                    :weekend-mode 'different :weekend-daily 1800
                                    :weekend-days '(5 6)
                                    :custom-goals '(("2026-11-09" . 0) ("2026-11-03" . 2500)))))
    (unwind-protect
        (progn
          (org-scribe-planner--save-plan plan file)
          (let ((loaded (org-scribe-planner--load-plan file)))
            (should (eq (org-scribe-plan-pattern loaded) 'random))
            (should (equal (org-scribe-plan-pattern-config loaded) '(:min 0.5 :max 1.5 :seed 99)))
            (should (eq (org-scribe-plan-weekend-mode loaded) 'different))
            (should (equal (org-scribe-plan-weekend-days loaded) '(5 6)))
            (should (= (org-scribe-plan-weekend-daily loaded) 1800))
            (should (equal (sort (copy-sequence (org-scribe-plan-custom-goals loaded))
                                 (lambda (a b) (string< (car a) (car b))))
                           '(("2026-11-03" . 2500) ("2026-11-09" . 0))))
            (should (equal (test-patterns--goals loaded) (test-patterns--goals plan)))))
      (dolist (buffer (buffer-list))
        (when (equal (buffer-file-name buffer) file) (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest test-planner-patterns-flat-plan-file-unchanged ()
  "A flat plan with ordinary weekends writes none of the new properties."
  (let* ((dir (make-temp-file "org-scribe-patterns-" t))
         (file (expand-file-name "plan.org" dir)))
    (unwind-protect
        (progn
          (org-scribe-planner--save-plan (test-patterns--plan) file)
          (with-temp-buffer
            (insert-file-contents file)
            (dolist (property '("PATTERN" "WEEKEND_MODE" "CUSTOM_GOALS"))
              (goto-char (point-min))
              (should-not (re-search-forward (format "^:%s:" property) nil t)))))
      (dolist (buffer (buffer-list))
        (when (equal (buffer-file-name buffer) file) (kill-buffer buffer)))
      (delete-directory dir t))))

;;; What changed?

(ert-deftest test-planner-patterns-change-pattern-keeps-past-days ()
  "A new pattern reshapes only the days to come; past goals stay."
  (let ((plan (test-patterns--plan))
        (test-patterns--today "2026-11-06"))
    (test-patterns--run plan '("Writing pattern")
      ((symbol-function 'org-scribe-planner--read-pattern) (lambda () '(rising :start 0.5 :end 1.5))))
    (let ((goals (test-patterns--goals plan)))
      (should (eq (org-scribe-plan-pattern plan) 'rising))
      (should (equal (cl-subseq goals 0 5) (make-list 5 1000)))
      (should (< (nth 5 goals) (car (last goals))))
      (should (= (apply #'+ goals) 14000)))))

(ert-deftest test-planner-patterns-read-pattern-stores-random-seed ()
  "Choosing the random pattern stores a seed, so its goals don't change."
  (cl-letf (((symbol-function 'completing-read)
             (test-patterns--choose "Random" "moderate")))
    (let ((choice (org-scribe-planner--read-pattern)))
      (should (eq (car choice) 'random))
      (should (integerp (plist-get (cdr choice) :seed))))))

(ert-deftest test-planner-patterns-weekends-off-keep-end-date ()
  "Weekends off, keeping the end date: the weekdays take more words."
  (let ((plan (test-patterns--plan)))
    (test-patterns--run plan '("Weekends" "No writing" "Keep the end date")
      ((symbol-function 'org-scribe-planner--read-weekend-days) (lambda (_) '(6 0))))
    (should (eq (org-scribe-plan-weekend-mode plan) 'rest))
    (should (string= (org-scribe-plan-end-date plan) "2026-11-14"))
    (should (= (org-scribe-plan-daily-words plan) 1400))))

(ert-deftest test-planner-patterns-weekends-off-move-end-date ()
  "Weekends off, keeping the daily target: the plan grows by its weekends."
  (let ((plan (test-patterns--plan)))
    (test-patterns--run plan '("Weekends" "No writing" "Move the end date")
      ((symbol-function 'org-scribe-planner--read-weekend-days) (lambda (_) '(6 0))))
    (should (= (org-scribe-plan-daily-words plan) 1000))
    (should (= (apply #'+ (test-patterns--goals plan)) 14000))
    ;; 14 weekdays from Sunday Nov 1 end on Thursday Nov 19
    (should (string= (org-scribe-plan-end-date plan) "2026-11-19"))))

(ert-deftest test-planner-patterns-set-day-goal-change-total ()
  "A day goal, keeping the end date and the daily target: the total changes."
  (let ((plan (test-patterns--plan)))
    (test-patterns--run plan '("Day goal: set" "Keep the end date and the daily")
      ((symbol-function 'org-scribe-planner--read-date) (lambda (&rest _) "2026-11-05"))
      ((symbol-function 'org-scribe-planner--read-non-negative-number) (lambda (&rest _) 3000)))
    (should (= (org-scribe-plan-total-words plan) 16000))
    (should (= (nth 4 (test-patterns--goals plan)) 3000))))

(ert-deftest test-planner-patterns-set-day-goal-refused ()
  "A day goal on a spare day is refused with the reason."
  (let ((plan (test-patterns--plan :spare-days '("2026-11-05"))))
    (should-error
     (test-patterns--run plan '("Day goal: set")
       ((symbol-function 'org-scribe-planner--read-date) (lambda (&rest _) "2026-11-05")))
     :type 'user-error)))

(ert-deftest test-planner-patterns-remove-day-goal ()
  "Removing a day goal gives the day back to the plan."
  (let ((plan (test-patterns--plan :custom-goals '(("2026-11-05" . 0)))))
    (test-patterns--run plan '("Day goal: remove" "2026-11-05" "Keep the end date"))
    (should-not (org-scribe-plan-custom-goals plan))
    (should (= (nth 4 (test-patterns--goals plan)) 1000))))

;;; An aborted change

(ert-deftest test-planner-patterns-aborted-change-restores-plan ()
  "Quitting at the adjustment question leaves the active plan as it was.
Regression: the spare day stayed in the plan in memory, unsaved, with a
window that no longer matched it."
  (let* ((plan (test-patterns--plan))
         (org-scribe-planner--current-plan plan)
         (org-scribe-planner--current-plan-file "/tmp/unused-plan.org")
         (answers (list "Add/modify spare days")))
    (cl-letf (((symbol-function 'org-scribe-planner--get-today-date)
               (lambda () test-patterns--today))
              ((symbol-function 'org-scribe-planner--save-plan) #'ignore)
              ((symbol-function 'org-scribe-planner-show-calendar) #'ignore)
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'org-scribe-planner--configure-spare-days)
               (lambda (p) (setf (org-scribe-plan-spare-days p) '("2026-11-05"))))
              ((symbol-function 'completing-read)
               (lambda (&rest _)
                 (if answers (pop answers) (signal 'quit nil)))))
      (condition-case nil
          (org-scribe-planner-recalculate)
        (quit nil)))
    (should-not (org-scribe-plan-spare-days org-scribe-planner--current-plan))
    (should (string= (org-scribe-plan-end-date org-scribe-planner--current-plan)
                     "2026-11-14"))))

;;; Recalculation keeps the weekend ratio

(ert-deftest test-planner-patterns-recalculation-keeps-weekend-ratio ()
  "Keeping the end date, the weekend goal keeps its ratio to the weekday goal."
  (cl-letf (((symbol-function 'org-scribe-planner--get-today-date) (lambda () "2026-11-08"))
            ((symbol-function 'org-scribe-planner--save-plan) #'ignore)
            ((symbol-function 'org-scribe-planner--refresh-agenda-if-synced) #'ignore)
            ((symbol-function 'org-scribe-planner-show-calendar) #'ignore)
            ((symbol-function 'message) #'ignore))
    (let ((plan (test-patterns--plan :weekend-mode 'different :weekend-daily 1200)))
      (should (= (org-scribe-plan-daily-words plan) 920))
      ;; A week in, nothing written
      (org-scribe-planner-recalculate-remaining-days plan "/tmp/unused-plan.org")
      (let ((ratio (/ (float (org-scribe-plan-weekend-daily plan))
                      (org-scribe-plan-daily-words plan))))
        (should (< (abs (- ratio (/ 1200.0 920))) 0.01)))
      ;; The past week keeps its goals; the running total still ends at the goal
      (should (= (plist-get (car (last (org-scribe-planner--generate-day-schedule plan)))
                            :cumulative)
                 14000)))))

;;; A daily target of 0

(ert-deftest test-planner-patterns-zero-daily-fails-fast ()
  "With no daily pace and nothing recurring, finding the days fails at once."
  (let ((plan (test-patterns--plan :weekend-mode 'rest))
        (start (float-time)))
    (should-error (org-scribe-planner--calculate-time plan 0))
    (should (< (- (float-time) start) 1.0))
    (should (= (org-scribe-plan-writing-days plan) 14))))

(provide 'test-planner-patterns)
;;; test-planner-patterns.el ends here
