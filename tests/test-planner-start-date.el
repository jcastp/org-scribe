;;; test-planner-start-date.el --- Tests for moving a plan's start date -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for moving the start date (`org-scribe-planner--move-start-date'
;; and the "Start date" option of "What changed?"): writing days kept,
;; spare days and day goals kept on their dates or shifted, recorded words
;; and locked goals never moving, a started plan rebased, the warnings, and
;; past locked days always staying inside the plan.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-scribe-planner)

;;; Helpers

(defvar test-start--today "2026-10-25"
  "The date `org-scribe-planner--get-today-date' returns in these tests.")

(defmacro test-start--on (today &rest body)
  "Run BODY with today stubbed to TODAY, and saving and display stubbed."
  (declare (indent 1))
  `(let ((test-start--today ,today))
     (cl-letf (((symbol-function 'org-scribe-planner--get-today-date)
                (lambda () test-start--today))
               ((symbol-function 'org-scribe-planner--save-plan) #'ignore)
               ((symbol-function 'org-scribe-planner--refresh-agenda-if-synced) #'ignore)
               ((symbol-function 'org-scribe-planner-show-calendar) #'ignore)
               ((symbol-function 'message) #'ignore))
       ,@body)))

(defun test-start--plan (&rest args)
  "30,000 words over 30 days from Sunday Nov 1, 2026 (1,000 a day), with ARGS."
  (let ((plan (apply #'make-org-scribe-plan
                     (append args (list :title "P" :total-words 30000 :days 30
                                        :start-date "2026-11-01")))))
    (org-scribe-planner--calculate-missing-variable plan)
    plan))

(defun test-start--log (plan date total)
  "Record TOTAL as PLAN's manuscript count on DATE."
  (setf (org-scribe-plan-daily-word-counts plan)
        (cons (cons date (list :words total :note ""))
              (org-scribe-plan-daily-word-counts plan)))
  (setf (org-scribe-plan-current-words plan) total))

(defun test-start--choose (&rest prefixes)
  "A `completing-read' stub answering with PREFIXES, one per call."
  (lambda (_prompt collection &optional predicate &rest _)
    (let ((prefix (pop prefixes)))
      (or (cl-find-if (lambda (c) (string-prefix-p prefix c))
                      (all-completions "" collection predicate))
          (error "No candidate starts with %S" prefix)))))

;;; The move itself

(ert-deftest test-planner-start-move-later-keeps-writing-days ()
  "A week later: same writing days, the end date moves a week too."
  (test-start--on "2026-10-25"
    (let ((plan (test-start--plan)))
      (should (= (org-scribe-planner--move-start-date plan "2026-11-08") 7))
      (should (string= (org-scribe-plan-start-date plan) "2026-11-08"))
      (should (string= (org-scribe-plan-end-date plan) "2026-12-07"))
      (should (= (org-scribe-plan-writing-days plan) 30))
      (should (= (org-scribe-plan-daily-words plan) 1000)))))

(ert-deftest test-planner-start-move-keeps-dated-days-on-their-dates ()
  "Kept on their dates: a spare day inside stays; a day goal now outside is ignored."
  (test-start--on "2026-10-25"
    (let ((plan (test-start--plan :spare-days '("2026-11-10")
                                  :custom-goals '(("2026-11-03" . 2500)))))
      (org-scribe-planner--move-start-date plan "2026-11-08")
      (should (equal (org-scribe-plan-spare-days plan) '("2026-11-10")))
      (should (equal (org-scribe-plan-custom-goals plan) '(("2026-11-03" . 2500))))
      (should-not (org-scribe-planner--in-window-p plan "2026-11-03"))
      ;; The goal outside the plan no longer counts: 30,000 over 29 writing days
      (should (= (apply #'+ (mapcar (lambda (d) (plist-get d :words))
                                    (org-scribe-planner--generate-day-schedule plan)))
                 30000)))))

(ert-deftest test-planner-start-move-shifts-dated-days ()
  "Shifted: spare days and day goals from today on move with the start."
  (test-start--on "2026-10-25"
    (let ((plan (test-start--plan :spare-days '("2026-11-10")
                                  :custom-goals '(("2026-11-03" . 2500)))))
      (org-scribe-planner--move-start-date plan "2026-11-08" t)
      (should (equal (org-scribe-plan-spare-days plan) '("2026-11-17")))
      (should (equal (org-scribe-plan-custom-goals plan) '(("2026-11-10" . 2500)))))))

(ert-deftest test-planner-start-move-shift-leaves-the-past ()
  "Past spare days and day goals are history: they don't shift."
  (test-start--on "2026-11-06"
    (let ((plan (test-start--plan :spare-days '("2026-11-03" "2026-11-10")
                                  :custom-goals '(("2026-11-05" . 0) ("2026-11-12" . 2000)))))
      (org-scribe-planner--move-start-date plan "2026-11-02" t)
      (should (equal (sort (copy-sequence (org-scribe-plan-spare-days plan)) #'string<)
                     '("2026-11-03" "2026-11-11")))
      (should (equal (sort (copy-sequence (org-scribe-plan-custom-goals plan))
                           (lambda (a b) (string< (car a) (car b))))
                     '(("2026-11-05" . 0) ("2026-11-13" . 2000)))))))

(ert-deftest test-planner-start-move-shift-never-replaces-past-goal ()
  "A shifted day goal landing on a past day goal's date is dropped."
  (test-start--on "2026-11-06"
    (let ((plan (test-start--plan :custom-goals '(("2026-11-05" . 0) ("2026-11-06" . 3000)))))
      (org-scribe-planner--move-start-date plan "2026-10-31" t)
      (should (equal (org-scribe-plan-custom-goals plan) '(("2026-11-05" . 0)))))))

(ert-deftest test-planner-start-move-started-plan-is-rebased ()
  "A plan under way: its open days cover the words actually left."
  (test-start--on "2026-11-11"
    (let ((plan (test-start--plan)))
      (org-scribe-planner--lock-past-goals plan)
      (test-start--log plan "2026-11-10" 5000)
      (org-scribe-planner--move-start-date plan "2026-11-03")
      (let ((open (cl-remove-if (lambda (d) (string< (plist-get d :date) "2026-11-11"))
                                (org-scribe-planner--generate-day-schedule plan))))
        (should (= (apply #'+ (mapcar (lambda (d) (plist-get d :words)) open)) 25000)))
      ;; Nov 1-2 keep their locked goals, outside the plan now
      (should (assoc "2026-11-01" (org-scribe-plan-locked-goals plan)))
      (should-not (org-scribe-planner--in-window-p plan "2026-11-01")))))

(ert-deftest test-planner-start-move-unstarted-plan-is-not-rebased ()
  "Moving a plan that hadn't started into the past doesn't forgive the past days."
  (test-start--on "2026-11-04"
    (let ((plan (test-start--plan :start-date "2026-11-10")))
      (org-scribe-planner--move-start-date plan "2026-11-01")
      (should (= (org-scribe-plan-schedule-adjustment plan) 0))
      ;; Nov 1-3 are locked as they are, and nothing was written on them
      (should (= (length (org-scribe-plan-locked-goals plan)) 3))
      (should (eq (plist-get (org-scribe-planner--status plan) :status) 'behind)))))

(ert-deftest test-planner-start-move-earlier-keeps-locked-days-inside ()
  "Moving the start earlier never ends the plan before its locked days.
Regression (found by the random invariant tests): the writing days were
kept, so the window could end before past days already locked."
  (test-start--on "2026-11-03"
    (let ((plan (make-org-scribe-plan :title "P" :total-words 2000 :days 2
                                      :start-date "2026-11-01")))
      (org-scribe-planner--calculate-missing-variable plan)
      (org-scribe-planner--lock-past-goals plan)
      (org-scribe-planner--move-start-date plan "2026-10-27")
      ;; The window reaches Nov 2 again; Oct 27-31 join it, locked
      (should (string= (org-scribe-plan-end-date plan) "2026-11-02"))
      (should (= (org-scribe-plan-writing-days plan) 7))
      (should (>= (org-scribe-plan-writing-days plan)
                  (org-scribe-planner--min-writing-days plan)))
      (should (assoc "2026-10-29" (org-scribe-plan-locked-goals plan))))))

(ert-deftest test-planner-start-min-writing-days-reaches-last-locked-day ()
  "The minimum length runs through the last locked day, gaps included."
  (let ((plan (make-org-scribe-plan :start-date "2026-11-01"
                                    :locked-goals '(("2026-11-01" . 1000) ("2026-11-05" . 1000))
                                    :spare-days '("2026-11-03"))))
    ;; Nov 1-5 without the spare Nov 3
    (should (= (org-scribe-planner--min-writing-days plan) 4))))

;;; Warnings

(ert-deftest test-planner-start-move-warnings ()
  "Words outside the new plan, past days joining it, a started plan."
  (test-start--on "2026-11-11"
    (let ((plan (test-start--plan)))
      (org-scribe-planner--lock-past-goals plan)
      (test-start--log plan "2026-11-02" 2000)
      (let ((moved (org-scribe-planner--copy-plan plan)))
        (org-scribe-planner--move-start-date moved "2026-11-05")
        (let ((warnings (string-join (org-scribe-planner--start-move-warnings plan moved) "\n")))
          (should (string-match-p "1 day(s) with words written (2000 words) are outside" warnings))
          (should (string-match-p "Some past (locked) days are now outside the plan" warnings))
          (should (string-match-p "The plan has started" warnings))))
      (let ((moved (org-scribe-planner--copy-plan plan)))
        (org-scribe-planner--move-start-date moved "2026-10-29")
        (should (string-match-p "3 day(s) before today join the plan"
                                (string-join (org-scribe-planner--start-move-warnings plan moved)
                                             "\n")))))))

;;; The "Start date" option

(defmacro test-start--run (plan answers &rest stubs)
  "Run \"What changed?\" on PLAN answering ANSWERS, with STUBS."
  (declare (indent 2))
  `(let ((org-scribe-planner--current-plan ,plan)
         (org-scribe-planner--current-plan-file "/tmp/unused-plan.org"))
     (cl-letf (((symbol-function 'completing-read) (apply #'test-start--choose ,answers))
               ,@stubs)
       (org-scribe-planner-recalculate))))

(ert-deftest test-planner-start-command-same-date-refused ()
  "The current start date again is refused; nothing changes."
  (test-start--on "2026-10-25"
    (let ((plan (test-start--plan)))
      (should-error
       (test-start--run plan '("Start date")
         ((symbol-function 'org-scribe-planner--read-date) (lambda (&rest _) "2026-11-01")))
       :type 'user-error))))

(ert-deftest test-planner-start-command-without-dated-days ()
  "With no spare days or day goals ahead: one option, previewed with the dates."
  (test-start--on "2026-10-25"
    (let ((plan (test-start--plan))
          (offered nil))
      (let ((org-scribe-planner--current-plan plan)
            (org-scribe-planner--current-plan-file "/tmp/unused-plan.org")
            (answers (list "Start date")))
        (cl-letf (((symbol-function 'org-scribe-planner--read-date) (lambda (&rest _) "2026-11-08"))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt collection &optional predicate &rest _)
                     (if answers
                         (pop answers)
                       (setq offered (all-completions "" collection predicate))
                       (car offered)))))
          (org-scribe-planner-recalculate)))
      (should (equal offered '("Move the start date → 2026-11-08 to 2026-12-07, 1000 words/day")))
      ;; The command changes the active plan in place
      (should (string= (org-scribe-plan-start-date plan) "2026-11-08")))))

(ert-deftest test-planner-start-command-offers-keep-or-shift ()
  "With dated days ahead: keep them (default) or shift them, with the notice."
  (test-start--on "2026-10-25"
    (let* ((plan (test-start--plan :custom-goals '(("2026-11-03" . 2500))))
           (prompt-seen nil)
           (offered nil)
           (answers (list "Start date")))
      (let ((org-scribe-planner--current-plan plan)
            (org-scribe-planner--current-plan-file "/tmp/unused-plan.org"))
        (cl-letf (((symbol-function 'org-scribe-planner--read-date) (lambda (&rest _) "2026-11-05"))
                  ((symbol-function 'completing-read)
                   (lambda (prompt collection &optional predicate &rest _)
                     (if answers
                         (pop answers)
                       (setq prompt-seen prompt
                             offered (all-completions "" collection predicate))
                       (cl-find-if (lambda (c) (string-prefix-p "Shift" c)) offered)))))
          (org-scribe-planner-recalculate))
        (should (string-match-p "Moving the start 4 day(s) later" prompt-seen))
        (should (string-match-p "1 would fall outside the plan (2026-11-03 day goal of 2500 words)"
                                prompt-seen))
        (should (string-prefix-p "Keep them on their dates" (car offered)))
        (should (cl-some (lambda (c) (string-prefix-p "Shift them all 4 day(s) later (they land on other weekdays)" c))
                         offered))
        (should (equal (org-scribe-plan-custom-goals org-scribe-planner--current-plan)
                       '(("2026-11-07" . 2500))))))))

(ert-deftest test-planner-start-command-declined-warning-restores ()
  "Declining after the warnings leaves the active plan as it was."
  (test-start--on "2026-11-11"
    (let ((plan (test-start--plan)))
      (org-scribe-planner--lock-past-goals plan)
      (test-start--log plan "2026-11-02" 2000)
      (let ((org-scribe-planner--current-plan plan)
            (org-scribe-planner--current-plan-file "/tmp/unused-plan.org"))
        (cl-letf (((symbol-function 'org-scribe-planner--read-date) (lambda (&rest _) "2026-11-05"))
                  ((symbol-function 'completing-read)
                   (test-start--choose "Start date" "Move the start date"))
                  ((symbol-function 'y-or-n-p) (lambda (_) nil)))
          (should-error (org-scribe-planner-recalculate) :type 'user-error))
        (should (string= (org-scribe-plan-start-date org-scribe-planner--current-plan)
                         "2026-11-01"))))))

(provide 'test-planner-start-date)
;;; test-planner-start-date.el ends here
