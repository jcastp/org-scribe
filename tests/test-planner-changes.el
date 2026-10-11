;;; test-planner-changes.el --- Tests for changing a plan with previews -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for "What changed?" (`org-scribe-planner-recalculate'): each value
;; change asks which other value to keep; spare-day changes ask how the
;; plan absorbs them (keep the end date, move it, or change the total);
;; every option is previewed on a copy before one is applied; and a plan
;; synced to the agenda gets its entries regenerated.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-scribe-planner)

;;; Helpers

(defvar test-changes--today "2026-10-25"
  "The date `org-scribe-planner--get-today-date' returns in these tests.")

(defvar test-changes--candidates nil
  "The candidates of every question asked, newest first.")

(defun test-changes--choose (&rest prefixes)
  "Return a `completing-read' stub answering with PREFIXES, one per call.
Each answer is the first candidate starting with its prefix; every
question's candidates are pushed onto `test-changes--candidates'."
  (lambda (_prompt collection &optional predicate &rest _)
    (let ((candidates (all-completions "" collection predicate))
          (prefix (pop prefixes)))
      (push (all-completions "" collection) test-changes--candidates)
      (or (cl-find-if (lambda (c) (string-prefix-p prefix c)) candidates)
          (error "No candidate starts with %S" prefix)))))

(defun test-changes--plan ()
  "30,000 words over 30 writing days from Nov 1, 2026: 1,000 a day."
  (let ((plan (make-org-scribe-plan :title "P" :total-words 30000 :days 30
                                    :start-date "2026-11-01")))
    (org-scribe-planner--calculate-missing-variable plan)
    plan))

(defmacro test-changes--run (plan answers &rest stubs)
  "Run \"What changed?\" on PLAN, answering questions with ANSWERS.
STUBS are extra `cl-letf' bindings.  Saving and display are stubbed."
  (declare (indent 2))
  `(let ((org-scribe-planner--current-plan ,plan)
         (org-scribe-planner--current-plan-file "/tmp/unused-plan.org"))
     ;; Reset, not let-bound: the test reads the questions afterwards
     (setq test-changes--candidates nil)
     (cl-letf (((symbol-function 'org-scribe-planner--get-today-date)
                (lambda () test-changes--today))
               ((symbol-function 'org-scribe-planner--save-plan) #'ignore)
               ((symbol-function 'org-scribe-planner--refresh-agenda-if-synced) #'ignore)
               ((symbol-function 'org-scribe-planner-show-calendar) #'ignore)
               ((symbol-function 'message) #'ignore)
               ((symbol-function 'completing-read) (apply #'test-changes--choose ,answers))
               ,@stubs)
       (org-scribe-planner-recalculate))))

(defun test-changes--add-spare (&rest dates)
  "A `--configure-spare-days' stub that adds DATES."
  (lambda (plan)
    (setf (org-scribe-plan-spare-days plan)
          (append (org-scribe-plan-spare-days plan) dates))))

;;; Copies

(ert-deftest test-planner-changes-copy-is-independent ()
  "Changing a copy's lists leaves the plan alone."
  (let* ((plan (make-org-scribe-plan :spare-days (list "2026-11-02")
                                     :locked-goals (list (cons "2026-11-01" 1000))
                                     :daily-word-counts (list (cons "2026-11-01"
                                                                    (list :words 900)))))
         (copy (org-scribe-planner--copy-plan plan)))
    (push "2026-11-03" (org-scribe-plan-spare-days copy))
    (setcdr (car (org-scribe-plan-locked-goals copy)) 5)
    (plist-put (cdr (car (org-scribe-plan-daily-word-counts copy))) :words 1)
    (should (equal (org-scribe-plan-spare-days plan) '("2026-11-02")))
    (should (equal (org-scribe-plan-locked-goals plan) '(("2026-11-01" . 1000))))
    (should (= (plist-get (cdr (car (org-scribe-plan-daily-word-counts plan))) :words) 900))))

;;; The adjustment question after spare days change

(ert-deftest test-planner-changes-spare-day-keep-end-date ()
  "A spare day, keeping the end date: same end, the daily target goes up."
  (let ((plan (test-changes--plan)))
    (test-changes--run plan '("Add/modify spare days" "Keep the end date")
      ((symbol-function 'org-scribe-planner--configure-spare-days)
       (test-changes--add-spare "2026-11-10")))
    (should (string= (org-scribe-plan-end-date plan) "2026-11-30"))
    (should (= (org-scribe-plan-total-words plan) 30000))
    (should (= (org-scribe-plan-daily-words plan) 1034))))

(ert-deftest test-planner-changes-spare-day-move-end-date ()
  "A spare day, keeping the daily target: the end date moves a day."
  (let ((plan (test-changes--plan)))
    (test-changes--run plan '("Add/modify spare days" "Move the end date")
      ((symbol-function 'org-scribe-planner--configure-spare-days)
       (test-changes--add-spare "2026-11-10")))
    (should (string= (org-scribe-plan-end-date plan) "2026-12-01"))
    (should (= (org-scribe-plan-daily-words plan) 1000))
    (should (= (org-scribe-plan-total-words plan) 30000))))

(ert-deftest test-planner-changes-spare-day-change-total ()
  "A spare day, keeping the end date and the daily target: the total drops."
  (let ((plan (test-changes--plan)))
    (test-changes--run plan '("Add/modify spare days" "Keep the end date and the daily")
      ((symbol-function 'org-scribe-planner--configure-spare-days)
       (test-changes--add-spare "2026-11-10")))
    (should (string= (org-scribe-plan-end-date plan) "2026-11-30"))
    (should (= (org-scribe-plan-daily-words plan) 1000))
    (should (= (org-scribe-plan-total-words plan) 29000))))

(ert-deftest test-planner-changes-question-previews-every-option ()
  "Each option shows its outcome before one is chosen."
  (let ((plan (test-changes--plan)))
    (test-changes--run plan '("Add/modify spare days" "Keep the end date")
      ((symbol-function 'org-scribe-planner--configure-spare-days)
       (test-changes--add-spare "2026-11-10")))
    (let ((question (car test-changes--candidates)))
      (should (= (length question) 3))
      (should (member "Keep the end date (change the daily target) → ends 2026-11-30, 1034 words/day, total 30000 words"
                      question))
      (should (member "Move the end date (keep 1000 words/day) → ends 2026-12-01, 1000 words/day, total 30000 words"
                      question))
      (should (member "Keep the end date and the daily target (change the total) → ends 2026-11-30, 1000 words/day, total 29000 words"
                      question)))))

(ert-deftest test-planner-changes-no-question-when-nothing-moves ()
  "A spare day outside the plan changes no goal: no question is asked."
  (let ((plan (test-changes--plan)))
    (test-changes--run plan '("Add/modify spare days")
      ((symbol-function 'org-scribe-planner--configure-spare-days)
       (test-changes--add-spare "2027-03-01")))
    (should (= (length test-changes--candidates) 1))
    (should (string= (org-scribe-plan-end-date plan) "2026-11-30"))))

(ert-deftest test-planner-changes-remove-spare-day-move-end ()
  "Removing a spare day, keeping the daily target, brings the end date in."
  (let ((plan (test-changes--plan)))
    (setf (org-scribe-plan-spare-days plan) (list "2026-11-10"))
    (org-scribe-planner--calculate-time plan 1000)
    (should (string= (org-scribe-plan-end-date plan) "2026-12-01"))
    (test-changes--run plan '("Remove spare days" "2026-11-10" "Move the end date"))
    (should (string= (org-scribe-plan-end-date plan) "2026-11-30"))
    (should (= (org-scribe-plan-daily-words plan) 1000))))

(ert-deftest test-planner-changes-move-end-not-offered-without-daily ()
  "With a daily target of 0 there is no daily pace to keep."
  (let* ((plan (test-changes--plan))
         (before (org-scribe-planner--copy-plan plan))
         (test-changes--candidates nil))
    (setf (org-scribe-plan-daily-words before) 0)
    (setf (org-scribe-plan-spare-days plan) (list "2026-11-10"))
    (cl-letf (((symbol-function 'completing-read)
               (test-changes--choose "Keep the end date (change"))
              ((symbol-function 'message) #'ignore))
      (org-scribe-planner--offer-adjustment plan before))
    (should (= (length (car test-changes--candidates)) 2))
    (should-not (cl-some (lambda (c) (string-prefix-p "Move" c))
                         (car test-changes--candidates)))))

(ert-deftest test-planner-changes-failing-option-cannot-be-chosen ()
  "An option that fails on its preview says why and isn't selectable."
  (let ((plan (test-changes--plan))
        (offered nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection predicate &rest _)
                 (setq offered (all-completions "" collection predicate))
                 (car offered)))
              ((symbol-function 'message) #'ignore))
      (org-scribe-planner--choose-change
       plan "Pick:"
       `(("Broken" . ,(lambda (_p) (error "Cannot do that")))
         ("Works" . ,(lambda (p) (setf (org-scribe-plan-total-words p) 40000)
                       (org-scribe-planner--sync-daily p))))))
    (should (equal (length offered) 1))
    (should (string-prefix-p "Works" (car offered)))
    (should (= (org-scribe-plan-total-words plan) 40000))))

;;; One value changes: which of the others to keep

(ert-deftest test-planner-changes-total-keep-writing-days ()
  "A new total, keeping the writing days: the daily target follows."
  (let ((plan (test-changes--plan)))
    (test-changes--run plan '("Total words" "Writing days")
      ((symbol-function 'org-scribe-planner--read-positive-number) (lambda (&rest _) 45000)))
    (should (= (org-scribe-plan-daily-words plan) 1500))
    (should (string= (org-scribe-plan-end-date plan) "2026-11-30"))))

(ert-deftest test-planner-changes-total-keep-daily ()
  "A new total, keeping the daily target: the end date moves."
  (let ((plan (test-changes--plan)))
    (test-changes--run plan '("Total words" "Daily target")
      ((symbol-function 'org-scribe-planner--read-positive-number) (lambda (&rest _) 45000)))
    (should (= (org-scribe-plan-daily-words plan) 1000))
    (should (= (org-scribe-plan-writing-days plan) 45))
    (should (string= (org-scribe-plan-end-date plan) "2026-12-15"))))

(ert-deftest test-planner-changes-daily-keep-writing-days ()
  "A new daily target, keeping the writing days: the total follows."
  (let ((plan (test-changes--plan)))
    (test-changes--run plan '("Daily words" "Writing days")
      ((symbol-function 'org-scribe-planner--read-positive-number) (lambda (&rest _) 1200)))
    (should (= (org-scribe-plan-total-words plan) 36000))
    (should (string= (org-scribe-plan-end-date plan) "2026-11-30"))))

(ert-deftest test-planner-changes-days-keep-daily ()
  "A new length, keeping the daily target: the total follows."
  (let ((plan (test-changes--plan)))
    (test-changes--run plan '("Days available" "Daily target")
      ((symbol-function 'org-scribe-planner--read-days) (lambda (&rest _) 40)))
    (should (= (org-scribe-plan-total-words plan) 40000))
    (should (= (org-scribe-plan-daily-words plan) 1000))
    (should (string= (org-scribe-plan-end-date plan) "2026-12-10"))))

(ert-deftest test-planner-changes-two-values-daily-and-days ()
  "Two values: daily target and days give the total."
  (let ((plan (test-changes--plan))
        (numbers (list 800)))
    (test-changes--run plan '("Two values" "Total words" "Apply")
      ((symbol-function 'org-scribe-planner--read-positive-number)
       (lambda (&rest _) (pop numbers)))
      ((symbol-function 'org-scribe-planner--read-days) (lambda (&rest _) 20)))
    (should (= (org-scribe-plan-total-words plan) 16000))
    (should (string= (org-scribe-plan-end-date plan) "2026-11-20"))))

(ert-deftest test-planner-changes-two-values-total-and-daily ()
  "Two values: total and daily target give the days."
  (let ((plan (test-changes--plan))
        (numbers (list 12000 1500)))
    (test-changes--run plan '("Two values" "Days" "Apply")
      ((symbol-function 'org-scribe-planner--read-positive-number)
       (lambda (&rest _) (pop numbers))))
    (should (= (org-scribe-plan-writing-days plan) 8))
    (should (= (apply #'+ (mapcar (lambda (d) (plist-get d :words))
                                  (org-scribe-planner--generate-day-schedule plan)))
               12000))))

(ert-deftest test-planner-changes-past-days-keep-goals ()
  "Changes made after the plan started leave its past days alone."
  (let ((plan (test-changes--plan))
        (test-changes--today "2026-11-06"))
    (test-changes--run plan '("Total words" "Writing days")
      ((symbol-function 'org-scribe-planner--read-positive-number) (lambda (&rest _) 55000)))
    (let ((goals (mapcar (lambda (d) (plist-get d :words))
                         (org-scribe-planner--generate-day-schedule plan))))
      (should (equal (cl-subseq goals 0 5) (make-list 5 1000)))
      (should (= (apply #'+ goals) 55000)))))

;;; Agenda entries follow the plan

(ert-deftest test-planner-changes-agenda-refreshed-after-change ()
  "A plan synced to the agenda gets its entries regenerated, rest days skipped."
  (let* ((dir (make-temp-file "org-scribe-changes-" t))
         (file (expand-file-name "plan.org" dir))
         (org-scribe-planner-sync-to-agenda t)
         (plan (make-org-scribe-plan :title "P" :total-words 3000 :days 3
                                     :start-date "2026-11-01")))
    (unwind-protect
        (progn
          (org-scribe-planner--calculate-missing-variable plan)
          (org-scribe-planner--save-plan plan file)
          (org-scribe-planner--add-agenda-entries plan file)
          (setf (org-scribe-plan-total-words plan) 6000)
          (setf (org-scribe-plan-locked-goals plan) (list (cons "2026-11-01" 0)))
          (org-scribe-planner--sync-daily plan)
          (org-scribe-planner--save-plan plan file)
          (org-scribe-planner--refresh-agenda-if-synced plan file)
          (with-temp-buffer
            (insert-file-contents file)
            (should (= (how-many "^\\*\\*\\* TODO Write" (point-min) (point-max)) 2))
            (should (= (how-many "^\\*\\*\\* TODO Write 3000 words" (point-min) (point-max)) 2))
            (should (= (how-many "^\\*\\* Agenda Entries" (point-min) (point-max)) 1))))
      (dolist (buffer (buffer-list))
        (when (equal (buffer-file-name buffer) file)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest test-planner-changes-agenda-left-alone-when-never-synced ()
  "A plan never synced to the agenda gets no agenda entries."
  (let* ((dir (make-temp-file "org-scribe-changes-" t))
         (file (expand-file-name "plan.org" dir))
         (plan (make-org-scribe-plan :title "P" :total-words 3000 :days 3
                                     :start-date "2026-11-01")))
    (unwind-protect
        (progn
          (org-scribe-planner--calculate-missing-variable plan)
          (org-scribe-planner--save-plan plan file)
          (org-scribe-planner--refresh-agenda-if-synced plan file)
          (with-temp-buffer
            (insert-file-contents file)
            (should (= (how-many "Agenda Entries" (point-min) (point-max)) 0))))
      (dolist (buffer (buffer-list))
        (when (equal (buffer-file-name buffer) file)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory dir t))))

(provide 'test-planner-changes)
;;; test-planner-changes.el ends here
