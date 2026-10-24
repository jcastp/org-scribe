;;; test-planner-presets.el --- Tests for preset plans and new-plan defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for starting a new plan from a preset (`org-scribe-planner-presets'),
;; with the preset for the project's type offered first and its values and
;; suggestions open to change, and for the defaults offered to plans made
;; from scratch (pattern, weekends).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-scribe-planner)

;;; Helpers

(defmacro test-presets--wizard (answers &rest body)
  "Run BODY around the new-plan wizard with I/O stubbed.
ANSWERS is a plist: :start (preset label prefix or \"From scratch\"),
:yes (list of y-or-n-p answers, in order), :numbers (answers to the
number prompts; nil means accept the default), :type (the project type).
In BODY, `plan' is the plan created and `questions' the y-or-n-p prompts."
  (declare (indent 1))
  `(let* ((org-scribe-planner-after-plan-load-hook nil)
          (org-scribe-planner--current-plan nil)
          (org-scribe-planner--current-plan-file nil)
          (org-scribe-planner-wordcount-function nil)
          (yes (plist-get ,answers :yes))
          (numbers (plist-get ,answers :numbers))
          (start (plist-get ,answers :start))
          (questions nil))
     (cl-letf (((symbol-function 'org-scribe-project-type)
                (lambda () (or (plist-get ,answers :type) 'unknown)))
               ((symbol-function 'completing-read)
                (lambda (prompt collection &rest _)
                  (if (string-prefix-p "Start from" prompt)
                      (or (cl-find-if (lambda (c) (string-prefix-p start c))
                                      (all-completions "" collection))
                          start)
                    ;; Scratch plans: total and days known
                    "Total words + Daily words → Calculate days needed")))
               ((symbol-function 'org-scribe-planner--read-positive-number)
                (lambda (_prompt &optional default) (or (pop numbers) default)))
               ((symbol-function 'org-scribe-planner--read-non-negative-number)
                (lambda (_prompt &optional default) (or default 0)))
               ((symbol-function 'org-scribe-planner--read-days)
                (lambda (_prompt &optional default) (or (pop numbers) default)))
               ((symbol-function 'org-scribe-planner--read-date)
                (lambda (&rest _) "2026-11-02"))
               ((symbol-function 'y-or-n-p)
                (lambda (prompt) (push prompt questions) (pop yes)))
               ((symbol-function 'org-scribe-planner--save-plan) #'ignore)
               ((symbol-function 'org-scribe-planner-show-calendar) #'ignore)
               ((symbol-function 'org-scribe-planner--get-today-date) (lambda () "2026-10-25"))
               ((symbol-function 'message) #'ignore))
       (org-scribe-planner-new-plan "P" "/tmp/unused-plan.org")
       (let ((plan org-scribe-planner--current-plan))
         (setq questions (nreverse questions))
         ,@body))))

(defun test-presets--goals (plan)
  "PLAN's whole-word day goals."
  (mapcar (lambda (d) (plist-get d :words)) (org-scribe-planner--generate-day-schedule plan)))

;;; The preset question

(ert-deftest test-planner-presets-label ()
  "A preset is described with its numbers and pattern."
  (should (string= (org-scribe-planner--describe-preset
                    "Short story" '(:description "A short story in a week" :total 5000
                                    :days 7 :pattern rising))
                   "Short story — A short story in a week (5000 words in 7 days, rising)"))
  (should (string= (org-scribe-planner--describe-preset
                    "500 words a day" '(:description "A habit" :total 15000 :daily 500
                                        :pattern flat))
                   "500 words a day — A habit (15000 words at 500 a day)")))

(ert-deftest test-planner-presets-default-follows-project-type ()
  "The preset for the project's type is the default answer."
  (dolist (case '((novel . "Novel draft") (short-story . "Short story") (unknown . "From scratch")))
    (let ((offered-default nil))
      (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () (car case)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt _collection _pred _req _init _hist default)
                   (setq offered-default default)
                   "From scratch")))
        (should-not (org-scribe-planner--read-preset)))
      (should (string-prefix-p (cdr case) offered-default)))))

(ert-deftest test-planner-presets-none-configured-asks-nothing ()
  "With no presets the question isn't asked."
  (let ((org-scribe-planner-presets nil))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (error "Asked"))))
      (should-not (org-scribe-planner--read-preset)))))

;;; New plans from a preset

(ert-deftest test-planner-presets-short-story-with-suggestions ()
  "Short story preset, its rising pattern and weekend goal accepted."
  (test-presets--wizard '(:start "Short story" :yes (t t nil) :type short-story)
    (should (= (org-scribe-plan-total-words plan) 5000))
    (should (string= (org-scribe-plan-end-date plan) "2026-11-08"))
    (should (eq (org-scribe-plan-pattern plan) 'rising))
    (should (eq (org-scribe-plan-weekend-mode plan) 'different))
    ;; 1.5 × the 714 words a day of a flat week
    (should (= (org-scribe-plan-weekend-daily plan) 1071))
    (should (= (apply #'+ (test-presets--goals plan)) 5000))
    (should (string-match-p "suggested rising pattern" (nth 0 questions)))
    (should (string-match-p "Suggested weekends: a goal of their own" (nth 1 questions)))))

(ert-deftest test-planner-presets-suggestions-declined ()
  "Declining the suggestions leaves the plan steady, with ordinary weekends."
  (test-presets--wizard '(:start "Short story" :yes (nil nil nil))
    (should (eq (org-scribe-plan-pattern plan) 'flat))
    (should (eq (org-scribe-plan-weekend-mode plan) 'same))
    (should (= (apply #'+ (test-presets--goals plan)) 5000))))

(ert-deftest test-planner-presets-values-can-change ()
  "The preset's numbers are defaults: a different total and length are kept."
  (test-presets--wizard '(:start "Novel draft" :yes (nil nil) :numbers (90000 90))
    (should (= (org-scribe-plan-total-words plan) 90000))
    (should (= (org-scribe-plan-writing-days plan) 90))
    (should (= (org-scribe-plan-daily-words plan) 1000))))

(ert-deftest test-planner-presets-daily-preset-calculates-days ()
  "A preset given in words a day gets the days it needs."
  (test-presets--wizard '(:start "500 words a day" :yes (nil))
    (should (= (org-scribe-plan-daily-words plan) 500))
    (should (= (org-scribe-plan-writing-days plan) 30))
    ;; Steady and ordinary weekends: only the spare-days question
    (should (= (length questions) 1))))

;;; Defaults for plans made from scratch

(ert-deftest test-planner-presets-defaults-applied-to-scratch-plans ()
  "Default valley pattern and weekends off, accepted; the days follow."
  (let ((org-scribe-planner-default-pattern 'valley)
        (org-scribe-planner-default-weekend-mode 'rest))
    (test-presets--wizard '(:start "From scratch" :yes (t t nil) :numbers (10000 1000))
      (should (eq (org-scribe-plan-pattern plan) 'valley))
      (should (equal (org-scribe-plan-pattern-config plan) '(:peak 1.5 :valley 0.7)))
      (should (eq (org-scribe-plan-weekend-mode plan) 'rest))
      ;; 10 weekdays from Monday Nov 2, the weekends off: Friday Nov 13
      (should (string= (org-scribe-plan-end-date plan) "2026-11-13"))
      (should (= (apply #'+ (test-presets--goals plan)) 10000))
      (should (string-match-p "default valley pattern" (nth 0 questions))))))

(ert-deftest test-planner-presets-default-intensity-and-weekend-goal ()
  "The default intensity and weekend goal are used."
  (let ((org-scribe-planner-default-pattern 'rising)
        (org-scribe-planner-default-pattern-intensity 'extreme)
        (org-scribe-planner-default-weekend-mode 'different)
        (org-scribe-planner-default-weekend-daily 2000))
    (test-presets--wizard '(:start "From scratch" :yes (t t nil) :numbers (14000 1000))
      (should (equal (org-scribe-plan-pattern-config plan) '(:start 0.3 :end 2.0)))
      (should (= (org-scribe-plan-weekend-daily plan) 2000)))))

(ert-deftest test-planner-presets-steady-defaults-ask-nothing-more ()
  "With the defaults left steady, scratch plans are asked nothing new."
  (test-presets--wizard '(:start "From scratch" :yes (nil) :numbers (10000 1000))
    (should (eq (org-scribe-plan-pattern plan) 'flat))
    (should (= (length questions) 1))
    (should (string-match-p "spare" (car questions)))))

;;; The project's total is the default total

(defmacro test-presets--with-project-target (target &rest body)
  "Run BODY with the current project's recorded word target stubbed as TARGET."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'org-scribe-planner--project-word-target)
              (lambda () ,target)))
     ,@body))

(ert-deftest test-planner-presets-project-target-is-the-default-total ()
  "In a project with a recorded target, the preset's total gives way to it."
  (test-presets--with-project-target 50000
    (test-presets--wizard '(:start "Novel draft" :yes (nil nil))
      (should (= (org-scribe-plan-total-words plan) 50000)))))

(ert-deftest test-planner-presets-without-a-project-target-keep-the-preset-total ()
  (test-presets--with-project-target nil
    (test-presets--wizard '(:start "Novel draft" :yes (nil nil))
      (should (= (org-scribe-plan-total-words plan) 80000)))))

(ert-deftest test-planner-presets-project-target-is-offered-from-scratch ()
  (test-presets--with-project-target 50000
    (test-presets--wizard '(:start "From scratch" :yes (nil nil) :numbers (nil 500))
      (should (= (org-scribe-plan-total-words plan) 50000)))))

(provide 'test-planner-presets)
;;; test-planner-presets.el ends here
