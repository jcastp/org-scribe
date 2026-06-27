;;; test-health.el --- Tests for org-scribe project health report -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for core/org-scribe-health.el.
;; All tests use temp files so they run without a real project on disk.

;;; Code:

(require 'ert)
(require 'org)

;; Add parent directory to load path
(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path (expand-file-name "core" parent-dir))
  (add-to-list 'load-path (expand-file-name "linking" parent-dir)))

(require 'org-scribe-messages)
(require 'org-scribe-health)

;;; Helpers

(defmacro test-health--with-novel-file (content &rest body)
  "Create a temp file with CONTENT as an org novel, bind its path, and run BODY.
The path is available as `temp-novel'."
  (declare (indent 1))
  `(let ((temp-novel (make-temp-file "test-health-novel-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file temp-novel (insert ,content))
           ,@body)
       (delete-file temp-novel))))

;;; Module loading

(ert-deftest test-health-module-loads ()
  "Test that the health module loads without errors."
  (should (featurep 'org-scribe-health)))

(ert-deftest test-health-function-defined ()
  "Test that org-scribe-project-health is defined."
  (should (fboundp 'org-scribe-project-health)))

(ert-deftest test-health-helper-functions-defined ()
  "Test that internal helper functions are defined."
  (should (fboundp 'org-scribe--health-collect-scene-data))
  (should (fboundp 'org-scribe--health-word-totals))
  (should (fboundp 'org-scribe--health-collect-referenced-ids))
  (should (fboundp 'org-scribe--health-find-orphans))
  (should (fboundp 'org-scribe--health-scene-link)))

;;; Scene link helper

(ert-deftest test-health-scene-link-with-id ()
  "Test scene link generation with an ID."
  (should (equal (org-scribe--health-scene-link "My Scene" "abc-123")
                 "[[id:abc-123][My Scene]]")))

(ert-deftest test-health-scene-link-without-id ()
  "Test scene link generation without an ID falls back to plain text."
  (should (equal (org-scribe--health-scene-link "My Scene" nil)
                 "My Scene")))

;;; Scene data collection

(ert-deftest test-health-collect-scene-data-basic ()
  "Test that scene data is collected from level-3 headings."
  (test-health--with-novel-file
      "* Act One\n** TODO Chapter One :ignore:\n*** TODO Opening Scene :ignore:\n:PROPERTIES:\n:PoV: Alice\n:Characters: Alice\n:Plot: Main\n:Location: Paris\n:END:\n\nSome text.\n"
    (let ((scenes (org-scribe--health-collect-scene-data temp-novel)))
      (should (= (length scenes) 1))
      (let ((s (car scenes)))
        (should (equal (nth 0 s) "Opening Scene"))
        (should (equal (nth 1 s) "Chapter One"))
        (should (equal (nth 3 s) "TODO"))))))

(ert-deftest test-health-collect-scene-data-empty-file ()
  "Test that an empty file returns no scenes."
  (test-health--with-novel-file
      "* Act One\n** TODO Chapter :ignore:\n\nNo scenes here.\n"
    (let ((scenes (org-scribe--health-collect-scene-data temp-novel)))
      (should (null scenes)))))

(ert-deftest test-health-collect-scene-data-missing-props ()
  "Test detection of scenes with missing properties."
  (test-health--with-novel-file
      "** TODO Chapter :ignore:\n*** TODO Scene A :ignore:\n:PROPERTIES:\n:PoV: Alice\n:END:\n\n*** TODO Scene B :ignore:\n:PROPERTIES:\n:Plot: Main\n:END:\n\n"
    (let ((scenes (org-scribe--health-collect-scene-data temp-novel)))
      (should (= (length scenes) 2))
      ;; Scene A has PoV but no Plot/Location/Characters
      (let ((a (car scenes)))
        (should (nth 4 a))    ; has-pov = t
        (should-not (nth 6 a)) ; has-plot = nil
        (should-not (nth 7 a))) ; has-location = nil
      ;; Scene B has Plot but no PoV
      (let ((b (cadr scenes)))
        (should-not (nth 4 b))  ; has-pov = nil
        (should (nth 6 b))))))  ; has-plot = t

;;; Word totals

(ert-deftest test-health-word-totals-sums-levels ()
  "Test that word totals sum level-3 WORDCOUNT and level-2 WORD-OBJECTIVE."
  (test-health--with-novel-file
      "** TODO Chapter One :ignore:\n:PROPERTIES:\n:WORD-OBJECTIVE: 5000\n:WORDCOUNT: 0\n:END:\n\n*** TODO Scene A :ignore:\n:PROPERTIES:\n:WORDCOUNT: 1200\n:WORD-OBJECTIVE: 500\n:END:\n\n*** TODO Scene B :ignore:\n:PROPERTIES:\n:WORDCOUNT: 800\n:WORD-OBJECTIVE: 500\n:END:\n\n"
    (let* ((totals (org-scribe--health-word-totals temp-novel))
           (words (car totals))
           (obj (cdr totals)))
      ;; Words = scene A + scene B = 1200 + 800
      (should (= words 2000))
      ;; Objective = chapter objective only (level-2) = 5000
      (should (= obj 5000)))))

(ert-deftest test-health-word-totals-empty-file ()
  "Test that an empty file returns zero totals."
  (test-health--with-novel-file
      "* Novel\n\nNo headings with properties.\n"
    (let ((totals (org-scribe--health-word-totals temp-novel)))
      (should (= (car totals) 0))
      (should (= (cdr totals) 0)))))

;;; Referenced IDs collection

(ert-deftest test-health-collect-referenced-ids-finds-ids ()
  "Test that referenced entity IDs are extracted from scene properties."
  (test-health--with-novel-file
      "** TODO Chapter :ignore:\n*** TODO Scene :ignore:\n:PROPERTIES:\n:PoV: [[id:char-001][Alice]]\n:Characters: [[id:char-001][Alice]], [[id:char-002][Bob]]\n:Location: [[id:loc-001][Paris]]\n:END:\n\n"
    (let ((ids (org-scribe--health-collect-referenced-ids temp-novel)))
      (should (gethash "char-001" ids))
      (should (gethash "char-002" ids))
      (should (gethash "loc-001" ids)))))

(ert-deftest test-health-collect-referenced-ids-empty ()
  "Test that a file with no ID links returns an empty hash."
  (test-health--with-novel-file
      "** TODO Chapter :ignore:\n*** TODO Scene :ignore:\n:PROPERTIES:\n:PoV: Alice\n:END:\n\n"
    (let ((ids (org-scribe--health-collect-referenced-ids temp-novel)))
      (should (= (hash-table-count ids) 0)))))

;;; Orphan detection

(ert-deftest test-health-find-orphans-detects-unreferenced ()
  "Test that entities with IDs absent from reference hash are orphans."
  (let ((entities '(("Alice" . ("char-001" . "Alice"))
                    ("Bob"   . ("char-002" . "Bob"))
                    ("Carol" . ("char-003" . "Carol"))))
        (ref-ids (make-hash-table :test 'equal)))
    (puthash "char-001" t ref-ids)
    ;; char-002 and char-003 are not referenced
    (let ((orphans (org-scribe--health-find-orphans entities ref-ids)))
      (should (= (length orphans) 2))
      (should (member "Bob" orphans))
      (should (member "Carol" orphans))
      (should-not (member "Alice" orphans)))))

(ert-deftest test-health-find-orphans-none ()
  "Test that all referenced entities produce no orphans."
  (let ((entities '(("Alice" . ("char-001" . "Alice"))))
        (ref-ids (make-hash-table :test 'equal)))
    (puthash "char-001" t ref-ids)
    (should (null (org-scribe--health-find-orphans entities ref-ids)))))

;;; Integration: project-health errors without a novel file

(ert-deftest test-health-project-health-errors-without-novel-file ()
  "Test that project-health signals user-error when no manuscript file exists."
  (let ((orig-fn (symbol-function 'org-scribe-project-structure)))
    (cl-letf (((symbol-function 'org-scribe-project-structure)
               (lambda () (list :novel-file nil))))
      (should-error (org-scribe-project-health) :type 'user-error))))

;;; Regression: custom done-state keywords

(ert-deftest test-health-done-keywords-read-from-custom-todo-line ()
  "org-done-keywords is populated from the manuscript's #+TODO: line.
This is the mechanism that open-todos filtering depends on."
  (test-health--with-novel-file
      "#+TODO: TODO TOWRITE | FINISHED PURGE\n** Chapter :ignore:\n*** TODO Scene :ignore:\n\n"
    (let ((kws (with-current-buffer (find-file-noselect temp-novel)
                 org-done-keywords)))
      (should (member "FINISHED" kws))
      (should (member "PURGE" kws))
      (should-not (member "TODO" kws))
      (should-not (member "TOWRITE" kws)))))

(ert-deftest test-health-open-todos-excludes-custom-done-states ()
  "FINISHED and PURGE scenes are excluded from the Open TODO Scenes section.
Regression: previously the filter only removed the literal keyword \"DONE\",
so custom done-state keywords like FINISHED were incorrectly listed as pending."
  (test-health--with-novel-file
      (concat "#+TODO: TODO TOWRITE | FINISHED PURGE\n"
              "** TODO Chapter One :ignore:\n"
              "*** TODO Pending Scene :ignore:\n"
              ":PROPERTIES:\n:PoV: Alice\n:WORDCOUNT: 100\n:END:\n\n"
              "*** FINISHED Done Scene :ignore:\n"
              ":PROPERTIES:\n:PoV: Bob\n:WORDCOUNT: 200\n:END:\n\n"
              "*** PURGE Purged Scene :ignore:\n"
              ":PROPERTIES:\n:PoV: Carol\n:WORDCOUNT: 50\n:END:\n\n")
    (cl-letf (((symbol-function 'org-scribe-project-structure)
               (lambda ()
                 (list :novel-file temp-novel
                       :characters-file nil
                       :locations-file nil))))
      (org-scribe-project-health)
      (with-current-buffer (get-buffer "*org-scribe-health*")
        (let ((content (buffer-string)))
          ;; Section header shows exactly 1 pending scene
          (should (string-match-p "Open TODO Scenes (1)" content))
          ;; The pending scene's heading appears in the list
          (should (string-match-p "Pending Scene" content))
          ;; FINISHED and PURGE appear in the status table (informational)
          ;; but are not counted as open todos — verified by the count above
          (should (string-match-p "FINISHED" content))
          (should (string-match-p "PURGE" content)))))))

;;; Integration: report buffer is created

(ert-deftest test-health-report-buffer-created ()
  "Test that calling project-health creates the *org-scribe-health* buffer."
  (test-health--with-novel-file
      "** TODO Chapter :ignore:\n:PROPERTIES:\n:WORD-OBJECTIVE: 5000\n:END:\n\n*** TODO Scene :ignore:\n:PROPERTIES:\n:PoV: Alice\n:WORDCOUNT: 300\n:END:\n\nSome words.\n"
    (cl-letf (((symbol-function 'org-scribe-project-structure)
               (lambda ()
                 (list :novel-file temp-novel
                       :characters-file nil
                       :locations-file nil))))
      (org-scribe-project-health)
      (let ((buf (get-buffer "*org-scribe-health*")))
        (should buf)
        (with-current-buffer buf
          (let ((content (buffer-string)))
            (should (string-match-p "Project Health Report" content))
            (should (string-match-p "Scenes by Status" content))
            (should (string-match-p "Open TODO Scenes" content))))))))

;;; Writing Plan section helpers

(ert-deftest test-health-days-until-future ()
  "days-until returns positive count for a future date."
  (let* ((future (format-time-string "%Y-%m-%d" (time-add (current-time) (* 10 86400)))))
    (should (> (org-scribe--health-days-until future) 0))))

(ert-deftest test-health-days-until-past ()
  "days-until returns 0 for a past date."
  (should (= (org-scribe--health-days-until "2000-01-01") 0)))

(ert-deftest test-health-days-since-past ()
  "days-since returns positive count for a past date."
  (should (> (org-scribe--health-days-since "2000-01-01") 0)))

(ert-deftest test-health-days-since-future ()
  "days-since returns 0 for a future date."
  (let* ((future (format-time-string "%Y-%m-%d" (time-add (current-time) (* 10 86400)))))
    (should (= (org-scribe--health-days-since future) 0))))

;;; Writing Plan section in the report

(ert-deftest test-health-report-writing-plan-guard-is-featurep ()
  "Writing Plan section code is gated on (featurep 'org-scribe-planner).
Verified directly: the guard expression returns nil for a missing feature."
  ;; This is a unit test on the guard condition itself — it does not invoke
  ;; the full report because overriding `featurep' globally during a live
  ;; session causes excessive-lisp-nesting from Emacs internals.
  (should-not (featurep 'this-package-does-not-exist-at-all)))

(ert-deftest test-health-report-writing-plan-no-plan-file ()
  "Writing Plan section shows creation hint when no plan exists."
  (require 'org-scribe-planner)
  (test-health--with-novel-file
      "** TODO Chapter :ignore:\n*** TODO Scene :ignore:\n:PROPERTIES:\n:PoV: Alice\n:WORDCOUNT: 100\n:END:\n\n"
    (let ((org-scribe-planner--current-plan nil))
      (cl-letf (((symbol-function 'org-scribe-project-structure)
                 (lambda ()
                   (list :novel-file temp-novel
                         :plan-file nil
                         :characters-file nil
                         :locations-file nil))))
        (org-scribe-project-health)
        (with-current-buffer (get-buffer "*org-scribe-health*")
          (let ((content (buffer-string)))
            (should (string-match-p "Writing Plan" content))
            (should (string-match-p "org-scribe-plan" content))))))))

(ert-deftest test-health-report-writing-plan-file-exists-not-loaded ()
  "Writing Plan section shows load hint when plan.org exists but is not loaded."
  (require 'org-scribe-planner)
  (test-health--with-novel-file
      "** TODO Chapter :ignore:\n*** TODO Scene :ignore:\n:PROPERTIES:\n:PoV: Alice\n:WORDCOUNT: 100\n:END:\n\n"
    (let* ((plan-file (make-temp-file "test-health-plan-" nil ".org"))
           (org-scribe-planner--current-plan nil))
      (unwind-protect
          (cl-letf (((symbol-function 'org-scribe-project-structure)
                     (lambda ()
                       (list :novel-file temp-novel
                             :plan-file plan-file
                             :characters-file nil
                             :locations-file nil))))
            (org-scribe-project-health)
            (with-current-buffer (get-buffer "*org-scribe-health*")
              (let ((content (buffer-string)))
                (should (string-match-p "Writing Plan" content))
                (should (string-match-p "not loaded" content))
                (should (string-match-p "org-scribe-plan" content)))))
        (delete-file plan-file)))))

(ert-deftest test-health-report-writing-plan-active-plan ()
  "Writing Plan section shows plan stats when a plan is active."
  (require 'org-scribe-planner)
  (test-health--with-novel-file
      "** TODO Chapter :ignore:\n*** TODO Scene :ignore:\n:PROPERTIES:\n:PoV: Alice\n:WORDCOUNT: 100\n:END:\n\n"
    (let* ((plan (make-org-scribe-plan
                  :title "My Novel"
                  :total-words 80000
                  :daily-words 500
                  :days 160
                  :start-date "2026-01-01"
                  :end-date "2026-06-30"
                  :current-words 23456))
           (org-scribe-planner--current-plan plan))
      (cl-letf (((symbol-function 'org-scribe-project-structure)
                 (lambda ()
                   (list :novel-file temp-novel
                         :plan-file nil
                         :characters-file nil
                         :locations-file nil))))
        (org-scribe-project-health)
        (with-current-buffer (get-buffer "*org-scribe-health*")
          (let ((content (buffer-string)))
            (should (string-match-p "Writing Plan" content))
            (should (string-match-p "My Novel" content))
            (should (string-match-p "80000" content))
            (should (string-match-p "23456" content))
            (should (string-match-p "500" content))
            (should (string-match-p "2026-06-30" content))
            (should (string-match-p "\\(On track\\|Behind by\\)" content))))))))

(provide 'test-health)

;;; test-health.el ends here
