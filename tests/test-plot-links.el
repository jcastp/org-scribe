;;; test-plot-links.el --- Tests for plot thread linking system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for the plot thread linking module.
;; Basic functionality tests to ensure the module loads
;; and core functions are available.
;;
;; Note: The helper functions for extracting text from ID links
;; are tested in test-search-links.el and work for all link types
;; (characters, locations, and plot threads).

;;; Code:

(require 'ert)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../search" default-directory))
  (add-to-list 'load-path (expand-file-name "../linking" default-directory))
  (add-to-list 'load-path (expand-file-name "../capture" default-directory)))

(require 'org-scribe-plot-links)

;;; Module Loading Tests

(ert-deftest test-plot-links-module-loads ()
  "Test that org-scribe-plot-links module loads without errors."
  (should (featurep 'org-scribe-plot-links)))

;;; Function Availability Tests

(ert-deftest test-plot-links-functions-defined ()
  "Test that all public plot thread linking functions are defined."
  ;; Core functions
  (should (fboundp 'org-scribe-add-plot-thread-ids))
  (should (fboundp 'org-scribe-insert-plot-thread-link))
  (should (fboundp 'org-scribe-insert-multiple-plot-thread-links))
  (should (fboundp 'org-scribe-set-scene-plot-threads))
  (should (fboundp 'org-scribe-jump-to-plot-thread))

  ;; Batch operations
  (should (fboundp 'org-scribe-link-scene-plot-threads))
  (should (fboundp 'org-scribe-link-all-scene-plot-threads))

  ;; Setup wizard
  (should (fboundp 'org-scribe-setup-plot-thread-links)))

;;; Helper Function Tests

(ert-deftest test-plot-thread-link-creation ()
  "Test plot thread link creation with ID alist."
  (let* ((id-alist '(("Main Plot" . ("plot-main-001" . "Main Plot"))
                     ("Subplot" . ("plot-sub-001" . "Subplot: Romance"))))
         (link1 (org-scribe--create-plot-thread-link "Main Plot" id-alist))
         (link2 (org-scribe--create-plot-thread-link "Subplot" id-alist))
         (link3 (org-scribe--create-plot-thread-link "Unknown" id-alist)))

    ;; Should create ID link for known thread
    (should (string= link1 "[[id:plot-main-001][Main Plot]]"))
    (should (string= link2 "[[id:plot-sub-001][Subplot]]"))

    ;; Should return plain text for unknown thread (fallback)
    (should (string= link3 "Unknown"))))

(ert-deftest test-plot-thread-name-extraction ()
  "Test extracting plot thread name from heading."
  ;; This is a simple wrapper around org functions
  ;; Just verify it's callable
  (should (fboundp 'org-scribe--get-plot-thread-name-at-point)))

(ert-deftest test-plot-thread-file-detection ()
  "Test plot thread file detection."
  ;; Verify the function exists and is callable
  (should (fboundp 'org-scribe--get-plot-thread-file))

  ;; The actual behavior depends on project structure
  ;; which requires a full project setup, so we just
  ;; verify the function is defined)
  )

;;; Integration Tests (require project structure)

(ert-deftest test-plot-thread-database-structure ()
  "Test that plot thread database returns correct structure."
  ;; The function should return nil if no plot file exists
  ;; or a list of (NAME . (ID . HEADING)) tuples
  (let ((result (org-scribe--get-all-plot-threads)))
    ;; Result should be either nil or a list
    (should (or (null result)
                (listp result)))

    ;; If not nil, each element should be a cons cell
    (when result
      (dolist (item result)
        (should (consp item))
        (should (stringp (car item)))  ; Name is a string
        (should (consp (cdr item)))     ; (ID . HEADING) is a cons
        (should (stringp (cadr item)))  ; ID is a string
        ))))

;;; Phase 2 Function Tests

(ert-deftest test-plot-thread-report-function-defined ()
  "Test that plot thread report function is defined."
  (should (fboundp 'org-scribe-plot-thread-report)))

(ert-deftest test-plot-thread-stats-function-defined ()
  "Test that plot thread statistics function is defined."
  (should (fboundp 'org-scribe-plot-thread-stats)))

(ert-deftest test-plot-thread-timeline-dblock-defined ()
  "Test that timeline dynamic block function is defined."
  (should (fboundp 'org-dblock-write:plot-thread-timeline)))

;;; Health Report Coverage Tests (M2)

(defmacro test-plot--with-novel-and-plot-files (novel-content plot-content &rest body)
  "Bind temp novel/plot files with NOVEL-CONTENT/PLOT-CONTENT, run BODY.
Stubs `org-scribe-project-structure' and `org-scribe-project-type' so the
plot module resolves both files without a real project on disk."
  (declare (indent 2))
  `(let ((temp-novel (make-temp-file "test-plot-novel-" nil ".org"))
         (temp-plot (make-temp-file "test-plot-plot-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file temp-novel (insert ,novel-content))
           (with-temp-file temp-plot (insert ,plot-content))
           (cl-letf (((symbol-function 'org-scribe-project-structure)
                      (lambda () (list :novel-file temp-novel :plot-file temp-plot)))
                     ((symbol-function 'org-scribe-project-type)
                      (lambda () 'novel)))
             ,@body))
       (delete-file temp-novel)
       (delete-file temp-plot))))

(ert-deftest test-plot-report-total-scenes-counts-scenes-without-plot ()
  "\"Total scenes\" in the health report must count every scene, not just
those with a Plot property.
Regression test for M2: previously \"Total scenes\" and \"Scenes with
plot threads\" were both computed from the same plot-carrying-scenes
list, so they were always equal and never reflected scenes missing a
Plot property — the very thing the report is meant to flag."
  (test-plot--with-novel-and-plot-files
      (concat "* Chapter 1\n"
              "*** Scene 1\n:PROPERTIES:\n:Plot: Main Plot\n:END:\n"
              "*** Scene 2\n:PROPERTIES:\n:Plot: Main Plot\n:END:\n"
              "*** Scene 3\n")  ; no Plot property
      "* Main Plot\n:PROPERTIES:\n:ID: plot-main-001\n:END:\n"
    (org-scribe-plot-thread-report)
    (with-current-buffer "*Plot Thread Health Report*"
      (let ((text (buffer-string)))
        (should (string-match-p "Total scenes: 3" text))
        (should (string-match-p "Scenes with plot threads: 2" text))))
    (kill-buffer "*Plot Thread Health Report*")))

(ert-deftest test-plot-report-coverage-uses-true-total-not-plot-carrying-scenes ()
  "Thread coverage percentage must be computed against the true total
scene count, not just scenes that already carry a Plot property.
Regression test for M2: a thread appearing in every plot-carrying scene
used to be reported as 100% coverage even when most of the manuscript's
scenes had no Plot property at all — overstating coverage in exactly the
case the report exists to catch."
  (test-plot--with-novel-and-plot-files
      (concat "* Chapter 1\n"
              "*** Scene 1\n:PROPERTIES:\n:Plot: Main Plot\n:END:\n"
              "*** Scene 2\n"   ; no Plot property
              "*** Scene 3\n"   ; no Plot property
              "*** Scene 4\n")  ; no Plot property
      "* Main Plot\n:PROPERTIES:\n:ID: plot-main-001\n:END:\n"
    (org-scribe-plot-thread-report)
    (with-current-buffer "*Plot Thread Health Report*"
      (let ((text (buffer-string)))
        ;; 1 of 4 total scenes = 25.0%, not 1 of 1 plot-carrying scene = 100.0%
        (should (string-match-p "Scenes: 1 of 4 (25\\.0%)" text))))
    (kill-buffer "*Plot Thread Health Report*")))

(ert-deftest test-plot-thread-helper-functions ()
  "Test that helper functions for analysis are defined."
  (should (fboundp 'org-scribe--get-all-scenes-with-plots))
  (should (fboundp 'org-scribe--find-thread-in-scenes))
  (should (fboundp 'org-scribe--calculate-thread-gap)))

(ert-deftest test-calculate-thread-gap ()
  "Test thread gap calculation."
  (let* ((all-scenes '(("Scene 1" "Ch 1" ("A"))
                       ("Scene 2" "Ch 1" ("A"))
                       ("Scene 3" "Ch 2" ("B"))
                       ("Scene 4" "Ch 2" ("B"))
                       ("Scene 5" "Ch 3" ("A"))
                       ("Scene 6" "Ch 3" ("A"))))
         (appearances '(("Scene 1" "Ch 1" ("A"))
                       ("Scene 2" "Ch 1" ("A"))
                       ("Scene 5" "Ch 3" ("A"))
                       ("Scene 6" "Ch 3" ("A"))))
         (gap (org-scribe--calculate-thread-gap appearances all-scenes)))
    ;; Thread A appears in scenes 1,2,5,6 - gap of 2 scenes (3 and 4)
    (should (= gap 2))))

;;; Plot Thread File Resolution Tests

(ert-deftest test-get-plot-thread-file-resolves-spanish-trama-file ()
  "org-scribe--get-plot-thread-file finds objects/trama.org in Spanish projects."
  (let* ((temp-dir (make-temp-file "org-scribe-plot-file-es-" t))
         (org-scribe--project-type-cache nil))
    (unwind-protect
        (let ((default-directory temp-dir))
          (make-directory (expand-file-name "objects" temp-dir) t)
          (with-temp-file (expand-file-name "objects/trama.org" temp-dir)
            (insert "* Trama\n"))
          (should (equal (file-truename (org-scribe--get-plot-thread-file))
                         (file-truename (expand-file-name "objects/trama.org" temp-dir)))))
      (setq org-scribe--project-type-cache nil)
      (delete-directory temp-dir t))))

(ert-deftest test-get-plot-thread-file-resolves-english-plot-file ()
  "org-scribe--get-plot-thread-file finds objects/plot.org in English projects."
  (let* ((temp-dir (make-temp-file "org-scribe-plot-file-en-" t))
         (org-scribe--project-type-cache nil))
    (unwind-protect
        (let ((default-directory temp-dir))
          (make-directory (expand-file-name "objects" temp-dir) t)
          (with-temp-file (expand-file-name "objects/plot.org" temp-dir)
            (insert "* Plot\n"))
          (should (equal (file-truename (org-scribe--get-plot-thread-file))
                         (file-truename (expand-file-name "objects/plot.org" temp-dir)))))
      (setq org-scribe--project-type-cache nil)
      (delete-directory temp-dir t))))

;;; Heading Predicate Tests

(ert-deftest test-plot-heading-p-detects-top-level ()
  "org-scribe--plot-heading-p matches a level-1 plot thread heading."
  (with-temp-buffer
    (org-mode)
    (insert "* Main Plot\n** Escalation\n")
    (goto-char (point-min))
    (org-back-to-heading)
    (should (org-scribe--plot-heading-p))))

(ert-deftest test-plot-heading-p-rejects-subsection ()
  "org-scribe--plot-heading-p must not match a subsection under a plot thread heading."
  (with-temp-buffer
    (org-mode)
    (insert "* Main Plot\n** Thread Notes\n** Resolution\n")
    (goto-char (point-min))
    (dolist (heading '("Thread Notes" "Resolution"))
      (goto-char (point-min))
      (search-forward heading)
      (org-back-to-heading)
      (should-not (org-scribe--plot-heading-p)))))

;;; Short-story Heading Predicate Tests (H10)

(ert-deftest test-plot-heading-p-short-story-matches-level-2-under-plot-threads ()
  "In short-story projects, plot threads are level-2 headings under
\"* Plot Threads\" (see the shipped notes.org template and
`org-scribe-capture-plot-thread', which files new threads there), not
level-1.  Regression test for H10: the predicate previously required
level 1 unconditionally, so short-story plot threads were never found."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Plot Threads\n\n** Betrayal Subplot\n:PROPERTIES:\n:THREAD-TYPE: Subplot\n:END:\n")
      (goto-char (point-min))
      (search-forward "Betrayal")
      (org-back-to-heading)
      (should (org-scribe--plot-heading-p)))))

(ert-deftest test-plot-heading-p-short-story-rejects-plot-threads-wrapper ()
  "The level-1 \"* Plot Threads\" section header itself is not an entity.
Regression test for H10: its own heading text contains \"Thread\" and
would otherwise match the regexp fallback, becoming a phantom entity."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Plot Threads\n\n** Betrayal Subplot\n:PROPERTIES:\n:THREAD-TYPE: Subplot\n:END:\n")
      (goto-char (point-min))
      (org-back-to-heading)
      (should-not (org-scribe--plot-heading-p)))))

;;; Run tests

(defun org-scribe-plot-links-run-tests ()
  "Run all plot thread linking tests."
  (interactive)
  (ert "^test-plot-"))

(provide 'test-plot-links)

;;; test-plot-links.el ends here
