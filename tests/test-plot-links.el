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
  (should (fboundp 'org-scribe/add-plot-thread-ids))
  (should (fboundp 'org-scribe/insert-plot-thread-link))
  (should (fboundp 'org-scribe/insert-multiple-plot-thread-links))
  (should (fboundp 'org-scribe/set-scene-plot-threads))
  (should (fboundp 'org-scribe/jump-to-plot-thread))

  ;; Batch operations
  (should (fboundp 'org-scribe/link-scene-plot-threads))
  (should (fboundp 'org-scribe/link-all-scene-plot-threads))

  ;; Setup wizard
  (should (fboundp 'org-scribe/setup-plot-thread-links)))

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
  (should (fboundp 'org-scribe/plot-thread-report)))

(ert-deftest test-plot-thread-stats-function-defined ()
  "Test that plot thread statistics function is defined."
  (should (fboundp 'org-scribe/plot-thread-stats)))

(ert-deftest test-plot-thread-timeline-dblock-defined ()
  "Test that timeline dynamic block function is defined."
  (should (fboundp 'org-dblock-write:plot-thread-timeline)))

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

;;; Run tests

(defun org-scribe-plot-links-run-tests ()
  "Run all plot thread linking tests."
  (interactive)
  (ert "^test-plot-"))

(provide 'test-plot-links)

;;; test-plot-links.el ends here
