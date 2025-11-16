;;; run-all-tests.el --- Master test runner for emacs-writing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Master test runner for the entire emacs-writing test suite.
;; This file loads all test modules and provides functions to run
;; all tests or specific test suites.
;;
;; Usage:
;;   Interactive: M-x emacs-writing-run-all-tests
;;   Batch mode: emacs -batch -l tests/run-all-tests.el -f emacs-writing-run-tests-batch
;;
;; Test Coverage:
;;   - Core utilities (writing-core.el, writing-config.el)
;;   - Project creation (writing-project.el)
;;   - Capture system (writing-capture.el)
;;   - Search functions (writing-search.el)
;;   - Character linking (writing-character-links.el)
;;   - Location linking (writing-location-links.el)
;;   - Plot thread linking (writing-plot-links.el)
;;   - Column view enhancement (writing-column-view.el)
;;   - Export filters (writing-export.el)
;;   - Word counting (writing-wordcount.el)

;;; Code:

(require 'ert)

;;; Setup load paths

(let ((tests-dir (file-name-directory
                  (or load-file-name buffer-file-name))))
  (let ((default-directory (file-name-directory (directory-file-name tests-dir))))
    ;; Add all module directories to load path
    (dolist (dir '("." "core" "counting" "templates" "modes" "search"
                   "language" "capture" "linking" "export" "ui"))
      (add-to-list 'load-path (expand-file-name dir default-directory)))))

;;; Load test files

(defvar emacs-writing-test-files
  '("writing-test"           ; Core utilities
    "test-wordcount"         ; Word counting
    "test-project"           ; Project creation
    "test-capture"           ; Capture system
    "test-search"            ; Search functions
    "test-search-links"      ; Link extraction helpers
    "test-character-links"   ; Character linking
    "test-location-links"    ; Location linking
    "test-plot-links"        ; Plot thread linking
    "test-column-view"       ; Column view enhancement
    "test-export")           ; Export filters
  "List of test files (without .el extension).")

(defun emacs-writing-load-tests ()
  "Load all test files."
  (let ((tests-dir (file-name-directory
                    (or load-file-name buffer-file-name default-directory))))
    (dolist (test-file emacs-writing-test-files)
      (let ((full-path (expand-file-name (concat test-file ".el") tests-dir)))
        (if (file-exists-p full-path)
            (progn
              (message "Loading test file: %s" test-file)
              (load-file full-path))
          (message "Warning: Test file not found: %s" full-path))))))

;;; Test execution functions

;;;###autoload
(defun emacs-writing-run-all-tests ()
  "Load and run all emacs-writing tests interactively."
  (interactive)
  (emacs-writing-load-tests)
  (message "Running all emacs-writing tests...")
  (ert t))

;;;###autoload
(defun emacs-writing-run-tests-batch ()
  "Load and run all emacs-writing tests in batch mode.
Suitable for CI/CD pipelines and automated testing."
  (emacs-writing-load-tests)
  (ert-run-tests-batch-and-exit t))

;;; Selective test running

;;;###autoload
(defun emacs-writing-run-core-tests ()
  "Run core utility tests only."
  (interactive)
  (load-file (expand-file-name "writing-test.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^writing-test-"))

;;;###autoload
(defun emacs-writing-run-project-tests ()
  "Run project creation tests only."
  (interactive)
  (load-file (expand-file-name "test-project.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-project-\\|^test-validate-\\|^test-template-\\|^test-insert-"))

;;;###autoload
(defun emacs-writing-run-capture-tests ()
  "Run capture system tests only."
  (interactive)
  (load-file (expand-file-name "test-capture.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-capture-\\|^test-create-\\|^test-character-\\|^test-location-\\|^test-object-\\|^test-timeline-"))

;;;###autoload
(defun emacs-writing-run-search-tests ()
  "Run search function tests only."
  (interactive)
  (load-file (expand-file-name "test-search.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (load-file (expand-file-name "test-search-links.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-search-\\|^test-extract-\\|^test-property-"))

;;;###autoload
(defun emacs-writing-run-linking-tests ()
  "Run all linking system tests (character, location, plot)."
  (interactive)
  (let ((tests-dir (file-name-directory (or load-file-name buffer-file-name))))
    (load-file (expand-file-name "test-character-links.el" tests-dir))
    (load-file (expand-file-name "test-location-links.el" tests-dir))
    (load-file (expand-file-name "test-plot-links.el" tests-dir))
    (load-file (expand-file-name "test-column-view.el" tests-dir)))
  (ert "^test-character-\\|^test-location-\\|^test-plot-\\|^test-column-"))

;;;###autoload
(defun emacs-writing-run-export-tests ()
  "Run export filter tests only."
  (interactive)
  (load-file (expand-file-name "test-export.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-export-\\|^test-scene-break-"))

;;;###autoload
(defun emacs-writing-run-wordcount-tests ()
  "Run word counting tests only.
Note: Requires org-context-extended to be installed."
  (interactive)
  (load-file (expand-file-name "test-wordcount.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (if (featurep 'org-context-extended)
      (ert "^writing-test-wordcount-")
    (message "Skipping wordcount tests - org-context-extended not available")))

;;; Test statistics

(defun emacs-writing-test-statistics ()
  "Display statistics about the test suite."
  (interactive)
  (emacs-writing-load-tests)
  (let* ((all-tests (ert-select-tests t t))
         (test-count (length all-tests))
         (test-files (length emacs-writing-test-files)))
    (message "=== emacs-writing Test Suite Statistics ===")
    (message "Test files: %d" test-files)
    (message "Total tests: %d" test-count)
    (message "")
    (message "Test coverage:")
    (message "  - Core utilities:      %d tests"
             (length (ert-select-tests "^writing-test-" t)))
    (message "  - Project creation:    %d tests"
             (length (ert-select-tests "^test-project-\\|^test-validate-\\|^test-template-\\|^test-insert-" t)))
    (message "  - Capture system:      %d tests"
             (length (ert-select-tests "^test-capture-\\|^test-create-" t)))
    (message "  - Search functions:    %d tests"
             (length (ert-select-tests "^test-search-\\|^test-extract-\\|^test-property-" t)))
    (message "  - Character linking:   %d tests"
             (length (ert-select-tests "^test-character-" t)))
    (message "  - Location linking:    %d tests"
             (length (ert-select-tests "^test-location-" t)))
    (message "  - Plot linking:        %d tests"
             (length (ert-select-tests "^test-plot-" t)))
    (message "  - Column view:         %d tests"
             (length (ert-select-tests "^test-column-" t)))
    (message "  - Export filters:      %d tests"
             (length (ert-select-tests "^test-export-\\|^test-scene-break-" t)))
    (message "  - Word counting:       %d tests"
             (length (ert-select-tests "^writing-test-wordcount-" t)))
    (message "=========================================")))

;;; Batch mode entry point

;; When loaded in batch mode, run tests automatically
(when noninteractive
  (message "Running emacs-writing tests in batch mode...")
  (emacs-writing-run-tests-batch))

(provide 'run-all-tests)

;;; run-all-tests.el ends here
