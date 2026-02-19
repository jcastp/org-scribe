;;; run-all-tests.el --- Master test runner for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Master test runner for the entire org-scribe test suite.
;; This file loads all test modules and provides functions to run
;; all tests or specific test suites.
;;
;; Usage:
;;   Interactive: M-x org-scribe-run-all-tests
;;   Batch mode: emacs -batch -l tests/run-all-tests.el -f org-scribe-run-tests-batch
;;
;; Test Coverage:
;;   - Core utilities (org-scribe-core.el, org-scribe-config.el)
;;   - Core utilities extended (org-scribe-core.el - project type/structure)
;;   - Messages (org-scribe-messages.el)
;;   - Project creation (org-scribe-project.el)
;;   - Capture system (org-scribe-capture.el)
;;   - Search functions (org-scribe-search.el)
;;   - Character linking (org-scribe-character-links.el)
;;   - Location linking (org-scribe-location-links.el)
;;   - Plot thread linking (org-scribe-plot-links.el)
;;   - Character relationships (org-scribe-character-relationships.el)
;;   - Link display name updates (org-scribe-link-update.el)
;;   - Column view enhancement (org-scribe-column-view.el)
;;   - Export filters (org-scribe-export.el)
;;   - Word counting (org-scribe-wordcount.el)
;;   - Dictionary and language tools (org-scribe-dictionary.el)

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

(defvar org-scribe-test-files
  '("org-scribe-test"              ; Core utilities (basic)
    "test-core-extended"           ; Core utilities (project type/structure)
    "test-messages"                ; Message system
    "test-wordcount"               ; Word counting
    "test-project"                 ; Project creation
    "test-capture"                 ; Capture system
    "test-search"                  ; Search functions
    "test-search-links"            ; Link extraction helpers
    "test-character-links"         ; Character linking
    "test-location-links"          ; Location linking
    "test-plot-links"              ; Plot thread linking
    "test-character-relationships" ; Character relationship system
    "test-link-update"             ; Link display name updates
    "test-column-view"             ; Column view enhancement
    "test-export"                  ; Export filters
    "test-dictionary")             ; Dictionary and language tools
  "List of test files (without .el extension).")

(defun org-scribe-load-tests ()
  "Load all test files."
  (let ((tests-dir (file-name-directory
                    (or load-file-name buffer-file-name default-directory))))
    (dolist (test-file org-scribe-test-files)
      (let ((full-path (expand-file-name (concat test-file ".el") tests-dir)))
        (if (file-exists-p full-path)
            (progn
              (message "Loading test file: %s" test-file)
              (load-file full-path))
          (message "Warning: Test file not found: %s" full-path))))))

;;; Test execution functions

;;;###autoload
(defun org-scribe-run-all-tests ()
  "Load and run all org-scribe tests interactively."
  (interactive)
  (org-scribe-load-tests)
  (message "Running all org-scribe tests...")
  (ert t))

;;;###autoload
(defun org-scribe-run-tests-batch ()
  "Load and run all org-scribe tests in batch mode.
Suitable for CI/CD pipelines and automated testing."
  (org-scribe-load-tests)
  (ert-run-tests-batch-and-exit t))

;;; Selective test running

;;;###autoload
(defun org-scribe-run-core-tests ()
  "Run core utility tests only."
  (interactive)
  (load-file (expand-file-name "org-scribe-test.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^org-scribe-test-"))

;;;###autoload
(defun org-scribe-run-project-tests ()
  "Run project creation tests only."
  (interactive)
  (load-file (expand-file-name "test-project.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-project-\\|^test-validate-\\|^test-template-\\|^test-insert-"))

;;;###autoload
(defun org-scribe-run-capture-tests ()
  "Run capture system tests only."
  (interactive)
  (load-file (expand-file-name "test-capture.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-capture-\\|^test-create-\\|^test-character-\\|^test-location-\\|^test-object-\\|^test-timeline-"))

;;;###autoload
(defun org-scribe-run-search-tests ()
  "Run search function tests only."
  (interactive)
  (load-file (expand-file-name "test-search.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (load-file (expand-file-name "test-search-links.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-search-\\|^test-extract-\\|^test-property-"))

;;;###autoload
(defun org-scribe-run-linking-tests ()
  "Run all linking system tests (character, location, plot, relationships, updates)."
  (interactive)
  (let ((tests-dir (file-name-directory (or load-file-name buffer-file-name))))
    (load-file (expand-file-name "test-character-links.el" tests-dir))
    (load-file (expand-file-name "test-location-links.el" tests-dir))
    (load-file (expand-file-name "test-plot-links.el" tests-dir))
    (load-file (expand-file-name "test-character-relationships.el" tests-dir))
    (load-file (expand-file-name "test-link-update.el" tests-dir))
    (load-file (expand-file-name "test-column-view.el" tests-dir)))
  (ert "^test-character-\\|^test-location-\\|^test-plot-\\|^test-relationships-\\|^test-link-update-\\|^test-column-"))

;;;###autoload
(defun org-scribe-run-messages-tests ()
  "Run message system tests only."
  (interactive)
  (load-file (expand-file-name "test-messages.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-messages-"))

;;;###autoload
(defun org-scribe-run-dictionary-tests ()
  "Run dictionary and language tool tests only."
  (interactive)
  (load-file (expand-file-name "test-dictionary.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-dictionary-"))

;;;###autoload
(defun org-scribe-run-export-tests ()
  "Run export filter tests only."
  (interactive)
  (load-file (expand-file-name "test-export.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-export-\\|^test-scene-break-"))

;;;###autoload
(defun org-scribe-run-wordcount-tests ()
  "Run word counting tests only.
Note: Requires org-context-extended to be installed."
  (interactive)
  (load-file (expand-file-name "test-wordcount.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (if (featurep 'org-context-extended)
      (ert "^org-scribe-test-wordcount-")
    (message "Skipping wordcount tests - org-context-extended not available")))

;;; Test statistics

(defun org-scribe-test-statistics ()
  "Display statistics about the test suite."
  (interactive)
  (org-scribe-load-tests)
  (let* ((all-tests (ert-select-tests t t))
         (test-count (length all-tests))
         (test-files (length org-scribe-test-files)))
    (message "=== org-scribe Test Suite Statistics ===")
    (message "Test files: %d" test-files)
    (message "Total tests: %d" test-count)
    (message "")
    (message "Test coverage:")
    (message "  - Core utilities:      %d tests"
             (length (ert-select-tests "^org-scribe-test-" t)))
    (message "  - Core extended:       %d tests"
             (length (ert-select-tests "^test-core-" t)))
    (message "  - Messages:            %d tests"
             (length (ert-select-tests "^test-messages-" t)))
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
    (message "  - Relationships:       %d tests"
             (length (ert-select-tests "^test-relationships-" t)))
    (message "  - Link updates:        %d tests"
             (length (ert-select-tests "^test-link-update-" t)))
    (message "  - Column view:         %d tests"
             (length (ert-select-tests "^test-column-" t)))
    (message "  - Export filters:      %d tests"
             (length (ert-select-tests "^test-export-\\|^test-scene-break-" t)))
    (message "  - Word counting:       %d tests"
             (length (ert-select-tests "^org-scribe-test-wordcount-" t)))
    (message "  - Dictionary:          %d tests"
             (length (ert-select-tests "^test-dictionary-" t)))
    (message "=========================================")))

;;; Batch mode entry point

;; When loaded in batch mode, run tests automatically
(when noninteractive
  (message "Running org-scribe tests in batch mode...")
  (org-scribe-run-tests-batch))

(provide 'run-all-tests)

;;; run-all-tests.el ends here
