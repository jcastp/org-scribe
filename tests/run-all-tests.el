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
;;   - Writing modes (org-scribe-modes.el)
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
;;   - Project health report (org-scribe-health.el)
;;   - Bundled Tempel snippets (snippets/org-scribe-tempel.eld)

;;; Code:

(require 'ert)

;; `emacs -Q` / `emacs -batch` skip the init file, so package.el is never
;; initialized and installed dependencies (org-ql, writeroom-mode, hydra,
;; ...) are absent from `load-path'.  Initialize it explicitly.
(require 'package)
(package-initialize)

;;; Setup load paths

(let ((tests-dir (file-name-directory
                  (or load-file-name buffer-file-name))))
  (let ((default-directory (file-name-directory (directory-file-name tests-dir))))
    ;; Add all module directories to load path
    (dolist (dir '("." "core" "counting" "templates" "modes" "search"
                   "language" "capture" "linking" "export" "reporting" "ui"
                   "planning" "snippets"))
      (add-to-list 'load-path (expand-file-name dir default-directory)))))

;;; Load test files

(defvar org-scribe-test-files
  '("org-scribe-test"              ; Core utilities (basic)
    "test-core-extended"           ; Core utilities (project type/structure)
    "test-messages"                ; Message system
    "test-wordcount"               ; Word counting
    "test-scene-property-localization" ; Localized scene property aliases
    "test-project"                 ; Project creation
    "test-capture"                 ; Capture system
    "test-modes"                   ; Writing environment modes
    "test-search"                  ; Search functions
    "test-search-links"            ; Link extraction helpers
    "test-character-links"         ; Character linking
    "test-location-links"          ; Location linking
    "test-plot-links"              ; Plot thread linking
    "test-character-relationships" ; Character relationship system
    "test-link-update"             ; Link display name updates
    "test-column-view"             ; Column view enhancement
    "test-export"                  ; Export filters
    "test-dictionary"              ; Dictionary and language tools
    "test-mythes"                  ; Offline MyThes thesaurus reader
    "test-lemma"                   ; Hunspell lemmatization
    "test-wikcionario"             ; Local Wikcionario definitions
    "test-health"                  ; Project health report
    "test-overlays"                ; Entity tooltip system
    "test-tempel-snippets"         ; Bundled Tempel snippets
    "test-sistema-templates"       ; Sistema template set + entity recognition
    "test-template-parity"         ; novel-en / novel-es structural parity
    "test-template-references"     ; Template F8-key/command references are real
    ;; Writing planner (planning/org-scribe-planner.el)
    "test-planner-calculation"     ; Calculation engine
    "test-planner-io"              ; Plan save/load round-trips
    "test-planner-schedule"        ; Schedule generation and date helpers
    "test-planner-milestones"      ; Milestone tracking
    "test-planner-data-helpers"    ; Daily-count data helpers
    "test-planner-dates"           ; Date validation
    "test-planner-buffer-safety"   ; Buffer erase safety
    "test-planner-hooks"           ; Integration hooks and pluggable fn-vars
    "test-planner-daily-sync"      ; Automatic daily word-count tracking
    "test-planner-agenda"          ; Org-agenda sync integration
    "test-planner-gate")           ; Per-project planner gate
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
;;
;; Each function below loads one test file and runs the tests matching a
;; regexp.  The regexps are matched against every test ERT knows about, not
;; just the file just loaded, so in a session where the whole suite has been
;; loaded a runner can also pick up same-prefixed tests from other files
;; (e.g. `test-location-capture-hook-*' lives in test-location-links.el but
;; matches the capture runner).  A handful of such overlaps remain and are
;; harmless — they run a related test twice.  What the regexps must not do
;; is *miss* tests defined in the file they load, or pull in a whole other
;; suite; both were happening before, which is why several enumerate their
;; alternatives instead of using a short prefix.

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
  ;; Matches what test-project.el actually defines.  Note the enumerated
  ;; test-template-* alternatives: a bare `^test-template-' also selects the
  ;; test-template-parity-* tests, which live in a file this function does
  ;; not load — harmless in a fresh session, but in a session where the full
  ;; suite has been loaded it silently runs another file's tests under this
  ;; name.  The `create'/`backward'/`obsolete' prefixes were missing
  ;; entirely, so project-creation tests never ran under this runner.
  (ert (concat "^test-project-\\|^test-validate-\\|^test-insert-"
               "\\|^test-backward-\\|^test-obsolete-"
               "\\|^test-create-novel-project-\\|^test-create-short-story-project-"
               "\\|^test-short-story-template-"
               "\\|^test-template-\\(?:directory\\|language\\|variable\\|no-plan\\)")))

;;;###autoload
(defun org-scribe-run-capture-tests ()
  "Run capture system tests only."
  (interactive)
  (load-file (expand-file-name "test-capture.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  ;; The bare `^test-character-' / `^test-location-' alternatives used to
  ;; pull in the character and location *linking* tests, from files this
  ;; function does not load.  Both are narrowed to the capture-side names
  ;; test-capture.el defines.
  (ert (concat "^test-capture-"
               "\\|^test-character-\\(?:capture\\|template\\)"
               "\\|^test-location-\\(?:capture\\|template\\)"
               "\\|^test-object-\\|^test-timeline-\\|^test-plot-thread-template"
               "\\|^test-create-novel-capture-file\\|^test-create-short-story-notes-file")))

;;;###autoload
(defun org-scribe-run-modes-tests ()
  "Run writing-modes tests only."
  (interactive)
  (load-file (expand-file-name "test-modes.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-modes-"))

;;;###autoload
(defun org-scribe-run-search-tests ()
  "Run search function tests only."
  (interactive)
  (load-file (expand-file-name "test-search.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (load-file (expand-file-name "test-search-links.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  ;; `^test-edit-' picks up the org-scribe-edit-string regexp tests, which
  ;; live in test-search.el but do not share the test-search- prefix.
  (ert "^test-search-\\|^test-extract-\\|^test-property-\\|^test-edit-"))

;;;###autoload
(defun org-scribe-run-tempel-tests ()
  "Run bundled Tempel snippet tests only."
  (interactive)
  (load-file (expand-file-name "test-tempel-snippets.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-tempel-"))

;;;###autoload
(defun org-scribe-run-sistema-tests ()
  "Run sistema template set and entity-recognition tests only."
  (interactive)
  (load-file (expand-file-name "test-sistema-templates.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-sistema-"))

;;;###autoload
(defun org-scribe-run-parity-tests ()
  "Run template set structural parity tests only."
  (interactive)
  (load-file (expand-file-name "test-template-parity.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-template-parity-"))

;;;###autoload
(defun org-scribe-run-template-reference-tests ()
  "Run template key/command-reference tests only."
  (interactive)
  (load-file (expand-file-name "test-template-references.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-template-references-"))

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
(defun org-scribe-run-mythes-tests ()
  "Run offline MyThes thesaurus tests only."
  (interactive)
  (load-file (expand-file-name "test-mythes.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-mythes-"))

;;;###autoload
(defun org-scribe-run-lemma-tests ()
  "Run hunspell lemmatization tests only."
  (interactive)
  (load-file (expand-file-name "test-lemma.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-lemma-"))

;;;###autoload
(defun org-scribe-run-wikcionario-tests ()
  "Run local Wikcionario definition tests only."
  (interactive)
  (load-file (expand-file-name "test-wikcionario.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-wikcionario-"))

;;;###autoload
(defun org-scribe-run-health-tests ()
  "Run project health report tests only."
  (interactive)
  (load-file (expand-file-name "test-health.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-health-"))

;;;###autoload
(defun org-scribe-run-export-tests ()
  "Run export filter tests only."
  (interactive)
  (load-file (expand-file-name "test-export.el"
                               (file-name-directory (or load-file-name buffer-file-name))))
  (ert "^test-export-\\|^test-scene-break-"))

;;;###autoload
(defun org-scribe-run-planner-tests ()
  "Run all writing planner tests."
  (interactive)
  (let ((tests-dir (file-name-directory (or load-file-name buffer-file-name))))
    (dolist (file '("test-planner-calculation"
                    "test-planner-io"
                    "test-planner-schedule"
                    "test-planner-milestones"
                    "test-planner-data-helpers"
                    "test-planner-dates"
                    "test-planner-buffer-safety"
                    "test-planner-hooks"
                    "test-planner-daily-sync"
                    "test-planner-agenda"
                    "test-planner-gate"))
      (load-file (expand-file-name (concat file ".el") tests-dir))))
  (ert "^test-planner-"))

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
    (message "  - Writing modes:       %d tests"
             (length (ert-select-tests "^test-modes-" t)))
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
    (message "  - MyThes thesaurus:    %d tests"
             (length (ert-select-tests "^test-mythes-" t)))
    (message "  - Lemmatization:       %d tests"
             (length (ert-select-tests "^test-lemma-" t)))
    (message "  - Wikcionario:         %d tests"
             (length (ert-select-tests "^test-wikcionario-" t)))
    (message "  - Writing planner:     %d tests"
             (length (ert-select-tests "^test-planner-" t)))
    (message "=========================================")))

;;; Batch mode entry point

;; When loaded in batch mode, run tests automatically
(when noninteractive
  (message "Running org-scribe tests in batch mode...")
  (org-scribe-run-tests-batch))

(provide 'run-all-tests)

;;; run-all-tests.el ends here
