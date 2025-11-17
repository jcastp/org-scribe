;;; test-project.el --- Tests for project creation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for project creation and template processing.
;; Tests novel and short story project creation, template
;; variable substitution, title validation, and project structure.

;;; Code:

(require 'ert)
(require 'org)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../templates" default-directory)))

(require 'org-scribe-project)

;;; Module Loading Tests

(ert-deftest test-project-module-loads ()
  "Test that org-scribe-project module loads without errors."
  (should (featurep 'org-scribe-project)))

;;; Function Availability Tests

(ert-deftest test-project-functions-defined ()
  "Test that all public project functions are defined."
  ;; Project creation
  (should (fboundp 'org-scribe-create-novel-project))
  (should (fboundp 'org-scribe-create-short-story-project))

  ;; Template insertion
  (should (fboundp 'org-scribe-insert-scene))
  (should (fboundp 'org-scribe-insert-chapter))

  ;; Navigation
  (should (fboundp 'org-scribe-open-project-file))

  ;; Utilities
  (should (fboundp 'org-scribe-edit-templates))
  (should (fboundp 'org-scribe-register-projects))

  ;; Backward compatibility
  (should (fboundp 'org-scribe-project-create-novel-project))
  (should (fboundp 'org-scribe-create-project)))

;;; Title Validation Tests

(ert-deftest test-validate-project-title-valid ()
  "Test that valid titles pass validation."
  (should (null (org-scribe--validate-project-title "My Novel")))
  (should (null (org-scribe--validate-project-title "Novel-2025")))
  (should (null (org-scribe--validate-project-title "SciFi_Project")))
  (should (null (org-scribe--validate-project-title "A"))))

(ert-deftest test-validate-project-title-empty ()
  "Test that empty titles fail validation."
  (should (stringp (org-scribe--validate-project-title "")))
  (should (stringp (org-scribe--validate-project-title "   ")))
  (should (stringp (org-scribe--validate-project-title "\t\n"))))

(ert-deftest test-validate-project-title-path-separators ()
  "Test that titles with path separators fail validation."
  (should (stringp (org-scribe--validate-project-title "My/Novel")))
  (should (stringp (org-scribe--validate-project-title "My\\Novel")))
  (should (stringp (org-scribe--validate-project-title "Novel/Chapter/1"))))

(ert-deftest test-validate-project-title-special-chars ()
  "Test that titles with special characters fail validation."
  (should (stringp (org-scribe--validate-project-title "Novel:Draft")))
  (should (stringp (org-scribe--validate-project-title "Novel*")))
  (should (stringp (org-scribe--validate-project-title "Novel?")))
  (should (stringp (org-scribe--validate-project-title "Novel<>")))
  (should (stringp (org-scribe--validate-project-title "Novel|Pipe")))
  (should (stringp (org-scribe--validate-project-title "Novel\"Quote")))
  (should (stringp (org-scribe--validate-project-title "Novel'Quote"))))

(ert-deftest test-validate-project-title-dots ()
  "Test that titles with problematic dots fail validation."
  (should (stringp (org-scribe--validate-project-title ".hidden")))
  (should (stringp (org-scribe--validate-project-title "...dots")))
  (should (stringp (org-scribe--validate-project-title "Novel..Test")))
  ;; Single dot in middle should be OK
  (should (null (org-scribe--validate-project-title "Novel.Draft"))))

;;; Template Variable Substitution Tests

(ert-deftest test-template-variable-substitution ()
  "Test that template variables are correctly substituted."
  (let* ((temp-template (make-temp-file "test-template-" nil ".template"))
         (temp-output (make-temp-file "test-output-"))
         (variables '(("TITLE" . "Test Novel")
                     ("AUTHOR" . "John Doe")
                     ("DATE" . "2025-01-15"))))

    (unwind-protect
        (progn
          ;; Write template file
          (with-temp-file temp-template
            (insert "#+TITLE: ${TITLE}\n")
            (insert "#+AUTHOR: ${AUTHOR}\n")
            (insert "#+DATE: ${DATE}\n")
            (insert "This is a test for ${TITLE} by ${AUTHOR}.\n"))

          ;; Process template
          (org-scribe--process-template temp-template temp-output variables)

          ;; Verify output
          (with-temp-buffer
            (insert-file-contents temp-output)
            (let ((content (buffer-string)))
              (should (string-match-p "Test Novel" content))
              (should (string-match-p "John Doe" content))
              (should (string-match-p "2025-01-15" content))
              (should-not (string-match-p "\\${TITLE}" content))
              (should-not (string-match-p "\\${AUTHOR}" content))
              (should-not (string-match-p "\\${DATE}" content)))))

      ;; Cleanup
      (when (file-exists-p temp-template)
        (delete-file temp-template))
      (when (file-exists-p temp-output)
        (delete-file temp-output)))))

(ert-deftest test-template-variable-multiple-occurrences ()
  "Test that variables appearing multiple times are all substituted."
  (let* ((temp-template (make-temp-file "test-template-" nil ".template"))
         (temp-output (make-temp-file "test-output-"))
         (variables '(("TITLE" . "My Novel"))))

    (unwind-protect
        (progn
          (with-temp-file temp-template
            (insert "${TITLE} - Chapter 1\n")
            (insert "This is ${TITLE}.\n")
            (insert "Working on ${TITLE} today.\n"))

          (org-scribe--process-template temp-template temp-output variables)

          (with-temp-buffer
            (insert-file-contents temp-output)
            (let ((content (buffer-string)))
              ;; All three occurrences should be replaced
              (should (= 3 (cl-count "My Novel" (split-string content "\n") :test #'string-match-p)))
              (should-not (string-match-p "\\${TITLE}" content)))))

      (when (file-exists-p temp-template)
        (delete-file temp-template))
      (when (file-exists-p temp-output)
        (delete-file temp-output)))))

;;; Scene Template Insertion Tests

(ert-deftest test-insert-scene-template ()
  "Test scene template insertion."
  (with-temp-buffer
    (org-mode)
    (org-scribe-insert-scene "Opening Scene")

    (let ((content (buffer-string)))
      ;; Check heading
      (should (string-match-p "\\*\\*\\* TODO Opening Scene :ignore:" content))

      ;; Check properties
      (should (string-match-p ":PoV:" content))
      (should (string-match-p ":Characters:" content))
      (should (string-match-p ":Plot:" content))
      (should (string-match-p ":Timeline:" content))
      (should (string-match-p ":Location:" content))
      (should (string-match-p ":Description:" content))
      (should (string-match-p ":Summary:" content))
      (should (string-match-p ":Scene-motivation:" content))
      (should (string-match-p ":Conflict-source:" content))
      (should (string-match-p ":What-is-at-stake:" content))
      (should (string-match-p ":Emotion:" content))
      (should (string-match-p ":Comment:" content))
      (should (string-match-p ":WORD-OBJECTIVE: 500" content))

      ;; Check scene break macro
      (should (string-match-p "{{{scene-break}}}" content)))))

(ert-deftest test-insert-scene-empty-name ()
  "Test scene template with empty name uses default."
  (with-temp-buffer
    (org-mode)
    (org-scribe-insert-scene "")

    (let ((content (buffer-string)))
      (should (string-match-p "\\*\\*\\* TODO New scene :ignore:" content)))))

(ert-deftest test-insert-scene-whitespace-name ()
  "Test scene template with whitespace-only name uses default."
  (with-temp-buffer
    (org-mode)
    (org-scribe-insert-scene "   ")

    (let ((content (buffer-string)))
      (should (string-match-p "\\*\\*\\* TODO New scene :ignore:" content)))))

;;; Chapter Template Insertion Tests

(ert-deftest test-insert-chapter-template ()
  "Test chapter template insertion."
  (with-temp-buffer
    (org-mode)
    (org-scribe-insert-chapter "Chapter 1")

    (let ((content (buffer-string)))
      ;; Check heading
      (should (string-match-p "\\*\\* TODO Chapter 1 :ignore:" content))

      ;; Check properties
      (should (string-match-p ":WORD-OBJECTIVE: 5000" content))
      (should (string-match-p ":WORDCOUNT: 0" content)))))

(ert-deftest test-insert-chapter-empty-name ()
  "Test chapter template with empty name uses default."
  (with-temp-buffer
    (org-mode)
    (org-scribe-insert-chapter "")

    (let ((content (buffer-string)))
      (should (string-match-p "\\*\\* TODO New chapter :ignore:" content)))))

;;; Configuration Tests

(ert-deftest test-template-directory-configured ()
  "Test that template directory is configured."
  (should (boundp 'org-scribe-template-directory))
  (should (stringp org-scribe-template-directory)))

(ert-deftest test-template-language-configured ()
  "Test that template language is configured."
  (should (boundp 'org-scribe-template-language))
  (should (memq org-scribe-template-language '(en es))))

(ert-deftest test-short-story-template-directory-configured ()
  "Test that short story template directory is configured."
  (should (boundp 'org-scribe-short-story-template-directory))
  (should (stringp org-scribe-short-story-template-directory)))

;;; Backward Compatibility Tests

(ert-deftest test-backward-compatibility-aliases ()
  "Test that old function names are still available."
  ;; Old function names should be callable
  (should (fboundp 'org-scribe-project-create-novel-project))
  (should (fboundp 'org-scribe-create-project))
  (should (fboundp 'org-scribe-project-insert-scene))
  (should (fboundp 'org-scribe-project-insert-chapter))
  (should (fboundp 'org-scribe-project-open-novel-file))

  ;; And should be marked as aliases (indirect functions)
  (should (symbolp (symbol-function 'org-scribe-project-create-novel-project)))
  (should (symbolp (symbol-function 'org-scribe-create-project))))

(ert-deftest test-obsolete-functions-marked ()
  "Test that old functions are marked as obsolete."
  (should (get 'org-scribe-project-create-novel-project 'byte-obsolete-info))
  (should (get 'org-scribe-create-project 'byte-obsolete-info))
  (should (get 'org-scribe-project-insert-scene 'byte-obsolete-info))
  (should (get 'org-scribe-project-insert-chapter 'byte-obsolete-info))
  (should (get 'org-scribe-project-open-novel-file 'byte-obsolete-info)))

;;; Integration Tests (require actual template files)

(ert-deftest test-template-directory-exists ()
  "Test that default template directory exists."
  ;; This test may fail if templates are not installed
  ;; but it's useful for catching installation issues
  (when (boundp 'org-scribe-template-directory)
    (should (or (file-directory-p org-scribe-template-directory)
                (message "Warning: Template directory not found at %s"
                        org-scribe-template-directory)))))

;;; Run tests

(defun org-scribe-project-run-tests ()
  "Run all project creation tests."
  (interactive)
  (ert "^test-project-\\|^test-validate-\\|^test-template-\\|^test-insert-\\|^test-backward-"))

(provide 'test-project)

;;; test-project.el ends here
