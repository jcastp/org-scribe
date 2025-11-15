;;; test-column-view.el --- Tests for column view enhancements -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for the column view link stripping module.
;; Verifies that ID links are properly stripped in column view display
;; while preserving backward compatibility with plain text properties.

;;; Code:

(require 'ert)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../search" default-directory))
  (add-to-list 'load-path (expand-file-name "../linking" default-directory)))

(require 'writing-column-view)
(require 'writing-search)  ; For writing--extract-link-text

;;; Module Loading Tests

(ert-deftest test-column-view-module-loads ()
  "Test that writing-column-view module loads without errors."
  (should (featurep 'writing-column-view)))

;;; Function Availability Tests

(ert-deftest test-column-view-functions-defined ()
  "Test that all column view functions are defined."
  ;; Core function
  (should (fboundp 'writing--column-view-strip-links))
  (should (fboundp 'writing--column-view-advice))

  ;; Enable/disable functions
  (should (fboundp 'writing-column-view-enable))
  (should (fboundp 'writing-column-view-disable))
  (should (fboundp 'writing-column-view-toggle)))

(ert-deftest test-column-view-customization-exists ()
  "Test that customization variable exists."
  (should (boundp 'writing-column-view-strip-links)))

;;; Link Stripping Tests

(ert-deftest test-column-view-strip-single-link ()
  "Test stripping a single ID link."
  (let ((writing-column-view-strip-links t))
    (should (string= "Alex"
                     (writing--column-view-strip-links
                      "[[id:char-alex-001][Alex]]")))))

(ert-deftest test-column-view-strip-multiple-links ()
  "Test stripping multiple ID links separated by commas."
  (let ((writing-column-view-strip-links t))
    (should (string= "Alex, Sam"
                     (writing--column-view-strip-links
                      "[[id:char-alex-001][Alex]], [[id:char-sam-002][Sam]]")))))

(ert-deftest test-column-view-strip-plot-links ()
  "Test stripping plot thread links."
  (let ((writing-column-view-strip-links t))
    (should (string= "Main Plot, Romance Subplot"
                     (writing--column-view-strip-links
                      "[[id:plot-main-001][Main Plot]], [[id:plot-romance-002][Romance Subplot]]")))))

(ert-deftest test-column-view-plain-text-unchanged ()
  "Test that plain text without links is unchanged."
  (let ((writing-column-view-strip-links t))
    (should (string= "Alex"
                     (writing--column-view-strip-links "Alex")))
    (should (string= "Alex, Sam"
                     (writing--column-view-strip-links "Alex, Sam")))))

(ert-deftest test-column-view-nil-value ()
  "Test that nil values are handled correctly."
  (let ((writing-column-view-strip-links t))
    (should (null (writing--column-view-strip-links nil)))))

(ert-deftest test-column-view-empty-string ()
  "Test that empty strings are handled correctly."
  (let ((writing-column-view-strip-links t))
    (should (string= "" (writing--column-view-strip-links "")))))

(ert-deftest test-column-view-non-string-value ()
  "Test that non-string values are passed through unchanged."
  (let ((writing-column-view-strip-links t))
    (should (= 42 (writing--column-view-strip-links 42)))
    (should (eq 'symbol (writing--column-view-strip-links 'symbol)))
    (should (equal '(1 2 3) (writing--column-view-strip-links '(1 2 3))))))

(ert-deftest test-column-view-disabled ()
  "Test that link stripping can be disabled."
  (let ((writing-column-view-strip-links nil))
    (should (string= "[[id:char-alex-001][Alex]]"
                     (writing--column-view-strip-links
                      "[[id:char-alex-001][Alex]]")))))

(ert-deftest test-column-view-mixed-content ()
  "Test mixed links and plain text."
  (let ((writing-column-view-strip-links t))
    ;; Text before link
    (should (string= "Chapter 1: Alex"
                     (writing--column-view-strip-links
                      "Chapter 1: [[id:char-alex-001][Alex]]")))

    ;; Text after link
    (should (string= "Alex (protagonist)"
                     (writing--column-view-strip-links
                      "[[id:char-alex-001][Alex]] (protagonist)")))))

(ert-deftest test-column-view-complex-display-text ()
  "Test links with complex display text containing special characters."
  (let ((writing-column-view-strip-links t))
    (should (string= "Alex's Coffee Shop"
                     (writing--column-view-strip-links
                      "[[id:loc-coffee-001][Alex's Coffee Shop]]")))

    (should (string= "Plot: A-story (main)"
                     (writing--column-view-strip-links
                      "[[id:plot-main-001][Plot: A-story (main)]]")))))

;;; Advice Integration Tests

(ert-deftest test-column-view-advice-installed ()
  "Test that advice is installed when enabled."
  (writing-column-view-enable)
  (should (advice-member-p #'writing--column-view-advice
                           'org-columns--displayed-value))
  (writing-column-view-disable))

(ert-deftest test-column-view-advice-removed ()
  "Test that advice can be removed."
  (writing-column-view-enable)
  (writing-column-view-disable)
  (should-not (advice-member-p #'writing--column-view-advice
                               'org-columns--displayed-value)))

(ert-deftest test-column-view-toggle ()
  "Test toggle function."
  ;; Ensure we start disabled
  (writing-column-view-disable)

  ;; Toggle on
  (writing-column-view-toggle)
  (should (advice-member-p #'writing--column-view-advice
                           'org-columns--displayed-value))

  ;; Toggle off
  (writing-column-view-toggle)
  (should-not (advice-member-p #'writing--column-view-advice
                               'org-columns--displayed-value)))

;;; Edge Case Tests

(ert-deftest test-column-view-malformed-links ()
  "Test handling of malformed or partial links."
  (let ((writing-column-view-strip-links t))
    ;; Incomplete link (missing closing brackets)
    (should (string= "[[id:char-alex-001][Alex"
                     (writing--column-view-strip-links
                      "[[id:char-alex-001][Alex")))

    ;; Link without ID prefix (shouldn't match)
    (should (string= "[[file:test.org][Link]]"
                     (writing--column-view-strip-links
                      "[[file:test.org][Link]]")))))

(ert-deftest test-column-view-unicode-display-text ()
  "Test links with Unicode characters in display text."
  (let ((writing-column-view-strip-links t))
    (should (string= "Café Français"
                     (writing--column-view-strip-links
                      "[[id:loc-cafe-001][Café Français]]")))

    (should (string= "José García"
                     (writing--column-view-strip-links
                      "[[id:char-jose-001][José García]]")))))

(ert-deftest test-column-view-whitespace-preservation ()
  "Test that whitespace in display text is preserved."
  (let ((writing-column-view-strip-links t))
    (should (string= "  Alex  "
                     (writing--column-view-strip-links
                      "[[id:char-alex-001][  Alex  ]]")))

    (should (string= "Alex,  Sam"
                     (writing--column-view-strip-links
                      "[[id:char-alex-001][Alex]],  [[id:char-sam-002][Sam]]")))))

;;; Run tests

(defun writing-column-view-run-tests ()
  "Run all column view tests."
  (interactive)
  (ert "^test-column-view-"))

(provide 'test-column-view)

;;; test-column-view.el ends here
