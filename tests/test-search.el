;;; test-search.el --- Tests for search functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for org-ql based search functions.
;; Tests function availability and input validation for searches by
;; POV, character, plot, and location.
;;
;; Note: Link extraction helpers (org-scribe--extract-link-text,
;; org-scribe--property-contains-p, org-scribe--property-to-list) are
;; comprehensively tested in test-search-links.el.

;;; Code:

(require 'ert)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../search" default-directory)))

(require 'org-scribe-search)

;;; Module Loading Tests

(ert-deftest test-search-module-loads ()
  "Test that org-scribe-search module loads without errors."
  (should (featurep 'org-scribe-search)))

;;; Function Availability Tests

(ert-deftest test-search-functions-defined ()
  "Test that all public search functions are defined."
  ;; Search by property
  (should (fboundp 'org-scribe/org-find-pov))
  (should (fboundp 'org-scribe/org-find-character))
  (should (fboundp 'org-scribe/org-find-plot))
  (should (fboundp 'org-scribe/org-find-location))

  ;; Search TODO items
  (should (fboundp 'org-scribe/search-todos-recursive)))

;;; Helper Function Tests
;; Note: Detailed tests for helper functions are in test-search-links.el
;; These tests just verify the functions are available

(ert-deftest test-search-helper-functions-defined ()
  "Test that helper functions are defined."
  (should (fboundp 'org-scribe--extract-link-text))
  (should (fboundp 'org-scribe--property-contains-p))
  (should (fboundp 'org-scribe--property-to-list)))

;;; Search Function Behavior Tests

(ert-deftest test-search-pov-requires-char ()
  "Test that POV search requires a character name."
  ;; These should raise user-error when given empty string
  (should-error (org-scribe/org-find-pov "") :type 'user-error)
  (should-error (org-scribe/org-find-pov "  ") :type 'user-error))

(ert-deftest test-search-character-requires-name ()
  "Test that character search requires a name."
  (should-error (org-scribe/org-find-character "") :type 'user-error)
  (should-error (org-scribe/org-find-character "  ") :type 'user-error))

(ert-deftest test-search-plot-requires-keyword ()
  "Test that plot search requires a keyword."
  (should-error (org-scribe/org-find-plot "") :type 'user-error)
  (should-error (org-scribe/org-find-plot "  ") :type 'user-error))

(ert-deftest test-search-location-requires-name ()
  "Test that location search requires a name."
  (should-error (org-scribe/org-find-location "") :type 'user-error)
  (should-error (org-scribe/org-find-location "  ") :type 'user-error))

;;; Run tests

(defun org-scribe-search-run-tests ()
  "Run all search function tests."
  (interactive)
  (ert "^test-search-"))

(provide 'test-search)

;;; test-search.el ends here
