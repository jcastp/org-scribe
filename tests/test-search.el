;;; test-search.el --- Tests for search functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for org-ql based search functions.
;; Tests function availability and input validation for searches by
;; POV, character, plot, and location.
;;
;; Note: Link extraction helpers (writing--extract-link-text,
;; writing--property-contains-p, writing--property-to-list) are
;; comprehensively tested in test-search-links.el.

;;; Code:

(require 'ert)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../search" default-directory)))

(require 'writing-search)

;;; Module Loading Tests

(ert-deftest test-search-module-loads ()
  "Test that writing-search module loads without errors."
  (should (featurep 'writing-search)))

;;; Function Availability Tests

(ert-deftest test-search-functions-defined ()
  "Test that all public search functions are defined."
  ;; Search by property
  (should (fboundp 'writing/org-find-pov))
  (should (fboundp 'writing/org-find-character))
  (should (fboundp 'writing/org-find-plot))
  (should (fboundp 'writing/org-find-location))

  ;; Search TODO items
  (should (fboundp 'writing/search-todos-recursive)))

;;; Helper Function Tests
;; Note: Detailed tests for helper functions are in test-search-links.el
;; These tests just verify the functions are available

(ert-deftest test-search-helper-functions-defined ()
  "Test that helper functions are defined."
  (should (fboundp 'writing--extract-link-text))
  (should (fboundp 'writing--property-contains-p))
  (should (fboundp 'writing--property-to-list)))

;;; Search Function Behavior Tests

(ert-deftest test-search-pov-requires-char ()
  "Test that POV search requires a character name."
  ;; These should raise user-error when given empty string
  (should-error (writing/org-find-pov "") :type 'user-error)
  (should-error (writing/org-find-pov "  ") :type 'user-error))

(ert-deftest test-search-character-requires-name ()
  "Test that character search requires a name."
  (should-error (writing/org-find-character "") :type 'user-error)
  (should-error (writing/org-find-character "  ") :type 'user-error))

(ert-deftest test-search-plot-requires-keyword ()
  "Test that plot search requires a keyword."
  (should-error (writing/org-find-plot "") :type 'user-error)
  (should-error (writing/org-find-plot "  ") :type 'user-error))

(ert-deftest test-search-location-requires-name ()
  "Test that location search requires a name."
  (should-error (writing/org-find-location "") :type 'user-error)
  (should-error (writing/org-find-location "  ") :type 'user-error))

;;; Run tests

(defun writing-search-run-tests ()
  "Run all search function tests."
  (interactive)
  (ert "^test-search-"))

(provide 'test-search)

;;; test-search.el ends here
