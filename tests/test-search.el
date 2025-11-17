;;; test-search.el --- Tests for search functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for org-ql based search functions.
;; Tests function availability, input validation, and i18n support
;; for searches by POV, character, plot, and location.
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
(require 'org-scribe-i18n)

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

;;; I18n String Tests

(ert-deftest test-search-pov-strings-english ()
  "Test PoV search strings are available in English."
  (should (org-scribe-i18n-string 'search-pov-free 'en))
  (should (org-scribe-i18n-string 'search-pov-fuzzy 'en))
  (should (string-match-p "PoV" (org-scribe-i18n-string 'search-pov-free 'en))))

(ert-deftest test-search-pov-strings-spanish ()
  "Test PoV search strings are available in Spanish."
  (should (org-scribe-i18n-string 'search-pov-free 'es))
  (should (org-scribe-i18n-string 'search-pov-fuzzy 'es))
  (should (string-match-p "PdV" (org-scribe-i18n-string 'search-pov-free 'es))))

(ert-deftest test-search-character-strings-english ()
  "Test character search strings are available in English."
  (should (org-scribe-i18n-string 'search-char-free 'en))
  (should (org-scribe-i18n-string 'search-char-fuzzy 'en))
  (should (string-match-p "[Cc]haracter" (org-scribe-i18n-string 'search-char-free 'en))))

(ert-deftest test-search-character-strings-spanish ()
  "Test character search strings are available in Spanish."
  (should (org-scribe-i18n-string 'search-char-free 'es))
  (should (org-scribe-i18n-string 'search-char-fuzzy 'es))
  (should (string-match-p "[Pp]ersonaje" (org-scribe-i18n-string 'search-char-free 'es))))

(ert-deftest test-search-plot-strings-english ()
  "Test plot search strings are available in English."
  (should (org-scribe-i18n-string 'search-plot-free 'en))
  (should (org-scribe-i18n-string 'search-plot-fuzzy 'en))
  (should (string-match-p "[Pp]lot" (org-scribe-i18n-string 'search-plot-free 'en))))

(ert-deftest test-search-plot-strings-spanish ()
  "Test plot search strings are available in Spanish."
  (should (org-scribe-i18n-string 'search-plot-free 'es))
  (should (org-scribe-i18n-string 'search-plot-fuzzy 'es))
  (should (string-match-p "[Tt]rama" (org-scribe-i18n-string 'search-plot-free 'es))))

(ert-deftest test-search-location-strings-english ()
  "Test location search strings are available in English."
  (should (org-scribe-i18n-string 'search-loc-free 'en))
  (should (org-scribe-i18n-string 'search-loc-fuzzy 'en))
  (should (string-match-p "[Ll]ocation" (org-scribe-i18n-string 'search-loc-free 'en))))

(ert-deftest test-search-location-strings-spanish ()
  "Test location search strings are available in Spanish."
  (should (org-scribe-i18n-string 'search-loc-free 'es))
  (should (org-scribe-i18n-string 'search-loc-fuzzy 'es))
  (should (string-match-p "[Ll]ocalización" (org-scribe-i18n-string 'search-loc-free 'es))))

(ert-deftest test-search-todo-strings-english ()
  "Test TODO search strings are available in English."
  (should (org-scribe-i18n-string 'todo-search-title 'en))
  (should (string-match-p "TODO" (org-scribe-i18n-string 'todo-search-title 'en))))

(ert-deftest test-search-todo-strings-spanish ()
  "Test TODO search strings are available in Spanish."
  (should (org-scribe-i18n-string 'todo-search-title 'es))
  (should (string-match-p "TODO" (org-scribe-i18n-string 'todo-search-title 'es))))

;;; Error Message Tests

(ert-deftest test-search-error-empty-character-english ()
  "Test empty character error message in English."
  (let ((msg (org-scribe-i18n-string 'error-empty-character 'en)))
    (should msg)
    (should (string-match-p "[Cc]haracter" msg))
    (should (string-match-p "empty" msg))))

(ert-deftest test-search-error-empty-character-spanish ()
  "Test empty character error message in Spanish."
  (let ((msg (org-scribe-i18n-string 'error-empty-character 'es)))
    (should msg)
    (should (string-match-p "[Pp]ersonaje" msg))
    (should (string-match-p "vacío" msg))))

(ert-deftest test-search-error-empty-location-english ()
  "Test empty location error message in English."
  (let ((msg (org-scribe-i18n-string 'error-empty-location 'en)))
    (should msg)
    (should (string-match-p "[Ll]ocation" msg))
    (should (string-match-p "empty" msg))))

(ert-deftest test-search-error-empty-location-spanish ()
  "Test empty location error message in Spanish."
  (let ((msg (org-scribe-i18n-string 'error-empty-location 'es)))
    (should msg)
    (should (string-match-p "[Ll]ocalización" msg))
    (should (string-match-p "vacía" msg))))

(ert-deftest test-search-error-empty-plot-english ()
  "Test empty plot error message in English."
  (let ((msg (org-scribe-i18n-string 'error-empty-plot 'en)))
    (should msg)
    (should (string-match-p "[Pp]lot" msg))
    (should (string-match-p "empty" msg))))

(ert-deftest test-search-error-empty-plot-spanish ()
  "Test empty plot error message in Spanish."
  (let ((msg (org-scribe-i18n-string 'error-empty-plot 'es)))
    (should msg)
    (should (string-match-p "[Tt]rama" msg))
    (should (string-match-p "vacío" msg))))

(ert-deftest test-search-error-org-ql-required-english ()
  "Test org-ql required error message in English."
  (let ((msg (org-scribe-i18n-string 'error-org-ql-required 'en)))
    (should msg)
    (should (string-match-p "org-ql" msg))))

(ert-deftest test-search-error-org-ql-required-spanish ()
  "Test org-ql required error message in Spanish."
  (let ((msg (org-scribe-i18n-string 'error-org-ql-required 'es)))
    (should msg)
    (should (string-match-p "org-ql" msg))))

(ert-deftest test-search-no-org-files-message-english ()
  "Test no org files message in English."
  (let ((msg (org-scribe-i18n-string 'msg-no-org-files 'en)))
    (should msg)
    ;; Message should be a format string with %s placeholder
    (should (string-match-p "%s" msg))))

(ert-deftest test-search-no-org-files-message-spanish ()
  "Test no org files message in Spanish."
  (let ((msg (org-scribe-i18n-string 'msg-no-org-files 'es)))
    (should msg)
    ;; Message should be a format string with %s placeholder
    (should (string-match-p "%s" msg))))

;;; Run tests

(defun org-scribe-search-run-tests ()
  "Run all search function tests."
  (interactive)
  (ert "^test-search-"))

(provide 'test-search)

;;; test-search.el ends here
