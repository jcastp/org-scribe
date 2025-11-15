;;; test-search-links.el --- Tests for ID link extraction in searches -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for the helper functions that extract text from ID links
;; in property values for search functions.

;;; Code:

(require 'ert)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../search" default-directory)))

(require 'writing-search)

;;; Tests for writing--extract-link-text

(ert-deftest test-extract-link-text-plain ()
  "Test extracting text from plain text (no links)."
  (should (string= (writing--extract-link-text "Alex") "Alex"))
  (should (string= (writing--extract-link-text "Sam Chen") "Sam Chen"))
  (should (string= (writing--extract-link-text "Alex, Sam") "Alex, Sam")))

(ert-deftest test-extract-link-text-single-link ()
  "Test extracting text from single ID link."
  (should (string= (writing--extract-link-text "[[id:abc123][Alex]]") "Alex"))
  (should (string= (writing--extract-link-text "[[id:char-sam-001][Sam Chen]]") "Sam Chen")))

(ert-deftest test-extract-link-text-multiple-links ()
  "Test extracting text from multiple ID links."
  (should (string= (writing--extract-link-text "[[id:abc123][Alex]], [[id:def456][Sam]]")
                   "Alex, Sam"))
  (should (string= (writing--extract-link-text "[[id:abc][Alex]], [[id:def][Sam]], [[id:ghi][Morgan]]")
                   "Alex, Sam, Morgan")))

(ert-deftest test-extract-link-text-mixed ()
  "Test extracting text from mixed plain text and links."
  ;; This shouldn't normally happen, but handle it gracefully
  (should (string= (writing--extract-link-text "[[id:abc][Alex]], Sam, [[id:def][Morgan]]")
                   "Alex, Sam, Morgan")))

(ert-deftest test-extract-link-text-nil ()
  "Test extracting text from nil."
  (should (null (writing--extract-link-text nil))))

(ert-deftest test-extract-link-text-empty ()
  "Test extracting text from empty string."
  (should (string= (writing--extract-link-text "") "")))

;;; Tests for writing--property-contains-p

(ert-deftest test-property-contains-plain ()
  "Test property contains with plain text."
  (should (writing--property-contains-p "Alex" "Alex"))
  (should (writing--property-contains-p "Sam Chen" "Sam"))
  (should-not (writing--property-contains-p "Alex" "Sam")))

(ert-deftest test-property-contains-link ()
  "Test property contains with ID link."
  (should (writing--property-contains-p "[[id:abc123][Alex]]" "Alex"))
  (should (writing--property-contains-p "[[id:abc123][Sam Chen]]" "Sam"))
  (should-not (writing--property-contains-p "[[id:abc123][Alex]]" "Sam")))

(ert-deftest test-property-contains-multiple-links ()
  "Test property contains with multiple ID links."
  (should (writing--property-contains-p "[[id:abc][Alex]], [[id:def][Sam]]" "Alex"))
  (should (writing--property-contains-p "[[id:abc][Alex]], [[id:def][Sam]]" "Sam"))
  (should-not (writing--property-contains-p "[[id:abc][Alex]], [[id:def][Sam]]" "Morgan")))

(ert-deftest test-property-contains-nil ()
  "Test property contains with nil."
  (should-not (writing--property-contains-p nil "Alex")))

;;; Tests for writing--property-to-list

(ert-deftest test-property-to-list-plain ()
  "Test converting plain text property to list."
  (should (equal (writing--property-to-list "Alex") '("Alex")))
  (should (equal (writing--property-to-list "Alex, Sam") '("Alex" "Sam")))
  (should (equal (writing--property-to-list "Alex, Sam, Morgan") '("Alex" "Sam" "Morgan"))))

(ert-deftest test-property-to-list-links ()
  "Test converting ID links property to list."
  (should (equal (writing--property-to-list "[[id:abc][Alex]]") '("Alex")))
  (should (equal (writing--property-to-list "[[id:abc][Alex]], [[id:def][Sam]]")
                 '("Alex" "Sam")))
  (should (equal (writing--property-to-list "[[id:abc][Alex]], [[id:def][Sam]], [[id:ghi][Morgan]]")
                 '("Alex" "Sam" "Morgan"))))

(ert-deftest test-property-to-list-whitespace ()
  "Test that whitespace is trimmed."
  (should (equal (writing--property-to-list "Alex , Sam , Morgan")
                 '("Alex" "Sam" "Morgan")))
  (should (equal (writing--property-to-list "[[id:abc][Alex]] , [[id:def][Sam]]")
                 '("Alex" "Sam"))))

(ert-deftest test-property-to-list-nil ()
  "Test converting nil to list."
  (should (null (writing--property-to-list nil))))

;;; Run tests

(defun writing-search-run-link-tests ()
  "Run all search link extraction tests."
  (interactive)
  (ert "^test-"))

(provide 'test-search-links)

;;; test-search-links.el ends here
