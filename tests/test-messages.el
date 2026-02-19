;;; test-messages.el --- Tests for org-scribe messages -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for the centralized message system in org-scribe-messages.el.
;; Covers message retrieval, format argument substitution, pluralization,
;; and error handling for unknown keys.

;;; Code:

(require 'ert)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory)))

(require 'org-scribe-messages)

;;; Module Loading Tests

(ert-deftest test-messages-module-loads ()
  "Test that org-scribe-messages module loads without errors."
  (should (featurep 'org-scribe-messages)))

;;; Function Availability Tests

(ert-deftest test-messages-functions-defined ()
  "Test that message functions are defined."
  (should (fboundp 'org-scribe-msg))
  (should (fboundp 'org-scribe-plural))
  (should (boundp 'org-scribe-messages)))

;;; org-scribe-msg Tests

(ert-deftest test-messages-msg-simple ()
  "Test retrieving a message without format arguments."
  (should (string= "New scene" (org-scribe-msg 'default-scene-name)))
  (should (string= "New chapter" (org-scribe-msg 'default-chapter-name))))

(ert-deftest test-messages-msg-with-single-arg ()
  "Test retrieving a message with one format argument."
  (should (string= "Inserted link to Alice"
                   (org-scribe-msg 'msg-inserted-link "Alice")))
  (should (string= "Set PoV to Bob"
                   (org-scribe-msg 'msg-set-pov "Bob"))))

(ert-deftest test-messages-msg-with-multiple-args ()
  "Test retrieving a message with multiple format arguments."
  (should (string= "Updated character links in 5 scenes"
                   (org-scribe-msg 'msg-updated-links 5 "s")))
  (should (string= "Updated character links in 1 scene"
                   (org-scribe-msg 'msg-updated-links 1 ""))))

(ert-deftest test-messages-msg-with-string-and-number ()
  "Test message formatting with mixed arg types."
  (should (string= "Novel project 'My Novel' created successfully at /tmp/my-novel"
                   (org-scribe-msg 'project-creation-success-novel
                                   "My Novel" "/tmp/my-novel"))))

(ert-deftest test-messages-msg-unknown-key-signals-error ()
  "Test that requesting an unknown key signals an error."
  (should-error (org-scribe-msg 'this-key-does-not-exist)
                :type 'error))

(ert-deftest test-messages-msg-returns-string ()
  "Test that org-scribe-msg always returns a string."
  (should (stringp (org-scribe-msg 'default-scene-name)))
  (should (stringp (org-scribe-msg 'msg-inserted-link "X")))
  (should (stringp (org-scribe-msg 'error-empty-title))))

(ert-deftest test-messages-msg-all-error-keys-exist ()
  "Test that commonly used error keys are defined."
  (should (org-scribe-msg 'error-empty-title))
  (should (org-scribe-msg 'error-path-separator))
  (should (org-scribe-msg 'error-no-characters-found))
  (should (org-scribe-msg 'error-no-locations-found))
  (should (org-scribe-msg 'error-no-plot-threads-found)))

(ert-deftest test-messages-msg-all-prompt-keys-exist ()
  "Test that search/prompt keys are defined."
  (should (org-scribe-msg 'search-pov-prompt))
  (should (org-scribe-msg 'search-char-prompt))
  (should (org-scribe-msg 'search-plot-prompt))
  (should (org-scribe-msg 'search-loc-prompt))
  (should (org-scribe-msg 'prompt-select-character))
  (should (org-scribe-msg 'prompt-select-location))
  (should (org-scribe-msg 'prompt-select-plot-thread)))

;;; org-scribe-plural Tests

(ert-deftest test-messages-plural-singular ()
  "Test plural returns singular suffix when count is 1."
  (should (string= "" (org-scribe-plural 1 "")))
  (should (string= "x" (org-scribe-plural 1 "x"))))

(ert-deftest test-messages-plural-plural ()
  "Test plural returns 's' when count is not 1."
  (should (string= "s" (org-scribe-plural 0 "")))
  (should (string= "s" (org-scribe-plural 2 "")))
  (should (string= "s" (org-scribe-plural 5 "")))
  (should (string= "s" (org-scribe-plural 100 ""))))

(ert-deftest test-messages-plural-negative ()
  "Test plural with negative count returns 's'."
  (should (string= "s" (org-scribe-plural -1 ""))))

(ert-deftest test-messages-plural-in-message ()
  "Test using plural inside a message."
  (should (string= "Updated character links in 1 scene"
                   (org-scribe-msg 'msg-updated-links 1 (org-scribe-plural 1 ""))))
  (should (string= "Updated character links in 3 scenes"
                   (org-scribe-msg 'msg-updated-links 3 (org-scribe-plural 3 "")))))

;;; Message Repository Tests

(ert-deftest test-messages-repository-is-alist ()
  "Test that org-scribe-messages is a proper alist."
  (should (listp org-scribe-messages))
  (should (> (length org-scribe-messages) 0))
  ;; Each entry should be a cons cell
  (dolist (entry org-scribe-messages)
    (should (consp entry))
    (should (symbolp (car entry)))
    (should (stringp (cdr entry)))))

;;; Run tests

(defun org-scribe-messages-run-tests ()
  "Run all message tests."
  (interactive)
  (ert "^test-messages-"))

(provide 'test-messages)

;;; test-messages.el ends here
