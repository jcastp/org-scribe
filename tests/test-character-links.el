;;; test-character-links.el --- Tests for character linking system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for the character linking module.
;; Tests character timeline functionality, helper functions,
;; and link creation.
;;
;; Note: The helper functions for extracting text from ID links
;; are tested in test-search-links.el and work for all link types
;; (characters, locations, and plot threads).

;;; Code:

(require 'ert)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../search" default-directory))
  (add-to-list 'load-path (expand-file-name "../linking" default-directory))
  (add-to-list 'load-path (expand-file-name "../capture" default-directory)))

(require 'writing-character-links)

;;; Module Loading Tests

(ert-deftest test-character-links-module-loads ()
  "Test that writing-character-links module loads without errors."
  (should (featurep 'writing-character-links)))

;;; Function Availability Tests

(ert-deftest test-character-links-functions-defined ()
  "Test that all public character linking functions are defined."
  ;; Core functions
  (should (fboundp 'writing/add-character-ids))
  (should (fboundp 'writing/set-pov-character))
  (should (fboundp 'writing/set-scene-characters))
  (should (fboundp 'writing/insert-character-link))
  (should (fboundp 'writing/insert-multiple-character-links))
  (should (fboundp 'writing/jump-to-pov-character))

  ;; Batch operations
  (should (fboundp 'writing/link-scene-characters))
  (should (fboundp 'writing/link-all-scene-characters))

  ;; Setup wizard
  (should (fboundp 'writing/setup-character-links)))

;;; Character Timeline Tests

(ert-deftest test-character-timeline-dblock-defined ()
  "Test that character timeline dynamic block function is defined."
  (should (fboundp 'org-dblock-write:character-timeline)))

(ert-deftest test-character-timeline-helper-functions-defined ()
  "Test that character timeline helper functions are defined."
  (should (fboundp 'writing--get-all-scenes-with-characters))
  (should (fboundp 'writing--collect-unique-characters))
  (should (fboundp 'writing--character-symbol)))

(ert-deftest test-character-symbol-pov ()
  "Test character symbol for PoV character."
  (should (string= "◆"
                   (writing--character-symbol "Alice" "Alice" '("Bob" "Charlie")))))

(ert-deftest test-character-symbol-present ()
  "Test character symbol for present (non-PoV) character."
  (should (string= "●"
                   (writing--character-symbol "Bob" "Alice" '("Bob" "Charlie")))))

(ert-deftest test-character-symbol-absent ()
  "Test character symbol for absent character."
  (should (string= ""
                   (writing--character-symbol "David" "Alice" '("Bob" "Charlie")))))

(ert-deftest test-character-symbol-pov-precedence ()
  "Test that PoV takes precedence when character in both properties."
  ;; If Alice is PoV and also in Characters list, should show ◆ (not both)
  (should (string= "◆"
                   (writing--character-symbol "Alice" "Alice" '("Alice" "Bob")))))

(ert-deftest test-character-symbol-no-pov ()
  "Test character symbol when no PoV (nil PoV)."
  (should (string= "●"
                   (writing--character-symbol "Alice" nil '("Alice" "Bob"))))
  (should (string= ""
                   (writing--character-symbol "Charlie" nil '("Alice" "Bob")))))

(ert-deftest test-character-symbol-no-characters ()
  "Test character symbol when no Characters list (nil or empty)."
  (should (string= "◆"
                   (writing--character-symbol "Alice" "Alice" nil)))
  (should (string= ""
                   (writing--character-symbol "Bob" "Alice" nil)))
  (should (string= ""
                   (writing--character-symbol "Charlie" "Alice" '()))))

(ert-deftest test-collect-unique-characters ()
  "Test collecting unique character names from scenes."
  (let ((scenes '(("Scene 1" "Ch 1" "Alice" ("Alice" "Bob"))
                  ("Scene 2" "Ch 1" "Bob" ("Bob" "Charlie"))
                  ("Scene 3" "Ch 2" "Alice" ("Alice" "Bob" "Charlie")))))
    ;; Should return unique characters, sorted alphabetically
    (should (equal '("Alice" "Bob" "Charlie")
                   (writing--collect-unique-characters scenes)))))

(ert-deftest test-collect-unique-characters-with-nil ()
  "Test collecting characters when some scenes have nil PoV or Characters."
  (let ((scenes '(("Scene 1" "Ch 1" "Alice" nil)            ; PoV only
                  ("Scene 2" "Ch 1" nil ("Bob" "Charlie"))  ; Characters only
                  ("Scene 3" "Ch 2" "Alice" ("Alice" "Bob")))))
    (should (equal '("Alice" "Bob" "Charlie")
                   (writing--collect-unique-characters scenes)))))

(ert-deftest test-collect-unique-characters-empty-strings ()
  "Test that empty strings are filtered out."
  (let ((scenes '(("Scene 1" "Ch 1" "Alice" ("Alice" ""))
                  ("Scene 2" "Ch 1" "" ("Bob"))
                  ("Scene 3" "Ch 2" "Charlie" ("Charlie")))))
    ;; Empty strings should be filtered
    (should (equal '("Alice" "Bob" "Charlie")
                   (writing--collect-unique-characters scenes)))))

(ert-deftest test-collect-unique-characters-duplicates ()
  "Test that duplicate characters are deduplicated."
  (let ((scenes '(("Scene 1" "Ch 1" "Alice" ("Alice" "Bob"))
                  ("Scene 2" "Ch 1" "Alice" ("Alice" "Bob"))
                  ("Scene 3" "Ch 2" "Alice" ("Alice" "Bob")))))
    ;; Should have each character only once
    (should (equal '("Alice" "Bob")
                   (writing--collect-unique-characters scenes)))))

(ert-deftest test-collect-unique-characters-sorting ()
  "Test that characters are sorted alphabetically."
  (let ((scenes '(("Scene 1" "Ch 1" "Zoe" ("Zoe" "Bob"))
                  ("Scene 2" "Ch 1" "Alice" ("Alice" "Charlie")))))
    ;; Should be alphabetically sorted
    (should (equal '("Alice" "Bob" "Charlie" "Zoe")
                   (writing--collect-unique-characters scenes)))))

(ert-deftest test-collect-unique-characters-empty ()
  "Test collecting characters from empty scenes list."
  (should (equal '() (writing--collect-unique-characters '()))))

;;; Run tests

(defun writing-character-links-run-tests ()
  "Run all character linking tests."
  (interactive)
  (ert "^test-character-"))

(provide 'test-character-links)

;;; test-character-links.el ends here
