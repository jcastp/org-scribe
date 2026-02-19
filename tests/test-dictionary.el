;;; test-dictionary.el --- Tests for dictionary and language tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for org-scribe-dictionary.el.
;; Covers module loading, function availability, and input validation.
;;
;; Note: Functions that make network requests (RAE API, WordReference)
;; are not tested here as they require external connectivity.
;; The formatting functions (rae-format-result, rae-format-conjugations)
;; are tested with mock data.

;;; Code:

(require 'ert)
(require 'json)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../language" default-directory)))

(require 'org-scribe-dictionary)

;;; Module Loading Tests

(ert-deftest test-dictionary-module-loads ()
  "Test that org-scribe-dictionary module loads without errors."
  (should (featurep 'org-scribe-dictionary)))

;;; Function Availability Tests

(ert-deftest test-dictionary-functions-defined ()
  "Test that all dictionary functions are defined."
  (should (fboundp 'org-scribe/rae-api-lookup))
  (should (fboundp 'org-scribe/rae-api-random))
  (should (fboundp 'org-scribe/sinonimo))
  (should (fboundp 'org-scribe/rae-format-result))
  (should (fboundp 'org-scribe/rae-format-conjugations)))

;;; Input Validation Tests

(ert-deftest test-dictionary-rae-lookup-empty-word ()
  "Test that empty word raises a user-error."
  (should-error (org-scribe/rae-api-lookup "")
                :type 'user-error))

(ert-deftest test-dictionary-rae-lookup-whitespace-word ()
  "Test that whitespace-only word raises a user-error."
  (should-error (org-scribe/rae-api-lookup "   ")
                :type 'user-error))

(ert-deftest test-dictionary-sinonimo-empty-word ()
  "Test that empty word raises a user-error in synonym lookup."
  (should-error (org-scribe/sinonimo "")
                :type 'user-error))

(ert-deftest test-dictionary-sinonimo-whitespace-word ()
  "Test that whitespace-only word raises user-error in synonym lookup."
  (should-error (org-scribe/sinonimo "  ")
                :type 'user-error))

;;; org-scribe/rae-format-result Tests (with mock data)

(defun test-dictionary--make-mock-json ()
  "Create a mock JSON data hash table for testing rae-format-result."
  (let* ((sense (make-hash-table :test 'equal))
         (meaning (make-hash-table :test 'equal))
         (data (make-hash-table :test 'equal))
         (json (make-hash-table :test 'equal)))
    ;; Sense
    (puthash "category" "adj." sense)
    (puthash "description" "De color azul." sense)
    (puthash "synonyms" '() sense)
    (puthash "antonyms" '() sense)
    ;; Meaning
    (puthash "origin" nil meaning)
    (puthash "senses" (list sense) meaning)
    (puthash "conjugations" nil meaning)
    ;; Data
    (puthash "word" "azul" data)
    (puthash "meanings" (list meaning) data)
    ;; Root
    (puthash "data" data json)
    (puthash "ok" t json)
    json))

(ert-deftest test-dictionary-format-result-inserts-title ()
  "Test that rae-format-result inserts word as title."
  (with-temp-buffer
    (org-mode)
    (org-scribe/rae-format-result (test-dictionary--make-mock-json) "azul")
    (goto-char (point-min))
    (should (search-forward "azul" nil t))))

(ert-deftest test-dictionary-format-result-inserts-definitions ()
  "Test that rae-format-result inserts definitions section."
  (with-temp-buffer
    (org-mode)
    (org-scribe/rae-format-result (test-dictionary--make-mock-json) "azul")
    (goto-char (point-min))
    (should (search-forward "Definiciones" nil t))))

(ert-deftest test-dictionary-format-result-inserts-description ()
  "Test that rae-format-result inserts the sense description."
  (with-temp-buffer
    (org-mode)
    (org-scribe/rae-format-result (test-dictionary--make-mock-json) "azul")
    (goto-char (point-min))
    (should (search-forward "De color azul." nil t))))

(ert-deftest test-dictionary-format-result-with-synonyms ()
  "Test that rae-format-result shows synonyms when present."
  (let* ((sense (make-hash-table :test 'equal))
         (meaning (make-hash-table :test 'equal))
         (data (make-hash-table :test 'equal))
         (json (make-hash-table :test 'equal)))
    (puthash "category" "adj." sense)
    (puthash "description" "De color azul." sense)
    (puthash "synonyms" '("celeste" "cobalto") sense)
    (puthash "antonyms" '() sense)
    (puthash "origin" nil meaning)
    (puthash "senses" (list sense) meaning)
    (puthash "conjugations" nil meaning)
    (puthash "word" "azul" data)
    (puthash "meanings" (list meaning) data)
    (puthash "data" data json)
    (with-temp-buffer
      (org-mode)
      (org-scribe/rae-format-result json "azul")
      (goto-char (point-min))
      (should (search-forward "celeste" nil t)))))

(ert-deftest test-dictionary-format-result-with-origin ()
  "Test that rae-format-result shows etymology when present."
  (let* ((origin (make-hash-table :test 'equal))
         (sense (make-hash-table :test 'equal))
         (meaning (make-hash-table :test 'equal))
         (data (make-hash-table :test 'equal))
         (json (make-hash-table :test 'equal)))
    (puthash "text" "Del árabe azraq." origin)
    (puthash "category" "adj." sense)
    (puthash "description" "De color azul." sense)
    (puthash "synonyms" '() sense)
    (puthash "antonyms" '() sense)
    (puthash "origin" origin meaning)
    (puthash "senses" (list sense) meaning)
    (puthash "conjugations" nil meaning)
    (puthash "word" "azul" data)
    (puthash "meanings" (list meaning) data)
    (puthash "data" data json)
    (with-temp-buffer
      (org-mode)
      (org-scribe/rae-format-result json "azul")
      (goto-char (point-min))
      (should (search-forward "Etimología" nil t))
      (should (search-forward "árabe" nil t)))))

;;; org-scribe/rae-format-conjugations Tests (with mock data)

(ert-deftest test-dictionary-format-conjugations-inserts-example-block ()
  "Test that rae-format-conjugations wraps content in #+begin_example."
  (let ((conjugations (make-hash-table :test 'equal)))
    ;; Simple mock conjugation: one mood with one tense
    (let ((tenses (make-hash-table :test 'equal))
          (persons (make-hash-table :test 'equal)))
      (puthash "1sg" "corro" persons)
      (puthash "presente" persons tenses)
      (puthash "indicativo" tenses conjugations))
    (with-temp-buffer
      (org-scribe/rae-format-conjugations conjugations)
      (let ((content (buffer-string)))
        (should (string-match-p "begin_example" content))
        (should (string-match-p "end_example" content))
        (should (string-match-p "INDICATIVO" content))))))

(ert-deftest test-dictionary-format-conjugations-nil ()
  "Test that nil conjugations are handled gracefully."
  (with-temp-buffer
    (org-scribe/rae-format-conjugations nil)
    ;; Should not insert anything for nil
    (should (= 0 (buffer-size)))))

;;; Run tests

(defun org-scribe-dictionary-run-tests ()
  "Run all dictionary tests."
  (interactive)
  (ert "^test-dictionary-"))

(provide 'test-dictionary)

;;; test-dictionary.el ends here
