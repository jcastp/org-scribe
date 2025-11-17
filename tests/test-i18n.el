;;; test-i18n.el --- Unit tests for org-scribe-i18n -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests for the org-scribe i18n (internationalization) module.
;; Tests translation retrieval, language detection, caching, and helper functions.

;;; Code:

(require 'ert)
(require 'org-scribe-i18n)

;;; String Retrieval Tests

(ert-deftest org-scribe-i18n-test-string-retrieval-english ()
  "Test string retrieval in English."
  (should (equal (org-scribe-i18n-string 'default-scene-name 'en)
                 "New scene"))
  (should (equal (org-scribe-i18n-string 'default-chapter-name 'en)
                 "New chapter"))
  (should (equal (org-scribe-i18n-string 'error-empty-character 'en)
                 "Character name cannot be empty")))

(ert-deftest org-scribe-i18n-test-string-retrieval-spanish ()
  "Test string retrieval in Spanish."
  (should (equal (org-scribe-i18n-string 'default-scene-name 'es)
                 "Nueva escena"))
  (should (equal (org-scribe-i18n-string 'default-chapter-name 'es)
                 "Nuevo capítulo"))
  (should (equal (org-scribe-i18n-string 'error-empty-character 'es)
                 "El nombre del personaje no puede estar vacío")))

(ert-deftest org-scribe-i18n-test-missing-key ()
  "Test behavior when translation key doesn't exist."
  (let ((result (org-scribe-i18n-string 'nonexistent-key 'en)))
    (should (string-match-p "Missing translation" result))
    (should (string-match-p "nonexistent-key" result))))

(ert-deftest org-scribe-i18n-test-fallback-to-english ()
  "Test fallback to English when translation missing in target language."
  ;; If we add a key to English but not Spanish, it should fall back
  (let ((org-scribe-i18n-strings
         '((en (test-only-en . "English only"))
           (es ()))))
    (should (equal (org-scribe-i18n-string 'test-only-en 'es)
                   "English only"))))

;;; Format String Tests

(ert-deftest org-scribe-i18n-test-format-english ()
  "Test formatted string retrieval in English."
  (let ((result (format (org-scribe-i18n-string 'project-creation-success-novel 'en)
                       "My Novel" "/path/to/project")))
    (should (string-match-p "My Novel" result))
    (should (string-match-p "/path/to/project" result))
    (should (string-match-p "successfully" result))))

(ert-deftest org-scribe-i18n-test-format-spanish ()
  "Test formatted string retrieval in Spanish."
  (let ((result (format (org-scribe-i18n-string 'project-creation-success-novel 'es)
                       "Mi Novela" "/ruta/al/proyecto")))
    (should (string-match-p "Mi Novela" result))
    (should (string-match-p "/ruta/al/proyecto" result))
    (should (string-match-p "exitosamente" result))))

;;; Macro Tests

(ert-deftest org-scribe-i18n-test-macro-simple ()
  "Test org-scribe-i18n macro without format args.
Since org-scribe-template-language is not defined in tests,
this tests the default behavior (English)."
  (let ((org-scribe-i18n--project-language-cache nil)
        (default-directory "/tmp/nonexistent-project/"))
    ;; Without variable defined, should default to English
    (should (equal (org-scribe-i18n default-scene-name)
                   "New scene"))))

(ert-deftest org-scribe-i18n-test-macro-with-format ()
  "Test org-scribe-i18n macro with format arguments.
Tests default behavior (English)."
  (let ((org-scribe-i18n--project-language-cache nil)
        (default-directory "/tmp/nonexistent-project/"))
    (should (string-match-p "My Novel"
                           (org-scribe-i18n project-creation-success-novel
                                           "My Novel" "/path")))))

;;; Heading Pattern Tests

(ert-deftest org-scribe-i18n-test-heading-pattern-single-language ()
  "Test heading pattern for single language."
  (should (equal (org-scribe-i18n-heading-pattern 'heading-characters 'en)
                 "Characters"))
  (should (equal (org-scribe-i18n-heading-pattern 'heading-characters 'es)
                 "Personajes")))

(ert-deftest org-scribe-i18n-test-heading-pattern-bilingual ()
  "Test bilingual heading pattern generation."
  (let ((pattern (org-scribe-i18n-heading-pattern 'heading-characters)))
    ;; Should match both English and Spanish
    (should (string-match-p pattern "Characters"))
    (should (string-match-p pattern "Personajes"))
    ;; Should not match unrelated text
    (should-not (string-match-p pattern "Random Text"))))

(ert-deftest org-scribe-i18n-test-heading-pattern-multiple ()
  "Test multiple heading patterns."
  (let ((char-pattern (org-scribe-i18n-heading-pattern 'heading-characters))
        (loc-pattern (org-scribe-i18n-heading-pattern 'heading-locations))
        (plot-pattern (org-scribe-i18n-heading-pattern 'heading-plot-threads)))
    (should (string-match-p char-pattern "Characters"))
    (should (string-match-p char-pattern "Personajes"))
    (should (string-match-p loc-pattern "Locations"))
    (should (string-match-p loc-pattern "Localizaciones"))
    (should (string-match-p plot-pattern "Plot Threads"))
    (should (string-match-p plot-pattern "Hilos Argumentales"))))

;;; Language Detection Tests

(ert-deftest org-scribe-i18n-test-detect-language-fallback ()
  "Test language detection falls back to default when variable undefined.
When org-scribe-template-language is not bound, should default to 'en."
  (let ((default-directory "/tmp/nonexistent-project/"))
    ;; Without the variable defined, should default to 'en
    (should (eq (org-scribe-i18n--detect-project-language-uncached) 'en))))

(ert-deftest org-scribe-i18n-test-detect-language-from-marker ()
  "Test language detection from .org-scribe-project file."
  (let ((temp-dir (make-temp-file "org-scribe-test-" t))
        (default-directory nil))
    (unwind-protect
        (progn
          ;; Create marker file with English language
          (with-temp-file (expand-file-name ".org-scribe-project" temp-dir)
            (insert "# Writing project: Test\n")
            (insert "# Created: 2025-01-01\n")
            (insert "# Language: en\n"))

          ;; Test detection
          (let ((default-directory temp-dir)
                (org-scribe-template-language 'es)) ; Set to Spanish to test override
            (should (eq (org-scribe-i18n--detect-project-language-uncached) 'en)))

          ;; Update to Spanish
          (with-temp-file (expand-file-name ".org-scribe-project" temp-dir)
            (insert "# Writing project: Test\n")
            (insert "# Language: es\n"))

          (let ((default-directory temp-dir)
                (org-scribe-template-language 'en))
            (should (eq (org-scribe-i18n--detect-project-language-uncached) 'es))))
      ;; Cleanup
      (delete-directory temp-dir t))))

;;; Cache Tests

(ert-deftest org-scribe-i18n-test-cache-basic ()
  "Test that caching works for language detection."
  (let* ((org-scribe-i18n--project-language-cache nil)
         (org-scribe-template-language 'en)
         (default-directory "/tmp/test-project/"))
    ;; First call should cache
    (org-scribe-i18n--detect-project-language)
    (should org-scribe-i18n--project-language-cache)
    ;; Cached value should be retrievable
    (let ((cached (alist-get default-directory
                            org-scribe-i18n--project-language-cache
                            nil nil #'string=)))
      (should (eq cached 'en)))))

(ert-deftest org-scribe-i18n-test-clear-cache ()
  "Test cache clearing function."
  (let ((org-scribe-i18n--project-language-cache
         '(("/project1/" . en) ("/project2/" . es))))
    (org-scribe-i18n-clear-cache)
    (should-not org-scribe-i18n--project-language-cache)))

;;; Utility Function Tests

(ert-deftest org-scribe-i18n-test-get-all-keys ()
  "Test getting all translation keys."
  (let ((keys (org-scribe-i18n-get-all-keys)))
    (should (listp keys))
    (should (memq 'default-scene-name keys))
    (should (memq 'default-chapter-name keys))
    (should (memq 'error-empty-character keys))))

(ert-deftest org-scribe-i18n-test-missing-translations ()
  "Test detection of missing translations."
  ;; Create a scenario with missing Spanish translations
  (let ((org-scribe-i18n-strings
         '((en (key1 . "Value 1")
               (key2 . "Value 2")
               (key3 . "Value 3"))
           (es (key1 . "Valor 1")
               ;; key2 and key3 missing
               ))))
    (let ((missing (org-scribe-i18n-missing-translations 'es)))
      (should (memq 'key2 missing))
      (should (memq 'key3 missing))
      (should-not (memq 'key1 missing)))))

;;; Integration Tests

(ert-deftest org-scribe-i18n-test-capture-labels-english ()
  "Test capture label strings in English."
  (should (equal (org-scribe-i18n-string 'capture-character-name 'en)
                 "Character Name"))
  (should (equal (org-scribe-i18n-string 'capture-location-type 'en)
                 "Type"))
  (should (equal (org-scribe-i18n-string 'role-protagonist 'en)
                 "Protagonist")))

(ert-deftest org-scribe-i18n-test-capture-labels-spanish ()
  "Test capture label strings in Spanish."
  (should (equal (org-scribe-i18n-string 'capture-character-name 'es)
                 "Nombre del Personaje"))
  (should (equal (org-scribe-i18n-string 'capture-location-type 'es)
                 "Tipo"))
  (should (equal (org-scribe-i18n-string 'role-protagonist 'es)
                 "Protagonista")))

(ert-deftest org-scribe-i18n-test-error-messages-completeness ()
  "Test that all error message keys exist in both languages."
  (let ((error-keys '(error-empty-character
                     error-empty-location
                     error-empty-plot
                     error-no-org-mode
                     error-project-exists
                     error-template-not-found
                     error-no-project
                     error-org-ql-required)))
    (dolist (key error-keys)
      (should (org-scribe-i18n-string key 'en))
      (should (org-scribe-i18n-string key 'es)))))

(ert-deftest org-scribe-i18n-test-validation-messages-completeness ()
  "Test that all validation message keys exist in both languages."
  (let ((validation-keys '(validation-empty-title
                          validation-path-separator
                          validation-colon
                          validation-special-chars
                          validation-starts-dot
                          validation-double-dots)))
    (dolist (key validation-keys)
      (should (org-scribe-i18n-string key 'en))
      (should (org-scribe-i18n-string key 'es)))))

;;; Test Runner

(defun org-scribe-i18n-run-tests ()
  "Run all i18n tests and display results."
  (interactive)
  (ert-run-tests-batch "^org-scribe-i18n-test-"))

(provide 'test-i18n)

;;; test-i18n.el ends here
