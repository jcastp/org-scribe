;;; test-project-creation.el --- Test project creation with i18n -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for project creation with internationalization support.
;; Tests that projects can be created in both English and Spanish.

;;; Code:

(require 'ert)
(require 'org-scribe-i18n)
(require 'org-scribe-project)

;;; Validation Tests

(ert-deftest org-scribe-test-validate-empty-title ()
  "Test validation rejects empty title with i18n message."
  (let ((error-msg (org-scribe--validate-project-title "")))
    (should error-msg)
    ;; Should contain translated error message
    (should (or (string-match-p "empty" error-msg)
                (string-match-p "vacío" error-msg)))))

(ert-deftest org-scribe-test-validate-path-separator ()
  "Test validation rejects path separators with i18n message."
  (let ((error-msg (org-scribe--validate-project-title "my/project")))
    (should error-msg)
    (should (or (string-match-p "separator" error-msg)
                (string-match-p "separador" error-msg)))))

(ert-deftest org-scribe-test-validate-colon ()
  "Test validation rejects colons with i18n message."
  (let ((error-msg (org-scribe--validate-project-title "my:project")))
    (should error-msg)
    (should (or (string-match-p "colon" error-msg)
                (string-match-p "punto" error-msg)))))

(ert-deftest org-scribe-test-validate-special-chars ()
  "Test validation rejects special characters with i18n message."
  (let ((error-msg (org-scribe--validate-project-title "my*project")))
    (should error-msg)
    (should (or (string-match-p "special" error-msg)
                (string-match-p "especial" error-msg)))))

(ert-deftest org-scribe-test-validate-valid-title ()
  "Test validation accepts valid title."
  (should-not (org-scribe--validate-project-title "My Valid Project"))
  (should-not (org-scribe--validate-project-title "mi-proyecto-valido"))
  (should-not (org-scribe--validate-project-title "Project_123")))

;;; Project Creation Tests (Non-Interactive)

(ert-deftest org-scribe-test-novel-project-english ()
  "Test novel project creation in English."
  (let* ((temp-dir (make-temp-file "org-scribe-test-novel-" t))
         (project-name "Test-English-Novel")
         (project-dir (expand-file-name project-name temp-dir)))
    (unwind-protect
        (progn
          ;; Create project programmatically
          (org-scribe-create-novel-project temp-dir project-name 'en)

          ;; Verify project directory was created
          (should (file-directory-p project-dir))

          ;; Verify marker file exists and contains correct language
          (let ((marker-file (expand-file-name ".org-scribe-project" project-dir)))
            (should (file-exists-p marker-file))
            (with-temp-buffer
              (insert-file-contents marker-file)
              (should (string-match-p "Language: en" (buffer-string)))))

          ;; Verify README.org exists
          (should (file-exists-p (expand-file-name "README.org" project-dir)))

          ;; Verify novel.org exists (English template)
          (should (file-exists-p (expand-file-name "novel.org" project-dir))))

      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest org-scribe-test-novel-project-spanish ()
  "Test novel project creation in Spanish."
  (let* ((temp-dir (make-temp-file "org-scribe-test-novela-" t))
         (project-name "Prueba-Novela-Espanol")
         (project-dir (expand-file-name project-name temp-dir)))
    (unwind-protect
        (progn
          ;; Create project programmatically
          (org-scribe-create-novel-project temp-dir project-name 'es)

          ;; Verify project directory was created
          (should (file-directory-p project-dir))

          ;; Verify marker file exists and contains correct language
          (let ((marker-file (expand-file-name ".org-scribe-project" project-dir)))
            (should (file-exists-p marker-file))
            (with-temp-buffer
              (insert-file-contents marker-file)
              ;; Should contain "Idioma: es" (Spanish) or "Language: es" (fallback)
              (should (or (string-match-p "Idioma: es" (buffer-string))
                         (string-match-p "Language: es" (buffer-string))))))

          ;; Verify README.org exists
          (should (file-exists-p (expand-file-name "README.org" project-dir)))

          ;; Verify novela.org exists (Spanish template)
          (should (file-exists-p (expand-file-name "novela.org" project-dir))))

      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest org-scribe-test-short-story-project-english ()
  "Test short story project creation in English."
  (let* ((temp-dir (make-temp-file "org-scribe-test-story-" t))
         (project-name "Test-English-Story")
         (project-dir (expand-file-name project-name temp-dir)))
    (unwind-protect
        (progn
          ;; Create project programmatically
          (org-scribe-create-short-story-project temp-dir project-name 'en)

          ;; Verify project directory was created
          (should (file-directory-p project-dir))

          ;; Verify marker file exists and contains correct info
          (let ((marker-file (expand-file-name ".org-scribe-project" project-dir)))
            (should (file-exists-p marker-file))
            (with-temp-buffer
              (insert-file-contents marker-file)
              (let ((content (buffer-string)))
                (should (string-match-p "Language: en" content))
                (should (string-match-p "short-story" content)))))

          ;; Verify story.org exists (English template)
          (should (file-exists-p (expand-file-name "story.org" project-dir))))

      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest org-scribe-test-short-story-project-spanish ()
  "Test short story project creation in Spanish."
  (let* ((temp-dir (make-temp-file "org-scribe-test-cuento-" t))
         (project-name "Prueba-Cuento-Espanol")
         (project-dir (expand-file-name project-name temp-dir)))
    (unwind-protect
        (progn
          ;; Create project programmatically
          (org-scribe-create-short-story-project temp-dir project-name 'es)

          ;; Verify project directory was created
          (should (file-directory-p project-dir))

          ;; Verify marker file exists and contains correct info
          (let ((marker-file (expand-file-name ".org-scribe-project" project-dir)))
            (should (file-exists-p marker-file))
            (with-temp-buffer
              (insert-file-contents marker-file)
              (let ((content (buffer-string)))
                ;; Should contain "Idioma: es" (Spanish) or "Language: es" (fallback)
                (should (or (string-match-p "Idioma: es" content)
                           (string-match-p "Language: es" content)))
                (should (string-match-p "short-story" content)))))

          ;; Verify cuento.org exists (Spanish template)
          (should (file-exists-p (expand-file-name "cuento.org" project-dir))))

      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Error Handling Tests

(ert-deftest org-scribe-test-reject-invalid-title ()
  "Test that invalid titles are rejected with appropriate error."
  (let ((temp-dir (make-temp-file "org-scribe-test-invalid-" t)))
    (unwind-protect
        (progn
          ;; Should error with empty title
          (should-error
           (org-scribe-create-novel-project temp-dir "" 'en))

          ;; Should error with path separator
          (should-error
           (org-scribe-create-novel-project temp-dir "my/project" 'en))

          ;; Should error with special character
          (should-error
           (org-scribe-create-novel-project temp-dir "my*project" 'en)))

      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest org-scribe-test-reject-existing-project ()
  "Test that creating over existing directory is rejected."
  (let* ((temp-dir (make-temp-file "org-scribe-test-existing-" t))
         (project-name "Existing-Project")
         (project-dir (expand-file-name project-name temp-dir)))
    (unwind-protect
        (progn
          ;; Create first project
          (org-scribe-create-novel-project temp-dir project-name 'en)

          ;; Trying to create again should error
          (should-error
           (org-scribe-create-novel-project temp-dir project-name 'en)))

      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Test Runner

(defun org-scribe-project-creation-run-tests ()
  "Run all project creation tests and display results."
  (interactive)
  (ert-run-tests-batch "^org-scribe-test-"))

(provide 'test-project-creation)

;;; test-project-creation.el ends here
