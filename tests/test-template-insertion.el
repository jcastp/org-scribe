;;; test-template-insertion.el --- Test template insertion with i18n -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for template insertion (scene/chapter) with internationalization support.
;; Tests that templates can be inserted with language-specific defaults.

;;; Code:

(require 'ert)
(require 'org-scribe-i18n)
(require 'org-scribe-project)

;;; Scene Insertion Tests

(ert-deftest org-scribe-test-insert-scene-with-name ()
  "Test scene insertion with explicit name."
  (with-temp-buffer
    (org-mode)
    ;; Insert scene programmatically
    (org-scribe-insert-scene "Battle Scene")

    ;; Verify heading was created
    (goto-char (point-min))
    (should (re-search-forward "\\*\\*\\* TODO Battle Scene :ignore:" nil t))

    ;; Verify properties drawer
    (should (re-search-forward ":PROPERTIES:" nil t))
    (should (re-search-forward ":PoV:" nil t))
    (should (re-search-forward ":Characters:" nil t))
    (should (re-search-forward ":Plot:" nil t))))

(ert-deftest org-scribe-test-insert-scene-empty-name-english ()
  "Test scene insertion with empty name defaults to English."
  (with-temp-buffer
    (org-mode)
    (let ((org-scribe-i18n--project-language-cache nil)
          (default-directory "/tmp/nonexistent/"))
      ;; Insert scene with empty name
      (org-scribe-insert-scene "")

      ;; Verify default English name was used
      (goto-char (point-min))
      (should (re-search-forward "\\*\\*\\* TODO New scene :ignore:" nil t)))))

(ert-deftest org-scribe-test-insert-scene-in-spanish-project ()
  "Test scene insertion in Spanish project uses Spanish default."
  (let* ((temp-dir (make-temp-file "org-scribe-test-es-" t))
         (temp-file (expand-file-name "test.org" temp-dir))
         (marker-file (expand-file-name ".org-scribe-project" temp-dir)))
    (unwind-protect
        (progn
          ;; Create Spanish project marker
          (with-temp-file marker-file
            (insert "# Language: es\n"))

          ;; Create test org file
          (with-temp-file temp-file
            (insert "* Test\n"))

          ;; Open file and insert scene
          (find-file temp-file)
          (goto-char (point-max))
          (org-scribe-i18n-clear-cache)  ; Clear cache to pick up new marker
          (org-scribe-insert-scene "")

          ;; Verify Spanish default name was used
          (goto-char (point-min))
          (should (re-search-forward "\\*\\*\\* TODO Nueva escena :ignore:" nil t))

          (kill-buffer (current-buffer)))

      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest org-scribe-test-insert-scene-not-in-org-mode ()
  "Test scene insertion fails in non-org-mode buffer."
  (with-temp-buffer
    (fundamental-mode)
    (should-error (org-scribe-insert-scene "Test Scene"))))

;;; Chapter Insertion Tests

(ert-deftest org-scribe-test-insert-chapter-with-name ()
  "Test chapter insertion with explicit name."
  (with-temp-buffer
    (org-mode)
    ;; Insert chapter programmatically
    (org-scribe-insert-chapter "Chapter One")

    ;; Verify heading was created
    (goto-char (point-min))
    (should (re-search-forward "\\*\\* TODO Chapter One :ignore:" nil t))

    ;; Verify properties drawer
    (should (re-search-forward ":PROPERTIES:" nil t))
    (should (re-search-forward ":WORD-OBJECTIVE: 5000" nil t))
    (should (re-search-forward ":WORDCOUNT: 0" nil t))))

(ert-deftest org-scribe-test-insert-chapter-empty-name-english ()
  "Test chapter insertion with empty name defaults to English."
  (with-temp-buffer
    (org-mode)
    (let ((org-scribe-i18n--project-language-cache nil)
          (default-directory "/tmp/nonexistent/"))
      ;; Insert chapter with empty name
      (org-scribe-insert-chapter "")

      ;; Verify default English name was used
      (goto-char (point-min))
      (should (re-search-forward "\\*\\* TODO New chapter :ignore:" nil t)))))

(ert-deftest org-scribe-test-insert-chapter-in-spanish-project ()
  "Test chapter insertion in Spanish project uses Spanish default."
  (let* ((temp-dir (make-temp-file "org-scribe-test-es-chap-" t))
         (temp-file (expand-file-name "test.org" temp-dir))
         (marker-file (expand-file-name ".org-scribe-project" temp-dir)))
    (unwind-protect
        (progn
          ;; Create Spanish project marker
          (with-temp-file marker-file
            (insert "# Language: es\n"))

          ;; Create test org file
          (with-temp-file temp-file
            (insert "* Test\n"))

          ;; Open file and insert chapter
          (find-file temp-file)
          (goto-char (point-max))
          (org-scribe-i18n-clear-cache)  ; Clear cache to pick up new marker
          (org-scribe-insert-chapter "")

          ;; Verify Spanish default name was used
          (goto-char (point-min))
          (should (re-search-forward "\\*\\* TODO Nuevo capítulo :ignore:" nil t))

          (kill-buffer (current-buffer)))

      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest org-scribe-test-insert-chapter-not-in-org-mode ()
  "Test chapter insertion fails in non-org-mode buffer."
  (with-temp-buffer
    (fundamental-mode)
    (should-error (org-scribe-insert-chapter "Test Chapter"))))

;;; Integration Tests

(ert-deftest org-scribe-test-insert-multiple-scenes ()
  "Test inserting multiple scenes in succession."
  (with-temp-buffer
    (org-mode)
    (org-scribe-insert-scene "Opening")
    (goto-char (point-max))
    (org-scribe-insert-scene "Conflict")
    (goto-char (point-max))
    (org-scribe-insert-scene "Resolution")

    ;; Verify all three scenes exist
    (goto-char (point-min))
    (should (re-search-forward "\\*\\*\\* TODO Opening :ignore:" nil t))
    (should (re-search-forward "\\*\\*\\* TODO Conflict :ignore:" nil t))
    (should (re-search-forward "\\*\\*\\* TODO Resolution :ignore:" nil t))))

(ert-deftest org-scribe-test-scene-has-all-properties ()
  "Test that inserted scene has all required properties."
  (with-temp-buffer
    (org-mode)
    (org-scribe-insert-scene "Test Scene")

    (goto-char (point-min))
    (let ((required-properties '(":PoV:" ":Characters:" ":Plot:" ":Timeline:"
                                ":Location:" ":Description:" ":Summary:"
                                ":Scene-motivation:" ":Conflict-source:"
                                ":What-is-at-stake:" ":Emotion:" ":Comment:"
                                ":WORD-OBJECTIVE:")))
      (dolist (prop required-properties)
        (goto-char (point-min))
        (should (re-search-forward (regexp-quote prop) nil t))))))

(ert-deftest org-scribe-test-chapter-has-required-properties ()
  "Test that inserted chapter has required properties."
  (with-temp-buffer
    (org-mode)
    (org-scribe-insert-chapter "Test Chapter")

    (goto-char (point-min))
    (should (re-search-forward ":WORD-OBJECTIVE: 5000" nil t))
    (should (re-search-forward ":WORDCOUNT: 0" nil t))))

;;; Test Runner

(defun org-scribe-template-insertion-run-tests ()
  "Run all template insertion tests and display results."
  (interactive)
  (ert-run-tests-batch "^org-scribe-test-insert-"))

(provide 'test-template-insertion)

;;; test-template-insertion.el ends here
