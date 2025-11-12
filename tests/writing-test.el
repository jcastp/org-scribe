;;; writing-test.el --- Tests for emacs-writing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Basic test infrastructure for emacs-writing package.
;; Uses ERT (Emacs Lisp Regression Testing).

;;; Code:

(require 'ert)
(require 'org)

;; Add parent directory to load path
(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path parent-dir)
  (add-to-list 'load-path (expand-file-name "core" parent-dir))
  (add-to-list 'load-path (expand-file-name "counting" parent-dir)))

(require 'writing-core)
(require 'writing-config)

;;; Core Tests

(ert-deftest writing-test-sanitize-filename ()
  "Test that filename sanitization works correctly."
  (should (string= "test_file" (writing/sanitize-filename "test/file")))
  (should (string= "test_file" (writing/sanitize-filename "test\\file")))
  (should (string= "test_file" (writing/sanitize-filename "test:file")))
  (should (string= "file" (writing/sanitize-filename ".file")))
  (should (string= "file" (writing/sanitize-filename "...file"))))

(ert-deftest writing-test-window-perc ()
  "Test window percentage calculation."
  (let ((frame-width 200))
    (cl-letf (((symbol-function 'frame-width) (lambda () frame-width)))
      (should (= 50 (writing/window-perc 0.25)))
      (should (= 100 (writing/window-perc 0.5)))
      (should (= 200 (writing/window-perc 1.0))))))

(ert-deftest writing-test-project-root ()
  "Test project root detection."
  ;; This test requires a temporary directory structure
  (let* ((temp-dir (make-temp-file "writing-test-" t))
         (default-directory temp-dir))
    (unwind-protect
        (progn
          ;; Should return current directory when no project markers exist
          (should (string= temp-dir (writing-project-root))))
      ;; Cleanup
      (delete-directory temp-dir t))))

;;; Configuration Tests

(ert-deftest writing-test-config-variables-exist ()
  "Test that configuration variables are defined."
  (should (boundp 'writing-novel-file-names))
  (should (boundp 'writing-notes-dir-names))
  (should (boundp 'my-writing-env-work-theme))
  (should (boundp 'my-writing-env-normal-theme))
  (should (boundp 'writing/scene-break-replacements)))

(ert-deftest writing-test-scene-break-replacements ()
  "Test scene break replacement configuration."
  (should (alist-get 'ascii writing/scene-break-replacements))
  (should (alist-get 'html writing/scene-break-replacements))
  (should (alist-get 'latex writing/scene-break-replacements))
  (should (alist-get t writing/scene-break-replacements)))

;;; Utility Tests

(ert-deftest writing-test-feature-detection ()
  "Test feature detection mechanism."
  (should (eq nil (writing-check-feature 'nonexistent-package)))
  (should (eq t (writing-check-feature 'org))))

;;; Running Tests

(defun writing-run-tests ()
  "Run all emacs-writing tests."
  (interactive)
  (ert-run-tests-interactively "^writing-test-"))

(provide 'writing-test)

;;; writing-test.el ends here
