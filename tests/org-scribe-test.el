;;; org-scribe-test.el --- Tests for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Basic test infrastructure for org-scribe package.
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

(require 'org-scribe-core)
(require 'org-scribe-config)

;;; Core Tests

(ert-deftest org-scribe-test-sanitize-filename ()
  "Test that filename sanitization works correctly."
  (should (string= "test_file" (org-scribe-sanitize-filename "test/file")))
  (should (string= "test_file" (org-scribe-sanitize-filename "test\\file")))
  (should (string= "test_file" (org-scribe-sanitize-filename "test:file")))
  (should (string= "file" (org-scribe-sanitize-filename ".file")))
  (should (string= "file" (org-scribe-sanitize-filename "...file"))))

(ert-deftest org-scribe-test-window-perc ()
  "Test window percentage calculation."
  (let ((frame-width 200))
    (cl-letf (((symbol-function 'frame-width) (lambda () frame-width)))
      (should (= 50 (org-scribe-window-perc 0.25)))
      (should (= 100 (org-scribe-window-perc 0.5)))
      (should (= 200 (org-scribe-window-perc 1.0))))))

(ert-deftest org-scribe-test-project-root ()
  "Test project root detection."
  ;; This test requires a temporary directory structure
  (let* ((temp-dir (make-temp-file "org-scribe-test-" t))
         (default-directory temp-dir))
    (unwind-protect
        (progn
          ;; Should return current directory when no project markers exist
          (should (string= temp-dir (org-scribe-project-root))))
      ;; Cleanup
      (delete-directory temp-dir t))))

;;; Configuration Tests

(ert-deftest org-scribe-test-config-variables-exist ()
  "Test that configuration variables are defined."
  (should (boundp 'org-scribe-env-work-theme))
  (should (boundp 'org-scribe-env-normal-theme))
  (should (boundp 'org-scribe-scene-break-replacements)))

;; The contents of `org-scribe-scene-break-replacements' are asserted in
;; test-export.el, next to the filter that consumes them, and
;; `org-scribe-check-feature' in test-core-extended.el, which covers both
;; the available and unavailable branches.  Both were duplicated here.

;;; Running Tests

(defun org-scribe-run-tests ()
  "Run all org-scribe tests."
  (interactive)
  (ert-run-tests-interactively "^org-scribe-test-"))

;; Captured at load time: `load-file-name' is nil when the test runs.
(defconst org-scribe-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root.")

(ert-deftest test-no-obsolete-let-macros-in-shipped-code ()
  "Shipped code uses `when-let*'/`if-let*', obsolete-free on Emacs 31.
The unstarred forms print a byte-compile warning for every use at load."
  (let* ((root org-scribe-test--root)
         (hits nil))
    (dolist (file (directory-files-recursively root "\\.el\\'"))
      (unless (string-match-p "/\\(tests\\|\\.git\\)/" file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward "(\\(when-let\\|if-let\\|and-let\\) " nil t)
            (push (format "%s:%d" (file-name-nondirectory file) (line-number-at-pos)) hits)))))
    (should (equal nil hits))))

(provide 'org-scribe-test)

;;; org-scribe-test.el ends here
