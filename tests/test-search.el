;;; test-search.el --- Tests for search functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for org-ql based search functions.
;; Tests function availability and input validation for searches by
;; POV, character, plot, and location.
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

;;; Module Loading Tests

(ert-deftest test-search-module-loads ()
  "Test that org-scribe-search module loads without errors."
  (should (featurep 'org-scribe-search)))

;;; Function Availability Tests

(ert-deftest test-search-functions-defined ()
  "Test that all public search functions are defined."
  ;; Search by property
  (should (fboundp 'org-scribe-org-find-pov))
  (should (fboundp 'org-scribe-org-find-character))
  (should (fboundp 'org-scribe-org-find-plot))
  (should (fboundp 'org-scribe-org-find-location))

  ;; Search TODO items
  (should (fboundp 'org-scribe-search-todos-recursive)))

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
  (should-error (org-scribe-org-find-pov "") :type 'user-error)
  (should-error (org-scribe-org-find-pov "  ") :type 'user-error))

(ert-deftest test-search-character-requires-name ()
  "Test that character search requires a name."
  (should-error (org-scribe-org-find-character "") :type 'user-error)
  (should-error (org-scribe-org-find-character "  ") :type 'user-error))

(ert-deftest test-search-plot-requires-keyword ()
  "Test that plot search requires a keyword."
  (should-error (org-scribe-org-find-plot "") :type 'user-error)
  (should-error (org-scribe-org-find-plot "  ") :type 'user-error))

(ert-deftest test-search-location-requires-name ()
  "Test that location search requires a name."
  (should-error (org-scribe-org-find-location "") :type 'user-error)
  (should-error (org-scribe-org-find-location "  ") :type 'user-error))

;;; TODO Search Tests

(ert-deftest test-search-todos-recursive-execution ()
  "Test that search-todos-recursive executes without errors.
This test verifies the function can handle the :auto-map super-group
correctly by extracting file names from item text properties."
  (skip-unless (featurep 'org-ql))

  ;; Create temporary directory with test files
  (let* ((temp-dir (make-temp-file "org-scribe-test-" t))
         (test-file-1 (expand-file-name "test1.org" temp-dir))
         (test-file-2 (expand-file-name "test2.org" temp-dir)))

    (unwind-protect
        (progn
          ;; Create test org files with TODO items
          (with-temp-file test-file-1
            (insert "* TODO First task\n")
            (insert "* DONE Completed task\n")
            (insert "* TODO Second task\n"))

          (with-temp-file test-file-2
            (insert "* TODO Another task\n")
            (insert "* Some heading\n"))

          ;; Open one of the files and run the search
          (with-current-buffer (find-file-noselect test-file-1)
            ;; This should not raise an error
            (should-not (condition-case err
                           (progn
                             (org-scribe-search-todos-recursive)
                             nil) ; No error
                         (error err)))))

      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; org-scribe-edit-string regexp (M1)

(ert-deftest test-edit-string-matches-edit-marker ()
  "org-scribe-edit-string matches a literal *EDIT* marker.
Regression test for M1: the Lisp string \"\\*EDIT\\*\\|\\*NOTE\\*\" (single
backslash before each star) produces the regexp *EDIT*\\|*NOTE*, in which
a bare `*' is not a metacharacter needing escape but the *trailing* `*'
after EDIT/NOTE is misread as quantifying the preceding letter, so the
literal closing star was never required to match."
  (should (string-match-p org-scribe-edit-string "see *EDIT* here")))

(ert-deftest test-edit-string-matches-note-marker ()
  "org-scribe-edit-string matches a literal *NOTE* marker."
  (should (string-match-p org-scribe-edit-string "do *NOTE* this")))

(ert-deftest test-edit-string-rejects-broken-match-without-closing-star ()
  "org-scribe-edit-string must not match text missing the closing star.
Before the fix, the regexp actually compiled to *EDI(T*) i.e. \"*EDI\"
followed by zero-or-more \"T\"s, so \"*EDI\" alone (no closing star)
incorrectly matched."
  (should-not (string-match-p org-scribe-edit-string "see *EDI here, no closing star"))
  (should-not (string-match-p org-scribe-edit-string "do *NOT do this, no closing star")))

;;; org-scribe-search-edits-recursive: search root resolution
;;
;; `rgrep' is stubbed so these exercise root resolution only, without
;; spawning a find/grep subprocess.

(ert-deftest test-search-edits-uses-project-root ()
  "Inside a project, the edits search greps from the project root."
  (let* ((temp-dir (file-name-as-directory (make-temp-file "org-scribe-edits-" t)))
         (captured nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".org-scribe-project" temp-dir)
            (insert "Type: novel\n"))
          (cl-letf (((symbol-function 'rgrep)
                     (lambda (regexp files dir &optional _confirm)
                       (setq captured (list regexp files dir)))))
            (let ((default-directory temp-dir))
              (org-scribe-search-edits-recursive)))
          (should (equal org-scribe-edit-string (nth 0 captured)))
          (should (equal "*.org" (nth 1 captured)))
          (should (file-equal-p temp-dir (nth 2 captured))))
      (delete-directory temp-dir t))))

(ert-deftest test-search-edits-falls-back-outside-project ()
  "Outside a project, the edits search falls back to the buffer's directory.
Regression test: `org-scribe-project-root' returns nil outside a
project, and passing nil to `rgrep' as its DIR argument fails instead of
degrading.  `org-scribe-search-todos-recursive' already had this
fallback; the edits search did not."
  (let* ((temp-dir (file-name-as-directory (make-temp-file "org-scribe-noproj-" t)))
         (captured-dir 'unset))
    (unwind-protect
        (cl-letf (((symbol-function 'rgrep)
                   (lambda (_regexp _files dir &optional _confirm)
                     (setq captured-dir dir)))
                  ((symbol-function 'org-scribe-project-root) (lambda () nil)))
          (let ((default-directory temp-dir))
            (org-scribe-search-edits-recursive))
          (should (stringp captured-dir))
          (should (file-equal-p temp-dir captured-dir)))
      (delete-directory temp-dir t))))

;;; Run tests

(defun org-scribe-search-run-tests ()
  "Run all search function tests."
  (interactive)
  (ert "^test-search-"))

(provide 'test-search)

;;; test-search.el ends here
