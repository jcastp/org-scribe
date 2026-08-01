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

(ert-deftest test-search-edits-rgrep-uses-project-root ()
  "Inside a project, the rgrep fallback greps from the project root."
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
              (org-scribe-search-edits-rgrep)))
          (should (equal org-scribe-edit-string (nth 0 captured)))
          (should (equal "*.org" (nth 1 captured)))
          (should (file-equal-p temp-dir (nth 2 captured))))
      (delete-directory temp-dir t))))

(ert-deftest test-search-edits-rgrep-falls-back-outside-project ()
  "Outside a project, the rgrep fallback uses the buffer's directory.
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
            (org-scribe-search-edits-rgrep))
          (should (stringp captured-dir))
          (should (file-equal-p temp-dir captured-dir)))
      (delete-directory temp-dir t))))

;;; Edit marker index

(defmacro org-scribe-test--with-edits-project (dir-var content &rest body)
  "Run BODY in a temp project rooted at DIR-VAR containing CONTENT.
CONTENT is inserted into \"novel.org\" at the project root."
  (declare (indent 2))
  `(let ((,dir-var (file-name-as-directory
                    (make-temp-file "org-scribe-index-" t))))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".org-scribe-project" ,dir-var)
             (insert "Type: novel\n"))
           (with-temp-file (expand-file-name "novel.org" ,dir-var)
             (insert ,content))
           ,@body)
       (delete-directory ,dir-var t))))

(defun org-scribe-test--markers (dir)
  "Collect edit markers from novel.org in DIR."
  (org-scribe--edits-collect-file (expand-file-name "novel.org" dir)))

(ert-deftest test-search-edits-index-parses-category ()
  "An *EDIT* with a known category is split into category and body."
  (org-scribe-test--with-edits-project dir
      "* Scene 1\n#+begin_comment\n*EDIT*: plot - test this\n#+end_comment\n"
    (let ((marker (car (org-scribe-test--markers dir))))
      (should (equal "EDIT" (plist-get marker :type)))
      (should (equal "plot" (plist-get marker :category)))
      (should (equal "test this" (plist-get marker :text)))
      (should (equal "Scene 1" (plist-get marker :heading))))))

(ert-deftest test-search-edits-index-keeps-multiline-body ()
  "A marker spanning several lines keeps its whole body.
This is the main thing the structured index buys over a line-based
grep, which would show only the line carrying the marker."
  (org-scribe-test--with-edits-project dir
      (concat "* Scene 1\n#+begin_comment\n*EDIT*: prose - first line\n"
              "second line\nthird line\n#+end_comment\n")
    (let ((marker (car (org-scribe-test--markers dir))))
      (should (equal "first line\nsecond line\nthird line"
                     (plist-get marker :text))))))

(ert-deftest test-search-edits-index-unknown-category-goes-to-other ()
  "An *EDIT* with an unrecognised category lands in the catch-all.
A typo must move a marker, never hide it, so the bogus category is
kept in the visible body."
  (org-scribe-test--with-edits-project dir
      "* Scene 1\n#+begin_comment\n*EDIT*: plto - typo here\n#+end_comment\n"
    (let ((marker (car (org-scribe-test--markers dir))))
      (should (null (plist-get marker :category)))
      (should (equal "plto - typo here" (plist-get marker :text))))))

(ert-deftest test-search-edits-index-empty-category-goes-to-other ()
  "An *EDIT* with an empty category lands in the catch-all.
Tempel inserts the \" - \" separator even when the category prompt is
answered with RET, so this shape occurs in normal use."
  (org-scribe-test--with-edits-project dir
      "* Scene 1\n#+begin_comment\n*EDIT*:  - no category\n#+end_comment\n"
    (let ((marker (car (org-scribe-test--markers dir))))
      (should (null (plist-get marker :category)))
      (should (equal "no category" (plist-get marker :text))))))

(ert-deftest test-search-edits-index-note-with-dash-is-not-split ()
  "A *NOTE* containing \" - \" is never split into a category.
Only *EDIT* carries a category; *NOTE* bodies are taken verbatim."
  (org-scribe-test--with-edits-project dir
      (concat "* Scene 1\n#+begin_comment\n"
              "*NOTE*: plot - this whole line is the note\n#+end_comment\n")
    (let ((marker (car (org-scribe-test--markers dir))))
      (should (equal "NOTE" (plist-get marker :type)))
      (should (null (plist-get marker :category)))
      (should (equal "plot - this whole line is the note"
                     (plist-get marker :text))))))

(ert-deftest test-search-edits-index-ignores-prose-outside-comment ()
  "Markers in ordinary prose are not indexed.
Only `comment-block' elements are traversed, so prose that merely
mentions *NOTE* cannot produce a false positive — the reason the index
is immune to a problem the rgrep search has."
  (org-scribe-test--with-edits-project dir
      "* Scene 1\nShe wrote *NOTE* on the margin, and *EDIT*: plot - nope.\n"
    (should (null (org-scribe-test--markers dir)))))

(ert-deftest test-search-edits-index-ignores-summary-marker ()
  "*SUMMARY* markers are deliberately excluded from the index.
They describe prose that does not exist yet, live under a TODO heading,
and are found with the TODO search instead.  Do not \"fix\" this by
adding *SUMMARY* to `org-scribe--edits-marker-regexp'."
  (org-scribe-test--with-edits-project dir
      (concat "* TODO Scene 12\n#+begin_comment\n"
              "*SUMMARY*: Alex opens the box.\n#+end_comment\n")
    (should (null (org-scribe-test--markers dir)))))

(ert-deftest test-search-edits-index-handles-block-before-first-heading ()
  "A marker before the first heading is indexed with a nil heading."
  (org-scribe-test--with-edits-project dir
      "#+begin_comment\n*NOTE*: no heading above me\n#+end_comment\n* Scene 1\n"
    (let ((marker (car (org-scribe-test--markers dir))))
      (should marker)
      (should (null (plist-get marker :heading)))
      (should (equal "no heading above me" (plist-get marker :text))))))

(ert-deftest test-search-edits-index-records-line-number ()
  "Each marker records the line it occupies, for navigation."
  (org-scribe-test--with-edits-project dir
      (concat "* Scene 1\nProse.\n\n#+begin_comment\n"
              "*EDIT*: scene - fix this\n#+end_comment\n")
    (let ((marker (car (org-scribe-test--markers dir))))
      ;; Lines: 1 heading, 2 prose, 3 blank, 4 begin_comment, 5 marker.
      (should (equal 5 (plist-get marker :line))))))

(ert-deftest test-search-edits-index-collects-multiple-markers-per-block ()
  "Several markers in one comment block are indexed separately."
  (org-scribe-test--with-edits-project dir
      (concat "* Scene 1\n#+begin_comment\n*EDIT*: plot - first\n"
              "*NOTE*: second\n#+end_comment\n")
    (let ((markers (org-scribe-test--markers dir)))
      (should (equal 2 (length markers)))
      (should (equal '("EDIT" "NOTE") (mapcar (lambda (m) (plist-get m :type))
                                              markers))))))

(ert-deftest test-search-edits-index-renders-grouped-buffer ()
  "The index buffer groups edits by category and notes separately."
  (org-scribe-test--with-edits-project dir
      (concat "* Scene 1\n#+begin_comment\n*EDIT*: plot - plot problem\n"
              "#+end_comment\n* Scene 2\n#+begin_comment\n"
              "*NOTE*: remember this\n#+end_comment\n")
    (let ((buffer (org-scribe--edits-build dir)))
      (unwind-protect
          (with-current-buffer buffer
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "^\\* Edits$" text))
              (should (string-match-p "^\\*\\* plot$" text))
              (should (string-match-p "plot problem" text))
              (should (string-match-p "^\\* Notes$" text))
              (should (string-match-p "remember this" text))
              ;; Entries link back to the source line.
              (should (string-match-p "\\[\\[file:.*::[0-9]+\\]\\[Scene 1\\]\\]" text))))
        (kill-buffer buffer)))))

(ert-deftest test-search-edits-index-empty-categories-configurable ()
  "Empty categories appear or not per `org-scribe-edits-index-show-empty-categories'."
  (org-scribe-test--with-edits-project dir
      "* Scene 1\n#+begin_comment\n*EDIT*: plot - only plot\n#+end_comment\n"
    (dolist (case '((t . t) (nil . nil)))
      (let* ((org-scribe-edits-index-show-empty-categories (car case))
             (buffer (org-scribe--edits-build dir)))
        (unwind-protect
            (with-current-buffer buffer
              (let ((has-empty (string-match-p
                                "^\\*\\* prose$"
                                (buffer-substring-no-properties
                                 (point-min) (point-max)))))
                (should (equal (cdr case) (and has-empty t)))))
          (kill-buffer buffer))))))

(ert-deftest test-search-edits-compat-alias ()
  "The old command name still works, now pointing at the index."
  (should (fboundp 'org-scribe-search-edits-recursive))
  (should (eq (indirect-function 'org-scribe-search-edits-recursive)
              (indirect-function 'org-scribe-search-edits))))

(ert-deftest test-search-edits-refresh-on-save-is-inert-when-hidden ()
  "The save hook does no work while the index buffer is not displayed.
The index is rebuilt by reparsing every project file, so it must not
run on every save just because an index was opened once."
  (org-scribe-test--with-edits-project dir
      "* Scene 1\n#+begin_comment\n*EDIT*: plot - one\n#+end_comment\n"
    (let ((buffer (org-scribe--edits-build dir))
          (rebuilt nil))
      (unwind-protect
          (cl-letf (((symbol-function 'org-scribe--edits-build)
                     (lambda (&rest _) (setq rebuilt t))))
            ;; Batch mode: the buffer exists but is in no window.
            (with-temp-buffer
              (setq buffer-file-name (expand-file-name "novel.org" dir))
              (org-scribe--edits-refresh-on-save))
            (should-not rebuilt))
        (kill-buffer buffer)))))

;;; Run tests

(defun org-scribe-search-run-tests ()
  "Run all search function tests."
  (interactive)
  (ert "^test-search-"))

(provide 'test-search)

;;; test-search.el ends here
