;;; test-refile.el --- Tests for project-wide org-refile targets -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; A writer refiling inside an org-scribe project wants every project file
;; offered as a target, not just the current buffer (Org's own default) —
;; moving a scene between chapters, or filing a loose note directly under
;; the right character, should not require switching buffers first.
;;
;; `org-scribe--project-refile-files' derives the file list from
;; `org-scribe-project-structure', which is also how README.org and the
;; writing journal end up excluded: neither is a key that function ever
;; resolves, so no name-based filtering is needed here to leave them out.
;;
;; `org-scribe--refile-enable' / `-disable' apply and restore
;; `org-refile-targets' / `org-refile-use-outline-path' buffer-locally, and
;; must round-trip a saved value of nil correctly — `org-refile-use-outline-path'
;; defaults to nil, so a naive "nil means nothing was saved" check would
;; silently fail to restore it, the same trap a bare `Weight' read falls
;; into elsewhere in this package.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)

(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory)))

(require 'org-scribe-core)
(require 'org-scribe-config)

;; `org-scribe-mode' is the minor mode `org-scribe.el' defines; these tests
;; exercise only core.el's refile logic and do not load the whole package,
;; so forward-declare the variable it reads, mirroring the same
;; early-defvar pattern `org-scribe-messages.el' uses for
;; `org-scribe-message-language'.
(defvar org-scribe-mode nil)

;;; Helpers

(defmacro test-refile--with-temp-project (dir-setup &rest body)
  "Execute BODY inside a temp directory configured by DIR-SETUP.
DIR-SETUP is a list of file/dir relative paths to create:
  - Strings ending in '/' create directories
  - Pairs (PATH . CONTENT) create files with content
  - Other strings create empty files
Clears the project type cache before and after, and runs BODY in a
freshly created buffer visiting the project so buffer-local refile
variables can be exercised."
  (declare (indent 1))
  `(let* ((temp-dir (make-temp-file "org-scribe-refile-test-" t))
          (default-directory temp-dir))
     (unwind-protect
         (progn
           (setq org-scribe--project-type-cache nil)
           (dolist (item ,dir-setup)
             (cond
              ((and (stringp item) (string-suffix-p "/" item))
               (make-directory (expand-file-name item temp-dir) t))
              ((consp item)
               (let ((path (expand-file-name (car item) temp-dir)))
                 (make-directory (file-name-directory path) t)
                 (with-temp-file path (insert (cdr item)))))
              ((stringp item)
               (write-region "" nil (expand-file-name item temp-dir)))))
           (with-temp-buffer
             (setq default-directory temp-dir)
             ,@body))
       (setq org-scribe--project-type-cache nil)
       (delete-directory temp-dir t))))

;;; org-scribe--project-refile-files

(ert-deftest test-refile-functions-defined ()
  "Test that the refile-target functions and defcustom are defined."
  (should (fboundp 'org-scribe--project-refile-files))
  (should (fboundp 'org-scribe--refile-enable))
  (should (fboundp 'org-scribe--refile-disable))
  (should (fboundp 'org-scribe--refile-enabled-p))
  (should (fboundp 'org-scribe--refile-maybe-setup))
  (should (boundp 'org-scribe-refile-project-wide)))

(ert-deftest test-refile-files-empty-project ()
  "Test that a project with no resolvable files returns an empty list."
  (test-refile--with-temp-project '()
    (should (null (org-scribe--project-refile-files)))))

(ert-deftest test-refile-files-novel-project ()
  "Test that every existing novel-project file is included."
  (test-refile--with-temp-project
      '(("novel.org" . "#+TITLE: Test\n")
        ("objects/characters.org" . "#+TITLE: Characters\n")
        ("objects/locations.org" . "#+TITLE: Locations\n")
        ("objects/plot.org" . "#+TITLE: Plot\n")
        ("design.org" . "#+TITLE: Design\n")
        ("plan.org" . "# Writing plan\n"))
    (let ((files (org-scribe--project-refile-files)))
      (should (= 6 (length files)))
      (should (cl-some (lambda (f) (string-suffix-p "novel.org" f)) files))
      (should (cl-some (lambda (f) (string-suffix-p "objects/characters.org" f)) files))
      (should (cl-some (lambda (f) (string-suffix-p "objects/locations.org" f)) files))
      (should (cl-some (lambda (f) (string-suffix-p "objects/plot.org" f)) files))
      (should (cl-some (lambda (f) (string-suffix-p "design.org" f)) files))
      (should (cl-some (lambda (f) (string-suffix-p "plan.org" f)) files)))))

(ert-deftest test-refile-files-short-story-project ()
  "Test that a consolidated short-story notes.org is included."
  (test-refile--with-temp-project
      '(("story.org" . "#+TITLE: Test\n")
        ("notes.org" . "#+TITLE: Notes\n"))
    (let ((files (org-scribe--project-refile-files)))
      (should (= 2 (length files)))
      (should (cl-some (lambda (f) (string-suffix-p "story.org" f)) files))
      (should (cl-some (lambda (f) (string-suffix-p "notes.org" f)) files)))))

(ert-deftest test-refile-files-excludes-journal-and-readme ()
  "A writing journal and README are never refile sources.
Neither is a key `org-scribe-project-structure' ever resolves, so their
mere presence on disk must not smuggle them into the file list."
  (test-refile--with-temp-project
      '(("novel.org" . "#+TITLE: Test\n")
        ("journal.org" . "#+TITLE: Journal\n")
        ("README.org" . "#+TITLE: Read me\n"))
    (let ((files (org-scribe--project-refile-files)))
      (should (= 1 (length files)))
      (should-not (cl-some (lambda (f) (string-suffix-p "journal.org" f)) files))
      (should-not (cl-some (lambda (f) (string-suffix-p "README.org" f)) files)))))

;;; End-to-end: Org itself must accept the spec

(ert-deftest test-refile-get-targets-accepts-the-spec-without-erroring ()
  "`org-refile-get-targets' must actually parse the spec set by enable.
A plain equality check on `org-refile-targets' is not enough: the entry
`(org-scribe--project-refile-files)' (an omitted target description,
read as nil) is equally `equal'-comparable-looking to a caller but is
rejected by `org-refile-get-targets' with \"Bad refiling target
description\", because nil is not one of the forms it recognizes — only
`t' means \"all headlines.\"  This exercises the real Org entry point so
that regression is caught here rather than in normal use."
  (test-refile--with-temp-project
      '(("novel.org" . "* Chapter One\n** Scene\nSome text.\n")
        ("objects/characters.org" . "* Alice\n:PROPERTIES:\n:Role: Protagonist\n:END:\n"))
    (org-mode)
    (org-scribe--refile-enable)
    (unwind-protect
        (let ((targets (org-refile-get-targets)))
          (should targets)
          (should (cl-some (lambda (tg) (string-suffix-p "novel.org" (nth 1 tg))) targets))
          (should (cl-some (lambda (tg) (string-suffix-p "characters.org" (nth 1 tg))) targets)))
      (dolist (rel '("novel.org" "objects/characters.org"))
        (let ((buf (get-file-buffer (expand-file-name rel temp-dir))))
          (when buf (kill-buffer buf)))))))

;;; org-scribe--refile-enable / -disable

(ert-deftest test-refile-enable-sets-buffer-local-targets ()
  "Test that enabling points targets at the project file-list function."
  (test-refile--with-temp-project '(("novel.org" . "#+TITLE: Test\n"))
    (org-scribe--refile-enable)
    (should (equal org-refile-targets '((org-scribe--project-refile-files . t))))
    (should (eq org-refile-use-outline-path 'file))
    (should (eq org-refile-use-cache t))
    (should (org-scribe--refile-enabled-p))))

(ert-deftest test-refile-disable-restores-saved-values ()
  "Test that disabling restores the buffer's pre-enable values exactly."
  (test-refile--with-temp-project '(("novel.org" . "#+TITLE: Test\n"))
    (setq-local org-refile-targets '((nil . (:maxlevel . 3))))
    (setq-local org-refile-use-outline-path nil)
    (org-scribe--refile-enable)
    (org-scribe--refile-disable)
    (should (equal org-refile-targets '((nil . (:maxlevel . 3)))))
    (should (eq org-refile-use-outline-path nil))
    (should-not (org-scribe--refile-enabled-p))))

(ert-deftest test-refile-disable-restores-nil-outline-path-correctly ()
  "A saved value of nil must round-trip, not be mistaken for \"unset\".
`org-refile-use-outline-path' defaults to nil, which is exactly the trap
a naive save/restore falls into (see commentary at the top of this file)."
  (test-refile--with-temp-project '(("novel.org" . "#+TITLE: Test\n"))
    (should (eq org-refile-use-outline-path nil))
    (org-scribe--refile-enable)
    (should (eq org-refile-use-outline-path 'file))
    (org-scribe--refile-disable)
    (should (eq org-refile-use-outline-path nil))))

(ert-deftest test-refile-disable-is-noop-when-never-enabled ()
  "Test that disabling before ever enabling leaves refile variables alone."
  (test-refile--with-temp-project '(("novel.org" . "#+TITLE: Test\n"))
    (setq-local org-refile-targets '((nil . (:maxlevel . 3))))
    (org-scribe--refile-disable)
    (should (equal org-refile-targets '((nil . (:maxlevel . 3)))))
    (should-not (org-scribe--refile-enabled-p))))

(ert-deftest test-refile-enable-twice-keeps-original-saved-value ()
  "Test that a second enable does not overwrite the originally saved value."
  (test-refile--with-temp-project '(("novel.org" . "#+TITLE: Test\n"))
    (setq-local org-refile-targets '((nil . (:maxlevel . 3))))
    (org-scribe--refile-enable)
    (org-scribe--refile-enable)
    (org-scribe--refile-disable)
    (should (equal org-refile-targets '((nil . (:maxlevel . 3)))))))

;;; org-scribe--refile-maybe-setup

(ert-deftest test-refile-maybe-setup-enables-in-known-project ()
  "Test that the hook function enables when mode is on and project is known."
  (test-refile--with-temp-project '(("novel.org" . "#+TITLE: Test\n"))
    (let ((org-scribe-mode t)
          (org-scribe-refile-project-wide t))
      (org-scribe--refile-maybe-setup)
      (should (org-scribe--refile-enabled-p)))))

(ert-deftest test-refile-maybe-setup-leaves-unknown-project-alone ()
  "Test that the hook function does not apply outside a recognized project."
  (test-refile--with-temp-project '("some-other-file.txt")
    (let ((org-scribe-mode t)
          (org-scribe-refile-project-wide t))
      (org-scribe--refile-maybe-setup)
      (should-not (org-scribe--refile-enabled-p)))))

(ert-deftest test-refile-maybe-setup-respects-disabled-customization ()
  "Test that `org-scribe-refile-project-wide' nil disables the feature."
  (test-refile--with-temp-project '(("novel.org" . "#+TITLE: Test\n"))
    (let ((org-scribe-mode t)
          (org-scribe-refile-project-wide nil))
      (org-scribe--refile-maybe-setup)
      (should-not (org-scribe--refile-enabled-p)))))

(ert-deftest test-refile-maybe-setup-tears-down-when-mode-disabled ()
  "Test that turning `org-scribe-mode' off tears down a prior override."
  (test-refile--with-temp-project '(("novel.org" . "#+TITLE: Test\n"))
    (let ((org-scribe-refile-project-wide t))
      (let ((org-scribe-mode t))
        (org-scribe--refile-maybe-setup)
        (should (org-scribe--refile-enabled-p)))
      (let ((org-scribe-mode nil))
        (org-scribe--refile-maybe-setup)
        (should-not (org-scribe--refile-enabled-p))))))

;;; Run tests

(defun org-scribe-refile-run-tests ()
  "Run all refile-target tests."
  (interactive)
  (ert "^test-refile-"))

(provide 'test-refile)

;;; test-refile.el ends here
