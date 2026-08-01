;;; test-tempel-snippets.el --- Tests for the bundled Tempel snippets -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for snippets/org-scribe-tempel.eld, the optional Tempel snippet
;; file that inserts inline edit markers.
;;
;; These tests read the file as lisp-data and assert its shape.  They do
;; not require Tempel to be installed: the point is that the shipped file
;; stays valid and stays consistent with the parser in
;; search/org-scribe-search.el, which is what actually reads the markers
;; the snippets produce.

;;; Code:

(require 'ert)

(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../search" default-directory))
  (add-to-list 'load-path (expand-file-name ".." default-directory)))

(require 'org-scribe-config)
;; Also provides `org-scribe-tempel-setup' / `-snippets-file': the
;; snippets live beside the parser that reads what they produce.
(require 'org-scribe-search)

;; Tempel is optional, so `tempel-path' is not special unless Tempel is
;; loaded.  Declaring it here makes the `let' bindings below dynamic, so
;; `org-scribe-tempel-setup' actually sees them.
(defvar tempel-path)

(defvar org-scribe-test--tempel-dir
  ;; Captured at load time: `load-file-name' is nil once ERT runs the
  ;; tests themselves.
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding this test file.")

(defun org-scribe-test--tempel-file ()
  "Return the path to the bundled snippet file."
  (expand-file-name "../snippets/org-scribe-tempel.eld"
                    org-scribe-test--tempel-dir))

(defun org-scribe-test--tempel-forms ()
  "Return every top-level form in the bundled snippet file."
  (with-temp-buffer
    (insert-file-contents (org-scribe-test--tempel-file))
    (goto-char (point-min))
    (let ((forms nil))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(ert-deftest test-tempel-snippets-file-exists ()
  "The bundled snippet file ships with the package."
  (should (file-exists-p (org-scribe-test--tempel-file)))
  (should (equal (org-scribe-test--tempel-file)
                 (org-scribe-tempel-snippets-file))))

(ert-deftest test-tempel-snippets-file-is-valid-lisp-data ()
  "The snippet file parses as lisp-data.
Tempel reads it with `read', so a syntax error would break every
snippet in the file, not just the malformed one."
  (should (org-scribe-test--tempel-forms)))

(ert-deftest test-tempel-snippets-declare-org-mode ()
  "The snippets are scoped to `org-mode'.
Tempel applies templates by the mode symbol preceding them in the file."
  (should (memq 'org-mode (org-scribe-test--tempel-forms))))

(ert-deftest test-tempel-snippets-define-edit-and-note ()
  "The file defines the `edit' and `note' snippets."
  (let ((names (mapcar #'car (seq-filter #'consp (org-scribe-test--tempel-forms)))))
    (should (memq 'edit names))
    (should (memq 'note names))))

(ert-deftest test-tempel-snippets-omit-summary ()
  "The file must not define a `summary' snippet.
*SUMMARY* describes prose that does not exist yet: it belongs under a
TODO heading and is found with the TODO search, deliberately outside the
edit index.  Shipping a snippet for it would blur that distinction — see
`test-search-edits-index-ignores-summary-marker'."
  (let ((names (mapcar #'car (seq-filter #'consp (org-scribe-test--tempel-forms)))))
    (should-not (memq 'summary names))))

(ert-deftest test-tempel-snippets-wrap-markers-in-comment-block ()
  "Both snippets wrap their marker in a comment block.
This is the whole point of the convention: it is what keeps markers out
of exports and out of word counts."
  (dolist (name '(edit note))
    (let* ((form (seq-find (lambda (f) (and (consp f) (eq name (car f))))
                           (org-scribe-test--tempel-forms)))
           (strings (seq-filter #'stringp form)))
      (should (member "#+begin_comment" strings))
      (should (member "#+end_comment" strings)))))

(ert-deftest test-tempel-snippets-edit-reads-category-variable ()
  "The `edit' snippet prompts from `org-scribe-edit-categories'.
Reading the variable rather than hardcoding the list is what keeps the
prompt and the index grouping from drifting apart."
  (let ((form (seq-find (lambda (f) (and (consp f) (eq 'edit (car f))))
                        (org-scribe-test--tempel-forms))))
    (should (string-match-p "org-scribe-edit-categories" (format "%S" form)))))

(ert-deftest test-tempel-snippets-produce-parseable-markers ()
  "Markers shaped like the snippets' output parse as the index expects.
Guards the seam between the two files: the snippet emits the text and
`org-scribe--edits-parse-block' reads it back, and nothing else checks
that they agree on the grammar."
  ;; What the `edit' snippet produces once its prompts are filled in.
  (let ((markers (org-scribe--edits-parse-block "*EDIT*: plot - the body")))
    (should (equal 1 (length markers)))
    (should (equal "EDIT" (plist-get (car markers) :type)))
    (should (equal "plot" (plist-get (car markers) :category)))
    (should (equal "the body" (plist-get (car markers) :text))))
  ;; What the `note' snippet produces.
  (let ((markers (org-scribe--edits-parse-block "*NOTE*: the body")))
    (should (equal 1 (length markers)))
    (should (equal "NOTE" (plist-get (car markers) :type)))
    (should (null (plist-get (car markers) :category)))
    (should (equal "the body" (plist-get (car markers) :text)))))

(ert-deftest test-tempel-snippets-every-category-round-trips ()
  "Every default category is recognised by the parser.
If a category were added to `org-scribe-edit-categories' but the parser
disagreed about the separator, markers would silently land in `other'."
  (dolist (category org-scribe-edit-categories)
    (let ((markers (org-scribe--edits-parse-block
                    (format "*EDIT*: %s - body text" category))))
      (should (equal category (plist-get (car markers) :category))))))

(ert-deftest test-tempel-setup-errors-without-tempel ()
  "`org-scribe-tempel-setup' fails clearly when Tempel is absent."
  (skip-unless (not (boundp 'tempel-path)))
  (should-error (org-scribe-tempel-setup) :type 'user-error))

;;; org-scribe-tempel-setup: `tempel-path' normalization
;;
;; `tempel-path' accepts a single path string, a list, or a glob, and its
;; *default* is a single string.  `add-to-list' signals wrong-type-argument
;; on a string, so setup has to normalize before adding — this is a
;; regression test for that, found by running against real Tempel.

(ert-deftest test-tempel-setup-handles-string-path ()
  "A string `tempel-path' is normalized to a list, keeping the old value."
  (let ((tempel-path "~/.emacs.d/templates"))
    (org-scribe-tempel-setup)
    (should (listp tempel-path))
    (should (member (org-scribe-tempel-snippets-file) tempel-path))
    (should (member "~/.emacs.d/templates" tempel-path))))

(ert-deftest test-tempel-setup-handles-list-path ()
  "A list `tempel-path' gains the snippet file without losing entries."
  (let ((tempel-path (list "~/a.eld" "~/b.eld")))
    (org-scribe-tempel-setup)
    (should (member (org-scribe-tempel-snippets-file) tempel-path))
    (should (member "~/a.eld" tempel-path))
    (should (member "~/b.eld" tempel-path))))

(ert-deftest test-tempel-setup-is-idempotent ()
  "Calling setup twice does not add the snippet file twice."
  (let ((tempel-path nil))
    (org-scribe-tempel-setup)
    (org-scribe-tempel-setup)
    (should (equal 1 (seq-count (lambda (p)
                                  (equal p (org-scribe-tempel-snippets-file)))
                                tempel-path)))))

(provide 'test-tempel-snippets)

;;; test-tempel-snippets.el ends here
