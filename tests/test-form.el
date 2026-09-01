;;; test-form.el --- Tests for the per-project narrative form -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; The narrative form (flash / cuento / novelette / novella / novel) is
;; recorded in `.org-scribe-project' as a "# Form:" line and read back by
;; `org-scribe-project-form'.  It answers a different question from
;; `Type': Type says how the project is laid out on disk (objects/ vs a
;; consolidated notes.org) and every file resolver branches on it; Form
;; says which method the project is written under.  They do not line up
;; one-to-one — a novella is a `novel' project written to reduced breadth,
;; and flash/cuento/novelette are all `short-story' projects.
;;
;; The load-bearing property tested here is that *nil means legacy*.
;; Projects created before forms existed have no such line, and the whole
;; feature is built so that they behave exactly as they did — the same
;; convention `org-scribe-planner-gate' uses for an undecided gate.  A
;; form guessed from a word count would silently change what a report
;; says about work already in progress, which is why nothing infers one.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path (expand-file-name "core" parent-dir))
  (add-to-list 'load-path (expand-file-name "templates" parent-dir)))

(require 'org-scribe-core)

;;; Fixture

(defmacro test-form--with-project (root-var marker-lines &rest body)
  "Bind ROOT-VAR to a temp project whose marker file holds MARKER-LINES."
  (declare (indent 2))
  `(let ((,root-var (make-temp-file "test-form-" t)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".org-scribe-project" ,root-var)
             (insert "# Writing project: Form Test\n" ,marker-lines))
           ,@body)
       (delete-directory ,root-var t))))

;;; Reading the marker

(ert-deftest test-form-reads-recorded-form ()
  "A recorded form comes back as its symbol."
  (test-form--with-project root "# Type: short-story\n# Form: cuento\n"
    (should (eq 'cuento (org-scribe-project-form root)))))

(ert-deftest test-form-absent-line-is-nil ()
  "A project with no Form line reports nil, not a guessed default.
nil is the legacy signal every caller keys off; defaulting it here would
silently enrol projects created before forms existed."
  (test-form--with-project root "# Type: short-story\n"
    (should (null (org-scribe-project-form root)))))

(ert-deftest test-form-unknown-value-is-nil ()
  "A Form line naming something that is not a form reports nil.
A typo (\"# Form: cuentos\") must degrade to legacy behavior rather than
producing a form symbol nothing in the package knows how to handle."
  (test-form--with-project root "# Type: short-story\n# Form: cuentos\n"
    (should (null (org-scribe-project-form root)))))

(ert-deftest test-form-value-is-case-and-space-insensitive ()
  "Forms are read case-insensitively and trimmed.
The line is hand-editable — the templates tell writers to change it there
— so \"# Form: Cuento \" has to work."
  (test-form--with-project root "# Type: short-story\n# Form: Cuento \n"
    (should (eq 'cuento (org-scribe-project-form root)))))

(ert-deftest test-form-round-trips-through-marker-set ()
  "The generic marker accessors write a form the reader accepts."
  (test-form--with-project root "# Type: novel\n"
    (should (null (org-scribe-project-form root)))
    (org-scribe--project-marker-set root "Form" "novella")
    (should (eq 'novella (org-scribe-project-form root)))))

;;; Short fiction predicate

(ert-deftest test-form-short-fiction-covers-the-seed-forms ()
  "Flash, cuento and novelette are short fiction; novella and novel are not.
The line is drawn where the method changes, not where the word count
does: a novella is written with the full thirteen plot points, so it
belongs with the novel however short it is."
  (dolist (form '(flash cuento novelette))
    (test-form--with-project root (format "# Type: short-story\n# Form: %s\n" form)
      (should (org-scribe-project-short-fiction-p root))))
  (dolist (form '(novella novel))
    (test-form--with-project root (format "# Form: %s\n" form)
      (should-not (org-scribe-project-short-fiction-p root)))))

(ert-deftest test-form-short-fiction-false-for-legacy-project ()
  "A project with no form is not short fiction, so legacy behavior holds."
  (test-form--with-project root "# Type: short-story\n"
    (should-not (org-scribe-project-short-fiction-p root))))

;;; The form table

(ert-deftest test-form-table-covers-both-project-types ()
  "Every form maps to a real structural project type, and both are covered."
  (dolist (form (mapcar #'car org-scribe--forms))
    (should (memq (org-scribe-form-project-type form) '(novel short-story))))
  (should (equal '(flash cuento novelette)
                 (org-scribe-forms-for-project-type 'short-story)))
  (should (equal '(novella novel)
                 (org-scribe-forms-for-project-type 'novel))))

(ert-deftest test-form-default-targets-ascend-with-the-bands ()
  "Default word targets rise with the forms and sit inside their bands.
The numbers are what a new project starts with, so a target above its own
band's ceiling would hand every writer of that form an impossible goal on
day one."
  (let ((targets (mapcar (lambda (form)
                           (org-scribe-form-default-target (car form)))
                         org-scribe--forms)))
    (should (equal targets (sort (copy-sequence targets) #'<))))
  (dolist (entry org-scribe--forms)
    (let ((max (plist-get (cdr entry) :max-words))
          (target (plist-get (cdr entry) :default-target)))
      (should target)
      (when max
        (should (<= target max))))))

(ert-deftest test-form-default-target-nil-for-unknown-form ()
  "An unknown form has no default target, rather than a misleading one."
  (should (null (org-scribe-form-default-target 'epic))))

(provide 'test-form)
;;; test-form.el ends here
