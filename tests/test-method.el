;;; test-method.el --- Tests for the per-project plotting-method marker -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for `org-scribe-project-method' and `org-scribe--methods'
;; (core/org-scribe-core.el).  Modelled on tests/test-planner-gate.el,
;; whose marker-file accessors this feature reuses.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add module directories to load path
(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path (expand-file-name "core" parent-dir)))

(require 'org-scribe-core)

;;; Fixture

(defmacro test-method--with-project (root-var &rest body)
  "Bind ROOT-VAR to a fresh temp project dir with a marker file, run BODY."
  (declare (indent 1))
  `(let ((,root-var (make-temp-file "test-method-" t)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".org-scribe-project" ,root-var)
             (insert "# Writing project: Method Test Novel\n# Type: novel\n"))
           ,@body)
       (delete-directory ,root-var t))))

;;; org-scribe--methods table

(ert-deftest test-method-table-has-three-entries ()
  "The method table carries exactly the three known methods."
  (should (equal (sort (mapcar #'car org-scribe--methods) #'string-lessp)
                 '(helice matriz sistema))))

(ert-deftest test-method-sistema-has-no-overlay ()
  "Sistema ships no overlay directory: its design file never moves."
  (should (null (plist-get (alist-get 'sistema org-scribe--methods) :overlay))))

(ert-deftest test-method-helice-and-matriz-have-overlays ()
  "Hélice and Matriz each name a distinct overlay subdirectory."
  (let ((helice-overlay (plist-get (alist-get 'helice org-scribe--methods) :overlay))
        (matriz-overlay (plist-get (alist-get 'matriz org-scribe--methods) :overlay)))
    (should (stringp helice-overlay))
    (should (stringp matriz-overlay))
    (should-not (string= helice-overlay matriz-overlay))))

(ert-deftest test-method-every-entry-has-both-labels ()
  "Every method entry carries an English and a Spanish prompt label."
  (dolist (entry org-scribe--methods)
    (should (stringp (plist-get (cdr entry) :label-en)))
    (should (stringp (plist-get (cdr entry) :label-es)))))

;;; org-scribe-project-method

(ert-deftest test-method-returns-sistema-when-undecided ()
  "A project with no `# Method:' line resolves to `sistema, not nil.
This is the legacy-preservation rule: every project created before this
feature existed is a Sistema project, so there is no undecided state."
  (test-method--with-project root
    (should (eq 'sistema (org-scribe-project-method root)))))

(ert-deftest test-method-returns-sistema-without-marker-file ()
  "A directory with no marker file at all also resolves to `sistema."
  (let ((root (make-temp-file "test-method-nomarker-" t)))
    (unwind-protect
        (should (eq 'sistema (org-scribe-project-method root)))
      (delete-directory root t))))

(ert-deftest test-method-reads-recorded-helice ()
  "An explicit `# Method: helice' line resolves to `helice."
  (test-method--with-project root
    (org-scribe--project-marker-set root "Method" "helice")
    (should (eq 'helice (org-scribe-project-method root)))))

(ert-deftest test-method-reads-recorded-matriz ()
  "An explicit `# Method: matriz' line resolves to `matriz."
  (test-method--with-project root
    (org-scribe--project-marker-set root "Method" "matriz")
    (should (eq 'matriz (org-scribe-project-method root)))))

(ert-deftest test-method-reads-recorded-sistema-explicitly ()
  "An explicit `# Method: sistema' line resolves the same as no line at all."
  (test-method--with-project root
    (org-scribe--project-marker-set root "Method" "sistema")
    (should (eq 'sistema (org-scribe-project-method root)))))

(ert-deftest test-method-value-is-case-and-space-insensitive ()
  "The marker value is matched case- and whitespace-insensitively."
  (test-method--with-project root
    (org-scribe--project-marker-set root "Method" "  Helice ")
    (should (eq 'helice (org-scribe-project-method root)))))

(ert-deftest test-method-unknown-value-falls-back-to-sistema ()
  "A marker value that names no known method resolves to `sistema.
Never inferred, never nil: an unrecognized value is treated the same as
none at all, per the `Method is recorded, never inferred' rule."
  (test-method--with-project root
    (org-scribe--project-marker-set root "Method" "helix")
    (should (eq 'sistema (org-scribe-project-method root)))))

(ert-deftest test-method-round-trips-through-marker-set ()
  "Setting the marker and reading it back via `org-scribe-project-method'
agrees with a direct `org-scribe--project-marker-get' read."
  (test-method--with-project root
    (org-scribe--project-marker-set root "Method" "matriz")
    (should (string= "matriz" (org-scribe--project-marker-get root "Method")))
    (should (eq 'matriz (org-scribe-project-method root)))))

(ert-deftest test-method-uses-current-project-root-by-default ()
  "Method without a ROOT argument resolves via `org-scribe-project-root'."
  (test-method--with-project root
    (org-scribe--project-marker-set root "Method" "helice")
    (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () root)))
      (should (eq 'helice (org-scribe-project-method))))))

(ert-deftest test-method-marker-set-preserves-other-lines ()
  "Setting the Method marker does not disturb other marker-file lines."
  (test-method--with-project root
    (org-scribe--project-marker-set root "Method" "helice")
    (should (string= "Method Test Novel"
                     (org-scribe--project-marker-get root "Writing project")))
    (should (string= "novel" (org-scribe--project-marker-get root "Type")))))

(provide 'test-method)

;;; test-method.el ends here
