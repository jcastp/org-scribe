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

(ert-deftest test-method-table-has-four-entries ()
  "The method table carries exactly the four known methods."
  (should (equal (sort (mapcar #'car org-scribe--methods) #'string-lessp)
                 '(helice matriz simple sistema))))

(ert-deftest test-method-simple-replaces-the-whole-notebook ()
  "`simple' names a notebook directory, not a Foundations overlay."
  (let ((entry (alist-get 'simple org-scribe--methods)))
    (should (null (plist-get entry :overlay)))
    (should (equal "simple" (plist-get entry :notebook)))))

(ert-deftest test-method-offered-excludes-hidden-and-unknown ()
  "Only registered, non-hidden methods are offered."
  (should (org-scribe--method-offered-p 'sistema))
  (should (org-scribe--method-offered-p 'helice))
  (should-not (org-scribe--method-offered-p 'nonsense))
  (let ((org-scribe--methods
         '((sistema . (:overlay nil)) (simple . (:overlay nil :hidden t)))))
    (should-not (org-scribe--method-offered-p 'simple)))
  (let ((org-scribe--methods '((simple . (:overlay nil :notebook "simple")))))
    (should (org-scribe--method-offered-p 'simple))))

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

(ert-deftest test-method-every-entry-has-a-label-in-every-registered-pack ()
  "Every method symbol has a prompt label in every registered language pack.
Labels moved from `org-scribe--methods' own `:label-en'/`:label-es'
plist keys to each pack's `:method-labels' section (see
\"i18n-extended.org\", Step 7); this is the pack-driven equivalent of
the old direct presence check."
  (dolist (lang (org-scribe-languages))
    (let ((labels (plist-get (org-scribe-lang-pack lang) :method-labels)))
      (dolist (entry org-scribe--methods)
        (should (stringp (alist-get (car entry) labels)))))))

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

(ert-deftest test-method-reads-recorded-simple ()
  "An explicit `# Method: simple' line resolves to `simple, not `sistema.
Registering it in the table is what stops it falling back."
  (test-method--with-project root
    (org-scribe--project-marker-set root "Method" "simple")
    (should (eq 'simple (org-scribe-project-method root)))))

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

(ert-deftest test-method-resolves-sistema-when-no-project-and-no-root-arg ()
  "Outside any project, with no ROOT argument either, this must not fall
through to reading \".org-scribe-project\" against `default-directory' --
which could pick up an unrelated marker file that happens to sit there --
and must resolve to `sistema without consulting the filesystem at all.
Pins the specific gap: a stray marker file in `default-directory', with
a Method line naming something other than `sistema, must not leak in.

The marker file is written directly with `with-temp-file', not via
`org-scribe--project-marker-set' (which does nothing when the file does
not already exist) -- otherwise this would trivially pass for the wrong
reason, with no marker file for `default-directory' to leak in the
first place."
  (let ((stray-dir (make-temp-file "test-method-stray-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".org-scribe-project" stray-dir)
            (insert "# Writing project: Stray\n# Method: helice\n"))
          (should (equal "helice" (org-scribe--project-marker-get stray-dir "Method")))
          (let ((default-directory (file-name-as-directory stray-dir)))
            (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () nil)))
              (should (eq 'sistema (org-scribe-project-method))))))
      (delete-directory stray-dir t))))

(ert-deftest test-method-does-not-intern-a-symbol-for-an-unknown-value ()
  "A typo or garbage \"# Method:\" value must not permanently intern a new
symbol into the obarray -- it should be checked with `intern-soft', which
finds any of the three known method symbols (already interned from
`org-scribe--methods' own definition) exactly as `intern' would, but
returns nil instead of interning anything for a value that matches none
of them."
  (test-method--with-project root
    (let ((garbage-name (format "org-scribe-test-method-garbage-%d" (random 1000000))))
      (should-not (intern-soft garbage-name))
      (org-scribe--project-marker-set root "Method" garbage-name)
      (should (eq 'sistema (org-scribe-project-method root)))
      (should-not (intern-soft garbage-name)))))

(ert-deftest test-method-creation-refuses-a-hidden-method ()
  "While `simple' is hidden, creating a project with it signals an error.
Otherwise it would silently deploy the Sistema notebook."
  (require 'org-scribe-project)
  (should-error (org-scribe--require-method-available 'simple) :type 'user-error)
  (should-not (org-scribe--require-method-available nil))
  (should-not (org-scribe--require-method-available 'helice)))

(ert-deftest test-method-short-story-prompt-is-skipped-while-simple-is-hidden ()
  "With `simple' hidden, nothing is asked and a short story is unchanged."
  (require 'org-scribe-project)
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _) (error "Must not prompt"))))
    (should (null (org-scribe--read-short-story-method 'en)))))

(ert-deftest test-method-short-story-prompt-offers-standard-and-simple ()
  "Once `simple' is offered, the prompt maps Standard to nil and Simple to it."
  (require 'org-scribe-project)
  (let ((org-scribe--methods '((sistema . (:overlay nil))
                               (simple . (:overlay nil :notebook "simple")))))
    (dolist (lang '(en es))
      (let* ((labels (plist-get (org-scribe-lang-pack lang) :method-labels))
             (simple (alist-get 'simple labels))
             asked)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p coll &rest _) (setq asked coll) simple)))
          (should (eq 'simple (org-scribe--read-short-story-method lang)))
          (should (= 2 (length asked))))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p _c _a _r _i _h def) def)))
          (should (null (org-scribe--read-short-story-method lang))))))))

(ert-deftest test-method-novel-prompt-omits-hidden-methods ()
  "The novel prompt lists the shipped methods and not a hidden one."
  (require 'org-scribe-project)
  (let (asked)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p coll &rest _) (setq asked coll) (car coll))))
      (org-scribe--read-method 'en))
    (should (= 3 (length asked)))
    (should-not (cl-find-if (lambda (l) (string-prefix-p "Simple" l)) asked))))

(ert-deftest test-method-short-story-marker-written-only-for-a-method ()
  "A short story created without METHOD has no `# Method:' line."
  (require 'org-scribe-project)
  (let* ((base (make-temp-file "test-method-ss-" t))
         (dir (expand-file-name "Tale" base)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'find-file) #'ignore)
                    ((symbol-function 'project-remember-project) #'ignore)
                    ((symbol-function 'org-scribe--git) (lambda (&rest _) t)))
            (org-scribe-create-short-story-project base "Tale" 'en 1000))
          (should (null (org-scribe--project-marker-get dir "Method")))
          (should (eq 'sistema (org-scribe-project-method dir))))
      (delete-directory base t))))

(provide 'test-method)

;;; test-method.el ends here
