;;; test-core-extended.el --- Extended tests for org-scribe-core -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Extended tests for core utility functions in org-scribe-core.el.
;; Covers project type detection, project structure detection,
;; helper utilities, and error handling macros.
;;
;; These complement the basic tests in org-scribe-test.el, focusing
;; on the project type detection strategies and structure discovery.

;;; Code:

(require 'ert)
(require 'org)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory)))

(require 'org-scribe-core)
(require 'org-scribe-messages)

;;; Helper: Clear the project type cache

(defmacro test-core--with-temp-project (dir-setup &rest body)
  "Execute BODY inside a temp directory configured by DIR-SETUP.
DIR-SETUP is a list of file/dir relative paths to create:
  - Strings ending in '/' create directories
  - Pairs (PATH . CONTENT) create files with content
  - Other strings create empty files
Clears the project type cache before and after."
  (declare (indent 1))
  `(let* ((temp-dir (make-temp-file "org-scribe-core-test-" t))
          (default-directory temp-dir))
     (unwind-protect
         (progn
           ;; Clear cache
           (setq org-scribe--project-type-cache nil)
           ;; Create files/dirs from setup
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
           ,@body)
       ;; Cleanup
       (setq org-scribe--project-type-cache nil)
       (delete-directory temp-dir t))))

;;; Module Loading Tests

(ert-deftest test-core-module-loads ()
  "Test that org-scribe-core module loads without errors."
  (should (featurep 'org-scribe-core)))

;;; Function Availability Tests

(ert-deftest test-core-functions-defined ()
  "Test that all core functions are defined."
  (should (fboundp 'org-scribe-project-root))
  (should (fboundp 'org-scribe-project-type))
  (should (fboundp 'org-scribe-project-structure))
  (should (fboundp 'org-scribe-check-feature))
  (should (fboundp 'org-scribe-window-perc))
  (should (fboundp 'org-scribe-sanitize-filename))
  (should (fboundp 'org-scribe-validate-directory))
  (should (fboundp 'org-scribe--find-existing-file))
  (should (fboundp 'org-scribe--find-existing-dir)))

;;; org-scribe-project-type Tests

(ert-deftest test-core-project-type-marker-file-novel ()
  "Test project type detection via marker file for novel."
  (test-core--with-temp-project
      '((".org-scribe-project" . "# Type: novel\n# Title: Test Novel\n"))
    (should (eq 'novel (org-scribe-project-type)))))

(ert-deftest test-core-project-type-marker-file-short-story ()
  "Test project type detection via marker file for short story."
  (test-core--with-temp-project
      '((".org-scribe-project" . "# Type: short-story\n# Title: Test Story\n"))
    (should (eq 'short-story (org-scribe-project-type)))))

(ert-deftest test-core-project-type-objects-dir ()
  "Test project type detection via objects/ directory."
  (test-core--with-temp-project
      '("objects/")
    (should (eq 'novel (org-scribe-project-type)))))

(ert-deftest test-core-project-type-story-org ()
  "Test project type detection via story.org file."
  (test-core--with-temp-project
      '("story.org")
    (should (eq 'short-story (org-scribe-project-type)))))

(ert-deftest test-core-project-type-cuento-org ()
  "Test project type detection via cuento.org (Spanish short story)."
  (test-core--with-temp-project
      '("cuento.org")
    (should (eq 'short-story (org-scribe-project-type)))))

(ert-deftest test-core-project-type-novel-org ()
  "Test project type detection via novel.org file."
  (test-core--with-temp-project
      '("novel.org")
    (should (eq 'novel (org-scribe-project-type)))))

(ert-deftest test-core-project-type-novela-org ()
  "Test project type detection via novela.org (Spanish novel)."
  (test-core--with-temp-project
      '("novela.org")
    (should (eq 'novel (org-scribe-project-type)))))

(ert-deftest test-core-project-type-unknown ()
  "Test project type returns 'unknown when no indicators present."
  (test-core--with-temp-project
      '("some-other-file.txt")
    (should (eq 'unknown (org-scribe-project-type)))))

(ert-deftest test-core-project-type-marker-takes-priority ()
  "Test that marker file type takes priority over directory heuristics."
  ;; Marker says short-story but objects/ dir is also present
  (test-core--with-temp-project
      '((".org-scribe-project" . "# Type: short-story\n")
        "objects/")
    (should (eq 'short-story (org-scribe-project-type)))))

(ert-deftest test-core-project-type-cached ()
  "Test that project type is cached after first detection."
  (test-core--with-temp-project
      '("novel.org")
    (let ((type1 (org-scribe-project-type))
          (type2 (org-scribe-project-type)))
      (should (eq type1 type2))
      ;; Cache should have an entry, keyed by the normalized root so that
      ;; different spellings of the same directory share one entry.
      (should (assoc (org-scribe--normalize-project-root default-directory)
                      org-scribe--project-type-cache
                      #'string=)))))

(ert-deftest test-core-project-type-cache-normalizes-trailing-slash ()
  "Test that a trailing-slash spelling of a root reuses the same cache entry."
  (test-core--with-temp-project
      '("novel.org")
    (org-scribe-project-type)
    (let ((without-slash (directory-file-name default-directory)))
      ;; Both spellings should resolve to the same normalized cache key.
      (should (string= (org-scribe--normalize-project-root default-directory)
                        (org-scribe--normalize-project-root without-slash))))))

(ert-deftest test-core-project-type-cache-clear-invalidates-root ()
  "Test that `org-scribe-project-type-cache-clear' drops the entry for ROOT."
  (test-core--with-temp-project
      '("novel.org")
    (org-scribe-project-type)
    (should (assoc (org-scribe--normalize-project-root default-directory)
                    org-scribe--project-type-cache #'string=))
    (org-scribe-project-type-cache-clear default-directory)
    (should-not (assoc (org-scribe--normalize-project-root default-directory)
                        org-scribe--project-type-cache #'string=))))

(ert-deftest test-core-project-type-cache-clear-all ()
  "Test that `org-scribe-project-type-cache-clear' with no argument wipes the cache."
  (test-core--with-temp-project
      '("novel.org")
    (org-scribe-project-type)
    (should org-scribe--project-type-cache)
    (org-scribe-project-type-cache-clear)
    (should-not org-scribe--project-type-cache)))

;;; org-scribe-project-structure Tests

(ert-deftest test-core-project-structure-returns-plist ()
  "Test that project structure returns a plist."
  (test-core--with-temp-project
      '()
    (let ((structure (org-scribe-project-structure)))
      (should (listp structure))
      (should (plist-get structure :root)))))

(ert-deftest test-core-project-structure-novel-files ()
  "Test project structure detects novel files correctly."
  (test-core--with-temp-project
      '(("novel.org" . "#+TITLE: Test\n")
        ("objects/characters.org" . "#+TITLE: Characters\n")
        ("objects/locations.org" . "#+TITLE: Locations\n")
        ("objects/plot.org" . "#+TITLE: Plot\n"))
    (let ((structure (org-scribe-project-structure)))
      (should (string-suffix-p "novel.org"
                               (plist-get structure :novel-file)))
      (should (string-suffix-p "objects/characters.org"
                               (plist-get structure :characters-file)))
      (should (string-suffix-p "objects/locations.org"
                               (plist-get structure :locations-file)))
      (should (string-suffix-p "objects/plot.org"
                               (plist-get structure :plot-file))))))

(ert-deftest test-core-project-structure-nil-for-missing ()
  "Test project structure returns nil for missing files."
  (test-core--with-temp-project
      '()
    (let ((structure (org-scribe-project-structure)))
      (should (null (plist-get structure :novel-file)))
      (should (null (plist-get structure :characters-file)))
      (should (null (plist-get structure :locations-file))))))

(ert-deftest test-core-project-structure-spanish-files ()
  "Test project structure detects Spanish file names."
  (test-core--with-temp-project
      '(("novela.org" . "#+TITLE: Novela\n")
        ("objects/personajes.org" . "#+TITLE: Personajes\n")
        ("objects/localizaciones.org" . "#+TITLE: Localizaciones\n")
        ("objects/trama.org" . "#+TITLE: Trama\n"))
    (let ((structure (org-scribe-project-structure)))
      (should (string-suffix-p "novela.org"
                               (plist-get structure :novel-file)))
      (should (string-suffix-p "objects/personajes.org"
                               (plist-get structure :characters-file)))
      (should (string-suffix-p "objects/localizaciones.org"
                               (plist-get structure :locations-file))))))

(ert-deftest test-core-project-structure-plan-file-present ()
  "Test that :plan-file is non-nil when plan.org exists."
  (test-core--with-temp-project
      '(("plan.org" . "# Writing plan\n"))
    (let ((structure (org-scribe-project-structure)))
      (should (plist-get structure :plan-file))
      (should (string-suffix-p "plan.org" (plist-get structure :plan-file))))))

(ert-deftest test-core-project-structure-plan-file-absent ()
  "Test that :plan-file is nil when plan.org does not exist."
  (test-core--with-temp-project
      '(("novel.org" . "#+TITLE: Test\n"))
    (let ((structure (org-scribe-project-structure)))
      (should (null (plist-get structure :plan-file))))))

;;; org-scribe--find-existing-file Tests

(ert-deftest test-core-find-existing-file-first ()
  "Test finding first existing file from alternatives."
  (test-core--with-temp-project
      '(("novel.org" . ""))
    (should (string-suffix-p
             "novel.org"
             (org-scribe--find-existing-file temp-dir "novel.org" "novela.org")))))

(ert-deftest test-core-find-existing-file-second ()
  "Test finding second file when first doesn't exist."
  (test-core--with-temp-project
      '(("novela.org" . ""))
    (should (string-suffix-p
             "novela.org"
             (org-scribe--find-existing-file temp-dir "novel.org" "novela.org")))))

(ert-deftest test-core-find-existing-file-none ()
  "Test returns nil when no files exist."
  (test-core--with-temp-project
      '()
    (should (null (org-scribe--find-existing-file
                   temp-dir "novel.org" "novela.org")))))

;;; org-scribe--find-existing-dir Tests

(ert-deftest test-core-find-existing-dir-first ()
  "Test finding first existing directory from alternatives."
  (test-core--with-temp-project
      '("notes/")
    (should (string-suffix-p
             "notes"
             (string-trim-right
              (org-scribe--find-existing-dir temp-dir "notes" "notas")
              "/")))))

(ert-deftest test-core-find-existing-dir-second ()
  "Test finding second directory when first doesn't exist."
  (test-core--with-temp-project
      '("notas/")
    (should (org-scribe--find-existing-dir temp-dir "notes" "notas"))))

(ert-deftest test-core-find-existing-dir-none ()
  "Test returns nil when no directories exist."
  (test-core--with-temp-project
      '()
    (should (null (org-scribe--find-existing-dir
                   temp-dir "notes" "notas")))))

;;; org-scribe-validate-directory Tests

(ert-deftest test-core-validate-directory-existing ()
  "Test validate-directory returns t for existing directory."
  (test-core--with-temp-project
      '("notes/")
    (should (eq t (org-scribe-validate-directory
                   (expand-file-name "notes" temp-dir))))))

(ert-deftest test-core-validate-directory-nonexistent ()
  "Test validate-directory returns nil for non-existent directory."
  (test-core--with-temp-project
      '()
    (should (null (org-scribe-validate-directory
                   (expand-file-name "nonexistent" temp-dir))))))

;;; org-scribe-with-error-handling Tests

(ert-deftest test-core-error-handling-success ()
  "Test that error handling wrapper returns body result on success."
  (let ((result (org-scribe-with-error-handling "test"
                  (+ 1 2))))
    (should (= 3 result))))

(ert-deftest test-core-error-handling-catches-error ()
  "Test that error handling wrapper catches errors and returns nil."
  (let ((result (org-scribe-with-error-handling "test"
                  (error "Intentional test error"))))
    (should (null result))))

;;; org-scribe-check-feature Tests

(ert-deftest test-core-check-feature-available ()
  "Test feature detection for available features."
  (should (eq t (org-scribe-check-feature 'org)))
  (should (eq t (org-scribe-check-feature 'cl-lib))))

(ert-deftest test-core-check-feature-unavailable ()
  "Test feature detection for unavailable features."
  (should (eq nil (org-scribe-check-feature 'this-package-does-not-exist))))

;;; org-scribe--split-property-list Tests (L4)

(ert-deftest test-core-split-property-list-plain ()
  "Test splitting a plain-text comma list."
  (should (equal (org-scribe--split-property-list "Alex") '("Alex")))
  (should (equal (org-scribe--split-property-list "Alex, Sam") '("Alex" "Sam"))))

(ert-deftest test-core-split-property-list-links ()
  "Test splitting a list of ID links."
  (should (equal (org-scribe--split-property-list "[[id:1][Alex]], [[id:2][Sam]]")
                 '("[[id:1][Alex]]" "[[id:2][Sam]]"))))

(ert-deftest test-core-split-property-list-comma-in-link-display-text ()
  "A comma inside a link's display text is not a separator (L4)."
  (should (equal (org-scribe--split-property-list "[[id:1][Smith, John]]")
                 '("[[id:1][Smith, John]]")))
  (should (equal (org-scribe--split-property-list "[[id:1][Smith, John]], [[id:2][Sam]]")
                 '("[[id:1][Smith, John]]" "[[id:2][Sam]]"))))

(ert-deftest test-core-split-property-list-trims-and-omits-empty ()
  "Test that items are trimmed and empty items are dropped."
  (should (equal (org-scribe--split-property-list " Alex ,  Sam  ") '("Alex" "Sam")))
  (should (equal (org-scribe--split-property-list "Alex,,Sam") '("Alex" "Sam"))))

;;; org-scribe--split-comma-list-protecting-names Tests (L4)

(ert-deftest test-core-split-protecting-names-no-known-comma-names ()
  "Without any comma-bearing known names, behaves like a plain comma split."
  (should (equal (org-scribe--split-comma-list-protecting-names
                  "Alex, Sam" '("Alex" "Sam"))
                 '("Alex" "Sam"))))

(ert-deftest test-core-split-protecting-names-protects-known-comma-name ()
  "A known entity name containing a comma is kept as one item (L4)."
  (should (equal (org-scribe--split-comma-list-protecting-names
                  "Smith, John" '("Smith, John"))
                 '("Smith, John")))
  (should (equal (org-scribe--split-comma-list-protecting-names
                  "Smith, John, Sam" '("Smith, John" "Sam"))
                 '("Smith, John" "Sam"))))

(ert-deftest test-core-split-protecting-names-unknown-comma-name-still-splits ()
  "A comma-bearing name not present in KNOWN-NAMES is still split naively."
  (should (equal (org-scribe--split-comma-list-protecting-names
                  "Smith, John" '("Someone Else"))
                 '("Smith" "John"))))

;;; org-scribe--escape-table-cell Tests (L8)

(ert-deftest test-core-escape-table-cell-escapes-pipe ()
  "A literal pipe is rewritten to the org literal-pipe escape (L8)."
  (should (equal (org-scribe--escape-table-cell "Bob | Alice") "Bob \\vert Alice"))
  (should (equal (org-scribe--escape-table-cell "a|b|c") "a\\vertb\\vertc")))

(ert-deftest test-core-escape-table-cell-flattens-newlines ()
  "A newline (illegal inside a table cell) is flattened to a space."
  (should (equal (org-scribe--escape-table-cell "line1\nline2") "line1 line2")))

(ert-deftest test-core-escape-table-cell-passes-through-plain-text ()
  "Text without pipes or newlines is returned unchanged."
  (should (equal (org-scribe--escape-table-cell "Alex") "Alex")))

;;; Run tests

(defun org-scribe-core-extended-run-tests ()
  "Run all extended core tests."
  (interactive)
  (ert "^test-core-"))

(provide 'test-core-extended)

;;; test-core-extended.el ends here
