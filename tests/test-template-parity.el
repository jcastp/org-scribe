;;; test-template-parity.el --- Structural parity between template sets -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; `novel-en' and `novel-es' are two renderings of *one* writing method.  A
;; good deal of org-scribe depends on that: the scene property alias table
;; pairs each key English-first/Spanish-second, the entity heading predicates
;; carry bilingual regexps, and every file resolver in
;; `org-scribe-project-structure' is a bilingual pair.  Nothing, however,
;; checked that the two sets actually *stay* parallel.
;;
;; They had not.  Before the sistema migration
;; `novel-en/objects/characters.org.template' was 2066 lines against its
;; Spanish counterpart's 89; `plot.org' was 445 against 115; and the two
;; manuscripts had different heading trees entirely — Spanish carried four
;; progress-tracking sections English never had, going back to the commit that
;; first added the file.  None of it was caught, because nothing looked.
;;
;; This file looks.  For every pair it compares:
;;
;;   - the heading tree: how many headings, at what depths, in what order; and
;;   - the set of property keys used, canonicalized through the scene-property
;;     alias table so that localized spellings compare equal.
;;
;; Heading *text* and prose are deliberately not compared: those are supposed
;; to differ.  What must not differ is the shape.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)

(defvar org-scribe-parity--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "Repository root, used to locate the shipped template files.")

(let ((default-directory org-scribe-parity--root))
  (add-to-list 'load-path (expand-file-name "core" default-directory)))

(require 'org-scribe-core)

;;; The pairing

(defconst org-scribe-parity--pairs
  '(("README.org.template"                . "README.org.template")
    ("novela.org.template"                . "novel.org.template")
    ("diseno.org.template"                . "design.org.template")
    ("revision.org.template"              . "revision.org.template")
    ("diario-escritura.org.template"      . "writing-journal.org.template")
    ("objects/personajes.org.template"    . "objects/characters.org.template")
    ("objects/localizaciones.org.template". "objects/locations.org.template")
    ("objects/trama.org.template"         . "objects/plot.org.template")
    ("objects/worldbuilding.org.template" . "objects/worldbuilding.org.template")
    ("objects/objetos.org.template"       . "objects/objects.org.template")
    ("objects/cronologia.org.template"    . "objects/timeline.org.template")
    ("notas/notas.org.template"           . "notes/notes.org.template")
    ("notas/investigacion.org.template"   . "notes/research.org.template"))
  "Spanish template -> English template, relative to each set's directory.
File names differ between the sets, so the pairing cannot be derived and
has to be declared.  `test-template-parity-pairing-is-complete' checks
that this list accounts for every shipped template on both sides.")

(defconst org-scribe-parity--known-divergent
  '(("diario-escritura.org.template"
     . "Habits and the writing calendar live outside the method on purpose, so
these two were never ports of each other and the migration did not touch
them.  English is the far longer of the two.  Reconciling them is a
decision about what the journal is for, not a translation task.")
    ("objects/objetos.org.template"
     . "Objects have no counterpart in the method; both files are pre-sistema
stubs that were never parallel.  English carries Status and
First-appearance properties Spanish lacks.")
    ("objects/cronologia.org.template"
     . "Timeline likewise has no counterpart in the method; English carries an
extra ID and Type property."))
  "Pairs known to diverge, each with the reason it is tolerated.
Every entry is a debt, not a licence.  `test-template-parity-exceptions-are-live'
fails when an entry no longer diverges, so a pair that gets fixed cannot
leave a stale exemption behind that would hide the next regression.")

;;; Helpers

(defun org-scribe-parity--canonical-property (name)
  "Return the canonical key for property NAME, or a symbol for NAME itself.
Localized scene properties (`Brecha' / `Gap') canonicalize to the same
key so the two sets compare equal.  Matching is case-insensitive because
`org-entry-properties' upcases property names while
`org-scribe--scene-property-aliases' stores them capitalized — comparing
literally silently reports every scene property as divergent."
  (or (car (cl-find-if
            (lambda (row)
              (cl-member name (cdr row) :test #'cl-equalp))
            org-scribe--scene-property-aliases))
      (intern (downcase name))))

(defun org-scribe-parity--shape (file)
  "Return (LEVELS . PROPERTY-KEYS) describing the structure of FILE.
LEVELS is the ordered list of heading depths; PROPERTY-KEYS is the sorted
set of canonicalized property names.  Heading text and prose are ignored
on purpose — they are supposed to differ between the two sets."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let (levels props)
      (org-map-entries
       (lambda ()
         (push (org-current-level) levels)
         (dolist (kv (org-entry-properties nil 'standard))
           (push (org-scribe-parity--canonical-property (car kv)) props))))
      (cons (nreverse levels)
            (sort (delete-dups props)
                  (lambda (a b) (string< (symbol-name a) (symbol-name b))))))))

(defun org-scribe-parity--path (set relative)
  "Absolute path of RELATIVE inside template SET (\"novel-es\" or \"novel-en\")."
  (expand-file-name (concat "org-scribe-templates/" set "/" relative)
                    org-scribe-parity--root))

(defun org-scribe-parity--divergent-p (es-name)
  "Return non-nil if the pair keyed by ES-NAME is a known exception."
  (assoc es-name org-scribe-parity--known-divergent))

(defun org-scribe-parity--templates (set)
  "Return the shipped .template files of SET, relative to its directory."
  (let ((dir (expand-file-name (concat "org-scribe-templates/" set)
                               org-scribe-parity--root)))
    (sort (mapcar (lambda (f) (file-relative-name f dir))
                  (directory-files-recursively dir "\\.template\\'"))
          #'string<)))

;;; Tests

(ert-deftest test-template-parity-pairing-is-complete ()
  "Every shipped template appears in the pairing, on both sides.
This is the check that catches a file added to one set only — the drift
that produced a 2066-line English character template with an 89-line
Spanish counterpart."
  (should (equal (sort (mapcar #'car org-scribe-parity--pairs) #'string<)
                 (org-scribe-parity--templates "novel-es")))
  (should (equal (sort (mapcar #'cdr org-scribe-parity--pairs) #'string<)
                 (org-scribe-parity--templates "novel-en"))))

(ert-deftest test-template-parity-heading-trees-match ()
  "Paired templates have identical heading trees: count, depth and order."
  (dolist (pair org-scribe-parity--pairs)
    (unless (org-scribe-parity--divergent-p (car pair))
      (let ((es (org-scribe-parity--shape (org-scribe-parity--path "novel-es" (car pair))))
            (en (org-scribe-parity--shape (org-scribe-parity--path "novel-en" (cdr pair)))))
        (should (equal (cons (car pair) (car es))
                       (cons (car pair) (car en))))))))

(ert-deftest test-template-parity-property-sets-match ()
  "Paired templates use the same property keys, once localization is undone.
Scene properties legitimately differ in spelling between the sets
\(`:Brecha:' against `:Gap:'), so they are compared through
`org-scribe--scene-property-aliases' rather than literally."
  (dolist (pair org-scribe-parity--pairs)
    (unless (org-scribe-parity--divergent-p (car pair))
      (let ((es (org-scribe-parity--shape (org-scribe-parity--path "novel-es" (car pair))))
            (en (org-scribe-parity--shape (org-scribe-parity--path "novel-en" (cdr pair)))))
        (should (equal (cons (car pair) (cdr es))
                       (cons (car pair) (cdr en))))))))

(ert-deftest test-template-parity-exceptions-are-live ()
  "Every tolerated exception still actually diverges.
An exemption that no longer applies is worse than no exemption: it hides
the next regression in a pair someone has already fixed.  When this test
fails, the fix is to delete the entry from
`org-scribe-parity--known-divergent', not to re-break the templates."
  (dolist (entry org-scribe-parity--known-divergent)
    (let* ((es-name (car entry))
           (pair (assoc es-name org-scribe-parity--pairs)))
      (should pair)
      (let ((es (org-scribe-parity--shape (org-scribe-parity--path "novel-es" (car pair))))
            (en (org-scribe-parity--shape (org-scribe-parity--path "novel-en" (cdr pair)))))
        (should-not (and (equal (car es) (car en))
                         (equal (cdr es) (cdr en))))))))

(ert-deftest test-template-parity-exceptions-carry-a-reason ()
  "Each exemption states why it is tolerated, in prose someone can act on."
  (dolist (entry org-scribe-parity--known-divergent)
    (should (stringp (cdr entry)))
    (should (> (length (cdr entry)) 40))))

(ert-deftest test-template-parity-manuscript-properties-are-known-keys ()
  "Every scene property in both manuscripts is a registered canonical key.
Catches a typo — `:Brehca:' — which would otherwise pass the parity check
only if the same typo were made in both sets, and would silently be
invisible to `org-scribe-scene-property-get' either way."
  (let ((known (mapcar #'car org-scribe--scene-property-aliases))
        ;; Structural properties that are not scene metadata.  CATEGORY is
        ;; not declared anywhere in the templates: Org synthesizes one for
        ;; every heading, so it turns up in `org-entry-properties' output
        ;; whether or not the file mentions it.
        (structural '(wordcount word-objective custom_id id category))
        (offenders nil))
    (dolist (spec '(("novel-es" . "novela.org.template")
                    ("novel-en" . "novel.org.template")))
      (with-temp-buffer
        (insert-file-contents (org-scribe-parity--path (car spec) (cdr spec)))
        (org-mode)
        (org-map-entries
         (lambda ()
           (dolist (kv (org-entry-properties nil 'standard))
             (let ((key (org-scribe-parity--canonical-property (car kv))))
               (unless (or (memq key known) (memq key structural))
                 (push (cons (cdr spec) (car kv)) offenders))))))))
    (should-not offenders)))

(ert-deftest test-template-parity-templates-carry-no-local-variables ()
  "No shipped template declares file-local variables.

The spelling dictionary is a property of the *project* — the language
lives in `.org-scribe-project' and picks the whole template set — so it
is written once to `.dir-locals.el' at creation time (see
`org-scribe--write-dir-locals') rather than repeated per file.

Before that, eight templates carried a `Local Variables' block and the
rest did not, which is why this test exists: the coverage matched no
rule, the two sets diverged on it in a way `org-scribe-parity--shape'
cannot see (it compares headings and properties, not comments), and the
two manuscripts used an `eval:' form, which is never a safe file-local
and so made Emacs prompt on every open.  A block added back to one
template would quietly re-create all three problems."
  (let (offenders)
    (dolist (set '("novel-es" "novel-en" "short-story-es" "short-story-en"))
      (dolist (relative (org-scribe-parity--templates set))
        (with-temp-buffer
          (insert-file-contents (org-scribe-parity--path set relative))
          (goto-char (point-min))
          (when (re-search-forward "^# Local Variables:" nil t)
            (push (concat set "/" relative) offenders)))))
    (should-not offenders)))

(provide 'test-template-parity)
;;; test-template-parity.el ends here
