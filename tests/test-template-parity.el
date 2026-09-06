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
    ("cuaderno-borradores.org.template"   . "scratchpad.org.template")
    ("notas/notas.org.template"           . "notes/notes.org.template")
    ("notas/investigacion.org.template"   . "notes/research.org.template"))
  "Spanish template -> English template, relative to each set's directory,
for the novel family.  File names differ between the sets, so the
pairing cannot be derived and has to be declared.
`test-template-parity-pairing-is-complete' checks that this list accounts
for every shipped template on both sides.")

(defconst org-scribe-parity--short-story-pairs
  '(("cuento.org.template"              . "story.org.template")
    ("notas.org.template"               . "notes.org.template")
    ("README.org.template"              . "README.org.template")
    ("cuaderno-borradores.org.template" . "scratchpad.org.template"))
  "Spanish template -> English template, relative to each set's directory,
for the short-story family.  Mirrors `org-scribe-parity--pairs' for the
novel family, kept as a separate list because the two families are never
compared against each other (see `org-scribe-parity--families') — they
render two different methods and legitimately differ in sections,
property sets and design core.")

(defconst org-scribe-parity--families
  '((:es "novel-es"       :en "novel-en"       :pairs org-scribe-parity--pairs)
    (:es "short-story-es" :en "short-story-en" :pairs org-scribe-parity--short-story-pairs))
  "Every base template family this file checks for ES/EN structural parity.
Each entry names the Spanish and English set directories and the symbol
holding their file pairing (looked up with `symbol-value' rather than
stored as the list itself, so each pairing keeps its own defconst and
docstring).  Deliberately excludes the plotting-method overlays under
`org-scribe-templates/methods/', which have their own pairing
\(`org-scribe-parity--method-pairs') and their own tests below — they are
overlays on top of `novel-es'/`novel-en', not a third family of full
template sets, and a design-file-only overlay has no heading tree or
property set of its own to speak of beyond what those tests already
check.

The families are read independently everywhere in this file: no test
ever compares a `novel-*' template against a `short-story-*' one. They
render two different methods and are expected to differ in sections,
property sets and design core — only each family's own ES/EN pair must
stay parallel.")

(defconst org-scribe-parity--known-divergent
  '(("novel-es/diario-escritura.org.template"
     . "Habits and the writing calendar live outside the method on purpose, so
these two were never ports of each other and the migration did not touch
them.  English is the far longer of the two.  Reconciling them is a
decision about what the journal is for, not a translation task.")
    ("novel-es/objects/objetos.org.template"
     . "Objects have no counterpart in the method; both files are pre-sistema
stubs that were never parallel.  English carries Status and
First-appearance properties Spanish lacks.")
    ("novel-es/objects/cronologia.org.template"
     . "Timeline likewise has no counterpart in the method; English carries an
extra ID and Type property."))
  "Pairs known to diverge, each with the reason it is tolerated.
Keys are \"SET/RELATIVE-PATH\" (the Spanish set name, a slash, then the
Spanish-side path from `org-scribe-parity--pairs' or
`org-scribe-parity--short-story-pairs') rather than a bare relative path:
`README.org.template' is a pair in *both* families, and a bare-filename
key would silently exempt whichever family's pair happened to be looked
up first instead of the one actually intended.

Every entry is a debt, not a licence.  `test-template-parity-exceptions-are-live'
fails when an entry no longer diverges, so a pair that gets fixed cannot
leave a stale exemption behind that would hide the next regression.")

(defconst org-scribe-parity--method-pairs
  '(("methods/helice/es/diseno.org.template" . "methods/helice/en/design.org.template")
    ("methods/matriz/es/diseno.org.template" . "methods/matriz/en/design.org.template"))
  "Spanish overlay template -> English overlay template, for the plotting
methods added beside the Sistema (see `org-scribe--methods',
core/org-scribe-core.el).  Kept separate from `org-scribe-parity--pairs'
because that list's own tests hardcode the \"novel-es\"/\"novel-en\" set
names in `org-scribe-parity--path'; these files live under
`org-scribe-templates/methods/' instead, one design file per method per
language, with no base-set counterpart the way `novel-*'s files have.
`test-template-parity-method-overlay-pairing-is-complete' checks this
list accounts for every shipped overlay template on both sides, the same
guarantee `test-template-parity-pairing-is-complete' gives the base sets.")

(defconst org-scribe-parity--all-sets
  '("novel-es" "novel-en" "short-story-es" "short-story-en"
    "methods/helice/es" "methods/helice/en"
    "methods/matriz/es" "methods/matriz/en")
  "Every shipped template set, including the plotting-method overlays.
Used by the three cross-set checks (no stray `Local Variables' block,
every template declares its language, and in the shape jinx parses) so
that a new overlay set is covered by construction rather than by
remembering to add it to three separate literal lists.")

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

(defun org-scribe-parity--divergent-p (set es-name)
  "Return non-nil if the pair keyed by SET/ES-NAME is a known exception.
SET is the Spanish set name (e.g. \"novel-es\"); ES-NAME is the Spanish
side of the pair, relative to that set's directory.  See
`org-scribe-parity--known-divergent' for why the key must be qualified
by SET rather than a bare filename."
  (assoc (concat set "/" es-name) org-scribe-parity--known-divergent))

(defun org-scribe-parity--templates (set)
  "Return the shipped .template files of SET, relative to its directory."
  (let ((dir (expand-file-name (concat "org-scribe-templates/" set)
                               org-scribe-parity--root)))
    (sort (mapcar (lambda (f) (file-relative-name f dir))
                  (directory-files-recursively dir "\\.template\\'"))
          #'string<)))

;;; Tests
;;
;; Every test below iterates `org-scribe-parity--families' rather than
;; naming "novel-es"/"novel-en" directly, so a family's own pairing gets
;; the same guarantees as every other family's -- including the
;; short-story one, which had none of these checks until this file was
;; extended to iterate families instead of one hardcoded pair.  Never
;; compare across families: each entry's :es/:en names one family's own
;; ES/EN pair, and no test here ever mixes one family's file against
;; another's.

(ert-deftest test-template-parity-pairing-is-complete ()
  "Every shipped template appears in the pairing, on both sides, in every family.
This is the check that catches a file added to one set only — the drift
that produced a 2066-line English character template with an 89-line
Spanish counterpart."
  (dolist (family org-scribe-parity--families)
    (let ((es-set (plist-get family :es))
          (en-set (plist-get family :en))
          (pairs (symbol-value (plist-get family :pairs))))
      (should (equal (sort (mapcar #'car pairs) #'string<)
                     (org-scribe-parity--templates es-set)))
      (should (equal (sort (mapcar #'cdr pairs) #'string<)
                     (org-scribe-parity--templates en-set))))))

(ert-deftest test-template-parity-heading-trees-match ()
  "Paired templates have identical heading trees: count, depth and order,
within every family."
  (dolist (family org-scribe-parity--families)
    (let ((es-set (plist-get family :es))
          (en-set (plist-get family :en))
          (pairs (symbol-value (plist-get family :pairs))))
      (dolist (pair pairs)
        (unless (org-scribe-parity--divergent-p es-set (car pair))
          (let ((es (org-scribe-parity--shape (org-scribe-parity--path es-set (car pair))))
                (en (org-scribe-parity--shape (org-scribe-parity--path en-set (cdr pair)))))
            (should (equal (cons (concat es-set "/" (car pair)) (car es))
                           (cons (concat es-set "/" (car pair)) (car en))))))))))

(ert-deftest test-template-parity-property-sets-match ()
  "Paired templates use the same property keys, once localization is undone,
within every family.  Scene properties legitimately differ in spelling
between the sets (`:Brecha:' against `:Gap:'), so they are compared
through `org-scribe--scene-property-aliases' rather than literally."
  (dolist (family org-scribe-parity--families)
    (let ((es-set (plist-get family :es))
          (en-set (plist-get family :en))
          (pairs (symbol-value (plist-get family :pairs))))
      (dolist (pair pairs)
        (unless (org-scribe-parity--divergent-p es-set (car pair))
          (let ((es (org-scribe-parity--shape (org-scribe-parity--path es-set (car pair))))
                (en (org-scribe-parity--shape (org-scribe-parity--path en-set (cdr pair)))))
            (should (equal (cons (concat es-set "/" (car pair)) (cdr es))
                           (cons (concat es-set "/" (car pair)) (cdr en))))))))))

(ert-deftest test-template-parity-exceptions-are-live ()
  "Every tolerated exception still actually diverges.
An exemption that no longer applies is worse than no exemption: it hides
the next regression in a pair someone has already fixed.  When this test
fails, the fix is to delete the entry from
`org-scribe-parity--known-divergent', not to re-break the templates."
  (dolist (entry org-scribe-parity--known-divergent)
    (let* ((key (car entry))
           (slash (string-search "/" key))
           (es-set (substring key 0 slash))
           (es-name (substring key (1+ slash)))
           (family (cl-find-if (lambda (f) (equal (plist-get f :es) es-set))
                               org-scribe-parity--families))
           (en-set (plist-get family :en))
           (pair (assoc es-name (symbol-value (plist-get family :pairs)))))
      (should pair)
      (let ((es (org-scribe-parity--shape (org-scribe-parity--path es-set (car pair))))
            (en (org-scribe-parity--shape (org-scribe-parity--path en-set (cdr pair)))))
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
    (dolist (set org-scribe-parity--all-sets)
      (dolist (relative (org-scribe-parity--templates set))
        (with-temp-buffer
          (insert-file-contents (org-scribe-parity--path set relative))
          (goto-char (point-min))
          (when (re-search-forward "^# Local Variables:" nil t)
            (push (concat set "/" relative) offenders)))))
    (should-not offenders)))

;;; The plotting-method overlays

(ert-deftest test-template-parity-method-overlay-pairing-is-complete ()
  "Every shipped method-overlay template appears in the pairing, on both
sides.  Mirrors `test-template-parity-pairing-is-complete' for the
overlay tree under `org-scribe-templates/methods/'."
  (let ((es-files (sort (mapcar (lambda (f) (file-relative-name f org-scribe-parity--root))
                                (directory-files-recursively
                                 (expand-file-name "org-scribe-templates/methods" org-scribe-parity--root)
                                 "\\.template\\'"))
                        #'string<)))
    (should (equal (sort (append (mapcar (lambda (p) (concat "org-scribe-templates/" (car p)))
                                         org-scribe-parity--method-pairs)
                                 (mapcar (lambda (p) (concat "org-scribe-templates/" (cdr p)))
                                         org-scribe-parity--method-pairs))
                         #'string<)
                   es-files))))

(ert-deftest test-template-parity-method-overlays-heading-trees-match ()
  "Each plotting method's ES/EN design file shares one heading tree,
exactly like `test-template-parity-heading-trees-match' requires of the
base `novel-es'/`novel-en' pair.  There are no known-divergent overlay
pairs, unlike the base set: both files of a method pair are written
together as part of this feature, so there is no legacy drift to
tolerate."
  (dolist (pair org-scribe-parity--method-pairs)
    (let ((es (org-scribe-parity--shape
               (expand-file-name (concat "org-scribe-templates/" (car pair)) org-scribe-parity--root)))
          (en (org-scribe-parity--shape
               (expand-file-name (concat "org-scribe-templates/" (cdr pair)) org-scribe-parity--root))))
      (should (equal (cons (car pair) (car es)) (cons (car pair) (car en)))))))

(ert-deftest test-template-parity-method-overlays-property-sets-match ()
  "Each plotting method's ES/EN design file uses the same property keys.
Both are expected to be empty: the design file overlays hold no entity
headings (Q1 keeps the disasters/collisions as a plain table, not a
linked entity), so this mainly guards against one language accidentally
growing a `:PROPERTIES:' drawer the other lacks."
  (dolist (pair org-scribe-parity--method-pairs)
    (let ((es (org-scribe-parity--shape
               (expand-file-name (concat "org-scribe-templates/" (car pair)) org-scribe-parity--root)))
          (en (org-scribe-parity--shape
               (expand-file-name (concat "org-scribe-templates/" (cdr pair)) org-scribe-parity--root))))
      (should (equal (cons (car pair) (cdr es)) (cons (car pair) (cdr en)))))))

(ert-deftest test-template-parity-every-template-declares-its-language ()
  "Every shipped template carries a `#+LANGUAGE:' keyword for its set.

This is the per-file half of the language declaration; the project-wide
half is the generated `.dir-locals.el'.  The keyword matters for a file
read outside its project tree, where no directory-local applies, and it
is what jinx reads natively (`jinx--get-org-language').

Sixteen of the thirty-two templates used to carry it and sixteen did
not, with no rule behind the split: in a Spanish project the notes, the
journal, the timeline and both READMEs fell back to the user's global
language, so they were spell-checked — and completed from a word list —
in the wrong language.  Coverage that matches no rule is exactly the
drift `org-scribe-parity--shape' cannot see, since it compares headings
and properties, not keywords."
  (let (offenders)
    (dolist (set org-scribe-parity--all-sets)
      (let ((expected (if (string-suffix-p "es" set) "es" "en")))
        (dolist (relative (org-scribe-parity--templates set))
          (with-temp-buffer
            (insert-file-contents (org-scribe-parity--path set relative))
            (goto-char (point-min))
            (let ((case-fold-search t))
              (unless (and (re-search-forward
                            "^#\\+LANGUAGE: +\\([a-z_]+\\) *$" nil t)
                           (equal (match-string 1) expected))
                (push (concat set "/" relative) offenders)))))))
    (should-not offenders)))

(ert-deftest test-template-parity-language-keyword-matches-jinx-regexp ()
  "The keyword is written in the shape jinx actually parses.

Jinx reads the language with `\"^ *#\\+language: +\\([a-z_]+\\) *$\"'.
Two things about that are easy to break without noticing: the value must
be a bare code with nothing after it — a trailing comment, or a form
like `es-ES', simply fails to match — and the match relies on
`case-fold-search', which Org buffers set, to accept the uppercase
`#+LANGUAGE:' the templates use.  A template that fails this is not
broken in any visible way; it just silently keeps the global language."
  (dolist (set org-scribe-parity--all-sets)
    (dolist (relative (org-scribe-parity--templates set))
      (with-temp-buffer
        (insert-file-contents (org-scribe-parity--path set relative))
        (org-mode)
        (goto-char (point-min))
        (should (re-search-forward "^ *#\\+language: +\\([a-z_]+\\) *$"
                                   nil t))))))

(provide 'test-template-parity)
;;; test-template-parity.el ends here
