;;; test-lang.el --- Tests for the language pack registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for `lang/org-scribe-lang.el' and the two shipped packs
;; (`lang/org-scribe-lang-en.el', `lang/org-scribe-lang-es.el').  See
;; "i18n-extended.org" for the design this pins down.
;;
;; Structural key-set/shape tests (does every registered pack agree on
;; what it declares) live here; `tests/test-messages.el' separately
;; tests `org-scribe-msg''s own behavior (language switching, format
;; substitution, the dynamic-shadowing fallback path) against the two
;; shipped packs' derived `org-scribe-messages-en'/`-es' variables.  The
;; two message-parity tests below duplicate a check `test-messages.el'
;; also makes, deliberately: each file is a self-sufficient suite for
;; its own module (the pack registry vs. the message API), not a single
;; check split awkwardly across two files.
;;
;; `test-lang-third-language-resolves' is the acceptance test for the
;; whole language-pack effort: a language registered from nothing but
;; `tests/fixtures/org-scribe-lang-fr.el' plus two accessor calls must
;; resolve a real project tree end to end.  If that requires touching
;; anything outside `tests/', the seams described in "i18n-extended.org"
;; are not real.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; `load-file-name'/`buffer-file-name' are only meaningfully bound while
;; this file itself is being loaded -- not later, when a test body runs
;; under `ert-run-tests-batch-and-exit' in batch mode, where both are
;; nil.  Capture the paths this file needs ONCE, here at top level, and
;; have every later reference (including inside test bodies) use these
;; constants instead of re-deriving from `load-file-name'.
(defconst org-scribe--test-lang-tests-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding this file, captured at load time.")

(defconst org-scribe--test-lang-repo-root
  (file-name-directory (directory-file-name org-scribe--test-lang-tests-dir))
  "The repository root, captured at load time.")

;; Add module directories to load path
(add-to-list 'load-path (expand-file-name "lang" org-scribe--test-lang-repo-root))
(add-to-list 'load-path (expand-file-name "core" org-scribe--test-lang-repo-root))

(require 'org-scribe-lang)
(require 'org-scribe-lang-en)
(require 'org-scribe-lang-es)
(require 'org-scribe-core)

;; The `fr' fixture is loaded (its `defconst' evaluated, `provide'd) but
;; never registered here at top level -- each test that needs it
;; registers it itself and unregisters it in an `unwind-protect', so no
;; other test in the suite, run before or after, ever sees a "fr" entry
;; in the live registry.
(load (expand-file-name "fixtures/org-scribe-lang-fr" org-scribe--test-lang-tests-dir))
;; `org-scribe-lang-fr' is defined by the `load' above, not visible to
;; the byte-compiler statically the way a `require'd symbol would be.
(defvar org-scribe-lang-fr)

(defmacro test-lang--with-fr-registered (&rest body)
  "Register the `fr' fixture pack, run BODY, then unregister it.
Uses `assq-delete-all' directly on `org-scribe--language-packs' rather
than any public unregister function -- none exists, deliberately: a
shipped pack is never meant to be removed once registered, only a test
fixture reaches into the variable directly like this."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (org-scribe-register-language 'fr org-scribe-lang-fr)
         ,@body)
     (setq org-scribe--language-packs
           (assq-delete-all 'fr org-scribe--language-packs))))

;;; Structural key-set parity (the two shipped packs)

(ert-deftest test-lang-packs-have-identical-key-sets ()
  "Every registered pack declares the same top-level plist keys."
  (let* ((languages (org-scribe-languages))
         (reference-keys (sort (org-scribe--test-lang-plist-keys
                                (org-scribe-lang-pack (car languages)))
                               #'string<)))
    (dolist (lang (cdr languages))
      (should (equal reference-keys
                     (sort (org-scribe--test-lang-plist-keys (org-scribe-lang-pack lang))
                          #'string<))))))

(defun org-scribe--test-lang-plist-keys (plist)
  "Return the keys of PLIST as a list of strings."
  (let (keys)
    (while plist
      (push (symbol-name (car plist)) keys)
      (setq plist (cddr plist)))
    keys))

(defun org-scribe--test-lang-section-key-sets-match (section)
  "Assert every registered pack's SECTION alist has the same keys."
  (let* ((languages (org-scribe-languages))
         (reference-keys (sort (mapcar #'car (plist-get (org-scribe-lang-pack (car languages)) section))
                               #'string-lessp)))
    (dolist (lang (cdr languages))
      (should (equal reference-keys
                     (sort (mapcar #'car (plist-get (org-scribe-lang-pack lang) section))
                          #'string-lessp))))))

(ert-deftest test-lang-files-key-sets-match ()
  "Every registered pack's `:files' section has the same concept keys."
  (org-scribe--test-lang-section-key-sets-match :files))

(ert-deftest test-lang-dirs-key-sets-match ()
  "Every registered pack's `:dirs' section has the same concept keys."
  (org-scribe--test-lang-section-key-sets-match :dirs))

(ert-deftest test-lang-properties-key-sets-match ()
  "Every registered pack's `:properties', `:headings', `:templates' and
`:method-labels' sections each have the same keys across packs."
  (dolist (section '(:properties :headings :templates :method-labels))
    (org-scribe--test-lang-section-key-sets-match section)))

;;; Message parity (see this file's Commentary for why this duplicates
;;; a test-messages.el check on purpose)

(ert-deftest test-lang-message-key-sets-match ()
  "Every registered pack's `:messages' section defines the same keys."
  (org-scribe--test-lang-section-key-sets-match :messages))

(defun org-scribe--test-lang-format-spec-count (template)
  "Return the number of %s/%d specifiers in TEMPLATE."
  (let ((count 0) (pos 0))
    (while (string-match "%[sd]" template pos)
      (setq count (1+ count))
      (setq pos (match-end 0)))
    count))

(ert-deftest test-lang-message-format-specs-match ()
  "For every message key, the %s/%d specifier count agrees across every
registered pack, each compared against the first-registered language."
  (let* ((languages (org-scribe-languages))
         (reference-table (plist-get (org-scribe-lang-pack (car languages)) :messages)))
    (dolist (lang (cdr languages))
      (let ((table (plist-get (org-scribe-lang-pack lang) :messages)))
        (dolist (entry reference-table)
          (let* ((key (car entry))
                 (reference-template (cdr entry))
                 (template (alist-get key table)))
            (should template)
            (should (= (org-scribe--test-lang-format-spec-count reference-template)
                      (org-scribe--test-lang-format-spec-count template)))))))))

;;; Filename hygiene

(ert-deftest test-lang-file-paths-use-declared-dirs ()
  "Every `:files' value naming a subdirectory uses that pack's own
`:dirs' spelling for it.  Catches, e.g., a pack that declares
`:dirs' \\='((objects . \"objets\")) but still writes
\"objects/personnages.org\" in `:files' -- the English directory name
under a French pack."
  (dolist (lang (org-scribe-languages))
    (let* ((pack (org-scribe-lang-pack lang))
           (declared-dirs (mapcar #'cdr (plist-get pack :dirs))))
      (dolist (row (plist-get pack :files))
        (let* ((path (cdr row))
               (slash (string-search "/" path)))
          (when slash
            (should (member (substring path 0 slash) declared-dirs))))))))

(ert-deftest test-lang-no-duplicate-filenames-within-pack ()
  "No two concepts in one pack resolve to the same file path."
  (dolist (lang (org-scribe-languages))
    (let ((paths (mapcar #'cdr (plist-get (org-scribe-lang-pack lang) :files))))
      (should (= (length paths) (length (delete-dups (copy-sequence paths))))))))

(ert-deftest test-lang-filenames-unique-across-packs ()
  "No filename appears in two packs under different concepts.
The same filename under the SAME concept in two packs is fine and
common (\"revision.org\", \"README.org\" -- identical in every shipped
language); the same filename under two DIFFERENT concepts would make
cross-language resolution (`org-scribe-lang-all') ambiguous about
which concept a resolved path actually names."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (lang (org-scribe-languages))
      (dolist (row (plist-get (org-scribe-lang-pack lang) :files))
        (let* ((concept (car row))
               (path (cdr row))
               (existing (gethash path seen)))
          (if existing
              (should (eq existing concept))
            (puthash path concept seen)))))))

;;; Accessor contracts (D3: structural accessors signal, message lookup
;;; never does)

(ert-deftest test-lang-structural-accessor-signals ()
  "`org-scribe-lang-file' signals on an unknown key; `org-scribe-lang-message'
never does (D3 -- a missing structural key is a packaging bug and must
not silently resolve to another language's spelling; a missing message
is a translation gap and must never surface as a Lisp error mid-sentence)."
  (should-error (org-scribe-lang-file 'totally-bogus-concept-zzz 'en))
  (should (stringp (org-scribe-lang-message 'totally-bogus-message-key-zzz 'en))))

(ert-deftest test-lang-message-falls-back ()
  "A key present only in the fallback language resolves via the
fallback; a key present in no registered pack returns its own symbol
name.  Uses the `fr' fixture, whose tiny `:messages' table is missing
almost every key the shipped packs define."
  (test-lang--with-fr-registered
    ;; `msg-inserted-link' is in the English pack but not in `fr''s
    ;; two-entry fixture table; `org-scribe-fallback-language' defaults
    ;; to 'en.
    (should (equal (org-scribe-lang-message 'msg-inserted-link 'fr)
                   (org-scribe-lang-message 'msg-inserted-link 'en)))
    ;; A key present in no registered pack at all.
    (should (equal (org-scribe-lang-message 'totally-nonexistent-key-zzz 'fr)
                   "totally-nonexistent-key-zzz"))))

;;; The acceptance test

(ert-deftest test-lang-third-language-resolves ()
  "A language registered from nothing but a pack file resolves a real
project tree end to end.  Builds a project by hand using only the
`fr' fixture's own spellings (roman.org, objets/, objets/personnages.org)
and asserts both `org-scribe-project-type' and
`org-scribe-project-structure' -- reading no French-specific code,
only the live registry -- find it correctly."
  (let ((temp-dir (make-temp-file "org-scribe-lang-fr-test-" t))
        (org-scribe--project-type-cache nil)
        (org-scribe--project-language-cache nil))
    (unwind-protect
        (test-lang--with-fr-registered
          (make-directory (expand-file-name "objets" temp-dir) t)
          (with-temp-file (expand-file-name "roman.org" temp-dir)
            (insert "#+TITLE: Test\n"))
          (with-temp-file (expand-file-name "objets/personnages.org" temp-dir)
            (insert "#+TITLE: Personnages\n"))
          (let ((default-directory temp-dir))
            ;; No .org-scribe-project marker: type detection must find
            ;; this via the structural probes (the "objets" directory,
            ;; then "roman.org"), not a marker line.
            (should (eq 'novel (org-scribe-project-type)))
            (let ((structure (org-scribe-project-structure)))
              (should (string-suffix-p "roman.org" (plist-get structure :manuscript-file)))
              (should (string-suffix-p "objets/personnages.org"
                                       (plist-get structure :characters-file))))))
      (setq org-scribe--project-type-cache nil)
      (setq org-scribe--project-language-cache nil)
      (delete-directory temp-dir t))))

;;; The regrowth guard

(defconst org-scribe--test-lang-localized-literals
  '("novela.org" "cuento.org" "diseno.org" "personajes.org"
    "localizaciones.org" "objetos" "trama.org" "cronologia.org"
    "notas" "investigacion.org" "Personajes" "Ambientaci"
    "Hilos de" "irrenunciables" "Puerta de salida" "Línea Temporal")
  "Spanish-specific literals a shipped, non-test, non-pack .el file must
never name directly -- see `test-lang-no-hardcoded-localized-names-in-code'.")

(ert-deftest test-lang-no-hardcoded-localized-names-in-code ()
  "No shipped .el file outside lang/ names a localized string literally.
This does not test behavior; it prevents regrowth of the exact bug
class this whole refactor removed -- a filename, heading or property
spelling hand-typed in a second place, independently of the language
pack that is supposed to be its only source, free to drift out of
step with it unnoticed.  Mirrors
`test-template-parity-templates-carry-no-local-variables' in spirit.

Deliberately greps the whole shipped tree rather than a fixed file
list, so a literal reintroduced in a file nobody thought to check is
still caught.  Docstrings and comments in shipped code are not exempt
-- see \"i18n-extended.org\", Step 9: an explanatory Spanish example in
a comment is exactly as fragile as one in a resolver, since neither is
checked against the packs, and this project's own history (the
`org-scribe--methods' labels, the `--section-heading-aliases' comment)
is that an unindexed example rots exactly like code does.  The fix,
each time this test finds something, is to reword the prose, not to
carve out an exemption."
  (let* ((default-directory org-scribe--test-lang-repo-root)
         (files (seq-filter
                 (lambda (f) (not (or (string-prefix-p "lang/" f)
                                     (string-prefix-p "tests/" f))))
                 (split-string
                  (shell-command-to-string "git ls-files '*.el'")
                  "\n" t)))
         (offenders nil))
    (dolist (file files)
      (when (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (let ((content (buffer-string)))
            (dolist (literal org-scribe--test-lang-localized-literals)
              (when (string-search literal content)
                (push (cons file literal) offenders)))))))
    (should (null offenders))))

(provide 'test-lang)

;;; test-lang.el ends here
