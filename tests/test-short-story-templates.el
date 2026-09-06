;;; test-short-story-templates.el --- Tests for the short-story template set -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for the short-story notes.org / notas.org templates' entity
;; recognition, modelled on tests/test-sistema-templates.el.
;;
;; The point of this file, specifically, is the scenario the shipped
;; templates used to fail: a writer replaces a placeholder heading
;; (\"Protagonist: [Name]\", \"Main Location(s)\") with the entity's real
;; name.  Before the templates carried an explicit :Role:/:Type: property,
;; recognition fell back to matching the placeholder's own heading text
;; against a regexp -- so the entity was found only until the writer did
;; exactly what the template asked them to do, at which point it silently
;; stopped being found by every completion, search and health-report
;; check.  A naive "are the entities found?" test against the template
;; as shipped would not have caught this, because the placeholder text
;; itself happens to match the fallback regexp; this file always renames
;; first.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)

;;; Add paths

(defvar org-scribe-ss-test--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "Repository root, used to locate the shipped template files.")

(let ((default-directory org-scribe-ss-test--root))
  (dolist (dir '("core" "search" "linking" "capture" "templates"))
    (add-to-list 'load-path (expand-file-name dir default-directory))))

(require 'org-scribe-character-links)
(require 'org-scribe-location-links)

;;; Helpers

(defun org-scribe-ss-test--template (relative)
  "Return the absolute path of template RELATIVE to the templates directory."
  (expand-file-name (concat "org-scribe-templates/" relative)
                    org-scribe-ss-test--root))

(defmacro org-scribe-ss-test--with-template (relative &rest body)
  "Visit template RELATIVE in `org-mode', with `org-scribe-project-type'
stubbed to \\='short-story, and run BODY with point at the start.
Stubbing the project type is necessary because these templates are read
from a bare temp buffer, not a real project directory the type
detection strategies could otherwise resolve from disk."
  (declare (indent 1))
  `(let ((file (org-scribe-ss-test--template ,relative)))
     (should (file-exists-p file))
     (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
       (with-temp-buffer
         (insert-file-contents file)
         (org-mode)
         (goto-char (point-min))
         ,@body))))

(defun org-scribe-ss-test--rename-heading (old new)
  "Replace the first occurrence of heading text OLD with NEW.
Simulates a writer overwriting a template placeholder with the entity's
real name, which is exactly the moment the template contract is
supposed to survive."
  (goto-char (point-min))
  (should (re-search-forward (concat "^\\(\\*+ \\)" (regexp-quote old) "$") nil t))
  (replace-match (concat "\\1" new)))

(defun org-scribe-ss-test--heading-recognized-p (heading predicate)
  "Return non-nil if the heading matching HEADING satisfies PREDICATE.
Searches for HEADING literally (post-rename), moves to it, and calls
PREDICATE with point there.  Uses `beginning-of-line', not
`org-back-to-heading': the shipped templates carry `#+STARTUP: overview',
which folds every level-2 heading invisible, and `org-back-to-heading'
(called with its default INVISIBLE-OK of nil) skips an invisible heading
line entirely rather than stopping on it -- landing on the outer, still-
visible level-1 wrapper instead.  Since the search regexp is already
anchored to the start of the heading line, moving to its beginning needs
no heading-aware navigation at all."
  (goto-char (point-min))
  (should (re-search-forward (concat "^\\*+ " (regexp-quote heading) "$") nil t))
  (beginning-of-line)
  (funcall predicate))

;;; English: characters survive a rename

(ert-deftest test-short-story-en-protagonist-survives-rename ()
  "The protagonist is still recognized as a character after the
placeholder heading is replaced with a real name -- because recognition
now goes through :Role:, not the word \"Protagonist\" in the heading."
  (org-scribe-ss-test--with-template "short-story-en/notes.org.template"
    (org-scribe-ss-test--rename-heading "Protagonist: [Name]" "Mara Ilić")
    (should (org-scribe-ss-test--heading-recognized-p
             "Mara Ilić" #'org-scribe--character-heading-p))))

(ert-deftest test-short-story-en-supporting-character-survives-rename ()
  (org-scribe-ss-test--with-template "short-story-en/notes.org.template"
    (org-scribe-ss-test--rename-heading "Supporting Character: [Name]" "Devon Ashworth")
    (should (org-scribe-ss-test--heading-recognized-p
             "Devon Ashworth" #'org-scribe--character-heading-p))))

(ert-deftest test-short-story-en-antagonist-survives-rename ()
  (org-scribe-ss-test--with-template "short-story-en/notes.org.template"
    (org-scribe-ss-test--rename-heading "Antagonist: [Name]" "Corvin Blackwell")
    (should (org-scribe-ss-test--heading-recognized-p
             "Corvin Blackwell" #'org-scribe--character-heading-p))))

(ert-deftest test-short-story-en-character-name-is-not-the-empty-string ()
  "Regression test for the blank :NAME: bug: `org-scribe--entity-name-at-point'
must resolve to the heading text, not the empty string, for every
character the template ships as-is (no rename needed to expose this
one -- the bug was in the template's own shipped shape)."
  (org-scribe-ss-test--with-template "short-story-en/notes.org.template"
    (let (names)
      (org-map-entries
       (lambda ()
         (when (org-scribe--character-heading-p)
           (push (org-scribe--entity-name-at-point) names))))
      (should (= (length names) 3))
      (dolist (name names)
        (should (org-string-nw-p name))))))

;;; English: location survives a rename

(ert-deftest test-short-story-en-main-location-survives-rename ()
  "The main location is still recognized after the placeholder heading
is replaced with a real place name -- recognition now goes through
:Type:, not the word \"Location\" in the heading."
  (org-scribe-ss-test--with-template "short-story-en/notes.org.template"
    (org-scribe-ss-test--rename-heading "Main Location(s)" "The Salt Marsh Inn")
    (should (org-scribe-ss-test--heading-recognized-p
             "The Salt Marsh Inn" #'org-scribe--location-heading-p))))

;;; Spanish: characters survive a rename

(ert-deftest test-short-story-es-protagonist-survives-rename ()
  (org-scribe-ss-test--with-template "short-story-es/notas.org.template"
    (org-scribe-ss-test--rename-heading "Protagonista: [Nombre]" "Marisol Aguirre")
    (should (org-scribe-ss-test--heading-recognized-p
             "Marisol Aguirre" #'org-scribe--character-heading-p))))

(ert-deftest test-short-story-es-supporting-character-survives-rename ()
  (org-scribe-ss-test--with-template "short-story-es/notas.org.template"
    (org-scribe-ss-test--rename-heading "Personaje Secundario: [Nombre]" "Tomás Rivadeneira")
    (should (org-scribe-ss-test--heading-recognized-p
             "Tomás Rivadeneira" #'org-scribe--character-heading-p))))

(ert-deftest test-short-story-es-antagonist-survives-rename ()
  (org-scribe-ss-test--with-template "short-story-es/notas.org.template"
    (org-scribe-ss-test--rename-heading "Antagonista: [Nombre]" "Ezequiel Duarte")
    (should (org-scribe-ss-test--heading-recognized-p
             "Ezequiel Duarte" #'org-scribe--character-heading-p))))

(ert-deftest test-short-story-es-role-values-are-english ()
  "Per docs/glossary.org, :Role: property values are English in both
template sets -- this is not a localization gap, it is policy, so the
Spanish template must not carry a translated value like \"Protagonista\"."
  (org-scribe-ss-test--with-template "short-story-es/notas.org.template"
    (let (roles)
      (org-map-entries
       (lambda ()
         (when-let ((role (org-entry-get nil "Role")))
           (push role roles))))
      (should (equal (sort roles #'string<)
                     (sort '("Protagonist" "Supporting" "Antagonist") #'string<))))))

;;; Spanish: location survives a rename

(ert-deftest test-short-story-es-main-location-survives-rename ()
  (org-scribe-ss-test--with-template "short-story-es/notas.org.template"
    (org-scribe-ss-test--rename-heading "Ubicación(es) Principal(es)" "La Posada del Cabo")
    (should (org-scribe-ss-test--heading-recognized-p
             "La Posada del Cabo" #'org-scribe--location-heading-p))))

;;; Neither language's template ships a stray :NAME:/:TYPE: to regress to

(ert-deftest test-short-story-templates-carry-no-name-property ()
  "Neither shipped short-story template carries a :NAME:/:NOMBRE: property
any more -- the heading is the entity's name, per the template contract
every other entity type in this package follows."
  (dolist (relative '("short-story-en/notes.org.template"
                      "short-story-es/notas.org.template"))
    (with-temp-buffer
      (insert-file-contents (org-scribe-ss-test--template relative))
      (goto-char (point-min))
      (should-not (re-search-forward "^:NAME:\\|^:NOMBRE:" nil t)))))

(ert-deftest test-short-story-templates-carry-no-type-property ()
  "Neither shipped short-story template still classifies characters via
the literal, all-caps :TYPE:/:TIPO: -- that has become :Role: for
characters, matching the novel sets and the property the entity
predicate actually reads.  Case-sensitive on purpose: locations do carry
a mixed-case :Type: (the property `org-scribe--location-heading-p'
reads), and `case-fold-search' defaults to t, so a case-insensitive
search here would also -- wrongly -- flag that one."
  (dolist (relative '("short-story-en/notes.org.template"
                      "short-story-es/notas.org.template"))
    (with-temp-buffer
      (insert-file-contents (org-scribe-ss-test--template relative))
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (should-not (re-search-forward "^:TYPE:\\|^:TIPO:" nil t))))))

;;; Run tests

(defun org-scribe-short-story-templates-run-tests ()
  "Run all short-story template tests."
  (interactive)
  (ert "^test-short-story-"))

(provide 'test-short-story-templates)

;;; test-short-story-templates.el ends here
