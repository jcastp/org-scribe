;;; test-capture.el --- Tests for capture system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for the project-aware capture system.
;; Tests capture target file detection, file creation,
;; and template availability.

;;; Code:

(require 'ert)
(require 'org)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../capture" default-directory)))

(require 'org-scribe-core)
(require 'org-scribe-capture)

;;; Function Availability Tests

;;; Helper Function Tests

;;; Capture Template Tests

(ert-deftest test-capture-templates-defined ()
  "Test that capture templates are defined."
  (should (fboundp 'org-scribe-capture-templates))
  (should (listp (org-scribe-capture-templates)))
  (should (> (length (org-scribe-capture-templates)) 0)))

(ert-deftest test-character-capture-templates-defined ()
  "Test that character capture templates are defined."
  (should (fboundp 'org-scribe-character-capture-templates))
  (should (listp (org-scribe-character-capture-templates)))
  (should (> (length (org-scribe-character-capture-templates)) 0)))

(ert-deftest test-location-capture-templates-defined ()
  "Test that location capture templates are defined."
  (should (fboundp 'org-scribe-location-capture-templates))
  (should (listp (org-scribe-location-capture-templates)))
  (should (> (length (org-scribe-location-capture-templates)) 0)))

(ert-deftest test-object-capture-templates-defined ()
  "Test that object capture templates are defined."
  (should (fboundp 'org-scribe-object-capture-templates))
  (should (listp (org-scribe-object-capture-templates)))
  (should (> (length (org-scribe-object-capture-templates)) 0)))

(ert-deftest test-timeline-capture-templates-defined ()
  "Test that timeline capture templates are defined."
  (should (fboundp 'org-scribe-timeline-capture-templates))
  (should (listp (org-scribe-timeline-capture-templates)))
  (should (> (length (org-scribe-timeline-capture-templates)) 0)))

;;; File Creation Tests

;;; Target File Detection Tests

(ert-deftest test-capture-target-file-detection ()
  "Test that capture target file detection works."
  ;; Without a proper project structure, should return current buffer or default
  (let ((target (org-scribe-capture-target-file)))
    (should (stringp target))
    (should (string-match-p "\\.org$" target))))

;;; Template Structure Tests

(ert-deftest test-character-template-has-required-fields ()
  "The character template files the method's ficha completa.
Asserts the causal chain in order (Ghost -> Lie -> Weakness ->
Desire/Need), because the chain is what the ficha is for: each link is
derived from the one above it, and a template that lists them out of
order invites filling them in out of order."
  (let ((template-string (nth 4 (car (org-scribe-character-capture-templates 'en)))))
    ;; The Role property is what `org-scribe--character-heading-p' reads,
    ;; and the heading must stay free for the character's name.
    (should (string-match-p ":ID:" template-string))
    (should (string-match-p ":Role:" template-string))
    (should (string-match-p "Protagonist|Opponent|Ally" template-string))
    (should-not (string-match-p ":Gender:" template-string))
    ;; The causal chain, in order.  Anchored to the "- Label ::" list form:
    ;; a bare label also matches inside the property drawer, where :Stance:
    ;; precedes everything and makes the order check pass or fail by accident.
    (let ((positions (mapcar (lambda (label)
                               (string-match (regexp-quote (concat "- " label " ::"))
                                             template-string))
                             '("Concept" "Stance" "Ghost" "Lie" "Trouble"
                               "Psychological Weakness" "Moral Weakness"))))
      (should (cl-every #'identity positions))
      (should (equal positions (sort (copy-sequence positions) #'<))))
    ;; Desire and Need, each in three parts.
    (should (string-match-p "Desire" template-string))
    (should (string-match-p "Need" template-string))
    (should (= 2 (cl-count "Goal ::" (split-string template-string "\n")
                           :test (lambda (a b) (string-match-p (regexp-quote a) b)))))
    (should (string-match-p "Change Milestones" template-string))
    ;; The superseded model must be gone entirely.
    (dolist (old '("Physical Description" "Personality" "Character Arc"
                   "Goal, Motivation, Conflict" "Psychological Flaw"))
      (should-not (string-match-p (regexp-quote old) template-string)))))

(ert-deftest test-location-template-has-required-fields ()
  "The setting template files the method's four setting Aspects.
Echo and Hazard are the two that make a place part of the argument
rather than scenery; the gazetteer fields of the previous model
(climate, population, terrain) are deliberately gone."
  (let ((template-string (nth 4 (car (org-scribe-location-capture-templates 'en)))))
    (should (string-match-p ":ID:" template-string))
    (should (string-match-p ":Type:" template-string))
    (dolist (aspect '("Echo" "Hazard" "Character" "Symbol"))
      (should (string-match-p aspect template-string)))
    (should (string-match-p "Description" template-string))
    (dolist (old '("Geography" "Cultural Aspects" "Notable Features"
                   "Atmosphere & Mood" ":Population:" ":Climate:"))
      (should-not (string-match-p (regexp-quote old) template-string)))))

(ert-deftest test-object-template-has-required-fields ()
  "Test that object template includes all required fields."
  (let ((template-string (nth 4 (car (org-scribe-object-capture-templates 'en)))))
    (should (string-match-p ":Type:" template-string))
    (should (string-match-p ":Owner:" template-string))
    (should (string-match-p ":Status:" template-string))
    (should (string-match-p "Physical Description" template-string))
    (should (string-match-p "Origin" template-string))
    (should (string-match-p "Properties" template-string))
    (should (string-match-p "Plot" template-string))
    (should (string-match-p "Symbolism" template-string))))

(ert-deftest test-timeline-template-has-required-fields ()
  "Test that timeline template includes all required fields."
  (let ((template-string (nth 4 (car (org-scribe-timeline-capture-templates 'en)))))
    (should (string-match-p ":ID:" template-string))
    (should (string-match-p ":Type:" template-string))
    (should (string-match-p ":Date:" template-string))
    (should (string-match-p ":Characters:" template-string))
    (should (string-match-p ":Location:" template-string))
    (should (string-match-p "Description" template-string))
    (should (string-match-p "Consequences" template-string))
    (should (string-match-p "Connections" template-string))))

;;; Sub-heading levels

(ert-deftest test-object-and-timeline-templates-do-not-skip-a-level ()
  "Sub-headings sit one level below the entry, not two.
org-capture shifts the whole template relative to its target, so a
\"***\" under a \"*\" left a gap wherever the entry was filed."
  (dolist (builder '(org-scribe-object-capture-templates
                     org-scribe-timeline-capture-templates))
    (dolist (lang '(en es))
      (let* ((template (nth 4 (car (funcall builder lang))))
             (stars (delq nil
                          (mapcar (lambda (l)
                                    (when (string-match "^\\(\\*+\\) " l)
                                      (length (match-string 1 l))))
                                  (split-string template "\n")))))
        (should (equal (list builder 1) (list builder (car stars))))
        (should (cdr stars))
        (dolist (n (cdr stars))
          (should (equal (list builder 2) (list builder n))))))))

;;; Role default

(ert-deftest test-commands-outside-a-project-refuse-and-create-nothing ()
  "Outside a project, commands that need one refuse, and write no files.
`org-scribe-project-root' falls back to the current directory, so these
used to treat any folder as a project: a .dir-locals.el written there,
entity files created next to an unrelated Org file, and a raw
error from the plot setup commands."
  (let* ((dir (file-name-as-directory (make-temp-file "loose-" t)))
         (default-directory dir))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "inbox.org" dir) (insert "* Buy milk\n"))
          (org-scribe-project-type-cache-clear)
          (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (dolist (cmd '(org-scribe-update-dir-locals
                           org-scribe-capture-character org-scribe-capture-location
                           org-scribe-capture-object org-scribe-capture-timeline
                           org-scribe-capture-plot-thread
                           org-scribe-setup-character-links org-scribe-setup-plot-thread-links
                           org-scribe-setup-plot-point-links org-scribe-setup-character-relationships
                           org-scribe-relink-project org-scribe-project-health))
              (should (equal (list cmd 'user-error)
                             (list cmd (condition-case err (progn (funcall cmd) 'ok)
                                         (user-error 'user-error)
                                         (error err)))))))
          (should (equal '("inbox.org")
                         (directory-files dir nil directory-files-no-dot-files-regexp))))
      (org-scribe-project-type-cache-clear)
      (delete-directory dir t))))

(ert-deftest test-object-capture-type-default-is-not-magical ()
  "Pressing RET on the object Type prompt must not make every object magical."
  (dolist (lang '(en es))
    (let ((template (nth 4 (car (org-scribe-object-capture-templates lang)))))
      (should (string-match-p ":Type: %\\^{[^|}]+|\\(Artifact\\|Artefacto\\)|" template))
      (should (string-match-p "Magical\\|Mágico" template)))))

(ert-deftest test-character-capture-role-default-is-not-protagonist ()
  "Pressing RET on the Role prompt must not mint another protagonist."
  (dolist (lang '(en es))
    (let ((template (nth 4 (car (org-scribe-character-capture-templates lang)))))
      (should (string-match-p "Protagonist|Opponent|Ally" template))
      (should (string-match-p ":Role: %^{[^|}]+|Thematic Supporting|" template)))))

;;; Auto-ID Generation Tests

(ert-deftest test-character-template-generates-id ()
  "Test that character template includes auto-ID generation."
  (let ((template-string (nth 4 (car (org-scribe-character-capture-templates 'en)))))
    ;; Should have %(org-id-new) in ID property
    (should (string-match-p ":ID: %(org-id-new)" template-string))))

(ert-deftest test-location-template-generates-id ()
  "Test that location template includes auto-ID generation."
  (let ((template-string (nth 4 (car (org-scribe-location-capture-templates 'en)))))
    (should (string-match-p ":ID: %(org-id-new)" template-string))))

(ert-deftest test-timeline-template-generates-id ()
  "Test that timeline template includes auto-ID generation."
  (let ((template-string (nth 4 (car (org-scribe-timeline-capture-templates 'en)))))
    (should (string-match-p ":ID: %(org-id-new)" template-string))))

;;; ─────────────────────────────────────────────
;;; File Creation Routing — both project types
;;; ─────────────────────────────────────────────

;;; ─────────────────────────────────────────────
;;; create-if-missing — auto-creates target file
;;; ─────────────────────────────────────────────

;;; ─────────────────────────────────────────────
;;; Project-type routing — without filesystem
;;; ─────────────────────────────────────────────

;;; ─────────────────────────────────────────────
;;; Regression: project root resolution
;;; ─────────────────────────────────────────────

;;; ─────────────────────────────────────────────
;;; ID assignment on finalisation
;;; ─────────────────────────────────────────────

(ert-deftest test-capture-finalize-hook-adds-id-to-heading ()
  "Test that the finalize hook adds an ID to a heading that lacks one."
  (let ((temp-file (make-temp-file "test-finalize-" nil ".org")))
    (unwind-protect
        (progn
          ;; Create a minimal entity file with one heading and no ID
          (with-temp-file temp-file
            (insert "* Alice\n:PROPERTIES:\n:Role: Protagonist\n:END:\n\n"))
          (with-current-buffer (find-file-noselect temp-file)
            (unwind-protect
                (let ((org-capture-mode t))
                  (cl-letf (((symbol-function 'org-scribe-capture-target-file)
                             (lambda (&rest _) temp-file)))
                    (org-scribe--capture-finalize-add-entity-id)
                    ;; The hook must have added an :ID: property
                    (should (string-match-p ":ID:" (buffer-string)))))
              (kill-buffer (current-buffer)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-capture-finalize-hook-skips-existing-id ()
  "Test that the finalize hook does not overwrite an existing ID."
  (let ((temp-file (make-temp-file "test-finalize-existing-" nil ".org"))
        (fixed-id "test-existing-id-12345"))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert (format "* Bob\n:PROPERTIES:\n:ID: %s\n:Role: Mentor\n:END:\n\n"
                            fixed-id)))
          (with-current-buffer (find-file-noselect temp-file)
            (unwind-protect
                (let ((org-capture-mode t))
                  (cl-letf (((symbol-function 'org-scribe-capture-target-file)
                             (lambda (&rest _) temp-file)))
                    (org-scribe--capture-finalize-add-entity-id)
                    ;; The original ID must still be present and unchanged
                    (should (string-match-p fixed-id (buffer-string)))))
              (kill-buffer (current-buffer)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-capture-finalize-hook-inactive-outside-capture ()
  "Test that the finalize hook is a no-op when org-capture-mode is nil."
  (let ((temp-file (make-temp-file "test-finalize-noop-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Charlie\n:PROPERTIES:\n:Role: Ally\n:END:\n\n"))
          (with-current-buffer (find-file-noselect temp-file)
            (unwind-protect
                (let ((org-capture-mode nil))   ; simulate being outside capture
                  (org-scribe--capture-finalize-add-entity-id)
                  ;; No ID should have been added
                  (should-not (string-match-p ":ID:" (buffer-string))))
              (kill-buffer (current-buffer)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; ─────────────────────────────────────────────
;;; --capture-goto-section (H10)
;;; ─────────────────────────────────────────────
;; Note capture files under a top-level Notes heading

;;; ─────────────────────────────────────────────
;; org-scribe--capture-goto-section positions point so a `file+function'
;; capture files entities under the right level-1 section in short-story
;; projects, matching org-scribe--character-heading-p /
;; --location-heading-p / --plot-heading-p (which require exactly that
;; nesting there), while preserving the flat top-level-append behavior
;; novel projects need.

(ert-deftest test-capture-goto-section-short-story-lands-on-existing-heading ()
  "In a short-story buffer, point ends up ON an existing section heading.
`org-capture-set-target-location' only treats a `file+function' target as
\"file under this entry\" when `org-at-heading-p' is true after the
function runs (see org-capture-place-entry); landing anywhere else in the
section would misfile the capture as a flat top-level entry instead."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Characters\n\n** Protagonist: Alice\n:PROPERTIES:\n:Role: Protagonist\n:END:\n")
      (insert "* Plot Outline\n\n** Premise\n")
      (org-scribe--capture-goto-section 'characters)
      (should (org-at-heading-p))
      (should (string= (org-get-heading t t t t) "Characters")))))

(ert-deftest test-capture-goto-section-short-story-creates-missing-section ()
  "When the target section is absent, it is created at the buffer end."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Plot Outline\n\n** Premise\n")
      (org-scribe--capture-goto-section 'setting)
      (should (org-at-heading-p))
      (should (string= (org-get-heading t t t t) "Setting")))))

(ert-deftest test-capture-goto-section-short-story-finds-spanish-alias ()
  "A Spanish short-story project's \"Personajes\" heading is recognized
directly, instead of creating a redundant English \"Characters\" section."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Personajes\n\n** Protagonista: Alicia\n")
      (org-scribe--capture-goto-section 'characters)
      (should (org-at-heading-p))
      (should (string= (org-get-heading t t t t) "Personajes"))
      ;; No duplicate English section was created
      (should-not (string-match-p "\\* Characters" (buffer-string))))))

(ert-deftest test-capture-goto-objects-section-lands-on-existing-heading ()
  "An object capture files under the shipped notes.org template's own
\"Objects\" section, matching what manual/00-feature-index.org already
claims — mirrors `test-capture-goto-section-short-story-lands-on-existing-heading'
for the `characters' section."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Objects\n\n")
      (insert "* Timeline\n\n")
      (org-scribe--capture-goto-objects-section)
      (should (org-at-heading-p))
      (should (string= (org-get-heading t t t t) "Objects")))))

(ert-deftest test-capture-goto-objects-section-creates-missing-section ()
  "When no \"Objects\" section exists yet, it is created at the buffer end,
rather than the object being appended as a stray top-level heading."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Characters\n\n")
      (org-scribe--capture-goto-objects-section)
      (should (org-at-heading-p))
      (should (string= (org-get-heading t t t t) "Objects")))))

(ert-deftest test-capture-goto-objects-section-finds-spanish-alias ()
  "A Spanish short-story project's \"Objetos\" heading (the shipped
notas.org.template's own text) is recognized directly."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Objetos\n\n")
      (org-scribe--capture-goto-objects-section)
      (should (org-at-heading-p))
      (should (string= (org-get-heading t t t t) "Objetos"))
      (should-not (string-match-p "\\* Objects" (buffer-string))))))

(ert-deftest test-capture-goto-timeline-section-lands-on-existing-heading ()
  "A timeline-event capture files under the shipped notes.org template's
own \"Timeline\" section."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Objects\n\n")
      (insert "* Timeline\n\n")
      (org-scribe--capture-goto-timeline-section)
      (should (org-at-heading-p))
      (should (string= (org-get-heading t t t t) "Timeline")))))

(ert-deftest test-capture-goto-timeline-section-creates-missing-section ()
  "When no \"Timeline\" section exists yet, it is created at the buffer end."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Characters\n\n")
      (org-scribe--capture-goto-timeline-section)
      (should (org-at-heading-p))
      (should (string= (org-get-heading t t t t) "Timeline")))))

(ert-deftest test-capture-goto-timeline-section-finds-spanish-alias ()
  "A Spanish short-story project's \"Línea Temporal\" heading (the shipped
notas.org.template's own text) is recognized directly, and does not
create a redundant English \"Timeline\" section."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Línea Temporal\n\n")
      (org-scribe--capture-goto-timeline-section)
      (should (org-at-heading-p))
      (should (string= (org-get-heading t t t t) "Línea Temporal"))
      (should-not (string-match-p "\\* Timeline" (buffer-string))))))

;;; ─────────────────────────────────────────────
;;; Language-aware capture templates (i18n)
;;; ─────────────────────────────────────────────

(ert-deftest test-character-template-spanish ()
  "The character template renders the method's vocabulary in Spanish."
  (let ((template-string (nth 4 (car (org-scribe-character-capture-templates 'es)))))
    (dolist (term '("Nombre del personaje" "Concepto" "Postura" "Fantasma"
                    "Mentira" "Apuro" "Debilidad psicológica" "Debilidad moral"
                    "Deseo" "Necesidad" "Hitos de cambio"))
      (should (string-match-p term template-string)))
    (should-not (string-match-p "Ghost" template-string))
    (should-not (string-match-p "Psychological Weakness" template-string))
    ;; Role *values* stay English in both languages: they are machine-facing
    ;; identifiers read by the entity predicate, not display text.
    (should (string-match-p "Protagonist|Opponent|Ally" template-string))))

(ert-deftest test-location-template-spanish ()
  "The setting template renders the method's vocabulary in Spanish."
  (let ((template-string (nth 4 (car (org-scribe-location-capture-templates 'es)))))
    (dolist (term '("Nombre del escenario" "Eco" "Escollo" "Símbolo" "Descripción"))
      (should (string-match-p term template-string)))
    (should-not (string-match-p "Hazard" template-string))
    (should-not (string-match-p "Geography" template-string))))

(ert-deftest test-object-template-spanish ()
  "Test that the object template renders in Spanish when requested."
  (let ((template-string (nth 4 (car (org-scribe-object-capture-templates 'es)))))
    (should (string-match-p "Simbolismo" template-string))
    (should-not (string-match-p "Symbolism" template-string))))

(ert-deftest test-timeline-template-spanish ()
  "Test that the timeline template renders in Spanish when requested."
  (let ((template-string (nth 4 (car (org-scribe-timeline-capture-templates 'es)))))
    (should (string-match-p "Consecuencias" template-string))
    (should-not (string-match-p "Consequences" template-string))))

(ert-deftest test-plot-thread-template-spanish ()
  "The plot thread template renders in Spanish and files a real thread.
The `%\\2' positional backreference of the previous template is gone:
the type is now prompted straight into the THREAD-TYPE property, which
removes the one place where reordering the prompts silently produced a
thread whose type was its own name."
  (let ((template-string (nth 4 (car (org-scribe-plot-thread-capture-templates 'es)))))
    (dolist (term '("Nombre de la línea" "Postura" "Deseo propio"
                    "Cruces con la línea principal" "Resolución"))
      (should (string-match-p term template-string)))
    ;; THREAD-TYPE is what `org-scribe--plot-heading-p' reads.
    (should (string-match-p ":THREAD-TYPE: %\\^{" template-string))
    (should-not (string-match-p "%\\\\2" template-string))))

(ert-deftest test-capture-templates-follow-project-language ()
  "Test that templates default to `org-scribe-project-language' when no
explicit LANGUAGE argument is given, so a Spanish project's capture UI
is Spanish without the caller having to know about it."
  (cl-letf (((symbol-function 'org-scribe-project-language) (lambda () 'es)))
    (should (string-match-p "Nombre del Personaje"
                            (nth 4 (car (org-scribe-character-capture-templates)))))))

;;; ─────────────────────────────────────────────
;;; Weight prompt
;;; ─────────────────────────────────────────────

(defun org-scribe-test--fill-template (template weight-answer)
  "Expand TEMPLATE with `org-capture-fill-template', answering WEIGHT-ANSWER.
Every other prompt gets a placeholder.  Uses the real expander rather
than a hand-rolled substitution: a home-made `%^{...}' reader mishandles
the |-alternative form and turns harness bugs into apparent template bugs."
  (cl-letf (((symbol-function 'read-string)
             (lambda (prompt &rest _)
               (if (string-match-p "Weight\\|Peso" prompt) weight-answer "x")))
            ((symbol-function 'completing-read)
             (lambda (prompt coll &rest _)
               (cond ((string-match-p "Weight\\|Peso" prompt) weight-answer)
                     ((functionp coll) "x")
                     ((car (all-completions "" coll)))
                     (t "x"))))
            ((symbol-function 'org-id-new) (lambda (&rest _) "test-id-1")))
    (org-capture-fill-template template)))

(ert-deftest test-character-template-prompts-for-weight ()
  "The character capture asks for Weight and files the answer.
Free text, not a |-alternative list: -1 and an empty answer both have to
be typable, and alternatives would make the first one the default."
  (let ((template-string (nth 4 (car (org-scribe-character-capture-templates 'en)))))
    (should (string-match-p ":Weight: %\\^{" template-string))
    (should (string-match-p "-1" template-string))
    (should (member ":Weight: -1"
                    (split-string
                     (org-scribe-test--fill-template template-string "-1")
                     "\n")))))

(ert-deftest test-short-story-character-capture-matches-template ()
  "In a short-story project a captured character gets the template's own
profile, not the novel ficha -- one notes file, one profile shape."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (dolist (lang '(en es))
      (let* ((template (nth 4 (car (org-scribe-character-capture-templates lang))))
             (filled (org-scribe-test--fill-template template "")))
        ;; RET takes the first alternative: it must not be Protagonist,
        ;; or every capture after the first mints another one.
        (should (string-match-p ":Role: %^{[^|}]+|Supporting|" template))
        (should (string-match-p "^:Role: " filled))
        (should (string-match-p "^:Gender:$" filled))
        (should (string-match-p (if (eq lang 'es) "^- Personalidad ::" "^- Personality ::")
                                filled))
        (should-not (string-match-p "Ghost\\|Fantasma\\|Hélice\\|:Stance:\\|:Weight:" filled))))))

(ert-deftest test-short-story-location-capture-matches-template ()
  "In a short-story project a captured location gets the template's own
profile (Description / Important details / Atmosphere), not the Aspects."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (let ((filled (org-scribe-test--fill-template
                   (nth 4 (car (org-scribe-location-capture-templates 'en))) "")))
      (should (string-match-p "^:Time-period:$" filled))
      (should (string-match-p "^- Atmosphere ::$" filled))
      (should-not (string-match-p ":Echo:\\|Hazard" filled)))))

(ert-deftest test-novel-character-capture-keeps-the-ficha ()
  "The novel profile is unchanged."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'novel)))
    (should (string-match-p ":Stance:"
                            (nth 4 (car (org-scribe-character-capture-templates 'en)))))))

(ert-deftest test-plot-thread-template-prompts-for-weight ()
  "The plot-thread capture asks for Weight and files the answer."
  (let ((template-string (nth 4 (car (org-scribe-plot-thread-capture-templates 'en)))))
    (should (string-match-p ":Weight: %\\^{" template-string))
    (should (string-match-p ":Weight: -1"
                            (org-scribe-test--fill-template template-string "-1")))))

(ert-deftest test-weight-prompt-accepts-empty-answer ()
  "Declining the Weight prompt leaves the property blank, not zero.
The blank is what the templates ship too, and
`org-scribe--parse-weight' reads it as unweighted."
  (let* ((template-string (nth 4 (car (org-scribe-character-capture-templates 'en))))
         (filled (org-scribe-test--fill-template template-string "")))
    (should (string-match-p ":Weight: *\n" filled))
    (should-not (string-match-p ":Weight: *0" filled))))

(ert-deftest test-weight-prompt-localized ()
  "The Weight prompt is translated, while the property key stays English.
`org-scribe--get-entity-weight' looks up the literal \"Weight\", so a
translated key would make the property invisible to the timeline."
  (let ((template-string (nth 4 (car (org-scribe-character-capture-templates 'es)))))
    (should (string-match-p ":Weight: %\\^{Peso" template-string))))


;;; ─────────────────────────────────────────────
;;; The design notebook as capture target (M5)
;;; ─────────────────────────────────────────────

(ert-deftest test-capture-functions-defined ()
  "Every public capture command and the single target-file function exist;
the per-entity file functions are gone."
  (dolist (fn '(org-scribe-capture-to-file org-scribe-capture-character
                org-scribe-capture-location org-scribe-capture-object
                org-scribe-capture-timeline org-scribe-capture-plot-thread
                org-scribe-capture-target-file
                org-scribe--create-notebook-file))
    (should (fboundp fn)))
  (dolist (fn '(org-scribe-capture-character-file org-scribe-capture-location-file
                org-scribe-capture-object-file org-scribe-capture-timeline-file
                org-scribe-capture-plot-thread-file org-scribe--capture-entity-file
                org-scribe--create-capture-file org-scribe--create-plot-file
                org-scribe--create-short-story-notes-file
                org-scribe--create-novel-capture-file))
    (should-not (fboundp fn))))

(defun test-capture--notebook (type language)
  "Return the text of a notebook created for TYPE and LANGUAGE."
  (let ((file (make-temp-file "test-notebook-" nil ".org")))
    (unwind-protect
        (progn
          (delete-file file)
          (org-scribe--create-notebook-file file type language)
          (with-temp-buffer (insert-file-contents file) (buffer-string)))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest test-create-notebook-file-novel-has-parts-and-sections ()
  "A novel notebook nests the five capturable sections inside their parts."
  (let ((text (test-capture--notebook 'novel 'en)))
    (should (string-match-p "^#\\+TITLE: " text))
    (should (string-match-p "^#\\+AUTHOR: " text))
    (should (string-match-p "^\\* Plot\n\n\\*\\* Plot Threads\n" text))
    (should (string-match-p "^\\* Cast\n\n\\*\\* Characters\n" text))
    (should (string-match-p "^\\* World\n\n\\*\\* Setting\n\n\\*\\* Objects\n\n\\*\\* Timeline\n" text))))

(ert-deftest test-create-notebook-file-short-story-has-level-one-sections ()
  "A short-story notebook has the same sections at level 1, in no part."
  (let ((text (test-capture--notebook 'short-story 'en)))
    (dolist (heading '("Characters" "Setting" "Plot Threads" "Objects" "Timeline"))
      (should (string-match-p (format "^\\* %s$" heading) text)))
    (should-not (string-match-p "^\\*\\* " text))
    (should-not (string-match-p "^\\* \\(Cast\\|World\\|Plot\\)$" text))))

(ert-deftest test-create-notebook-file-carries-no-placeholders-or-prose ()
  "Only headings and the file header: no entity, drawer or comment."
  (dolist (type '(novel short-story))
    (let ((text (test-capture--notebook type 'en)))
      (should-not (string-match-p "^:PROPERTIES:\\|^#[^+]\\|^- " text)))))

(ert-deftest test-create-notebook-file-is-spanish-for-a-spanish-project ()
  "Headings come from the Spanish pack, and no English section is written."
  (let ((novel (test-capture--notebook 'novel 'es))
        (story (test-capture--notebook 'short-story 'es)))
    (should (string-match-p "^\\* Reparto\n\n\\*\\* Personajes\n" novel))
    (should (string-match-p "^\\* Mundo\n\n\\*\\* Ambientación\n" novel))
    (should (string-match-p "^\\* Trama\n\n\\*\\* Hilos de la Trama\n" novel))
    (should (string-match-p "^\\* Personajes$" story))
    (dolist (text (list novel story))
      (should-not (string-match-p "^\\*+ \\(Characters\\|Setting\\|Cast\\|World\\)" text)))))

(ert-deftest test-create-notebook-file-sections-are-recognized ()
  "What it writes is what the section finder matches, in both layouts."
  (dolist (type '(novel short-story))
    (dolist (lang '(en es))
      (with-temp-buffer
        (org-mode)
        (insert (test-capture--notebook type lang))
        (dolist (key org-scribe--notebook-sections)
          (should (org-scribe--find-section-heading key)))))))

(ert-deftest test-capture-target-file-finds-the-design-file-in-any-project-type ()
  "Every capture, for a novel or a short story, goes to the one design file."
  (dolist (type '(novel short-story))
    (let ((dir (make-temp-file "test-target-" t)))
      (unwind-protect
          (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () dir))
                    ((symbol-function 'org-scribe-project-type) (lambda () type)))
            (let ((design (expand-file-name "diseno.org" dir)))
              (with-temp-file design (insert "* Reparto\n"))
              (should (string= design (org-scribe-capture-target-file)))))
        (delete-directory dir t)))))

(ert-deftest test-capture-target-file-create-if-missing-uses-the-project-language ()
  "A missing notebook is created under the project language's file name."
  (dolist (spec '((es . "diseno.org") (en . "design.org")))
    (let ((dir (make-temp-file "test-target-create-" t)))
      (unwind-protect
          (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () dir))
                    ((symbol-function 'org-scribe-project-type) (lambda () 'novel))
                    ((symbol-function 'org-scribe-project-language) (lambda () (car spec))))
            (let ((result (org-scribe-capture-target-file t)))
              (should (string= result (expand-file-name (cdr spec) dir)))
              (should (file-exists-p result))))
        (delete-directory dir t)))))

(ert-deftest test-capture-target-file-uses-org-scribe-root-not-project-current ()
  "The notebook is resolved under `org-scribe-project-root', not the VC root."
  (let ((project-root (make-temp-file "test-capture-project-" t))
        (git-root (make-temp-file "test-capture-git-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () project-root))
                  ((symbol-function 'org-scribe-project-type) (lambda () 'novel))
                  ((symbol-function 'project-current)
                   (lambda (&rest _) `(vc Git . ,git-root))))
          (let ((result (org-scribe-capture-target-file)))
            (should (string-prefix-p project-root result))
            (should-not (string-prefix-p git-root result))))
      (delete-directory project-root t)
      (delete-directory git-root t))))

;;; goto-section: any level, created under its part

(ert-deftest test-capture-goto-section-lands-on-existing-heading-at-any-level ()
  (dolist (text '("* Characters\n** Alice\n* Plot\n"
                  "* Cast\n** Characters\n*** Alice\n* Plot\n"))
    (with-temp-buffer
      (org-mode)
      (insert text)
      (org-scribe--capture-goto-section 'characters)
      (should (org-at-heading-p))
      (should (string= "Characters" (org-get-heading t t t t))))))

(ert-deftest test-capture-goto-section-creates-missing-section-under-its-part ()
  "With the part present, the section is its last child, one level deeper."
  (with-temp-buffer
    (org-mode)
    (insert "* Cast\n** Cast Check\n*** Coverage\n* World\n** Setting\n")
    (org-scribe--capture-goto-section 'characters)
    (should (org-at-heading-p))
    (should (string= "Characters" (org-get-heading t t t t)))
    (should (= 2 (org-current-level)))
    (org-up-heading-safe)
    (should (string= "Cast" (org-get-heading t t t t)))
    ;; last child of Cast, before World
    (should (string-match-p "\\*\\*\\* Coverage\n\\*\\* Characters\n\\* World"
                            (buffer-string)))))

(ert-deftest test-capture-goto-section-creates-at-level-one-when-no-part ()
  "With no part in the file (the short-story shape) the section is level 1."
  (with-temp-buffer
    (org-mode)
    (insert "* Plot Threads\n\n")
    (org-scribe--capture-goto-section 'setting)
    (should (= 1 (org-current-level)))
    (should (string= "Setting" (org-get-heading t t t t)))))

(ert-deftest test-capture-goto-section-uses-project-language-spelling ()
  "A missing section is written in the project's language, and an existing
one in another language's spelling is reused rather than duplicated."
  (cl-letf (((symbol-function 'org-scribe-project-language) (lambda () 'es)))
    (with-temp-buffer
      (org-mode)
      (insert "* Reparto\n")
      (org-scribe--capture-goto-section 'characters)
      (should (string= "Personajes" (org-get-heading t t t t)))
      (should (= 2 (org-current-level))))
    (with-temp-buffer
      (org-mode)
      (insert "* Cast\n** Characters\n")
      (org-scribe--capture-goto-section 'characters)
      (should (string= "Characters" (org-get-heading t t t t)))
      (should-not (string-match-p "Personajes" (buffer-string))))))

(ert-deftest test-capture-goto-section-ignores-narrowing ()
  (with-temp-buffer
    (org-mode)
    (insert "* Intro\n* Characters\n")
    (goto-char (point-min))
    (narrow-to-region (point-min) (line-end-position))
    (org-scribe--capture-goto-section 'characters)
    (should (string= "Characters" (org-get-heading t t t t)))))

(ert-deftest test-capture-goto-notes-section-is-the-scratchpad-part ()
  "General notes go to the Scratchpad part, created at level 1 if missing."
  (with-temp-buffer
    (org-mode)
    (insert "* Cast\n** Characters\n*** Notes\n* Scratchpad\n** Loose ideas\n")
    (org-scribe--capture-goto-notes-section)
    (should (string= "Scratchpad" (org-get-heading t t t t))))
  (with-temp-buffer
    (org-mode)
    (insert "* Cast\n")
    (org-scribe--capture-goto-notes-section)
    (should (= 1 (org-current-level)))
    (should (string= "Scratchpad" (org-get-heading t t t t)))))

;;; End to end through the real `org-capture'

(defmacro test-capture--with-notebook (language type text &rest body)
  "Run BODY in a temp project of TYPE and LANGUAGE whose design file holds TEXT.
Binds `notebook' to the file."
  (declare (indent 3))
  `(let* ((root (make-temp-file "test-capture-e2e-" t))
          (manuscript (org-scribe-lang-file 'manuscript-novel ,language))
          (notebook (expand-file-name (org-scribe-lang-file 'design ,language) root)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".org-scribe-project" root)
             (insert (format "# Writing project: T\n# Type: %s\n# Language: %s\n" ,type ,language)))
           (with-temp-file (expand-file-name manuscript root) (insert "* Act\n"))
           (with-temp-file notebook (insert ,text))
           (let ((default-directory (file-name-as-directory root)))
             (org-scribe-project-type-cache-clear root)
             ,@body))
       (when (get-file-buffer notebook)
         (with-current-buffer (get-file-buffer notebook) (set-buffer-modified-p nil))
         (kill-buffer (get-file-buffer notebook)))
       (delete-directory root t))))

(defun test-capture--capture-character ()
  "Capture a character through the real template, answering every prompt."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Alice"))
            ((symbol-function 'completing-read) (lambda (&rest _) "Alice")))
    (let ((org-capture-templates (org-scribe-character-capture-templates)))
      (org-capture nil "c")
      (org-capture-finalize))))

(ert-deftest test-capture-character-lands-at-level-three-under-characters ()
  "A captured novel character is a level-3 child of `** Characters' in Cast,
with its sub-headings shifted to level 4, and is recognized as a character."
  (test-capture--with-notebook 'en 'novel "* Cast\n** Characters\n*** Bob\n* World\n"
    (test-capture--capture-character)
    (with-current-buffer (find-file-noselect notebook)
      (revert-buffer t t)
      (goto-char (point-min))
      (re-search-forward "^\\*+ Alice$")
      (should (= 3 (org-current-level)))
      (should (string= "Characters" (save-excursion (org-up-heading-safe)
                                                    (org-get-heading t t t t))))
      (should (org-scribe--character-heading-p))
      (should (string-match-p "^\\*\\*\\*\\* " (buffer-substring (point) (point-max))))
      ;; before the next part
      (should (< (point) (save-excursion (re-search-forward "^\\* World")))))))

(ert-deftest test-capture-character-creates-its-section-under-the-part ()
  "With no `Characters' section, it is created under Cast first."
  (test-capture--with-notebook 'en 'novel "* Cast\n* World\n"
    (test-capture--capture-character)
    (with-current-buffer (find-file-noselect notebook)
      (revert-buffer t t)
      (goto-char (point-min))
      (re-search-forward "^\\*+ Alice$")
      (should (= 3 (org-current-level)))
      (org-up-heading-safe)
      (should (string= "Characters" (org-get-heading t t t t)))
      (should (= 2 (org-current-level)))
      (org-up-heading-safe)
      (should (string= "Cast" (org-get-heading t t t t))))))

(ert-deftest test-capture-character-spanish-project-gets-spanish-sections ()
  (test-capture--with-notebook 'es 'novel "* Reparto\n"
    (test-capture--capture-character)
    (with-current-buffer (find-file-noselect notebook)
      (revert-buffer t t)
      (should (string-match-p "^\\*\\* Personajes$" (buffer-string)))
      (should-not (string-match-p "Characters" (buffer-string))))))

(ert-deftest test-capture-character-in-a-short-story-lands-at-level-two ()
  (test-capture--with-notebook 'en 'short-story "* Characters\n** Bob\n* Plot Threads\n"
    (test-capture--capture-character)
    (with-current-buffer (find-file-noselect notebook)
      (revert-buffer t t)
      (goto-char (point-min))
      (re-search-forward "^\\*+ Alice$")
      (should (= 2 (org-current-level)))
      (should (org-scribe--character-heading-p)))))

(ert-deftest test-capture-note-is-filed-in-the-scratchpad-and-gets-no-id ()
  "A general note lands under the Scratchpad part, not inside a character
profile's own `Notes' sub-heading, and is not given an entity ID."
  (test-capture--with-notebook 'en 'novel
      "* Cast\n** Characters\n*** Alice\n**** Notes\n- profile\n* Scratchpad\n** Loose ideas\n"
    (let ((org-capture-templates (org-scribe-capture-templates)))
      (org-capture nil "w")
      (insert "the letter is forged")
      (org-capture-finalize))
    (with-current-buffer (find-file-noselect notebook)
      (revert-buffer t t)
      (goto-char (point-min))
      (re-search-forward "the letter is forged")
      (org-back-to-heading t)
      (should (= 2 (org-current-level)))
      (should (string= "Scratchpad" (save-excursion (org-up-heading-safe)
                                                    (org-get-heading t t t t))))
      (should-not (org-entry-get nil "ID")))))

;;; ─────────────────────────────────────────────
;;; Run tests
;;; ─────────────────────────────────────────────

(defun org-scribe-capture-run-tests ()
  "Run all capture system tests."
  (interactive)
  (ert "^test-capture-\\|^test-create-\\|^test-character-\\|^test-location-\\|^test-object-\\|^test-timeline-"))

(provide 'test-capture)

;;; test-capture.el ends here
