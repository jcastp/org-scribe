;;; org-scribe-capture.el --- Capture system for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Project-aware capture system for writing notes.
;; Automatically determines the appropriate notes file based on project structure.

;;; Code:

(require 'cl-lib)
(require 'org-capture)
(require 'project)
(require 'org-scribe-core)
(require 'org-scribe-config)

;;; File Creation Helpers

(defun org-scribe--file-header (title &optional startup)
  "Return a standard org file header string.
Includes #+TITLE (TITLE), #+AUTHOR (current user), #+DATE (today).
When STARTUP is a non-nil string, appends a #+STARTUP line."
  (concat
   (format "#+TITLE: %s\n" title)
   (format "#+AUTHOR: %s\n" user-full-name)
   (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d"))
   (when startup (format "#+STARTUP: %s\n" startup))
   "\n"))

(defun org-scribe--create-plot-file (filepath is-short-story &optional language)
  "Create a basic plot file for captures.
FILEPATH is the path where the file should be created.
IS-SHORT-STORY determines the structure.
LANGUAGE, if non-nil, overrides `org-scribe-project-language' for
content generation."
  (let ((org-scribe-message-language (or language (org-scribe-project-language))))
    (with-temp-file filepath
      (insert (org-scribe--file-header (org-scribe-msg 'capture-title-plot-structure) "overview"))
      (if is-short-story
          (progn
            (insert (format "* %s\n\n" (org-scribe-msg 'capture-plot-outline)))
            (insert (format "** %s\n\n" (org-scribe-msg 'capture-plot-premise)))
            (insert (format "** %s\n\n" (org-scribe-msg 'capture-plot-setup)))
            (insert (format "** %s\n\n" (org-scribe-msg 'capture-plot-central-conflict)))
            (insert (format "** %s\n\n" (org-scribe-msg 'capture-plot-resolution)))
            (insert (format "* %s\n\n" (org-scribe-msg 'capture-plot-threads)))
            (insert (format "%s\n\n" (org-scribe-msg 'capture-plot-threads-hint-short))))
        (progn
          (insert (format "* %s\n\n%s\n\n"
                          (org-scribe-msg 'capture-plot-premise)
                          (org-scribe-msg 'capture-plot-premise-hint)))
          (insert (format "* %s\n\n" (org-scribe-msg 'capture-plot-main-plot)))
          (insert (format "** %s\n\n" (org-scribe-msg 'capture-plot-central-conflict)))
          (insert (format "** %s\n\n" (org-scribe-msg 'capture-plot-main-dramatic-question)))
          (insert (format "* %s\n\n" (org-scribe-msg 'capture-plot-subplots)))
          (insert (format "* %s\n\n" (org-scribe-msg 'capture-plot-threads)))
          (insert (format "%s\n\n" (org-scribe-msg 'capture-plot-threads-hint-novel))))))))

(defun org-scribe--create-short-story-notes-file (filepath &optional language)
  "Create a comprehensive notes.org file for short story projects.
FILEPATH is the path where the file should be created.
LANGUAGE, if non-nil, overrides `org-scribe-project-language' for
content generation."
  (let ((title (file-name-base (directory-file-name (file-name-directory filepath))))
        (org-scribe-message-language (or language (org-scribe-project-language))))
    (with-temp-file filepath
      (insert (org-scribe--file-header (org-scribe-msg 'capture-title-project-notes title) "overview"))
      (insert (format "* %s\n\n" (org-scribe-msg 'capture-ss-characters)))
      (insert (format "** %s\n" (org-scribe-msg 'capture-ss-protagonist-name)))
      ;; :Role: is deliberately English regardless of LANGUAGE (see
      ;; docs/glossary.org: entity property values for Role are English in
      ;; both template sets), matching the shipped short-story templates
      ;; this recovery file stands in for.  No :NAME: -- the heading is the
      ;; name; see `org-scribe--entity-name-at-point'.
      (insert ":PROPERTIES:\n:Role: Protagonist\n:Age:\n:Gender:\n:END:\n\n")
      (insert (format "- %s\n- %s\n- %s\n\n"
                      (org-scribe-msg 'capture-ss-personality)
                      (org-scribe-msg 'capture-ss-goal)
                      (org-scribe-msg 'capture-ss-conflict)))
      (insert (format "* %s\n\n" (org-scribe-msg 'capture-plot-outline)))
      (insert (format "** %s\n\n** %s\n\n** %s\n\n** %s\n\n"
                      (org-scribe-msg 'capture-plot-premise)
                      (org-scribe-msg 'capture-plot-setup)
                      (org-scribe-msg 'capture-plot-central-conflict)
                      (org-scribe-msg 'capture-plot-resolution)))
      (insert (format "* %s\n\n" (org-scribe-msg 'capture-ss-setting)))
      (insert (format "** %s\n\n" (org-scribe-msg 'capture-ss-main-locations)))
      (insert (format "** %s\n\n" (org-scribe-msg 'capture-ss-locations)))
      (insert (format "* %s\n\n" (org-scribe-msg 'capture-ss-objects)))
      (insert (format "* %s\n\n" (org-scribe-msg 'capture-ss-timeline)))
      (insert (format "* %s\n\n" (org-scribe-msg 'capture-ss-research)))
      (insert (format "* %s\n\n" (org-scribe-msg 'capture-ss-revision-notes)))
      (insert (format "* %s\n\n" (org-scribe-msg 'capture-ss-random-ideas))))))

(defun org-scribe--create-novel-capture-file (filepath content-type &optional language)
  "Create an individual capture file for novel projects.
FILEPATH is the path where the file should be created.
CONTENT-TYPE is \\='characters, \\='locations, \\='objects, \\='timeline, or \\='notes.
LANGUAGE, if non-nil, overrides `org-scribe-project-language' for
content generation."
  (let ((org-scribe-message-language (or language (org-scribe-project-language))))
    (let ((titles (list (cons 'characters (org-scribe-msg 'capture-title-characters))
                        (cons 'locations  (org-scribe-msg 'capture-title-locations))
                        (cons 'objects    (org-scribe-msg 'capture-title-objects))
                        (cons 'timeline   (org-scribe-msg 'capture-title-timeline))
                        (cons 'notes      (org-scribe-msg 'capture-title-notes)))))
      (with-temp-file filepath
        (insert (org-scribe--file-header (alist-get content-type titles (org-scribe-msg 'capture-title-notes))))
        (when (eq content-type 'notes)
          (insert (format "* %s\n\n" (org-scribe-msg 'capture-notes-heading))))))))

(defun org-scribe--create-capture-file (filepath project-type content-type &optional language)
  "Create a capture target file based on project type.
FILEPATH is the path to create.
PROJECT-TYPE is 'novel, 'short-story, or 'unknown.
CONTENT-TYPE is 'characters, 'locations, 'objects, 'timeline, 'plot, or 'notes.
LANGUAGE, if non-nil, overrides `org-scribe-project-language' for
content generation.

For plot threads, creates a plot-structured file regardless of project type.
For short stories, creates notes.org with all standard headings.
For novels, creates individual files."
  (let ((target-dir (file-name-directory filepath)))
    (unless (file-directory-p target-dir)
      (make-directory target-dir t)))

  (cond
   ;; Plot threads get their own structure regardless of project type
   ((eq content-type 'plot)
    (org-scribe--create-plot-file filepath (eq project-type 'short-story) language))
   ;; Short story: create comprehensive notes.org
   ((eq project-type 'short-story)
    (org-scribe--create-short-story-notes-file filepath language))
   ;; Novel or unknown: create individual file
   (t
    (org-scribe--create-novel-capture-file filepath content-type language))))

;;; Capture Target File Detection

(defun org-scribe--capture-entity-file (concept &optional create-if-missing)
  "Find the appropriate capture file for CONCEPT.
CONCEPT is a key of the `:files' section of `lang/org-scribe-lang.el',
such as `characters' or `plot' -- the same symbol
`org-scribe-project-structure' resolves under the matching
`:CONCEPT-file' key, and also passed through as CONTENT-TYPE to
`org-scribe--create-capture-file' on the create path.  If
CREATE-IF-MISSING is non-nil, create the file if it doesn't exist.

For short stories, returns notes.org (or notas.org, ...) in the
project root.  For novels, resolves CONCEPT's own file
(objects/characters.org, objetos/personajes.org, ...) via
`org-scribe--resolve', trying every registered language's spelling.

When none of those candidates exists yet, the file to create is chosen by
`org-scribe-project-language', not hardcoded to English: a Spanish
project gets its own spelling (notas.org, or objetos/personajes.org),
not the English one.  This path is only reached for a project whose
consolidated notes file (or, for a novel, the relevant entity file)
does not exist on disk yet — normal project creation always ships it, so
in ordinary use `org-scribe--resolve' above already finds a match — but
a project missing it (an older project, one assembled by hand, or a
deliberately deleted file) must still get the file named for its own
language, not the tool's fallback default."
  (let* ((project-dir (or (org-scribe-project-root)
                         (file-name-directory (or (buffer-file-name) default-directory))))
         (project-type (org-scribe-project-type))
         (target
          (cond
           ;; Short story: use notes.org
           ((eq project-type 'short-story)
            (or (org-scribe--resolve project-dir 'notes-short)
                (expand-file-name (org-scribe-lang-file 'notes-short) project-dir)))
           ;; Novel or unknown: resolve CONCEPT's own file
           (t
            (or (org-scribe--resolve project-dir concept)
                (expand-file-name (org-scribe-lang-file concept) project-dir))))))

    (when (and create-if-missing (not (file-exists-p target)))
      (org-scribe--create-capture-file target project-type concept))

    target))

(defun org-scribe-capture-character-file (&optional create-if-missing)
  "Determine the appropriate file for character captures.
For novels: uses the `characters' concept's own file.  For short
stories: uses the notes file.  If CREATE-IF-MISSING is non-nil, create
the file if it doesn't exist.  See `org-scribe--capture-entity-file'."
  (org-scribe--capture-entity-file 'characters create-if-missing))

(defun org-scribe-capture-location-file (&optional create-if-missing)
  "Determine the appropriate file for location captures.
See `org-scribe-capture-character-file'; uses the `locations' concept."
  (org-scribe--capture-entity-file 'locations create-if-missing))

(defun org-scribe-capture-object-file (&optional create-if-missing)
  "Determine the appropriate file for object captures.
See `org-scribe-capture-character-file'; uses the `objects' concept."
  (org-scribe--capture-entity-file 'objects create-if-missing))

(defun org-scribe-capture-timeline-file (&optional create-if-missing)
  "Determine the appropriate file for timeline captures.
See `org-scribe-capture-character-file'; uses the `timeline' concept."
  (org-scribe--capture-entity-file 'timeline create-if-missing))

(defun org-scribe-capture-plot-thread-file (&optional create-if-missing)
  "Determine the appropriate file for plot thread captures.
See `org-scribe-capture-character-file'; uses the `plot' concept."
  (org-scribe--capture-entity-file 'plot create-if-missing))

;;; Capture Target Positioning (short-story section nesting)
;;
;; Novel projects keep characters/locations as flat top-level headings in
;; their own dedicated file, so appending at the end of the buffer (the
;; plain `file' capture target) is correct.  Short-story projects instead
;; nest these entities as level-2 headings under a level-1 section of
;; notes.org ("* Characters", "* Setting") — see the shipped template and
;; `org-scribe--character-heading-p' / `org-scribe--location-heading-p',
;; which only recognize entities in that position.  These functions move
;; point to the end of the right section (creating it if missing) so
;; `file+function' capture targets file new entities where the linking
;; predicates expect to find them, instead of as stray top-level headings
;; the getters can never see.

(defun org-scribe--capture-goto-section (section-key)
  "Move point so a `file+function' capture files as a child of SECTION-KEY.
SECTION-KEY is a symbol such as \\='characters or \\='setting (see
`org-scribe--section-heading-aliases').  In short-story projects, finds
the section by any of its localized aliases (creating it, using the
English alias, at the end of the buffer if none is present) and leaves
point ON that heading line — `org-capture-set-target-location' detects
this via `org-at-heading-p' and treats the capture as filed under the
current entry, appending it at the end of the section's subtree at one
level deeper (see `org-capture-place-entry').
In novel projects (or outside a project), moves to the end of the buffer
on a blank line instead, so `org-at-heading-p' is false there and the
entry is appended as a flat top-level heading — preserving the original
plain `file' target's behavior for novel projects, whose entity files are
not sectioned this way."
  (if (eq (org-scribe-project-type) 'short-story)
      (let* ((aliases (alist-get section-key org-scribe--section-heading-aliases))
             (regexp (concat "^\\* \\(?:"
                             (mapconcat #'regexp-quote aliases "\\|")
                             "\\)[ \t]*$")))
        (goto-char (point-min))
        (if (re-search-forward regexp nil t)
            (forward-line 0)
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (format "* %s\n" (car aliases)))
          (forward-line -1)))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))))

(defun org-scribe--capture-goto-characters-section ()
  "Move point to where a new character capture should be filed.
See `org-scribe--capture-goto-section'."
  (org-scribe--capture-goto-section 'characters))

(defun org-scribe--capture-goto-setting-section ()
  "Move point to where a new location capture should be filed.
See `org-scribe--capture-goto-section'."
  (org-scribe--capture-goto-section 'setting))

(defun org-scribe--capture-goto-plot-threads-section ()
  "Move point to where a new plot thread capture should be filed.
For short stories, nests under the localized \"Plot Threads\" section
(matching whichever alias the project's own template already uses,
instead of the previous hardcoded English literal which could create a
redundant duplicate section in Spanish projects).  For novels, files as
a flat top-level heading, matching `org-scribe--plot-heading-p', which
requires level 1 there.  See `org-scribe--capture-goto-section'."
  (org-scribe--capture-goto-section 'plot-threads))

(defun org-scribe--capture-goto-objects-section ()
  "Move point to where a new object capture should be filed.
In short stories, nests under the localized \"Objects\" section of
notes.org.  Unlike characters/setting/plot-threads, no entity predicate
reads this section (objects are not a linked entity type in this
package), so this exists only so a captured object lands under the
section `manual/00-feature-index.org' already claims it does, instead of
appended as a stray heading at the end of the buffer.  See
`org-scribe--capture-goto-section'."
  (org-scribe--capture-goto-section 'objects))

(defun org-scribe--capture-goto-timeline-section ()
  "Move point to where a new timeline-event capture should be filed.
In short stories, nests under the localized \"Timeline\" section of
notes.org.  See `org-scribe--capture-goto-objects-section', which this
mirrors exactly, and `org-scribe--capture-goto-section'."
  (org-scribe--capture-goto-section 'timeline))

(defun org-scribe-capture-target-file (&optional create-if-missing)
  "Determine the appropriate notes file for org-capture in writing environment.
Uses `org-scribe-project-root' to find the project base directory.
Tries, in order:
1. The novel-subdir notes file (notes/notes.org, notas/notas.org, ...),
   whichever registered language's spelling exists (see
   `org-scribe--resolve').
2. \"novel-notes.org\" in the project root -- a legacy fallback, see below.
3. The short-story notes file (notes.org, notas.org, ...), whichever
   registered language's spelling exists.
4. The current buffer.
5. The project's own language's notes file name -- only reached when
   CREATE-IF-MISSING is non-nil and none of the above exist.

If CREATE-IF-MISSING is non-nil, create the first-priority file that
doesn't exist, named for `org-scribe-project-language' rather than
hardcoded to English -- mirroring `org-scribe--capture-entity-file''s
own fallback naming.

Priority 2 is a legacy fallback.  Novel projects created by older
versions shipped a \"novel-notes.org\" stub as an org-remark annotation
sink; the template was removed when org-remark support was dropped.
New projects always match priority 1, so this branch is
unreachable for them, but it is kept so that a pre-0.5.3 project whose
author put real content in that file still captures into it instead of
appearing to lose it.  This is also the reason the editing-mode right
pane routes through this function rather than naming a file directly
\(see `org-scribe--editing-right-panel-file').

Priority 3's short-story shape exists because a Spanish short-story
project ships its consolidated notes file as root-level \"notas.org\",
not a \"notas/\" subdirectory - without it, a fresh Spanish
short-story project matched no earlier priority and every general note
capture silently fell through to whatever buffer the capture was
invoked from instead of notas.org."
  (let* ((project-dir (or (org-scribe-project-root)
                         (file-name-directory (or (buffer-file-name) default-directory))))
         (novel-notes (expand-file-name "novel-notes.org" project-dir))
         (target (or (org-scribe--resolve project-dir 'notes-novel)
                     (and (file-exists-p novel-notes) novel-notes)
                     (org-scribe--resolve project-dir 'notes-short)
                     (buffer-file-name)
                     (expand-file-name (org-scribe-lang-file 'notes-short) project-dir))))
    (when (and create-if-missing
               (not (file-exists-p target)))
      (let ((target-dir (file-name-directory target)))
        (unless (file-directory-p target-dir)
          (make-directory target-dir t)))
      (let ((org-scribe-message-language (org-scribe-project-language)))
        (with-temp-file target
          (insert (format "#+TITLE: %s\n" (org-scribe-msg 'capture-title-notes)))
          (insert (format "#+AUTHOR: %s\n" user-full-name))
          (insert (format "#+EMAIL: %s\n\n" user-mail-address))
          (insert (format "* %s\n" (org-scribe-msg 'capture-notes-heading))))))
    target))

;;; Capture Templates
;;
;; The template lists below were plain `defvar's until the templates
;; became language-aware (so a Spanish project's capture UI shows
;; Spanish prompts/headings instead of always English).  Org capture
;; templates are read once per capture session from a plain list, not
;; re-evaluated per keystroke, so they cannot be `defvar's computed
;; once at load time if their strings must depend on the project
;; that's active when the capture command runs.  Each is now a
;; function taking an optional LANGUAGE override; callers rebuild the
;; list on every capture invocation via `org-scribe--run-capture'.

(defun org-scribe-capture-templates (&optional language)
  "Return capture templates specific to the writing environment.
LANGUAGE, if non-nil, overrides `org-scribe-project-language'."
  (let ((org-scribe-message-language (or language (org-scribe-project-language))))
    `(("w" ,(org-scribe-msg 'capture-writing-note-name) entry
       (file+headline org-scribe-capture-target-file ,(org-scribe-msg 'capture-notes-heading))
       "** TODO %?\n  %U\n  %i"
       :empty-lines 1))))

(defun org-scribe-character-capture-templates (&optional language)
  "Return capture templates for character profiles.
LANGUAGE, if non-nil, overrides `org-scribe-project-language'.

Files the ficha completa of the method: the causal chain
Ghost -> Lie -> Weakness -> Desire/Need, plus Plan, Aspects and change
milestones.  The heading is the character's *name*; the role lives in the
`Role' property, which is what `org-scribe--character-heading-p' reads, so
renaming a character later never unlinks them.

Built with `concat' rather than one `format' with forty positional
arguments: the previous shape made every field addition a re-count of the
argument list, and a mismatch produced a plausible-looking template with
the labels silently shifted by one."
  (let* ((org-scribe-message-language (or language (org-scribe-project-language))))
    (cl-flet ((m (key) (org-scribe-msg key)))
      `(("c" ,(m 'capture-char-name) entry
         (file+function org-scribe-capture-character-file
                        org-scribe--capture-goto-characters-section)
         ,(concat
           "* %^{" (m 'capture-char-name-prompt) "}\n"
           ":PROPERTIES:\n"
           ":ID: %(org-id-new)\n"
           ":Role: %^{" (m 'capture-char-role-prompt) "}\n"
           ":Stance:\n"
           ":Goal:\n"
           ":Age: %^{" (m 'capture-char-age-prompt) "}\n"
           ":Occupation: %^{" (m 'capture-char-occupation-prompt) "}\n"
           ":First-appearance: %^{" (m 'capture-char-first-appearance-prompt) "}\n"
           ":Weight: %^{" (m 'capture-char-weight-prompt) "}\n"
           ":RelationshipsData:\n"
           ":END:\n\n"
           "- " (m 'capture-char-concept) "\n"
           "  # " (m 'capture-char-concept-hint) "\n"
           "- " (m 'capture-char-stance) "\n"
           "  # " (m 'capture-char-stance-hint) "\n"
           "- " (m 'capture-char-ghost) "\n"
           "  # " (m 'capture-char-ghost-hint) "\n"
           "- " (m 'capture-char-lie) "\n"
           "  # " (m 'capture-char-lie-hint) "\n"
           "- " (m 'capture-char-trouble) "\n"
           "  # " (m 'capture-char-trouble-hint) "\n"
           "- " (m 'capture-char-psych-weakness) "\n"
           "- " (m 'capture-char-moral-weakness) "\n"
           "  # " (m 'capture-char-moral-weakness-hint) "\n\n"
           "** " (m 'capture-char-desire) "\n\n"
           "# " (m 'capture-char-desire-hint) "\n\n"
           "- " (m 'capture-char-goal) "\n"
           "- " (m 'capture-char-motivation) "\n"
           "- " (m 'capture-char-conflict) "\n\n"
           "** " (m 'capture-char-need) "\n\n"
           "# " (m 'capture-char-need-hint) "\n\n"
           "- " (m 'capture-char-goal) "\n"
           "- " (m 'capture-char-motivation) "\n"
           "- " (m 'capture-char-conflict) "\n\n"
           "** " (m 'capture-char-plan-heading) "\n\n"
           "- " (m 'capture-char-plan) "\n"
           "  # " (m 'capture-char-plan-hint) "\n"
           "- " (m 'capture-char-fallback-plan) "\n\n"
           "** " (m 'capture-char-aspects) "\n\n"
           "# " (m 'capture-char-aspects-hint) "\n\n"
           "- " (m 'capture-char-aspect) "\n"
           "- " (m 'capture-char-aspect) "\n"
           "- " (m 'capture-char-relationship) "\n"
           "- " (m 'capture-char-possession) "\n\n"
           "** " (m 'capture-char-backstory) "\n\n"
           "# " (m 'capture-char-backstory-hint) "\n\n"
           "%?\n\n"
           "** " (m 'capture-char-milestones) "\n\n"
           "# " (m 'capture-char-milestones-hint) "\n\n"
           (m 'capture-char-milestone-header) "\n"
           "|---|\n\n"
           "** " (m 'capture-char-notes) "\n")
         :empty-lines 1)))))

(defun org-scribe-location-capture-templates (&optional language)
  "Return capture templates for setting profiles.
LANGUAGE, if non-nil, overrides `org-scribe-project-language'.

Files the method's four setting Aspects — Echo, Hazard, Character and
Symbol — rather than the gazetteer fields (climate, population, terrain)
of the previous model.  A setting with no Aspect is scenery, and Pass 5
of the revision marks scenery for cutting."
  (let* ((org-scribe-message-language (or language (org-scribe-project-language))))
    (cl-flet ((m (key) (org-scribe-msg key)))
      `(("l" ,(m 'capture-loc-name) entry
         (file+function org-scribe-capture-location-file
                        org-scribe--capture-goto-setting-section)
         ,(concat
           "* %^{" (m 'capture-loc-name-prompt) "}\n"
           ":PROPERTIES:\n"
           ":ID: %(org-id-new)\n"
           ":Type: %^{" (m 'capture-loc-type-prompt) "}\n"
           ":Echo:\n"
           ":First-appearance: %^{" (m 'capture-loc-first-appearance-prompt) "}\n"
           ":END:\n\n"
           "- " (m 'capture-loc-echo) "\n"
           "  # " (m 'capture-loc-echo-hint) "\n"
           "- " (m 'capture-loc-hazard) "\n"
           "  # " (m 'capture-loc-hazard-hint) "\n"
           "- " (m 'capture-loc-character-aspect) "\n"
           "  # " (m 'capture-loc-character-aspect-hint) "\n"
           "- " (m 'capture-loc-symbol) "\n"
           "  # " (m 'capture-loc-symbol-hint) "\n\n"
           "** " (m 'capture-loc-general-description) "\n\n"
           "# " (m 'capture-loc-general-description-hint) "\n\n"
           "%?\n\n"
           "** " (m 'capture-loc-scenes-here) "\n\n"
           "** " (m 'capture-loc-notes) "\n")
         :empty-lines 1)))))

(defun org-scribe-object-capture-templates (&optional language)
  "Return capture templates for important objects.
LANGUAGE, if non-nil, overrides `org-scribe-project-language'."
  (let ((org-scribe-message-language (or language (org-scribe-project-language))))
    `(("o" ,(org-scribe-msg 'capture-obj-name) entry
       (file+function org-scribe-capture-object-file
                      org-scribe--capture-goto-objects-section)
       ,(format "* %%^{%s}
:PROPERTIES:
:ID: %%(org-id-new)
:Type: %%^{%s}
:Owner: %%^{%s}
:First-appearance: %%^{%s}
:Status: %%^{%s}
:END:

*** %s
%%?

*** %s
-

*** %s
-

*** %s
-

*** %s
-

*** %s
-

*** %s
-

*** %s
-

*** %s
- "
                       (org-scribe-msg 'capture-obj-name-prompt)
                       (org-scribe-msg 'capture-obj-type-prompt)
                       (org-scribe-msg 'capture-obj-owner-prompt)
                       (org-scribe-msg 'capture-obj-first-appearance-prompt)
                       (org-scribe-msg 'capture-obj-status-prompt)
                       (org-scribe-msg 'capture-obj-physical-description)
                       (org-scribe-msg 'capture-obj-origin)
                       (org-scribe-msg 'capture-obj-properties)
                       (org-scribe-msg 'capture-obj-importance-plot)
                       (org-scribe-msg 'capture-obj-history)
                       (org-scribe-msg 'capture-obj-symbolism)
                       (org-scribe-msg 'capture-obj-current-location)
                       (org-scribe-msg 'capture-obj-rules)
                       (org-scribe-msg 'capture-obj-notes))
       :empty-lines 1))))

(defun org-scribe-timeline-capture-templates (&optional language)
  "Return capture templates for timeline events.
LANGUAGE, if non-nil, overrides `org-scribe-project-language'."
  (let ((org-scribe-message-language (or language (org-scribe-project-language))))
    `(("t" ,(org-scribe-msg 'capture-tl-name) entry
       (file+function org-scribe-capture-timeline-file
                      org-scribe--capture-goto-timeline-section)
       ,(format "* %%^{%s}
:PROPERTIES:
:ID: %%(org-id-new)
:Type: %%^{%s}
:Relevance:
:Date: %%^{%s}
:Time:
:Duration:
:Characters: %%^{%s}
:Location: %%^{%s}
:Chapter: %%^{%s}
:END:

*** %s
%%?

*** %s
-

*** %s
- %s

*** %s
- "
                       (org-scribe-msg 'capture-tl-name-prompt)
                       (org-scribe-msg 'capture-tl-type-prompt)
                       (org-scribe-msg 'capture-tl-date-prompt)
                       (org-scribe-msg 'capture-tl-characters-prompt)
                       (org-scribe-msg 'capture-tl-location-prompt)
                       (org-scribe-msg 'capture-tl-chapter-prompt)
                       (org-scribe-msg 'capture-tl-description)
                       (org-scribe-msg 'capture-tl-consequences)
                       (org-scribe-msg 'capture-tl-connections)
                       (org-scribe-msg 'capture-tl-connections-hint)
                       (org-scribe-msg 'capture-tl-notes))
       :empty-lines 1))))

(defun org-scribe-plot-thread-capture-templates (&optional language)
  "Return capture templates for plot threads (narrative lines).
LANGUAGE, if non-nil, overrides `org-scribe-project-language'.

Files a subplot in the method's terms: which Stance it embodies, who
carries it, its own desire, and its crossings with the main line.  A
thread that never crosses the main line is not a subplot but a second
novel inside the first.

Note this captures narrative *lines*, not the thirteen non-negotiables.
Those are a fixed set shipped by the template and are never captured —
see `linking/org-scribe-plot-point-links.el'."
  (let* ((org-scribe-message-language (or language (org-scribe-project-language))))
    (cl-flet ((m (key) (org-scribe-msg key)))
      `(("p" ,(m 'capture-pt-name) entry
         (file+function org-scribe-capture-plot-thread-file
                        org-scribe--capture-goto-plot-threads-section)
         ,(concat
           "* %^{" (m 'capture-pt-name-prompt) "}\n"
           ":PROPERTIES:\n"
           ":ID: %(org-id-new)\n"
           ":THREAD-TYPE: %^{" (m 'capture-pt-type-prompt) "}\n"
           ":Stance:\n"
           ":STATUS: %^{" (m 'capture-pt-status-prompt) "}\n"
           ":FIRST-APPEARANCE:\n"
           ":Weight: %^{" (m 'capture-pt-weight-prompt) "}\n"
           ":END:\n\n"
           "- " (m 'capture-pt-stance-prompt) " ::\n"
           "- " (m 'capture-pt-carried-by-prompt) " ::\n\n"
           "** " (m 'capture-pt-own-desire) "\n\n"
           "# " (m 'capture-pt-own-desire-hint) "\n\n"
           "%?\n\n"
           "** " (m 'capture-pt-crossings) "\n\n"
           "# " (m 'capture-pt-crossings-hint) "\n\n"
           "1.\n2.\n3.\n\n"
           "** " (m 'capture-pt-resolution) "\n\n"
           "** " (m 'capture-pt-notes) "\n")
         :empty-lines 1)))))

;;; Capture Function

(defun org-scribe--run-capture (file-fn templates &optional key)
  "Ensure the capture target file exists, then run org-capture.
FILE-FN is called with t to create the target file if missing.
TEMPLATES is bound to `org-capture-templates' for the capture session.
KEY is the template key to select directly; if nil, present the full menu."
  (funcall file-fn t)
  (let ((org-capture-templates templates))
    (if key
        (org-capture nil key)
      (org-capture))))

;;;###autoload
(defun org-scribe-capture-to-file ()
  "Capture notes to writing project or file.
Automatically determines the appropriate notes file based on project structure."
  (interactive)
  (org-scribe--run-capture #'org-scribe-capture-target-file
                           (org-scribe-capture-templates)))

;;;###autoload
(defun org-scribe-capture-character ()
  "Capture a character profile to the characters file.
Automatically determines the appropriate characters file based on project structure.
Creates a comprehensive character template with prompts for:
- Name, Role, Age, Gender, Occupation
- Physical description
- Personality traits
- Background
- Motivation and character arc
- Relationships with other characters"
  (interactive)
  (org-scribe--run-capture #'org-scribe-capture-character-file
                           (org-scribe-character-capture-templates) "c"))

;;;###autoload
(defun org-scribe-capture-location ()
  "Capture a location profile to the locations file.
Automatically determines the appropriate locations file based on project structure.
Creates a comprehensive location template with prompts for:
- Name, Type, Importance, Climate, Population
- Physical description
- Geography and environment
- Culture and society
- History and plot significance
- Atmosphere and mood"
  (interactive)
  (org-scribe--run-capture #'org-scribe-capture-location-file
                           (org-scribe-location-capture-templates) "l"))

;;;###autoload
(defun org-scribe-capture-object ()
  "Capture an important object to the objects file.
Automatically determines the appropriate objects file based on project structure.
Creates a comprehensive object template with prompts for:
- Name, Type, Owner, Status
- Physical description
- Properties and abilities
- Origin and history
- Plot significance
- Current location and limitations"
  (interactive)
  (org-scribe--run-capture #'org-scribe-capture-object-file
                           (org-scribe-object-capture-templates) "o"))

;;;###autoload
(defun org-scribe-capture-timeline ()
  "Capture a timeline event to the timeline file.
Automatically determines the appropriate timeline file based on project structure.
Creates a comprehensive timeline event template with prompts for:
- Event name, Date/time, Story day, Chapter
- Location and characters involved
- Event description
- Consequences and connections
- Type of event (action, revelation, etc.)"
  (interactive)
  (org-scribe--run-capture #'org-scribe-capture-timeline-file
                           (org-scribe-timeline-capture-templates) "t"))

;;;###autoload
(defun org-scribe-capture-plot-thread ()
  "Capture a plot thread to the plot file.
Automatically determines the appropriate plot file based on project structure.
Creates a plot thread entry with:
- Name and Type (Main Plot, Subplot, etc.)
- Auto-generated ID for linking
- Description and connection to main plot
- Key scenes where thread appears
- Resolution notes

This is useful when:
- You discover a new subplot while writing
- You notice a thematic pattern emerging
- You want to track a storyline across scenes
- Beta readers suggest developing a thread

The template is intentionally minimal - capture the essence quickly,
then elaborate later during planning or revision."
  (interactive)
  (org-scribe--run-capture #'org-scribe-capture-plot-thread-file
                           (org-scribe-plot-thread-capture-templates) "p"))

;;; Unified Capture Hook

(defun org-scribe--capture-finalize-add-entity-id ()
  "Hook to add ID to newly captured entities (characters, locations, plot threads).
Runs before capture is finalized.  Checks if the capture target matches
any known entity file and ensures the heading has an ID.

This is a safety net - capture templates already include ID generation
via %(org-id-new), but this ensures any entity heading without an ID
gets one automatically."
  (when (and (boundp 'org-capture-mode)
             org-capture-mode
             (buffer-file-name))
    (let* ((buf-file (buffer-file-name))
           (entity-files (delq nil
                               (list (ignore-errors (org-scribe-capture-character-file))
                                     (ignore-errors (org-scribe-capture-location-file))
                                     (ignore-errors (org-scribe-capture-plot-thread-file)))))
           (match (cl-some (lambda (target)
                             (or (string= buf-file target)
                                 (string= buf-file (expand-file-name target))))
                           entity-files)))
      (when match
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^\\*+ " nil t)
            (org-back-to-heading)
            (unless (org-entry-get nil "ID")
              (org-id-get-create))))))))

(add-hook 'org-capture-before-finalize-hook #'org-scribe--capture-finalize-add-entity-id)

(provide 'org-scribe-capture)

;;; org-scribe-capture.el ends here
