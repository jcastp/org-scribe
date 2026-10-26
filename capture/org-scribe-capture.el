;;; org-scribe-capture.el --- Capture system for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Project-aware capture system for writing notes.
;; Every capture files into the project's design notebook.

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

;; One file holds every capturable entity: the design notebook.  A novel
;; keeps its sections inside parts (see `org-scribe--section-part'), a
;; short story keeps them as level-1 sections.

(defconst org-scribe--notebook-sections
  '(characters setting plot-threads objects timeline)
  "Section keys a capture can file into, in the short-story order.")

(defun org-scribe--create-notebook-file (filepath project-type &optional language)
  "Create a minimal design notebook at FILEPATH.
PROJECT-TYPE is \\='novel, \\='short-story or \\='unknown.  A novel gets the
parts holding the five capturable sections, with the sections inside
them; a short story gets the same five sections at level 1.  LANGUAGE,
if non-nil, overrides `org-scribe-project-language'.

Every heading comes from `org-scribe-lang-heading', the datum the
linking layer and `org-scribe--capture-goto-section' match against, so
the text written and the alias cannot drift apart.  The file carries no
placeholder entities and no instructional prose: it is created around a
real capture, so a placeholder would read as a duplicate of the thing
just filed.  A properly created project never reaches this function;
it exists for an older project, one assembled by hand, or a deliberately
deleted file."
  (let* ((resolved-language (or language (org-scribe-project-language)))
         (org-scribe-message-language resolved-language)
         (title (org-scribe-project-title (file-name-directory filepath))))
    (make-directory (file-name-directory filepath) t)
    (with-temp-file filepath
      (insert (org-scribe--file-header
               (org-scribe-msg 'capture-title-project-notes title) "overview"))
      (if (eq project-type 'short-story)
          (dolist (key org-scribe--notebook-sections)
            (insert (format "* %s\n\n" (org-scribe-lang-heading key resolved-language))))
        (dolist (part '(plot-part cast world))
          (insert (format "* %s\n\n" (org-scribe-lang-heading part resolved-language)))
          (dolist (key org-scribe--notebook-sections)
            (when (eq (alist-get key org-scribe--section-part) part)
              (insert (format "** %s\n\n" (org-scribe-lang-heading key resolved-language))))))))))

;;; Capture Target File Detection

(defun org-scribe-capture-target-file (&optional create-if-missing)
  "Return the design notebook, the file every capture files into.
Looks for the notebook in the project root, whichever registered
language's spelling exists (see `org-scribe--resolve'); failing that
the current buffer's file; failing that the notebook named for
`org-scribe-project-language'.  If CREATE-IF-MISSING is non-nil and the
file does not exist, create it with `org-scribe--create-notebook-file'."
  (let* ((project-dir (or (org-scribe-project-root)
                          (file-name-directory (or (buffer-file-name) default-directory))))
         (target (or (org-scribe--resolve project-dir 'design)
                     (buffer-file-name)
                     (expand-file-name (org-scribe-lang-file 'design) project-dir))))
    (when (and create-if-missing (not (file-exists-p target)))
      (org-scribe--create-notebook-file target (org-scribe-project-type)))
    target))

;;; Capture Target Positioning
;;
;; Every entity lives under a section heading of the design notebook, at
;; whatever level the layout puts it.  These functions move point onto the
;; right section heading (creating it if missing) so `file+function'
;; capture targets file new entities where the linking predicates expect
;; to find them.

(defun org-scribe--capture-goto-section (section-key)
  "Move point onto the heading of SECTION-KEY, creating it if missing.
SECTION-KEY is a symbol such as \\='characters or \\='setting (a key of the
`:headings' section of the language packs).  The heading is found at any
level by any of its localized aliases (`org-scribe--find-section-heading').
If it is missing, it is created in the project language's spelling:
as the last child of its part, one level deeper, when the part exists
\(`org-scribe--section-part'); otherwise as a level-1 heading at the end of
the buffer.  Point is left ON the heading line, so `org-capture' files
the entry as a child, with its level adjusted (see `org-capture-place-entry')."
  (let ((pos (org-scribe--find-section-heading section-key)))
    (if pos
        (progn (widen) (goto-char pos) (forward-line 0))
      (let* ((language (org-scribe-project-language))
             (part-pos (when-let* ((part (alist-get section-key org-scribe--section-part)))
                         (org-scribe--find-section-heading part))))
        (widen)
        (if part-pos
            (progn
              (goto-char part-pos)
              (let ((level (1+ (org-current-level))))
                (org-end-of-subtree t t)
                (unless (bolp) (insert "\n"))
                (insert (format "%s %s\n" (make-string level ?*)
                                (org-scribe-lang-heading section-key language)))
                (forward-line -1)))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (format "* %s\n" (org-scribe-lang-heading section-key language)))
          (forward-line -1))))))

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
See `org-scribe--capture-goto-section'."
  (org-scribe--capture-goto-section 'plot-threads))

(defun org-scribe--capture-goto-objects-section ()
  "Move point to where a new object capture should be filed.
No entity predicate reads this section (objects are not a linked entity
type in this package); it exists so a captured object lands in the
Objects section instead of at the end of the buffer.  See
`org-scribe--capture-goto-section'."
  (org-scribe--capture-goto-section 'objects))

(defun org-scribe--capture-goto-timeline-section ()
  "Move point to where a new timeline-event capture should be filed.
See `org-scribe--capture-goto-section'."
  (org-scribe--capture-goto-section 'timeline))

(defun org-scribe--capture-goto-notes-section ()
  "Move point onto the Scratchpad heading, creating it if absent.
General notes are filed there: it is the notebook's one place that asks
nothing of the writer.  Leaves point ON the heading line, so the capture
is filed under it.  See `org-scribe--capture-goto-section'."
  (org-scribe--capture-goto-section 'scratchpad-part))

(defun org-scribe--capture-table (header)
  "Return HEADER, an Org table row, with a separator and an empty row.
The separator and the empty row are as wide as HEADER's cells, so the
captured table reads like the template's without a TAB to realign it."
  (let ((widths (mapcar #'string-width
                        (butlast (cdr (split-string header "|"))))))
    (concat header "\n"
            "|" (mapconcat (lambda (w) (make-string w ?-)) widths "+") "|\n"
            "|" (mapconcat (lambda (w) (make-string w ?\s)) widths "|") "|\n")))

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
       (file+function org-scribe-capture-target-file
                      org-scribe--capture-goto-notes-section)
       "* %?\n  %U\n  %i"
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
the labels silently shifted by one.

In a short-story project the profile is the short-story template's own
(Personality, Goal, Conflict, Key trait, Background): a novel ficha
filed into the same notes file as the template's characters gave it two
incompatible profile shapes."
  (let* ((org-scribe-message-language (or language (org-scribe-project-language)))
         (short-story (eq (org-scribe-project-type) 'short-story)))
    (cl-flet ((m (key) (org-scribe-msg key)))
      `(("c" ,(m 'capture-char-name) entry
         (file+function org-scribe-capture-target-file
                        org-scribe--capture-goto-characters-section)
         ,(if short-story
              (concat
               "* %^{" (m 'capture-char-name-prompt) "}\n"
               ":PROPERTIES:\n"
               ":ID: %(org-id-new)\n"
               ":Role: %^{" (m 'capture-ss-char-role-prompt) "}\n"
               ":Age: %^{" (m 'capture-char-age-prompt) "}\n"
               ":Gender:\n"
               ":RelationshipsData:\n"
               ":END:\n\n"
               "- " (m 'capture-ss-personality) " %?\n"
               "- " (m 'capture-ss-goal) "\n"
               "- " (m 'capture-ss-conflict) "\n"
               "- " (m 'capture-ss-trait) "\n"
               "- " (m 'capture-ss-background) "\n")
          (concat
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
           (org-scribe--capture-table (m 'capture-char-milestone-header)) "\n"
           "** " (m 'capture-char-notes) "\n"))
         :empty-lines 1)))))

(defun org-scribe-location-capture-templates (&optional language)
  "Return capture templates for setting profiles.
LANGUAGE, if non-nil, overrides `org-scribe-project-language'.

Files the method's four setting Aspects — Echo, Hazard, Character and
Symbol — rather than the gazetteer fields (climate, population, terrain)
of the previous model.  A setting with no Aspect is scenery, and Pass 5
of the revision marks scenery for cutting.

In a short-story project the profile is the short-story template's own
\(Description, Important details, Atmosphere), for the same reason as in
`org-scribe-character-capture-templates'."
  (let* ((org-scribe-message-language (or language (org-scribe-project-language)))
         (short-story (eq (org-scribe-project-type) 'short-story)))
    (cl-flet ((m (key) (org-scribe-msg key)))
      `(("l" ,(m 'capture-loc-name) entry
         (file+function org-scribe-capture-target-file
                        org-scribe--capture-goto-setting-section)
         ,(if short-story
              (concat
               "* %^{" (m 'capture-loc-name-prompt) "}\n"
               ":PROPERTIES:\n"
               ":ID: %(org-id-new)\n"
               ":Type: %^{" (m 'capture-loc-type-prompt) "}\n"
               ":Time-period:\n"
               ":END:\n\n"
               "- " (m 'capture-ss-loc-description) " %?\n"
               "- " (m 'capture-ss-loc-details) "\n"
               "- " (m 'capture-ss-loc-atmosphere) "\n")
          (concat
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
           "** " (m 'capture-loc-notes) "\n"))
         :empty-lines 1)))))

(defun org-scribe-object-capture-templates (&optional language)
  "Return capture templates for important objects.
LANGUAGE, if non-nil, overrides `org-scribe-project-language'.
The sub-headings are one level below the object, so wherever org-capture
files the entry they sit directly under it."
  (let ((org-scribe-message-language (or language (org-scribe-project-language))))
    (cl-flet ((m (key) (org-scribe-msg key)))
      `(("o" ,(m 'capture-obj-name) entry
         (file+function org-scribe-capture-target-file
                        org-scribe--capture-goto-objects-section)
         ,(concat
           "* %^{" (m 'capture-obj-name-prompt) "}\n"
           ":PROPERTIES:\n"
           ":ID: %(org-id-new)\n"
           ":Type: %^{" (m 'capture-obj-type-prompt) "}\n"
           ":Owner: %^{" (m 'capture-obj-owner-prompt) "}\n"
           ":First-appearance: %^{" (m 'capture-obj-first-appearance-prompt) "}\n"
           ":Status: %^{" (m 'capture-obj-status-prompt) "}\n"
           ":END:\n\n"
           "** " (m 'capture-obj-physical-description) "\n%?\n\n"
           "** " (m 'capture-obj-origin) "\n-\n\n"
           "** " (m 'capture-obj-properties) "\n-\n\n"
           "** " (m 'capture-obj-importance-plot) "\n-\n\n"
           "** " (m 'capture-obj-history) "\n-\n\n"
           "** " (m 'capture-obj-symbolism) "\n-\n\n"
           "** " (m 'capture-obj-current-location) "\n-\n\n"
           "** " (m 'capture-obj-rules) "\n-\n\n"
           "** " (m 'capture-obj-notes) "\n- ")
         :empty-lines 1)))))

(defun org-scribe-timeline-capture-templates (&optional language)
  "Return capture templates for timeline events.
LANGUAGE, if non-nil, overrides `org-scribe-project-language'.
The sub-headings are one level below the event; see
`org-scribe-object-capture-templates'."
  (let ((org-scribe-message-language (or language (org-scribe-project-language))))
    (cl-flet ((m (key) (org-scribe-msg key)))
      `(("t" ,(m 'capture-tl-name) entry
         (file+function org-scribe-capture-target-file
                        org-scribe--capture-goto-timeline-section)
         ,(concat
           "* %^{" (m 'capture-tl-name-prompt) "}\n"
           ":PROPERTIES:\n"
           ":ID: %(org-id-new)\n"
           ":Type: %^{" (m 'capture-tl-type-prompt) "}\n"
           ":Relevance:\n"
           ":Date: %^{" (m 'capture-tl-date-prompt) "}\n"
           ":Time:\n"
           ":Duration:\n"
           ":Characters: %^{" (m 'capture-tl-characters-prompt) "}\n"
           ":Location: %^{" (m 'capture-tl-location-prompt) "}\n"
           ":Chapter: %^{" (m 'capture-tl-chapter-prompt) "}\n"
           ":END:\n\n"
           "** " (m 'capture-tl-description) "\n%?\n\n"
           "** " (m 'capture-tl-consequences) "\n-\n\n"
           "** " (m 'capture-tl-connections) "\n- " (m 'capture-tl-connections-hint) "\n\n"
           "** " (m 'capture-tl-notes) "\n- ")
         :empty-lines 1)))))

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
         (file+function org-scribe-capture-target-file
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
KEY is the template key to select directly; if nil, present the full menu.

Org places every entry with `org-paste-subtree', which echoes \"Clipboard
pasted as level N subtree\": noise for a capture, where nothing came from
the clipboard.  The message is kept out of the echo area (it still
reaches *Messages*) for this capture only."
  (funcall file-fn t)
  (let ((org-capture-templates templates))
    (advice-add 'org-paste-subtree :around #'org-scribe--capture-quiet-paste)
    (unwind-protect
        (if key
            (org-capture nil key)
          (org-capture))
      (advice-remove 'org-paste-subtree #'org-scribe--capture-quiet-paste))))

(defun org-scribe--capture-quiet-paste (orig &rest args)
  "Call ORIG with ARGS, keeping its messages out of the echo area."
  (let ((inhibit-message t))
    (apply orig args)))

;;;###autoload
(defun org-scribe-capture-to-file ()
  "Capture notes to writing project or file.
Files into the design notebook's Scratchpad part."
  (interactive)
  ;; Pass the key: with none, `org-capture' shows its template menu even
  ;; for a single template (plus Org's "Customize org-capture-templates").
  (org-scribe--run-capture #'org-scribe-capture-target-file
                           (org-scribe-capture-templates) "w"))

;;;###autoload
(defun org-scribe-capture-character ()
  "Capture a character profile to the design notebook.
Files it under the matching section, creating the section if needed.
Creates a comprehensive character template with prompts for:
- Name, Role, Age, Gender, Occupation
- Physical description
- Personality traits
- Background
- Motivation and character arc
- Relationships with other characters"
  (interactive)
  (org-scribe--require-project)
  (org-scribe--run-capture #'org-scribe-capture-target-file
                           (org-scribe-character-capture-templates) "c"))

;;;###autoload
(defun org-scribe-capture-location ()
  "Capture a location profile to the design notebook.
Files it under the matching section, creating the section if needed.
Creates a comprehensive location template with prompts for:
- Name, Type, Importance, Climate, Population
- Physical description
- Geography and environment
- Culture and society
- History and plot significance
- Atmosphere and mood"
  (interactive)
  (org-scribe--require-project)
  (org-scribe--run-capture #'org-scribe-capture-target-file
                           (org-scribe-location-capture-templates) "l"))

;;;###autoload
(defun org-scribe-capture-object ()
  "Capture an important object to the design notebook.
Files it under the matching section, creating the section if needed.
Creates a comprehensive object template with prompts for:
- Name, Type, Owner, Status
- Physical description
- Properties and abilities
- Origin and history
- Plot significance
- Current location and limitations"
  (interactive)
  (org-scribe--require-project)
  (org-scribe--run-capture #'org-scribe-capture-target-file
                           (org-scribe-object-capture-templates) "o"))

;;;###autoload
(defun org-scribe-capture-timeline ()
  "Capture a timeline event to the design notebook.
Files it under the matching section, creating the section if needed.
Creates a comprehensive timeline event template with prompts for:
- Event name, Date/time, Story day, Chapter
- Location and characters involved
- Event description
- Consequences and connections
- Type of event (action, revelation, etc.)"
  (interactive)
  (org-scribe--require-project)
  (org-scribe--run-capture #'org-scribe-capture-target-file
                           (org-scribe-timeline-capture-templates) "t"))

;;;###autoload
(defun org-scribe-capture-plot-thread ()
  "Capture a plot thread to the design notebook.
Files it under the matching section, creating the section if needed.
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
  (org-scribe--require-project)
  (org-scribe--run-capture #'org-scribe-capture-target-file
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
             (buffer-file-name)
             ;; A general note is not an entity.
             (not (equal (org-capture-get :key) "w")))
    (let* ((buf-file (buffer-file-name))
           (entity-files (delq nil (list (ignore-errors (org-scribe-capture-target-file)))))
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
