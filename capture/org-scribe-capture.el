;;; org-scribe-capture.el --- Capture system for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Project-aware capture system for writing notes.
;; Automatically determines the appropriate notes file based on project structure.

;;; Code:

(require 'org-capture)
(require 'project)
(require 'org-scribe-core)
(require 'org-scribe-config)

;;; File Creation Helpers

(defun org-scribe--create-plot-file (filepath is-short-story)
  "Create a basic plot file for captures.
FILEPATH is the path where the file should be created.
IS-SHORT-STORY determines the structure."
  (with-temp-file filepath
    (insert "#+TITLE: Plot Structure\n")
    (insert "#+AUTHOR: " user-full-name "\n")
    (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n")
    (insert "#+STARTUP: overview\n\n")

    (if is-short-story
        (progn
          (insert "* Plot Outline\n\n")
          (insert "** Premise\n\n")
          (insert "** Setup\n\n")
          (insert "** Central Conflict\n\n")
          (insert "** Resolution\n\n")
          (insert "* Plot Threads\n\n")
          (insert "[Plot threads will appear here when captured]\n\n"))
      ;; Novel structure
      (progn
        (insert "* Premise\n\nWhat is the story about in one or two sentences?\n\n")
        (insert "* Main Plot\n\n")
        (insert "** Central Conflict\n\n")
        (insert "** Main Dramatic Question\n\n")
        (insert "* Subplots\n\n")
        (insert "* Plot Threads\n\n")
        (insert "Track your plot threads here. Use F8 F8 p to capture new threads.\n\n")))))

(defun org-scribe--create-short-story-notes-file (filepath)
  "Create a comprehensive notes.org file for short story projects.
FILEPATH is the path where the file should be created."
  (with-temp-file filepath
    (insert "#+TITLE: " (file-name-base (directory-file-name (file-name-directory filepath))) " - Planning & Notes\n")
    (insert "#+AUTHOR: " user-full-name "\n")
    (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n")
    (insert "#+STARTUP: overview\n\n")

    ;; Standard headings for short story notes
    (insert "* Characters\n\n")
    (insert "** Protagonist: [Name]\n")
    (insert ":PROPERTIES:\n:TYPE: Protagonist\n:NAME:\n:AGE:\n:GENDER:\n:END:\n\n")
    (insert "- Personality ::\n- Goal ::\n- Conflict ::\n\n")

    (insert "* Plot Outline\n\n")
    (insert "** Premise\n\n** Setup\n\n** Central Conflict\n\n** Resolution\n\n")

    (insert "* Setting\n\n")
    (insert "** Main Location(s)\n\n")
    (insert "** Locations\n\n")

    (insert "* Objects\n\n")

    (insert "* Timeline\n\n")

    (insert "* Research & References\n\n")

    (insert "* Revision Notes\n\n")

    (insert "* Random Ideas & Inspiration\n\n")))

(defun org-scribe--create-novel-capture-file (filepath content-type)
  "Create an individual capture file for novel projects.
FILEPATH is the path where the file should be created.
CONTENT-TYPE is 'characters, 'locations, 'objects, 'timeline, or 'notes."
  (with-temp-file filepath
    (pcase content-type
      ('characters
       (insert "#+TITLE: Character Database\n")
       (insert "#+AUTHOR: " user-full-name "\n")
       (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
       )
      ('locations
       (insert "#+TITLE: Locations & World Building\n")
       (insert "#+AUTHOR: " user-full-name "\n")
       (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
       )
      ('objects
       (insert "#+TITLE: Important Objects\n")
       (insert "#+AUTHOR: " user-full-name "\n")
       (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
       )
      ('timeline
       (insert "#+TITLE: Story Timeline\n")
       (insert "#+AUTHOR: " user-full-name "\n")
       (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
       )
      ('notes
       (insert "#+TITLE: Writing Notes\n")
       (insert "#+AUTHOR: " user-full-name "\n")
       (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
       (insert "* Notes\n\n")))))

(defun org-scribe--create-capture-file (filepath project-type content-type)
  "Create a capture target file based on project type.
FILEPATH is the path to create.
PROJECT-TYPE is 'novel, 'short-story, or 'unknown.
CONTENT-TYPE is 'characters, 'locations, 'objects, 'timeline, or 'notes.

For short stories, creates notes.org with all standard headings.
For novels, creates individual files."
  (let ((target-dir (file-name-directory filepath)))
    (unless (file-directory-p target-dir)
      (make-directory target-dir t)))

  (if (eq project-type 'short-story)
      ;; Short story: create comprehensive notes.org
      (org-scribe--create-short-story-notes-file filepath)
    ;; Novel or unknown: create individual file
    (org-scribe--create-novel-capture-file filepath content-type)))

;;; Capture Target File Detection

(defun org-scribe/capture-location-file (&optional create-if-missing)
  "Determine the appropriate file for location captures.
For novels: Uses objects/locations.org (or localizaciones.org)
For short stories: Uses notes.org (or notas.org)
For unknown projects: Falls back to objects/locations.org

If CREATE-IF-MISSING is non-nil, create the file if it doesn't exist."
  (let* ((project-dir (if-let ((project (project-current)))
                          (project-root project)
                        (file-name-directory (or (buffer-file-name) default-directory))))
         (project-type (org-scribe-project-type))
         (target
          (cond
           ;; Short story: use notes.org
           ((eq project-type 'short-story)
            (cond
             ((file-exists-p (expand-file-name "notes.org" project-dir))
              (expand-file-name "notes.org" project-dir))
             ((file-exists-p (expand-file-name "notas.org" project-dir))
              (expand-file-name "notas.org" project-dir))
             (t (expand-file-name "notes.org" project-dir))))

           ;; Novel or unknown: use objects/locations.org
           (t
            (cond
             ((file-exists-p (expand-file-name "objects/locations.org" project-dir))
              (expand-file-name "objects/locations.org" project-dir))
             ((file-exists-p (expand-file-name "objects/localizaciones.org" project-dir))
              (expand-file-name "objects/localizaciones.org" project-dir))
             ((file-exists-p (expand-file-name "locations.org" project-dir))
              (expand-file-name "locations.org" project-dir))
             ((file-exists-p (expand-file-name "localizaciones.org" project-dir))
              (expand-file-name "localizaciones.org" project-dir))
             (t (expand-file-name "objects/locations.org" project-dir)))))))

    ;; Create file if needed
    (when (and create-if-missing (not (file-exists-p target)))
      (org-scribe--create-capture-file target project-type 'locations))

    target))

(defun org-scribe/capture-object-file (&optional create-if-missing)
  "Determine the appropriate file for object captures.
For novels: Uses objects/objects.org (or objetos.org)
For short stories: Uses notes.org (or notas.org)
For unknown projects: Falls back to objects/objects.org

If CREATE-IF-MISSING is non-nil, create the file if it doesn't exist."
  (let* ((project-dir (if-let ((project (project-current)))
                          (project-root project)
                        (file-name-directory (or (buffer-file-name) default-directory))))
         (project-type (org-scribe-project-type))
         (target
          (cond
           ;; Short story: use notes.org
           ((eq project-type 'short-story)
            (cond
             ((file-exists-p (expand-file-name "notes.org" project-dir))
              (expand-file-name "notes.org" project-dir))
             ((file-exists-p (expand-file-name "notas.org" project-dir))
              (expand-file-name "notas.org" project-dir))
             (t (expand-file-name "notes.org" project-dir))))

           ;; Novel or unknown: use objects/objects.org
           (t
            (cond
             ((file-exists-p (expand-file-name "objects/objects.org" project-dir))
              (expand-file-name "objects/objects.org" project-dir))
             ((file-exists-p (expand-file-name "objects/objetos.org" project-dir))
              (expand-file-name "objects/objetos.org" project-dir))
             ((file-exists-p (expand-file-name "objects.org" project-dir))
              (expand-file-name "objects.org" project-dir))
             ((file-exists-p (expand-file-name "objetos.org" project-dir))
              (expand-file-name "objetos.org" project-dir))
             (t (expand-file-name "objects/objects.org" project-dir)))))))

    ;; Create file if needed
    (when (and create-if-missing (not (file-exists-p target)))
      (org-scribe--create-capture-file target project-type 'objects))

    target))

(defun org-scribe/capture-timeline-file (&optional create-if-missing)
  "Determine the appropriate file for timeline captures.
For novels: Uses objects/timeline.org (or cronologia.org)
For short stories: Uses notes.org (or notas.org)
For unknown projects: Falls back to objects/timeline.org

If CREATE-IF-MISSING is non-nil, create the file if it doesn't exist."
  (let* ((project-dir (if-let ((project (project-current)))
                          (project-root project)
                        (file-name-directory (or (buffer-file-name) default-directory))))
         (project-type (org-scribe-project-type))
         (target
          (cond
           ;; Short story: use notes.org
           ((eq project-type 'short-story)
            (cond
             ((file-exists-p (expand-file-name "notes.org" project-dir))
              (expand-file-name "notes.org" project-dir))
             ((file-exists-p (expand-file-name "notas.org" project-dir))
              (expand-file-name "notas.org" project-dir))
             (t (expand-file-name "notes.org" project-dir))))

           ;; Novel or unknown: use objects/timeline.org
           (t
            (cond
             ((file-exists-p (expand-file-name "objects/timeline.org" project-dir))
              (expand-file-name "objects/timeline.org" project-dir))
             ((file-exists-p (expand-file-name "objects/cronologia.org" project-dir))
              (expand-file-name "objects/cronologia.org" project-dir))
             ((file-exists-p (expand-file-name "timeline.org" project-dir))
              (expand-file-name "timeline.org" project-dir))
             ((file-exists-p (expand-file-name "cronologia.org" project-dir))
              (expand-file-name "cronologia.org" project-dir))
             (t (expand-file-name "objects/timeline.org" project-dir)))))))

    ;; Create file if needed
    (when (and create-if-missing (not (file-exists-p target)))
      (org-scribe--create-capture-file target project-type 'timeline))

    target))

(defun org-scribe/capture-character-file (&optional create-if-missing)
  "Determine the appropriate file for character captures.
For novels: Uses objects/characters.org (or personajes.org)
For short stories: Uses notes.org (or notas.org)
For unknown projects: Falls back to objects/characters.org

If CREATE-IF-MISSING is non-nil, create the file if it doesn't exist."
  (let* ((project-dir (if-let ((project (project-current)))
                          (project-root project)
                        (file-name-directory (or (buffer-file-name) default-directory))))
         (project-type (org-scribe-project-type))
         (target
          (cond
           ;; Short story: use notes.org
           ((eq project-type 'short-story)
            (cond
             ((file-exists-p (expand-file-name "notes.org" project-dir))
              (expand-file-name "notes.org" project-dir))
             ((file-exists-p (expand-file-name "notas.org" project-dir))
              (expand-file-name "notas.org" project-dir))
             (t (expand-file-name "notes.org" project-dir))))

           ;; Novel or unknown: use objects/characters.org
           (t
            (cond
             ((file-exists-p (expand-file-name "objects/characters.org" project-dir))
              (expand-file-name "objects/characters.org" project-dir))
             ((file-exists-p (expand-file-name "objects/personajes.org" project-dir))
              (expand-file-name "objects/personajes.org" project-dir))
             ((file-exists-p (expand-file-name "characters.org" project-dir))
              (expand-file-name "characters.org" project-dir))
             ((file-exists-p (expand-file-name "personajes.org" project-dir))
              (expand-file-name "personajes.org" project-dir))
             (t (expand-file-name "objects/characters.org" project-dir)))))))

    ;; Create file if needed
    (when (and create-if-missing (not (file-exists-p target)))
      (org-scribe--create-capture-file target project-type 'characters))

    target))

(defun org-scribe/capture-plot-thread-file (&optional create-if-missing)
  "Determine the appropriate file for plot thread captures.
For novels: Uses objects/plot.org (or objects/trama.org)
For short stories: Uses notes.org (or notas.org)
For unknown projects: Falls back to objects/plot.org

If CREATE-IF-MISSING is non-nil, create the file if it doesn't exist."
  (let* ((project-dir (if-let ((project (project-current)))
                          (project-root project)
                        (file-name-directory (or (buffer-file-name) default-directory))))
         (project-type (org-scribe-project-type))
         (target
          (cond
           ;; Short story: use notes.org
           ((eq project-type 'short-story)
            (cond
             ((file-exists-p (expand-file-name "notes.org" project-dir))
              (expand-file-name "notes.org" project-dir))
             ((file-exists-p (expand-file-name "notas.org" project-dir))
              (expand-file-name "notas.org" project-dir))
             (t (expand-file-name "notes.org" project-dir))))

           ;; Novel or unknown: use objects/plot.org
           (t
            (cond
             ((file-exists-p (expand-file-name "objects/plot.org" project-dir))
              (expand-file-name "objects/plot.org" project-dir))
             ((file-exists-p (expand-file-name "objects/trama.org" project-dir))
              (expand-file-name "objects/trama.org" project-dir))
             ((file-exists-p (expand-file-name "plot.org" project-dir))
              (expand-file-name "plot.org" project-dir))
             (t (expand-file-name "objects/plot.org" project-dir)))))))

    ;; Create file if needed
    (when (and create-if-missing (not (file-exists-p target)))
      (let ((dir (file-name-directory target)))
        (unless (file-directory-p dir)
          (make-directory dir t))
        (org-scribe--create-plot-file target (eq project-type 'short-story))))

    target))

(defun org-scribe/capture-target-file (&optional create-if-missing)
  "Determine the appropriate notes file for org-capture in writing environment.
Uses `project-current' to find the project base directory as reference point.
Returns the file path based on the following priority:
1. notes/notes.org (relative to project root)
2. notas/notas.org - Spanish (relative to project root)
3. novel-notes.org (in project root)
4. notes.org (in project root)
5. current buffer if none of the above exist

If CREATE-IF-MISSING is non-nil, create the first priority notes
file that doesn't exist."
  (let* ((project-dir (if-let ((project (project-current)))
                          (project-root project)
                        (file-name-directory (or (buffer-file-name) default-directory))))
         (notes-subdir-en (expand-file-name "notes/notes.org" project-dir))
         (notes-subdir-es (expand-file-name "notas/notas.org" project-dir))
         (novel-notes (expand-file-name "novel-notes.org" project-dir))
         (notes (expand-file-name "notes.org" project-dir))
         (target (cond
                  ((file-exists-p notes-subdir-en) notes-subdir-en)
                  ((file-exists-p notes-subdir-es) notes-subdir-es)
                  ((file-exists-p novel-notes) novel-notes)
                  ((file-exists-p notes) notes)
                  (t (or (buffer-file-name)
                         (expand-file-name "notes.org" project-dir))))))
    (when (and create-if-missing
               (not (file-exists-p target)))
      (let ((target-dir (file-name-directory target)))
        (unless (file-directory-p target-dir)
          (make-directory target-dir t)))
      (with-temp-file target
        (insert "#+TITLE: Writing Notes\n")
        (insert (format "#+AUTHOR: %s\n" user-full-name))
        (insert (format "#+EMAIL: %s\n\n" user-mail-address))
        (insert "* Notes\n")))
    target))

;;; Capture Templates

(defvar org-scribe/capture-templates
  '(("w" "Writing Note" entry
     (file+headline org-scribe/capture-target-file "Notes")
     "** TODO %?\n  %U\n  %i"
     :empty-lines 1))
  "Capture templates specific to the writing environment.")

(defvar org-scribe/character-capture-templates
  '(("c" "Character Profile" entry
     (file org-scribe/capture-character-file)
     "* %^{Character Name}
:PROPERTIES:
:ID: %(org-id-new)
:Role: %^{Role|Protagonist|Opponent|Antagonist|Supporting|Minor|Ally|Mentor}
:Weight: %^{Weight|1.0|2.0|3.0|4.0|5.0}
:Age: %^{Age}
:Gender: %^{Gender}
:Occupation: %^{Occupation}
:Goal:
:Motivation:
:Conflict:
:Arc:
:First-appearance: %^{First Appearance Chapter}
:RelationshipsData:
:END:

** Physical Description

- Height ::
- Build ::
- Hair ::
- Eyes ::
- Distinctive features ::

** Personality

- Main traits ::
- Strengths ::
- Weaknesses ::
- Fears ::
- Desire ::
- Need ::
- Psychological Flaw ::
- Moral Flaw :: 

** Background

- Family ::
- Education ::
- Occupation ::
- Formative events ::

** Goal, Motivation, Conflict
*** Internal
- Goal ::
- Motivation ::
- Conflict ::
*** External
- Goal ::
- Motivation ::
- Conflict ::

** Character Arc

- Initial state ::
- Turning point ::
- Transformation ::
- Final state ::

** Relationships

- With other characters ::

** Notes
- "
     :empty-lines 1
     ))
  "Capture templates for character profiles.")

(defvar org-scribe/location-capture-templates
  '(("l" "Location" entry
     (file org-scribe/capture-location-file)
     "* %^{Location Name}
:PROPERTIES:
:ID: %(org-id-new)
:Type: %^{Type|City|Building|Room|Natural|Region|Country}
:Importance: %^{Importance|Major|Supporting|Minor}
:First-appearance: %^{First Appearance Chapter}
:Climate: %^{Climate}
:Population: %^{Population}
:END:

** General Description
%?

** Geography

- Location ::
- Terrain ::
- Climate ::
- Natural resources ::

** Cultural Aspects & Society

- Language ::
- Customs ::
- Religion ::
- Government ::

** History
-

** Notable Features
-

** Importance in the Plot
-

** Specific Places
-

** Atmosphere & Mood
-

** Map/Reference Image
-

** Notes
- "
     :empty-lines 1))
  "Capture templates for location profiles.")

(defvar org-scribe/object-capture-templates
  '(("o" "Object" entry
     (file org-scribe/capture-object-file)
     "* %^{Object Name}
:PROPERTIES:
:ID: %(org-id-new)
:Type: %^{Type|Magical|Artifact|Weapon|Tool|Symbolic|Technology}
:Owner: %^{Current Owner}
:First-appearance: %^{First Appearance Chapter}
:Status: %^{Status|Active|Lost|Destroyed|Hidden}
:END:

*** Physical Description
%?

*** Origin
-

*** Properties
-

*** Importance in the Plot
-

*** Object History
-

*** Symbolism
- 

*** Current Location
-

*** Rules & Limitations
-

*** Notes
- "
     :empty-lines 1))
  "Capture templates for important objects.")

(defvar org-scribe/timeline-capture-templates
  '(("t" "Timeline Event" entry
     (file org-scribe/capture-timeline-file)
     "* %^{Event Name}
:PROPERTIES:
:ID: %(org-id-new)
:Type: %^{Type|Action|Revelation|Character|World|Backstory}
:Relevance:
:Date: %^{Date/Time in Story}
:Time:
:Duration:
:Characters: %^{Characters Involved}
:Location: %^{Location}
:Chapter: %^{Chapter(s)}
:END:

*** Description
%?

*** Consequences
-

*** Connections
- Links to:

*** Notes
- "
     :empty-lines 1))
  "Capture templates for timeline events.")

(defvar org-scribe/plot-thread-capture-templates
  '(("p" "Plot Thread" entry
     (file+headline org-scribe/capture-plot-thread-file "Plot Threads")
     "** %^{Thread Name} %^{Type|Subplot|Main Plot|B-Plot|C-Plot|Thematic Thread}
:PROPERTIES:
:ID: %(org-id-new)
:THREAD-TYPE: %\\2
:STATUS: %^{Status|Emerging|Planned|In Progress|Needs Development|Complete}
:Weight: %^{Weight|1.0|2.0|3.0|4.0|5.0}
:FIRST-APPEARANCE:
:END:

*** Description

%^{Brief description of this plot thread}

*** Connection to Main Plot

%^{How does this thread connect to or support the main plot?}

*** Key Scenes

- %?

*** Resolution

[How should this thread resolve?]

*** Notes

[Quick capture notes - can be messy]
"
     :empty-lines 1))
  "Capture templates for plot threads.")

;;; Capture Function

;;;###autoload
(defun org-scribe/capture-to-file ()
  "Capture notes to writing project or file.
Automatically determines the appropriate notes file based on project structure."
  (interactive)
  ;; Choose the destination that actually exists
  (let ((target (org-scribe/capture-target-file t)))  ; Create if missing
    ;; Build a temporary capture template that points at TARGET
    (let ((org-capture-templates
           org-scribe/capture-templates))
      ;; Run the capture UI
      (org-capture))))

;;;###autoload
(defun org-scribe/capture-character ()
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
  ;; Find or create the characters file
  (let ((target (org-scribe/capture-character-file t)))  ; Create if missing
    ;; Use the character capture template
    (let ((org-capture-templates
           org-scribe/character-capture-templates))
      ;; Run the capture UI
      (org-capture nil "c"))))

;;;###autoload
(defun org-scribe/capture-location ()
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
  ;; Find or create the locations file
  (let ((target (org-scribe/capture-location-file t)))  ; Create if missing
    ;; Use the location capture template
    (let ((org-capture-templates
           org-scribe/location-capture-templates))
      ;; Run the capture UI
      (org-capture nil "l"))))

;;;###autoload
(defun org-scribe/capture-object ()
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
  ;; Find or create the objects file
  (let ((target (org-scribe/capture-object-file t)))  ; Create if missing
    ;; Use the object capture template
    (let ((org-capture-templates
           org-scribe/object-capture-templates))
      ;; Run the capture UI
      (org-capture nil "o"))))

;;;###autoload
(defun org-scribe/capture-timeline ()
  "Capture a timeline event to the timeline file.
Automatically determines the appropriate timeline file based on project structure.
Creates a comprehensive timeline event template with prompts for:
- Event name, Date/time, Story day, Chapter
- Location and characters involved
- Event description
- Consequences and connections
- Type of event (action, revelation, etc.)"
  (interactive)
  ;; Find or create the timeline file
  (let ((target (org-scribe/capture-timeline-file t)))  ; Create if missing
    ;; Use the timeline capture template
    (let ((org-capture-templates
           org-scribe/timeline-capture-templates))
      ;; Run the capture UI
      (org-capture nil "t"))))

;;;###autoload
(defun org-scribe/capture-plot-thread ()
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
  ;; Find or create the plot file
  (let ((target (org-scribe/capture-plot-thread-file t)))  ; Create if missing
    ;; Use the plot thread capture template
    (let ((org-capture-templates
           org-scribe/plot-thread-capture-templates))
      ;; Run the capture UI
      (org-capture nil "p"))))

(provide 'org-scribe-capture)

;;; org-scribe-capture.el ends here
