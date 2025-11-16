;;; writing-capture.el --- Capture system for emacs-writing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Project-aware capture system for writing notes.
;; Automatically determines the appropriate notes file based on project structure.

;;; Code:

(require 'org-capture)
(require 'project)
(require 'writing-core)
(require 'writing-config)

;;; File Creation Helpers

(defun writing--create-short-story-notes-file (filepath)
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

(defun writing--create-novel-capture-file (filepath content-type)
  "Create an individual capture file for novel projects.
FILEPATH is the path where the file should be created.
CONTENT-TYPE is 'characters, 'locations, 'objects, 'timeline, or 'notes."
  (with-temp-file filepath
    (pcase content-type
      ('characters
       (insert "#+TITLE: Character Database\n")
       (insert "#+AUTHOR: " user-full-name "\n")
       (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
       ;;(insert "* Characters\n\n")
       )
      ('locations
       (insert "#+TITLE: Locations & World Building\n")
       (insert "#+AUTHOR: " user-full-name "\n")
       (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
       (insert "* Locations\n\n"))
      ('objects
       (insert "#+TITLE: Important Objects\n")
       (insert "#+AUTHOR: " user-full-name "\n")
       (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
       (insert "* Objects\n\n"))
      ('timeline
       (insert "#+TITLE: Story Timeline\n")
       (insert "#+AUTHOR: " user-full-name "\n")
       (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
       (insert "* Timeline\n\n"))
      ('notes
       (insert "#+TITLE: Writing Notes\n")
       (insert "#+AUTHOR: " user-full-name "\n")
       (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
       (insert "* Notes\n\n")))))

(defun writing--create-capture-file (filepath project-type content-type)
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
      (writing--create-short-story-notes-file filepath)
    ;; Novel or unknown: create individual file
    (writing--create-novel-capture-file filepath content-type)))

;;; Capture Target File Detection

(defun writing/capture-location-file (&optional create-if-missing)
  "Determine the appropriate file for location captures.
For novels: Uses plan/locations.org (or localizaciones.org)
For short stories: Uses notes.org (or notas.org)
For unknown projects: Falls back to plan/locations.org

If CREATE-IF-MISSING is non-nil, create the file if it doesn't exist."
  (let* ((project-dir (if-let ((project (project-current)))
                          (project-root project)
                        (file-name-directory (or (buffer-file-name) default-directory))))
         (project-type (writing-project-type))
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

           ;; Novel or unknown: use plan/locations.org
           (t
            (cond
             ((file-exists-p (expand-file-name "plan/locations.org" project-dir))
              (expand-file-name "plan/locations.org" project-dir))
             ((file-exists-p (expand-file-name "plan/localizaciones.org" project-dir))
              (expand-file-name "plan/localizaciones.org" project-dir))
             ((file-exists-p (expand-file-name "locations.org" project-dir))
              (expand-file-name "locations.org" project-dir))
             ((file-exists-p (expand-file-name "localizaciones.org" project-dir))
              (expand-file-name "localizaciones.org" project-dir))
             (t (expand-file-name "plan/locations.org" project-dir)))))))

    ;; Create file if needed
    (when (and create-if-missing (not (file-exists-p target)))
      (writing--create-capture-file target project-type 'locations))

    target))

(defun writing/capture-object-file (&optional create-if-missing)
  "Determine the appropriate file for object captures.
For novels: Uses plan/objects.org (or objetos.org)
For short stories: Uses notes.org (or notas.org)
For unknown projects: Falls back to plan/objects.org

If CREATE-IF-MISSING is non-nil, create the file if it doesn't exist."
  (let* ((project-dir (if-let ((project (project-current)))
                          (project-root project)
                        (file-name-directory (or (buffer-file-name) default-directory))))
         (project-type (writing-project-type))
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

           ;; Novel or unknown: use plan/objects.org
           (t
            (cond
             ((file-exists-p (expand-file-name "plan/objects.org" project-dir))
              (expand-file-name "plan/objects.org" project-dir))
             ((file-exists-p (expand-file-name "plan/objetos.org" project-dir))
              (expand-file-name "plan/objetos.org" project-dir))
             ((file-exists-p (expand-file-name "objects.org" project-dir))
              (expand-file-name "objects.org" project-dir))
             ((file-exists-p (expand-file-name "objetos.org" project-dir))
              (expand-file-name "objetos.org" project-dir))
             (t (expand-file-name "plan/objects.org" project-dir)))))))

    ;; Create file if needed
    (when (and create-if-missing (not (file-exists-p target)))
      (writing--create-capture-file target project-type 'objects))

    target))

(defun writing/capture-timeline-file (&optional create-if-missing)
  "Determine the appropriate file for timeline captures.
For novels: Uses plan/timeline.org (or cronologia.org)
For short stories: Uses notes.org (or notas.org)
For unknown projects: Falls back to plan/timeline.org

If CREATE-IF-MISSING is non-nil, create the file if it doesn't exist."
  (let* ((project-dir (if-let ((project (project-current)))
                          (project-root project)
                        (file-name-directory (or (buffer-file-name) default-directory))))
         (project-type (writing-project-type))
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

           ;; Novel or unknown: use plan/timeline.org
           (t
            (cond
             ((file-exists-p (expand-file-name "plan/timeline.org" project-dir))
              (expand-file-name "plan/timeline.org" project-dir))
             ((file-exists-p (expand-file-name "plan/cronologia.org" project-dir))
              (expand-file-name "plan/cronologia.org" project-dir))
             ((file-exists-p (expand-file-name "timeline.org" project-dir))
              (expand-file-name "timeline.org" project-dir))
             ((file-exists-p (expand-file-name "cronologia.org" project-dir))
              (expand-file-name "cronologia.org" project-dir))
             (t (expand-file-name "plan/timeline.org" project-dir)))))))

    ;; Create file if needed
    (when (and create-if-missing (not (file-exists-p target)))
      (writing--create-capture-file target project-type 'timeline))

    target))

(defun writing/capture-character-file (&optional create-if-missing)
  "Determine the appropriate file for character captures.
For novels: Uses plan/characters.org (or personajes.org)
For short stories: Uses notes.org (or notas.org)
For unknown projects: Falls back to plan/characters.org

If CREATE-IF-MISSING is non-nil, create the file if it doesn't exist."
  (let* ((project-dir (if-let ((project (project-current)))
                          (project-root project)
                        (file-name-directory (or (buffer-file-name) default-directory))))
         (project-type (writing-project-type))
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

           ;; Novel or unknown: use plan/characters.org
           (t
            (cond
             ((file-exists-p (expand-file-name "plan/characters.org" project-dir))
              (expand-file-name "plan/characters.org" project-dir))
             ((file-exists-p (expand-file-name "plan/personajes.org" project-dir))
              (expand-file-name "plan/personajes.org" project-dir))
             ((file-exists-p (expand-file-name "characters.org" project-dir))
              (expand-file-name "characters.org" project-dir))
             ((file-exists-p (expand-file-name "personajes.org" project-dir))
              (expand-file-name "personajes.org" project-dir))
             (t (expand-file-name "plan/characters.org" project-dir)))))))

    ;; Create file if needed
    (when (and create-if-missing (not (file-exists-p target)))
      (writing--create-capture-file target project-type 'characters))

    target))

(defun writing/capture-target-file (&optional create-if-missing)
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

(defvar writing/capture-templates
  '(("w" "Writing Note" entry
     (file+headline writing/capture-target-file "Notes")
     "** TODO %?\n  %U\n  %i"
     :empty-lines 1))
  "Capture templates specific to the writing environment.")

(defvar writing/character-capture-templates
  '(("c" "Character Profile" entry
     (file writing/capture-character-file)
     "* %^{Character Name}
:PROPERTIES:
:ID: %(org-id-new)
:Role: %^{Role|Protagonist|Antagonist|Supporting|Minor}
:Age: %^{Age}
:Gender: %^{Gender}
:Occupation: %^{Occupation}
:First-appearance: %^{First Appearance Chapter}
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
     :empty-lines 1))
  "Capture templates for character profiles.")

(defvar writing/location-capture-templates
  '(("l" "Location" entry
     (file writing/capture-location-file)
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

(defvar writing/object-capture-templates
  '(("o" "Object" entry
     (file+headline writing/capture-object-file "Objects")
     "** %^{Object Name}
:PROPERTIES:
:Type: %^{Type|Magical|Artifact|Weapon|Tool|Symbolic|Technology}
:Owner: %^{Current Owner}
:First-appearance: %^{First Appearance Chapter}
:Status: %^{Status|Active|Lost|Destroyed|Hidden}
:END:

*** Physical Description
%?

*** Properties & Abilities
-

*** Origin & History
-

*** Significance to Plot
-

*** Current Location
-

*** Rules & Limitations
-

*** Notes
- "
     :empty-lines 1))
  "Capture templates for important objects.")

(defvar writing/timeline-capture-templates
  '(("t" "Timeline Event" entry
     (file+headline writing/capture-timeline-file "Timeline")
     "** %^{Event Name}
:PROPERTIES:
:Date: %^{Date/Time in Story}
:Story-day: %^{Story Day (e.g., Day 1, Day 15)}
:Chapter: %^{Chapter(s)}
:Location: %^{Location}
:Characters: %^{Characters Involved}
:Type: %^{Type|Action|Revelation|Character|World|Backstory}
:END:

*** Event Description
%?

*** Consequences
-

*** Connections
- Links to:

*** Notes
- "
     :empty-lines 1))
  "Capture templates for timeline events.")

;;; Capture Function

;;;###autoload
(defun writing/capture-to-file ()
  "Capture notes to writing project or file.
Automatically determines the appropriate notes file based on project structure."
  (interactive)
  ;; Choose the destination that actually exists
  (let ((target (writing/capture-target-file t)))  ; Create if missing
    ;; Build a temporary capture template that points at TARGET
    (let ((org-capture-templates
           writing/capture-templates))
      ;; Run the capture UI
      (org-capture))))

;;;###autoload
(defun writing/capture-character ()
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
  (let ((target (writing/capture-character-file t)))  ; Create if missing
    ;; Use the character capture template
    (let ((org-capture-templates
           writing/character-capture-templates))
      ;; Run the capture UI
      (org-capture nil "c"))))

;;;###autoload
(defun writing/capture-location ()
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
  (let ((target (writing/capture-location-file t)))  ; Create if missing
    ;; Use the location capture template
    (let ((org-capture-templates
           writing/location-capture-templates))
      ;; Run the capture UI
      (org-capture nil "l"))))

;;;###autoload
(defun writing/capture-object ()
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
  (let ((target (writing/capture-object-file t)))  ; Create if missing
    ;; Use the object capture template
    (let ((org-capture-templates
           writing/object-capture-templates))
      ;; Run the capture UI
      (org-capture nil "o"))))

;;;###autoload
(defun writing/capture-timeline ()
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
  (let ((target (writing/capture-timeline-file t)))  ; Create if missing
    ;; Use the timeline capture template
    (let ((org-capture-templates
           writing/timeline-capture-templates))
      ;; Run the capture UI
      (org-capture nil "t"))))

(provide 'writing-capture)

;;; writing-capture.el ends here
