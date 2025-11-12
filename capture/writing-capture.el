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

;;; Capture Target File Detection

(defun writing/capture-location-file (&optional create-if-missing)
  "Determine the appropriate locations file for org-capture in writing environment.
Uses `project-current' to find the project base directory as reference point.
Returns the file path based on the following priority:
1. plan/locations.org (relative to project root)
2. plan/localizaciones.org - Spanish (relative to project root)
3. locations.org (in project root)
4. localizaciones.org (in project root)
5. current buffer if none of the above exist

If CREATE-IF-MISSING is non-nil, create the first priority locations
file that doesn't exist."
  (let* ((project-dir (if-let ((project (project-current)))
                          (project-root project)
                        (file-name-directory (or (buffer-file-name) default-directory))))
         (locations-subdir-en (expand-file-name "plan/locations.org" project-dir))
         (locations-subdir-es (expand-file-name "plan/localizaciones.org" project-dir))
         (locations-en (expand-file-name "locations.org" project-dir))
         (locations-es (expand-file-name "localizaciones.org" project-dir))
         (target (cond
                  ((file-exists-p locations-subdir-en) locations-subdir-en)
                  ((file-exists-p locations-subdir-es) locations-subdir-es)
                  ((file-exists-p locations-en) locations-en)
                  ((file-exists-p locations-es) locations-es)
                  (t (or (buffer-file-name)
                         (expand-file-name "plan/locations.org" project-dir))))))
    (when (and create-if-missing
               (not (file-exists-p target)))
      (let ((target-dir (file-name-directory target)))
        (unless (file-directory-p target-dir)
          (make-directory target-dir t)))
      (with-temp-file target
        (insert "#+TITLE: Locations & World Building\n")
        (insert (format "#+AUTHOR: %s\n" user-full-name))
        (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d")))
        (insert "* Locations\n\n")
        (insert "This file contains location descriptions and world building details.\n\n")))
    target))

(defun writing/capture-object-file (&optional create-if-missing)
  "Determine the appropriate objects file for org-capture in writing environment.
Uses `project-current' to find the project base directory as reference point.
Returns the file path based on the following priority:
1. plan/objects.org (relative to project root)
2. plan/objetos.org - Spanish (relative to project root)
3. objects.org (in project root)
4. objetos.org (in project root)
5. current buffer if none of the above exist

If CREATE-IF-MISSING is non-nil, create the first priority objects
file that doesn't exist."
  (let* ((project-dir (if-let ((project (project-current)))
                          (project-root project)
                        (file-name-directory (or (buffer-file-name) default-directory))))
         (objects-subdir-en (expand-file-name "plan/objects.org" project-dir))
         (objects-subdir-es (expand-file-name "plan/objetos.org" project-dir))
         (objects-en (expand-file-name "objects.org" project-dir))
         (objects-es (expand-file-name "objetos.org" project-dir))
         (target (cond
                  ((file-exists-p objects-subdir-en) objects-subdir-en)
                  ((file-exists-p objects-subdir-es) objects-subdir-es)
                  ((file-exists-p objects-en) objects-en)
                  ((file-exists-p objects-es) objects-es)
                  (t (or (buffer-file-name)
                         (expand-file-name "plan/objects.org" project-dir))))))
    (when (and create-if-missing
               (not (file-exists-p target)))
      (let ((target-dir (file-name-directory target)))
        (unless (file-directory-p target-dir)
          (make-directory target-dir t)))
      (with-temp-file target
        (insert "#+TITLE: Important Objects\n")
        (insert (format "#+AUTHOR: %s\n" user-full-name))
        (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d")))
        (insert "* Objects\n\n")
        (insert "This file contains descriptions of important objects, artifacts, and items.\n\n")))
    target))

(defun writing/capture-timeline-file (&optional create-if-missing)
  "Determine the appropriate timeline file for org-capture in writing environment.
Uses `project-current' to find the project base directory as reference point.
Returns the file path based on the following priority:
1. plan/timeline.org (relative to project root)
2. plan/cronologia.org - Spanish (relative to project root)
3. timeline.org (in project root)
4. cronologia.org (in project root)
5. current buffer if none of the above exist

If CREATE-IF-MISSING is non-nil, create the first priority timeline
file that doesn't exist."
  (let* ((project-dir (if-let ((project (project-current)))
                          (project-root project)
                        (file-name-directory (or (buffer-file-name) default-directory))))
         (timeline-subdir-en (expand-file-name "plan/timeline.org" project-dir))
         (timeline-subdir-es (expand-file-name "plan/cronologia.org" project-dir))
         (timeline-en (expand-file-name "timeline.org" project-dir))
         (timeline-es (expand-file-name "cronologia.org" project-dir))
         (target (cond
                  ((file-exists-p timeline-subdir-en) timeline-subdir-en)
                  ((file-exists-p timeline-subdir-es) timeline-subdir-es)
                  ((file-exists-p timeline-en) timeline-en)
                  ((file-exists-p timeline-es) timeline-es)
                  (t (or (buffer-file-name)
                         (expand-file-name "plan/timeline.org" project-dir))))))
    (when (and create-if-missing
               (not (file-exists-p target)))
      (let ((target-dir (file-name-directory target)))
        (unless (file-directory-p target-dir)
          (make-directory target-dir t)))
      (with-temp-file target
        (insert "#+TITLE: Story Timeline\n")
        (insert (format "#+AUTHOR: %s\n" user-full-name))
        (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d")))
        (insert "* Timeline\n\n")
        (insert "This file contains the chronological timeline of story events.\n\n")))
    target))

(defun writing/capture-character-file (&optional create-if-missing)
  "Determine the appropriate characters file for org-capture in writing environment.
Uses `project-current' to find the project base directory as reference point.
Returns the file path based on the following priority:
1. plan/characters.org (relative to project root)
2. plan/personajes.org - Spanish (relative to project root)
3. characters.org (in project root)
4. personajes.org (in project root)
5. current buffer if none of the above exist

If CREATE-IF-MISSING is non-nil, create the first priority characters
file that doesn't exist."
  (let* ((project-dir (if-let ((project (project-current)))
                          (project-root project)
                        (file-name-directory (or (buffer-file-name) default-directory))))
         (characters-subdir-en (expand-file-name "plan/characters.org" project-dir))
         (characters-subdir-es (expand-file-name "plan/personajes.org" project-dir))
         (characters-en (expand-file-name "characters.org" project-dir))
         (characters-es (expand-file-name "personajes.org" project-dir))
         (target (cond
                  ((file-exists-p characters-subdir-en) characters-subdir-en)
                  ((file-exists-p characters-subdir-es) characters-subdir-es)
                  ((file-exists-p characters-en) characters-en)
                  ((file-exists-p characters-es) characters-es)
                  (t (or (buffer-file-name)
                         (expand-file-name "plan/characters.org" project-dir))))))
    (when (and create-if-missing
               (not (file-exists-p target)))
      (let ((target-dir (file-name-directory target)))
        (unless (file-directory-p target-dir)
          (make-directory target-dir t)))
      (with-temp-file target
        (insert "#+TITLE: Character Database\n")
        (insert (format "#+AUTHOR: %s\n" user-full-name))
        (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d")))
        (insert "* Characters\n\n")
        (insert "This file contains character profiles for the novel.\n\n")))
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
     (file+headline writing/capture-character-file "Characters")
     "** %^{Character Name}
:PROPERTIES:
:Role: %^{Role|Protagonist|Antagonist|Supporting|Minor}
:Age: %^{Age}
:Gender: %^{Gender}
:Occupation: %^{Occupation}
:First-appearance: %^{First Appearance Chapter}
:END:

*** Physical Description
%?

*** Personality
-

*** Background
-

*** Motivation
-

*** Character Arc
-

*** Relationships
-

*** Notes
- "
     :empty-lines 1))
  "Capture templates for character profiles.")

(defvar writing/location-capture-templates
  '(("l" "Location" entry
     (file+headline writing/capture-location-file "Locations")
     "** %^{Location Name}
:PROPERTIES:
:Type: %^{Type|City|Building|Room|Natural|Region|Country}
:Importance: %^{Importance|Major|Supporting|Minor}
:First-appearance: %^{First Appearance Chapter}
:Climate: %^{Climate}
:Population: %^{Population}
:END:

*** Description
%?

*** Geography
-

*** Culture & Society
-

*** History
-

*** Notable Features
-

*** Plot Significance
-

*** Atmosphere & Mood
-

*** Notes
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
