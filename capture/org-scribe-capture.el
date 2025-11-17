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
(require 'org-scribe-i18n)

;;; File Creation Helpers

(defun org-scribe--create-plot-file (filepath is-short-story)
  "Create a basic plot file for captures.
FILEPATH is the path where the file should be created.
IS-SHORT-STORY determines the structure."
  (with-temp-file filepath
    (insert (format "#+TITLE: %s\n" (org-scribe-i18n file-title-plot)))
    (insert "#+AUTHOR: " user-full-name "\n")
    (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n")
    (insert "#+STARTUP: overview\n\n")

    (if is-short-story
        (progn
          (insert (format "* %s\n\n" (org-scribe-i18n plot-outline-heading)))
          (insert (format "** %s\n\n" (org-scribe-i18n plot-premise-heading)))
          (insert (format "** %s\n\n" (org-scribe-i18n plot-setup-heading)))
          (insert (format "** %s\n\n" (org-scribe-i18n plot-central-conflict)))
          (insert (format "** %s\n\n" (org-scribe-i18n section-resolution)))
          (insert (format "* %s\n\n" (org-scribe-i18n plot-threads-heading)))
          (insert "[Plot threads will appear here when captured]\n\n"))
      ;; Novel structure
      (progn
        (insert (format "* %s\n\n%s\n\n"
                        (org-scribe-i18n plot-premise-heading)
                        (org-scribe-i18n plot-premise-question)))
        (insert (format "* %s\n\n" (org-scribe-i18n plot-main-heading)))
        (insert (format "** %s\n\n" (org-scribe-i18n plot-central-conflict)))
        (insert (format "** %s\n\n" (org-scribe-i18n plot-dramatic-question)))
        (insert (format "* %s\n\n" (org-scribe-i18n plot-subplots-heading)))
        (insert (format "* %s\n\n" (org-scribe-i18n plot-threads-heading)))
        (insert (format "%s\n\n" (org-scribe-i18n plot-threads-instruction)))))))

(defun org-scribe--create-short-story-notes-file (filepath)
  "Create a comprehensive notes.org file for short story projects.
FILEPATH is the path where the file should be created."
  (let ((project-name (file-name-base (directory-file-name (file-name-directory filepath)))))
    (with-temp-file filepath
      (insert (format "#+TITLE: %s\n"
                      (format (org-scribe-i18n file-title-planning) project-name)))
      (insert "#+AUTHOR: " user-full-name "\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n")
      (insert "#+STARTUP: overview\n\n")

      ;; Standard headings for short story notes
      (insert (format "* %s\n\n" (org-scribe-i18n heading-characters)))
      (insert (format "** %s\n" (org-scribe-i18n notes-protagonist-heading)))
      (insert (format ":PROPERTIES:\n:TYPE: %s\n:NAME:\n:AGE:\n:GENDER:\n:END:\n\n"
                      (org-scribe-i18n role-protagonist)))
      (insert "- Personality ::\n- Goal ::\n- Conflict ::\n\n")

      (insert (format "* %s\n\n" (org-scribe-i18n plot-outline-heading)))
      (insert (format "** %s\n\n" (org-scribe-i18n plot-premise-heading)))
      (insert (format "** %s\n\n" (org-scribe-i18n plot-setup-heading)))
      (insert (format "** %s\n\n" (org-scribe-i18n plot-central-conflict)))
      (insert (format "** %s\n\n" (org-scribe-i18n section-resolution)))

      (insert (format "* %s\n\n" (org-scribe-i18n notes-setting-heading)))
      (insert (format "** %s\n\n" (org-scribe-i18n notes-main-location)))
      (insert (format "** %s\n\n" (org-scribe-i18n heading-locations)))

      (insert (format "* %s\n\n" (org-scribe-i18n heading-objects)))

      (insert (format "* %s\n\n" (org-scribe-i18n heading-timeline)))

      (insert (format "* %s\n\n" (org-scribe-i18n notes-research-heading)))

      (insert (format "* %s\n\n" (org-scribe-i18n notes-revision-heading)))

      (insert (format "* %s\n\n" (org-scribe-i18n notes-ideas-heading))))))

(defun org-scribe--create-novel-capture-file (filepath content-type)
  "Create an individual capture file for novel projects.
FILEPATH is the path where the file should be created.
CONTENT-TYPE is 'characters, 'locations, 'objects, 'timeline, or 'notes."
  (with-temp-file filepath
    (pcase content-type
      ('characters
       (insert (format "#+TITLE: %s\n" (org-scribe-i18n file-title-characters)))
       (insert "#+AUTHOR: " user-full-name "\n")
       (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
       ;;(insert (format "* %s\n\n" (org-scribe-i18n heading-characters)))
       )
      ('locations
       (insert (format "#+TITLE: %s\n" (org-scribe-i18n file-title-locations)))
       (insert "#+AUTHOR: " user-full-name "\n")
       (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
       ;;(insert (format "* %s\n\n" (org-scribe-i18n heading-locations)))
       )
      ('objects
       (insert (format "#+TITLE: %s\n" (org-scribe-i18n file-title-objects)))
       (insert "#+AUTHOR: " user-full-name "\n")
       (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
       ;;(insert (format "* %s\n\n" (org-scribe-i18n heading-objects)))
       )
      ('timeline
       (insert (format "#+TITLE: %s\n" (org-scribe-i18n file-title-timeline)))
       (insert "#+AUTHOR: " user-full-name "\n")
       (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
       ;;(insert (format "* %s\n\n" (org-scribe-i18n heading-timeline)))
       )
      ('notes
       (insert (format "#+TITLE: %s\n" (org-scribe-i18n file-title-notes)))
       (insert "#+AUTHOR: " user-full-name "\n")
       (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
       (insert (format "* %s\n\n" (org-scribe-i18n heading-notes)))))))

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
For novels: Uses plan/locations.org (or localizaciones.org)
For short stories: Uses notes.org (or notas.org)
For unknown projects: Falls back to plan/locations.org

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
      (org-scribe--create-capture-file target project-type 'locations))

    target))

(defun org-scribe/capture-object-file (&optional create-if-missing)
  "Determine the appropriate file for object captures.
For novels: Uses plan/objects.org (or objetos.org)
For short stories: Uses notes.org (or notas.org)
For unknown projects: Falls back to plan/objects.org

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
      (org-scribe--create-capture-file target project-type 'objects))

    target))

(defun org-scribe/capture-timeline-file (&optional create-if-missing)
  "Determine the appropriate file for timeline captures.
For novels: Uses plan/timeline.org (or cronologia.org)
For short stories: Uses notes.org (or notas.org)
For unknown projects: Falls back to plan/timeline.org

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
      (org-scribe--create-capture-file target project-type 'timeline))

    target))

(defun org-scribe/capture-character-file (&optional create-if-missing)
  "Determine the appropriate file for character captures.
For novels: Uses plan/characters.org (or personajes.org)
For short stories: Uses notes.org (or notas.org)
For unknown projects: Falls back to plan/characters.org

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
      (org-scribe--create-capture-file target project-type 'characters))

    target))

(defun org-scribe/capture-plot-thread-file (&optional create-if-missing)
  "Determine the appropriate file for plot thread captures.
For novels: Uses plan/plot.org (or plan/trama.org)
For short stories: Uses notes.org (or notas.org)
For unknown projects: Falls back to plan/plot.org

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

           ;; Novel or unknown: use plan/plot.org
           (t
            (cond
             ((file-exists-p (expand-file-name "plan/plot.org" project-dir))
              (expand-file-name "plan/plot.org" project-dir))
             ((file-exists-p (expand-file-name "plan/trama.org" project-dir))
              (expand-file-name "plan/trama.org" project-dir))
             ((file-exists-p (expand-file-name "plot.org" project-dir))
              (expand-file-name "plot.org" project-dir))
             (t (expand-file-name "plan/plot.org" project-dir)))))))

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

;;; Capture Template Generation

(defun org-scribe--character-capture-template ()
  "Generate character capture template with i18n strings."
  (let ((name (org-scribe-i18n capture-character-name))
        (role (org-scribe-i18n capture-character-role))
        (age (org-scribe-i18n capture-character-age))
        (gender (org-scribe-i18n capture-character-gender))
        (occupation (org-scribe-i18n capture-character-occupation))
        (first-app (org-scribe-i18n capture-character-first-appearance))
        (role-prot (org-scribe-i18n role-protagonist))
        (role-antag (org-scribe-i18n role-antagonist))
        (role-supp (org-scribe-i18n role-supporting))
        (role-minor (org-scribe-i18n role-minor))
        (phys-desc (org-scribe-i18n section-physical-description))
        (pers (org-scribe-i18n section-personality))
        (background (org-scribe-i18n section-background))
        (gmc (org-scribe-i18n section-goal-motivation-conflict))
        (internal (org-scribe-i18n section-internal))
        (external (org-scribe-i18n section-external))
        (arc (org-scribe-i18n section-character-arc))
        (rels (org-scribe-i18n section-relationships))
        (notes (org-scribe-i18n section-notes)))
    (format "* %%^{%s}
:PROPERTIES:
:ID: %%(org-id-new)
:Role: %%^{%s|%s|%s|%s|%s}
:Age: %%^{%s}
:Gender: %%^{%s}
:Occupation: %%^{%s}
:First-appearance: %%^{%s}
:END:

** %s

- Height ::
- Build ::
- Hair ::
- Eyes ::
- Distinctive features ::

** %s

- Main traits ::
- Strengths ::
- Weaknesses ::
- Fears ::
- Desire ::
- Need ::
- Psychological Flaw ::
- Moral Flaw ::

** %s

- Family ::
- Education ::
- Occupation ::
- Formative events ::

** %s
*** %s
- Goal ::
- Motivation ::
- Conflict ::
*** %s
- Goal ::
- Motivation ::
- Conflict ::

** %s

- Initial state ::
- Turning point ::
- Transformation ::
- Final state ::

** %s

- With other characters ::

** %s
- "
            name role role-prot role-antag role-supp role-minor
            age gender occupation first-app
            phys-desc pers background
            gmc internal external arc rels notes)))

(defun org-scribe--location-capture-template ()
  "Generate location capture template with i18n strings."
  (let ((name (org-scribe-i18n capture-location-name))
        (type-label (org-scribe-i18n capture-location-type))
        (importance (org-scribe-i18n capture-location-importance))
        (first-app (org-scribe-i18n capture-character-first-appearance))
        (climate (org-scribe-i18n capture-location-climate))
        (population (org-scribe-i18n capture-location-population))
        (type-city (org-scribe-i18n type-city))
        (type-building (org-scribe-i18n type-building))
        (type-room (org-scribe-i18n type-room))
        (type-natural (org-scribe-i18n type-natural))
        (type-region (org-scribe-i18n type-region))
        (type-country (org-scribe-i18n type-country))
        (imp-major (org-scribe-i18n importance-major))
        (imp-supp (org-scribe-i18n importance-supporting))
        (imp-minor (org-scribe-i18n importance-minor))
        (gen-desc (org-scribe-i18n section-general-description))
        (geog (org-scribe-i18n section-geography))
        (cultural (org-scribe-i18n section-cultural-aspects))
        (history (org-scribe-i18n section-history))
        (features (org-scribe-i18n section-notable-features))
        (plot-imp (org-scribe-i18n section-plot-importance))
        (places (org-scribe-i18n section-specific-places))
        (atmos (org-scribe-i18n section-atmosphere))
        (map (org-scribe-i18n section-map-reference))
        (notes (org-scribe-i18n section-notes)))
    (format "* %%^{%s}
:PROPERTIES:
:ID: %%(org-id-new)
:Type: %%^{%s|%s|%s|%s|%s|%s|%s}
:Importance: %%^{%s|%s|%s|%s}
:First-appearance: %%^{%s}
:Climate: %%^{%s}
:Population: %%^{%s}
:END:

** %s
%%?

** %s

- Location ::
- Terrain ::
- Climate ::
- Natural resources ::

** %s

- Language ::
- Customs ::
- Religion ::
- Government ::

** %s
-

** %s
-

** %s
-

** %s
-

** %s
-

** %s
-

** %s
- "
            name type-label type-city type-building type-room type-natural type-region type-country
            importance imp-major imp-supp imp-minor first-app climate population
            gen-desc geog cultural history features plot-imp places atmos map notes)))

(defun org-scribe--object-capture-template ()
  "Generate object capture template with i18n strings."
  (let ((name (org-scribe-i18n capture-object-name))
        (type-label (org-scribe-i18n capture-object-type))
        (owner (org-scribe-i18n capture-object-owner))
        (first-app (org-scribe-i18n capture-character-first-appearance))
        (status-label (org-scribe-i18n capture-object-status))
        (type-magical (org-scribe-i18n object-type-magical))
        (type-artifact (org-scribe-i18n object-type-artifact))
        (type-weapon (org-scribe-i18n object-type-weapon))
        (type-tool (org-scribe-i18n object-type-tool))
        (type-symbolic (org-scribe-i18n object-type-symbolic))
        (type-tech (org-scribe-i18n object-type-technology))
        (status-active (org-scribe-i18n status-active))
        (status-lost (org-scribe-i18n status-lost))
        (status-destroyed (org-scribe-i18n status-destroyed))
        (status-hidden (org-scribe-i18n status-hidden))
        (phys-desc (org-scribe-i18n section-physical-description))
        (origin (org-scribe-i18n section-origin))
        (properties (org-scribe-i18n section-properties))
        (plot-imp (org-scribe-i18n section-plot-importance))
        (obj-hist (org-scribe-i18n section-object-history))
        (symbolism (org-scribe-i18n section-symbolism))
        (curr-loc (org-scribe-i18n section-current-location))
        (rules (org-scribe-i18n section-rules-limitations))
        (notes (org-scribe-i18n section-notes)))
    (format "* %%^{%s}
:PROPERTIES:
:ID: %%(org-id-new)
:Type: %%^{%s|%s|%s|%s|%s|%s|%s}
:Owner: %%^{%s}
:First-appearance: %%^{%s}
:Status: %%^{%s|%s|%s|%s|%s}
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
            name type-label type-magical type-artifact type-weapon type-tool type-symbolic type-tech
            owner first-app status-label status-active status-lost status-destroyed status-hidden
            phys-desc origin properties plot-imp obj-hist symbolism curr-loc rules notes)))

(defun org-scribe--timeline-capture-template ()
  "Generate timeline capture template with i18n strings."
  (let ((event (org-scribe-i18n capture-timeline-event))
        (type-label (org-scribe-i18n capture-timeline-type))
        (date (org-scribe-i18n capture-timeline-date))
        (characters (org-scribe-i18n capture-timeline-characters))
        (location (org-scribe-i18n capture-timeline-location))
        (chapter (org-scribe-i18n capture-timeline-chapter))
        (type-action (org-scribe-i18n timeline-type-action))
        (type-revelation (org-scribe-i18n timeline-type-revelation))
        (type-character (org-scribe-i18n timeline-type-character))
        (type-world (org-scribe-i18n timeline-type-world))
        (type-backstory (org-scribe-i18n timeline-type-backstory))
        (desc (org-scribe-i18n section-description))
        (conseq (org-scribe-i18n section-consequences))
        (connect (org-scribe-i18n section-connections))
        (notes (org-scribe-i18n section-notes)))
    (format "* %%^{%s}
:PROPERTIES:
:ID: %%(org-id-new)
:Type: %%^{%s|%s|%s|%s|%s|%s}
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
- Links to:

*** %s
- "
            event type-label type-action type-revelation type-character type-world type-backstory
            date characters location chapter
            desc conseq connect notes)))

(defun org-scribe--plot-thread-capture-template ()
  "Generate plot thread capture template with i18n strings."
  (let ((thread (org-scribe-i18n capture-plot-thread))
        (type-label (org-scribe-i18n capture-plot-type))
        (status-label (org-scribe-i18n capture-plot-status))
        (type-subplot (org-scribe-i18n plot-type-subplot))
        (type-main (org-scribe-i18n plot-type-main))
        (type-b (org-scribe-i18n plot-type-b-plot))
        (type-c (org-scribe-i18n plot-type-c-plot))
        (type-thematic (org-scribe-i18n plot-type-thematic))
        (status-emerging (org-scribe-i18n plot-status-emerging))
        (status-planned (org-scribe-i18n plot-status-planned))
        (status-progress (org-scribe-i18n plot-status-in-progress))
        (status-needs (org-scribe-i18n plot-status-needs-dev))
        (status-complete (org-scribe-i18n plot-status-complete))
        (desc (org-scribe-i18n section-description))
        (connection (org-scribe-i18n section-connection-main-plot))
        (key-scenes (org-scribe-i18n section-key-scenes))
        (resolution (org-scribe-i18n section-resolution))
        (notes (org-scribe-i18n section-notes)))
    (format "** %%^{%s} %%^{%s|%s|%s|%s|%s|%s}
:PROPERTIES:
:ID: %%(org-id-new)
:THREAD-TYPE: %%\\2
:STATUS: %%^{%s|%s|%s|%s|%s|%s}
:FIRST-APPEARANCE:
:END:

*** %s

%%^{Brief description of this plot thread}

*** %s

%%^{How does this thread connect to or support the main plot?}

*** %s

- %%?

*** %s

[How should this thread resolve?]

*** %s

[Quick capture notes - can be messy]
"
            thread type-label type-subplot type-main type-b type-c type-thematic
            status-label status-emerging status-planned status-progress status-needs status-complete
            desc connection key-scenes resolution notes)))

;;; Capture Functions

;;;###autoload
(defun org-scribe/capture-to-file ()
  "Capture notes to writing project or file.
Automatically determines the appropriate notes file based on project structure."
  (interactive)
  (let* ((target (org-scribe/capture-target-file t))
         (org-capture-templates
          `(("w" "Writing Note" entry
             (file+headline ,target ,(org-scribe-i18n heading-notes))
             "** TODO %?\n  %U\n  %i"
             :empty-lines 1))))
    (org-capture)))

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
  (let* ((target (org-scribe/capture-character-file t))
         (template (org-scribe--character-capture-template))
         (org-capture-templates
          `(("c" "Character Profile" entry
             (file ,target)
             ,template
             :empty-lines 1))))
    (org-capture nil "c")))

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
  (let* ((target (org-scribe/capture-location-file t))
         (template (org-scribe--location-capture-template))
         (org-capture-templates
          `(("l" "Location" entry
             (file ,target)
             ,template
             :empty-lines 1))))
    (org-capture nil "l")))

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
  (let* ((target (org-scribe/capture-object-file t))
         (template (org-scribe--object-capture-template))
         (org-capture-templates
          `(("o" "Object" entry
             (file ,target)
             ,template
             :empty-lines 1))))
    (org-capture nil "o")))

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
  (let* ((target (org-scribe/capture-timeline-file t))
         (template (org-scribe--timeline-capture-template))
         (org-capture-templates
          `(("t" "Timeline Event" entry
             (file ,target)
             ,template
             :empty-lines 1))))
    (org-capture nil "t")))

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
  (let* ((target (org-scribe/capture-plot-thread-file t))
         (template (org-scribe--plot-thread-capture-template))
         (org-capture-templates
          `(("p" "Plot Thread" entry
             (file+headline ,target ,(org-scribe-i18n plot-threads-heading))
             ,template
             :empty-lines 1))))
    (org-capture nil "p")))

(provide 'org-scribe-capture)

;;; org-scribe-capture.el ends here
