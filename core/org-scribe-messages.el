;;; org-scribe-messages.el --- Centralized user-facing messages -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Centralized repository for all user-facing strings in org-scribe,
;; in two languages: English (`org-scribe-messages-en') and Spanish
;; (`org-scribe-messages-es').  `org-scribe-msg' looks up the key in
;; whichever language `org-scribe-message-language' selects, falls back
;; to English when the key is missing from that language's alist, and
;; falls back to the symbol's own name as a last resort — it never
;; signals an error for a genuinely unregistered key.
;;
;; Every message key must be present in BOTH alists; `tests/test-messages.el'
;; enforces this (key-set parity, and matching counts of %s/%d format
;; specifiers between the two languages for the same key).
;;
;; Usage:
;;   (org-scribe-msg 'default-scene-name)
;;   => "New scene"                (English, the default)
;;
;;   (let ((org-scribe-message-language 'es))
;;     (org-scribe-msg 'default-scene-name))
;;   => "Escena nueva"
;;
;;   (org-scribe-msg 'msg-inserted-link "Alex")
;;   => "Inserted link to Alex"

;;; Code:

;; `org-scribe-message-language' is a `defcustom' in
;; core/org-scribe-config.el (per the project's convention that all
;; defcustoms live there), but this file loads before config.el in the
;; module load order (see org-scribe.el) and `org-scribe-msg' needs the
;; variable at load time.  A plain `defvar' here supplies the default
;; ('en) until config.el's `defcustom' runs and adds the customize
;; metadata on top — `defcustom', like `defvar', does not override an
;; already-bound value, so this is safe and the two forms cooperate
;; rather than conflict.
(defvar org-scribe-message-language 'en
  "Language for user-facing messages: `en' (English) or `es' (Spanish).
The real `defcustom' lives in `core/org-scribe-config.el'; see this
file's Commentary for why a forward `defvar' is needed here too.")

(defconst org-scribe-messages-en
  '(
    ;; Default values
    (default-scene-name . "New scene")
    (default-chapter-name . "New chapter")

    ;; Project creation
    (project-creation-base-dir . "Base directory for project: ")
    (project-creation-novel-title . "Novel title: ")
    (project-creation-short-story-title . "Short story title: ")
    (project-creation-language-prompt . "Template language: ")
    (project-creation-success-novel . "Novel project '%s' created successfully at %s")
    (project-creation-success-short-story . "Short story project '%s' created successfully at %s")
    (project-already-exists . "Project directory '%s' already exists!")
    (msg-projects-registered . "Scanned and registered %d project(s) under %s")

    ;; Writing templates
    (scene-name-prompt . "Scene name: ")
    (chapter-name-prompt . "Chapter name: ")
    (not-in-org-mode . "This command can only be used in org-mode buffers")
    (not-in-novel-project . "Not in a novel project directory")

    ;; Capture
    (capture-character-name . "Character Name")
    (capture-location-type . "Location Type")
    (capture-object-type . "Object Type")

    ;; Capture — file headers / titles
    (capture-title-plot-structure . "Plot Structure")
    (capture-title-project-notes . "%s - Planning & Notes")
    (capture-title-characters . "Character Database")
    (capture-title-locations . "Locations & World Building")
    (capture-title-objects . "Important Objects")
    (capture-title-timeline . "Story Timeline")
    (capture-title-notes . "Writing Notes")

    ;; Capture — plot file content
    (capture-plot-outline . "Plot Outline")
    (capture-plot-premise . "Premise")
    (capture-plot-premise-hint . "What is the story about in one or two sentences?")
    (capture-plot-setup . "Setup")
    (capture-plot-central-conflict . "Central Conflict")
    (capture-plot-resolution . "Resolution")
    (capture-plot-threads . "Plot Threads")
    (capture-plot-threads-hint-short . "[Plot threads will appear here when captured]")
    (capture-plot-threads-hint-novel . "Track your plot threads here. Use F8 F8 p to capture new threads.")
    (capture-plot-main-plot . "Main Plot")
    (capture-plot-main-dramatic-question . "Main Dramatic Question")
    (capture-plot-subplots . "Subplots")
    (capture-notes-heading . "Notes")

    ;; Capture — short story notes.org content
    (capture-ss-characters . "Characters")
    (capture-ss-protagonist-name . "Protagonist: [Name]")
    (capture-ss-personality . "Personality ::")
    (capture-ss-goal . "Goal ::")
    (capture-ss-conflict . "Conflict ::")
    (capture-ss-setting . "Setting")
    (capture-ss-main-locations . "Main Location(s)")
    (capture-ss-locations . "Locations")
    (capture-ss-objects . "Objects")
    (capture-ss-timeline . "Timeline")
    (capture-ss-research . "Research & References")
    (capture-ss-revision-notes . "Revision Notes")
    (capture-ss-random-ideas . "Random Ideas & Inspiration")

    ;; Capture — writing note template
    (capture-writing-note-key . "w")
    (capture-writing-note-name . "Writing Note")

    ;; Capture — character template
    (capture-char-key . "c")
    (capture-char-name . "Character Profile")
    (capture-char-name-prompt . "Character Name")
    (capture-char-role-prompt . "Role|Protagonist|Opponent|Antagonist|Supporting|Minor|Ally|Mentor")
    (capture-char-weight-prompt . "Weight|1.0|2.0|3.0|4.0|5.0")
    (capture-char-age-prompt . "Age")
    (capture-char-gender-prompt . "Gender")
    (capture-char-occupation-prompt . "Occupation")
    (capture-char-first-appearance-prompt . "First Appearance Chapter")
    (capture-char-physical-description . "Physical Description")
    (capture-char-height . "Height ::")
    (capture-char-build . "Build ::")
    (capture-char-hair . "Hair ::")
    (capture-char-eyes . "Eyes ::")
    (capture-char-distinctive-features . "Distinctive features ::")
    (capture-char-personality . "Personality")
    (capture-char-main-traits . "Main traits ::")
    (capture-char-strengths . "Strengths ::")
    (capture-char-weaknesses . "Weaknesses ::")
    (capture-char-fears . "Fears ::")
    (capture-char-desire . "Desire ::")
    (capture-char-need . "Need ::")
    (capture-char-psychological-flaw . "Psychological Flaw ::")
    (capture-char-moral-flaw . "Moral Flaw ::")
    (capture-char-background . "Background")
    (capture-char-family . "Family ::")
    (capture-char-education . "Education ::")
    (capture-char-occupation-field . "Occupation ::")
    (capture-char-formative-events . "Formative events ::")
    (capture-char-gmc . "Goal, Motivation, Conflict")
    (capture-char-internal . "Internal")
    (capture-char-external . "External")
    (capture-char-goal . "Goal ::")
    (capture-char-motivation . "Motivation ::")
    (capture-char-conflict . "Conflict ::")
    (capture-char-arc . "Character Arc")
    (capture-char-initial-state . "Initial state ::")
    (capture-char-turning-point . "Turning point ::")
    (capture-char-transformation . "Transformation ::")
    (capture-char-final-state . "Final state ::")
    (capture-char-relationships . "Relationships")
    (capture-char-with-others . "With other characters ::")
    (capture-char-notes . "Notes")

    ;; Capture — location template
    (capture-loc-key . "l")
    (capture-loc-name . "Location")
    (capture-loc-name-prompt . "Location Name")
    (capture-loc-type-prompt . "Type|City|Building|Room|Natural|Region|Country")
    (capture-loc-importance-prompt . "Importance|Major|Supporting|Minor")
    (capture-loc-first-appearance-prompt . "First Appearance Chapter")
    (capture-loc-climate-prompt . "Climate")
    (capture-loc-population-prompt . "Population")
    (capture-loc-general-description . "General Description")
    (capture-loc-geography . "Geography")
    (capture-loc-location . "Location ::")
    (capture-loc-terrain . "Terrain ::")
    (capture-loc-climate . "Climate ::")
    (capture-loc-natural-resources . "Natural resources ::")
    (capture-loc-culture . "Cultural Aspects & Society")
    (capture-loc-language . "Language ::")
    (capture-loc-customs . "Customs ::")
    (capture-loc-religion . "Religion ::")
    (capture-loc-government . "Government ::")
    (capture-loc-history . "History")
    (capture-loc-notable-features . "Notable Features")
    (capture-loc-importance-plot . "Importance in the Plot")
    (capture-loc-specific-places . "Specific Places")
    (capture-loc-atmosphere . "Atmosphere & Mood")
    (capture-loc-map-reference . "Map/Reference Image")
    (capture-loc-notes . "Notes")

    ;; Capture — object template
    (capture-obj-key . "o")
    (capture-obj-name . "Object")
    (capture-obj-name-prompt . "Object Name")
    (capture-obj-type-prompt . "Type|Magical|Artifact|Weapon|Tool|Symbolic|Technology")
    (capture-obj-owner-prompt . "Current Owner")
    (capture-obj-first-appearance-prompt . "First Appearance Chapter")
    (capture-obj-status-prompt . "Status|Active|Lost|Destroyed|Hidden")
    (capture-obj-physical-description . "Physical Description")
    (capture-obj-origin . "Origin")
    (capture-obj-properties . "Properties")
    (capture-obj-importance-plot . "Importance in the Plot")
    (capture-obj-history . "Object History")
    (capture-obj-symbolism . "Symbolism")
    (capture-obj-current-location . "Current Location")
    (capture-obj-rules . "Rules & Limitations")
    (capture-obj-notes . "Notes")

    ;; Capture — timeline template
    (capture-tl-key . "t")
    (capture-tl-name . "Timeline Event")
    (capture-tl-name-prompt . "Event Name")
    (capture-tl-type-prompt . "Type|Action|Revelation|Character|World|Backstory")
    (capture-tl-date-prompt . "Date/Time in Story")
    (capture-tl-characters-prompt . "Characters Involved")
    (capture-tl-location-prompt . "Location")
    (capture-tl-chapter-prompt . "Chapter(s)")
    (capture-tl-description . "Description")
    (capture-tl-consequences . "Consequences")
    (capture-tl-connections . "Connections")
    (capture-tl-connections-hint . "Links to:")
    (capture-tl-notes . "Notes")

    ;; Capture — plot thread template
    (capture-pt-key . "p")
    (capture-pt-name . "Plot Thread")
    (capture-pt-name-prompt . "Thread Name")
    (capture-pt-type-prompt . "Type|Subplot|Main Plot|B-Plot|C-Plot|Thematic Thread")
    (capture-pt-status-prompt . "Status|Emerging|Planned|In Progress|Needs Development|Complete")
    (capture-pt-weight-prompt . "Weight|1.0|2.0|3.0|4.0|5.0")
    (capture-pt-description . "Description")
    (capture-pt-description-prompt . "Brief description of this plot thread")
    (capture-pt-connection-main . "Connection to Main Plot")
    (capture-pt-connection-main-prompt . "How does this thread connect to or support the main plot?")
    (capture-pt-key-scenes . "Key Scenes")
    (capture-pt-resolution . "Resolution")
    (capture-pt-resolution-hint . "[How should this thread resolve?]")
    (capture-pt-notes . "Notes")
    (capture-pt-notes-hint . "[Quick capture notes - can be messy]")

    ;; Search
    (search-pov-prompt . "Find PoV character [fuzzy]: ")
    (search-pov-prompt-free . "Character (POV) [substring]: ")
    (search-char-prompt . "Find character [fuzzy]: ")
    (search-char-prompt-free . "Character name [substring]: ")
    (search-plot-prompt . "Find plot thread [fuzzy]: ")
    (search-plot-prompt-free . "Plot term [substring]: ")
    (search-loc-prompt . "Find location [fuzzy]: ")
    (search-loc-prompt-free . "Location [substring]: ")
    (msg-no-org-files . "No .org files found in %s and subdirectories")

    ;; Search — edit marker index
    (edits-index-title . "Edit markers")
    (edits-index-section-edits . "Edits")
    (edits-index-section-notes . "Notes")
    (edits-index-other-category . "other")
    (edits-index-no-heading . "(before first heading)")
    (edits-index-none-found . "No edit markers found in %s")
    (edits-index-found . "%d marker%s in %d file%s")
    (edits-index-help . "RET or mouse-1 follows a marker to its place in the manuscript.  g refreshes, q buries.")

    ;; Character linking
    (prompt-select-character . "Select character: ")
    (prompt-select-pov . "Select PoV character: ")
    (prompt-select-characters-multi . "Select character (RET to finish): ")
    (msg-inserted-link . "Inserted link to %s")
    (msg-inserted-links . "Inserted %d character link%s")
    (msg-set-pov . "Set PoV to %s")
    (msg-set-characters . "Set Characters to: %s")
    (msg-updated-pov . "Updated PoV property")
    (msg-updated-characters . "Updated Characters property")
    (msg-updated-pov-and-chars . "Updated PoV and Characters properties")
    (msg-no-updates-needed . "No character properties found or already linked")
    (msg-updated-links . "Updated character links in %d scene%s")
    (msg-added-ids . "Added IDs to %d character heading%s")
    (msg-jump-to-pov . "Jumped to PoV character")
    (msg-no-characters-selected . "No characters selected")
    (msg-character-ids-updated . "Character IDs updated in %s")
    (msg-setting-up-links . "Setting up character linking system...")
    (msg-setup-complete . "Character linking system setup complete!")

    ;; Location linking
    (prompt-select-location . "Select location: ")
    (prompt-select-locations-multi . "Select location (RET to finish): ")
    (msg-set-location . "Set Location to %s")
    (msg-set-locations . "Set Locations to: %s")
    (msg-updated-location . "Updated Location property")
    (msg-updated-locations . "Updated Locations property")
    (msg-updated-location-links . "Updated location links in %d scene%s")
    (msg-added-location-ids . "Added IDs to %d location heading%s")
    (msg-location-ids-updated . "Location IDs updated in %s")
    (msg-no-locations-selected . "No locations selected")
    (msg-inserted-location-links . "Inserted %d location link%s")
    (msg-setting-up-location-links . "Setting up location linking system...")
    (msg-location-setup-complete . "Location linking system setup complete!")

    ;; Plot linking
    (prompt-select-plot-thread . "Select plot thread: ")
    (prompt-select-plot-threads-multi . "Select plot thread (RET to finish): ")
    (msg-set-plot-thread . "Set Plot to %s")
    (msg-set-plot-threads . "Set Plot threads to: %s")
    (msg-updated-plot . "Updated Plot property")
    (msg-updated-plot-threads . "Updated Plot property")
    (msg-updated-plot-links . "Updated plot thread links in %d scene%s")
    (msg-added-plot-ids . "Added IDs to %d plot thread heading%s")
    (msg-plot-ids-updated . "Plot thread IDs updated in %s")
    (msg-no-plot-threads-selected . "No plot threads selected")
    (msg-inserted-plot-links . "Inserted %d plot thread link%s")
    (msg-no-plot-updates-needed . "No Plot property found or already linked")
    (msg-jump-to-plot-thread . "Jumped to plot thread")
    (msg-setting-up-plot-links . "Setting up plot thread linking system...")
    (msg-plot-setup-complete . "Plot thread linking system setup complete!")
    (msg-plot-health-report . "Plot thread health report generated")
    (msg-plot-stats . "Plot threads: %d | Scenes: %d | Threads with warnings: %d")
    (msg-no-plot-property . "No Plot property in current heading")
    (msg-no-plot-threads-in-property . "No plot threads found in Plot property")
    (msg-plot-not-id-link . "Plot thread '%s' is not an ID link")
    (prompt-jump-to-plot . "Jump to plot thread: ")

    ;; Link name updates
    (msg-updated-link-names . "Updated link names")
    (msg-updated-all-link-names . "Updated %s link names in %d scene%s")
    (msg-updated-all-links-scene . "Updated link names in %d scene%s")
    (msg-relink-complete . "Relink complete: refreshed links in %d scene%s in %s")
    (msg-relink-no-novel . "No manuscript file found in this project")
    (msg-no-link-updates . "No %s link names needed updating")
    (msg-updated-pov-link-names . "Updated PoV link names")
    (msg-updated-characters-link-names . "Updated Characters link names")
    (msg-updated-pov-and-chars-link-names . "Updated PoV and Characters link names")
    (msg-updated-plot-link-names . "Updated Plot link names")

    ;; Character relationships
    (prompt-relationship-from-character . "From character: ")
    (prompt-relationship-to-character . "To character: ")
    (prompt-relationship-type . "Relationship type: ")
    (prompt-remove-relationship . "Remove relationship: ")
    (msg-added-relationship . "Added %s relationship: %s → %s")
    (msg-removed-relationship . "Removed %s's relationship: %s")
    (msg-no-relationships . "No relationships found for this character.")
    (msg-no-relationships-in-project . "No relationships found in project.")
    (msg-relationship-setup-complete . "Added RelationshipsData property to %d character%s")
    (msg-no-other-characters . "No other characters found. Create more characters first.")
    (error-no-relationships . "No relationships defined for %s")

    ;; Column view
    (msg-column-view-enabled . "Column view link stripping enabled")
    (msg-column-view-disabled . "Column view link stripping disabled")

    ;; Overlay tooltips
    (msg-overlays-enabled . "Entity tooltips enabled")
    (msg-overlays-disabled . "Entity tooltips disabled")

    ;; Scene word counts
    (msg-scenes-wordcount-updated . "Updated word count on %d scene%s")
    (msg-wordcount-region . "%d words in region")
    (msg-wordcount-buffer . "%d words in buffer")
    (msg-wordcount-degraded . "Counted without org-context-extended: totals include Org metadata. Install it for accurate counts (M-x org-scribe-setup-check).")

    ;; Dictionary / language tools
    (error-word-empty . "Word cannot be empty")
    (error-word-lookup . "Error looking up word: %s")
    (msg-word-not-found . "Word not found: %s")
    (msg-word-suggestions . "Suggestions:")
    (error-random-word . "Error fetching random word: %s")
    (error-word-parse . "Error parsing RAE response: %s")
    (error-random-word-parse . "Error parsing random word response: %s")

    ;; File operations
    (file-not-found . "File %s doesn't exist. Create it? ")
    (file-open-prompt . "Open file: ")

    ;; Questions / confirmations
    (question-link-existing-scenes . "Link characters in existing scenes? ")
    (question-link-existing-locations . "Link locations in existing scenes? ")
    (question-link-existing-plots . "Link plot threads in existing scenes? ")
    (question-create-directory . "Directory %s does not exist. Create it? ")

    ;; Errors — input validation
    (error-empty-title . "Title cannot be empty or contain only whitespace")
    (error-path-separator . "Title cannot contain path separators (/ or \\)")
    (error-title-colon . "Title cannot contain colons (:)")
    (error-title-special-chars . "Title cannot contain special characters (* ? < > | \" ')")
    (error-title-dot . "Title cannot start with a dot (.)")
    (error-title-double-dot . "Title cannot contain double dots (..)")
    (error-empty-character . "Character name cannot be empty")
    (error-empty-location . "Location cannot be empty")
    (error-empty-plot . "Plot term cannot be empty")

    ;; Errors — missing features / files
    (error-no-characters-found . "No characters found. Create characters first or add IDs with org-scribe-add-character-ids.")
    (error-no-locations-found . "No locations found. Create locations first or add IDs with org-scribe-add-location-ids.")
    (error-no-plot-threads-found . "No plot threads found. Create plot threads first or add IDs with org-scribe-add-plot-thread-ids.")
    (error-no-character-file . "No character file found. Create characters first.")
    (error-no-location-file . "No location file found. Create locations first.")
    (error-no-plot-file . "No plot file found. Create plot threads first.")
    (error-no-pov-property . "No PoV property found")
    (error-pov-not-link . "PoV property is not an ID link. Use org-scribe-set-pov-character to create a link.")
    (error-plot-not-link . "Plot property is not an ID link. Use org-scribe-set-scene-plot-threads to create a link.")
    (error-no-id-for-character . "No ID found for %s")
    (error-no-id-for-location . "No ID found for %s")
    (error-no-id-for-plot . "No ID found for %s")
    (error-template-not-found . "Template directory not found: %s")
    (error-org-ql-required . "org-ql package is required for search functions")
    (error-org-context-required . "org-context-extended package is required for accurate word counting")
    (error-writeroom-required . "writeroom-mode is required for writing environment modes")
    (error-feature-not-available . "Feature %s not available. Install required package")
    (error-no-org-file . "Current buffer is not visiting a file; cannot enable `org-scribe-editing-mode'")

    ;; Workspace dispatcher
    (prompt-select-workspace . "Workspace layout: ")
    (msg-workspace-set . "Workspace: %s")
    (msg-workspace-normal . "Workspace: normal editing")
    (error-unknown-workspace . "Unknown workspace layout: %s")

    ;; Health report — text-level statistics
    (msg-health-pov-word-share-heading . "Per-PoV Word Share")
    (msg-health-pov-word-share-table-header . "| PoV | Scenes | Words | % of Total |")
    (msg-health-pov-none-label . "(no PoV)")
    (msg-health-chapter-length-heading . "Chapter Length Spread")
    (msg-health-chapter-length-table-header . "| Chapter | Words |")
    (msg-health-chapter-length-summary . "Min: %d words · Max: %d words · Mean: %.1f words · Median: %.1f words")
    (msg-health-chapter-length-outlier-legend . "* marks a chapter more than ~2x the mean or under ~0.5x — informational, not a judgment.")

    ;; Pluralization helpers (used in code)
    (plural-empty . "")
    (plural-s . "s")
    )
  "English messages.  See `org-scribe-messages-es' for the Spanish set.
Each entry is (KEY . MESSAGE-TEMPLATE) where MESSAGE-TEMPLATE can include
printf-style format specifiers (%s, %d, etc.) for dynamic content.")

(defconst org-scribe-messages-es
  '(
    ;; Default values
    (default-scene-name . "Escena nueva")
    (default-chapter-name . "Capítulo nuevo")

    ;; Project creation
    (project-creation-base-dir . "Directorio base del proyecto: ")
    (project-creation-novel-title . "Título de la novela: ")
    (project-creation-short-story-title . "Título del relato: ")
    (project-creation-language-prompt . "Idioma de la plantilla: ")
    (project-creation-success-novel . "Proyecto de novela «%s» creado correctamente en %s")
    (project-creation-success-short-story . "Proyecto de relato «%s» creado correctamente en %s")
    (project-already-exists . "¡El directorio del proyecto «%s» ya existe!")
    (msg-projects-registered . "Se escanearon y registraron %d proyecto(s) en %s")

    ;; Writing templates
    (scene-name-prompt . "Nombre de la escena: ")
    (chapter-name-prompt . "Nombre del capítulo: ")
    (not-in-org-mode . "Este comando solo se puede usar en búferes de org-mode")
    (not-in-novel-project . "No se encuentra en un directorio de proyecto de novela")

    ;; Capture
    (capture-character-name . "Nombre del personaje")
    (capture-location-type . "Tipo de localización")
    (capture-object-type . "Tipo de objeto")

    ;; Capture — file headers / titles
    (capture-title-plot-structure . "Estructura de la Trama")
    (capture-title-project-notes . "%s - Planificación y Notas")
    (capture-title-characters . "Base de Datos de Personajes")
    (capture-title-locations . "Localizaciones y Ambientación")
    (capture-title-objects . "Objetos Importantes")
    (capture-title-timeline . "Cronología de la Historia")
    (capture-title-notes . "Notas de Escritura")

    ;; Capture — plot file content
    (capture-plot-outline . "Esquema de la Trama")
    (capture-plot-premise . "Premisa")
    (capture-plot-premise-hint . "¿De qué trata la historia en una o dos frases?")
    (capture-plot-setup . "Planteamiento")
    (capture-plot-central-conflict . "Conflicto Central")
    (capture-plot-resolution . "Resolución")
    (capture-plot-threads . "Hilos de Trama")
    (capture-plot-threads-hint-short . "[Los hilos de trama aparecerán aquí al capturarlos]")
    (capture-plot-threads-hint-novel . "Registre aquí sus hilos de trama. Use F8 F8 p para capturar nuevos hilos.")
    (capture-plot-main-plot . "Trama Principal")
    (capture-plot-main-dramatic-question . "Pregunta Dramática Principal")
    (capture-plot-subplots . "Subtramas")
    (capture-notes-heading . "Notas")

    ;; Capture — short story notes.org content
    (capture-ss-characters . "Personajes")
    (capture-ss-protagonist-name . "Protagonista: [Nombre]")
    (capture-ss-personality . "Personalidad ::")
    (capture-ss-goal . "Objetivo ::")
    (capture-ss-conflict . "Conflicto ::")
    (capture-ss-setting . "Ambientación")
    (capture-ss-main-locations . "Localización(es) Principal(es)")
    (capture-ss-locations . "Localizaciones")
    (capture-ss-objects . "Objetos")
    (capture-ss-timeline . "Cronología")
    (capture-ss-research . "Investigación y Referencias")
    (capture-ss-revision-notes . "Notas de Revisión")
    (capture-ss-random-ideas . "Ideas Sueltas e Inspiración")

    ;; Capture — writing note template
    (capture-writing-note-key . "w")
    (capture-writing-note-name . "Nota de Escritura")

    ;; Capture — character template
    (capture-char-key . "c")
    (capture-char-name . "Perfil de Personaje")
    (capture-char-name-prompt . "Nombre del Personaje")
    (capture-char-role-prompt . "Rol|Protagonista|Oponente|Antagonista|Secundario|Menor|Aliado|Mentor")
    (capture-char-weight-prompt . "Peso|1.0|2.0|3.0|4.0|5.0")
    (capture-char-age-prompt . "Edad")
    (capture-char-gender-prompt . "Género")
    (capture-char-occupation-prompt . "Ocupación")
    (capture-char-first-appearance-prompt . "Capítulo de Primera Aparición")
    (capture-char-physical-description . "Descripción Física")
    (capture-char-height . "Altura ::")
    (capture-char-build . "Complexión ::")
    (capture-char-hair . "Pelo ::")
    (capture-char-eyes . "Ojos ::")
    (capture-char-distinctive-features . "Rasgos distintivos ::")
    (capture-char-personality . "Personalidad")
    (capture-char-main-traits . "Rasgos principales ::")
    (capture-char-strengths . "Fortalezas ::")
    (capture-char-weaknesses . "Debilidades ::")
    (capture-char-fears . "Miedos ::")
    (capture-char-desire . "Deseo ::")
    (capture-char-need . "Necesidad ::")
    (capture-char-psychological-flaw . "Defecto psicológico ::")
    (capture-char-moral-flaw . "Defecto moral ::")
    (capture-char-background . "Trasfondo")
    (capture-char-family . "Familia ::")
    (capture-char-education . "Educación ::")
    (capture-char-occupation-field . "Ocupación ::")
    (capture-char-formative-events . "Eventos formativos ::")
    (capture-char-gmc . "Objetivo, Motivación, Conflicto")
    (capture-char-internal . "Interno")
    (capture-char-external . "Externo")
    (capture-char-goal . "Objetivo ::")
    (capture-char-motivation . "Motivación ::")
    (capture-char-conflict . "Conflicto ::")
    (capture-char-arc . "Arco del Personaje")
    (capture-char-initial-state . "Estado inicial ::")
    (capture-char-turning-point . "Punto de inflexión ::")
    (capture-char-transformation . "Transformación ::")
    (capture-char-final-state . "Estado final ::")
    (capture-char-relationships . "Relaciones")
    (capture-char-with-others . "Con otros personajes ::")
    (capture-char-notes . "Notas")

    ;; Capture — location template
    (capture-loc-key . "l")
    (capture-loc-name . "Localización")
    (capture-loc-name-prompt . "Nombre de la Localización")
    (capture-loc-type-prompt . "Tipo|Ciudad|Edificio|Habitación|Natural|Región|País")
    (capture-loc-importance-prompt . "Importancia|Mayor|Secundaria|Menor")
    (capture-loc-first-appearance-prompt . "Capítulo de Primera Aparición")
    (capture-loc-climate-prompt . "Clima")
    (capture-loc-population-prompt . "Población")
    (capture-loc-general-description . "Descripción General")
    (capture-loc-geography . "Geografía")
    (capture-loc-location . "Ubicación ::")
    (capture-loc-terrain . "Terreno ::")
    (capture-loc-climate . "Clima ::")
    (capture-loc-natural-resources . "Recursos naturales ::")
    (capture-loc-culture . "Aspectos Culturales y Sociedad")
    (capture-loc-language . "Idioma ::")
    (capture-loc-customs . "Costumbres ::")
    (capture-loc-religion . "Religión ::")
    (capture-loc-government . "Gobierno ::")
    (capture-loc-history . "Historia")
    (capture-loc-notable-features . "Rasgos Destacables")
    (capture-loc-importance-plot . "Importancia en la Trama")
    (capture-loc-specific-places . "Lugares Específicos")
    (capture-loc-atmosphere . "Atmósfera y Ambiente")
    (capture-loc-map-reference . "Mapa/Imagen de Referencia")
    (capture-loc-notes . "Notas")

    ;; Capture — object template
    (capture-obj-key . "o")
    (capture-obj-name . "Objeto")
    (capture-obj-name-prompt . "Nombre del Objeto")
    (capture-obj-type-prompt . "Tipo|Mágico|Artefacto|Arma|Herramienta|Simbólico|Tecnología")
    (capture-obj-owner-prompt . "Propietario Actual")
    (capture-obj-first-appearance-prompt . "Capítulo de Primera Aparición")
    (capture-obj-status-prompt . "Estado|Activo|Perdido|Destruido|Oculto")
    (capture-obj-physical-description . "Descripción Física")
    (capture-obj-origin . "Origen")
    (capture-obj-properties . "Propiedades")
    (capture-obj-importance-plot . "Importancia en la Trama")
    (capture-obj-history . "Historia del Objeto")
    (capture-obj-symbolism . "Simbolismo")
    (capture-obj-current-location . "Ubicación Actual")
    (capture-obj-rules . "Reglas y Limitaciones")
    (capture-obj-notes . "Notas")

    ;; Capture — timeline template
    (capture-tl-key . "t")
    (capture-tl-name . "Evento de la Cronología")
    (capture-tl-name-prompt . "Nombre del Evento")
    (capture-tl-type-prompt . "Tipo|Acción|Revelación|Personaje|Mundo|Trasfondo")
    (capture-tl-date-prompt . "Fecha/Momento en la Historia")
    (capture-tl-characters-prompt . "Personajes Involucrados")
    (capture-tl-location-prompt . "Localización")
    (capture-tl-chapter-prompt . "Capítulo(s)")
    (capture-tl-description . "Descripción")
    (capture-tl-consequences . "Consecuencias")
    (capture-tl-connections . "Conexiones")
    (capture-tl-connections-hint . "Enlaza con:")
    (capture-tl-notes . "Notas")

    ;; Capture — plot thread template
    (capture-pt-key . "p")
    (capture-pt-name . "Hilo de Trama")
    (capture-pt-name-prompt . "Nombre del Hilo")
    (capture-pt-type-prompt . "Tipo|Subtrama|Trama Principal|Trama B|Trama C|Hilo Temático")
    (capture-pt-status-prompt . "Estado|Emergiendo|Planeado|En Progreso|Necesita Desarrollo|Completo")
    (capture-pt-weight-prompt . "Peso|1.0|2.0|3.0|4.0|5.0")
    (capture-pt-description . "Descripción")
    (capture-pt-description-prompt . "Breve descripción de este hilo de trama")
    (capture-pt-connection-main . "Conexión con la Trama Principal")
    (capture-pt-connection-main-prompt . "¿Cómo se conecta este hilo con la trama principal o la sustenta?")
    (capture-pt-key-scenes . "Escenas Clave")
    (capture-pt-resolution . "Resolución")
    (capture-pt-resolution-hint . "[¿Cómo debería resolverse este hilo?]")
    (capture-pt-notes . "Notas")
    (capture-pt-notes-hint . "[Notas rápidas de captura - pueden ser informales]")

    ;; Search
    (search-pov-prompt . "Buscar personaje PoV [difuso]: ")
    (search-pov-prompt-free . "Personaje (PoV) [subcadena]: ")
    (search-char-prompt . "Buscar personaje [difuso]: ")
    (search-char-prompt-free . "Nombre del personaje [subcadena]: ")
    (search-plot-prompt . "Buscar trama [difuso]: ")
    (search-plot-prompt-free . "Término de trama [subcadena]: ")
    (search-loc-prompt . "Buscar localización [difuso]: ")
    (search-loc-prompt-free . "Localización [subcadena]: ")
    (msg-no-org-files . "No se encontraron archivos .org en %s ni en sus subdirectorios")

    ;; Search — edit marker index
    (edits-index-title . "Marcas de edición")
    (edits-index-section-edits . "Ediciones")
    (edits-index-section-notes . "Notas")
    (edits-index-other-category . "otras")
    (edits-index-no-heading . "(antes del primer encabezado)")
    (edits-index-none-found . "No se encontraron marcas de edición en %s")
    (edits-index-found . "%d marca%s en %d archivo%s")
    (edits-index-help . "RET o mouse-1 abre la marca en el manuscrito.  g actualiza, q oculta.")

    ;; Character linking
    (prompt-select-character . "Seleccionar personaje: ")
    (prompt-select-pov . "Seleccionar personaje PoV: ")
    (prompt-select-characters-multi . "Seleccionar personaje (RET para terminar): ")
    (msg-inserted-link . "Enlace insertado a %s")
    (msg-inserted-links . "Insertados %d enlace%s de personaje")
    (msg-set-pov . "PoV establecido en %s")
    (msg-set-characters . "Personajes establecidos en: %s")
    (msg-updated-pov . "Propiedad PoV actualizada")
    (msg-updated-characters . "Propiedad Characters actualizada")
    (msg-updated-pov-and-chars . "Propiedades PoV y Characters actualizadas")
    (msg-no-updates-needed . "No se encontraron propiedades de personaje o ya estaban enlazadas")
    (msg-updated-links . "Enlaces de personaje actualizados en %d escena%s")
    (msg-added-ids . "IDs añadidos a %d encabezado%s de personaje")
    (msg-jump-to-pov . "Se saltó al personaje PoV")
    (msg-no-characters-selected . "No se seleccionó ningún personaje")
    (msg-character-ids-updated . "IDs de personaje actualizados en %s")
    (msg-setting-up-links . "Configurando el sistema de enlace de personajes…")
    (msg-setup-complete . "¡Configuración del sistema de enlace de personajes completa!")

    ;; Location linking
    (prompt-select-location . "Seleccionar localización: ")
    (prompt-select-locations-multi . "Seleccionar localización (RET para terminar): ")
    (msg-set-location . "Localización establecida en %s")
    (msg-set-locations . "Localizaciones establecidas en: %s")
    (msg-updated-location . "Propiedad Location actualizada")
    (msg-updated-locations . "Propiedad Locations actualizada")
    (msg-updated-location-links . "Enlaces de localización actualizados en %d escena%s")
    (msg-added-location-ids . "IDs añadidos a %d encabezado%s de localización")
    (msg-location-ids-updated . "IDs de localización actualizados en %s")
    (msg-no-locations-selected . "No se seleccionó ninguna localización")
    (msg-inserted-location-links . "Insertados %d enlace%s de localización")
    (msg-setting-up-location-links . "Configurando el sistema de enlace de localizaciones…")
    (msg-location-setup-complete . "¡Configuración del sistema de enlace de localizaciones completa!")

    ;; Plot linking
    (prompt-select-plot-thread . "Seleccionar hilo de trama: ")
    (prompt-select-plot-threads-multi . "Seleccionar hilo de trama (RET para terminar): ")
    (msg-set-plot-thread . "Trama establecida en %s")
    (msg-set-plot-threads . "Hilos de trama establecidos en: %s")
    (msg-updated-plot . "Propiedad Plot actualizada")
    (msg-updated-plot-threads . "Propiedad Plot actualizada")
    (msg-updated-plot-links . "Enlaces de hilos de trama actualizados en %d escena%s")
    (msg-added-plot-ids . "IDs añadidos a %d encabezado%s de hilo de trama")
    (msg-plot-ids-updated . "IDs de hilos de trama actualizados en %s")
    (msg-no-plot-threads-selected . "No se seleccionó ningún hilo de trama")
    (msg-inserted-plot-links . "Insertados %d enlace%s de hilo de trama")
    (msg-no-plot-updates-needed . "No se encontró la propiedad Plot o ya estaba enlazada")
    (msg-jump-to-plot-thread . "Se saltó al hilo de trama")
    (msg-setting-up-plot-links . "Configurando el sistema de enlace de hilos de trama…")
    (msg-plot-setup-complete . "¡Configuración del sistema de enlace de hilos de trama completa!")
    (msg-plot-health-report . "Informe de estado de los hilos de trama generado")
    (msg-plot-stats . "Hilos de trama: %d | Escenas: %d | Hilos con avisos: %d")
    (msg-no-plot-property . "No hay propiedad Plot en el encabezado actual")
    (msg-no-plot-threads-in-property . "No se encontraron hilos de trama en la propiedad Plot")
    (msg-plot-not-id-link . "El hilo de trama «%s» no es un enlace de ID")
    (prompt-jump-to-plot . "Saltar al hilo de trama: ")

    ;; Link name updates
    (msg-updated-link-names . "Nombres de enlace actualizados")
    (msg-updated-all-link-names . "Nombres de enlace de %s actualizados en %d escena%s")
    (msg-updated-all-links-scene . "Nombres de enlace actualizados en %d escena%s")
    (msg-relink-complete . "Reenlace completo: enlaces actualizados en %d escena%s en %s")
    (msg-relink-no-novel . "No se encontró ningún archivo de manuscrito en este proyecto")
    (msg-no-link-updates . "Ningún nombre de enlace de %s necesitaba actualizarse")
    (msg-updated-pov-link-names . "Nombres de enlace de PoV actualizados")
    (msg-updated-characters-link-names . "Nombres de enlace de personajes actualizados")
    (msg-updated-pov-and-chars-link-names . "Nombres de enlace de PoV y personajes actualizados")
    (msg-updated-plot-link-names . "Nombres de enlace de trama actualizados")

    ;; Character relationships
    (prompt-relationship-from-character . "Personaje de origen: ")
    (prompt-relationship-to-character . "Personaje de destino: ")
    (prompt-relationship-type . "Tipo de relación: ")
    (prompt-remove-relationship . "Eliminar relación: ")
    (msg-added-relationship . "Relación de tipo %s añadida: %s → %s")
    (msg-removed-relationship . "Relación de %s eliminada: %s")
    (msg-no-relationships . "No se encontraron relaciones para este personaje.")
    (msg-no-relationships-in-project . "No se encontraron relaciones en el proyecto.")
    (msg-relationship-setup-complete . "Propiedad RelationshipsData añadida a %d personaje%s")
    (msg-no-other-characters . "No se encontraron más personajes. Cree más personajes primero.")
    (error-no-relationships . "No hay relaciones definidas para %s")

    ;; Column view
    (msg-column-view-enabled . "Simplificación de enlaces en vista de columnas activada")
    (msg-column-view-disabled . "Simplificación de enlaces en vista de columnas desactivada")

    ;; Overlay tooltips
    (msg-overlays-enabled . "Información emergente de entidades activada")
    (msg-overlays-disabled . "Información emergente de entidades desactivada")

    ;; Scene word counts
    (msg-scenes-wordcount-updated . "Recuento de palabras actualizado en %d escena%s")
    (msg-wordcount-region . "%d palabras en la región")
    (msg-wordcount-buffer . "%d palabras en el búfer")
    (msg-wordcount-degraded . "Contado sin org-context-extended: los totales incluyen metadatos de Org. Instálelo para un recuento preciso (M-x org-scribe-setup-check).")

    ;; Dictionary / language tools
    (error-word-empty . "La palabra no puede estar vacía")
    (error-word-lookup . "Error al buscar la palabra: %s")
    (msg-word-not-found . "Palabra no encontrada: %s")
    (msg-word-suggestions . "Sugerencias:")
    (error-random-word . "Error al obtener una palabra aleatoria: %s")
    (error-word-parse . "Error al analizar la respuesta de la RAE: %s")
    (error-random-word-parse . "Error al analizar la respuesta de palabra aleatoria: %s")

    ;; File operations
    (file-not-found . "El archivo %s no existe. ¿Crearlo? ")
    (file-open-prompt . "Abrir archivo: ")

    ;; Questions / confirmations
    (question-link-existing-scenes . "¿Enlazar personajes en las escenas existentes? ")
    (question-link-existing-locations . "¿Enlazar localizaciones en las escenas existentes? ")
    (question-link-existing-plots . "¿Enlazar hilos de trama en las escenas existentes? ")
    (question-create-directory . "El directorio %s no existe. ¿Crearlo? ")

    ;; Errors — input validation
    (error-empty-title . "El título no puede estar vacío ni contener solo espacios en blanco")
    (error-path-separator . "El título no puede contener separadores de ruta (/ o \\)")
    (error-title-colon . "El título no puede contener dos puntos (:)")
    (error-title-special-chars . "El título no puede contener caracteres especiales (* ? < > | \" ')")
    (error-title-dot . "El título no puede empezar con un punto (.)")
    (error-title-double-dot . "El título no puede contener puntos dobles (..)")
    (error-empty-character . "El nombre del personaje no puede estar vacío")
    (error-empty-location . "La localización no puede estar vacía")
    (error-empty-plot . "El término de trama no puede estar vacío")

    ;; Errors — missing features / files
    (error-no-characters-found . "No se encontraron personajes. Cree personajes primero o añada IDs con org-scribe-add-character-ids.")
    (error-no-locations-found . "No se encontraron localizaciones. Cree localizaciones primero o añada IDs con org-scribe-add-location-ids.")
    (error-no-plot-threads-found . "No se encontraron hilos de trama. Cree hilos de trama primero o añada IDs con org-scribe-add-plot-thread-ids.")
    (error-no-character-file . "No se encontró el archivo de personajes. Cree personajes primero.")
    (error-no-location-file . "No se encontró el archivo de localizaciones. Cree localizaciones primero.")
    (error-no-plot-file . "No se encontró el archivo de trama. Cree hilos de trama primero.")
    (error-no-pov-property . "No se encontró la propiedad PoV")
    (error-pov-not-link . "La propiedad PoV no es un enlace de ID. Use org-scribe-set-pov-character para crear un enlace.")
    (error-plot-not-link . "La propiedad Plot no es un enlace de ID. Use org-scribe-set-scene-plot-threads para crear un enlace.")
    (error-no-id-for-character . "No se encontró ID para %s")
    (error-no-id-for-location . "No se encontró ID para %s")
    (error-no-id-for-plot . "No se encontró ID para %s")
    (error-template-not-found . "No se encontró el directorio de plantillas: %s")
    (error-org-ql-required . "El paquete org-ql es necesario para las funciones de búsqueda")
    (error-org-context-required . "El paquete org-context-extended es necesario para un recuento de palabras preciso")
    (error-writeroom-required . "writeroom-mode es necesario para los modos de entorno de escritura")
    (error-feature-not-available . "La función %s no está disponible. Instale el paquete necesario")
    (error-no-org-file . "El búfer actual no está visitando ningún archivo; no se puede activar `org-scribe-editing-mode'")

    ;; Workspace dispatcher
    (prompt-select-workspace . "Disposición del espacio de trabajo: ")
    (msg-workspace-set . "Espacio de trabajo: %s")
    (msg-workspace-normal . "Espacio de trabajo: edición normal")
    (error-unknown-workspace . "Disposición de espacio de trabajo desconocida: %s")

    ;; Health report — text-level statistics
    (msg-health-pov-word-share-heading . "Reparto de Palabras por PoV")
    (msg-health-pov-word-share-table-header . "| PoV | Escenas | Palabras | % del Total |")
    (msg-health-pov-none-label . "(sin PoV)")
    (msg-health-chapter-length-heading . "Dispersión de Longitud de Capítulos")
    (msg-health-chapter-length-table-header . "| Capítulo | Palabras |")
    (msg-health-chapter-length-summary . "Mín: %d palabras · Máx: %d palabras · Media: %.1f palabras · Mediana: %.1f palabras")
    (msg-health-chapter-length-outlier-legend . "* señala un capítulo con más del ~2x de la media o menos del ~0.5x — informativo, no un juicio.")

    ;; Pluralization helpers (used in code)
    (plural-empty . "")
    (plural-s . "s")
    )
  "Spanish messages.  See `org-scribe-messages-en' for the English set
and this file's Commentary for the parity requirement between the two.")

(defun org-scribe-msg (key &rest args)
  "Get user-facing message for KEY and format with ARGS.

KEY is a symbol that identifies the message in the alist selected by
`org-scribe-message-language' (`org-scribe-messages-en' or
`org-scribe-messages-es').  ARGS are optional format arguments to
substitute into the message template.

Falls back to `org-scribe-messages-en' when KEY is missing from the
selected language, and to KEY's own symbol name when it is missing from
English too — this function never signals an error for an unregistered
key.

Examples:
  (org-scribe-msg 'default-scene-name)
  => \"New scene\"

  (org-scribe-msg 'msg-inserted-link \"Alex\")
  => \"Inserted link to Alex\"

  (org-scribe-msg 'msg-updated-links 5 \"s\")
  => \"Updated character links in 5 scenes\""
  (let* ((table (if (eq org-scribe-message-language 'es)
                    org-scribe-messages-es
                  org-scribe-messages-en))
         (template (or (alist-get key table)
                       (alist-get key org-scribe-messages-en)
                       (symbol-name key))))
    (if args
        (apply #'format template args)
      template)))

(defun org-scribe-plural (count singular-suffix)
  "Return appropriate plural suffix based on COUNT.
SINGULAR-SUFFIX is the suffix to use when COUNT is 1 (usually empty string).
Otherwise returns 's'.

Examples:
  (org-scribe-plural 1 \"\") => \"\"
  (org-scribe-plural 5 \"\") => \"s\"
  (org-scribe-plural 0 \"\") => \"s\"

This is a helper for constructing grammatically correct messages.
Every message in `org-scribe-messages-es' that uses this suffix
attaches it to a noun whose Spanish plural is also formed by adding
just \"s\" (e.g. \"enlace\"/\"enlaces\", \"escena\"/\"escenas\",
\"encabezado\"/\"encabezados\") — nouns needing \"-es\" (e.g.
\"localización\"/\"localizaciones\") are rephrased so the pluralized
word is always one of the safe \"-s\" nouns instead."
  (if (= count 1) singular-suffix "s"))

(provide 'org-scribe-messages)

;;; org-scribe-messages.el ends here
