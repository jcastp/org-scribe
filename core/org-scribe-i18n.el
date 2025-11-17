;;; org-scribe-i18n.el --- Internationalization support for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Internationalization (i18n) support for org-scribe.
;; Provides translation tables and helper functions for bilingual
;; (English/Spanish) support with extensibility for additional languages.
;;
;; Key features:
;; - Translation tables for all user-facing strings
;; - Automatic project language detection
;; - Runtime language switching support
;; - Bilingual heading pattern matching
;; - Performance optimizations via caching
;;
;; Usage:
;;   (org-scribe-i18n default-scene-name)
;;   => "New scene" (in English projects)
;;   => "Nueva escena" (in Spanish projects)
;;
;;   (org-scribe-i18n project-creation-success-novel "My Novel" "/path")
;;   => "Novel project 'My Novel' created successfully at /path"

;;; Code:

(require 'org-scribe-config)

;;; Translation Tables

(defconst org-scribe-i18n-strings
  '((en
     ;; Project creation
     (project-creation-base-dir . "Base directory for project: ")
     (project-creation-novel-title . "Novel title: ")
     (project-creation-story-title . "Short story title: ")
     (project-creation-language . "Project language: ")
     (project-creation-success-novel . "Novel project '%s' created successfully at %s")
     (project-creation-success-story . "Short story project '%s' created successfully at %s")
     (project-created-marker . "Writing project: %s")
     (project-created-date . "Created: %s")
     (project-language-marker . "Language: %s")
     (project-type-marker . "Type: %s")

     ;; Template insertion
     (scene-name-prompt . "Scene name: ")
     (chapter-name-prompt . "Chapter name: ")
     (default-scene-name . "New scene")
     (default-chapter-name . "New chapter")

     ;; Search prompts
     (search-pov-free . "Character (POV) [substring]: ")
     (search-pov-fuzzy . "Find PoV character [fuzzy]: ")
     (search-char-free . "Character name [substring]: ")
     (search-char-fuzzy . "Find character [fuzzy]: ")
     (search-plot-free . "Plot term [substring]: ")
     (search-plot-fuzzy . "Find plot thread [fuzzy]: ")
     (search-loc-free . "Location [substring]: ")
     (search-loc-fuzzy . "Find location [fuzzy]: ")

     ;; Error messages
     (error-empty-character . "Character name cannot be empty")
     (error-empty-location . "Location cannot be empty")
     (error-empty-plot . "Plot term cannot be empty")
     (error-no-org-mode . "This command can only be used in org-mode buffers")
     (error-project-exists . "Project directory '%s' already exists!")
     (error-template-not-found . "Template directory not found: %s")
     (error-no-project . "Not in a writing project directory")
     (error-org-ql-required . "org-ql package is required for search functions")

     ;; Validation errors
     (validation-empty-title . "Title cannot be empty or contain only whitespace")
     (validation-path-separator . "Title cannot contain path separators (/ or \\)")
     (validation-colon . "Title cannot contain colons (:)")
     (validation-special-chars . "Title cannot contain special characters (* ? < > | \" ')")
     (validation-starts-dot . "Title cannot start with a dot (.)")
     (validation-double-dots . "Title cannot contain double dots (..)")

     ;; Capture prompts
     (capture-character-name . "Character Name")
     (capture-character-role . "Role")
     (capture-character-age . "Age")
     (capture-character-gender . "Gender")
     (capture-character-occupation . "Occupation")
     (capture-character-first-appearance . "First Appearance Chapter")
     (capture-location-name . "Location Name")
     (capture-location-type . "Type")
     (capture-location-importance . "Importance")
     (capture-location-climate . "Climate")
     (capture-location-population . "Population")
     (capture-object-name . "Object Name")
     (capture-object-type . "Type")
     (capture-object-owner . "Current Owner")
     (capture-object-status . "Status")
     (capture-timeline-event . "Event Name")
     (capture-timeline-type . "Type")
     (capture-timeline-date . "Date/Time in Story")
     (capture-timeline-characters . "Characters Involved")
     (capture-timeline-location . "Location")
     (capture-timeline-chapter . "Chapter(s)")
     (capture-plot-thread . "Thread Name")
     (capture-plot-type . "Type")
     (capture-plot-status . "Status")

     ;; Capture role/type options
     (role-protagonist . "Protagonist")
     (role-antagonist . "Antagonist")
     (role-supporting . "Supporting")
     (role-minor . "Minor")
     (type-city . "City")
     (type-building . "Building")
     (type-room . "Room")
     (type-natural . "Natural")
     (type-region . "Region")
     (type-country . "Country")
     (importance-major . "Major")
     (importance-supporting . "Supporting")
     (importance-minor . "Minor")
     (object-type-magical . "Magical")
     (object-type-artifact . "Artifact")
     (object-type-weapon . "Weapon")
     (object-type-tool . "Tool")
     (object-type-symbolic . "Symbolic")
     (object-type-technology . "Technology")
     (status-active . "Active")
     (status-lost . "Lost")
     (status-destroyed . "Destroyed")
     (status-hidden . "Hidden")
     (timeline-type-action . "Action")
     (timeline-type-revelation . "Revelation")
     (timeline-type-character . "Character")
     (timeline-type-world . "World")
     (timeline-type-backstory . "Backstory")
     (plot-type-subplot . "Subplot")
     (plot-type-main . "Main Plot")
     (plot-type-b-plot . "B-Plot")
     (plot-type-c-plot . "C-Plot")
     (plot-type-thematic . "Thematic Thread")
     (plot-status-emerging . "Emerging")
     (plot-status-planned . "Planned")
     (plot-status-in-progress . "In Progress")
     (plot-status-needs-dev . "Needs Development")
     (plot-status-complete . "Complete")

     ;; File creation titles
     (file-title-plot . "Plot Structure")
     (file-title-characters . "Character Database")
     (file-title-locations . "Locations & World Building")
     (file-title-objects . "Important Objects")
     (file-title-timeline . "Story Timeline")
     (file-title-notes . "Writing Notes")
     (file-title-planning . "%s - Planning & Notes")

     ;; Template section headings
     (section-physical-description . "Physical Description")
     (section-personality . "Personality")
     (section-background . "Background")
     (section-goal-motivation-conflict . "Goal, Motivation, Conflict")
     (section-internal . "Internal")
     (section-external . "External")
     (section-character-arc . "Character Arc")
     (section-relationships . "Relationships")
     (section-notes . "Notes")
     (section-general-description . "General Description")
     (section-geography . "Geography")
     (section-cultural-aspects . "Cultural Aspects & Society")
     (section-history . "History")
     (section-notable-features . "Notable Features")
     (section-plot-importance . "Importance in the Plot")
     (section-specific-places . "Specific Places")
     (section-atmosphere . "Atmosphere & Mood")
     (section-map-reference . "Map/Reference Image")
     (section-origin . "Origin")
     (section-properties . "Properties")
     (section-object-history . "Object History")
     (section-symbolism . "Symbolism")
     (section-current-location . "Current Location")
     (section-rules-limitations . "Rules & Limitations")
     (section-description . "Description")
     (section-consequences . "Consequences")
     (section-connections . "Connections")
     (section-connection-main-plot . "Connection to Main Plot")
     (section-key-scenes . "Key Scenes")
     (section-resolution . "Resolution")

     ;; Plot file content
     (plot-premise-heading . "Premise")
     (plot-premise-question . "What is the story about in one or two sentences?")
     (plot-main-heading . "Main Plot")
     (plot-central-conflict . "Central Conflict")
     (plot-dramatic-question . "Main Dramatic Question")
     (plot-subplots-heading . "Subplots")
     (plot-threads-heading . "Plot Threads")
     (plot-threads-instruction . "Track your plot threads here. Use F8 F8 p to capture new threads.")
     (plot-outline-heading . "Plot Outline")
     (plot-setup-heading . "Setup")

     ;; Short story notes content
     (notes-protagonist-heading . "Protagonist: [Name]")
     (notes-main-location . "Main Location(s)")
     (notes-research-heading . "Research & References")
     (notes-revision-heading . "Revision Notes")
     (notes-ideas-heading . "Random Ideas & Inspiration")
     (notes-setting-heading . "Setting")

     ;; Heading patterns for detection
     (heading-characters . "Characters")
     (heading-protagonist . "Protagonist")
     (heading-antagonist . "Antagonist")
     (heading-secondary . "Secondary")
     (heading-locations . "Locations")
     (heading-plot-threads . "Plot Threads")
     (heading-timeline . "Timeline")
     (heading-objects . "Objects")
     (heading-notes . "Notes")

     ;; File opening
     (file-open-prompt . "Open file: ")
     (file-not-found . "File %s doesn't exist. Create it? ")
     (not-in-project . "Not in a novel project directory")

     ;; Messages
     (msg-updated-pov-chars . "Updated PoV and Characters properties")
     (msg-updated-chars . "Updated Characters property")
     (msg-updated-pov-chars-names . "Updated PoV and Characters link names")
     (msg-updated-chars-names . "Updated Characters link names")
     (msg-set-characters . "Set Characters to: %s")
     (msg-language-switched . "Project language switched to %s")
     (msg-git-init-failed . "Failed to initialize git repository")
     (msg-no-org-files . "No .org files found in %s and subdirectories")

     ;; Linking system messages
     (msg-added-ids . "Added IDs to %d %s heading%s")
     (msg-no-file-found . "No %s file found. Create %s first.")
     (msg-ids-updated . "%s IDs updated in %s")
     (msg-no-items-found . "No %s found. Create %s first or add IDs with %s.")
     (msg-inserted-link . "Inserted link to %s")
     (msg-no-id-found . "No ID found for %s")
     (msg-inserted-links . "Inserted %d %s link%s")
     (msg-no-items-selected . "No %s selected")
     (msg-set-pov . "Set PoV to %s")
     (msg-set-location . "Set Location to: %s")
     (msg-set-plot . "Set Plot to: %s")
     (msg-jumped-to-pov . "Jumped to PoV character")
     (msg-no-pov-property . "No PoV property in current heading")
     (msg-updated-property . "Updated %s property")
     (msg-no-properties-found . "No %s properties found or already linked")
     (msg-updated-links-in-scenes . "Updated %s links in %d scene%s")
     (msg-auto-created-id . "Auto-created ID for new %s (via hook)")
     (msg-setup-system . "Setting up %s linking system...")
     (msg-setup-complete . "%s linking system setup complete!")
     (msg-updated-link-names . "Updated %s link names")
     (msg-no-link-names-update . "No %s link names needed updating")
     (msg-updated-names-in-scenes . "Updated %s link names in %d scene%s")

     ;; Linking prompts
     (prompt-select-character . "Select character: ")
     (prompt-select-pov . "Select PoV character: ")
     (prompt-select-location . "Select location: ")
     (prompt-select-plot . "Select plot thread: ")
     (prompt-select-item . "Select %s: ")
     (prompt-continue-selection . "Select another (empty to finish): ")

     ;; Linking item types (for messages)
     (item-character . "character")
     (item-characters . "characters")
     (item-location . "location")
     (item-locations . "locations")
     (item-plot-thread . "plot thread")
     (item-plot-threads . "plot threads")

     ;; TODO search results
     (todo-search-title . "TODO items in writing project"))

    (es
     ;; Creación de proyectos
     (project-creation-base-dir . "Directorio base para el proyecto: ")
     (project-creation-novel-title . "Título de la novela: ")
     (project-creation-story-title . "Título del cuento: ")
     (project-creation-language . "Idioma del proyecto: ")
     (project-creation-success-novel . "Proyecto de novela '%s' creado exitosamente en %s")
     (project-creation-success-story . "Proyecto de cuento '%s' creado exitosamente en %s")
     (project-created-marker . "Proyecto de escritura: %s")
     (project-created-date . "Creado: %s")
     (project-language-marker . "Idioma: %s")
     (project-type-marker . "Tipo: %s")

     ;; Inserción de plantillas
     (scene-name-prompt . "Nombre de la escena: ")
     (chapter-name-prompt . "Nombre del capítulo: ")
     (default-scene-name . "Nueva escena")
     (default-chapter-name . "Nuevo capítulo")

     ;; Prompts de búsqueda
     (search-pov-free . "Personaje (PDV) [subcadena]: ")
     (search-pov-fuzzy . "Buscar personaje PDV [difuso]: ")
     (search-char-free . "Nombre del personaje [subcadena]: ")
     (search-char-fuzzy . "Buscar personaje [difuso]: ")
     (search-plot-free . "Término de trama [subcadena]: ")
     (search-plot-fuzzy . "Buscar hilo argumental [difuso]: ")
     (search-loc-free . "Localización [subcadena]: ")
     (search-loc-fuzzy . "Buscar localización [difuso]: ")

     ;; Mensajes de error
     (error-empty-character . "El nombre del personaje no puede estar vacío")
     (error-empty-location . "La localización no puede estar vacía")
     (error-empty-plot . "El término de trama no puede estar vacío")
     (error-no-org-mode . "Este comando solo puede usarse en buffers de org-mode")
     (error-project-exists . "¡El directorio del proyecto '%s' ya existe!")
     (error-template-not-found . "Directorio de plantillas no encontrado: %s")
     (error-no-project . "No estás en un directorio de proyecto de escritura")
     (error-org-ql-required . "El paquete org-ql es requerido para las funciones de búsqueda")

     ;; Errores de validación
     (validation-empty-title . "El título no puede estar vacío o contener solo espacios")
     (validation-path-separator . "El título no puede contener separadores de ruta (/ o \\)")
     (validation-colon . "El título no puede contener dos puntos (:)")
     (validation-special-chars . "El título no puede contener caracteres especiales (* ? < > | \" ')")
     (validation-starts-dot . "El título no puede empezar con un punto (.)")
     (validation-double-dots . "El título no puede contener puntos dobles (..)")

     ;; Prompts de captura
     (capture-character-name . "Nombre del Personaje")
     (capture-character-role . "Rol")
     (capture-character-age . "Edad")
     (capture-character-gender . "Género")
     (capture-character-occupation . "Ocupación")
     (capture-character-first-appearance . "Primera Aparición - Capítulo")
     (capture-location-name . "Nombre de la Localización")
     (capture-location-type . "Tipo")
     (capture-location-importance . "Importancia")
     (capture-location-climate . "Clima")
     (capture-location-population . "Población")
     (capture-object-name . "Nombre del Objeto")
     (capture-object-type . "Tipo")
     (capture-object-owner . "Dueño Actual")
     (capture-object-status . "Estado")
     (capture-timeline-event . "Nombre del Evento")
     (capture-timeline-type . "Tipo")
     (capture-timeline-date . "Fecha/Hora en la Historia")
     (capture-timeline-characters . "Personajes Involucrados")
     (capture-timeline-location . "Localización")
     (capture-timeline-chapter . "Capítulo(s)")
     (capture-plot-thread . "Nombre del Hilo")
     (capture-plot-type . "Tipo")
     (capture-plot-status . "Estado")

     ;; Opciones de rol/tipo
     (role-protagonist . "Protagonista")
     (role-antagonist . "Antagonista")
     (role-supporting . "Secundario")
     (role-minor . "Menor")
     (type-city . "Ciudad")
     (type-building . "Edificio")
     (type-room . "Habitación")
     (type-natural . "Natural")
     (type-region . "Región")
     (type-country . "País")
     (importance-major . "Mayor")
     (importance-supporting . "Secundaria")
     (importance-minor . "Menor")
     (object-type-magical . "Mágico")
     (object-type-artifact . "Artefacto")
     (object-type-weapon . "Arma")
     (object-type-tool . "Herramienta")
     (object-type-symbolic . "Simbólico")
     (object-type-technology . "Tecnología")
     (status-active . "Activo")
     (status-lost . "Perdido")
     (status-destroyed . "Destruido")
     (status-hidden . "Oculto")
     (timeline-type-action . "Acción")
     (timeline-type-revelation . "Revelación")
     (timeline-type-character . "Personaje")
     (timeline-type-world . "Mundo")
     (timeline-type-backstory . "Trasfondo")
     (plot-type-subplot . "Subtrama")
     (plot-type-main . "Trama Principal")
     (plot-type-b-plot . "Trama B")
     (plot-type-c-plot . "Trama C")
     (plot-type-thematic . "Hilo Temático")
     (plot-status-emerging . "Emergente")
     (plot-status-planned . "Planeado")
     (plot-status-in-progress . "En Progreso")
     (plot-status-needs-dev . "Necesita Desarrollo")
     (plot-status-complete . "Completo")

     ;; Títulos de archivos para creación
     (file-title-plot . "Estructura de la Trama")
     (file-title-characters . "Base de Datos de Personajes")
     (file-title-locations . "Localizaciones y Construcción de Mundo")
     (file-title-objects . "Objetos Importantes")
     (file-title-timeline . "Cronología de la Historia")
     (file-title-notes . "Notas de Escritura")
     (file-title-planning . "%s - Planificación y Notas")

     ;; Encabezados de secciones de plantilla
     (section-physical-description . "Descripción Física")
     (section-personality . "Personalidad")
     (section-background . "Trasfondo")
     (section-goal-motivation-conflict . "Objetivo, Motivación, Conflicto")
     (section-internal . "Interno")
     (section-external . "Externo")
     (section-character-arc . "Arco del Personaje")
     (section-relationships . "Relaciones")
     (section-notes . "Notas")
     (section-general-description . "Descripción General")
     (section-geography . "Geografía")
     (section-cultural-aspects . "Aspectos Culturales y Sociedad")
     (section-history . "Historia")
     (section-notable-features . "Características Notables")
     (section-plot-importance . "Importancia en la Trama")
     (section-specific-places . "Lugares Específicos")
     (section-atmosphere . "Atmósfera y Ambiente")
     (section-map-reference . "Mapa/Imagen de Referencia")
     (section-origin . "Origen")
     (section-properties . "Propiedades")
     (section-object-history . "Historia del Objeto")
     (section-symbolism . "Simbolismo")
     (section-current-location . "Ubicación Actual")
     (section-rules-limitations . "Reglas y Limitaciones")
     (section-description . "Descripción")
     (section-consequences . "Consecuencias")
     (section-connections . "Conexiones")
     (section-connection-main-plot . "Conexión con la Trama Principal")
     (section-key-scenes . "Escenas Clave")
     (section-resolution . "Resolución")

     ;; Contenido de archivo de trama
     (plot-premise-heading . "Premisa")
     (plot-premise-question . "¿De qué trata la historia en una o dos oraciones?")
     (plot-main-heading . "Trama Principal")
     (plot-central-conflict . "Conflicto Central")
     (plot-dramatic-question . "Pregunta Dramática Principal")
     (plot-subplots-heading . "Subtramas")
     (plot-threads-heading . "Hilos Argumentales")
     (plot-threads-instruction . "Rastrea tus hilos argumentales aquí. Usa F8 F8 p para capturar nuevos hilos.")
     (plot-outline-heading . "Esquema de la Trama")
     (plot-setup-heading . "Planteamiento")

     ;; Contenido de notas de cuento corto
     (notes-protagonist-heading . "Protagonista: [Nombre]")
     (notes-main-location . "Localización(es) Principal(es)")
     (notes-research-heading . "Investigación y Referencias")
     (notes-revision-heading . "Notas de Revisión")
     (notes-ideas-heading . "Ideas Aleatorias e Inspiración")
     (notes-setting-heading . "Ambientación")

     ;; Patrones de encabezados para detección
     (heading-characters . "Personajes")
     (heading-protagonist . "Protagonista")
     (heading-antagonist . "Antagonista")
     (heading-secondary . "Secundario")
     (heading-locations . "Localizaciones")
     (heading-plot-threads . "Hilos Argumentales")
     (heading-timeline . "Cronología")
     (heading-objects . "Objetos")
     (heading-notes . "Notas")

     ;; Apertura de archivos
     (file-open-prompt . "Abrir archivo: ")
     (file-not-found . "El archivo %s no existe. ¿Crearlo? ")
     (not-in-project . "No estás en un directorio de proyecto de novela")

     ;; Mensajes
     (msg-updated-pov-chars . "Propiedades PDV y Personajes actualizadas")
     (msg-updated-chars . "Propiedad Personajes actualizada")
     (msg-updated-pov-chars-names . "Nombres de enlaces PDV y Personajes actualizados")
     (msg-updated-chars-names . "Nombres de enlaces Personajes actualizados")
     (msg-set-characters . "Personajes establecidos a: %s")
     (msg-language-switched . "Idioma del proyecto cambiado a %s")
     (msg-git-init-failed . "Fallo al inicializar repositorio git")
     (msg-no-org-files . "No se encontraron archivos .org en %s y subdirectorios")

     ;; Mensajes del sistema de enlaces
     (msg-added-ids . "Se agregaron IDs a %d encabezado%s de %s")
     (msg-no-file-found . "No se encontró archivo de %s. Crea %s primero.")
     (msg-ids-updated . "IDs de %s actualizados en %s")
     (msg-no-items-found . "No se encontraron %s. Crea %s primero o agrega IDs con %s.")
     (msg-inserted-link . "Enlace insertado a %s")
     (msg-no-id-found . "No se encontró ID para %s")
     (msg-inserted-links . "Se insertaron %d enlace%s de %s")
     (msg-no-items-selected . "No se seleccionaron %s")
     (msg-set-pov . "PdV establecido a %s")
     (msg-set-location . "Localización establecida a: %s")
     (msg-set-plot . "Trama establecida a: %s")
     (msg-jumped-to-pov . "Saltó al personaje PDV")
     (msg-no-pov-property . "No hay propiedad PDV en el encabezado actual")
     (msg-updated-property . "Propiedad %s actualizada")
     (msg-no-properties-found . "No se encontraron propiedades %s o ya están enlazadas")
     (msg-updated-links-in-scenes . "Enlaces de %s actualizados en %d escena%s")
     (msg-auto-created-id . "ID auto-creado para nuevo %s (via hook)")
     (msg-setup-system . "Configurando sistema de enlaces de %s...")
     (msg-setup-complete . "¡Configuración del sistema de enlaces de %s completa!")
     (msg-updated-link-names . "Nombres de enlaces de %s actualizados")
     (msg-no-link-names-update . "No fue necesario actualizar nombres de enlaces de %s")
     (msg-updated-names-in-scenes . "Nombres de enlaces de %s actualizados en %d escena%s")

     ;; Prompts de enlaces
     (prompt-select-character . "Seleccionar personaje: ")
     (prompt-select-pov . "Seleccionar personaje PdV: ")
     (prompt-select-location . "Seleccionar localización: ")
     (prompt-select-plot . "Seleccionar hilo argumental: ")
     (prompt-select-item . "Seleccionar %s: ")
     (prompt-continue-selection . "Seleccionar otro (vacío para terminar): ")

     ;; Tipos de elementos de enlace (para mensajes)
     (item-character . "personaje")
     (item-characters . "personajes")
     (item-location . "localización")
     (item-locations . "localizaciones")
     (item-plot-thread . "hilo argumental")
     (item-plot-threads . "hilos argumentales")

     ;; Resultados de búsqueda TODO
     (todo-search-title . "Items TODO en el proyecto de escritura")))
  "Translation table for all user-facing strings.
Organized as ((LANGUAGE (KEY . STRING) (KEY . STRING) ...) ...)
where LANGUAGE is 'en or 'es, KEY is a symbol, and STRING is the translation.")

;;; Language Detection Cache

(defvar org-scribe-i18n--project-language-cache nil
  "Alist of (PROJECT-ROOT . LANGUAGE) for caching language detection.
Cleared when changing projects or calling `org-scribe-i18n-clear-cache'.")

(defun org-scribe-i18n-clear-cache ()
  "Clear the project language detection cache.
Call this if you manually edit .org-scribe-project files."
  (interactive)
  (setq org-scribe-i18n--project-language-cache nil)
  (message "i18n cache cleared"))

;;; Language Detection Functions

(defun org-scribe-i18n--detect-project-language-uncached ()
  "Detect the language of the current project without using cache.
Returns 'en, 'es, or the value of `org-scribe-template-language' as fallback.

Detection strategy:
1. Read .org-scribe-project marker file if it exists (look for Language: line)
2. Fall back to `org-scribe-template-language' customization variable
3. Default to 'en if variable is not defined"
  (let* ((root (if (fboundp 'org-scribe-project-root)
                   (org-scribe-project-root)
                 default-directory))
         (marker-file (expand-file-name ".org-scribe-project" root))
         (default-lang (if (boundp 'org-scribe-template-language)
                           org-scribe-template-language
                         'en)))
    (if (file-exists-p marker-file)
        (with-temp-buffer
          (insert-file-contents marker-file)
          (goto-char (point-min))
          (if (re-search-forward "^# Language: \\(en\\|es\\)$" nil t)
              (intern (match-string 1))
            default-lang))
      default-lang)))

(defun org-scribe-i18n--detect-project-language ()
  "Detect project language with caching.
Returns 'en, 'es, or the value of `org-scribe-template-language'.
Results are cached per project root for performance."
  (let* ((root (if (fboundp 'org-scribe-project-root)
                   (org-scribe-project-root)
                 default-directory))
         (cached (alist-get root org-scribe-i18n--project-language-cache
                           nil nil #'string=)))
    (or cached
        (let ((lang (org-scribe-i18n--detect-project-language-uncached)))
          (push (cons root lang) org-scribe-i18n--project-language-cache)
          lang))))

;;; String Retrieval Functions

(defun org-scribe-i18n-string (key &optional lang)
  "Get translated string for KEY in language LANG.
If LANG is nil, auto-detect from project or use `org-scribe-template-language'.
KEY is a symbol like 'project-creation-novel-title.

Returns the translated string, or the English fallback if translation
is missing, or a placeholder string if KEY is not found at all."
  (let* ((language (or lang (org-scribe-i18n--detect-project-language)))
         (lang-table (alist-get language org-scribe-i18n-strings))
         (translation (alist-get key lang-table)))
    (or translation
        ;; Fallback to English if translation missing
        (alist-get key (alist-get 'en org-scribe-i18n-strings))
        ;; Last resort: show missing key
        (format "[Missing translation: %s]" key))))

(defmacro org-scribe-i18n (key &rest format-args)
  "Get translated string for KEY and optionally format with FORMAT-ARGS.

Usage:
  (org-scribe-i18n default-scene-name)
  => \"New scene\" or \"Nueva escena\"

  (org-scribe-i18n project-creation-success-novel \"My Novel\" \"/path\")
  => \"Novel project 'My Novel' created successfully at /path\"

KEY is not quoted - it's a symbol.
FORMAT-ARGS are passed to `format' if provided."
  (if format-args
      `(format (org-scribe-i18n-string ',key) ,@format-args)
    `(org-scribe-i18n-string ',key)))

;;; Heading Pattern Detection

(defun org-scribe-i18n-heading-pattern (heading-key &optional lang)
  "Get heading pattern for HEADING-KEY in language LANG.

If LANG is specified, returns the heading string for that language.
If LANG is nil, returns a regexp pattern that matches both English
and Spanish variants.

Examples:
  (org-scribe-i18n-heading-pattern 'heading-characters 'en)
  => \"Characters\"

  (org-scribe-i18n-heading-pattern 'heading-characters 'es)
  => \"Personajes\"

  (org-scribe-i18n-heading-pattern 'heading-characters)
  => \"\\\\(Characters\\\\|Personajes\\\\)\""
  (if lang
      (org-scribe-i18n-string heading-key lang)
    ;; Return pattern matching both languages
    (let ((en-heading (org-scribe-i18n-string heading-key 'en))
          (es-heading (org-scribe-i18n-string heading-key 'es)))
      (format "\\(%s\\|%s\\)"
              (regexp-quote en-heading)
              (regexp-quote es-heading)))))

;;; Language Switching

(defun org-scribe-switch-project-language (new-language)
  "Switch current project to NEW-LANGUAGE ('en or 'es).

This updates the .org-scribe-project marker file to specify the new
language. Note that this only affects future operations - existing
content is not automatically translated.

The language setting is used for:
- User-facing prompts and messages
- Template insertion (scene/chapter defaults)
- Search function prompts
- Capture template labels"
  (interactive
   (list (intern (completing-read
                 "New project language (en/es): "
                 '("en" "es") nil t nil nil
                 (symbol-name org-scribe-template-language)))))
  (unless (memq new-language '(en es))
    (user-error "Language must be 'en or 'es"))
  (let* ((root (if (fboundp 'org-scribe-project-root)
                   (org-scribe-project-root)
                 default-directory))
         (marker-file (expand-file-name ".org-scribe-project" root)))
    (if (file-exists-p marker-file)
        (progn
          (with-temp-buffer
            (insert-file-contents marker-file)
            (goto-char (point-min))
            (if (re-search-forward "^# Language: \\(en\\|es\\)$" nil t)
                (replace-match (format "# Language: %s" new-language))
              ;; No language line found, add one
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert (format "# Language: %s\n" new-language)))
            (write-region (point-min) (point-max) marker-file))
          ;; Clear cache for this project
          (setq org-scribe-i18n--project-language-cache
                (assoc-delete-all root org-scribe-i18n--project-language-cache))
          (message (org-scribe-i18n-string 'msg-language-switched new-language)
                   new-language))
      (user-error (org-scribe-i18n-string 'error-no-project)))))

;;; Utility Functions

(defun org-scribe-i18n-get-all-keys ()
  "Return list of all translation keys.
Useful for debugging and ensuring translation completeness."
  (let ((en-table (alist-get 'en org-scribe-i18n-strings)))
    (mapcar #'car en-table)))

(defun org-scribe-i18n-missing-translations (&optional lang)
  "Return list of keys missing translations in LANG (default: 'es).
Compares against English as the reference language."
  (interactive)
  (let* ((target-lang (or lang 'es))
         (en-keys (mapcar #'car (alist-get 'en org-scribe-i18n-strings)))
         (target-keys (mapcar #'car (alist-get target-lang org-scribe-i18n-strings)))
         (missing (cl-set-difference en-keys target-keys)))
    (when (called-interactively-p 'interactive)
      (if missing
          (message "Missing %s translations (%d): %s"
                   target-lang (length missing) missing)
        (message "All translations complete for %s!" target-lang)))
    missing))

(provide 'org-scribe-i18n)

;;; org-scribe-i18n.el ends here
