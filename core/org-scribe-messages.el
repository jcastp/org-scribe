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
