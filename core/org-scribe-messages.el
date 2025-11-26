;;; org-scribe-messages.el --- Centralized user-facing messages -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Centralized repository for all user-facing strings in org-scribe.
;; This makes it easy to maintain consistency and prepares for future
;; internationalization if needed.
;;
;; Usage:
;;   (org-scribe-msg 'default-scene-name)
;;   => "New scene"
;;
;;   (org-scribe-msg 'msg-inserted-link "Alex")
;;   => "Inserted link to Alex"

;;; Code:

(defconst org-scribe-messages
  '(
    ;; Default values
    (default-scene-name . "New scene")
    (default-chapter-name . "New chapter")

    ;; Project creation
    (project-creation-base-dir . "Base directory for project: ")
    (project-creation-novel-title . "Novel title: ")
    (project-creation-short-story-title . "Short story title: ")
    (project-creation-success-novel . "Novel project '%s' created successfully at %s")
    (project-creation-success-short-story . "Short story project '%s' created successfully at %s")
    (project-already-exists . "Project directory '%s' already exists!")

    ;; Template insertion
    (scene-name-prompt . "Scene name: ")
    (chapter-name-prompt . "Chapter name: ")
    (not-in-org-mode . "This command can only be used in org-mode buffers")
    (not-in-novel-project . "Not in a novel project directory")

    ;; Search prompts
    (search-pov-prompt . "Find PoV character [fuzzy]: ")
    (search-pov-prompt-free . "Character (POV) [substring]: ")
    (search-char-prompt . "Find character [fuzzy]: ")
    (search-char-prompt-free . "Character name [substring]: ")
    (search-plot-prompt . "Find plot thread [fuzzy]: ")
    (search-plot-prompt-free . "Plot term [substring]: ")
    (search-loc-prompt . "Find location [fuzzy]: ")
    (search-loc-prompt-free . "Location [substring]: ")

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
    (msg-jump-to-plot-thread . "Jumped to plot thread")

    ;; Link name updates
    (msg-updated-link-names . "Updated link names")
    (msg-updated-all-link-names . "Updated %s link names in %d scene%s")
    (msg-no-link-updates . "No %s link names needed updating")

    ;; Errors - validation
    (error-empty-title . "Title cannot be empty or contain only whitespace")
    (error-path-separator . "Title cannot contain path separators (/ or \\)")
    (error-title-colon . "Title cannot contain colons (:)")
    (error-title-special-chars . "Title cannot contain special characters (* ? < > | \" ')")
    (error-title-dot . "Title cannot start with a dot (.)")
    (error-title-double-dot . "Title cannot contain double dots (..)")
    (error-empty-character . "Character name cannot be empty")
    (error-empty-location . "Location cannot be empty")
    (error-empty-plot . "Plot term cannot be empty")

    ;; Errors - features/files
    (error-no-characters-found . "No characters found. Create characters first or add IDs with org-scribe/add-character-ids.")
    (error-no-locations-found . "No locations found. Create locations first or add IDs with org-scribe/add-location-ids.")
    (error-no-plot-threads-found . "No plot threads found. Create plot threads first or add IDs with org-scribe/add-plot-thread-ids.")
    (error-no-character-file . "No character file found. Create characters first.")
    (error-no-location-file . "No location file found. Create locations first.")
    (error-no-plot-file . "No plot file found. Create plot threads first.")
    (error-no-pov-property . "No PoV property found")
    (error-pov-not-link . "PoV property is not an ID link. Use org-scribe/set-pov-character to create a link.")
    (error-plot-not-link . "Plot property is not an ID link. Use org-scribe/set-scene-plot-threads to create a link.")
    (error-no-id-for-character . "No ID found for %s")
    (error-no-id-for-location . "No ID found for %s")
    (error-no-id-for-plot . "No ID found for %s")
    (error-template-not-found . "Template directory not found: %s")
    (error-org-ql-required . "org-ql package is required for search functions")
    (error-org-context-required . "org-context-extended package is required for accurate word counting")
    (error-writeroom-required . "writeroom-mode is required for writing environment modes")
    (error-feature-not-available . "Feature %s not available. Install required package")
    (error-no-org-file . "Current buffer is not visiting a file; cannot enable `org-scribe-editing-mode'")

    ;; Dictionary/Language tools
    (error-word-empty . "Word cannot be empty")
    (error-word-lookup . "Error al buscar la palabra: %s")
    (msg-word-not-found . "Palabra no encontrada: %s")
    (msg-word-suggestions . "Sugerencias:")
    (error-random-word . "Error al obtener palabra aleatoria: %s")
    (error-word-parse . "Error parsing RAE response: %s")
    (error-random-word-parse . "Error parsing random word response: %s")

    ;; Column view
    (msg-column-view-enabled . "Column view link stripping enabled")
    (msg-column-view-disabled . "Column view link stripping disabled")

    ;; Link updates (generic - all types)
    (msg-updated-all-links-scene . "Updated link names in %d scene%s")

    ;; Location linking (additional)
    (msg-setting-up-location-links . "Setting up location linking system...")
    (msg-location-setup-complete . "Location linking system setup complete!")

    ;; Plot linking (additional)
    (msg-setting-up-plot-links . "Setting up plot thread linking system...")
    (msg-plot-setup-complete . "Plot thread linking system setup complete!")
    (msg-plot-health-report . "Plot thread health report generated")
    (msg-plot-stats . "Plot threads: %d | Scenes: %d | Threads with warnings: %d")
    (msg-no-plot-property . "No Plot property in current heading")
    (msg-no-plot-threads-in-property . "No plot threads found in Plot property")
    (msg-plot-not-id-link . "Plot thread '%s' is not an ID link")
    (prompt-jump-to-plot . "Jump to plot thread: ")

    ;; Capture
    (capture-character-name . "Character Name")
    (capture-location-type . "Location Type")
    (capture-object-type . "Object Type")

    ;; Character Relationships
    (prompt-relationship-from-character . "From character: ")
    (prompt-relationship-to-character . "To character: ")
    (prompt-relationship-type . "Relationship type: ")
    (prompt-relationship-strength . "Strength (1-5): ")
    (prompt-relationship-sentiment . "Sentiment: ")
    (prompt-remove-relationship . "Remove relationship: ")
    (msg-added-relationship . "Added %s relationship: %s → %s (strength: %d, %s)")
    (msg-removed-relationship . "Removed %s's relationship: %s")
    (msg-no-relationships . "No relationships found for this character.")
    (msg-no-relationships-in-project . "No relationships found in project.")
    (msg-relationship-setup-complete . "Added RelationshipsData property to %d character%s")
    (msg-no-other-characters . "No other characters found. Create more characters first.")
    (error-no-relationships . "No relationships defined for %s")

    ;; Relationship Visualization
    (prompt-graph-format . "Format: ")
    (prompt-graph-character . "Character (blank for all): ")
    (prompt-graph-min-strength . "Minimum strength (blank for all): ")
    (msg-graph-inserted . "Relationship graph block inserted. Press C-c C-c to update.")
    (error-graphviz-not-found . "Graphviz 'dot' command not found. Install Graphviz to generate images.")
    (error-graph-render-failed . "Error rendering graph: dot exited with code %d")
    (msg-graph-format-unknown . "Unknown format: %s. Use 'dot, 'ascii, or 'table")

    ;; File operations
    (file-not-found . "File %s doesn't exist. Create it? ")
    (file-open-prompt . "Open file: ")

    ;; Questions
    (question-link-existing-scenes . "Link characters in existing scenes? ")
    (question-link-existing-locations . "Link locations in existing scenes? ")
    (question-link-existing-plots . "Link plot threads in existing scenes? ")
    (question-create-directory . "Directory %s does not exist. Create it? ")

    ;; Project registration
    (msg-projects-registered . "Scanned and registered %d project(s) under %s")

    ;; Search results
    (msg-no-org-files . "No .org files found in %s and subdirectories")

    ;; Pluralization helpers (used in code)
    (plural-empty . "")
    (plural-s . "s")
    )
  "Central repository for all user-facing messages in org-scribe.
Each entry is (KEY . MESSAGE-TEMPLATE) where MESSAGE-TEMPLATE can include
printf-style format specifiers (%s, %d, etc.) for dynamic content.")

(defun org-scribe-msg (key &rest args)
  "Get user-facing message for KEY and format with ARGS.

KEY is a symbol that identifies the message in `org-scribe-messages'.
ARGS are optional format arguments to substitute into the message template.

Examples:
  (org-scribe-msg 'default-scene-name)
  => \"New scene\"

  (org-scribe-msg 'msg-inserted-link \"Alex\")
  => \"Inserted link to Alex\"

  (org-scribe-msg 'msg-updated-links 5 \"s\")
  => \"Updated character links in 5 scenes\"

If KEY is not found in `org-scribe-messages', signals an error."
  (let ((template (alist-get key org-scribe-messages)))
    (if template
        (if args
            (apply #'format template args)
          template)
      (error "Unknown message key: %s" key))))

(defun org-scribe-plural (count singular-suffix)
  "Return appropriate plural suffix based on COUNT.
SINGULAR-SUFFIX is the suffix to use when COUNT is 1 (usually empty string).
Otherwise returns 's'.

Examples:
  (org-scribe-plural 1 \"\") => \"\"
  (org-scribe-plural 5 \"\") => \"s\"
  (org-scribe-plural 0 \"\") => \"s\"

This is a helper for constructing grammatically correct messages."
  (if (= count 1) singular-suffix "s"))

(provide 'org-scribe-messages)

;;; org-scribe-messages.el ends here
