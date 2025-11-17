;;; writing-character-links.el --- Character linking system for emacs-writing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides ID-based linking system for characters in writing projects.
;; Automatically creates unique IDs for characters and provides functions
;; to insert character links in scene properties.
;;
;; Features:
;; - Auto-create IDs when capturing new characters
;; - Insert character links with completion
;; - Link multiple characters at once
;; - Jump to character definition from properties
;; - Update all character links in document

;;; Code:

(require 'org)
(require 'org-id)
(require 'writing-core)
(require 'writing-capture)

;;; Character ID Management

(defun writing--ensure-character-has-id ()
  "Ensure the current character heading has a unique ID.
Creates an ID if one doesn't exist. Returns the ID."
  (org-id-get-create))

(defun writing--add-id-to-all-characters ()
  "Add IDs to all character headings in current buffer.
This function scans the characters file and ensures every
character heading has a unique ID property."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (org-map-entries
       (lambda ()
         (let* ((level (org-current-level))
                (role (org-entry-get nil "Role"))
                (type (org-entry-get nil "TYPE"))
                (is-character-heading
                 (and (>= level 1)  ; Level 1 or deeper
                      (or role  ; Has Role property (new template)
                          type  ; Has TYPE property (old template)
                          ;; Or is under "Characters" or similar heading
                          (save-excursion
                            (ignore-errors
                              (org-up-heading-safe)
                              (string-match-p "Character\\|Personaje\\|Protagonist\\|Antagonist\\|Secondary"
                                             (org-get-heading t t t t))))))))
           (when is-character-heading
             (unless (org-id-get)
               (org-id-get-create)
               (setq count (1+ count))))))
       nil 'file)
      (message "Added IDs to %d character heading%s"
               count (if (= count 1) "" "s")))))

(defun writing--get-character-name-at-point ()
  "Get the character name from current heading or NAME property."
  (or (org-entry-get nil "NAME")
      (org-get-heading t t t t)))

;;; Character Database Functions

(defun writing--get-character-file ()
  "Get the path to the characters file for the current project."
  (writing/capture-character-file))

(defun writing--get-all-characters ()
  "Return alist of (CHARACTER-NAME . (ID . HEADING)) from characters file.
Returns list of (NAME . (ID . HEADING-TEXT)) for all characters in the project."
  (let ((char-file (writing--get-character-file))
        result)
    (when (and char-file (file-exists-p char-file))
      (with-current-buffer (find-file-noselect char-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (let* ((level (org-current-level))
                   (id (org-id-get))
                   (name (writing--get-character-name-at-point))
                   (heading (org-get-heading t t t t))
                   (role (org-entry-get nil "Role"))
                   (type (org-entry-get nil "TYPE"))
                   ;; Check if this looks like a character heading
                   (is-character
                    (and (>= level 1)  ; Level 1 or deeper
                         id            ; Has an ID
                         name          ; Has a name
                         ;; Either has Role/TYPE property or is under Characters section
                         (or role      ; Has Role property (new template)
                             type      ; Has TYPE property (old template)
                             (save-excursion
                               (ignore-errors
                                 (org-up-heading-safe)
                                 (string-match-p "Character\\|Personaje\\|Protagonist\\|Antagonist\\|Secondary"
                                                (org-get-heading t t t t))))))))
              (when is-character
                (push (cons name (cons id heading)) result))))
          nil 'file))))
    (nreverse result)))

(defun writing--create-character-link (char-name id-alist)
  "Create an ID link for CHAR-NAME using ID-ALIST.
ID-ALIST should be in format ((NAME . (ID . HEADING)) ...).
Returns the link string or plain text if no ID found."
  (if-let* ((entry (assoc char-name id-alist))
            (id (cadr entry)))
      (format "[[id:%s][%s]]" id char-name)
    char-name)) ; Fallback to plain text if no ID found

;;; Interactive Functions

;;;###autoload
(defun writing/add-character-ids ()
  "Add unique IDs to all characters in the characters file.
This should be run once on existing projects to set up
the ID-based linking system."
  (interactive)
  (let ((char-file (writing--get-character-file)))
    (if (not (file-exists-p char-file))
        (message "No character file found. Create characters first.")
      (with-current-buffer (find-file-noselect char-file)
        (writing--add-id-to-all-characters)
        (save-buffer)
        (message "Character IDs updated in %s" char-file)))))

;;;###autoload
(defun writing/insert-character-link ()
  "Insert a character link in the current property.
Scans the characters file, presents a completion menu,
and inserts the selected character as an ID link.

Use this function when adding characters to scene properties."
  (interactive)
  (let* ((chars (writing--get-all-characters))
         (char-names (mapcar #'car chars)))
    (if (null char-names)
        (message "No characters found. Create characters first or add IDs with writing/add-character-ids.")
      (let* ((selected (completing-read "Select character: " char-names nil t))
             (entry (assoc selected chars))
             (id (cadr entry)))
        (if id
            (progn
              (insert (format "[[id:%s][%s]]" id selected))
              (message "Inserted link to %s" selected))
          (message "No ID found for %s" selected))))))

;;;###autoload
(defun writing/insert-multiple-character-links ()
  "Insert multiple character links separated by commas.
Useful for the :Characters: property which often lists
multiple characters in a scene."
  (interactive)
  (let* ((chars (writing--get-all-characters))
         (char-names (mapcar #'car chars))
         selected-chars
         links)
    (if (null char-names)
        (message "No characters found. Create characters first or add IDs with writing/add-character-ids.")
      ;; Multiple selection loop
      (while (let ((choice (completing-read
                           "Select character (RET to finish): "
                           char-names nil nil)))
               (when (and choice (not (string-empty-p choice)))
                 (push choice selected-chars)
                 t)))

      ;; Create links for all selected characters
      (setq selected-chars (nreverse selected-chars))
      (dolist (name selected-chars)
        (if-let* ((entry (assoc name chars))
                  (id (cadr entry)))
            (push (format "[[id:%s][%s]]" id name) links)
          (push name links)))

      (setq links (nreverse links))
      (if links
          (progn
            (insert (string-join links ", "))
            (message "Inserted %d character link%s"
                    (length links)
                    (if (= (length links) 1) "" "s")))
        (message "No characters selected")))))

;;;###autoload
(defun writing/set-pov-character ()
  "Set the PoV property to a character with ID link.
Specifically designed for the :PoV: property in scene headings."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (let* ((chars (writing--get-all-characters))
         (char-names (mapcar #'car chars)))
    (if (null char-names)
        (message "No characters found. Create characters first or add IDs with writing/add-character-ids.")
      (let* ((selected (completing-read "Select PoV character: " char-names nil t))
             (entry (assoc selected chars))
             (id (cadr entry)))
        (if id
            (progn
              (org-set-property "PoV" (format "[[id:%s][%s]]" id selected))
              (message "Set PoV to %s" selected))
          (message "No ID found for %s" selected))))))

;;;###autoload
(defun writing/set-scene-characters ()
  "Set the Characters property to multiple character ID links.
Specifically designed for the :Characters: property in scene headings."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (let* ((chars (writing--get-all-characters))
         (char-names (mapcar #'car chars))
         selected-chars
         links)
    (if (null char-names)
        (message "No characters found. Create characters first or add IDs with writing/add-character-ids.")
      ;; Multiple selection loop
      (while (let ((choice (completing-read
                           "Select character (RET to finish): "
                           char-names nil nil)))
               (when (and choice (not (string-empty-p choice)))
                 (push choice selected-chars)
                 t)))

      ;; Create links for all selected characters
      (setq selected-chars (nreverse selected-chars))
      (dolist (name selected-chars)
        (if-let* ((entry (assoc name chars))
                  (id (cadr entry)))
            (push (format "[[id:%s][%s]]" id name) links)
          (push name links)))

      (setq links (nreverse links))
      (if links
          (progn
            (org-set-property "Characters" (string-join links ", "))
            (message "Set Characters to: %s" (string-join selected-chars ", ")))
        (message "No characters selected")))))

;;;###autoload
(defun writing/jump-to-pov-character ()
  "Jump to the character definition for the PoV of current scene.
Follows the ID link in the :PoV: property."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (if-let ((pov (org-entry-get nil "PoV")))
      (progn
        ;; Extract ID from link if it's an ID link
        (if (string-match "\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" pov)
            (let ((id (match-string 1 pov)))
              (org-id-goto id)
              (message "Jumped to PoV character"))
          (message "PoV property is not an ID link. Use writing/set-pov-character to create a link.")))
    (message "No PoV property found")))

;;; Batch Update Functions

(defun writing--link-characters-in-property (property-name)
  "Convert character names to ID links in PROPERTY-NAME of current heading.
Handles both single characters and comma-separated lists."
  (when-let ((prop-value (org-entry-get nil property-name)))
    (let* ((id-alist (writing--get-all-characters))
           ;; Split on comma, trim whitespace
           (char-list (mapcar #'string-trim
                             (split-string prop-value "," t)))
           ;; Create links for each character
           (linked-chars (mapcar (lambda (name)
                                  (writing--create-character-link name id-alist))
                                char-list))
           (linked-string (string-join linked-chars ", ")))
      ;; Only update if we actually created links
      (unless (string= prop-value linked-string)
        (org-set-property property-name linked-string)
        t))))

;;;###autoload
(defun writing/link-scene-characters ()
  "Convert character names to ID links in current scene.
Updates both :PoV: and :Characters: properties."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((updated-pov (writing--link-characters-in-property "PoV"))
          (updated-chars (writing--link-characters-in-property "Characters")))
      (cond
       ((and updated-pov updated-chars)
        (message "Updated PoV and Characters properties"))
       (updated-pov
        (message "Updated PoV property"))
       (updated-chars
        (message "Updated Characters property"))
       (t
        (message "No character properties found or already linked"))))))

;;;###autoload
(defun writing/link-all-scene-characters ()
  "Convert character names to ID links in all scenes in current buffer.
Processes all headings with :PoV: or :Characters: properties."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (org-map-entries
       (lambda ()
         ;; Process if has PoV or Characters property
         (when (or (org-entry-get nil "PoV")
                   (org-entry-get nil "Characters"))
           (let ((updated-pov (writing--link-characters-in-property "PoV"))
                 (updated-chars (writing--link-characters-in-property "Characters")))
             (when (or updated-pov updated-chars)
               (setq count (1+ count))))))
       nil 'file)
      (message "Updated character links in %d scene%s"
               count (if (= count 1) "" "s")))))

;;; Integration with Capture System

(defun writing--capture-finalize-add-id ()
  "Hook function to add ID to newly captured characters.
This is called before a character capture is finalized.
Runs in the capture buffer before it's filed.

This is a safety net - the character capture template already includes
ID generation via %(org-id-new), but this ensures any character heading
without an ID gets one automatically."
  (when (and (boundp 'org-capture-mode)
             org-capture-mode
             (buffer-file-name))
    ;; Check if we're capturing to a characters file
    (let ((target (writing--get-character-file)))
      (when (and target
                 (file-exists-p target)
                 ;; Compare the target file with current buffer's file
                 ;; or the file we're capturing to
                 (or (string= (buffer-file-name) target)
                     (string= (buffer-file-name) (expand-file-name target))))
        ;; We're capturing a character, ensure it has an ID
        (save-excursion
          (goto-char (point-min))
          ;; In capture buffer, find the heading we're creating
          (when (re-search-forward "^\\*+ " nil t)
            (org-back-to-heading)
            (unless (org-entry-get nil "ID")
              (org-id-get-create)
              (message "Auto-created ID for new character (via hook)"))))))))

;; Add the hook - use before-finalize to ensure we're still in capture buffer
;; Note: This is redundant with the template's %(org-id-new) but serves as a safety net
(add-hook 'org-capture-before-finalize-hook #'writing--capture-finalize-add-id)

;;;###autoload
(defun writing/setup-character-links ()
  "Set up character linking system for current project.
This function:
1. Adds IDs to all existing characters
2. Ensures the capture hook is active
3. Optionally links existing scenes

Run this once when setting up ID-based character linking
in an existing project."
  (interactive)
  (message "Setting up character linking system...")

  ;; Step 1: Add IDs to characters
  (writing/add-character-ids)

  ;; Step 2: Ask if user wants to link existing scenes
  (when (y-or-n-p "Link characters in existing scenes? ")
    (let ((novel-file (plist-get (writing-project-structure) :novel-file)))
      (when (and novel-file (file-exists-p novel-file))
        (with-current-buffer (find-file-noselect novel-file)
          (writing/link-all-scene-characters)
          (save-buffer)))))

  (message "Character linking system setup complete!"))

;;; Character Timeline

;;; Helper Functions for Timeline

(defun writing--get-all-scenes-with-characters ()
  "Return list of all scenes with PoV or Characters properties.
Each entry is (SCENE-HEADING CHAPTER-HEADING POV-NAME CHARACTERS-LIST).
POV-NAME is a string (or nil if no PoV).
CHARACTERS-LIST is a list of character names (or nil if no Characters)."
  (let ((novel-file (plist-get (writing-project-structure) :novel-file))
        scenes)
    (when (and novel-file (file-exists-p novel-file))
      (with-current-buffer (find-file-noselect novel-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (when (= (org-current-level) 3)  ; Scenes are level 3
              (let* ((heading (org-get-heading t t t t))
                     (chapter (save-excursion
                               (outline-up-heading 1)
                               (org-get-heading t t t t)))
                     (pov-prop (org-entry-get nil "PoV"))
                     (chars-prop (org-entry-get nil "Characters")))
                ;; Include scene if it has PoV OR Characters
                (when (or pov-prop chars-prop)
                  ;; Extract display text from ID links
                  (require 'writing-search)
                  (let ((pov-name (when pov-prop
                                   (writing--extract-link-text pov-prop)))
                        (chars-list (when chars-prop
                                     (writing--property-to-list chars-prop))))
                    (push (list heading chapter pov-name chars-list) scenes))))))
          nil 'file))))
    (nreverse scenes)))

(defun writing--collect-unique-characters (scenes)
  "Extract unique character names from SCENES list.
SCENES format: ((SCENE CHAPTER POV CHARS-LIST) ...).
Returns sorted list of unique character names."
  (let ((characters (make-hash-table :test 'equal)))
    (dolist (scene scenes)
      (let ((pov (nth 2 scene))
            (chars-list (nth 3 scene)))
        ;; Add PoV character
        (when (and pov (not (string-empty-p pov)))
          (puthash pov t characters))
        ;; Add all Characters
        (when chars-list
          (dolist (char chars-list)
            (when (not (string-empty-p char))
              (puthash char t characters))))))
    ;; Return sorted list
    (sort (hash-table-keys characters) #'string<)))

(defun writing--character-symbol (char-name pov-name chars-list)
  "Return symbol for CHAR-NAME in a scene.
POV-NAME is the scene's PoV character (or nil).
CHARS-LIST is list of other characters in scene.
Returns:
  ◆ if character is PoV (implies presence)
  ● if character is in chars-list but not PoV
  empty string if character not present"
  (cond
   ;; PoV character - show ◆ (diamond, implies presence)
   ((and pov-name (string= char-name pov-name))
    "◆")
   ;; Present in Characters list - show ● (circle)
   ((and chars-list (member char-name chars-list))
    "●")
   ;; Not present - empty
   (t "")))

;;; Dynamic Block: Character Timeline

;;;###autoload
(defun org-dblock-write:character-timeline (params)
  "Generate timeline showing character appearances across scenes.
Shows which characters appear in which scenes, with distinction
between PoV characters and other appearances.

Symbols:
  ◆ = PoV character (implies presence)
  ● = Present in scene (not PoV)
  (blank) = Not present

Character information is extracted from :PoV: and :Characters: properties.
Character names are extracted from ID links like [[id:...][Name]].

PARAMS are ignored (reserved for future filtering options)."
  (let* ((scenes (writing--get-all-scenes-with-characters))
         (characters (writing--collect-unique-characters scenes)))

    (if (null scenes)
        (insert "No scenes with character properties found.\n")
      ;; Generate org table
      ;; Header row
      (insert "| Scene | Chapter |")
      (dolist (char characters)
        (insert (format " %s |" char)))
      (insert "\n")

      ;; Separator row
      (insert "|-------+---------+")
      (dolist (_ characters)
        (insert "--------+"))
      (insert "\n")

      ;; Data rows
      (dolist (scene scenes)
        (let ((scene-name (nth 0 scene))
              (chapter (nth 1 scene))
              (pov-name (nth 2 scene))
              (chars-list (nth 3 scene)))
          (insert (format "| %s | %s |" scene-name chapter))
          (dolist (char characters)
            (let ((symbol (writing--character-symbol char pov-name chars-list)))
              (insert (format " %s |" symbol))))
          (insert "\n")))

      ;; Align table
      (org-table-align))))

;;; Update Link Display Names

(require 'writing-link-update)

;;;###autoload
(defun writing/update-character-link-names ()
  "Update character link display names in current scene.

Refreshes both :PoV: and :Characters: properties to show current
character names from the characters database.

Use this after renaming a character in characters.org. The ID links
will still work, but this updates the display text to match the
current name.

Example:
  Before: [[id:char-alex-001][Alex Rivera]]
  After rename in database: [[id:char-alex-001][Alexandra Rivera]]

Returns t if any updates were made, nil otherwise."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let* ((chars-alist (writing--get-all-characters))
           (id-map (writing--build-id-to-name-map chars-alist))
           (updated-pov (writing--update-links-in-property "PoV" id-map))
           (updated-chars (writing--update-links-in-property "Characters" id-map)))
      (cond
       ((and updated-pov updated-chars)
        (message "Updated PoV and Characters link names"))
       (updated-pov
        (message "Updated PoV link names"))
       (updated-chars
        (message "Updated Characters link names"))
       (t
        (message "No character link names needed updating")))
      (or updated-pov updated-chars))))

;;;###autoload
(defun writing/update-all-character-link-names ()
  "Update character link display names in all scenes.

Scans characters database for current names and updates the display
text portion of ID links in :PoV: and :Characters: properties throughout
the entire manuscript.

This is useful after renaming characters in characters.org, as ID
links will still work but show the old name. This function refreshes
all display names to match the current database.

Example workflow:
  1. Rename \"Alex Rivera\" to \"Alexandra Rivera\" in characters.org
  2. Run this function (M-x writing/update-all-character-link-names)
  3. All 47 scenes updated automatically!

Returns the number of scenes updated."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((chars-alist (writing--get-all-characters))
           (id-map (writing--build-id-to-name-map chars-alist))
           (count 0))
      (org-map-entries
       (lambda ()
         (when (or (org-entry-get nil "PoV")
                   (org-entry-get nil "Characters"))
           (let ((updated-pov (writing--update-links-in-property "PoV" id-map))
                 (updated-chars (writing--update-links-in-property "Characters" id-map)))
             (when (or updated-pov updated-chars)
               (setq count (1+ count))))))
       nil 'file)
      (message "Updated character link names in %d scene%s"
               count (if (= count 1) "" "s"))
      count)))

(provide 'writing-character-links)

;;; writing-character-links.el ends here
