;;; org-scribe-character-links.el --- Character linking system for org-scribe -*- lexical-binding: t; -*-

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
(require 'org-scribe-core)
(require 'org-scribe-capture)
(require 'org-scribe-messages)

;;; Character ID Management

(defun org-scribe--ensure-character-has-id ()
  "Ensure the current character heading has a unique ID.
Creates an ID if one doesn't exist. Returns the ID."
  (org-id-get-create))

(defun org-scribe--add-id-to-all-characters ()
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
      (message (org-scribe-msg 'msg-added-ids count
                               (org-scribe-plural count ""))))))

(defun org-scribe--get-character-name-at-point ()
  "Get the character name from current heading or NAME property."
  (or (org-entry-get nil "NAME")
      (org-get-heading t t t t)))

;;; Character Database Functions

(defun org-scribe--get-character-file ()
  "Get the path to the characters file for the current project."
  (org-scribe/capture-character-file))

(defun org-scribe--get-all-characters ()
  "Return alist of (CHARACTER-NAME . (ID . HEADING)) from characters file.
Returns list of (NAME . (ID . HEADING-TEXT)) for all characters in the project."
  (let ((char-file (org-scribe--get-character-file))
        result)
    (when (and char-file (file-exists-p char-file))
      (with-current-buffer (find-file-noselect char-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (let* ((level (org-current-level))
                   (id (org-id-get))
                   (name (org-scribe--get-character-name-at-point))
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

(defun org-scribe--create-character-link (char-name id-alist)
  "Create an ID link for CHAR-NAME using ID-ALIST.
ID-ALIST should be in format ((NAME . (ID . HEADING)) ...).
Returns the link string or plain text if no ID found."
  (if-let* ((entry (assoc char-name id-alist))
            (id (cadr entry)))
      (format "[[id:%s][%s]]" id char-name)
    char-name)) ; Fallback to plain text if no ID found

;;; Interactive Functions

;;;###autoload
(defun org-scribe/add-character-ids ()
  "Add unique IDs to all characters in the characters file.
This should be run once on existing projects to set up
the ID-based linking system."
  (interactive)
  (let ((char-file (org-scribe--get-character-file)))
    (if (not (file-exists-p char-file))
        (message (org-scribe-msg 'error-no-character-file))
      (with-current-buffer (find-file-noselect char-file)
        (org-scribe--add-id-to-all-characters)
        (save-buffer)
        (message (org-scribe-msg 'msg-character-ids-updated char-file))))))

;;;###autoload
(defun org-scribe/insert-character-link ()
  "Insert a character link in the current property.
Scans the characters file, presents a completion menu,
and inserts the selected character as an ID link.

Use this function when adding characters to scene properties."
  (interactive)
  (let* ((chars (org-scribe--get-all-characters))
         (char-names (mapcar #'car chars)))
    (if (null char-names)
        (message (org-scribe-msg 'error-no-characters-found))
      (let* ((selected (completing-read (org-scribe-msg 'prompt-select-character) char-names nil t))
             (entry (assoc selected chars))
             (id (cadr entry)))
        (if id
            (progn
              (insert (format "[[id:%s][%s]]" id selected))
              (message (org-scribe-msg 'msg-inserted-link selected)))
          (message (org-scribe-msg 'error-no-id-for-character selected)))))))

;;;###autoload
(defun org-scribe/insert-multiple-character-links ()
  "Insert multiple character links separated by commas.
Useful for the :Characters: property which often lists
multiple characters in a scene."
  (interactive)
  (let* ((chars (org-scribe--get-all-characters))
         (char-names (mapcar #'car chars))
         selected-chars
         links)
    (if (null char-names)
        (message (org-scribe-msg 'error-no-characters-found))
      ;; Multiple selection loop
      (while (let ((choice (completing-read
                           (org-scribe-msg 'prompt-select-characters-multi)
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
            (message (org-scribe-msg 'msg-inserted-links
                                     (length links)
                                     (org-scribe-plural (length links) ""))))
        (message (org-scribe-msg 'msg-no-characters-selected))))))

;;;###autoload
(defun org-scribe/set-pov-character ()
  "Set the PoV property to a character with ID link.
Specifically designed for the :PoV: property in scene headings."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (let* ((chars (org-scribe--get-all-characters))
         (char-names (mapcar #'car chars)))
    (if (null char-names)
        (message (org-scribe-msg 'error-no-characters-found))
      (let* ((selected (completing-read (org-scribe-msg 'prompt-select-pov) char-names nil t))
             (entry (assoc selected chars))
             (id (cadr entry)))
        (if id
            (progn
              (org-set-property "PoV" (format "[[id:%s][%s]]" id selected))
              (message (org-scribe-msg 'msg-set-pov selected)))
          (message (org-scribe-msg 'error-no-id-for-character selected)))))))

;;;###autoload
(defun org-scribe/set-scene-characters ()
  "Set the Characters property to multiple character ID links.
Specifically designed for the :Characters: property in scene headings."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (let* ((chars (org-scribe--get-all-characters))
         (char-names (mapcar #'car chars))
         selected-chars
         links)
    (if (null char-names)
        (message (org-scribe-msg 'error-no-characters-found))
      ;; Multiple selection loop
      (while (let ((choice (completing-read
                           (org-scribe-msg 'prompt-select-characters-multi)
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
            (message (org-scribe-msg 'msg-set-characters (string-join selected-chars ", "))))
        (message (org-scribe-msg 'msg-no-characters-selected))))))

;;;###autoload
(defun org-scribe/jump-to-pov-character ()
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
              (message (org-scribe-msg 'msg-jump-to-pov)))
          (message (org-scribe-msg 'error-pov-not-link))))
    (message (org-scribe-msg 'error-no-pov-property))))

;;; Batch Update Functions

(defun org-scribe--link-characters-in-property (property-name)
  "Convert character names to ID links in PROPERTY-NAME of current heading.
Handles both single characters and comma-separated lists."
  (when-let ((prop-value (org-entry-get nil property-name)))
    (let* ((id-alist (org-scribe--get-all-characters))
           ;; Split on comma, trim whitespace
           (char-list (mapcar #'string-trim
                             (split-string prop-value "," t)))
           ;; Create links for each character
           (linked-chars (mapcar (lambda (name)
                                  (org-scribe--create-character-link name id-alist))
                                char-list))
           (linked-string (string-join linked-chars ", ")))
      ;; Only update if we actually created links
      (unless (string= prop-value linked-string)
        (org-set-property property-name linked-string)
        t))))

;;;###autoload
(defun org-scribe/link-scene-characters ()
  "Convert character names to ID links in current scene.
Updates both :PoV: and :Characters: properties."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((updated-pov (org-scribe--link-characters-in-property "PoV"))
          (updated-chars (org-scribe--link-characters-in-property "Characters")))
      (cond
       ((and updated-pov updated-chars)
        (message (org-scribe-msg 'msg-updated-pov-and-chars)))
       (updated-pov
        (message (org-scribe-msg 'msg-updated-pov)))
       (updated-chars
        (message (org-scribe-msg 'msg-updated-characters)))
       (t
        (message (org-scribe-msg 'msg-no-updates-needed)))))))

;;;###autoload
(defun org-scribe/link-all-scene-characters ()
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
           (let ((updated-pov (org-scribe--link-characters-in-property "PoV"))
                 (updated-chars (org-scribe--link-characters-in-property "Characters")))
             (when (or updated-pov updated-chars)
               (setq count (1+ count))))))
       nil 'file)
      (message (org-scribe-msg 'msg-updated-links count (org-scribe-plural count ""))))))

;;;###autoload
(defun org-scribe/setup-character-links ()
  "Set up character linking system for current project.
This function:
1. Adds IDs to all existing characters
2. Ensures the capture hook is active
3. Optionally links existing scenes

Run this once when setting up ID-based character linking
in an existing project."
  (interactive)
  (message (org-scribe-msg 'msg-setting-up-links))

  ;; Step 1: Add IDs to characters
  (org-scribe/add-character-ids)

  ;; Step 2: Ask if user wants to link existing scenes
  (when (y-or-n-p (org-scribe-msg 'question-link-existing-scenes))
    (let ((novel-file (plist-get (org-scribe-project-structure) :novel-file)))
      (when (and novel-file (file-exists-p novel-file))
        (with-current-buffer (find-file-noselect novel-file)
          (org-scribe/link-all-scene-characters)
          (save-buffer)))))

  (message (org-scribe-msg 'msg-setup-complete)))

;;; Character Timeline

;;; Helper Functions for Timeline

(defun org-scribe--get-all-scenes-with-characters ()
  "Return list of all scenes with PoV or Characters properties.
Each entry is (SCENE-HEADING CHAPTER-HEADING POV-NAME CHARACTERS-LIST).
POV-NAME is a string (or nil if no PoV).
CHARACTERS-LIST is a list of character names (or nil if no Characters)."
  (let ((novel-file (plist-get (org-scribe-project-structure) :novel-file))
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
                  (require 'org-scribe-search)
                  (let ((pov-name (when pov-prop
                                   (org-scribe--extract-link-text pov-prop)))
                        (chars-list (when chars-prop
                                     (org-scribe--property-to-list chars-prop))))
                    (push (list heading chapter pov-name chars-list) scenes))))))
          nil 'file))))
    (nreverse scenes)))

(defun org-scribe--get-character-weight (char-name)
  "Get the Weight property for CHAR-NAME from characters file.
Returns the weight as a float, or 999.0 if not found.

CHAR-NAME is matched against the :NAME: property or heading text,
using the same logic as org-scribe--get-character-name-at-point."
  (let ((char-file (org-scribe--get-character-file))
        (weight 999.0))  ; Default for characters without Weight
    (when (and char-file (file-exists-p char-file))
      (with-current-buffer (find-file-noselect char-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         ;; Use org-map-entries to find the character (same as org-scribe--get-all-characters)
         (catch 'found
           (org-map-entries
            (lambda ()
              (let* ((level (org-current-level))
                     (name (org-scribe--get-character-name-at-point))
                     (role (org-entry-get nil "Role"))
                     (type (org-entry-get nil "TYPE"))
                     (is-character
                      (and (>= level 1)
                           name
                           (or role type
                               (save-excursion
                                 (ignore-errors
                                   (org-up-heading-safe)
                                   (string-match-p "Character\\|Personaje\\|Protagonist\\|Antagonist\\|Secondary"
                                                  (org-get-heading t t t t))))))))
                ;; If this is a character and the name matches
                (when (and is-character (string= name char-name))
                  (when-let ((weight-str (org-entry-get nil "Weight")))
                    (setq weight (string-to-number weight-str)))
                  (throw 'found t))))
            nil 'file)))))
    weight))

(defun org-scribe--collect-unique-characters (scenes)
  "Extract unique character names from SCENES list.
SCENES format: ((SCENE CHAPTER POV CHARS-LIST) ...).
Returns list of unique character names, sorted by Weight property (ascending)."
  (let ((characters (make-hash-table :test 'equal)))
    ;; Collect unique character names (unchanged)
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

    ;; Build list of (name . weight) pairs
    (let ((char-weights nil))
      (dolist (char-name (hash-table-keys characters))
        (let ((weight (org-scribe--get-character-weight char-name)))
          (push (cons char-name weight) char-weights)))

      ;; Sort by weight (ascending), then alphabetically for ties
      (setq char-weights
            (sort char-weights
                  (lambda (a b)
                    (let ((weight-a (cdr a))
                          (weight-b (cdr b)))
                      (if (= weight-a weight-b)
                          ;; Weights equal - sort alphabetically
                          (string< (car a) (car b))
                        ;; Different weights - sort by weight
                        (< weight-a weight-b))))))

      ;; Extract just the names
      (mapcar #'car char-weights))))

(defun org-scribe--character-symbol (char-name pov-name chars-list)
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
    ;;"◆")
    "PoV")
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
  PoV = PoV character (implies presence)
  ● = Present in scene (not PoV)
  (blank) = Not present

Character columns are sorted by :Weight: property (ascending).
Characters without a Weight property appear last, sorted alphabetically.

Character information is extracted from :PoV: and :Characters: properties.
Character names are extracted from ID links like [[id:...][Name]].

PARAMS are ignored (reserved for future filtering options)."
  (let* ((scenes (org-scribe--get-all-scenes-with-characters))
         (characters (org-scribe--collect-unique-characters scenes)))

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
            (let ((symbol (org-scribe--character-symbol char pov-name chars-list)))
              (insert (format " %s |" symbol))))
          (insert "\n")))

      ;; Align table
      (org-table-align))))

;;; Update Link Display Names

(require 'org-scribe-link-update)

;;;###autoload
(defun org-scribe/update-character-link-names ()
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
    (let* ((chars-alist (org-scribe--get-all-characters))
           (id-map (org-scribe--build-id-to-name-map chars-alist))
           (updated-pov (org-scribe--update-links-in-property "PoV" id-map))
           (updated-chars (org-scribe--update-links-in-property "Characters" id-map)))
      (cond
       ((and updated-pov updated-chars)
        (message (org-scribe-msg 'msg-updated-pov-and-chars-link-names)))
       (updated-pov
        (message (org-scribe-msg 'msg-updated-pov-link-names)))
       (updated-chars
        (message (org-scribe-msg 'msg-updated-characters-link-names)))
       (t
        (message (org-scribe-msg 'msg-no-link-updates "character"))))
      (or updated-pov updated-chars))))

;;;###autoload
(defun org-scribe/update-all-character-link-names ()
  "Update character link display names in all scenes.

Scans characters database for current names and updates the display
text portion of ID links in :PoV: and :Characters: properties throughout
the entire manuscript.

This is useful after renaming characters in characters.org, as ID
links will still work but show the old name. This function refreshes
all display names to match the current database.

Example workflow:
  1. Rename \"Alex Rivera\" to \"Alexandra Rivera\" in characters.org
  2. Run this function (M-x org-scribe/update-all-character-link-names)
  3. All 47 scenes updated automatically!

Returns the number of scenes updated."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((chars-alist (org-scribe--get-all-characters))
           (id-map (org-scribe--build-id-to-name-map chars-alist))
           (count 0))
      (org-map-entries
       (lambda ()
         (when (or (org-entry-get nil "PoV")
                   (org-entry-get nil "Characters"))
           (let ((updated-pov (org-scribe--update-links-in-property "PoV" id-map))
                 (updated-chars (org-scribe--update-links-in-property "Characters" id-map)))
             (when (or updated-pov updated-chars)
               (setq count (1+ count))))))
       nil 'file)
      (message (org-scribe-msg 'msg-updated-all-link-names "character"
                              count (org-scribe-plural count "")))
      count)))

(provide 'org-scribe-character-links)

;;; org-scribe-character-links.el ends here
