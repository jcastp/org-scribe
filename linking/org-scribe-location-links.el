;;; org-scribe-location-links.el --- Location linking system for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides ID-based linking system for locations in writing projects.
;; Automatically creates unique IDs for locations and provides functions
;; to insert location links in scene properties.
;;
;; Features:
;; - Auto-create IDs when capturing new locations
;; - Insert location links with completion
;; - Link multiple locations at once
;; - Jump to location definition from properties
;; - Update all location links in document

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-scribe-core)
(require 'org-scribe-capture)
(require 'org-scribe-messages)

;;; Location ID Management

(defun org-scribe--ensure-location-has-id ()
  "Ensure the current location heading has a unique ID.
Creates an ID if one doesn't exist. Returns the ID."
  (org-id-get-create))

(defun org-scribe--add-id-to-all-locations ()
  "Add IDs to all location headings in current buffer.
This function scans the locations file and ensures every
location heading has a unique ID property."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (org-map-entries
       (lambda ()
         (let* ((level (org-current-level))
                (parent-props (org-entry-properties nil "TYPE"))
                (type (cdr (assoc "TYPE" parent-props)))
                (is-location-heading
                 (and (>= level 1)  ; Level 1 or deeper
                      (or type  ; Has TYPE property
                          ;; Or is under "Location" or similar heading
                          (save-excursion
                            (ignore-errors
                              (org-up-heading-safe)
                              (string-match-p "Location\\|Ubicación\\|Setting\\|Place\\|Lugar"
                                             (org-get-heading t t t t)))))))))
           (when is-location-heading
             (unless (org-id-get)
               (org-id-get-create)
               (setq count (1+ count))))))
       nil 'file)
      (message (org-scribe-msg 'msg-added-location-ids count (org-scribe-plural count "")))))

(defun org-scribe--get-location-name-at-point ()
  "Get the location name from current heading or NAME property."
  (or (org-entry-get nil "NAME")
      (org-get-heading t t t t)))

;;; Location Database Functions

(defun org-scribe--get-location-file ()
  "Get the path to the locations file for the current project."
  (org-scribe/capture-location-file))

(defun org-scribe--get-all-locations ()
  "Return alist of (LOCATION-NAME . (ID . HEADING)) from locations file.
Returns list of (NAME . (ID . HEADING-TEXT)) for all locations in the project."
  (let ((loc-file (org-scribe--get-location-file))
        result)
    (when (and loc-file (file-exists-p loc-file))
      (with-current-buffer (find-file-noselect loc-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (let* ((level (org-current-level))
                   (id (org-id-get))
                   (name (org-scribe--get-location-name-at-point))
                   (heading (org-get-heading t t t t))
                   ;; Check if this looks like a location heading
                   (is-location
                    (and (>= level 1)  ; Level 1 or deeper
                         id            ; Has an ID
                         name          ; Has a name
                         ;; Either has Type property or is under Location section
                         (or (org-entry-get nil "Type")  ; New template uses Type
                             (org-entry-get nil "TYPE")  ; Old template compatibility
                             (save-excursion
                               (ignore-errors
                                 (org-up-heading-safe)
                                 (string-match-p "Location\\|Ubicación\\|Setting\\|Place\\|Lugar"
                                                (org-get-heading t t t t))))))))
              (when is-location
                (push (cons name (cons id heading)) result))))
          nil 'file))))
    (nreverse result)))

(defun org-scribe--create-location-link (loc-name id-alist)
  "Create an ID link for LOC-NAME using ID-ALIST.
ID-ALIST should be in format ((NAME . (ID . HEADING)) ...).
Returns the link string or plain text if no ID found."
  (if-let* ((entry (assoc loc-name id-alist))
            (id (cadr entry)))
      (format "[[id:%s][%s]]" id loc-name)
    loc-name)) ; Fallback to plain text if no ID found

;;; Interactive Functions

;;;###autoload
(defun org-scribe/add-location-ids ()
  "Add unique IDs to all locations in the locations file.
This should be run once on existing projects to set up
the ID-based linking system."
  (interactive)
  (let ((loc-file (org-scribe--get-location-file)))
    (if (not (file-exists-p loc-file))
        (message (org-scribe-msg 'error-no-location-file))
      (with-current-buffer (find-file-noselect loc-file)
        (org-scribe--add-id-to-all-locations)
        (save-buffer)
        (message (org-scribe-msg 'msg-location-ids-updated loc-file))))))

;;;###autoload
(defun org-scribe/insert-location-link ()
  "Insert a location link in the current property.
Scans the locations file, presents a completion menu,
and inserts the selected location as an ID link.

Use this function when adding locations to scene properties."
  (interactive)
  (let* ((locs (org-scribe--get-all-locations))
         (loc-names (mapcar #'car locs)))
    (if (null loc-names)
        (message (org-scribe-msg 'error-no-locations-found))
      (let* ((selected (completing-read (org-scribe-msg 'prompt-select-location) loc-names nil t))
             (entry (assoc selected locs))
             (id (cadr entry)))
        (if id
            (progn
              (insert (format "[[id:%s][%s]]" id selected))
              (message (org-scribe-msg 'msg-inserted-link selected)))
          (message (org-scribe-msg 'error-no-id-for-location selected)))))))

;;;###autoload
(defun org-scribe/insert-multiple-location-links ()
  "Insert multiple location links separated by commas.
Useful for the :Location: property which often lists
multiple locations in a scene."
  (interactive)
  (let* ((locs (org-scribe--get-all-locations))
         (loc-names (mapcar #'car locs))
         selected-locs
         links)
    (if (null loc-names)
        (message (org-scribe-msg 'error-no-locations-found))
      ;; Multiple selection loop
      (while (let ((choice (completing-read
                           (org-scribe-msg 'prompt-select-locations-multi)
                           loc-names nil nil)))
               (when (and choice (not (string-empty-p choice)))
                 (push choice selected-locs)
                 t)))

      ;; Create links for all selected locations
      (setq selected-locs (nreverse selected-locs))
      (dolist (name selected-locs)
        (if-let* ((entry (assoc name locs))
                  (id (cadr entry)))
            (push (format "[[id:%s][%s]]" id name) links)
          (push name links)))

      (setq links (nreverse links))
      (if links
          (progn
            (insert (string-join links ", "))
            (message (org-scribe-msg 'msg-inserted-location-links (length links) (org-scribe-plural (length links) ""))))
        (message (org-scribe-msg 'msg-no-locations-selected))))))

;;;###autoload
(defun org-scribe/set-scene-locations ()
  "Set the Locations property to multiple location ID links.
Specifically designed for the :Location: property in scene headings."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (let* ((locs (org-scribe--get-all-locations))
         (loc-names (mapcar #'car locs))
         selected-locs
         links)
    (if (null loc-names)
        (message (org-scribe-msg 'error-no-locations-found))
      ;; Multiple selection loop
      (while (let ((choice (completing-read
                           (org-scribe-msg 'prompt-select-locations-multi)
                           loc-names nil nil)))
               (when (and choice (not (string-empty-p choice)))
                 (push choice selected-locs)
                 t)))

      ;; Create links for all selected locations
      (setq selected-locs (nreverse selected-locs))
      (dolist (name selected-locs)
        (if-let* ((entry (assoc name locs))
                  (id (cadr entry)))
            (push (format "[[id:%s][%s]]" id name) links)
          (push name links)))

      (setq links (nreverse links))
      (if links
          (progn
            (org-set-property "Location" (string-join links ", "))
            (message (org-scribe-msg 'msg-set-locations (string-join selected-locs ", "))))
        (message (org-scribe-msg 'msg-no-locations-selected))))))

;;; Batch Update Functions

(defun org-scribe--link-locations-in-property (property-name)
  "Convert location names to ID links in PROPERTY-NAME of current heading.
Handles both single locations and comma-separated lists."
  (when-let ((prop-value (org-entry-get nil property-name)))
    (let* ((id-alist (org-scribe--get-all-locations))
           ;; Split on comma, trim whitespace
           (loc-list (mapcar #'string-trim
                             (split-string prop-value "," t)))
           ;; Create links for each location
           (linked-locs (mapcar (lambda (name)
                                  (org-scribe--create-location-link name id-alist))
                                loc-list))
           (linked-string (string-join linked-locs ", ")))
      ;; Only update if we actually created links
      (unless (string= prop-value linked-string)
        (org-set-property property-name linked-string)
        t))))

;;;###autoload
(defun org-scribe/link-scene-locations ()
  "Convert location names to ID links in current scene.
Updates :Location: properties."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((updated-locs (org-scribe--link-locations-in-property "Location")))
      (cond
       (updated-locs
        (message (org-scribe-msg 'msg-updated-location)))
       (t
        (message (org-scribe-msg 'msg-no-updates-needed)))))))

;;;###autoload
(defun org-scribe/link-all-scene-locations ()
  "Convert location names to ID links in all scenes in current buffer.
Processes all headings with :Location: properties."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (org-map-entries
       (lambda ()
         ;; Process if has PoV or Locations property
         (when (org-entry-get nil "Location")
           (let ((updated-locs (org-scribe--link-locations-in-property "Location")))
             (when  updated-locs)
             (setq count (1+ count)))))
      nil 'file)
    (message (org-scribe-msg 'msg-updated-location-links count (org-scribe-plural count ""))))))

;;; Integration with Capture System

(defun org-scribe--capture-finalize-add-id ()
  "Hook function to add ID to newly captured locations.
This is called before a location capture is finalized.
Runs in the capture buffer before it's filed.

This is a safety net - the location capture template already includes
ID generation via %(org-id-new), but this ensures any location heading
without an ID gets one automatically."
  (when (and (boundp 'org-capture-mode)
             org-capture-mode
             (buffer-file-name))
    ;; Check if we're capturing to a locations file
    (let ((target (org-scribe--get-location-file)))
      (when (and target
                 (file-exists-p target)
                 ;; Compare the target file with current buffer's file
                 ;; or the file we're capturing to
                 (or (string= (buffer-file-name) target)
                     (string= (buffer-file-name) (expand-file-name target))))
        ;; We're capturing a location, ensure it has an ID
        (save-excursion
          (goto-char (point-min))
          ;; In capture buffer, find the heading we're creating
          (when (re-search-forward "^\\*+ " nil t)
            (org-back-to-heading)
            (unless (org-entry-get nil "ID")
              (org-id-get-create)
              (message "Auto-created ID for new location (via hook)"))))))))

;; Add the hook - use before-finalize to ensure we're still in capture buffer
;; Note: This is redundant with the template's %(org-id-new) but serves as a safety net
(add-hook 'org-capture-before-finalize-hook #'org-scribe--capture-finalize-add-id)

;;;###autoload
(defun org-scribe/setup-location-links ()
  "Set up location linking system for current project.
This function:
1. Adds IDs to all existing locations
2. Ensures the capture hook is active
3. Optionally links existing scenes

Run this once when setting up ID-based location linking
in an existing project."
  (interactive)
  (message (org-scribe-msg 'msg-setting-up-location-links))

  ;; Step 1: Add IDs to locations
  (org-scribe/add-location-ids)

  ;; Step 2: Ask if user wants to link existing scenes
  (when (y-or-n-p (org-scribe-msg 'question-link-existing-locations))
    (let ((novel-file (plist-get (org-scribe-project-structure) :novel-file)))
      (when (and novel-file (file-exists-p novel-file))
        (with-current-buffer (find-file-noselect novel-file)
          (org-scribe/link-all-scene-locations)
          (save-buffer)))))

  (message (org-scribe-msg 'msg-location-setup-complete)))

;;; Update Link Display Names

(require 'org-scribe-link-update)

;;;###autoload
(defun org-scribe/update-location-link-names ()
  "Update location link display names in current scene.

Refreshes :Location: property to show current location names from
the locations database.

Use this after renaming a location in locations.org. The ID links
will still work, but this updates the display text to match the
current name.

Example:
  Before: [[id:loc-downtown-001][Downtown Cafe]]
  After rename in database: [[id:loc-downtown-001][Downtown Coffee Shop]]

Returns t if any updates were made, nil otherwise."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let* ((locs-alist (org-scribe--get-all-locations))
           (id-map (org-scribe--build-id-to-name-map locs-alist))
           (updated-locs (org-scribe--update-links-in-property "Location" id-map)))
      (if updated-locs
          (message "Updated Location link names")
        (message "No location link names needed updating"))
      updated-locs)))

;;;###autoload
(defun org-scribe/update-all-location-link-names ()
  "Update location link display names in all scenes.

Scans locations database for current names and updates the display
text portion of ID links in :Location: properties throughout
the entire manuscript.

This is useful after renaming locations in locations.org, as ID
links will still work but show the old name. This function refreshes
all display names to match the current database.

Example workflow:
  1. Rename \"Downtown Cafe\" to \"Downtown Coffee Shop\" in locations.org
  2. Run this function (M-x org-scribe/update-all-location-link-names)
  3. All scenes updated automatically!

Returns the number of scenes updated."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((locs-alist (org-scribe--get-all-locations))
           (id-map (org-scribe--build-id-to-name-map locs-alist))
           (count 0))
      (org-map-entries
       (lambda ()
         (when (org-entry-get nil "Location")
           (let ((updated-locs (org-scribe--update-links-in-property "Location" id-map)))
             (when updated-locs
               (setq count (1+ count))))))
       nil 'file)
      (message "Updated location link names in %d scene%s"
               count (if (= count 1) "" "s"))
      count)))

(provide 'org-scribe-location-links)

;;; org-scribe-location-links.el ends here
