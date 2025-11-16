;;; writing-location-links.el --- Location linking system for emacs-writing -*- lexical-binding: t; -*-

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
(require 'writing-core)
(require 'writing-capture)

;;; Location ID Management

(defun writing--ensure-location-has-id ()
  "Ensure the current location heading has a unique ID.
Creates an ID if one doesn't exist. Returns the ID."
  (org-id-get-create))

(defun writing--add-id-to-all-locations ()
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
      (message "Added IDs to %d location heading%s"
               count (if (= count 1) "" "s"))))

(defun writing--get-location-name-at-point ()
  "Get the location name from current heading or NAME property."
  (or (org-entry-get nil "NAME")
      (org-get-heading t t t t)))

;;; Location Database Functions

(defun writing--get-location-file ()
  "Get the path to the locations file for the current project."
  (writing/capture-location-file))

(defun writing--get-all-locations ()
  "Return alist of (LOCATION-NAME . (ID . HEADING)) from locations file.
Returns list of (NAME . (ID . HEADING-TEXT)) for all locations in the project."
  (let ((loc-file (writing--get-location-file))
        result)
    (when (and loc-file (file-exists-p loc-file))
      (with-current-buffer (find-file-noselect loc-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (let* ((level (org-current-level))
                   (id (org-id-get))
                   (name (writing--get-location-name-at-point))
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

(defun writing--create-location-link (loc-name id-alist)
  "Create an ID link for LOC-NAME using ID-ALIST.
ID-ALIST should be in format ((NAME . (ID . HEADING)) ...).
Returns the link string or plain text if no ID found."
  (if-let* ((entry (assoc loc-name id-alist))
            (id (cadr entry)))
      (format "[[id:%s][%s]]" id loc-name)
    loc-name)) ; Fallback to plain text if no ID found

;;; Interactive Functions

;;;###autoload
(defun writing/add-location-ids ()
  "Add unique IDs to all locations in the locations file.
This should be run once on existing projects to set up
the ID-based linking system."
  (interactive)
  (let ((loc-file (writing--get-location-file)))
    (if (not (file-exists-p loc-file))
        (message "No location file found. Create location first.")
      (with-current-buffer (find-file-noselect loc-file)
        (writing--add-id-to-all-locations)
        (save-buffer)
        (message "Location IDs updated in %s" loc-file)))))

;;;###autoload
(defun writing/insert-location-link ()
  "Insert a location link in the current property.
Scans the locations file, presents a completion menu,
and inserts the selected location as an ID link.

Use this function when adding locations to scene properties."
  (interactive)
  (let* ((locs (writing--get-all-locations))
         (loc-names (mapcar #'car locs)))
    (if (null loc-names)
        (message "No locations found. Create locations first or add IDs with writing/add-location-ids.")
      (let* ((selected (completing-read "Select location: " loc-names nil t))
             (entry (assoc selected locs))
             (id (cadr entry)))
        (if id
            (progn
              (insert (format "[[id:%s][%s]]" id selected))
              (message "Inserted link to %s" selected))
          (message "No ID found for %s" selected))))))

;;;###autoload
(defun writing/insert-multiple-location-links ()
  "Insert multiple location links separated by commas.
Useful for the :Location: property which often lists
multiple locations in a scene."
  (interactive)
  (let* ((locs (writing--get-all-locations))
         (loc-names (mapcar #'car locs))
         selected-locs
         links)
    (if (null loc-names)
        (message "No locations found. Create locations first or add IDs with writing/add-location-ids.")
      ;; Multiple selection loop
      (while (let ((choice (completing-read
                           "Select location (RET to finish): "
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
            (message "Inserted %d location link%s"
                    (length links)
                    (if (= (length links) 1) "" "s")))
        (message "No locations selected")))))

;;;###autoload
(defun writing/set-scene-locations ()
  "Set the Locations property to multiple location ID links.
Specifically designed for the :Location: property in scene headings."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (let* ((locs (writing--get-all-locations))
         (loc-names (mapcar #'car locs))
         selected-locs
         links)
    (if (null loc-names)
        (message "No locations found. Create locations first or add IDs with writing/add-location-ids.")
      ;; Multiple selection loop
      (while (let ((choice (completing-read
                           "Select location (RET to finish): "
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
            (message "Set Location to: %s" (string-join selected-locs ", ")))
        (message "No locations selected")))))

;;; Batch Update Functions

(defun writing--link-locations-in-property (property-name)
  "Convert location names to ID links in PROPERTY-NAME of current heading.
Handles both single locations and comma-separated lists."
  (when-let ((prop-value (org-entry-get nil property-name)))
    (let* ((id-alist (writing--get-all-locations))
           ;; Split on comma, trim whitespace
           (loc-list (mapcar #'string-trim
                             (split-string prop-value "," t)))
           ;; Create links for each location
           (linked-locs (mapcar (lambda (name)
                                  (writing--create-location-link name id-alist))
                                loc-list))
           (linked-string (string-join linked-locs ", ")))
      ;; Only update if we actually created links
      (unless (string= prop-value linked-string)
        (org-set-property property-name linked-string)
        t))))

;;;###autoload
(defun writing/link-scene-locations ()
  "Convert location names to ID links in current scene.
Updates :Location: properties."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((updated-locs (writing--link-locations-in-property "Location")))
      (cond
       (updated-locs
        (message "Updated Location property"))
       (t
        (message "No location properties found or already linked"))))))

;;;###autoload
(defun writing/link-all-scene-locations ()
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
           (let ((updated-locs (writing--link-locations-in-property "Location")))
             (when  updated-locs)
             (setq count (1+ count)))))
      nil 'file)
    (message "Updated location links in %d scene%s"
             count (if (= count 1) "" "s")))))

;;; Integration with Capture System

(defun writing--capture-finalize-add-id ()
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
    (let ((target (writing--get-location-file)))
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
(add-hook 'org-capture-before-finalize-hook #'writing--capture-finalize-add-id)

;;;###autoload
(defun writing/setup-location-links ()
  "Set up location linking system for current project.
This function:
1. Adds IDs to all existing locations
2. Ensures the capture hook is active
3. Optionally links existing scenes

Run this once when setting up ID-based location linking
in an existing project."
  (interactive)
  (message "Setting up location linking system...")

  ;; Step 1: Add IDs to locations
  (writing/add-location-ids)

  ;; Step 2: Ask if user wants to link existing scenes
  (when (y-or-n-p "Link locations in existing scenes? ")
    (let ((novel-file (plist-get (writing-project-structure) :novel-file)))
      (when (and novel-file (file-exists-p novel-file))
        (with-current-buffer (find-file-noselect novel-file)
          (writing/link-all-scene-locations)
          (save-buffer)))))

  (message "Location linking system setup complete!"))

(provide 'writing-location-links)

;;; writing-location-links.el ends here
