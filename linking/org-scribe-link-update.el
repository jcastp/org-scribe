;;; org-scribe-link-update.el --- Link display name update functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides functions to update the display names of ID links when
;; entities are renamed in database files (characters.org, locations.org,
;; plot.org).
;;
;; When you rename a character from "Alex" to "Alexandra" in characters.org,
;; the ID links still work (they're ID-based), but the display text shows
;; the old name. These functions refresh the display text to match the
;; current database names.
;;
;; Features:
;; - Update single link display name
;; - Build ID-to-name mapping from database
;; - Update links in org properties
;; - Unified function to update all link types at once

;;; Code:

(require 'org)
(require 'org-scribe-core)

;;; Core Helper Functions

(defun org-scribe--update-link-display-name (link-string id-to-name-map)
  "Update display name in LINK-STRING using ID-TO-NAME-MAP.

LINK-STRING should be an org ID link like \"[[id:abc123][Old Name]]\".
ID-TO-NAME-MAP is a hash table mapping IDs to current names.

Returns the updated link string, or the original if:
- The link is not an ID link
- The ID is not found in the map
- The name already matches

Examples:
  Input:  \"[[id:char-alex-001][Alex Rivera]]\"
  Map:    {\"char-alex-001\" -> \"Alexandra Rivera\"}
  Output: \"[[id:char-alex-001][Alexandra Rivera]]\"

  Input:  \"Plain Text\"
  Output: \"Plain Text\" (unchanged)"
  (if (string-match "\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]*\\)\\]\\]" link-string)
      (let ((id (match-string 1 link-string))
            (old-name (match-string 2 link-string)))
        (if-let ((new-name (gethash id id-to-name-map)))
            (if (string= old-name new-name)
                link-string  ; No change needed
              ;; Name changed - update it
              (format "[[id:%s][%s]]" id new-name))
          ;; ID not found in database - keep as is (don't break working links)
          link-string))
    ;; Not an ID link - return unchanged
    link-string))

(defun org-scribe--build-id-to-name-map (items-alist)
  "Build hash table mapping ID to NAME from ITEMS-ALIST.

ITEMS-ALIST should be in format: ((NAME . (ID . HEADING)) ...)
This is the format returned by org-scribe--get-all-characters and similar.

Returns a hash table: {ID: NAME, ...}

Example:
  Input:  '((\"Alex Rivera\" . (\"char-alex-001\" . \"Alex Rivera\"))
           (\"Sam Chen\" . (\"char-sam-002\" . \"Sam Chen\")))
  Output: Hash table: {\"char-alex-001\" -> \"Alex Rivera\",
                       \"char-sam-002\" -> \"Sam Chen\"}"
  (let ((map (make-hash-table :test 'equal)))
    (dolist (item items-alist)
      (let ((name (car item))
            (id (cadr item)))
        (when (and id name)
          (puthash id name map))))
    map))

(defun org-scribe--update-links-in-property (property-name id-to-name-map)
  "Update link display names in PROPERTY-NAME of current heading.

Handles both single links and comma-separated lists of links.
Uses ID-TO-NAME-MAP to look up current names for each ID.

Returns t if any changes were made, nil otherwise.

This function:
1. Gets the property value
2. Splits on commas (for multi-value properties)
3. Updates each link's display name
4. Joins back together
5. Updates property if changed

Example:
  Property: \":Characters: [[id:abc][Alex]], [[id:def][Sam]]\"
  After rename: \":Characters: [[id:abc][Alexandra]], [[id:def][Samuel]]\""
  (when-let ((prop-value (org-entry-get nil property-name)))
    (let* ((links (split-string prop-value "," t))
           (updated-links (mapcar
                           (lambda (link)
                             (org-scribe--update-link-display-name
                              (string-trim link)
                              id-to-name-map))
                           links))
           (updated-string (string-join updated-links ", ")))
      ;; Only update if something changed
      (unless (string= prop-value updated-string)
        (org-set-property property-name updated-string)
        t))))

;;; Unified Update Function

;;;###autoload
(defun org-scribe/update-all-link-names ()
  "Update display names for all ID links (characters, locations, plots).

This function scans all database files (characters.org, locations.org,
plot.org) to get the current names, then updates the display text of
all ID links in scene properties throughout the manuscript.

Use this after renaming entities in your planning files. The ID links
will still work (they're ID-based), but this refreshes the display
text to show the current names.

Example:
  1. Rename \"Alex Rivera\" to \"Alexandra Rivera\" in characters.org
  2. Run this function
  3. All scenes updated: [[id:char-alex-001][Alexandra Rivera]]

Processes these properties:
  - :PoV: (character links)
  - :Characters: (character links)
  - :Location: (location links)
  - :Plot: (plot thread links)

Returns the number of scenes updated."
  (interactive)
  (let ((chars-map (condition-case nil
                       (progn
                         (require 'org-scribe-character-links)
                         (org-scribe--build-id-to-name-map (org-scribe--get-all-characters)))
                     (error (make-hash-table :test 'equal))))
        (locs-map (condition-case nil
                      (progn
                        (require 'org-scribe-location-links)
                        (org-scribe--build-id-to-name-map (org-scribe--get-all-locations)))
                    (error (make-hash-table :test 'equal))))
        (plots-map (condition-case nil
                       (progn
                         (require 'org-scribe-plot-links)
                         (org-scribe--build-id-to-name-map (org-scribe--get-all-plot-threads)))
                     (error (make-hash-table :test 'equal))))
        (count 0))
    (save-excursion
      (goto-char (point-min))
      ;; Single pass through all scenes
      (org-map-entries
       (lambda ()
         (when (or (org-entry-get nil "PoV")
                   (org-entry-get nil "Characters")
                   (org-entry-get nil "Location")
                   (org-entry-get nil "Plot"))
           (let ((updated-pov (org-scribe--update-links-in-property "PoV" chars-map))
                 (updated-chars (org-scribe--update-links-in-property "Characters" chars-map))
                 (updated-loc (org-scribe--update-links-in-property "Location" locs-map))
                 (updated-plot (org-scribe--update-links-in-property "Plot" plots-map)))
             (when (or updated-pov updated-chars updated-loc updated-plot)
               (setq count (1+ count))))))
       nil 'file))
    (message "Updated link names in %d scene%s" count (if (= count 1) "" "s"))
    count))

(provide 'org-scribe-link-update)

;;; org-scribe-link-update.el ends here
