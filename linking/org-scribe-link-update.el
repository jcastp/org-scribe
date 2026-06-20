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
(require 'org-scribe-config)
(require 'org-scribe-messages)

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
  "Update display names for all ID links in scene properties.

Reads entity types from `org-scribe-entity-registry'; any entity type
registered there (characters, locations, plot threads, and future types)
is automatically included without changes to this function.

Use this after renaming entities in planning files.  The ID links still
work (they are ID-based), but this refreshes display text to show the
current names.

Returns the number of scenes updated."
  (interactive)
  (require 'org-scribe-linking-core)
  ;; Build (property-name . id-map) alist from every registered entity.
  ;; An entity may own several properties (e.g. character owns PoV + Characters);
  ;; each gets its own entry pointing to the same id-map object.
  (let ((prop-maps
         (apply #'append
                (mapcar
                 (lambda (entry)
                   (let* ((entity (cdr entry))
                          (id-map (condition-case nil
                                      (org-scribe--build-id-to-name-map
                                       (org-scribe--get-all-entities entity))
                                    (error (make-hash-table :test 'equal)))))
                     (mapcar (lambda (prop) (cons prop id-map))
                             (plist-get entity :properties))))
                 org-scribe-entity-registry)))
        (count 0))
    (save-excursion
      (goto-char (point-min))
      (org-map-entries
       (lambda ()
         (let ((any-updated nil))
           (dolist (pm prop-maps)
             (when (org-scribe--update-links-in-property (car pm) (cdr pm))
               (setq any-updated t)))
           (when any-updated
             (setq count (1+ count)))))
       nil 'file))
    (message (org-scribe-msg 'msg-updated-all-links-scene count (org-scribe-plural count "")))
    count))

;;; One-Shot Project Relink (registry-driven)

(declare-function org-scribe--add-entity-ids "org-scribe-linking-core")
(declare-function org-scribe--link-all-scene-entities "org-scribe-linking-core")

;;;###autoload
(defun org-scribe/relink-project ()
  "Tidy every ID link in the project in a single pass.

For each entity type registered in `org-scribe-entity-registry'
\(characters, locations, plot threads, and any future types) this:

1. ensures every entity heading in its database file has an ID
   \(saving those files),
2. converts plain-text entity names in scene properties to ID links, and
3. refreshes the display text of existing links to match current names.

This one command replaces the per-entity add-ids / link-all /
update-names maintenance commands — and, being driven off the registry,
automatically covers new entity types with no change here.  The
manuscript buffer is saved when changes were made."
  (interactive)
  (require 'org-scribe-linking-core)
  ;; Phase 1 — make sure every entity has an ID in its database file.
  (dolist (entry org-scribe-entity-registry)
    (let* ((entity (cdr entry))
           (file (funcall (plist-get entity :file-fn))))
      (when (and file (file-exists-p file))
        (org-scribe--add-entity-ids entity))))
  ;; Phases 2 & 3 operate on the manuscript.
  (let ((novel-file (plist-get (org-scribe-project-structure) :novel-file)))
    (if (not (and novel-file (file-exists-p novel-file)))
        (user-error (org-scribe-msg 'msg-relink-no-novel))
      (with-current-buffer (find-file-noselect novel-file)
        ;; Phase 2 — convert plain names to links for every entity type.
        (dolist (entry org-scribe-entity-registry)
          (org-scribe--link-all-scene-entities (cdr entry)))
        ;; Phase 3 — refresh stale display names (already registry-driven).
        (let ((count (org-scribe/update-all-link-names)))
          (when (buffer-modified-p)
            (save-buffer))
          (message (org-scribe-msg 'msg-relink-complete
                                   count (org-scribe-plural count "")
                                   (file-name-nondirectory novel-file))))))))

;;; Auto-propagation on entity file save

(defun org-scribe--auto-update-links-after-save ()
  "Update link display names in the manuscript when an entity file is saved.
Runs via `after-save-hook', but only when `org-scribe-auto-relink' is
non-nil.  Beyond that, it acts only when all of the following are true:
- The saved buffer visits a file in an org-scribe project.
- That file is the characters, locations, or plot database for the project.
- The manuscript (novel.org) is already open in another buffer.

When the conditions are met, runs `org-scribe/update-all-link-names' in
the manuscript buffer and reports the number of scenes updated.  The
manuscript is NOT saved automatically; the message reminds the user to
save if any changes were made."
  (when (and org-scribe-auto-relink
             buffer-file-name (derived-mode-p 'org-mode))
    (ignore-errors
      (when-let* ((struct (org-scribe-project-structure))
                  (entity-files
                   (delq nil (list (plist-get struct :characters-file)
                                   (plist-get struct :locations-file)
                                   (plist-get struct :plot-file))))
                  ;; Only act when the saved file is an entity database file.
                  ((member (expand-file-name buffer-file-name)
                           (mapcar #'expand-file-name entity-files)))
                  (novel-file (plist-get struct :novel-file))
                  ((file-exists-p novel-file))
                  ;; Only act when the manuscript is already open (never
                  ;; open files unexpectedly as a side effect of saving).
                  (novel-buf (find-buffer-visiting novel-file)))
        (with-current-buffer novel-buf
          (let ((count (org-scribe/update-all-link-names)))
            (when (> count 0)
              (message "org-scribe: updated link names in %d scene(s) in %s — save to persist."
                       count (file-name-nondirectory novel-file)))))))))

(add-hook 'after-save-hook #'org-scribe--auto-update-links-after-save)

(provide 'org-scribe-link-update)

;;; org-scribe-link-update.el ends here
