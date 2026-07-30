;;; org-scribe-character-relationships.el --- Character relationship system for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides simple, typed relationship tracking between characters.
;; A relationship is directed: SOURCE -> (TARGET, TYPE), stored on the
;; source character's heading in the :RelationshipsData: property
;; (or :DatosRelaciones: for Spanish-language projects).
;;
;; Entries are separated by "; ", each in the format:
;;   [[id:TARGET-ID][Target Name]]|TYPE
;;
;; Where TYPE is a relationship type (mentor, rival, friend, family,
;; lover, enemy, ally, etc.) — see `org-scribe-relationship-types'.
;;
;; Example: "Ana — sister — Luis" is stored on Ana's heading as
;;   [[id:char-luis-001][Luis]]|sister
;;
;; Usage:
;;   M-x org-scribe-add-relationship     - Add a relationship to current character
;;   M-x org-scribe-remove-relationship  - Remove a relationship
;;   M-x org-scribe-show-character-relationships - List a character's relationships
;;   M-x org-scribe-show-all-relationships - Table of every relationship in the project

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-scribe-core)
(require 'org-scribe-character-links)
(require 'org-scribe-messages)

;;; Relationship Type Definitions

(defvar org-scribe-relationship-types
  '("mentor" "mentee" "opponent" "rival" "friend" "family" "lover" "enemy" "ally"
    "partner" "colleague" "acquaintance" "subordinate" "superior"
    "student" "teacher" "sibling" "parent" "child" "spouse")
  "Common relationship types for character relationships.")

;;; Parsing Functions

(defun org-scribe--parse-single-relationship (rel-string)
  "Parse a single relationship string into components.
REL-STRING should be in format: [[id:...][Name]]|TYPE
Returns (ID NAME TYPE) or nil if parsing fails."
  (when (and rel-string (string-match
                         "\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]|\\(.+\\)"
                         rel-string))
    (list (match-string 1 rel-string)     ; ID
          (match-string 2 rel-string)     ; Name
          (match-string 3 rel-string))))  ; Type

(defun org-scribe--parse-relationships (rel-property-value)
  "Parse relationship property value into list of relationships.
REL-PROPERTY-VALUE is the value of :RelationshipsData: property.
Returns list of (ID NAME TYPE) tuples."
  (when (and rel-property-value (not (string-empty-p rel-property-value)))
    (let ((parts (split-string rel-property-value ";" t " "))
          result)
      (dolist (part parts)
        (when-let ((parsed (org-scribe--parse-single-relationship part)))
          (push parsed result)))
      (nreverse result))))

(defun org-scribe--format-relationship (id name type)
  "Format a relationship into the standard string format.
Returns: [[id:ID][NAME]]|TYPE"
  (format "[[id:%s][%s]]|%s" id name type))

(defun org-scribe--relationships-to-string (relationships)
  "Convert list of relationship tuples to property string.
RELATIONSHIPS is a list of (ID NAME TYPE) tuples.
Returns a string suitable for :RelationshipsData: property."
  (mapconcat
   (lambda (rel)
     (apply #'org-scribe--format-relationship rel))
   relationships
   "; "))

;;; Character Relationship Database

(defun org-scribe--get-character-relationships ()
  "Get relationships for character at point.
Returns list of (ID NAME TYPE) tuples."
  (let ((rel-data (org-entry-get nil "RelationshipsData"))
        (datos-rel (org-entry-get nil "DatosRelaciones"))) ; Spanish property name
    (org-scribe--parse-relationships (or rel-data datos-rel))))

(defun org-scribe--find-character-by-name (char-name)
  "Find character by CHAR-NAME in characters file.
Returns the marker position of the character heading, or nil if not found."
  (let ((char-file (org-scribe--get-character-file))
        found-marker)
    (when (and char-file (file-exists-p char-file))
      (with-current-buffer (find-file-noselect char-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (when (and (not found-marker)
                      (string= (org-scribe--get-character-name-at-point) char-name))
              (setq found-marker (point-marker))))
          nil 'file))))
    found-marker))

(defun org-scribe--update-character-relationships (char-name relationships)
  "Update CHAR-NAME's relationships to RELATIONSHIPS.
RELATIONSHIPS is a list of (ID NAME TYPE) tuples."
  (when-let ((marker (org-scribe--find-character-by-name char-name)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (let* ((rel-string (if relationships
                             (org-scribe--relationships-to-string relationships)
                           ""))
              (property-name (if (org-entry-get nil "DatosRelaciones")
                               "DatosRelaciones"
                             "RelationshipsData")))
         (if (string-empty-p rel-string)
             (org-entry-delete nil property-name)
           (org-entry-put nil property-name rel-string))
         (save-buffer))))
    t))

(defun org-scribe--get-character-relationships-by-name (char-name)
  "Get relationships for CHAR-NAME.
Returns list of (ID NAME TYPE) tuples."
  (when-let ((marker (org-scribe--find-character-by-name char-name)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (org-scribe--get-character-relationships)))))

(defun org-scribe--get-all-relationships ()
  "Return alist of all character relationships in the project.
Format: ((CHAR-NAME . RELATIONSHIPS) ...) where RELATIONSHIPS is
list of (ID NAME TYPE) tuples."
  (let ((char-file (org-scribe--get-character-file))
        result)
    (when (and char-file (file-exists-p char-file))
      (with-current-buffer (find-file-noselect char-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (let* ((id (org-id-get))
                   (name (org-scribe--get-character-name-at-point))
                   (relationships (org-scribe--get-character-relationships)))
              (when (and id name relationships)
                (push (cons name relationships) result))))
          nil 'file))))
    (nreverse result)))

;;; Interactive Relationship Management

(defun org-scribe--with-source-character (min-chars action-fn)
  "Select a source character and call ACTION-FN with the selection.
ACTION-FN is called with (ALL-CHARS SOURCE-NAME EXISTING-RELS).
If fewer than MIN-CHARS characters exist, displays a message and returns nil."
  (let* ((all-chars (org-scribe--get-all-characters))
         (char-names (mapcar #'car all-chars)))
    (if (< (length char-names) min-chars)
        (message (org-scribe-msg (if (zerop (length char-names))
                                     'error-no-characters-found
                                   'msg-no-other-characters)))
      (let* ((source-name (completing-read (org-scribe-msg 'prompt-relationship-from-character)
                                           char-names nil t))
             (existing-rels (org-scribe--get-character-relationships-by-name source-name)))
        (funcall action-fn all-chars source-name existing-rels)))))

;;;###autoload
(defun org-scribe-add-relationship ()
  "Add a relationship between two characters.
Prompts for source character, target character, and relationship type."
  (interactive)
  (org-scribe--with-source-character
   2
   (lambda (all-chars source-name existing-rels)
     ;; Filter out source character from target selection
     (let* ((other-chars (seq-remove (lambda (c) (string= (car c) source-name))
                                     all-chars))
            (other-char-names (mapcar #'car other-chars)))
       (if (null other-char-names)
           (message (org-scribe-msg 'msg-no-other-characters))
         ;; Ask for target character
         (let* ((target-name (completing-read (org-scribe-msg 'prompt-relationship-to-character)
                                              other-char-names nil t))
                (target-entry (assoc target-name all-chars))
                (target-id (cadr target-entry))
                ;; Ask for relationship type
                (rel-type (completing-read (org-scribe-msg 'prompt-relationship-type)
                                           org-scribe-relationship-types
                                           nil nil))
                ;; Create new relationship tuple
                (new-rel (list target-id target-name rel-type))
                ;; Add to existing relationships
                (all-rels (append existing-rels (list new-rel))))
           ;; Update the source character's relationships
           (org-scribe--update-character-relationships source-name all-rels)
           (message (org-scribe-msg 'msg-added-relationship
                                    rel-type source-name target-name))))))))

;;;###autoload
(defun org-scribe-remove-relationship ()
  "Remove a relationship from a character.
Prompts for source character, then which relationship to remove."
  (interactive)
  (org-scribe--with-source-character
   1
   (lambda (_all-chars source-name existing-rels)
     (if (null existing-rels)
         (message (org-scribe-msg 'msg-no-relationships))
       ;; Ask which relationship to remove
       (let* ((rel-choices (mapcar (lambda (rel)
                                     (format "%s (%s)"
                                             (nth 1 rel)  ; Name
                                             (nth 2 rel))) ; Type
                                   existing-rels))
              (selected (completing-read (org-scribe-msg 'prompt-remove-relationship)
                                         rel-choices nil t))
              (selected-idx (cl-position selected rel-choices :test #'string=))
              (remaining-rels (append (cl-subseq existing-rels 0 selected-idx)
                                      (cl-subseq existing-rels (1+ selected-idx)))))
         ;; Update the source character's relationships
         (org-scribe--update-character-relationships source-name remaining-rels)
         (message (org-scribe-msg 'msg-removed-relationship source-name selected)))))))

;;; Display

(defun org-scribe--format-relationship-list (char-name relationships)
  "Format CHAR-NAME's RELATIONSHIPS as a plain list.
RELATIONSHIPS is list of (ID NAME TYPE) tuples."
  (let ((lines (list (format "Relationships for %s:" char-name) "")))
    (dolist (rel relationships)
      (push (format "  - %s: %s" (nth 2 rel) (nth 1 rel)) lines))
    (string-join (nreverse lines) "\n")))

(defun org-scribe--format-relationship-table (all-relationships)
  "Format ALL-RELATIONSHIPS as an org-mode table.
ALL-RELATIONSHIPS is alist of (CHAR-NAME . RELATIONSHIPS)."
  (let ((lines (list "|-----------+------------+------|"
                    "| Character | Related To | Type |")))
    (dolist (entry all-relationships)
      (let ((char-name (car entry))
            (rels (cdr entry)))
        (dolist (rel rels)
          (push (format "| %s | %s | %s |"
                       (org-scribe--escape-table-cell char-name)
                       (org-scribe--escape-table-cell (nth 1 rel))  ; Name
                       (org-scribe--escape-table-cell (nth 2 rel))) ; Type
                lines))))
    (string-join (nreverse lines) "\n")))

;;;###autoload
(defun org-scribe-show-character-relationships ()
  "Display relationships for a selected character in a temporary buffer."
  (interactive)
  (let* ((all-chars (org-scribe--get-all-characters))
         (char-names (mapcar #'car all-chars)))

    (if (null char-names)
        (message (org-scribe-msg 'error-no-characters-found))
      ;; Ask for which character to view
      (let* ((char-name (completing-read (org-scribe-msg 'prompt-relationship-from-character)
                                        char-names nil t))
             (relationships (org-scribe--get-character-relationships-by-name char-name)))

        (if (null relationships)
            (message (org-scribe-msg 'error-no-relationships char-name))
          (let ((list-text (org-scribe--format-relationship-list char-name relationships))
                (buf-name (format "*Relationships: %s*" char-name)))
            (with-current-buffer (get-buffer-create buf-name)
              (erase-buffer)
              (insert list-text)
              (insert "\n")
              (goto-char (point-min))
              (view-mode 1))
            (display-buffer buf-name)))))))

;;;###autoload
(defun org-scribe-show-all-relationships ()
  "Display all character relationships in the project as a table."
  (interactive)
  (let ((all-rels (org-scribe--get-all-relationships)))
    (if (null all-rels)
        (message (org-scribe-msg 'msg-no-relationships-in-project))
      (let ((table (org-scribe--format-relationship-table all-rels))
            (buf-name "*All Character Relationships*"))
        (with-current-buffer (get-buffer-create buf-name)
          (erase-buffer)
          (org-mode)
          (insert "#+TITLE: Character Relationships\n\n")
          (insert table)
          ;; Move to the table and align it
          (goto-char (point-min))
          (re-search-forward "^|" nil t)  ; Find the first table line
          (org-table-align)
          (goto-char (point-min))
          (view-mode 1))
        (display-buffer buf-name)))))

;;; Setup Function

;;;###autoload
(defun org-scribe-setup-character-relationships ()
  "Initialize character relationship system for current project.
Ensures all characters have IDs and adds empty RelationshipsData properties."
  (interactive)
  (let ((char-file (org-scribe--get-character-file)))
    (if (not (file-exists-p char-file))
        (message "No character file found. Create characters first.")
      (with-current-buffer (find-file-noselect char-file)
        ;; First ensure all characters have IDs
        (org-scribe--add-id-to-all-characters)

        ;; Then add RelationshipsData property if missing
        (org-with-wide-buffer
         (goto-char (point-min))
         (let ((count 0))
           (org-map-entries
            (lambda ()
              (when (and (org-id-get)
                        (not (org-entry-get nil "RelationshipsData"))
                        (not (org-entry-get nil "DatosRelaciones")))
                (org-entry-put nil "RelationshipsData" "")
                (setq count (1+ count))))
            nil 'file)
           (when (> count 0)
             (save-buffer))
           (message (org-scribe-msg 'msg-relationship-setup-complete
                                   count
                                   (org-scribe-plural count "")))))))))

(provide 'org-scribe-character-relationships)

;;; org-scribe-character-relationships.el ends here
