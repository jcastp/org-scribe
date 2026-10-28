;;; org-scribe-character-relationships.el --- Character relationship system for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides simple, typed relationship tracking between characters.
;; A relationship is directed: SOURCE -> (TARGET, TYPE), stored on the
;; source character's heading in the :RelationshipsData: property.
;; Older Spanish templates shipped a localized spelling of that property;
;; every pack's spelling is still read (see
;; `org-scribe--relationships-property-at-point').
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

(defvar org-scribe-relationship-types)

(defun org-scribe-relationship-type-choices ()
  "Return the relationship types to suggest, as a list of strings.
`org-scribe-relationship-types' when the writer set it, else the list in
the project language's pack, else the English one."
  (or org-scribe-relationship-types
      (plist-get (org-scribe-lang-pack (org-scribe-project-language))
                 :relationship-types)
      (plist-get (org-scribe-lang-pack 'en) :relationship-types)))

;;; Parsing Functions

(defun org-scribe--parse-single-relationship (rel-string)
  "Parse a single relationship string into components.
REL-STRING should be in format: [[id:...][Name]]|TYPE, or the older
[[id:...][Name]](TYPE,STRENGTH,VALENCE), of which only TYPE is read.
Returns (ID NAME TYPE) or nil if parsing fails.

An empty TYPE parses as \"\": older versions accepted an empty answer
and stored such entries, and dropping them here made them invisible and
deleted them on the character's next add or remove."
  (when (and rel-string
             (string-match "\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" rel-string))
    (let ((id (match-string 1 rel-string))
          (name (match-string 2 rel-string))
          (rest (substring rel-string (match-end 0))))
      (cond
       ((string-prefix-p "|" rest)
        (list id name (string-trim (substring rest 1))))
       ;; The older shape, \"(TYPE,STRENGTH,VALENCE)\": only the type is
       ;; kept.  Writing always uses the \"|TYPE\" form, so an entry read this
       ;; way is converted the next time its character's relationships are
       ;; written, instead of being dropped.
       ((string-match "\\`(\\([^,)]*\\),[^)]*)" rest)
        (list id name (string-trim (match-string 1 rest))))))))

(defun org-scribe--parse-relationships (rel-property-value)
  "Parse relationship property value into list of relationships.
REL-PROPERTY-VALUE is the value of :RelationshipsData: property.
Returns list of (ID NAME TYPE) tuples.

Entries are split where a new link starts, not on every \";\": the
type is free text, so \"friend; almost a brother\" is one type, and
splitting on the bare separator silently dropped everything after it."
  (when (and rel-property-value (not (string-empty-p rel-property-value)))
    (let ((starts nil)
          (pos 0)
          result)
      (while (string-match "\\[\\[id:" rel-property-value pos)
        (push (match-beginning 0) starts)
        (setq pos (match-end 0)))
      (setq starts (nreverse starts))
      (while starts
        (let* ((part (substring rel-property-value (car starts) (cadr starts)))
               (part (string-trim part nil "[ \t;]+")))
          (when-let* ((parsed (org-scribe--parse-single-relationship part)))
            (push parsed result)))
        (setq starts (cdr starts)))
      (nreverse result))))

(defun org-scribe--format-relationship (id name type)
  "Format a relationship into the standard string format.
Returns: [[id:ID][NAME]]|TYPE"
  (format "%s|%s" (org-scribe--format-id-link id name) type))

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

(defun org-scribe--read-relationship-type ()
  "Read a relationship type, asking again until it is not blank.
An empty type used to be announced as added and then never shown."
  (let (type)
    (while (string-empty-p
            (setq type (string-trim
                        (completing-read (org-scribe-msg 'prompt-relationship-type)
                                         (org-scribe-relationship-type-choices) nil nil))))
      (message "%s" (org-scribe-msg 'msg-relationship-type-required))
      (sit-for 1))
    type))

(defun org-scribe--relationships-property-name ()
  "Return the property name new relationship data is written under.
English in every project, like the other project-metadata properties:
it is a machine-facing identifier, not something the writer reads."
  (org-scribe-lang-property 'relationships 'en))

(defun org-scribe--relationships-property-at-point ()
  "Return the spelling of the relationships property on this heading, or nil.
Every registered pack's spelling is checked, so data written under an
older localized name is still found."
  (seq-find (lambda (name) (org-entry-get nil name))
            (org-scribe-lang-all :properties 'relationships)))

(defun org-scribe--get-character-relationships ()
  "Get relationships for character at point.
Returns list of (ID NAME TYPE) tuples."
  (when-let* ((property (org-scribe--relationships-property-at-point)))
    (org-scribe--parse-relationships (org-entry-get nil property))))

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
  (when-let* ((marker (org-scribe--find-character-by-name char-name)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (let* ((rel-string (if relationships
                             (org-scribe--relationships-to-string relationships)
                           ""))
              (property-name (or (org-scribe--relationships-property-at-point)
                                 (org-scribe--relationships-property-name))))
         (if (string-empty-p rel-string)
             (org-entry-delete nil property-name)
           (org-entry-put nil property-name rel-string))
         (save-buffer))))
    t))

(defun org-scribe--get-character-relationships-by-name (char-name)
  "Get relationships for CHAR-NAME.
Returns list of (ID NAME TYPE) tuples."
  (when-let* ((marker (org-scribe--find-character-by-name char-name)))
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

(defun org-scribe--relationship-character-at-point (all-chars)
  "Return the name of the character point is on, or nil.
ALL-CHARS is the alist from `org-scribe--get-all-characters'.  Walking up
from point, the first heading that is a character's own entry (its ID is
one of ALL-CHARS), or that carries a PoV naming a known character, wins:
so a character's profile at any depth, or a scene, gives a character."
  (when (and (derived-mode-p 'org-mode) (not (org-before-first-heading-p)))
    (let ((by-id (lambda (id)
                   (car (seq-find (lambda (c) (equal (cadr c) id)) all-chars))))
          (by-pov (lambda (pov)
                    (if (string-match "\\[\\[id:\\([^]]+\\)\\]" pov)
                        (car (seq-find (lambda (c) (equal (cadr c) (match-string 1 pov)))
                                       all-chars))
                      (car (assoc (string-trim pov) all-chars))))))
      (save-excursion
        (org-back-to-heading t)
        (catch 'found
          (while t
            (let ((id (org-entry-get nil "ID"))
                  (pov (org-scribe-scene-property-get 'pov)))
              (when-let* ((name (or (and id (funcall by-id id))
                                    (and pov (funcall by-pov pov)))))
                (throw 'found name)))
            (unless (org-up-heading-safe)
              (throw 'found nil))))))))

(defun org-scribe--read-relationship-character (char-names all-chars)
  "Read a character name from CHAR-NAMES, defaulting to the one at point.
ALL-CHARS is the alist from `org-scribe--get-all-characters'.  The
prompt used to start empty even with point on the character's heading."
  (let ((default (org-scribe--relationship-character-at-point all-chars)))
    (completing-read (format-prompt (org-scribe-msg 'prompt-relationship-from-character)
                                    default)
                     char-names nil t nil nil default)))

(defun org-scribe--with-source-character (min-chars action-fn)
  "Select a source character and call ACTION-FN with the selection.
ACTION-FN is called with (ALL-CHARS SOURCE-NAME EXISTING-RELS).
If fewer than MIN-CHARS characters exist, displays a message and returns nil."
  (let* ((all-chars (org-scribe--get-all-characters))
         (char-names (mapcar #'car all-chars)))
    (if (< (length char-names) min-chars)
        (message "%s" (org-scribe-msg (if (zerop (length char-names))
                                     'error-no-characters-found
                                   'msg-no-other-characters)))
      (let* ((source-name (org-scribe--read-relationship-character char-names all-chars))
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
           (message "%s" (org-scribe-msg 'msg-no-other-characters))
         ;; Ask for target character
         (let* ((target-name (completing-read (org-scribe-msg 'prompt-relationship-to-character)
                                              other-char-names nil t))
                (target-entry (assoc target-name all-chars))
                (target-id (cadr target-entry))
                ;; Ask for relationship type
                (rel-type (org-scribe--read-relationship-type))
                ;; Create new relationship tuple
                (new-rel (list target-id target-name rel-type))
                ;; Add to existing relationships
                (all-rels (append existing-rels (list new-rel))))
           ;; Update the source character's relationships
           (org-scribe--update-character-relationships source-name all-rels)
           (message "%s" (org-scribe-msg 'msg-added-relationship
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
         (message "%s" (org-scribe-msg 'msg-no-relationships))
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
         (message "%s" (org-scribe-msg 'msg-removed-relationship source-name selected)))))))

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
        (message "%s" (org-scribe-msg 'error-no-characters-found))
      ;; Ask for which character to view
      (let* ((char-name (org-scribe--read-relationship-character char-names all-chars))
             (relationships (org-scribe--get-character-relationships-by-name char-name)))

        (if (null relationships)
            (message "%s" (org-scribe-msg 'error-no-relationships char-name))
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
        (message "%s" (org-scribe-msg 'msg-no-relationships-in-project))
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

;;; Keeping stored names current

(defun org-scribe-refresh-relationship-names ()
  "Rewrite the target names stored in relationships to the current names.
A relationship stores its target as an ID link, so renaming a character
kept it working but left the old name in the relationships list and
table.  Called by `org-scribe-relink-project'.  Returns the number of
characters whose relationships changed."
  (let ((char-file (org-scribe--get-character-file))
        (count 0))
    (when (and char-file (file-exists-p char-file))
      (let ((names (make-hash-table :test 'equal)))
        (dolist (entry (org-scribe--get-all-characters))
          (puthash (cadr entry) (car entry) names))
        (with-current-buffer (find-file-noselect char-file)
          (org-with-wide-buffer
           (org-map-entries
            (lambda ()
              (when-let* ((property (org-scribe--relationships-property-at-point))
                          (rels (org-scribe--get-character-relationships)))
                (let ((renamed (mapcar (lambda (rel)
                                         (list (nth 0 rel)
                                               (gethash (nth 0 rel) names (nth 1 rel))
                                               (nth 2 rel)))
                                       rels)))
                  (unless (equal rels renamed)
                    (org-entry-put nil property
                                   (org-scribe--relationships-to-string renamed))
                    (setq count (1+ count))))))
            nil 'file))
          (when (buffer-modified-p) (save-buffer)))))
    count))

;;; Setup Function

;;;###autoload
(defun org-scribe-setup-character-relationships ()
  "Initialize character relationship system for current project.
Ensures all characters have IDs and adds empty RelationshipsData properties."
  (interactive)
  (org-scribe--require-project)
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
                        (not (org-scribe--relationships-property-at-point)))
                (org-entry-put nil (org-scribe--relationships-property-name) "")
                (setq count (1+ count))))
            nil 'file)
           (when (> count 0)
             (save-buffer))
           (message "%s" (org-scribe-msg 'msg-relationship-setup-complete
                                   count
                                   (org-scribe-plural count "")))))))))

(provide 'org-scribe-character-relationships)

;;; org-scribe-character-relationships.el ends here
