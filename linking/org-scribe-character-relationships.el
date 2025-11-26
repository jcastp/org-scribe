;;; org-scribe-character-relationships.el --- Character relationship system for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides structured relationship tracking between characters.
;; Relationships are stored in the :RelationshipsData: property in format:
;;   [[id:char-id][Name]](type,strength,sentiment); ...
;;
;; Where:
;;   - type: relationship type (mentor, rival, friend, family, lover, enemy, ally, etc.)
;;   - strength: 1-5 (1=acquaintance, 5=central to their life)
;;   - sentiment: positive, negative, neutral, complex
;;
;; Features:
;; - Parse and manage structured relationship data
;; - Interactive relationship linking with completion
;; - ASCII visualization of character relationships
;; - Relationship strength and type tracking
;;
;; Usage:
;;   M-x org-scribe/add-relationship     - Add a relationship to current character
;;   M-x org-scribe/remove-relationship  - Remove a relationship
;;   M-x org-scribe/show-character-relationships - Display relationship tree

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-scribe-core)
(require 'org-scribe-character-links)
(require 'org-scribe-messages)

;;; Relationship Type Definitions

(defvar org-scribe-relationship-types
  '("mentor" "mentee" "rival" "friend" "family" "lover" "enemy" "ally"
    "partner" "colleague" "acquaintance" "subordinate" "superior"
    "student" "teacher" "sibling" "parent" "child" "spouse")
  "Common relationship types for character relationships.")

(defvar org-scribe-relationship-sentiments
  '("positive" "negative" "neutral" "complex")
  "Possible relationship sentiments.")

;;; Parsing Functions

(defun org-scribe--parse-single-relationship (rel-string)
  "Parse a single relationship string into components.
REL-STRING should be in format: [[id:...][Name]](type,strength,sentiment)
Returns (ID NAME TYPE STRENGTH SENTIMENT) or nil if parsing fails."
  (when (and rel-string (string-match
                         "\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\](\\([^,]+\\),\\([0-9]+\\),\\([^)]+\\))"
                         rel-string))
    (list (match-string 1 rel-string)     ; ID
          (match-string 2 rel-string)     ; Name
          (match-string 3 rel-string)     ; Type
          (string-to-number (match-string 4 rel-string)) ; Strength
          (match-string 5 rel-string))))  ; Sentiment

(defun org-scribe--parse-relationships (rel-property-value)
  "Parse relationship property value into list of relationships.
REL-PROPERTY-VALUE is the value of :RelationshipsData: property.
Returns list of (ID NAME TYPE STRENGTH SENTIMENT) tuples."
  (when (and rel-property-value (not (string-empty-p rel-property-value)))
    (let ((parts (split-string rel-property-value ";" t " "))
          result)
      (dolist (part parts)
        (when-let ((parsed (org-scribe--parse-single-relationship part)))
          (push parsed result)))
      (nreverse result))))

(defun org-scribe--format-relationship (id name type strength sentiment)
  "Format a relationship into the standard string format.
Returns: [[id:ID][NAME]](TYPE,STRENGTH,SENTIMENT)"
  (format "[[id:%s][%s]](%s,%d,%s)"
          id name type strength sentiment))

(defun org-scribe--relationships-to-string (relationships)
  "Convert list of relationship tuples to property string.
RELATIONSHIPS is a list of (ID NAME TYPE STRENGTH SENTIMENT) tuples.
Returns a string suitable for :RelationshipsData: property."
  (mapconcat
   (lambda (rel)
     (apply #'org-scribe--format-relationship rel))
   relationships
   "; "))

;;; Character Relationship Database

(defun org-scribe--get-character-relationships ()
  "Get relationships for character at point.
Returns list of (ID NAME TYPE STRENGTH SENTIMENT) tuples."
  (let ((rel-data (org-entry-get nil "RelationshipsData"))
        (datos-rel (org-entry-get nil "DatosRelaciones"))) ; Spanish property name
    (org-scribe--parse-relationships (or rel-data datos-rel))))

(defun org-scribe--get-all-relationships ()
  "Return alist of all character relationships in the project.
Format: ((CHAR-NAME . RELATIONSHIPS) ...) where RELATIONSHIPS is
list of (ID NAME TYPE STRENGTH SENTIMENT) tuples."
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

;;;###autoload
(defun org-scribe/add-relationship ()
  "Add a relationship to the character at point.
Prompts for target character, relationship type, strength, and sentiment."
  (interactive)
  (unless (org-before-first-heading-p)
    (let* ((all-chars (org-scribe--get-all-characters))
           (current-char (org-scribe--get-character-name-at-point))
           (existing-rels (org-scribe--get-character-relationships))
           ;; Filter out current character from selection
           (other-chars (seq-remove (lambda (c) (string= (car c) current-char))
                                   all-chars))
           (char-names (mapcar #'car other-chars)))

      (if (null char-names)
          (message (org-scribe-msg 'msg-no-other-characters))
        (let* ((target-name (completing-read (org-scribe-msg 'prompt-relationship-character) char-names nil t))
               (target-entry (assoc target-name other-chars))
               (target-id (cadr target-entry))
               (rel-type (completing-read (org-scribe-msg 'prompt-relationship-type)
                                         org-scribe-relationship-types
                                         nil nil))
               (rel-strength (string-to-number
                            (completing-read (org-scribe-msg 'prompt-relationship-strength)
                                           '("1" "2" "3" "4" "5")
                                           nil t)))
               (rel-sentiment (completing-read (org-scribe-msg 'prompt-relationship-sentiment)
                                             org-scribe-relationship-sentiments
                                             nil t))
               ;; Create new relationship tuple
               (new-rel (list target-id target-name rel-type rel-strength rel-sentiment))
               ;; Add to existing relationships
               (all-rels (append existing-rels (list new-rel)))
               ;; Convert to string
               (rel-string (org-scribe--relationships-to-string all-rels))
               ;; Determine which property name to use
               (property-name (if (org-entry-get nil "DatosRelaciones")
                                "DatosRelaciones"
                              "RelationshipsData")))

          ;; Update the property
          (org-entry-put nil property-name rel-string)
          (message (org-scribe-msg 'msg-added-relationship
                                  rel-type target-name rel-strength rel-sentiment)))))))

;;;###autoload
(defun org-scribe/remove-relationship ()
  "Remove a relationship from the character at point.
Prompts for which relationship to remove."
  (interactive)
  (unless (org-before-first-heading-p)
    (let* ((existing-rels (org-scribe--get-character-relationships))
           (property-name (if (org-entry-get nil "DatosRelaciones")
                            "DatosRelaciones"
                          "RelationshipsData")))

      (if (null existing-rels)
          (message (org-scribe-msg 'msg-no-relationships))
        (let* ((rel-choices (mapcar (lambda (rel)
                                     (format "%s (%s, %d, %s)"
                                            (nth 1 rel)  ; Name
                                            (nth 2 rel)  ; Type
                                            (nth 3 rel)  ; Strength
                                            (nth 4 rel))) ; Sentiment
                                   existing-rels))
               (selected (completing-read (org-scribe-msg 'prompt-remove-relationship) rel-choices nil t))
               (selected-idx (cl-position selected rel-choices :test #'string=))
               (remaining-rels (append (cl-subseq existing-rels 0 selected-idx)
                                     (cl-subseq existing-rels (1+ selected-idx)))))

          (if (null remaining-rels)
              (org-entry-delete nil property-name)
            (org-entry-put nil property-name
                          (org-scribe--relationships-to-string remaining-rels)))

          (message (org-scribe-msg 'msg-removed-relationship selected)))))))

;;; ASCII Visualization

(defun org-scribe--ascii-relationship-tree (char-name relationships)
  "Generate ASCII tree of CHAR-NAME's RELATIONSHIPS.
RELATIONSHIPS is list of (ID NAME TYPE STRENGTH SENTIMENT) tuples."
  (let ((lines (list char-name)))
    (dotimes (i (length relationships))
      (let* ((rel (nth i relationships))
             (name (nth 1 rel))
             (type (nth 2 rel))
             (strength (nth 3 rel))
             (sentiment (nth 4 rel))
             (is-last (= i (1- (length relationships))))
             (prefix (if is-last "└─" "├─"))
             (sentiment-symbol (pcase sentiment
                                ("positive" "+")
                                ("negative" "-")
                                ("complex" "~")
                                (_ "·")))
             (strength-bars (make-string strength ?■)))
        (push (format "  %s(%s,%s)─> %s"
                     prefix
                     type
                     sentiment-symbol
                     name)
              lines)))
    (string-join (nreverse lines) "\n")))

(defun org-scribe--format-relationship-table (all-relationships)
  "Format ALL-RELATIONSHIPS as an org-mode table.
ALL-RELATIONSHIPS is alist of (CHAR-NAME . RELATIONSHIPS)."
  (let ((lines (list "| Character | Related To | Type | Strength | Sentiment |"
                    "|-----------+------------+------+----------+-----------|")))
    (dolist (entry all-relationships)
      (let ((char-name (car entry))
            (rels (cdr entry)))
        (dolist (rel rels)
          (push (format "| %s | %s | %s | %d | %s |"
                       char-name
                       (nth 1 rel)  ; Name
                       (nth 2 rel)  ; Type
                       (nth 3 rel)  ; Strength
                       (nth 4 rel)) ; Sentiment
                lines))))
    (string-join (nreverse lines) "\n")))

;;;###autoload
(defun org-scribe/show-character-relationships ()
  "Display relationships for character at point in a temporary buffer."
  (interactive)
  (unless (org-before-first-heading-p)
    (let* ((char-name (org-scribe--get-character-name-at-point))
           (relationships (org-scribe--get-character-relationships)))

      (if (null relationships)
          (message (org-scribe-msg 'error-no-relationships char-name))
        (let ((tree (org-scribe--ascii-relationship-tree char-name relationships))
              (buf-name (format "*Relationships: %s*" char-name)))
          (with-current-buffer (get-buffer-create buf-name)
            (erase-buffer)
            (insert (format "Relationships for %s\n\n" char-name))
            (insert tree)
            (insert "\n\n")
            (insert "Legend:\n")
            (insert "  + = positive relationship\n")
            (insert "  - = negative relationship\n")
            (insert "  ~ = complex relationship\n")
            (insert "  · = neutral relationship\n")
            (goto-char (point-min))
            (view-mode 1))
          (display-buffer buf-name))))))

;;;###autoload
(defun org-scribe/show-all-relationships ()
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
          (goto-char (point-min))
          (org-table-align)
          (view-mode 1))
        (display-buffer buf-name)))))

;;; Setup Function

;;;###autoload
(defun org-scribe/setup-character-relationships ()
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
