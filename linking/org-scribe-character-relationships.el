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
  '("mentor" "mentee" "opponent" "rival" "friend" "family" "lover" "enemy" "ally"
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
RELATIONSHIPS is a list of (ID NAME TYPE STRENGTH SENTIMENT) tuples."
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
Returns list of (ID NAME TYPE STRENGTH SENTIMENT) tuples."
  (when-let ((marker (org-scribe--find-character-by-name char-name)))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (org-scribe--get-character-relationships)))))

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
  "Add a relationship between two characters.
Prompts for source character, target character, relationship type, strength, and sentiment."
  (interactive)
  (let* ((all-chars (org-scribe--get-all-characters))
         (char-names (mapcar #'car all-chars)))

    (if (< (length char-names) 2)
        (message (org-scribe-msg 'msg-no-other-characters))
      ;; Ask for source character
      (let* ((source-name (completing-read (org-scribe-msg 'prompt-relationship-from-character)
                                          char-names nil t))
             ;; Get existing relationships for source character
             (existing-rels (org-scribe--get-character-relationships-by-name source-name))
             ;; Filter out source character from target selection
             (other-chars (seq-remove (lambda (c) (string= (car c) source-name))
                                     all-chars))
             (other-char-names (mapcar #'car other-chars)))

        (if (null other-char-names)
            (message (org-scribe-msg 'msg-no-other-characters))
          ;; Ask for target character
          (let* ((target-name (completing-read (org-scribe-msg 'prompt-relationship-to-character)
                                              other-char-names nil t))
                 (target-entry (assoc target-name all-chars))
                 (target-id (cadr target-entry))
                 ;; Ask for relationship details
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
                 (all-rels (append existing-rels (list new-rel))))

            ;; Update the source character's relationships
            (org-scribe--update-character-relationships source-name all-rels)
            (message (org-scribe-msg 'msg-added-relationship
                                    rel-type source-name target-name rel-strength rel-sentiment))))))))

;;;###autoload
(defun org-scribe/remove-relationship ()
  "Remove a relationship from a character.
Prompts for source character, then which relationship to remove."
  (interactive)
  (let* ((all-chars (org-scribe--get-all-characters))
         (char-names (mapcar #'car all-chars)))

    (if (null char-names)
        (message (org-scribe-msg 'error-no-characters-found))
      ;; Ask for source character
      (let* ((source-name (completing-read (org-scribe-msg 'prompt-relationship-from-character)
                                          char-names nil t))
             ;; Get existing relationships for source character
             (existing-rels (org-scribe--get-character-relationships-by-name source-name)))

        (if (null existing-rels)
            (message (org-scribe-msg 'msg-no-relationships))
          ;; Ask which relationship to remove
          (let* ((rel-choices (mapcar (lambda (rel)
                                       (format "%s (%s, %d, %s)"
                                              (nth 1 rel)  ; Name
                                              (nth 2 rel)  ; Type
                                              (nth 3 rel)  ; Strength
                                              (nth 4 rel))) ; Sentiment
                                     existing-rels))
                 (selected (completing-read (org-scribe-msg 'prompt-remove-relationship)
                                           rel-choices nil t))
                 (selected-idx (cl-position selected rel-choices :test #'string=))
                 (remaining-rels (append (cl-subseq existing-rels 0 selected-idx)
                                       (cl-subseq existing-rels (1+ selected-idx)))))

            ;; Update the source character's relationships
            (org-scribe--update-character-relationships source-name remaining-rels)
            (message (org-scribe-msg 'msg-removed-relationship source-name selected))))))))

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
            (display-buffer buf-name)))))))

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

;;; DOT Graph Visualization

(defun org-scribe--filter-relationships-by-strength (all-relationships min-strength)
  "Filter ALL-RELATIONSHIPS to only include those with strength >= MIN-STRENGTH.
ALL-RELATIONSHIPS is alist of (CHAR-NAME . RELATIONSHIPS).
Returns filtered alist in same format."
  (let (result)
    (dolist (entry all-relationships)
      (let* ((char-name (car entry))
             (rels (cdr entry))
             (filtered-rels (seq-filter (lambda (rel)
                                         (>= (nth 3 rel) min-strength))
                                       rels)))
        (when filtered-rels
          (push (cons char-name filtered-rels) result))))
    (nreverse result)))

(defun org-scribe--build-relationship-graph-data (&optional filter-strength)
  "Build graph data structure from all character relationships.
FILTER-STRENGTH if non-nil filters to relationships with strength >= value.
Returns alist of (CHAR-NAME . RELATIONSHIPS)."
  (let ((all-rels (org-scribe--get-all-relationships)))
    (if filter-strength
        (org-scribe--filter-relationships-by-strength all-rels filter-strength)
      all-rels)))

(defun org-scribe--sentiment-to-color (sentiment)
  "Convert SENTIMENT to a DOT graph color.
positive=green, negative=red, complex=purple, neutral=gray."
  (pcase sentiment
    ("positive" "forestgreen")
    ("negative" "crimson")
    ("complex" "purple")
    (_ "gray50")))

(defun org-scribe--generate-dot-code (graph-data &optional title)
  "Generate Graphviz DOT code from GRAPH-DATA.
GRAPH-DATA is alist of (CHAR-NAME . RELATIONSHIPS).
TITLE is optional graph title."
  (let ((dot-lines (list "  edge [fontsize=10];"
                        "  node [shape=box, style=filled, fillcolor=lightblue];"
                        "  rankdir=LR;"
                        "digraph character_relationships {"))
        (all-chars (make-hash-table :test 'equal)))

    ;; Add title if provided
    (when title
      (push (format "  labelloc=\"t\";\n  label=\"%s\";" title) dot-lines))

    ;; Collect all unique characters
    (dolist (entry graph-data)
      (let ((char-name (car entry))
            (rels (cdr entry)))
        (puthash char-name t all-chars)
        (dolist (rel rels)
          (puthash (nth 1 rel) t all-chars))))

    ;; Add node declarations
    (push "" dot-lines)
    (push "  // Character nodes" dot-lines)
    (maphash (lambda (char _)
               (push (format "  \"%s\";" char) dot-lines))
             all-chars)

    ;; Add edges
    (push "" dot-lines)
    (push "  // Relationships" dot-lines)
    (dolist (entry graph-data)
      (let ((source-char (car entry))
            (rels (cdr entry)))
        (dolist (rel rels)
          (let* ((target-char (nth 1 rel))
                 (rel-type (nth 2 rel))
                 (strength (nth 3 rel))
                 (sentiment (nth 4 rel))
                 (color (org-scribe--sentiment-to-color sentiment))
                 (penwidth (format "%.1f" (+ 0.5 (* strength 0.5)))))
            (push (format "  \"%s\" -> \"%s\" [label=\"%s (%d)\", color=\"%s\", penwidth=%s];"
                         source-char target-char rel-type strength color penwidth)
                  dot-lines)))))

    ;; Close graph
    (push "}" dot-lines)

    ;; Return as string
    (string-join (nreverse dot-lines) "\n")))

(defun org-scribe--generate-dot-code-for-character (char-name &optional filter-strength)
  "Generate DOT code showing CHAR-NAME's ego network.
Shows only relationships directly connected to CHAR-NAME.
FILTER-STRENGTH if non-nil filters to relationships with strength >= value."
  (let* ((all-rels (org-scribe--get-all-relationships))
         (filtered-rels (if filter-strength
                           (org-scribe--filter-relationships-by-strength all-rels filter-strength)
                         all-rels))
         ;; Get outgoing relationships from this character
         (char-rels-entry (assoc char-name filtered-rels))
         (outgoing-rels (when char-rels-entry (cdr char-rels-entry)))
         ;; Get incoming relationships to this character
         (incoming-rels nil))

    ;; Find all incoming relationships
    (dolist (entry filtered-rels)
      (let ((source (car entry))
            (rels (cdr entry)))
        (dolist (rel rels)
          (when (string= (nth 1 rel) char-name)
            (push (list source (nth 2 rel) (nth 3 rel) (nth 4 rel)) incoming-rels)))))

    ;; Build graph data with just this character's network
    (let ((graph-data (list (cons char-name outgoing-rels))))
      ;; Add entries for characters that have relationships TO our character
      (dolist (incoming incoming-rels)
        (let* ((source-char (nth 0 incoming))
               (rel-type (nth 1 incoming))
               (strength (nth 2 incoming))
               (sentiment (nth 3 incoming))
               (source-entry (assoc source-char graph-data))
               ;; Create relationship tuple for source -> char-name
               (rel-tuple (list nil char-name rel-type strength sentiment)))
          (if source-entry
              ;; Add to existing entry
              (setcdr source-entry (append (cdr source-entry) (list rel-tuple)))
            ;; Create new entry
            (push (cons source-char (list rel-tuple)) graph-data))))

      (org-scribe--generate-dot-code graph-data
                                     (format "%s's Relationship Network" char-name)))))

(defun org-scribe--render-dot-to-image (dot-code output-format)
  "Render DOT-CODE to image using Graphviz.
OUTPUT-FORMAT can be 'png, 'svg, 'pdf, etc.
Returns the path to the generated image file, or nil if rendering failed."
  (let* ((dot-executable (executable-find "dot"))
         (temp-dot (make-temp-file "org-scribe-graph" nil ".dot"))
         (temp-output (concat (file-name-sans-extension temp-dot)
                             "." (symbol-name output-format))))

    (if (not dot-executable)
        (progn
          (message (org-scribe-msg 'error-graphviz-not-found))
          nil)
      ;; Write DOT code to temp file
      (with-temp-file temp-dot
        (insert dot-code))

      ;; Render to image
      (let ((exit-code (call-process dot-executable nil nil nil
                                    (format "-T%s" (symbol-name output-format))
                                    temp-dot
                                    "-o" temp-output)))
        (if (= exit-code 0)
            temp-output
          (message (org-scribe-msg 'error-graph-render-failed exit-code))
          nil)))))

;;; Dynamic Block

;;;###autoload
(defun org-dblock-write:character-relationships (params)
  "Generate character relationship visualization as a dynamic block.
PARAMS can include:
  :format - Output format: 'dot (default), 'ascii, 'table
  :filter-strength - Minimum relationship strength (1-5)
  :character - Show only this character's ego network
  :image-format - Image format when :format is 'dot: 'png (default), 'svg, 'pdf
  :image-file - Save image to this file instead of temp file"
  (let* ((format (or (plist-get params :format) 'dot))
         (filter-strength (plist-get params :filter-strength))
         (character (plist-get params :character))
         (image-format (or (plist-get params :image-format) 'png))
         (image-file (plist-get params :image-file)))

    (cond
     ;; DOT graph format
     ((eq format 'dot)
      (let* ((dot-code (if character
                          (org-scribe--generate-dot-code-for-character character filter-strength)
                        (org-scribe--generate-dot-code
                         (org-scribe--build-relationship-graph-data filter-strength))))
             ;; Determine output filename
             (output-file (or image-file
                             (format "character-relationships.%s" (symbol-name image-format))))
             ;; Try to render the image
             (image-path (org-scribe--render-dot-to-image dot-code image-format)))

        ;; Always include :file parameter for org-babel export
        (insert "#+BEGIN_SRC dot :file " output-file " :exports results\n")
        (insert dot-code)
        (insert "\n#+END_SRC\n\n")

        ;; If we successfully rendered, copy to final location and show image
        (when image-path
          (when image-file
            (copy-file image-path image-file t))
          (insert "[[file:" output-file "]]\n"))))

     ;; ASCII tree format
     ((eq format 'ascii)
      (let ((all-rels (org-scribe--build-relationship-graph-data filter-strength)))
        (if character
            ;; Show single character
            (let ((char-rels (cdr (assoc character all-rels))))
              (if char-rels
                  (insert (org-scribe--ascii-relationship-tree character char-rels))
                (insert (format "No relationships found for %s\n" character))))
          ;; Show all characters
          (insert "#+BEGIN_EXAMPLE\n")
          (dolist (entry all-rels)
            (let ((char-name (car entry))
                  (rels (cdr entry)))
              (insert (org-scribe--ascii-relationship-tree char-name rels))
              (insert "\n\n")))
          (insert "#+END_EXAMPLE\n"))))

     ;; Table format
     ((eq format 'table)
      (let* ((all-rels (org-scribe--build-relationship-graph-data filter-strength))
             (filtered-rels (if character
                               (list (assoc character all-rels))
                             all-rels)))
        (insert (org-scribe--format-relationship-table filtered-rels))
        (insert "\n")))

     ;; Unknown format
     (t
      (insert (org-scribe-msg 'msg-graph-format-unknown format))
      (insert "\n")))))

;;;###autoload
(defun org-scribe/insert-relationship-block ()
  "Insert a character-relationships dynamic block at point."
  (interactive)
  (let* ((format (completing-read (org-scribe-msg 'prompt-graph-format)
                                 '("dot" "ascii" "table") nil t "dot"))
         (character (completing-read (org-scribe-msg 'prompt-graph-character)
                                    (cons "" (mapcar #'car (org-scribe--get-all-characters)))
                                    nil t))
         (strength (completing-read (org-scribe-msg 'prompt-graph-min-strength)
                                   '("" "1" "2" "3" "4" "5")
                                   nil t)))
    (insert "#+BEGIN: character-relationships")
    (insert " :format " format)
    (when (not (string-empty-p character))
      (insert " :character \"" character "\""))
    (when (not (string-empty-p strength))
      (insert " :filter-strength " strength))
    (insert "\n#+END:\n")
    (forward-line -1)
    (org-update-dblock)
    (message (org-scribe-msg 'msg-graph-inserted))))

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
