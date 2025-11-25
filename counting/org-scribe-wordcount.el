;;; org-scribe-wordcount.el --- Word counting functions for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Word counting functions for creative writing.
;; Integrates with org-context-extended to exclude comments, properties, and drawers.
;; Provides dynamic blocks for word count tables.

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-scribe-config)
(require 'org-scribe-messages)

;; Declare external functions to avoid compiler warnings
(declare-function org-context-count-words "org-context-extended")

;;; Word Count Property Function

;;;###autoload
(defun org-scribe/ews-org-count-words ()
  "Add word count to each heading property drawer in an Org mode buffer.
Uses org-context-count-words for accurate counting that excludes
comments, properties, drawers, etc.  Also creates a custom ID
for each heading to enable linking."
  (interactive)
  (unless (featurep 'org-context-extended)
    (user-error (org-scribe-msg 'error-org-context-required)))
  (org-map-entries
   (lambda ()
     (let* ((start (point))
            ;; Pass t (INVISIBLE-OK) to org-end-of-subtree to correctly find
            ;; the end of THIS subtree (not the end of the document)
            (end (save-excursion (org-end-of-subtree t)))
            (word-count (org-context-count-words start end t t t t t t
                                                  org-scribe-wordcount-default-ignore-tags)))
       (org-set-property "WORDCOUNT" (number-to-string word-count)))
     ;; Create the id to link the org heading
     (org-id-get-create))))

;;; Dynamic Block for Word Count Table

;;;###autoload
(defun org-dblock-write:org-generate-wordcount-table (params)
  "Generate a table with heading names, word counts, objectives, and progress.
This is a dynamic block function that can be inserted with:
  #+BEGIN: org-generate-wordcount-table
  #+END:

The table shows:
- Hierarchical heading structure (with - -- --- prefixes)
- Wordcount (from WORDCOUNT property)
- Objective (from WORD-OBJECTIVE property, optional)
- Progress (auto-calculated percentage when objective is set)

Sections tagged with 'noexport' are excluded."
  (interactive)
  ;; Execute the function of counting words, add the WORDCOUNT property,
  ;; and update the current word count (if org-context-extended is available).
  (when (featurep 'org-context-extended)
    (org-scribe/ews-org-count-words))
  (let (table-data)
    ;; Traverse each Org entry in the current buffer
    (org-map-entries
     (lambda ()
       (let* ((heading-components (org-heading-components))
              (heading (nth 4 heading-components))  ; Extract heading text
              (level (car heading-components))      ; Extract level
              (tags (org-get-tags))
              (wordcount-str (org-entry-get nil "WORDCOUNT"))
              (objective-str (org-entry-get nil "WORD-OBJECTIVE")))
         ;; Check if we get tags, and if they don't contain the noexport
         (unless (and tags (member "noexport" tags))
           ;; Convert properties to numbers
           (let* ((wordcount (if wordcount-str
                                (string-to-number wordcount-str)
                                0))
                  (objective (if objective-str
                                (string-to-number objective-str)
                                nil))  ; nil means no objective set
                  ;; Calculate progress (safe division)
                  (progress (when (and objective (> objective 0))
                             (* 100.0 (/ (float wordcount) objective)))))

             ;; Add the data in the table-data for each heading
             ;; Format with literal asterisks and org link
             (push (list
                    (format "%s %s"
                            (make-string level 45)  ; 45 is the minus sign in ASCII
                            (org-link-make-string heading))
                    wordcount
                    objective
                    progress)
                   table-data))))))

    ;; Insert table header
    (insert "| Heading         | Wordcount | Objective | Progress |\n"
            "|-----------------+-----------+-----------+----------|\n")

    ;; Insert table rows (reversed to maintain document order)
    (dolist (row (reverse table-data))
      (let ((heading-str (nth 0 row))
            (wordcount (nth 1 row))
            (objective (nth 2 row))
            (progress (nth 3 row)))
        (insert (format "| %s | %9d | %9s | %8s |\n"
                       heading-str
                       wordcount
                       (if objective
                           (format "%d" objective)
                         "")  ; Empty if no objective
                       (if progress
                           (format "%5.0f%%" progress)
                         "")))))  ; Empty if no progress

    ;; Align table once at the end for better performance
    (org-table-align)))

;;; Dynamic Block for Progress Table

;;;###autoload
(defun org-dblock-write:progress-by-act (params)
  "Generate a progress table from WORD-OBJECTIVE and WORDCOUNT properties.
This dynamic block extracts data from headings and auto-calculates progress.

PARAMS is a plist that can contain:
  :match    - Org match expression (default: \"LEVEL=1+ignore\")
              Examples: \"LEVEL=1+ignore\" for Acts
                       \"LEVEL=2+ignore\" for Chapters
                       \"LEVEL=3+ignore\" for Scenes
  :title    - Label for the Section column (default: \"Section\")

Usage:
  #+BEGIN: progress-by-act
  #+END:

  #+BEGIN: progress-by-act :match \"LEVEL=2/ignore\" :title \"Chapter\"
  #+END:

The table shows:
- Section name (extracted from heading)
- Target (from WORD-OBJECTIVE property)
- Actual (from WORDCOUNT property)
- Progress (auto-calculated percentage)
- TOTAL row with sums and overall progress

Handles missing properties safely:
- Missing WORD-OBJECTIVE defaults to 0
- Missing WORDCOUNT defaults to 0
- Division by zero returns N/A"
  (interactive)

  ;; Update word counts first
  (when (featurep 'org-context-extended)
    (org-scribe/ews-org-count-words))

  (let* ((match (or (plist-get params :match) "LEVEL=1+ignore"))
         (title (or (plist-get params :title) "Section"))
         (total-objective 0)
         (total-actual 0)
         table-data)

    ;; Collect data from matching headings
    (org-map-entries
     (lambda ()
       (let* ((heading (org-get-heading t t t t))
              (objective-str (org-entry-get nil "WORD-OBJECTIVE"))
              (wordcount-str (org-entry-get nil "WORDCOUNT")))

         ;; Only process entries that have WORD-OBJECTIVE property
         (when objective-str
           (let* (;; Convert to numbers
                  (objective (string-to-number objective-str))
                  (wordcount (if wordcount-str
                                (string-to-number wordcount-str)
                                0)))

             ;; Calculate progress percentage (safe division)
             (let ((progress (if (> objective 0)
                               (* 100.0 (/ (float wordcount) objective))
                               nil)))  ; nil indicates N/A

               ;; Accumulate totals
               (setq total-objective (+ total-objective objective))
               (setq total-actual (+ total-actual wordcount))

               ;; Store row data
               (push (list heading objective wordcount progress)
                     table-data))))))
     match)

    ;; Calculate total progress (safe division)
    (let ((total-progress (if (> total-objective 0)
                             (* 100.0 (/ (float total-actual) total-objective))
                             nil)))

      ;; Insert table header
      (insert (format "| %s | Target | Actual | Progress |\n" title))
      (insert "|---------+--------+--------+----------|\n")

      ;; Insert data rows (reversed to maintain document order)
      (dolist (row (reverse table-data))
        (let ((section (nth 0 row))
              (target (nth 1 row))
              (actual (nth 2 row))
              (progress (nth 3 row)))
          (insert (format "| %s | %6d | %6d | %8s |\n"
                         section
                         target
                         actual
                         (if progress
                             (format "%5.0f%%" progress)
                           "N/A")))))

      ;; Insert total row
      (insert "|---------+--------+--------+----------|\n")
      (insert (format "| TOTAL   | %6d | %6d | %8s |\n"
                     total-objective
                     total-actual
                     (if total-progress
                         (format "%5.0f%%" total-progress)
                       "N/A"))))

    ;; Align table
    (org-table-align)))

(provide 'org-scribe-wordcount)

;;; org-scribe-wordcount.el ends here
