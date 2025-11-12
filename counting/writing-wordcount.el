;;; writing-wordcount.el --- Word counting functions for emacs-writing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Word counting functions for creative writing.
;; Integrates with org-context-extended to exclude comments, properties, and drawers.
;; Provides dynamic blocks for word count tables.

;;; Code:

(require 'org)
(require 'org-element)
(require 'writing-config)

;; Declare external functions to avoid compiler warnings
(declare-function org-context-count-words "org-context-extended")

;;; Word Count Property Function

;;;###autoload
(defun writing/ews-org-count-words ()
  "Add word count to each heading property drawer in an Org mode buffer.
Uses org-context-count-words for accurate counting that excludes
comments, properties, drawers, etc.  Also creates a custom ID
for each heading to enable linking."
  (interactive)
  (unless (featurep 'org-context-extended)
    (user-error "org-context-extended package is required for accurate word counting"))
  (org-map-entries
   (lambda ()
     (let* ((start (point))
            (end (save-excursion (org-end-of-subtree)))
            (word-count (org-context-count-words start end t t t t t t
                                                  writing-wordcount-default-ignore-tags)))
       (org-set-property "WORDCOUNT" (number-to-string word-count)))
     ;; Create the id to link the org heading
     (org-id-get-create))))

;;; Dynamic Block for Word Count Table

;;;###autoload
(defun org-dblock-write:org-generate-wordcount-table (params)
  "Generate a table with heading names and their WORDCOUNT property in Org mode.
This is a dynamic block function that can be inserted with:
  #+BEGIN: org-generate-wordcount-table
  #+END:

The table will show all headings with their word counts, excluding
sections tagged with 'noexport'."
  (interactive)
  ;; Execute the function of counting words, add the WORDCOUNT property,
  ;; and update the current word count.
  (writing/ews-org-count-words)
  (let (table-data)
    ;; Traverse each Org entry in the current buffer
    (org-map-entries
     (lambda ()
       (let* ((heading-components (org-heading-components))
              (heading (nth 4 heading-components))  ; Extract heading text
              (level (car heading-components))      ; Extract level
              (tags (org-get-tags))
              (wordcount (org-entry-get nil "WORDCOUNT")))
         ;; Check if we get tags, and if they don't contain the noexport
         (unless (and tags (member "noexport" tags))
           ;; Add the data in the table-data for each heading
           ;; Format with literal asterisks and org link
           (push (cons
                  (format "=%s= %s"
                          (make-string level 42)  ; 42 is asterisk in ASCII
                          (org-link-make-string heading))
                  wordcount)
                 table-data)))))
    ;; Insert table header
    (insert "| Heading         | Wordcount |\n"
            "|-----------------+-----------|\n")
    ;; Insert table rows (reversed to maintain document order)
    (dolist (pair (reverse table-data))
      (insert (format "| %s | %s |\n" (car pair) (cdr pair))))
    ;; Align table once at the end for better performance
    (org-table-align)))

(provide 'writing-wordcount)

;;; writing-wordcount.el ends here
