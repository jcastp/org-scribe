;;; writing-search.el --- Search functions for emacs-writing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Search functions using org-ql for finding scenes by various criteria:
;; - POV (Point of View) character
;; - Characters appearing in scene
;; - Plot keywords
;; - Location
;; - TODO items across project
;;
;; These functions handle both plain text and ID-link format in properties.

;;; Code:

(require 'org)
(require 'writing-core)

;; Declare external functions
(declare-function org-ql-search "org-ql")

;;; Helper Functions for ID Links

(defun writing--extract-link-text (text)
  "Extract display text from Org ID links in TEXT.
Handles both ID links and plain text for backward compatibility.

Examples:
  \"[[id:abc123][Alex]]\" → \"Alex\"
  \"Alex\" → \"Alex\"
  \"[[id:abc123][Alex]], [[id:def456][Sam]]\" → \"Alex, Sam\""
  (if (not text)
      nil
    ;; Replace all ID links with their display text
    (let ((result text))
      ;; Match [[id:ANYTHING][DISPLAY-TEXT]] and replace with DISPLAY-TEXT
      (while (string-match "\\[\\[id:[^]]+\\]\\[\\([^]]+\\)\\]\\]" result)
        (setq result (replace-match "\\1" nil nil result)))
      result)))

(defun writing--property-contains-p (property-value search-term)
  "Check if PROPERTY-VALUE contains SEARCH-TERM.
Handles both plain text and ID links.
Case-insensitive search."
  (when property-value
    (let ((clean-text (writing--extract-link-text property-value)))
      (and clean-text
           (string-match-p (regexp-quote search-term) clean-text)))))

(defun writing--property-to-list (property-value)
  "Convert PROPERTY-VALUE to list of items.
Handles both plain text and ID links.
Splits on comma and extracts display text from links.

Examples:
  \"Alex, Sam\" → (\"Alex\" \"Sam\")
  \"[[id:abc][Alex]], [[id:def][Sam]]\" → (\"Alex\" \"Sam\")"
  (when property-value
    (let ((clean-text (writing--extract-link-text property-value)))
      (when clean-text
        (mapcar #'string-trim
                (split-string clean-text ","))))))

;;; Search by POV

;;;###autoload
(defun writing/org-find-pov (char)
  "Show sparse tree of scenes with POV character.
Searches for CHAR in the POV property of org headings.
Handles both plain text and ID-link format in properties.
Requires org-ql package to be installed."
  (interactive "sCharacter (POV): ")
  (when (string-empty-p (string-trim char))
    (user-error "Character name cannot be empty"))
  (unless (featurep 'org-ql)
    (user-error "org-ql package is required for search functions"))
  (org-ql-search (current-buffer)
    `(and (heading)
          (let ((pov (org-entry-get (point) "PoV")))
            (writing--property-contains-p pov ,char)))))

;;; Search by Character

;;;###autoload
(defun writing/org-find-character (char)
  "Show sparse tree of scenes with CHARACTER.
Searches for CHAR in the Characters property, which should contain
a comma-separated list of character names (plain text or ID links).
Requires org-ql package to be installed."
  (interactive "sCharacter name: ")
  (when (string-empty-p (string-trim char))
    (user-error "Character name cannot be empty"))
  (unless (featurep 'org-ql)
    (user-error "org-ql package is required for search functions"))
  (org-ql-search (current-buffer)
    `(and (heading)
          (let* ((chars-prop (org-entry-get (point) "Characters"))
                 (chars-list (writing--property-to-list chars-prop)))
            (member ,char chars-list)))))

;;; Search by Plot

;;;###autoload
(defun writing/org-find-plot (term)
  "Show sparse tree of scenes matching TERM in plot property.
Handles both plain text and ID-link format in properties.
Requires org-ql package to be installed."
  (interactive "sPlot term: ")
  (when (string-empty-p (string-trim term))
    (user-error "Plot term cannot be empty"))
  (unless (featurep 'org-ql)
    (user-error "org-ql package is required for search functions"))
  (org-ql-search (current-buffer)
    `(and (heading)
          (let ((plot (org-entry-get (point) "Plot")))
            (writing--property-contains-p plot ,term)))))

;;; Search by Location

;;;###autoload
(defun writing/org-find-location (loc)
  "Show sparse tree of scenes with LOCATION.
Handles both plain text and ID-link format in properties.
Requires org-ql package to be installed."
  (interactive "sLocation: ")
  (when (string-empty-p (string-trim loc))
    (user-error "Location cannot be empty"))
  (unless (featurep 'org-ql)
    (user-error "org-ql package is required for search functions"))
  (org-ql-search (current-buffer)
    `(and (heading)
          (let ((location (org-entry-get (point) "Location")))
            (writing--property-contains-p location ,loc)))))

;;; Recursive TODO Search

;;;###autoload
(defun writing/search-todos-recursive ()
  "Search for TODO items (not DONE) in current directory tree using org-ql.
Finds all .org files recursively from the current buffer's directory
and displays all TODO keywords with an active (non-DONE) status.
Results are grouped by file for easy navigation.
Requires org-ql package to be installed."
  (interactive)
  (unless (featurep 'org-ql)
    (user-error "org-ql package is required for search functions"))
  (let* ((current-dir (file-name-directory (or (buffer-file-name) default-directory)))
         (org-files (directory-files-recursively current-dir "\\.org$")))
    (if org-files
        (org-ql-search org-files
          '(and (todo) (not (done)))
          :title "TODO items in writing project"
          :super-groups '((:auto-map (lambda (item)
                                       (concat "File: "
                                               (file-name-nondirectory (buffer-file-name)))))))
      (message "No .org files found in %s and subdirectories" current-dir))))

(provide 'writing-search)

;;; writing-search.el ends here
