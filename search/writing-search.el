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

;;; Code:

(require 'org)
(require 'writing-core)

;; Declare external functions
(declare-function org-ql-search "org-ql")

;;; Search by POV

;;;###autoload
(defun writing/org-find-pov (char)
  "Show sparse tree of scenes with POV character.
Searches for CHAR in the POV property of org headings.
Requires org-ql package to be installed."
  (interactive "sCharacter (POV): ")
  (when (string-empty-p (string-trim char))
    (user-error "Character name cannot be empty"))
  (unless (featurep 'org-ql)
    (user-error "org-ql package is required for search functions"))
  (org-ql-search (current-buffer)
    `(and (heading)
          (let ((pov (org-entry-get (point) "POV")))
            (and pov (string-match-p ,char pov))))))

;;; Search by Character

;;;###autoload
(defun writing/org-find-character (char)
  "Show sparse tree of scenes with CHARACTER.
Searches for CHAR in the Characters property, which should contain
a comma-separated list of character names.
Requires org-ql package to be installed."
  (interactive "sCharacter name: ")
  (when (string-empty-p (string-trim char))
    (user-error "Character name cannot be empty"))
  (unless (featurep 'org-ql)
    (user-error "org-ql package is required for search functions"))
  (org-ql-search (current-buffer)
    `(and (heading)
          (let* ((chars-prop (org-entry-get (point) "Characters"))
                 (chars-list (when chars-prop
                               (mapcar #'string-trim
                                       (split-string chars-prop ",")))))
            (member ,char chars-list)))))

;;; Search by Plot

;;;###autoload
(defun writing/org-find-plot (term)
  "Show sparse tree of scenes matching TERM in plot property.
Requires org-ql package to be installed."
  (interactive "sPlot term: ")
  (when (string-empty-p (string-trim term))
    (user-error "Plot term cannot be empty"))
  (unless (featurep 'org-ql)
    (user-error "org-ql package is required for search functions"))
  (org-ql-search (current-buffer)
    `(and (heading)
          (let ((plot (org-entry-get (point) "plot")))
            (and plot (string-match-p ,term plot))))))

;;; Search by Location

;;;###autoload
(defun writing/org-find-location (loc)
  "Show sparse tree of scenes with LOCATION.
Requires org-ql package to be installed."
  (interactive "sLocation: ")
  (when (string-empty-p (string-trim loc))
    (user-error "Location cannot be empty"))
  (unless (featurep 'org-ql)
    (user-error "org-ql package is required for search functions"))
  (org-ql-search (current-buffer)
    `(and (heading)
          (let ((location (org-entry-get (point) "location")))
            (and location (string-match-p ,loc location))))))

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
