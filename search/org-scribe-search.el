;;; org-scribe-search.el --- Search functions for org-scribe -*- lexical-binding: t; -*-

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
(require 'org-scribe-core)
(require 'org-scribe-config)
(require 'org-scribe-messages)

;; Declare external functions
(declare-function org-ql-search "org-ql")
(declare-function org-scribe--get-all-characters "linking/org-scribe-character-links")
(declare-function org-scribe--get-all-locations "linking/org-scribe-location-links")
(declare-function org-scribe--get-all-plot-threads "linking/org-scribe-plot-links")

;;; Helper Functions for ID Links

(defun org-scribe--extract-link-text (text)
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

(defun org-scribe--property-contains-p (property-value search-term)
  "Check if PROPERTY-VALUE contains SEARCH-TERM.
Handles both plain text and ID links.
Case-insensitive search."
  (when property-value
    (let ((clean-text (org-scribe--extract-link-text property-value)))
      (and clean-text
           (string-match-p (regexp-quote search-term) clean-text)))))

(defun org-scribe--property-to-list (property-value)
  "Convert PROPERTY-VALUE to list of items.
Handles both plain text and ID links.
Splits on comma (but not on commas inside a link's display text — see
`org-scribe--split-property-list') and extracts display text from links.

Examples:
  \"Alex, Sam\" → (\"Alex\" \"Sam\")
  \"[[id:abc][Alex]], [[id:def][Sam]]\" → (\"Alex\" \"Sam\")
  \"[[id:abc][Smith, John]]\" → (\"Smith, John\")"
  (when property-value
    (mapcar #'org-scribe--extract-link-text
            (org-scribe--split-property-list property-value))))

;;; Shared Search Helpers

(defun org-scribe--read-search-term (feature-require get-all-fn prompt-completion prompt-free)
  "Read a search term with optional completion from entity database.
FEATURE-REQUIRE is the feature symbol to require (e.g. \\='org-scribe-character-links).
GET-ALL-FN is the function to call to get the entity alist.
PROMPT-COMPLETION is the message key for the completion prompt.
PROMPT-FREE is the message key for the free-text prompt."
  (let* ((items (condition-case nil
                    (progn
                      (require feature-require)
                      (funcall get-all-fn))
                  (error nil)))
         (names (mapcar #'car items)))
    (if (null names)
        (read-string (org-scribe-msg prompt-free))
      (completing-read (org-scribe-msg prompt-completion)
                       names nil nil nil nil nil))))

(defun org-scribe--search-property (term error-key property)
  "Search for TERM in PROPERTY across headings in current buffer.
ERROR-KEY is the message key for empty input validation.
PROPERTY is a canonical scene property key (e.g. \\='pov); every
localized alias for it (see `org-scribe-scene-property-aliases') is
checked so the search works regardless of the project's language."
  (when (string-empty-p (string-trim term))
    (user-error (org-scribe-msg error-key)))
  (unless (featurep 'org-ql)
    (user-error (org-scribe-msg 'error-org-ql-required)))
  (org-ql-search (current-buffer)
    `(and (heading)
          (let ((val (org-scribe-scene-property-get ',property)))
            (org-scribe--property-contains-p val ,term)))))

;;; Property-based scene search (PoV / Characters / Plot / Location)
;;
;; Each function is a thin wrapper: it reads a term (with completion from
;; the entity database when available) then calls `org-scribe--search-property'.

;;;###autoload
(defun org-scribe-org-find-pov (char)
  "Show sparse tree of scenes with POV character CHAR.
Uses completion from characters database when available.
Requires org-ql package."
  (interactive
   (list (org-scribe--read-search-term
          'org-scribe-character-links #'org-scribe--get-all-characters
          'search-pov-prompt 'search-pov-prompt-free)))
  (org-scribe--search-property char 'error-empty-character 'pov))

;;;###autoload
(defun org-scribe-org-find-character (char)
  "Show sparse tree of scenes with CHARACTER CHAR.
Uses completion from characters database when available.
Requires org-ql package."
  (interactive
   (list (org-scribe--read-search-term
          'org-scribe-character-links #'org-scribe--get-all-characters
          'search-char-prompt 'search-char-prompt-free)))
  (org-scribe--search-property char 'error-empty-character 'characters))

;;;###autoload
(defun org-scribe-org-find-plot (term)
  "Show sparse tree of scenes matching TERM in plot property.
Uses completion from plot database when available.
Requires org-ql package."
  (interactive
   (list (org-scribe--read-search-term
          'org-scribe-plot-links #'org-scribe--get-all-plot-threads
          'search-plot-prompt 'search-plot-prompt-free)))
  (org-scribe--search-property term 'error-empty-plot 'plot))

;;;###autoload
(defun org-scribe-org-find-location (loc)
  "Show sparse tree of scenes with LOCATION LOC.
Uses completion from locations database when available.
Requires org-ql package."
  (interactive
   (list (org-scribe--read-search-term
          'org-scribe-location-links #'org-scribe--get-all-locations
          'search-loc-prompt 'search-loc-prompt-free)))
  (org-scribe--search-property loc 'error-empty-location 'location))

;;; Recursive TODO Search

;;;###autoload
(defun org-scribe-search-todos-recursive ()
  "Search for TODO items (not DONE) in current directory tree using org-ql.
Finds all .org files recursively from the current buffer's directory
and displays all TODO keywords with an active (non-DONE) status.
Results are grouped by file for easy navigation.
Requires org-ql package to be installed."
  (interactive)
  (unless (featurep 'org-ql)
    (user-error (org-scribe-msg 'error-org-ql-required)))
  (let* ((project-root (or (org-scribe-project-root)
                          (file-name-directory (or (buffer-file-name) default-directory))))
         (org-files (directory-files-recursively project-root "\\.org$")))
    (if org-files
        (org-ql-search org-files
          `(todo ,@org-scribe-todo-keywords)
          :title "TODO items in writing project"
          :super-groups '((:auto-category t)))
      (message (org-scribe-msg 'msg-no-org-files project-root)))))

;;;###autoload
(defun org-scribe-search-edits-recursive ()
  "Search the project for inline edit and note markers using rgrep.

Matches `org-scribe-edit-string' (by default the \\=*EDIT\\=* and
\\=*NOTE\\=* markers) across every .org file below the project root.
The search is deliberately project-wide: markers left in the manuscript,
in the notes file and in research notes are all worth seeing in one
pass.

Markers are conventionally written inside an Org comment block, which
keeps them out of exports and out of word counts — see the manual for
the convention.  This is a plain text search, so it finds them wherever
they are written.

Outside a writing project, falls back to the current buffer's directory
rather than failing, matching `org-scribe-search-todos-recursive'."
  (interactive)
  (let ((root (or (org-scribe-project-root)
                  (file-name-directory (or (buffer-file-name) default-directory)))))
    (rgrep org-scribe-edit-string "*.org" root)))

(provide 'org-scribe-search)

;;; org-scribe-search.el ends here
