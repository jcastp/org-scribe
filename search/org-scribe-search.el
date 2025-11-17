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
Splits on comma and extracts display text from links.

Examples:
  \"Alex, Sam\" → (\"Alex\" \"Sam\")
  \"[[id:abc][Alex]], [[id:def][Sam]]\" → (\"Alex\" \"Sam\")"
  (when property-value
    (let ((clean-text (org-scribe--extract-link-text property-value)))
      (when clean-text
        (mapcar #'string-trim
                (split-string clean-text ","))))))

;;; Search by POV

;;;###autoload
(defun org-scribe/org-find-pov (char)
  "Show sparse tree of scenes with POV character.

Input method:
- If characters.org exists: completion menu with fuzzy matching
- Otherwise: free text input with substring matching

Search uses substring matching in both cases.
Handles both plain text and ID-link format in properties.
Requires org-ql package to be installed."
  (interactive
   (list
    (let* ((chars (condition-case nil
                      (progn
                        (require 'org-scribe-character-links)
                        (org-scribe--get-all-characters))
                    (error nil)))
           (char-names (mapcar #'car chars)))
      (if (null char-names)
          ;; No database - free text input
          (read-string (org-scribe-i18n search-pov-free))
        ;; Database available - completion with fuzzy matching
        (completing-read (org-scribe-i18n search-pov-fuzzy)
                        char-names
                        nil      ; predicate
                        nil      ; require-match = nil (allow free text)
                        nil      ; initial-input
                        nil      ; hist
                        nil))))) ; def
  (when (string-empty-p (string-trim char))
    (user-error (org-scribe-i18n error-empty-character)))
  (unless (featurep 'org-ql)
    (user-error (org-scribe-i18n error-org-ql-required)))
  (org-ql-search (current-buffer)
    `(and (heading)
          (let ((pov (org-entry-get (point) "PoV")))
            (org-scribe--property-contains-p pov ,char)))))

;;; Search by Character

;;;###autoload
(defun org-scribe/org-find-character (char)
  "Show sparse tree of scenes with CHARACTER.

Input method:
- If characters.org exists: completion menu with fuzzy matching
- Otherwise: free text input with substring matching

Search uses substring matching (not exact match) in the Characters property.
This means searching for 'Alex' will find 'Alex Rivera, Sam Chen'.
Handles both plain text and ID-link format in properties.
Requires org-ql package to be installed."
  (interactive
   (list
    (let* ((chars (condition-case nil
                      (progn
                        (require 'org-scribe-character-links)
                        (org-scribe--get-all-characters))
                    (error nil)))
           (char-names (mapcar #'car chars)))
      (if (null char-names)
          ;; No database - free text input
          (read-string (org-scribe-i18n search-char-free))
        ;; Database available - completion with fuzzy matching
        (completing-read (org-scribe-i18n search-char-fuzzy)
                        char-names
                        nil      ; predicate
                        nil      ; require-match = nil (allow free text)
                        nil      ; initial-input
                        nil      ; hist
                        nil))))) ; def
  (when (string-empty-p (string-trim char))
    (user-error (org-scribe-i18n error-empty-character)))
  (unless (featurep 'org-ql)
    (user-error (org-scribe-i18n error-org-ql-required)))
  ;; IMPORTANT: Changed from exact list matching to substring matching
  ;; Old: (member ,char chars-list) - required exact match
  ;; New: org-scribe--property-contains-p - allows substring match
  (org-ql-search (current-buffer)
    `(and (heading)
          (let ((chars-prop (org-entry-get (point) "Characters")))
            (org-scribe--property-contains-p chars-prop ,char)))))

;;; Search by Plot

;;;###autoload
(defun org-scribe/org-find-plot (term)
  "Show sparse tree of scenes matching TERM in plot property.

Input method:
- If plot.org exists: completion menu with fuzzy matching
- Otherwise: free text input with substring matching

Search uses substring matching in both cases.
Handles both plain text and ID-link format in properties.
Requires org-ql package to be installed."
  (interactive
   (list
    (let* ((threads (condition-case nil
                        (progn
                          (require 'org-scribe-plot-links)
                          (org-scribe--get-all-plot-threads))
                      (error nil)))
           (thread-names (mapcar #'car threads)))
      (if (null thread-names)
          ;; No database - free text input
          (read-string (org-scribe-i18n search-plot-free))
        ;; Database available - completion with fuzzy matching
        (completing-read (org-scribe-i18n search-plot-fuzzy)
                        thread-names
                        nil      ; predicate
                        nil      ; require-match = nil (allow free text)
                        nil      ; initial-input
                        nil      ; hist
                        nil))))) ; def
  (when (string-empty-p (string-trim term))
    (user-error (org-scribe-i18n error-empty-plot)))
  (unless (featurep 'org-ql)
    (user-error (org-scribe-i18n error-org-ql-required)))
  (org-ql-search (current-buffer)
    `(and (heading)
          (let ((plot (org-entry-get (point) "Plot")))
            (org-scribe--property-contains-p plot ,term)))))

;;; Search by Location

;;;###autoload
(defun org-scribe/org-find-location (loc)
  "Show sparse tree of scenes with LOCATION.

Input method:
- If locations.org exists: completion menu with fuzzy matching
- Otherwise: free text input with substring matching

Search uses substring matching in both cases.
Handles both plain text and ID-link format in properties.
Requires org-ql package to be installed."
  (interactive
   (list
    (let* ((locations (condition-case nil
                          (progn
                            (require 'org-scribe-location-links)
                            (org-scribe--get-all-locations))
                        (error nil)))
           (location-names (mapcar #'car locations)))
      (if (null location-names)
          ;; No database - free text input
          (read-string (org-scribe-i18n search-loc-free))
        ;; Database available - completion with fuzzy matching
        (completing-read (org-scribe-i18n search-loc-fuzzy)
                        location-names
                        nil      ; predicate
                        nil      ; require-match = nil (allow free text)
                        nil      ; initial-input
                        nil      ; hist
                        nil))))) ; def
  (when (string-empty-p (string-trim loc))
    (user-error (org-scribe-i18n error-empty-location)))
  (unless (featurep 'org-ql)
    (user-error (org-scribe-i18n error-org-ql-required)))
  (org-ql-search (current-buffer)
    `(and (heading)
          (let ((location (org-entry-get (point) "Location")))
            (org-scribe--property-contains-p location ,loc)))))

;;; Recursive TODO Search

;;;###autoload
(defun org-scribe/search-todos-recursive ()
  "Search for TODO items (not DONE) in current directory tree using org-ql.
Finds all .org files recursively from the current buffer's directory
and displays all TODO keywords with an active (non-DONE) status.
Results are grouped by file for easy navigation.
Requires org-ql package to be installed."
  (interactive)
  (unless (featurep 'org-ql)
    (user-error (org-scribe-i18n error-org-ql-required)))
  (let* ((current-dir (file-name-directory (or (buffer-file-name) default-directory)))
         (org-files (directory-files-recursively current-dir "\\.org$")))
    (if org-files
        (org-ql-search org-files
          '(and (todo) (not (done)))
          :title (org-scribe-i18n todo-search-title)
          :super-groups '((:auto-map (lambda (item)
                                       (concat "File: "
                                               (file-name-nondirectory (buffer-file-name)))))))
      (message (org-scribe-i18n msg-no-org-files current-dir)))))

(provide 'org-scribe-search)

;;; org-scribe-search.el ends here
