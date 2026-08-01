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
(defun org-scribe-search-edits-rgrep ()
  "Search the project for inline edit and note markers using rgrep.

Matches `org-scribe-edit-string' (by default the \\=*EDIT\\=* and
\\=*NOTE\\=* markers) across every .org file below the project root.
The search is deliberately project-wide: markers left in the manuscript,
in the notes file and in research notes are all worth seeing in one
pass.

This is the plain-text fallback for `org-scribe-search-edits', which
builds a structured index instead.  Use this one when your markers do
not follow the comment-block convention, or when you have customized
`org-scribe-edit-string' to find markers of your own.

Outside a writing project, falls back to the current buffer's directory
rather than failing, matching `org-scribe-search-todos-recursive'."
  (interactive)
  (let ((root (or (org-scribe-project-root)
                  (file-name-directory (or (buffer-file-name) default-directory)))))
    (rgrep org-scribe-edit-string "*.org" root)))

;;; Edit Marker Index
;;
;; Markers are written inside Org comment blocks:
;;
;;   #+begin_comment
;;   *EDIT*: plot - Alice's motive contradicts chapter 1
;;   #+end_comment
;;
;; Comment blocks are `comment-block' elements in the Org parse tree, so
;; they can be traversed with `org-element-map' — unlike `org-ql', which
;; queries headings only and cannot see comment content at all.  That
;; buys three things a line-based text search cannot give: the complete
;; multi-line marker body, the enclosing scene heading, and immunity to
;; false positives from prose that merely mentions *NOTE*.
;;
;; Only *EDIT* and *NOTE* are indexed.  *SUMMARY* is deliberately
;; excluded: it describes prose that does not exist yet, lives under a
;; TODO heading, and is found with the TODO search alongside every other
;; unwritten scene.  Do not "fix" this by adding it here — see
;; `test-search-edits-index-ignores-summary-marker'.

(defconst org-scribe--edits-marker-regexp
  "^[ \t]*\\*\\(EDIT\\|NOTE\\)\\*:[ \t]*\\(.*\\)$"
  "Regexp matching one marker line inside a comment block.
Group 1 is the marker type, group 2 the rest of the line.")

(defun org-scribe--edits-parse-category (text)
  "Split TEXT into a (CATEGORY . BODY) cons for an *EDIT* marker.
CATEGORY is nil when TEXT carries none, or carries one that is not in
`org-scribe-edit-categories'; BODY is TEXT with the category and its
\" - \" separator removed.  Matching is case-insensitive so that
\"Plot\" groups with \"plot\"."
  (let ((case-fold-search t))
    (cond
     ;; "category - body"
     ((string-match "\\`\\([^-]*[^ \t-]\\)[ \t]+-[ \t]+\\(.*\\)\\'" text)
      (let ((category (match-string 1 text))
            (body (match-string 2 text)))
        (if (seq-find (lambda (known) (string-equal-ignore-case known category))
                      org-scribe-edit-categories)
            (cons (downcase category) body)
          ;; Unknown category: keep it visible in the body rather than
          ;; silently discarding a typo.
          (cons nil text))))
     ;; Empty category, i.e. "*EDIT*: - body" (Tempel inserts the
     ;; separator even when the prompt is answered with RET).
     ((string-match "\\`-[ \t]+\\(.*\\)\\'" text)
      (cons nil (match-string 1 text)))
     (t (cons nil text)))))

(defun org-scribe--edits-parse-block (value)
  "Parse the body of one comment block, VALUE, into marker plists.
Returns a list of plists with :type, :category, :text and :line, where
:line is the marker's zero-based line offset within VALUE.  Lines
following a marker are appended to it, so multi-line markers survive
intact; text before the first marker is ignored."
  (let ((markers nil)
        (current nil)
        (line 0))
    (dolist (text (split-string value "\n"))
      (cond
       ((string-match org-scribe--edits-marker-regexp text)
        (when current (push current markers))
        (let* ((type (upcase (match-string 1 text)))
               (rest (match-string 2 text))
               (parsed (if (equal type "EDIT")
                           (org-scribe--edits-parse-category rest)
                         ;; *NOTE* has no category: never split its body,
                         ;; even when it happens to contain " - ".
                         (cons nil rest))))
          (setq current (list :type type
                              :category (car parsed)
                              :text (string-trim-right (cdr parsed))
                              :line line))))
       ;; Continuation line of the marker currently being read.
       ((and current (not (string-blank-p text)))
        (setq current
              (plist-put current :text
                         (concat (plist-get current :text)
                                 "\n" (string-trim-right text))))))
      (setq line (1+ line)))
    (when current (push current markers))
    (nreverse markers)))

(defun org-scribe--edits-collect-file (file)
  "Return the edit markers found in FILE.
Each marker is a plist with :type, :category, :text, :file, :heading
and :line (a 1-based line number in FILE, for navigation)."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((org-inhibit-startup t)
          (org-mode-hook nil))
      (delay-mode-hooks (org-mode)))
    (let ((collected nil))
      (org-element-map (org-element-parse-buffer) 'comment-block
        (lambda (block)
          (let* ((value (or (org-element-property :value block) ""))
                 (begin (org-element-property :begin block))
                 (heading (org-element-lineage block '(headline)))
                 ;; :begin is the "#+begin_comment" line; block content
                 ;; starts on the line after it.
                 (base-line (1+ (line-number-at-pos begin))))
            (dolist (marker (org-scribe--edits-parse-block value))
              ;; Build the plist explicitly rather than appending to
              ;; MARKER: appending would leave the parser's line *offset*
              ;; ahead of the absolute line number, and `plist-get'
              ;; returns the first match.
              (push (list :type (plist-get marker :type)
                          :category (plist-get marker :category)
                          :text (plist-get marker :text)
                          :file file
                          :heading (and heading
                                        (org-element-property :raw-value heading))
                          :line (+ base-line (plist-get marker :line)))
                    collected)))))
      (nreverse collected))))

(defun org-scribe--edits-collect (root)
  "Return every edit marker below ROOT, across all .org files."
  (let ((markers nil))
    (dolist (file (directory-files-recursively root "\\.org\\'"))
      (setq markers (append markers (org-scribe--edits-collect-file file))))
    markers))

(defun org-scribe--edits-insert-marker (marker root level)
  "Insert MARKER as an Org entry at LEVEL, with paths relative to ROOT."
  (let* ((file (plist-get marker :file))
         (heading (or (plist-get marker :heading)
                      (org-scribe-msg 'edits-index-no-heading)))
         (text (plist-get marker :text)))
    (insert (format "%s [[file:%s::%d][%s]] — %s\n"
                    (make-string level ?*)
                    file (plist-get marker :line)
                    heading (file-relative-name file root)))
    (unless (string-blank-p text)
      (dolist (line (split-string text "\n"))
        (insert "    " line "\n")))))

(defun org-scribe--edits-render (markers root)
  "Insert an Org rendering of MARKERS, with paths relative to ROOT."
  (let* ((edits (seq-filter (lambda (m) (equal "EDIT" (plist-get m :type))) markers))
         (notes (seq-filter (lambda (m) (equal "NOTE" (plist-get m :type))) markers))
         (other-label (org-scribe-msg 'edits-index-other-category))
         ;; Known categories first, in configured order, then the
         ;; catch-all bucket.
         (categories (append org-scribe-edit-categories (list nil))))
    (insert (format "* %s\n" (org-scribe-msg 'edits-index-section-edits)))
    (dolist (category categories)
      (let ((in-category
             (seq-filter (lambda (m)
                           (if category
                               (equal category (plist-get m :category))
                             (null (plist-get m :category))))
                         edits)))
        (when (or in-category
                  (and category org-scribe-edits-index-show-empty-categories))
          (insert (format "** %s\n" (or category other-label)))
          (dolist (marker in-category)
            (org-scribe--edits-insert-marker marker root 3)))))
    (insert (format "\n* %s\n" (org-scribe-msg 'edits-index-section-notes)))
    (dolist (marker notes)
      (org-scribe--edits-insert-marker marker root 2))))

(defvar org-scribe--edits-buffer-name "*org-scribe-edits*"
  "Name of the buffer holding the edit marker index.")

(defvar-local org-scribe--edits-root nil
  "Project root the current edit index was built from.")

(defun org-scribe--edits-build (root)
  "Build the edit index buffer for ROOT and return it."
  (let ((markers (org-scribe--edits-collect root))
        (buffer (get-buffer-create org-scribe--edits-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (point-before (point)))
        (erase-buffer)
        (insert (format "#+TITLE: %s\n" (org-scribe-msg 'edits-index-title)))
        (insert (format "#+STARTUP: showall\n\n"))
        (let ((files (delete-dups (mapcar (lambda (m) (plist-get m :file)) markers))))
          (insert (format "%s\n"
                          (org-scribe-msg 'edits-index-found
                                          (length markers)
                                          (org-scribe-plural (length markers) "")
                                          (length files)
                                          (org-scribe-plural (length files) "")))))
        (insert (format "# %s\n\n" (org-scribe-msg 'edits-index-help)))
        (org-scribe--edits-render markers root)
        (goto-char (min point-before (point-max))))
      (unless (derived-mode-p 'org-mode)
        (let ((org-inhibit-startup t))
          (org-mode)))
      (setq org-scribe--edits-root root)
      (setq buffer-read-only t)
      (add-hook 'after-save-hook #'org-scribe--edits-refresh-on-save)
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key (kbd "g") #'org-scribe-edits-refresh)
      (local-set-key (kbd "q") #'quit-window))
    buffer))

;;;###autoload
(defun org-scribe-edits-refresh ()
  "Rebuild the edit marker index from disk."
  (interactive)
  (let ((root (or (and (derived-mode-p 'org-mode) org-scribe--edits-root)
                  (org-scribe-project-root))))
    (when root
      (org-scribe--edits-build root))))

(defun org-scribe--edits-refresh-on-save ()
  "Rebuild the edit index after saving an Org file it covers.

Only does work when the index buffer is actually displayed, so a
buried index costs nothing.  Added to `after-save-hook' the first time
an index is built rather than at load time, so users who never open one
never pay for the hook."
  (when-let* ((buffer (get-buffer org-scribe--edits-buffer-name))
              ((get-buffer-window buffer t))
              (root (buffer-local-value 'org-scribe--edits-root buffer))
              (file (buffer-file-name))
              ((string-suffix-p ".org" file))
              ((file-in-directory-p file root)))
    (org-scribe--edits-build root)))

;;;###autoload
(defun org-scribe-search-edits (&optional arg)
  "Show a structured index of the project's inline edit markers.

Collects every \\=*EDIT\\=* and \\=*NOTE\\=* marker written inside an Org
comment block, across all .org files below the project root, and
presents them grouped by kind — \\=*EDIT\\=* markers further grouped by
their category (see `org-scribe-edit-categories'), \\=*NOTE\\=* markers
in a section of their own.  Each entry links back to the exact line it
came from.

\\=*SUMMARY\\=* markers are deliberately excluded; they describe scenes
not yet written and belong to the TODO search.

The search is project-wide by design: markers in the manuscript, the
notes file and research notes all surface in one pass.

With a prefix ARG, run the plain-text `org-scribe-search-edits-rgrep'
instead, which finds markers wherever they are written — including
outside comment blocks — at the cost of the grouping.

In the index buffer, `g' refreshes and `q' buries it."
  (interactive "P")
  (if arg
      (org-scribe-search-edits-rgrep)
    (let ((root (or (org-scribe-project-root)
                    (file-name-directory
                     (or (buffer-file-name) default-directory)))))
      (let ((buffer (org-scribe--edits-build root)))
        (if (with-current-buffer buffer
              (save-excursion
                (goto-char (point-min))
                (not (re-search-forward "^\\*\\*\\* " nil t))))
            (message (org-scribe-msg 'edits-index-none-found
                                     (abbreviate-file-name root)))
          (pop-to-buffer buffer))))))

;;; Tempel snippets (optional)
;;
;; The snippets that *write* markers live here, next to the parser that
;; *reads* them, because the two have to agree on the grammar.  The
;; `edit' snippet prompts from `org-scribe-edit-categories', the same
;; variable the index groups by, so the prompt and the grouping cannot
;; drift apart.

;; Tempel is an optional dependency: declared, never required.
(defvar tempel-path)

(defconst org-scribe--search-source-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding this file, used to locate bundled snippets.")

;;;###autoload
(defun org-scribe-tempel-snippets-file ()
  "Return the path to org-scribe's bundled Tempel snippet file.
The file defines `edit' and `note' snippets that insert inline edit
markers wrapped in a comment block, matching the convention
`org-scribe-search-edits' indexes."
  (expand-file-name "../snippets/org-scribe-tempel.eld"
                    org-scribe--search-source-directory))

;;;###autoload
(defun org-scribe-tempel-setup ()
  "Add org-scribe's bundled snippets to `tempel-path'.

Call this from your init file if you use Tempel and want the `edit' and
`note' snippets:

  (with-eval-after-load \\='tempel (org-scribe-tempel-setup))

This only ever adds to `tempel-path': your own templates file is never
read, modified, or overridden by org-scribe.  If both define a snippet
of the same name, whichever file comes first in `tempel-path' wins.

Note that `tempel-path' may hold a single path *string* rather than a
list — that is its default — so this normalizes it to a list first
rather than using `add-to-list', which would fail on a string.  Calling
this twice is harmless.

Signals a `user-error' when Tempel is not installed; the markers are
plain text and can always be typed by hand."
  (interactive)
  (unless (boundp 'tempel-path)
    (user-error "Tempel is not installed or not yet loaded"))
  (let ((file (org-scribe-tempel-snippets-file))
        (paths (if (listp tempel-path) tempel-path (list tempel-path))))
    (unless (member file paths)
      (setq tempel-path (cons file paths)))
    tempel-path))

;;;###autoload
(defalias 'org-scribe-search-edits-recursive #'org-scribe-search-edits
  "Compatibility alias for `org-scribe-search-edits'.
The command used to be an `rgrep' wrapper; that behavior now lives in
`org-scribe-search-edits-rgrep' and is still reachable with a prefix
argument.")

(provide 'org-scribe-search)

;;; org-scribe-search.el ends here
