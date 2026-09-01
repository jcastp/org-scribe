;;; org-scribe-core.el --- Core utilities for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core utility functions for the org-scribe package.
;; Includes project detection, feature checking, and common helpers.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'project)
(require 'org-scribe-messages)

;;; Project Detection

(defun org-scribe-project-root ()
  "Find root directory of writing project.
Looks for .org-scribe-project file, then falls back to `project-current'."
  (or (locate-dominating-file default-directory ".org-scribe-project")
      (when-let ((proj (project-current)))
        (project-root proj))
      default-directory))

(defvar org-scribe--project-type-cache nil
  "Alist of (PROJECT-ROOT . PROJECT-TYPE) for caching project type detection.
Keys are canonicalized with `org-scribe--normalize-project-root' so that
different spellings of the same directory (trailing slash, symlink) share one
entry.  Entries are invalidated via `org-scribe-project-type-cache-clear'.")

(defun org-scribe--normalize-project-root (root)
  "Return a canonical form of ROOT for use as a project-type cache key.
Resolves symlinks and normalizes trailing slashes so that different
spellings of the same directory map to the same cache entry."
  (file-truename (file-name-as-directory (expand-file-name root))))

(defun org-scribe-project-type-cache-clear (&optional root)
  "Invalidate the project-type cache.
With ROOT, remove only the entry for that project root (its type will be
re-detected on next use).  With no argument, clear the entire cache.

Call this after anything that can change what `org-scribe-project-type'
would detect for a project already in the cache: creating project marker
files/structure in an existing directory, or switching to a different
project whose root was previously misdetected."
  (if root
      (setq org-scribe--project-type-cache
            (assoc-delete-all (org-scribe--normalize-project-root root)
                               org-scribe--project-type-cache #'string=))
    (setq org-scribe--project-type-cache nil)))

(defun org-scribe-project-type ()
  "Detect the type of writing project.
Returns one of:
  'novel - Novel project (objects/ directory with separate files)
  'short-story - Short story project (consolidated notes.org)
  'unknown - Cannot determine project type

Detection strategy:
1. Check cache for this project root
2. Read .org-scribe-project marker file if it exists (look for Type: line)
3. Check for existence of objects/ directory structure (indicates novel)
4. Check for story.org or cuento.org (indicates short story)
5. Check for novel.org or novela.org (indicates novel)
6. Return 'unknown if none of the above"
  (let* ((root (org-scribe-project-root))
         (cache-key (org-scribe--normalize-project-root root))
         (cached (alist-get cache-key org-scribe--project-type-cache nil nil #'string=)))
    (if cached
        cached
      ;; Not cached, detect and cache
      (let ((type
             (cond
              ;; Strategy 1: Read marker file
              ((let ((type-str (org-scribe--project-marker-get root "Type")))
                 (cond
                  ((equal type-str "short-story") 'short-story)
                  ((equal type-str "novel") 'novel)
                  (t nil))))

              ;; Strategy 2: Check for objects/ directory (novel indicator)
              ((or (file-directory-p (expand-file-name "objects" root))
                   (file-directory-p (expand-file-name "objects/" root)))
               'novel)

              ;; Strategy 3: Check for story.org or cuento.org (short story indicator)
              ((or (file-exists-p (expand-file-name "story.org" root))
                   (file-exists-p (expand-file-name "cuento.org" root)))
               'short-story)

              ;; Strategy 4: Check for novel.org or novela.org (novel indicator)
              ((or (file-exists-p (expand-file-name "novel.org" root))
                   (file-exists-p (expand-file-name "novela.org" root)))
               'novel)

              ;; Unknown
              (t 'unknown))))
        ;; Cache the result
        (setq org-scribe--project-type-cache
              (cons (cons cache-key type) org-scribe--project-type-cache))
        type))))

;;; Project Marker File

(defun org-scribe--project-marker-get (root key)
  "Return the value recorded for KEY in ROOT's .org-scribe-project marker file.
KEY is a string such as \"Type\", \"Plan\", or \"Planner\", matched
case-insensitively against a line of the form \"# KEY: value\".  Returns
the trimmed value string, or nil if the marker file or the line does
not exist."
  (let ((marker-file (expand-file-name ".org-scribe-project" root)))
    (when (file-exists-p marker-file)
      (with-temp-buffer
        (insert-file-contents marker-file)
        (goto-char (point-min))
        (let ((case-fold-search t))
          (when (re-search-forward
                 (concat "^# " (regexp-quote key) ": \\(.*\\)$")
                 nil t)
            (string-trim (match-string 1))))))))

(defun org-scribe--project-marker-set (root key value)
  "Idempotently record KEY as VALUE in ROOT's .org-scribe-project marker file.
Replaces an existing \"# KEY: ...\" line (matched case-insensitively) in
place, or appends a new one, preserving the rest of the file.  Does
nothing if the marker file does not exist."
  (let ((marker-file (expand-file-name ".org-scribe-project" root)))
    (when (file-exists-p marker-file)
      (with-temp-buffer
        (insert-file-contents marker-file)
        (goto-char (point-min))
        (let ((case-fold-search t))
          (if (re-search-forward
               (concat "^# " (regexp-quote key) ": .*$")
               nil t)
              (replace-match (format "# %s: %s" key value))
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (format "# %s: %s\n" key value))))
        (write-region (point-min) (point-max) marker-file nil 'silent)))))

(defun org-scribe-planner-gate (&optional root)
  "Return the per-project writing-planner opt-in state.
Returns \\='yes, \\='no, or nil (undecided) read from the \"# Planner:\"
line of ROOT's (default: the current project's) .org-scribe-project
marker file.  nil means the project has not been asked yet, or has no
marker file at all.

This function lives in `org-scribe-core.el', not the planner module,
precisely so that callers which must not force-load the (lazily
autoloaded) planner — the hydra menu, the project health report — can
still check the gate first."
  (let* ((root (or root (org-scribe-project-root)))
         (value (org-scribe--project-marker-get root "Planner")))
    (cond ((equal value "yes") 'yes)
          ((equal value "no") 'no)
          (t nil))))

(defun org-scribe--find-existing-file (root &rest relative-paths)
  "Return the first existing file from RELATIVE-PATHS under ROOT, or nil."
  (cl-loop for path in relative-paths
           for full = (expand-file-name path root)
           when (file-exists-p full) return full))

(defun org-scribe--find-existing-dir (root &rest relative-paths)
  "Return the first existing directory from RELATIVE-PATHS under ROOT, or nil."
  (cl-loop for path in relative-paths
           for full = (expand-file-name path root)
           when (file-directory-p full) return full))

(defun org-scribe-project-structure ()
  "Detect project structure and return layout information.
Returns plist with:
  :root          - project root directory
  :novel-file    - main manuscript file (novel.org or novela.org)
  :notes-dir     - notes directory (notes/ or notas/)
  :notes-file    - notes file (notes/notes.org, notas/notas.org, notes.org, or notas.org)
  :characters-file - characters file (objects/characters.org or objects/personajes.org)
  :locations-file  - locations file (objects/locations.org or objects/localizaciones.org)
  :plot-file       - plot file (objects/plot.org or objects/trama.org)
  :timeline-file   - timeline file (objects/timeline.org or objects/cronologia.org)
  :objects-file    - objects file (objects/objects.org or objects/objetos.org)
  :design-file     - method design file (design.org or diseno.org), or nil
  :plan-file       - writing plan file (plan.org in the project root), or nil

All file/directory values are nil if the path does not exist."
  (let* ((root (org-scribe-project-root)))
    (list :root root
          :novel-file (org-scribe--find-existing-file root
                        "novel.org" "novela.org")
          :notes-dir (org-scribe--find-existing-dir root
                       "notes" "notas")
          :notes-file (org-scribe--find-existing-file root
                        "notes/notes.org" "notas/notas.org"
                        "notes.org" "notas.org")
          :characters-file (org-scribe--find-existing-file root
                             "objects/characters.org" "objects/personajes.org"
                             "characters.org" "personajes.org")
          :locations-file (org-scribe--find-existing-file root
                            "objects/locations.org" "objects/localizaciones.org"
                            "locations.org" "localizaciones.org")
          :plot-file (org-scribe--find-existing-file root
                       "objects/plot.org" "objects/trama.org"
                       "plot.org" "trama.org")
          :timeline-file (org-scribe--find-existing-file root
                           "objects/timeline.org" "objects/cronologia.org"
                           "timeline.org" "cronologia.org")
          :objects-file (org-scribe--find-existing-file root
                          "objects/objects.org" "objects/objetos.org"
                          "objects.org" "objetos.org")
          :design-file (org-scribe--find-existing-file root
                         "design.org" "diseno.org")
          :plan-file (org-scribe--find-existing-file root "plan.org"))))

;;; Scene Property Localization
;;
;; Scene metadata properties (PoV, Characters, Plot, ...) are stored as
;; literal Org property names.  English project templates and Spanish
;; project templates use different literal names for the same logical
;; property (e.g. "Characters" vs "Personajes"), so every reader/writer
;; of these properties must go through the alias table below instead of
;; hardcoding one literal name.

(defconst org-scribe--scene-property-aliases
  '((pov               . ("PoV"))
    (characters        . ("Characters" "Personajes"))
    (plot              . ("Plot" "Trama"))
    (plot-point        . ("Plot-point" "Punto-de-trama"))
    (timeline          . ("Timeline" "Linea-temporal"))
    (location          . ("Location" "Localizacion"))
    (description       . ("Description" "Descripcion"))
    (summary           . ("Summary" "Resumen"))
    (scene-motivation  . ("Scene-motivation" "Motivacion-escena"))
    (conflict-source   . ("Conflict-source" "Fuente-conflicto"))
    (gap               . ("Gap" "Brecha"))
    (what-is-at-stake  . ("What-is-at-stake" "Que-esta-en-juego"))
    (world-problem     . ("World-problem" "Problema-mundo"))
    (emotion           . ("Emotion" "Emocion"))
    (tension-level     . ("Tension-level" "Nivel-tension"))
    (outcome           . ("Outcome" "Resultado"))
    (sequel-decision   . ("Sequel-decision" "Decision-secuela"))
    (comment           . ("Comment" "Comentario")))
  "Canonical scene property key -> localized property name aliases.
Each value lists every literal Org property name known to be used for
that logical property, English first, then Spanish.

`beat' (\"Beat\" / \"Ritmo\") was removed: no module ever read it, and it
duplicated `plot-point' — the method's own structural classifier — with a
term borrowed from a different taxonomy.  A scene written before the
removal may still carry the property; unknown names pass through
`org-scribe-scene-property-aliases' unchanged, so nothing breaks.")

;; *EDIT* marker categories (`org-scribe-edit-categories') are plain
;; strings, not localized like scene properties, but templates for
;; different project languages still write category names in that
;; language -- the Spanish design template asks the writer for
;; "*EDIT*: diseño - ..." even though the canonical category is
;; "design".  Without an alias, that marker would file under the
;; catch-all "other" bucket instead of the "design" section it was
;; meant for.  The table below is deliberately small: most categories
;; ("plot", "scene", "character", "prose") are used as-is by every
;; template regardless of language, and only "design" currently has a
;; localized spelling in the templates.  Add an entry here, not a
;; second literal string, if that changes.

(defconst org-scribe--edit-category-aliases
  '(("design" . ("Diseño")))
  "Canonical *EDIT* category -> other spellings known to appear in templates.
Each key is a canonical value from `org-scribe-edit-categories'; each
value lists additional literal spellings (e.g. localized ones) that
should canonicalize to that key.  Matching is case-insensitive.")

(defun org-scribe-edit-category-canonical (category)
  "Return the canonical spelling of CATEGORY, or CATEGORY unchanged.
Looks CATEGORY up in `org-scribe--edit-category-aliases' (both the
canonical keys and their known alternate spellings), case-insensitively.
Returns CATEGORY as given when it matches nothing, so callers can pass
an unrecognized or already-canonical category safely."
  (or (car (cl-find-if
            (lambda (row)
              (or (string-equal-ignore-case (car row) category)
                  (cl-member category (cdr row) :test #'string-equal-ignore-case)))
            org-scribe--edit-category-aliases))
      category))

(defun org-scribe-project-language ()
  "Return the language symbol (\\='en or \\='es) for the current project.
Reads the \"# Language:\" line from the project's .org-scribe-project
marker file.  Falls back to `org-scribe-template-language' (or \\='en
if that is unbound) when no marker file or line is found."
  (let* ((root (org-scribe-project-root))
         (lang (org-scribe--project-marker-get root "Language")))
    (or (cond ((equal lang "es") 'es)
              ((equal lang "en") 'en))
        (and (boundp 'org-scribe-template-language)
             (default-value 'org-scribe-template-language))
        'en)))

(defun org-scribe-scene-property-aliases (canonical-key)
  "Return the list of literal property name aliases for CANONICAL-KEY.
CANONICAL-KEY is a symbol such as \\='characters or \\='plot (see
`org-scribe--scene-property-aliases').  If CANONICAL-KEY is not found
in the alias table, it is returned as a single-element list unchanged,
so callers may also pass a literal property name directly."
  (or (alist-get canonical-key org-scribe--scene-property-aliases)
      (list canonical-key)))

(defun org-scribe-scene-property-name (canonical-key &optional language)
  "Return the literal property name to write for CANONICAL-KEY.
LANGUAGE defaults to `org-scribe-project-language'."
  (let* ((aliases (org-scribe-scene-property-aliases canonical-key))
         (language (or language (org-scribe-project-language))))
    (or (and (eq language 'es) (nth 1 aliases))
        (car aliases))))

(defun org-scribe-scene-property-get (canonical-key)
  "Return the value of scene property CANONICAL-KEY at point.
Tries every known localized alias for CANONICAL-KEY and returns the
first non-nil value found via `org-entry-get'."
  (cl-some (lambda (prop) (org-entry-get nil prop))
           (org-scribe-scene-property-aliases canonical-key)))

(defun org-scribe-scene-property-set (canonical-key value)
  "Set scene property CANONICAL-KEY to VALUE at point.
Writes to whichever localized alias is already present on the heading;
if none is set yet, writes the alias matching the current project's
language (see `org-scribe-scene-property-name')."
  (let* ((aliases (org-scribe-scene-property-aliases canonical-key))
         (existing (cl-find-if (lambda (prop) (org-entry-get nil prop)) aliases)))
    (org-set-property (or existing (org-scribe-scene-property-name canonical-key)) value)))

;;; Comma-Separated Property Lists

;; Multi-value scene properties (Characters, Locations, Plot threads) are
;; stored as comma-joined lists, either as plain text ("Alex, Sam") or as
;; ID links ("[[id:1][Alex]], [[id:2][Sam]]").  A naive `split-string' on
;; "," breaks whenever an entity's own display name contains a comma (e.g.
;; "Smith, John"), splitting one name into two bogus items.  These two
;; helpers protect embedded commas before splitting.

(defun org-scribe--split-property-list (value)
  "Split VALUE on commas, without splitting inside [[...][...]] links.
VALUE is a multi-value scene property such as Characters or Locations,
already possibly containing ID links, e.g.
\"[[id:1][Smith, John]], [[id:2][Sam]]\".  A comma inside an ID link's
display text (bracket depth > 0) is not treated as an item separator, so a
linked entity whose display name itself contains a comma round-trips
correctly.  Plain-text (unlinked) items are still split on every comma,
since there is no bracket structure there to disambiguate an embedded
comma from a separator — use `org-scribe--split-comma-list-protecting-names'
first if the items are not yet linked.
Returns a list of trimmed, non-empty items."
  (let ((items nil)
        (start 0)
        (depth 0))
    (dotimes (i (length value))
      (let ((c (aref value i)))
        (cond
         ((eq c ?\[) (setq depth (1+ depth)))
         ((eq c ?\]) (setq depth (max 0 (1- depth))))
         ((and (eq c ?,) (zerop depth))
          (push (substring value start i) items)
          (setq start (1+ i))))))
    (push (substring value start) items)
    (delete "" (mapcar #'string-trim (nreverse items)))))

(defun org-scribe--split-comma-list-protecting-names (text known-names)
  "Split TEXT on commas, without splitting inside any name in KNOWN-NAMES.
TEXT is a plain-text, not-yet-linked comma-separated list of entity names.
KNOWN-NAMES is a list of known entity display names (e.g. the names from
`org-scribe--get-all-entities'); any of them that both contains a comma and
occurs verbatim in TEXT has that internal comma protected before
splitting, so a character named e.g. \"Smith, John\" is recognized as one
name instead of being split into \"Smith\" and \"John\" — as long as that
exact name appears in TEXT.  Names are matched longest-first so a name
that is a substring of another comma-bearing name is not partially
protected first.
Returns a list of trimmed, non-empty items."
  (let ((protected text)
        (placeholder (string ?\x01)))
    (dolist (name (sort (cl-remove-if-not (lambda (n) (string-match-p "," n)) known-names)
                        (lambda (a b) (> (length a) (length b)))))
      (setq protected
            (replace-regexp-in-string
             (regexp-quote name)
             (replace-regexp-in-string "," placeholder name t t)
             protected t t)))
    (delete "" (mapcar (lambda (s)
                         (string-trim (replace-regexp-in-string placeholder "," s t t)))
                       (split-string protected "," t)))))

;;; Org Table Cell Escaping

(defun org-scribe--escape-table-cell (text)
  "Escape TEXT so it is safe to interpolate into a |-delimited org table cell.
A literal \"|\" in TEXT would otherwise be read as a column separator,
shifting every following column; Org's own escape for a literal pipe
inside a table cell is the string \"\\vert\", which `org-table-align'
and export both render back as \"|\".  Newlines are flattened to spaces
since a table cell cannot contain one."
  (replace-regexp-in-string
   "\n" " "
   (replace-regexp-in-string "|" "\\vert" text t t)))

;;; Feature Detection

(defvar org-scribe--available-features nil
  "Alist of (FEATURE . AVAILABLE-P) for optional dependencies.")

(defun org-scribe-check-feature (feature)
  "Check if FEATURE is available and cache result.
Uses `require' rather than a bare `featurep' check so that an installed
but not-yet-loaded FEATURE is detected instead of reporting unavailable
just because nothing has loaded it yet.  Only positive results are
cached; a negative result is rechecked on the next call so that a
package installed later in the session (or made loadable via a
load-path change) is picked up without restarting Emacs."
  (let ((cached (assq feature org-scribe--available-features)))
    (if cached
        (cdr cached)
      (let ((available (and (require feature nil t) t)))
        (when available
          (push (cons feature available) org-scribe--available-features))
        available))))

(defmacro org-scribe-when-feature (feature &rest body)
  "Execute BODY if FEATURE is available, otherwise show message."
  (declare (indent 1))
  `(if (org-scribe-check-feature ',feature)
       (progn ,@body)
     (user-error (org-scribe-msg 'error-feature-not-available ',feature))))

;;; Short-story Entity Heading Helper

(defconst org-scribe--section-heading-aliases
  '((characters   . ("Characters" "Personajes"))
    (setting      . ("Setting" "Ambientación" "Ambientacion"))
    (plot-threads . ("Plot Threads" "Hilos de la Trama"))
    (plot-points  . ("The Thirteen Non-Negotiables" "Los trece irrenunciables"))
    (starting-gate . ("Starting Gate" "Puerta de salida")))
  "Canonical section key -> localized level-1 heading aliases.
Mirrors `org-scribe--scene-property-aliases': English and Spanish project
templates use different literal heading text for the same section
(\"Characters\" vs \"Personajes\", etc.), so entity heading predicates
recognize either via `org-scribe--heading-parent-section-p' instead of
hardcoding one literal name.")

(defun org-scribe--heading-parent-section-p (section-key)
  "Return non-nil if the level-1 parent of the heading at point is SECTION-KEY.
SECTION-KEY is a symbol such as \\='characters, \\='setting, or
\\='plot-threads (see `org-scribe--section-heading-aliases').  Used by
entity heading predicates to recognize short-story projects' notes.org
layout, where characters/locations/plot threads are nested as level-2
headings under a level-1 section rather than being top-level headings of
their own, as in novel projects.  Matches any localized alias for that
section, case-insensitively."
  (save-excursion
    (and (org-up-heading-safe)
         (= (org-current-level) 1)
         (let ((heading (org-get-heading t t t t)))
           (cl-some (lambda (alias) (string-equal-ignore-case alias heading))
                    (alist-get section-key org-scribe--section-heading-aliases))))))

;;; Helper Functions

(defun org-scribe-window-perc (pct)
  "Calculate window width as percentage of frame.

PCT should be a float between 0.0 and 1.0 representing the desired
percentage of `frame-width'.  Returns the floor of the calculation
as an integer suitable for window sizing functions.

Example: (org-scribe-window-perc 0.25) with a 200-char frame returns 50."
  (floor (* (frame-width) pct)))

(defun org-scribe-sanitize-filename (title)
  "Sanitize TITLE for safe use as filename component.
Removes path separators and other problematic characters."
  (let ((safe-title (replace-regexp-in-string "[/\\:]" "_" title)))
    (replace-regexp-in-string "^\\.+" "" safe-title)))

(defun org-scribe-validate-directory (directory &optional create)
  "Validate that DIRECTORY exists.
If CREATE is non-nil and directory doesn't exist, ask user to create it.
Returns t if directory exists or was created, nil otherwise."
  (cond
   ((file-directory-p directory) t)
   (create
    (when (yes-or-no-p (org-scribe-msg 'question-create-directory directory))
      (make-directory directory t)
      t))
   (t nil)))

;;; Error Handling Wrapper

(defmacro org-scribe-with-error-handling (name &rest body)
  "Execute BODY with standard error handling for writing functions.
NAME should be a string identifying the function for error messages."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error
      (message "Error in %s: %s" ,name (error-message-string err))
      nil)))

;;; Refile Targets

(defun org-scribe--project-refile-files ()
  "Return every real file in the current project, for `org-refile-targets'.
Derives the list from `org-scribe-project-structure' rather than probing
the filesystem again, keeping only its file-valued keys (`:root' and
`:notes-dir' are directories, not files, so they are excluded) and
dropping any that resolved to nil because that file does not exist in
this project.  Files `org-scribe-project-structure' never resolves at
all — README.org, the writing journal — are excluded for free by using
it as the sole source of truth here, with no name-based filtering
needed.

One file is resolved separately rather than through that plist: a
short-story manuscript (story.org/cuento.org) has no key of its own in
`org-scribe-project-structure' — its `:novel-file' only ever matches
novel.org/novela.org, so a short-story project's own manuscript would
otherwise be silently missing from its own refile targets, the one file
a writer most wants to refile from.  `org-scribe-project-structure' is
left alone rather than widened here: several other modules already key
behavior off `:novel-file' being nil for a short-story project, so
changing what it resolves is a larger, separate change than this
feature calls for."
  (let* ((structure (org-scribe-project-structure))
         (manuscript (or (plist-get structure :novel-file)
                         (org-scribe--find-existing-file
                          (plist-get structure :root) "story.org" "cuento.org"))))
    (delq nil
          (list manuscript
                (plist-get structure :notes-file)
                (plist-get structure :characters-file)
                (plist-get structure :locations-file)
                (plist-get structure :plot-file)
                (plist-get structure :timeline-file)
                (plist-get structure :objects-file)
                (plist-get structure :design-file)
                (plist-get structure :plan-file)))))

(defconst org-scribe--refile-unset 'org-scribe--refile-unset
  "Sentinel distinguishing \"nothing saved yet\" from a saved value of nil.
`org-refile-use-outline-path' defaults to nil, so a plain nil default on
the variables below would be indistinguishable from \"I saved nil\" —
the same trap a naive `Weight' read falls into elsewhere in this
package.  Never compare a saved slot to nil to decide whether it holds
a real value; use `eq' against this sentinel instead.")

(defvar-local org-scribe--refile-saved-targets org-scribe--refile-unset
  "This buffer's `org-refile-targets' from before org-scribe overrode it.
Restored by `org-scribe--refile-disable'.  See `org-scribe--refile-unset'.")

(defvar-local org-scribe--refile-saved-outline-path org-scribe--refile-unset
  "This buffer's `org-refile-use-outline-path' from before org-scribe overrode it.
Restored by `org-scribe--refile-disable'.  See `org-scribe--refile-unset'.")

(defun org-scribe--refile-enabled-p ()
  "Non-nil when this buffer's refile variables are currently org-scribe's."
  (not (eq org-scribe--refile-saved-targets org-scribe--refile-unset)))

(defun org-scribe--refile-enable ()
  "Point this buffer's refile targets at every file in its project.
Saves the buffer's current `org-refile-targets' and
`org-refile-use-outline-path' the first time this runs, so
`org-scribe--refile-disable' can restore exactly what was there before,
then sets both buffer-locally: targets to every real project file with
every heading level offered (`t', Org's own \"all headlines\" spelling —
see `org-scribe--project-refile-files' for the file list itself), and
outline-path display to `file'
so a completion candidate names the file it comes from (a scene in the
manuscript vs. a character in objects/characters.org read identically
by heading text alone). Also turns on Org's own refile cache
(`org-refile-use-cache'), since a project-wide file list is exactly the
case that cache exists for and it is off by default in stock Org."
  (unless (org-scribe--refile-enabled-p)
    (setq org-scribe--refile-saved-targets org-refile-targets
          org-scribe--refile-saved-outline-path org-refile-use-outline-path))
  ;; The cdr here is the target-description slot, not an optional
  ;; keyword-plist: `t' is Org's own spelling for "all headlines,"
  ;; and omitting it (a bare `(org-scribe--project-refile-files)') is
  ;; read by `org-refile-get-targets' as a target description of nil,
  ;; which is not one of its recognized forms and errors as "Bad
  ;; refiling target description."
  (setq-local org-refile-targets '((org-scribe--project-refile-files . t)))
  (setq-local org-refile-use-outline-path 'file)
  (setq org-refile-use-cache t))

(defun org-scribe--refile-disable ()
  "Restore this buffer's refile variables to their pre-org-scribe values.
No-op if `org-scribe--refile-enable' was never called in this buffer."
  (when (org-scribe--refile-enabled-p)
    (setq-local org-refile-targets org-scribe--refile-saved-targets)
    (setq-local org-refile-use-outline-path org-scribe--refile-saved-outline-path)
    (setq org-scribe--refile-saved-targets org-scribe--refile-unset
          org-scribe--refile-saved-outline-path org-scribe--refile-unset)))

(defun org-scribe--refile-maybe-setup ()
  "Set up or tear down project-wide refile targets for this buffer.
Added to `org-scribe-mode-hook', which runs whenever `org-scribe-mode' is
toggled on OR off — both directions matter here, since a buffer where
org-scribe-mode is turned off must not keep pointing `org-refile-targets'
at a now-stale project file list.  Applies only when `org-scribe-mode' is
on, `org-scribe-refile-project-wide' is non-nil, and this buffer is
inside a project org-scribe actually recognizes (`org-scribe-project-type'
is not `unknown') — otherwise any prior override is torn down and the
buffer's own refile configuration is left alone."
  (if (and org-scribe-mode
           (bound-and-true-p org-scribe-refile-project-wide)
           (not (eq (org-scribe-project-type) 'unknown)))
      (org-scribe--refile-enable)
    (org-scribe--refile-disable)))

(add-hook 'org-scribe-mode-hook #'org-scribe--refile-maybe-setup)

(provide 'org-scribe-core)

;;; org-scribe-core.el ends here
