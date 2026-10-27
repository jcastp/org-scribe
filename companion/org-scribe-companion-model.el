;;; org-scribe-companion-model.el --- Visual companion: project to snapshot -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Pure functions that turn a novel project into the snapshot the browser
;; shows (see the companion implementation plan, "Data model").  No
;; networking and no writing: the snapshot is built from the manuscript and
;; design buffers as they are, saved or not.
;;
;; The snapshot is a plist with keyword keys, ready for `json-serialize':
;; arrays are vectors, false is `:false', null is `:null'.  Beware of nil in
;; a plist value: `json-serialize' writes it as {}, so every possibly-empty
;; value goes through `org-scribe-companion--or-null' or a vector.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-id)
(require 'org-scribe-core)
(require 'org-scribe-lang)
(require 'org-scribe-linking-core)

(defvar org-scribe-companion-todo-colors)
(defvar org-scribe-entity-registry)

;;; Scene field kinds

(defconst org-scribe-companion--field-kinds
  '((pov        :kind link-one  :entity character)
    (characters :kind link-many :entity character)
    (plot       :kind link-many :entity plot)
    (plot-point :kind link-many :entity plot-point)
    (location   :kind link-many :entity location))
  "How the browser edits the scene properties that link to an entity.
Decided from the setter the linking module uses for each key:
`org-scribe-set-pov-character' sets one link; `org-scribe--set-scene-entity'
(behind `org-scribe-set-scene-characters', `-locations', `-plot-threads'
and `-plot-points') sets several.  Every key of the language packs'
`:properties' section that is not here is plain text, except those in
`org-scribe-companion--non-scene-fields'.  `tests/test-companion-model.el'
checks that every scene property named by `org-scribe-entity-registry' has
a row.")

(defconst org-scribe-companion--non-scene-fields '(relationships)
  "Language-pack property keys that are not scene fields.
`relationships' is character data, not something a scene carries.")

;;; Small helpers

(defun org-scribe-companion--or-null (value)
  "Return VALUE, or `:null' when it is nil."
  (if (null value) :null value))

(defun org-scribe-companion--bool (value)
  "Return t when VALUE is non-nil, else `:false'."
  (if value t :false))

(defun org-scribe-companion--keyword (string)
  "Return STRING interned as a keyword symbol."
  (intern (concat ":" string)))

;;; Fields and links

(defconst org-scribe-companion--id-link-regexp
  "\\`\\[\\[id:\\([^]]+\\)\\]\\[\\(.*\\)\\]\\]\\'"
  "Match an org ID link with a description; group 1 is the ID, 2 the text.")

(defun org-scribe-companion--parse-links (raw)
  "Split the property value RAW into ID links and plain names.
Returns (LINKS . UNLINKED): LINKS is a list of (:id ID :name NAME) plists
and UNLINKED a list of strings.  Commas inside a link's text do not split
it (see `org-scribe--split-property-list')."
  (let (links unlinked)
    (dolist (item (org-scribe--split-property-list raw))
      (if (string-match org-scribe-companion--id-link-regexp item)
          (push (list :id (match-string 1 item)
                      ;; Org adds a zero-width space after a name ending in "]"
                      :name (string-remove-suffix "\u200b" (match-string 2 item)))
                links)
        (push item unlinked)))
    (cons (nreverse links) (nreverse unlinked))))

(defun org-scribe-companion--props-table ()
  "Return the properties of the heading at point as a hash table.
Keys are the upcased property names (Org matches names case-insensitively),
values the strings.  Reads the drawer directly: `org-entry-properties'
costs about 0.4 ms a heading, which dominated the snapshot.  A repeated
name keeps its first value, as Org does; the `+' append syntax is not
interpreted."
  (let ((table (make-hash-table :test 'equal))
        (range (org-get-property-block)))
    (when range
      (save-excursion
        (goto-char (car range))
        (while (re-search-forward "^[ \t]*:\\([^: \t\n]+\\):[ \t]*\\(.*?\\)[ \t]*$"
                                  (cdr range) t)
          (let ((name (upcase (match-string-no-properties 1))))
            (unless (gethash name table)
              (puthash name (match-string-no-properties 2) table))))))
    table))

(defun org-scribe-companion--prop (props aliases)
  "Return the first non-blank value among ALIASES in the table PROPS, or nil.
ALIASES are upcased property names."
  (cl-some (lambda (alias) (org-string-nw-p (gethash alias props))) aliases))

(defun org-scribe-companion--field-value (props aliases kind)
  "Return the field plist for the scene property ALIASES of KIND, or nil.
PROPS is the table of the scene's properties.  Blank properties are
absent, so they give nil."
  (when-let* ((raw (org-scribe-companion--prop props aliases)))
    (if (eq kind 'text)
        (list :raw raw)
      (let ((parsed (org-scribe-companion--parse-links raw)))
        (append
         (list :raw raw
               :links (vconcat (mapcar (lambda (l)
                                         (list :id (plist-get l :id)
                                               :name (plist-get l :name)))
                                       (car parsed))))
         (when (cdr parsed)
           (list :unlinked (vconcat (cdr parsed)))))))))

(defun org-scribe-companion--field-specs (&optional language)
  "Return the scene fields for LANGUAGE as (KEY KIND ENTITY NAME ALIASES) lists.
ALIASES are every spelling of the property in any registered language,
resolved once here because resolving them per scene was the slow part.
Taken from the `:properties' section of the language pack, so a new
property shows up without a code change."
  (let ((language (or language (org-scribe-project-language))))
    (cl-loop for (key . _) in (plist-get (org-scribe-lang-pack language) :properties)
             unless (memq key org-scribe-companion--non-scene-fields)
             collect (let ((spec (alist-get key org-scribe-companion--field-kinds)))
                       (list key
                             (or (plist-get spec :kind) 'text)
                             (plist-get spec :entity)
                             (org-scribe-lang-property key language)
                             (mapcar #'upcase (org-scribe-scene-property-aliases key)))))))

(defun org-scribe-companion--scene-fields-json (specs)
  "Return the `sceneFields' vector for SPECS (see `--field-specs')."
  (vconcat
   (mapcar (lambda (spec)
             (pcase-let ((`(,key ,kind ,entity ,name ,_aliases) spec))
               (append (list :key (symbol-name key)
                             :name name
                             :kind (symbol-name kind))
                       (when entity (list :entity (symbol-name entity))))))
           specs)))

;;; Walking the manuscript

(defun org-scribe-companion--number (props name)
  "Return the integer value of property NAME (upcased) in table PROPS, or nil."
  (when-let* ((value (org-string-nw-p (gethash name props))))
    (when (string-match-p "\\`[0-9]+\\'" (string-trim value))
      (string-to-number value))))

(defun org-scribe-companion--heading-plist (props)
  "Return the plist common to acts, chapters and scenes at point.
PROPS is the heading's property table.  A heading without an ID
is read-only: the browser cannot address it."
  (let ((id (org-string-nw-p (gethash "ID" props))))
    (list :id (org-scribe-companion--or-null id)
          :title (org-get-heading t t t t)
          :todo (org-scribe-companion--or-null (org-get-todo-state))
          :words (or (org-scribe-companion--number props "WORDCOUNT") 0)
          :objective (org-scribe-companion--or-null
                      (org-scribe-companion--number props "WORD-OBJECTIVE"))
          :readOnly (org-scribe-companion--bool (null id)))))

(defun org-scribe-companion--scene-plist (props specs)
  "Return the scene plist for the heading at point, using field SPECS.
PROPS is the table of the scene's properties."
  (let (fields)
    (dolist (spec specs)
      (pcase-let ((`(,key ,kind ,_entity ,_name ,aliases) spec))
        (when-let* ((value (org-scribe-companion--field-value props aliases kind)))
          (setq fields (append fields
                               (list (org-scribe-companion--keyword (symbol-name key))
                                     value))))))
    (append (org-scribe-companion--heading-plist props)
            (list :fields fields))))   ; nil is {} in JSON, as wanted here

(defun org-scribe-companion--walk-manuscript (specs)
  "Return the acts of the current buffer as a list of plists, in order.
Acts are level-1 headings, chapters and scenes the levels of
`org-scribe-chapter-level' and `org-scribe-scene-level'.  A heading that
has or inherits `noexport' (the novel's apparatus) is skipped with
everything under it.  A chapter outside any act, or a scene outside any
chapter, gets an implicit parent with a null ID, so nothing is dropped.
Reads the whole buffer, ignoring any narrowing.  SPECS are the scene
field specs."
  (let ((chapter-level (org-scribe-chapter-level))
        (scene-level (org-scribe-scene-level))
        acts act chapter)
    (cl-labels
        ((ensure-act ()
           (unless act
             (setq act (list :id :null :title "" :todo :null :words 0
                             :objective :null :readOnly t :implicit t
                             :chapters nil))
             (push act acts)))
         (ensure-chapter ()
           (ensure-act)
           (unless chapter
             (setq chapter (list :id :null :title "" :todo :null :words 0
                                 :objective :null :readOnly t :implicit t
                                 :scenes nil))
             (plist-put act :chapters (cons chapter (plist-get act :chapters))))))
      (org-with-wide-buffer
       (goto-char (point-min))
       (org-map-entries
        (lambda ()
          (let ((level (org-current-level))
                (props (org-scribe-companion--props-table)))
            (cond
             ((= level 1)
              (setq act nil chapter nil)
              (unless (member "noexport" (org-get-tags))
                (setq act (append (org-scribe-companion--heading-plist props)
                                  (list :chapters nil)))
                (push act acts)))
             ((member "noexport" (org-get-tags)))
             ((eql level chapter-level)
              (ensure-act)
              (setq chapter (append (org-scribe-companion--heading-plist props)
                                    (list :scenes nil)))
              (plist-put act :chapters (cons chapter (plist-get act :chapters))))
             ((= level scene-level)
              (ensure-chapter)
              (plist-put chapter :scenes
                         (cons (org-scribe-companion--scene-plist props specs)
                               (plist-get chapter :scenes)))))))
        nil 'file)))
    ;; Lists were built back to front; fix the order and make vectors.
    (mapcar
     (lambda (a)
       (plist-put
        a :chapters
        (vconcat
         (mapcar (lambda (c)
                   (plist-put c :scenes (vconcat (nreverse (plist-get c :scenes)))))
                 (nreverse (plist-get a :chapters))))))
     (nreverse acts))))

;;; Entities

(defun org-scribe-companion--has-content-p ()
  "Return non-nil when the heading at point has a line of real content.
A line counts when it is neither blank nor an Org comment: the templates
give every plot point a `#' hint, so a non-empty body proves nothing."
  (let ((body (save-excursion
                (org-end-of-meta-data t)
                (buffer-substring-no-properties
                 (point) (save-excursion (org-end-of-subtree t t) (point))))))
    (cl-some (lambda (line)
               (let ((l (string-trim line)))
                 (and (not (string-empty-p l)) (not (string-prefix-p "#" l)))))
             (split-string body "\n"))))

;; Every entity heading predicate in the linking modules asks "is my parent
;; heading this section?" (`org-scribe--heading-parent-section-p'), and that
;; question costs a language-pack lookup per heading.  Knowing which section
;; each kind sits under lets the walk below call the real predicate only for
;; headings whose parent is such a section.  The predicate still decides;
;; this only skips headings it would certainly refuse.
(defconst org-scribe-companion--entity-sections
  '((character . characters)
    (location . setting)
    (plot . plot-threads)
    (plot-point . plot-points))
  "Entity kind -> the `:headings' key of the section its headings sit under.")

(defun org-scribe-companion--entity-plist (symbol)
  "Return the plist for the entity of kind SYMBOL at point."
  (let* ((weight (or (org-scribe--parse-weight (org-entry-get nil "Weight")) 999.0))
         (entity (list :id (org-scribe-companion--or-null (org-id-get))
                       :name (org-scribe--entity-name-at-point)
                       :weight weight
                       :hidden (org-scribe-companion--bool
                                (org-scribe--entity-hidden-weight-p weight)))))
    (when (eq symbol 'character)
      (when-let* ((role (org-string-nw-p (org-entry-get nil "Role"))))
        (setq entity (append entity (list :role role)))))
    (when (eq symbol 'plot-point)
      (setq entity (append entity
                           (list :hasContent (org-scribe-companion--bool
                                              (org-scribe-companion--has-content-p))))))
    entity))

(defun org-scribe-companion--entities ()
  "Return the `entities' plist: one vector per registered entity kind.
One pass over each entity file.  Each entity is (:id :name :weight
:hidden), plus `:role' for characters and `:hasContent' for plot points.
A negative weight sets `:hidden'; the entity stays in the list so the
browser can show that it is hidden.  A blank or non-numeric Weight is the
unweighted default 999.0 and is not hidden."
  (let* ((found (mapcar (lambda (entry) (cons (car entry) nil)) org-scribe-entity-registry))
         ;; The file of each kind, resolved once: `:file-fn' walks the
         ;; project structure, far too slow to call per heading.
         (kind-files (mapcar (lambda (entry)
                               (cons (car entry) (funcall (plist-get (cdr entry) :file-fn))))
                             org-scribe-entity-registry))
         (files (delete-dups (delq nil (mapcar #'cdr kind-files))))
         ;; Likewise the spellings of each section, in every language.
         (section-names (mapcar (lambda (pair)
                                  (cons (car pair) (org-scribe-lang-all :headings (cdr pair))))
                                org-scribe-companion--entity-sections)))
    (dolist (file files)
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (let (parents)               ; stack of (LEVEL . TITLE)
             (org-map-entries
              (lambda ()
                (let ((level (org-current-level)))
                  (while (and parents (>= (caar parents) level)) (pop parents))
                  (let ((parent (cdar parents)))
                    (dolist (entry org-scribe-entity-registry)
                      (let* ((symbol (car entry))
                             (config (cdr entry))
                             (section (alist-get symbol org-scribe-companion--entity-sections)))
                        (when (and (equal file (alist-get symbol kind-files))
                                   (or (null section)
                                       (and parent
                                            (cl-some (lambda (name)
                                                       (string-equal-ignore-case name parent))
                                                     (alist-get symbol section-names))))
                                   (funcall (plist-get config :heading-predicate)))
                          (push (org-scribe-companion--entity-plist symbol)
                                (alist-get symbol found))))))
                  (push (cons level (org-get-heading t t t t)) parents)))
              nil 'file))))))
    (cl-loop for (symbol . list) in (reverse found)
             append (list (org-scribe-companion--keyword (symbol-name symbol))
                          (vconcat (nreverse list))))))

;;; Revision and cache

(defun org-scribe-companion--buffer-revision (file)
  "Return a revision string for FILE: its buffer's tick, or its mtime."
  (if-let* ((buffer (and file (find-buffer-visiting file))))
      (with-current-buffer buffer
        (format "%d%s" (buffer-chars-modified-tick) (if (buffer-modified-p) "*" "")))
    (if (and file (file-exists-p file))
        (format "f%d" (floor (float-time (file-attribute-modification-time
                                          (file-attributes file)))))
      "0")))

(defun org-scribe-companion--revisions (structure)
  "Return (MANUSCRIPT . DESIGN) revision strings for STRUCTURE."
  (cons (org-scribe-companion--buffer-revision (plist-get structure :manuscript-file))
        (org-scribe-companion--buffer-revision (plist-get structure :design-file))))

(defun org-scribe-companion--format-revision (revisions)
  "Return the revision string for REVISIONS, a (MANUSCRIPT . DESIGN) pair."
  (format "m%s-d%s" (car revisions) (cdr revisions)))

(defun org-scribe-companion--revision-string (root)
  "Return the current revision string of the project at ROOT, without a snapshot.
The same string the snapshot carries in `:revision', for a fraction of
the cost."
  (let ((default-directory (file-name-as-directory (expand-file-name root))))
    (org-scribe-companion--format-revision
     (org-scribe-companion--revisions (org-scribe-project-structure)))))

(defvar org-scribe-companion--snapshot-cache nil
  "Alist of (ROOT . PLIST): what was last computed for each project.
PLIST holds `:m' (manuscript revision), `:acts' and `:todo' for the
manuscript half, `:d' (design revision) and `:entities' for the design
half.  The halves are cached apart because the manuscript changes much
more often (every drag), and walking the design notebook is the costlier
half.  Language and todo colours are not part of the revision; clear the
cache with `org-scribe-companion--clear-cache' after changing them.")

(defun org-scribe-companion--clear-cache ()
  "Forget all cached snapshots."
  (setq org-scribe-companion--snapshot-cache nil))

;;; Snapshot

(defun org-scribe-companion--buffer-info (file)
  "Return the `buffers' entry for FILE."
  (let ((buffer (and file (find-buffer-visiting file))))
    (list :file (if file (file-name-nondirectory file) :null)
          :modified (org-scribe-companion--bool
                     (and buffer (buffer-modified-p buffer))))))

(defun org-scribe-companion--todo-info ()
  "Return the `todo' entry from the current (manuscript) buffer."
  (list :keywords (vconcat org-todo-keywords-1)
        :done (vconcat org-done-keywords)
        :colors (cl-loop for (keyword . color) in org-scribe-companion-todo-colors
                         append (list (org-scribe-companion--keyword keyword) color))))

;;; Storyline data

(defvar org-scribe-plot-thread-gap-threshold)

(defun org-scribe-companion--scene-list (acts)
  "Return every scene of ACTS (the manuscript walk), in order."
  (cl-loop for act in (append acts nil)
           append (cl-loop for chapter in (append (plist-get act :chapters) nil)
                           append (append (plist-get chapter :scenes) nil))))

(defun org-scribe-companion--field-ids (scene key)
  "Return the entity IDs in field KEY (a keyword) of SCENE."
  (mapcar (lambda (link) (plist-get link :id))
          (append (plist-get (plist-get (plist-get scene :fields) key) :links) nil)))

(defun org-scribe-companion--plot-gaps (scenes)
  "Return a plist of plot thread ID to its largest gap over SCENES.
Same rule as `org-scribe--calculate-thread-gap': the scenes counted are
those carrying a Plot property, and the gap is the longest run of them
between two appearances of the thread.  Threads in fewer than two scenes
get 0."
  (let* ((with-plot (cl-remove-if-not
                     (lambda (scene) (plist-get (plist-get (plist-get scene :fields) :plot) :raw))
                     scenes))
         (positions (make-hash-table :test 'equal))
         result)
    (cl-loop for scene in with-plot for index from 0
             do (dolist (id (org-scribe-companion--field-ids scene :plot))
                  (push index (gethash id positions))))
    (maphash (lambda (id found)
               (let ((found (nreverse found)) (gap 0))
                 (cl-loop for (a b) on found while b
                          do (setq gap (max gap (- b a 1))))
                 (setq result (plist-put result (org-scribe-companion--keyword id) gap))))
             positions)
    result))

(defun org-scribe-companion--orphans (scenes entities)
  "Return ENTITIES' IDs, per kind, that no scene in SCENES refers to.
Kinds: character, location, plot-point (the ones `org-scribe-health' checks)."
  (let ((used (make-hash-table :test 'equal)))
    (dolist (scene scenes)
      (dolist (key '(:pov :characters :location :plot :plot-point))
        (dolist (id (org-scribe-companion--field-ids scene key))
          (puthash id t used))))
    (cl-loop for kind in '(:character :location :plot-point)
             append (list kind
                          (vconcat
                           (cl-loop for entity across (or (plist-get entities kind) [])
                                    for id = (plist-get entity :id)
                                    when (and (stringp id) (not (gethash id used)))
                                    collect id))))))

(defun org-scribe-companion--storyline (acts entities)
  "Return the `storyline' entry for ACTS and ENTITIES."
  (require 'org-scribe-plot-links)
  (let ((scenes (org-scribe-companion--scene-list acts)))
    (list :gapThreshold org-scribe-plot-thread-gap-threshold
          :plotGaps (org-scribe-companion--plot-gaps scenes)
          :orphans (org-scribe-companion--orphans scenes entities))))

(defun org-scribe-companion--snapshot (&optional root)
  "Return the snapshot plist of the project at ROOT (default: the current).
Cached per project and per half (see `org-scribe-companion--snapshot-cache'),
so asking again without an edit costs a revision check only."
  (let* ((root (file-name-as-directory
                (expand-file-name (or root (org-scribe-project-root)))))
         (default-directory root)
         (structure (org-scribe-project-structure))
         (manuscript (plist-get structure :manuscript-file))
         (design (plist-get structure :design-file))
         (language (org-scribe-project-language)))
    (unless (and manuscript (file-exists-p manuscript))
      (user-error "%s" (org-scribe-msg 'companion-error-no-manuscript)))
    ;; Visit the files first, so the revisions see their buffers.
    (dolist (file (list manuscript design))
      (when (and file (file-exists-p file)) (find-file-noselect file)))
    (let* ((revisions (org-scribe-companion--revisions structure))
           (specs (org-scribe-companion--field-specs language))
           (cache (alist-get root org-scribe-companion--snapshot-cache nil nil #'equal)))
      (unless (equal (plist-get cache :m) (car revisions))
        (with-current-buffer (find-file-noselect manuscript)
          (setq cache (plist-put cache :acts (org-scribe-companion--walk-manuscript specs)))
          (setq cache (plist-put cache :todo (org-scribe-companion--todo-info))))
        (setq cache (plist-put cache :m (car revisions))))
      (let ((redo-entities (not (equal (plist-get cache :d) (cdr revisions)))))
        (when redo-entities
          (setq cache (plist-put cache :entities (org-scribe-companion--entities)))
          (setq cache (plist-put cache :d (cdr revisions))))
        (when (or redo-entities (not (equal (plist-get cache :sm) (car revisions))))
          (setq cache (plist-put cache :storyline
                                 (org-scribe-companion--storyline
                                  (plist-get cache :acts) (plist-get cache :entities))))
          (setq cache (plist-put cache :sm (car revisions)))))
      (setf (alist-get root org-scribe-companion--snapshot-cache nil nil #'equal) cache)
      (list :revision (org-scribe-companion--format-revision revisions)
            :language (symbol-name language)
            :project (list :title (org-scribe-companion--or-null
                                   (org-scribe-project-title root))
                           :root (directory-file-name root)
                           :type (symbol-name (org-scribe-project-type)))
            :buffers (list :manuscript (org-scribe-companion--buffer-info manuscript)
                           :design (org-scribe-companion--buffer-info design))
            :todo (plist-get cache :todo)
            :sceneFields (org-scribe-companion--scene-fields-json specs)
            :acts (vconcat (plist-get cache :acts))
            :entities (plist-get cache :entities)
            :storyline (plist-get cache :storyline)))))

;;; Strings for the browser

(defun org-scribe-companion--strings (root)
  "Return the `companion-*' messages of ROOT's language as a plist.
Keys are keywords named like the message keys.  A message missing in the
project's language falls back to English.  The values are the raw
templates: the browser fills in any `%s'."
  (let* ((default-directory (file-name-as-directory (expand-file-name root)))
         (language (org-scribe-project-language))
         (english (plist-get (org-scribe-lang-pack 'en) :messages))
         (own (plist-get (org-scribe-lang-pack language) :messages)))
    (cl-loop for (key . text) in english
             when (string-prefix-p "companion-" (symbol-name key))
             append (list (org-scribe-companion--keyword (symbol-name key))
                          (or (alist-get key own) text)))))

(defun org-scribe-companion--snapshot-json (&optional root)
  "Return the snapshot of the project at ROOT as a JSON string."
  (json-serialize (org-scribe-companion--snapshot root)))

(provide 'org-scribe-companion-model)

;;; org-scribe-companion-model.el ends here
