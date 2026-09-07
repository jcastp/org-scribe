;;; org-scribe-core.el --- Core utilities for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core utility functions for the org-scribe package.
;; Includes project detection, feature checking, and common helpers.

;;; Code:

(require 'cl-lib)
(require 'org)
;; `org-refile-cache-clear' (used by `org-scribe--refile-invalidate-cache'
;; below) is not autoloaded and `(require 'org)' alone does not pull it in
;; -- confirmed empirically, not assumed.
(require 'org-refile)
(require 'project)
(require 'org-scribe-messages)
;; Several tables below (`org-scribe--scene-property-aliases' and
;; friends) are derived from the registered language packs at THIS file's
;; own load time, so the packs must be loaded -- and self-registered,
;; which each does on `require' -- before that point.  Required directly
;; here, not left to `org-scribe.el', because several test files
;; `require' this file standalone without ever loading that one.
(require 'org-scribe-lang)
(require 'org-scribe-lang-en)
(require 'org-scribe-lang-es)

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

(defvar org-scribe--project-language-cache nil
  "Alist of (PROJECT-ROOT . LANGUAGE) for caching project language detection.
Mirrors `org-scribe--project-type-cache' exactly -- same key
normalization (`org-scribe--normalize-project-root'), same invalidation
(`org-scribe-project-type-cache-clear').  A parallel cache rather than
folding language into the type cache's own value, so every existing
reader of `org-scribe--project-type-cache' keeps its current shape.
Worth caching because `org-scribe-project-language' re-reads the marker
file on every call, and `org-scribe-project-structure' is about to call
it once per resolved file instead of never.")

(defun org-scribe--normalize-project-root (root)
  "Return a canonical form of ROOT for use as a project-type cache key.
Resolves symlinks and normalizes trailing slashes so that different
spellings of the same directory map to the same cache entry."
  (file-truename (file-name-as-directory (expand-file-name root))))

(defun org-scribe-project-type-cache-clear (&optional root)
  "Invalidate the project-type and project-language caches.
With ROOT, remove only the entry for that project root (its type and
language will be re-detected on next use).  With no argument, clear
both caches entirely.

Call this after anything that can change what `org-scribe-project-type'
or `org-scribe-project-language' would detect for a project already in
the cache: creating project marker files/structure in an existing
directory, or switching to a different project whose root was
previously misdetected."
  (if root
      (let ((key (org-scribe--normalize-project-root root)))
        (setq org-scribe--project-type-cache
              (assoc-delete-all key org-scribe--project-type-cache #'string=))
        (setq org-scribe--project-language-cache
              (assoc-delete-all key org-scribe--project-language-cache #'string=)))
    (setq org-scribe--project-type-cache nil)
    (setq org-scribe--project-language-cache nil)))

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

              ;; Strategy 2: Check for a localized "objects" directory in
              ;; any registered language (novel indicator).  Reads every
              ;; registered pack, not just the two shipped ones, and not
              ;; scoped to any one language -- there is no marker line to
              ;; read a language from yet at this point.
              ((cl-some (lambda (name) (file-directory-p (expand-file-name name root)))
                        (org-scribe-lang-all :dirs 'objects))
               'novel)

              ;; Strategy 3: Check for a localized short-story manuscript
              ;; (story.org, cuento.org, ...) in any registered language.
              ((cl-some (lambda (name) (file-exists-p (expand-file-name name root)))
                        (org-scribe-lang-all :files 'manuscript-short))
               'short-story)

              ;; Strategy 4: Check for a localized novel manuscript
              ;; (novel.org, novela.org, ...) in any registered language.
              ((cl-some (lambda (name) (file-exists-p (expand-file-name name root)))
                        (org-scribe-lang-all :files 'manuscript-novel))
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

;;; Plotting Method

(defconst org-scribe--methods
  '((sistema . (:overlay nil))
    (helice  . (:overlay "helice"))
    (matriz  . (:overlay "matriz")))
  "Novel plotting methods known to org-scribe, keyed by their canonical
symbol (also the literal value written to the \"# Method:\" marker line).
Each value is a plist:

  :overlay    - subdirectory name under `org-scribe-templates/methods/'
                holding this method's design-file overlay, or nil for
                `sistema', which ships no overlay (its design file lives
                in the base `novel-en'/`novel-es' template sets and is
                never modified by another method's presence).

Creation-prompt labels used to live here too, as `:label-en'/`:label-es'
plist keys; they moved to each registered language pack's own
`:method-labels' section (see `lang/org-scribe-lang.el',
`org-scribe--read-method' in `templates/org-scribe-project.el') so that
a third language's labels do not require touching this table -- this
one is now exactly the symbol + overlay-directory table it should
always have been.  Adding a method still means adding one entry here
plus its overlay templates, plus a `:method-labels' entry in every
registered pack.")

(defun org-scribe-project-method (&optional root)
  "Return the plotting method recorded for ROOT's project.
Returns \\='sistema, \\='helice, or \\='matriz, read from the \"# Method:\"
line of ROOT's (default: the current project's) .org-scribe-project
marker file.  Returns \\='sistema, not nil, when ROOT is nil and no
project can be found either — \"not in a project\" is no more ambiguous
about which method applies than a project with no \"# Method:\" line at
all (see below), so this does not fall through to
`org-scribe--project-marker-get' with a nil ROOT, which would instead
resolve \".org-scribe-project\" against `default-directory' and could read
whatever marker file happens to sit there, project or not.

Unlike `org-scribe-planner-gate', there is no undecided state: every
project created before this feature existed was written under the
Sistema, and a project with no \"# Method:\" line at all is not
ambiguous about which method it uses — it is a Sistema project.  So a
missing or unrecognized marker value returns \\='sistema rather than
nil, which is what keeps every project created to date behaving
exactly as it did before this feature existed.

The value is matched case- and whitespace-insensitively, the same
tolerance `org-scribe-planner-gate' applies to its own marker values, so
a hand-edited \"# Method: Helice\" or \"# Method:  helice \" line still
resolves.  Matching uses `intern-soft', not `intern': every symbol
`org-scribe--methods' actually names (\\='sistema, \\='helice, \\='matriz)
is already interned from the literal quoted symbols in that table's own
definition, so `intern-soft' finds any of them exactly as `intern' would
-- but for a typo or a value that names no known method,
`intern-soft' returns nil (falling through to the \\='sistema default
below) instead of interning a fresh, permanent symbol into the obarray
for a value nothing will ever match again.

This function lives in `org-scribe-core.el', not the templates module,
for the same reason `org-scribe-planner-gate' does: a caller that must
not force-load the templates module, or force-load the (lazily
autoloaded) planner in that function's own case, can still check either
marker first."
  (let ((root (or root (org-scribe-project-root))))
    (if (null root)
        'sistema
      (let* ((value (org-scribe--project-marker-get root "Method"))
             (normalized (and value (intern-soft (downcase (string-trim value))))))
        (if (assq normalized org-scribe--methods)
            normalized
          'sistema)))))

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

(defun org-scribe--resolve (root concept)
  "Return ROOT's file for CONCEPT, trying the project language first.
CONCEPT is a key of the `:files' section of `lang/org-scribe-lang.el',
such as `characters' or `design'.  Falls back to every other registered
language's spelling (via `org-scribe-lang-all'), so a file copied in
from a project in another language still resolves.  Returns nil when
no candidate exists, or when ROOT is nil."
  (when root
    (cl-loop for name in (org-scribe-lang-all :files concept)
             for full = (expand-file-name name root)
             when (file-exists-p full) return full)))

(defun org-scribe--resolve-dir (root concept)
  "Return ROOT's directory for CONCEPT, trying the project language first.
The directory-valued sibling of `org-scribe--resolve': CONCEPT is a key
of the `:dirs' section of `lang/org-scribe-lang.el', such as `notes'."
  (when root
    (cl-loop for name in (org-scribe-lang-all :dirs concept)
             for full = (expand-file-name name root)
             when (file-directory-p full) return full)))

(defun org-scribe--resolve-manuscript (root type)
  "Return ROOT's manuscript file for project TYPE.
TYPE is `novel', `short-story', or `unknown' (see
`org-scribe-project-type').  For `unknown', tries both manuscript
concepts -- novel first, then short-story -- since the type could not
be determined from the project's marker or structure and either is
possible."
  (pcase type
    ('short-story (org-scribe--resolve root 'manuscript-short))
    ('novel (org-scribe--resolve root 'manuscript-novel))
    (_ (or (org-scribe--resolve root 'manuscript-novel)
           (org-scribe--resolve root 'manuscript-short)))))

(defun org-scribe-project-structure ()
  "Detect project structure and return layout information.
Returns plist with:
  :root          - project root directory
  :manuscript-file - the project's manuscript file (see
                   `org-scribe--resolve-manuscript')
  :novel-file    - deprecated synonym of `:manuscript-file', kept only so
                   existing call sites keep working.  Despite the name it
                   no longer means \"novel\" specifically -- it holds the
                   same value as `:manuscript-file' for both project
                   types.  New code should read `:manuscript-file'.
  :notes-dir     - notes directory
  :notes-file    - notes file, either shape (a novel's notes/ subdirectory
                   file, or a short story's root-level file)
  :characters-file - characters file
  :locations-file  - locations file
  :plot-file       - plot file
  :timeline-file   - timeline file
  :objects-file    - objects file
  :design-file     - method design file, or nil
  :plan-file       - writing plan file (in the project root), or nil

Every file name is resolved from the registered language packs (see
`org-scribe--resolve'); all file/directory values are nil if the path
does not exist."
  (let* ((root (org-scribe-project-root))
         (type (org-scribe-project-type))
         (manuscript (org-scribe--resolve-manuscript root type)))
    (list :root root
          :manuscript-file manuscript
          :novel-file manuscript
          :notes-dir (org-scribe--resolve-dir root 'notes)
          ;; The novel-subdir shape (notes/notes.org, notas/notas.org) is
          ;; tried before the short-story root-level shape (notes.org,
          ;; notas.org), matching the fixed order the two used to be
          ;; hand-listed in.
          :notes-file (or (org-scribe--resolve root 'notes-novel)
                           (org-scribe--resolve root 'notes-short))
          :characters-file (org-scribe--resolve root 'characters)
          :locations-file (org-scribe--resolve root 'locations)
          :plot-file (org-scribe--resolve root 'plot)
          :timeline-file (org-scribe--resolve root 'timeline)
          :objects-file (org-scribe--resolve root 'objects)
          :design-file (org-scribe--resolve root 'design)
          :plan-file (org-scribe--resolve root 'plan))))

;;; Outline Levels (Chapter / Scene)
;;
;; Which outline level means what differs by project type:
;;
;;   novel:        * Act   ** Chapter   *** Scene
;;   short story:  * Story Content   ** Scene
;;
;; `:chapter' is the level whose headings survive into compiled output as
;; visible headings; `:scene' is the level at and below which headings go
;; silent and become breaks.  `:scene-tag' is the tag scene headings
;; carry (used to build an `org-map-entries' match string), or nil for a
;; project type whose scenes carry no such tag.  Anything above
;; `:chapter' -- or above `:scene' when `:chapter' is nil -- is a
;; container: its heading is dropped and its children are processed in
;; order.  Dropping acts from compiled output is deliberate: acts in this
;; method are a planning structure, not a reading one.
;;
;; This table was originally private to `export/org-scribe-compile.el',
;; the only module that treated the level as anything but a hardcoded 3.
;; It lives here instead because several other modules -- word counting,
;; the entity linking core, the plot-thread report, the project health
;; report -- all separately hardcoded the same "scene is level 3"
;; assumption, which is simply wrong for a short-story project (whose
;; scenes are level 2).  One table, read by every one of them, is what
;; keeps that assumption from drifting out of step again.
;;
;; `short-story' carries `:scene-tag' "ignore", the same as a novel's --
;; the shipped short-story manuscript templates (story.org.template /
;; cuento.org.template) tag their scenes :ignore: too, matching a
;; novel's.  This used to be nil: the templates originally shipped
;; untagged scenes, and setting :scene-tag to "ignore" here before they
;; did made every scene-level function in the package silently find zero
;; scenes in every short story -- confirmed against a real project, not
;; assumed.  The table and the templates were changed together, in the
;; same commit, for exactly that reason: the two are one atomic decision,
;; not two independent ones.  If a project type is ever added whose
;; scenes are genuinely untagged, give that entry `:scene-tag' nil --
;; `org-scribe-scene-match' already falls back to an explicit
;; :noexport:-exclusion match for that case (see its docstring) -- rather
;; than leaving this one's value ambiguous between "no tag" and "not
;; decided yet."

(defconst org-scribe--project-levels
  '((novel       . (:chapter 2 :scene 3 :scene-tag "ignore"))
    (short-story . (:chapter nil :scene 2 :scene-tag "ignore")))
  "Per project type, the outline levels that carry chapters and scenes.
See the commentary above this constant for what each field means.")

(defun org-scribe-project-levels (&optional type)
  "Return the chapter/scene level plist for TYPE.
TYPE defaults to the current project's `org-scribe-project-type'.  Falls
back to the `novel' entry for an unrecognized TYPE (e.g. `unknown'),
since every property org-scribe reads via this table (scene level,
chapter level, scene tag) is well-defined for a novel and a project
whose type cannot be determined is not more likely to be a short story
than a novel."
  (let ((type (or type (org-scribe-project-type))))
    (or (alist-get type org-scribe--project-levels)
        (alist-get 'novel org-scribe--project-levels))))

(defun org-scribe-scene-level (&optional type)
  "Return the outline level of a scene heading for TYPE.
See `org-scribe-project-levels'."
  (plist-get (org-scribe-project-levels type) :scene))

(defun org-scribe-chapter-level (&optional type)
  "Return the outline level of a chapter heading for TYPE, or nil.
Nil means TYPE has no chapter level at all (a short story), not that it
could not be determined.  See `org-scribe-project-levels'."
  (plist-get (org-scribe-project-levels type) :chapter))

(defun org-scribe-scene-match (&optional type)
  "Return an `org-map-entries' MATCH string selecting TYPE's scenes.
When TYPE's scene level carries a tag (see `org-scribe-project-levels'),
the match is \"LEVEL=<scene>+<tag>\".  When it does not, the match is
\"LEVEL=<scene>-noexport\" instead of a bare \"LEVEL=<scene>\" -- the
`-noexport' exclusion is load-bearing here, not decorative: an untagged
project's apparatus headings (e.g. a short story's \"Synopsis\" or \"Word
Count Tracking\" under a :noexport: wrapper) sit at the very same level
as its scenes, and only the :noexport: tag -- inherited by Org's default
tag inheritance -- tells them apart from a real scene."
  (let* ((levels (org-scribe-project-levels type))
         (level (plist-get levels :scene))
         (tag (plist-get levels :scene-tag)))
    (if tag
        (format "LEVEL=%d+%s" level tag)
      (format "LEVEL=%d-noexport" level))))

;;; Language-Pack-Derived Alias Tables
;;
;; Scene metadata properties (PoV, Characters, Plot, ...) and level-1
;; section headings (Characters, Setting, ...) are stored as literal Org
;; text.  English project templates and Spanish project templates use
;; different literal spellings for the same logical property or section
;; (e.g. "Characters" vs "Personajes"), so every reader/writer of them
;; must go through an alias table instead of hardcoding one literal name.
;;
;; That data now lives in the registered language packs (see
;; `lang/org-scribe-lang.el', `lang/org-scribe-lang-en.el',
;; `lang/org-scribe-lang-es.el'), not here.  The two tables below are
;; rebuilt from the packs at load time via `org-scribe--pack-derived-alist'
;; rather than hand-authored, purely to keep a handful of direct readers
;; -- `capture/org-scribe-capture.el', a couple of tests that inspect the
;; raw shape -- working unmodified; every reader that can instead ask a
;; single question of the live registry (`org-scribe--heading-parent-section-p'
;; below, `org-scribe-scene-property-aliases') does that instead, which is
;; also what makes those readers see a language registered after this file
;; has already loaded.

(defun org-scribe--pack-derived-alist (section &optional variants-section)
  "Rebuild a legacy-shaped alist (KEY . ALIASES) from every registered
language pack's SECTION (and, if given, VARIANTS-SECTION), in pack
REGISTRATION order -- i.e. always English-then-Spanish today, regardless
of the current project's language.  Unlike `org-scribe-lang-all', this
never looks at `org-scribe-project-language': it exists solely to
reconstruct, once at load time, the exact shape and order the
hand-authored bilingual alias tables had before their data moved into
the language packs, for the few callers that still read that shape
directly with `alist-get' instead of going through an accessor.

ALIASES is deduplicated (first occurrence kept), so a key whose only
known spelling is identical in every language -- `pov', written \"PoV\"
in both shipped packs -- still comes back as a single-element list,
matching what the hand-authored table always had."
  (let (keys)
    (dolist (lang (org-scribe-languages))
      (dolist (row (plist-get (org-scribe-lang-pack lang) section))
        (unless (memq (car row) keys) (push (car row) keys))))
    (setq keys (nreverse keys))
    (mapcar
     (lambda (key)
       (cons key
             (delete-dups
              (append
               (delq nil
                     (mapcar (lambda (lang)
                               (alist-get key (plist-get (org-scribe-lang-pack lang) section)))
                             (org-scribe-languages)))
               (when variants-section
                 (apply #'append
                        (mapcar (lambda (lang)
                                  (alist-get key (plist-get (org-scribe-lang-pack lang) variants-section)))
                                (org-scribe-languages))))))))
     keys)))

(defconst org-scribe--scene-property-aliases
  (org-scribe--pack-derived-alist :properties)
  "Canonical scene property key -> localized property name aliases.
Each value lists every literal Org property name known to be used for
that logical property, English first, then Spanish.  Derived from the
registered language packs; see `org-scribe--pack-derived-alist'.

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
;; "design".  Without recognizing this, that marker would file under the
;; catch-all "other" bucket instead of the "design" section it was meant
;; for.  Each pack's `:edit-categories' section is deliberately small in
;; practice: most categories ("plot", "scene", "character", "prose") are
;; used as-is by every template regardless of language, and only
;; "design" currently has a localized spelling in the templates.

(defun org-scribe--edit-category-canonical-keys ()
  "Return every canonical *EDIT* category string known to any registered
language pack's `:edit-categories' section, in pack registration order.
This mirrors the *value* `org-scribe-edit-categories' (config.el) is
expected to hold, without depending on that defcustom: config.el loads
after this file, and `org-scribe-edit-category-canonical' must work for
callers -- including a few tests -- that load this file standalone."
  (let (keys)
    (dolist (lang (org-scribe-languages))
      (dolist (row (plist-get (org-scribe-lang-pack lang) :edit-categories))
        (let ((name (symbol-name (car row))))
          (unless (member name keys) (push name keys)))))
    (nreverse keys)))

(defun org-scribe-edit-category-canonical (category)
  "Return the canonical spelling of CATEGORY, or CATEGORY unchanged.
Checks CATEGORY, case-insensitively, against every registered language
pack's spelling of every known canonical category (see
`org-scribe--edit-category-canonical-keys' and the `:edit-categories'
section of `lang/org-scribe-lang.el').  Returns CATEGORY as given when
it matches nothing, so callers can pass an unrecognized or
already-canonical category safely."
  (or (cl-find-if
       (lambda (canonical)
         (cl-some (lambda (spelling) (string-equal-ignore-case spelling category))
                  (org-scribe-lang-all :edit-categories (intern canonical))))
       (org-scribe--edit-category-canonical-keys))
      category))

(defun org-scribe-project-language ()
  "Return the language symbol (\\='en or \\='es) for the current project.
Reads the \"# Language:\" line from the project's .org-scribe-project
marker file.  Falls back to `org-scribe-template-language' (or \\='en
if that is unbound) when no marker file or line is found.

Cached per project root in `org-scribe--project-language-cache' --
`org-scribe-project-structure' now calls this once per resolved file
instead of never, so re-reading the marker file on every call would
otherwise turn a single call into several stats and a file read.
Invalidate with `org-scribe-project-type-cache-clear' after anything
that changes a project's \"# Language:\" line."
  (let* ((root (org-scribe-project-root))
         (cache-key (org-scribe--normalize-project-root root))
         (cached (alist-get cache-key org-scribe--project-language-cache nil nil #'string=)))
    (or cached
        (let ((language
               (let ((lang (org-scribe--project-marker-get root "Language")))
                 (or (cond ((equal lang "es") 'es)
                           ((equal lang "en") 'en))
                     (and (boundp 'org-scribe-template-language)
                          (default-value 'org-scribe-template-language))
                     'en))))
          (setq org-scribe--project-language-cache
                (cons (cons cache-key language) org-scribe--project-language-cache))
          language))))

(defun org-scribe-scene-property-aliases (canonical-key)
  "Return the list of literal property name aliases for CANONICAL-KEY.
CANONICAL-KEY is a symbol such as \\='characters or \\='plot (see the
`:properties' section of `lang/org-scribe-lang.el').  Reads every
registered language pack live (via `org-scribe-lang-all'), so a
language registered after this file loaded is still recognized.  If
CANONICAL-KEY is not found in any registered pack, it is returned as a
single-element list unchanged, so callers may also pass a literal
property name directly."
  (or (org-scribe-lang-all :properties canonical-key)
      (list canonical-key)))

(defun org-scribe-scene-property-name (canonical-key &optional language)
  "Return the literal property name to write for CANONICAL-KEY.
LANGUAGE defaults to `org-scribe-project-language'.  Signals an error
if CANONICAL-KEY is not a known scene property in any registered
language pack -- see `org-scribe-lang-property'."
  (org-scribe-lang-property canonical-key language))

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
  (org-scribe--pack-derived-alist :headings :heading-variants)
  "Canonical section key -> localized level-1 heading aliases.
Mirrors `org-scribe--scene-property-aliases': English and Spanish project
templates use different literal heading text for the same section
(\"Characters\" vs \"Personajes\", etc.).  Derived from the registered
language packs; see `org-scribe--pack-derived-alist'.  Kept, in this
exact shape, only for `capture/org-scribe-capture.el''s direct readers
-- `org-scribe--heading-parent-section-p' below reads the live registry
instead, via `org-scribe-lang-all'.")

(defun org-scribe--heading-parent-section-p (section-key)
  "Return non-nil if the level-1 parent of the heading at point is SECTION-KEY.
SECTION-KEY is a symbol such as \\='characters, \\='setting, or
\\='plot-threads (see the `:headings' section of `lang/org-scribe-lang.el').
Used by entity heading predicates to recognize short-story projects'
notes.org layout, where characters/locations/plot threads are nested as
level-2 headings under a level-1 section rather than being top-level
headings of their own, as in novel projects.  Matches any localized
alias for that section, case-insensitively, reading every registered
language pack live via `org-scribe-lang-all' -- so a language
registered after this file loaded is still recognized."
  (save-excursion
    (and (org-up-heading-safe)
         (= (org-current-level) 1)
         (let ((heading (org-get-heading t t t t)))
           (cl-some (lambda (alias) (string-equal-ignore-case alias heading))
                    (org-scribe-lang-all :headings section-key))))))

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

The manuscript is read via `:manuscript-file', which resolves both a
novel's and a short story's manuscript (see `org-scribe-project-structure'),
so no separate short-story fallback is needed here."
  (let* ((structure (org-scribe-project-structure)))
    (delq nil
          (list (plist-get structure :manuscript-file)
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

(defvar-local org-scribe--refile-targets-was-local nil
  "Whether `org-refile-targets' already had a buffer-local value before
`org-scribe--refile-enable' gave this buffer its own.  Read by
`org-scribe--refile-disable' to decide *how* to restore: nil means the
buffer was simply tracking the global value (a plain `(setq
org-refile-targets ...)' in the writer's init file, most commonly), so
restoring correctly means going back to tracking it too
\(`kill-local-variable') rather than `setq-local'-ing a same-looking
snapshot that then permanently stops following later changes to the
global value.  Non-nil means the buffer had its own value already (a
dir-local, a file-local, an earlier `setq-local' from something else),
which restoring must put back as buffer-local, not discard.")

(defvar-local org-scribe--refile-outline-path-was-local nil
  "Whether `org-refile-use-outline-path' already had a buffer-local value
before `org-scribe--refile-enable' gave this buffer its own.  See
`org-scribe--refile-targets-was-local', which this mirrors exactly for
the other overridden variable.")

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
by heading text alone).

Deliberately does not touch `org-refile-use-cache'.  An earlier version
turned it on globally here (with a plain `setq', since it has no
buffer-local meaning), on the reasoning that a project-wide file list is
exactly the case that cache exists for — but nothing ever turned it back
off, not even `org-scribe--refile-disable', so opening a single
org-scribe file left every *other* Org file's refiling cached for the
rest of the session. If the writer has turned `org-refile-use-cache' on
themselves, `org-scribe--refile-invalidate-cache' (called from
`org-scribe-insert-scene', `org-scribe-insert-chapter' and after every
capture) keeps it from going stale against org-scribe's own commands
without org-scribe ever needing to flip the setting itself."
  (unless (org-scribe--refile-enabled-p)
    (setq org-scribe--refile-targets-was-local (local-variable-p 'org-refile-targets)
          org-scribe--refile-outline-path-was-local (local-variable-p 'org-refile-use-outline-path)
          org-scribe--refile-saved-targets org-refile-targets
          org-scribe--refile-saved-outline-path org-refile-use-outline-path))
  ;; The cdr here is the target-description slot, not an optional
  ;; keyword-plist: `t' is Org's own spelling for "all headlines,"
  ;; and omitting it (a bare `(org-scribe--project-refile-files)') is
  ;; read by `org-refile-get-targets' as a target description of nil,
  ;; which is not one of its recognized forms and errors as "Bad
  ;; refiling target description."
  (setq-local org-refile-targets '((org-scribe--project-refile-files . t)))
  (setq-local org-refile-use-outline-path 'file))

(defun org-scribe--refile-disable ()
  "Restore this buffer's refile variables to their pre-org-scribe values.
No-op if `org-scribe--refile-enable' was never called in this buffer.

Restoring is not simply `setq-local'-ing the saved values back: a plain
`setq-local' *always* leaves a buffer-local binding behind, even when
the buffer had none before org-scribe touched it (the ordinary case — a
writer's own refile setup is usually a bare `(setq org-refile-targets
...)' in their init file, tracked globally, not per buffer).  Doing that
would silently detach the buffer from the global variable forever after:
a later change to the global value — a customize edit, another package,
a fresh `setq' — would reach every other buffer and quietly not reach
this one.  `org-scribe--refile-targets-was-local' and
`-outline-path-was-local' record, at enable time, whether each variable
was genuinely buffer-local already; only then does disabling restore via
`setq-local', putting back the buffer's own prior value.  Otherwise it
uses `kill-local-variable', which is what actually undoes org-scribe's
override and lets the buffer resume tracking the global value, exactly
as if org-scribe had never touched it."
  (when (org-scribe--refile-enabled-p)
    (if org-scribe--refile-targets-was-local
        (setq-local org-refile-targets org-scribe--refile-saved-targets)
      (kill-local-variable 'org-refile-targets))
    (if org-scribe--refile-outline-path-was-local
        (setq-local org-refile-use-outline-path org-scribe--refile-saved-outline-path)
      (kill-local-variable 'org-refile-use-outline-path))
    (setq org-scribe--refile-saved-targets org-scribe--refile-unset
          org-scribe--refile-saved-outline-path org-scribe--refile-unset
          org-scribe--refile-targets-was-local nil
          org-scribe--refile-outline-path-was-local nil)))

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

(defun org-scribe--refile-invalidate-cache ()
  "Clear Org's refile cache, if the writer has one to clear.
`org-scribe-insert-scene', `org-scribe-insert-chapter' and every
org-scribe capture command add a new heading, and a heading added to a
file already in `org-refile-targets' is exactly what `org-refile-cache'
does not notice on its own — see `org-refile-use-cache''s docstring: the
cache is invalidated on a changed *file set*, not a changed file.
Without this, a writer who has `org-refile-use-cache' on (their own
choice; org-scribe no longer turns it on for them, see
`org-scribe--refile-enable') would refile into a project and not see the
scene they just wrote until manually clearing the cache
\(`C-u C-u C-c C-w').

A no-op, silently, when there is nothing cached — either because the
writer never turned `org-refile-use-cache' on, or because Org has not
populated it yet — so this is safe to call unconditionally after every
insert/capture rather than threading that check through each caller.
`org-refile-cache-clear' itself always prints \"Refile cache has been
cleared\"; that message is fine when the writer asks for it via
`C-u C-u C-c C-w', but is noise here, where this runs as a routine side
effect of an ordinary insert or capture the writer did not ask to hear
about."
  (when org-refile-cache
    (let ((inhibit-message t))
      (org-refile-cache-clear))))

;; Not scoped to org-scribe buffers or org-scribe's own captures: any
;; `org-capture' finalize can add a heading to a cached file, org-scribe's
;; or not, and `org-scribe--refile-invalidate-cache' is already a no-op
;; whenever there is nothing cached to clear.
(add-hook 'org-capture-after-finalize-hook #'org-scribe--refile-invalidate-cache)

(provide 'org-scribe-core)

;;; org-scribe-core.el ends here
