;;; org-scribe-config.el --- Configuration variables for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configuration variables (defcustom) for the org-scribe package.
;; Users can customize these in their init files.

;;; Code:

(defgroup org-scribe nil
  "Creative writing in Org-mode."
  :group 'org
  :prefix "org-scribe-")

;;; Messages

(defcustom org-scribe-message-language 'en
  "Language for user-facing messages: `en' (English) or `es' (Spanish).
Affects every string returned by `org-scribe-msg' — commands, prompts,
and error messages throughout org-scribe.  Unrelated to
`org-scribe-template-language' / `org-scribe-project-language', which
select which language's *project templates* a new project is created
from; see `core/org-scribe-messages.el' for why this defcustom has a
forward `defvar' there too."
  :type '(choice (const :tag "English" en)
                 (const :tag "Spanish" es))
  :group 'org-scribe)

;;; Project Structure

;; Project file and directory names are deliberately not configurable
;; here.  `org-scribe-project-structure' resolves them from the fixed
;; bilingual pairs the templates ship (novel.org / novela.org, notes/ /
;; notas/, and so on), and the manuscript pair also drives project-type
;; detection in `org-scribe-project-type', so a rename would leave the
;; project unrecognized.  Options that nothing reads are worse than no
;; options: they read as a supported way to rename these files.

(defcustom org-scribe-create-dirs-automatically t
  "When non-nil, create directories (characters/, research/) automatically."
  :type 'boolean
  :group 'org-scribe)

(defcustom org-scribe-projects-directory
  (expand-file-name "~/writing/")
  "Default base directory proposed when creating a new novel or short story project."
  :type 'directory
  :group 'org-scribe)

(defcustom org-scribe-stories-directory
  (expand-file-name "~/writing/exercises")
  "Directory where writing exercise files will be created."
  :type 'directory
  :group 'org-scribe)

;;; Writing Modes Configuration

(defgroup org-scribe-env nil
  "Customization group for writing environment modes."
  :group 'org-scribe)

(defcustom org-scribe-env-normal-theme 'ef-deuteranopia-dark
  "Theme for normal environment."
  :type 'symbol
  :group 'org-scribe-env)

(defcustom org-scribe-env-work-theme 'poet
  "Theme for writing environment."
  :type 'symbol
  :group 'org-scribe-env)

(defcustom org-scribe-env-normal-font 'regular
  "Fontaine preset for normal environment."
  :type 'symbol
  :group 'org-scribe-env)

(defcustom org-scribe-env-work-font 'org-scribe-big
  "Fontaine preset for writing environment."
  :type 'symbol
  :group 'org-scribe-env)

(defcustom org-scribe-env-work-width 80
  "Writeroom width for writing mode."
  :type 'integer
  :group 'org-scribe-env)

(defcustom org-scribe-env-normal-width 90
  "Writeroom width for normal mode."
  :type 'integer
  :group 'org-scribe-env)

;;; Dictionary and Language Tools

(defcustom org-scribe-sinonimo-window-width 80
  "Width of the synonyms side window."
  :type 'integer
  :group 'org-scribe)

(defcustom org-scribe-writing-companion-script nil
  "Path to writing companion Python script for exercise generation.
Set this to the absolute path of writing_companion.py on your system.
When nil, writing companion features that call this script are disabled.

Example:
  (setq org-scribe-writing-companion-script
        \"/home/user/scripts/writing_companion.py\")"
  :type '(choice (const :tag "Disabled" nil) file)
  :group 'org-scribe)

(defcustom org-scribe-exercise-templates
  '(("all" . "all prompt")
    ("character" . "character prompt")
    ("setting" . "setting prompt"))
  "Available writing exercise templates.
Each element is (NAME . SCRIPT-ARGS) where SCRIPT-ARGS are passed
to the Python script."
  :type '(alist :key-type string :value-type string)
  :group 'org-scribe)

;;; Export Configuration

(defcustom org-scribe-scene-break-replacements
  '((ascii . "\n***\n\n")
    (md . "\n***\n\n")
    (man . "\n***\n\n")
    (html . "<br><br><br>\n")
    (latex . "\\vspace{\\baselineskip}\\vspace{\\baselineskip}\\vspace{\\baselineskip}\n")
    (t . "\n\n\n"))
  "Alist of export backend symbols to scene break replacement strings.
The key t serves as the default for unlisted backends."
  :type '(alist :key-type symbol :value-type string)
  :group 'org-scribe)

;;; Search Configuration

(defcustom org-scribe-todo-keywords
  '("TODO" "ONGOING" "WAITING" "TOWRITE" "TOREVIEW" "REDO" "RESTRUCTURE")
  "TODO keywords recognised by org-scribe search functions.
Used by `org-scribe-search-todos-recursive' to find in-progress items.
Writers who use different TODO keyword sets can override this to match
their workflow."
  :type '(repeat string)
  :group 'org-scribe)

;;; Word Counting

(defcustom org-scribe-wordcount-default-ignore-tags '("noexport")
  "Default tags to ignore when counting words in org documents."
  :type '(repeat string)
  :group 'org-scribe)

;;; Overlay Tooltips

(defcustom org-scribe-overlays-enable nil
  "When non-nil, enable entity tooltips automatically in org-scribe buffers.
With this set to t, `org-scribe-overlays-mode' activates whenever
`org-scribe-mode' is turned on.  Moving point onto any [[id:...]] link
inside a scene property will display a tooltip showing the entity's
Role, Age, Occupation, Goal, Motivation, and Conflict.

You can also toggle tooltips manually at any time with:
  M-x org-scribe-overlays-mode"
  :type 'boolean
  :group 'org-scribe)

(defcustom org-scribe-overlays-display 'inline
  "How entity tooltips are displayed when `org-scribe-overlays-mode' is active.

\\='inline   — show the tooltip as styled text immediately after the ID link
               in the buffer (default).  No extra packages needed; works in
               both terminal and GUI Emacs.

\\='posframe  — show the tooltip in a posframe child frame near point.
               Requires the `posframe' package.  Falls back to \\='inline when
               posframe is unavailable or the display cannot support child
               frames (e.g. terminal sessions).

\\='echo      — show the tooltip in the echo area (original behaviour)."
  :type '(choice (const :tag "Inline after the link (default)" inline)
                 (const :tag "Posframe near point (requires posframe)" posframe)
                 (const :tag "Echo area" echo))
  :group 'org-scribe)

;;; Automation (opt-in save-time bookkeeping)

(defcustom org-scribe-auto-relink nil
  "When non-nil, refresh ID-link display names automatically on save.
When an entity database file (characters/locations/plot) is saved and the
manuscript is open, stale display names in scene properties are refreshed
to match the current entity names.  The manuscript buffer is modified but
NOT saved automatically; a message reminds you to save.

This replaces the need to run the `update-*-link-names' commands by hand.
Disabled by default so saving never edits another buffer unexpectedly."
  :type 'boolean
  :group 'org-scribe)

(defcustom org-scribe-auto-wordcount nil
  "When non-nil, update all WORDCOUNT properties and the writing plan on save.
On saving the manuscript buffer of an org-scribe project,
`org-scribe-ews-org-count-words' runs silently: it recomputes the WORDCOUNT
property on every heading in the buffer and, when the writing planner is
active, syncs today's word delta to the plan automatically.
Requires `org-context-extended' (no-op when that package is absent, to
avoid writing metadata-inclusive counts silently).

Disabled by default to keep saving fast and side-effect-free."
  :type 'boolean
  :group 'org-scribe)

(defcustom org-scribe-auto-wordcount-mint-ids nil
  "When non-nil, the save-triggered word count is also allowed to mint IDs.
`org-scribe-ews-org-count-words' normally calls `org-id-get-create' on
every heading it visits (so scenes can be linked); when
`org-scribe-auto-wordcount' silently re-runs it on every save, that
means merely saving the manuscript permanently inserts a fresh =:ID:=
property drawer on any heading that lacked one — buffer churn and larger
diffs the user never asked for.

Disabled by default so the silent save path only refreshes WORDCOUNT
properties; ID creation stays an explicit, visible action (interactively
running `org-scribe-ews-org-count-words' or `org-scribe-wordcount' with
one prefix argument always mints IDs regardless of this setting)."
  :type 'boolean
  :group 'org-scribe)

;;; Editing Mode Configuration

(defcustom org-scribe-editing-left-width-percent 0.25
  "Percentage of frame width for left panel in editing mode."
  :type 'float
  :group 'org-scribe)

(defcustom org-scribe-editing-right-width-percent 0.25
  "Percentage of frame width for right panel in editing mode."
  :type 'float
  :group 'org-scribe)

(defcustom org-scribe-editing-right-panel 'notes
  "What to display in the right pane of `org-scribe-editing-mode'.

Possible values:
- `notes'    -- the project notes file, as chosen by
                `org-scribe-capture-target-file' (the same file
                org-scribe captures write to).
- `edits'    -- a live index of the project's inline edit markers, as
                built by `org-scribe-search-edits'.  Unlike the other
                values this is a generated buffer, not a file, and it
                refreshes whenever you save an Org file in the project.
- `revision' -- the project revision file, when one exists.
- a string   -- a project-relative file path, e.g. \"notes/research.org\".
- a function -- called with the manuscript file name; must return the
                file name to display in the pane.

The pane used to show a per-manuscript companion file (\"novel.org\" ->
\"novel-notes.org\") that existed only as an org-remark annotation sink.
org-remark support has been removed and inline edit markers replaced it,
so the pane now defaults to the project notes file instead."
  :type '(choice (const :tag "Project notes file" notes)
                 (const :tag "Live edit marker index" edits)
                 (const :tag "Project revision file" revision)
                 (string :tag "Project-relative file path")
                 (function :tag "Function returning a file name"))
  :group 'org-scribe)

(defcustom org-scribe-editing-theme 'leuven
  "Theme to use in editing mode."
  :type 'symbol
  :group 'org-scribe)

(defcustom org-scribe-editing-fill-column-width 90
  "Column width for visual-fill-column in editing mode."
  :type 'integer
  :group 'org-scribe)

(defcustom org-scribe-editing-fontaine-preset 'editing
  "Fontaine preset to use in editing mode."
  :type 'symbol
  :group 'org-scribe)

;;; Search

(defcustom org-scribe-edit-string
  "\\*EDIT\\*\\|\\*NOTE\\*"
  "Regexp to search for edit notes in the manuscript.
The default matches headings or text containing *EDIT* or *NOTE*.

Used by `org-scribe-search-edits-rgrep', the plain-text fallback
search.  The structured index built by `org-scribe-search-edits' does
not use this regexp: it parses the marker grammar directly, so that it
can tell *EDIT* from *NOTE* and read the category off an *EDIT*.
Customize this when you want the text search to find markers of your
own beyond the two org-scribe knows about."
  :type 'string
  :group 'org-scribe)

(defcustom org-scribe-edit-categories
  '("plot" "scene" "character" "prose")
  "Known categories for *EDIT* markers, used to group the edit index.

An *EDIT* marker carries a category before a \" - \" separator:

  *EDIT*: plot - Alice's motive contradicts chapter 1

`org-scribe-search-edits' groups the index by these categories.  A
marker whose category is absent, empty, or not in this list is grouped
under a catch-all \"other\" heading rather than being dropped, so a
typo moves a marker but never hides it.

*NOTE* markers have no category and are listed in their own section."
  :type '(repeat string)
  :group 'org-scribe)

(defcustom org-scribe-edits-index-show-empty-categories t
  "Whether the edit index lists categories that have no markers.

When non-nil, every category in `org-scribe-edit-categories' gets a
heading even when empty, so the index doubles as a checklist of what is
outstanding in each.  When nil, only categories with markers appear."
  :type 'boolean
  :group 'org-scribe)

(provide 'org-scribe-config)

;;; org-scribe-config.el ends here
