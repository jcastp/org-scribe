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

;;; Project Structure

(defcustom org-scribe-novel-file-names '("novel.org" "novela.org" "manuscript.org")
  "List of filenames to try when looking for main manuscript."
  :type '(repeat string)
  :group 'org-scribe)

(defcustom org-scribe-notes-dir-names '("notes" "notas")
  "List of directory names to try when looking for notes directory."
  :type '(repeat string)
  :group 'org-scribe)

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
inside a scene property will then display a one-line tooltip in the echo
area showing the entity's Role, Age, Goal, and Motivation.

You can also toggle tooltips manually at any time with:
  M-x org-scribe-overlays-mode"
  :type 'boolean
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
  "When non-nil, refresh scene WORDCOUNT properties on save.
On saving a manuscript buffer in an org-scribe project, the WORDCOUNT
property of each scene heading is recomputed quietly (no IDs are created,
no echo-area noise).  Requires `org-context-extended'.

Disabled by default to keep saving fast and side-effect-free."
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

(provide 'org-scribe-config)

;;; org-scribe-config.el ends here
