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
  :group 'writing)

(defcustom org-scribe-notes-dir-names '("notes" "notas")
  "List of directory names to try when looking for notes directory."
  :type '(repeat string)
  :group 'writing)

(defcustom org-scribe-create-dirs-automatically t
  "When non-nil, create directories (characters/, research/) automatically."
  :type 'boolean
  :group 'writing)

(defcustom org-scribe-stories-directory
  (expand-file-name "~/writing/exercises")
  "Directory where writing exercise files will be created."
  :type 'directory
  :group 'writing)

;;; Writing Modes Configuration

(defgroup org-scribe-env nil
  "Customization group for writing environment modes."
  :group 'writing)

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

(defcustom org-scribe/sinonimo-window-width 80
  "Width of the synonyms side window."
  :type 'integer
  :group 'writing)

(defcustom org-scribe/python-script-path
  (expand-file-name "~/Nextcloud/escritura/software/writing_companion/writing_companion.py")
  "Path to writing companion Python script for exercise generation."
  :type 'file
  :group 'writing)

(defcustom org-scribe/exercise-templates
  '(("all" . "all prompt")
    ("character" . "character prompt")
    ("setting" . "setting prompt"))
  "Available writing exercise templates.
Each element is (NAME . SCRIPT-ARGS) where SCRIPT-ARGS are passed
to the Python script."
  :type '(alist :key-type string :value-type string)
  :group 'writing)

;;; Export Configuration

(defcustom org-scribe/scene-break-replacements
  '((ascii . "\n***\n\n")
    (md . "\n***\n\n")
    (man . "\n***\n\n")
    (html . "<br><br><br>\n")
    (latex . "\\vspace{\\baselineskip}\\vspace{\\baselineskip}\\vspace{\\baselineskip}\n")
    (t . "\n\n\n"))
  "Alist of export backend symbols to scene break replacement strings.
The key t serves as the default for unlisted backends."
  :type '(alist :key-type symbol :value-type string)
  :group 'writing)

;;; Word Counting

(defcustom org-scribe-wordcount-default-ignore-tags '("noexport")
  "Default tags to ignore when counting words in org documents."
  :type '(repeat string)
  :group 'writing)

;;; Editing Mode Configuration

(defcustom org-scribe-editing-left-width-percent 0.25
  "Percentage of frame width for left panel in editing mode."
  :type 'float
  :group 'writing)

(defcustom org-scribe-editing-right-width-percent 0.25
  "Percentage of frame width for right panel in editing mode."
  :type 'float
  :group 'writing)

(defcustom org-scribe-editing-theme 'leuven
  "Theme to use in editing mode."
  :type 'symbol
  :group 'writing)

(defcustom org-scribe-editing-fill-column-width 90
  "Column width for visual-fill-column in editing mode."
  :type 'integer
  :group 'writing)

(defcustom org-scribe-editing-fontaine-preset 'editing
  "Fontaine preset to use in editing mode."
  :type 'symbol
  :group 'writing)

(provide 'org-scribe-config)

;;; org-scribe-config.el ends here
