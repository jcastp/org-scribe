;;; writing-column-view.el --- Column view enhancements for emacs-writing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Enhances org-mode column view to display clean text instead of ID link syntax.
;;
;; When properties contain ID links like [[id:abc123][Display Name]], column view
;; will show only "Display Name" instead of the full link syntax.
;;
;; This module automatically integrates with org-mode's column view system
;; using advice to filter displayed values.
;;
;; Features:
;; - Automatic link stripping in column view
;; - Preserves links in properties (navigation still works)
;; - Backward compatible with plain text properties
;; - Works for all linked properties (PoV, Characters, Plot, Location)
;; - Optional - can be disabled via customization

;;; Code:

(require 'org)
(require 'org-colview)

;; Declare external functions
(declare-function writing--extract-link-text "writing-search")

;;; Customization

(defgroup writing-column-view nil
  "Column view enhancements for emacs-writing."
  :group 'writing
  :prefix "writing-column-view-")

(defcustom writing-column-view-strip-links t
  "Whether to strip ID links in column view display.
When non-nil, ID links like [[id:abc][Name]] will display as \"Name\".
When nil, full link syntax is displayed.

This only affects column view display - the actual property values
remain unchanged and links still work with \\[org-open-at-point]."
  :type 'boolean
  :group 'writing-column-view)

;;; Core Functions

(defun writing--column-view-strip-links (value)
  "Strip ID link syntax from VALUE for column view display.
Returns plain display text extracted from ID links.
Returns VALUE unchanged if:
- VALUE is nil
- VALUE is not a string
- VALUE doesn't contain ID links
- `writing-column-view-strip-links' is nil

Examples:
  \"[[id:abc123][Alex]]\" → \"Alex\"
  \"[[id:abc][Alex]], [[id:def][Sam]]\" → \"Alex, Sam\"
  \"Plain text\" → \"Plain text\"
  nil → nil"
  (if (and writing-column-view-strip-links
           value
           (stringp value)
           (string-match-p "\\[\\[id:" value))
      ;; Has ID links - extract display text
      (progn
        (require 'writing-search)
        (writing--extract-link-text value))
    ;; No links or disabled - return unchanged
    value))

(defun writing--column-view-advice (original-value)
  "Advice function to strip ID links from column view display.
This is added as :filter-return advice to `org-columns--displayed-value'.
ORIGINAL-VALUE is the return value from the original function."
  (writing--column-view-strip-links original-value))

;;; Enable/Disable Functions

(defun writing-column-view-enable ()
  "Enable ID link stripping in column view.
Adds advice to `org-columns--displayed-value' to strip link syntax."
  (interactive)
  (advice-add 'org-columns--displayed-value :filter-return
              #'writing--column-view-advice)
  (message "Column view link stripping enabled"))

(defun writing-column-view-disable ()
  "Disable ID link stripping in column view.
Removes advice from `org-columns--displayed-value'."
  (interactive)
  (advice-remove 'org-columns--displayed-value
                 #'writing--column-view-advice)
  (message "Column view link stripping disabled"))

(defun writing-column-view-toggle ()
  "Toggle ID link stripping in column view."
  (interactive)
  (if (advice-member-p #'writing--column-view-advice
                       'org-columns--displayed-value)
      (writing-column-view-disable)
    (writing-column-view-enable)))

;;; Automatic Setup

;; Enable by default when module is loaded
(when writing-column-view-strip-links
  (writing-column-view-enable))

(provide 'writing-column-view)

;;; writing-column-view.el ends here
