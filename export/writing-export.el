;;; writing-export.el --- Export filters for emacs-writing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Export filters and utilities for creative writing.
;; Includes scene break replacement for different export backends.

;;; Code:

(require 'ox)
(require 'writing-config)

;;; Scene Break Filter

(defun org-export-replace-scene-breaks (text backend _info)
  "Replace SCENE-BREAK markers with backend-appropriate scene separators.
TEXT is the export output, BACKEND is the export backend symbol.
Uses `writing/scene-break-replacements' for customization.

Usage: Add this macro to your org file:
  #+MACRO: scene-break SCENE-BREAK

Then use {{{scene-break}}} wherever you want a scene break."
  (let ((replacement (or (alist-get backend writing/scene-break-replacements)
                         (alist-get t writing/scene-break-replacements))))
    (replace-regexp-in-string "SCENE-BREAK" replacement text t t)))

;; Add to export filter list
(add-to-list 'org-export-filter-final-output-functions
             #'org-export-replace-scene-breaks)

(provide 'writing-export)

;;; writing-export.el ends here
