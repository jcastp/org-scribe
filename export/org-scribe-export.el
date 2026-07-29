;;; org-scribe-export.el --- Export filters for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Export filters and utilities for creative writing.
;; Includes scene break replacement for different export backends.

;;; Code:

(require 'ox)
(require 'org-scribe-config)

;;; Scene Break Filter

(defun org-scribe--export-replace-scene-breaks (text backend _info)
  "Replace SCENE-BREAK markers with backend-appropriate scene separators.
TEXT is the export output, BACKEND is the export backend symbol.
Uses `org-scribe-scene-break-replacements' for customization.

Usage: Add this macro to your org file:
  #+MACRO: scene-break SCENE-BREAK

Then use {{{scene-break}}} wherever you want a scene break."
  (let ((replacement (or (alist-get backend org-scribe-scene-break-replacements)
                         (alist-get t org-scribe-scene-break-replacements))))
    (replace-regexp-in-string "SCENE-BREAK" replacement text t t)))

(defun org-scribe--export-in-scribe-context-p (info)
  "Return non-nil if the document being exported belongs to org-scribe.
INFO is the export communication channel plist passed to filter functions.
Checked via either of:
  - the exported file lives under an org-scribe project (a directory tree
    containing a .org-scribe-project marker), or
  - the source buffer has `org-scribe-mode' enabled (covers unsaved
    buffers, where :input-file is nil).
Used to scope `org-scribe--export-replace-scene-breaks' to org-scribe
documents, instead of rewriting every literal occurrence of \"SCENE-BREAK\"
in the final output of *any* org export in the session."
  (or (when-let* ((file (plist-get info :input-file)))
        (locate-dominating-file (file-name-directory file) ".org-scribe-project"))
      (when-let* ((buffer-name (plist-get info :input-buffer))
                  (buffer (get-buffer buffer-name)))
        (and (boundp 'org-scribe-mode)
             (buffer-local-value 'org-scribe-mode buffer)))))

(defun org-scribe--export-filter-scene-breaks (text backend info)
  "Export filter: rewrite SCENE-BREAK markers only for org-scribe documents.
Registered on `org-export-filter-final-output-functions'.  Delegates to
`org-scribe--export-replace-scene-breaks' when
`org-scribe--export-in-scribe-context-p' says INFO belongs to an org-scribe
document; otherwise returns TEXT unchanged, so exporting an unrelated org
file that happens to contain the literal string \"SCENE-BREAK\" is not
affected."
  (if (org-scribe--export-in-scribe-context-p info)
      (org-scribe--export-replace-scene-breaks text backend info)
    text))

;; Add to export filter list
(add-to-list 'org-export-filter-final-output-functions
             #'org-scribe--export-filter-scene-breaks)

(provide 'org-scribe-export)

;;; org-scribe-export.el ends here
