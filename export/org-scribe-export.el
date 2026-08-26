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

;;; :ignore: Tag Activation (via ox-extra, optional)

;; Every scene and chapter heading in the shipped templates carries the
;; :ignore: tag, intending the heading's *title* to drop from export
;; while its body is kept -- exactly what ox-extra's `ignore-headlines'
;; extra does (from the optional `org-contrib' package). Plain Org has
;; no built-in handling for a tag literally named "ignore" --
;; `org-export-exclude-tags' defaults to ("noexport") only -- so
;; without this, :ignore: is inert and TODO keywords and titles leak
;; into every exported manuscript. `org-scribe-setup-check' (in
;; org-scribe.el) lists `ox-extra' among the optional dependencies so
;; a writer who has not installed it is told why exports look wrong,
;; rather than left to discover it silently.

(defvar org-scribe--ox-extra-available (require 'ox-extra nil t)
  "Non-nil when `ox-extra' (from the optional org-contrib package) is loaded.")

(declare-function org-export-ignore-headlines "ox-extra" (data backend info))

(defun org-scribe--export-filter-ignore-headlines (data backend info)
  "Apply ox-extra's `org-export-ignore-headlines' to org-scribe documents only.
Delegates to it when `org-scribe--export-in-scribe-context-p' says INFO
belongs to an org-scribe document; otherwise returns DATA unchanged, so
a :ignore:-tagged heading in an unrelated Org file is never affected
just because this package happens to be loaded -- the same scoping
`org-scribe--export-filter-scene-breaks' already applies to scene
breaks."
  (if (org-scribe--export-in-scribe-context-p info)
      (org-export-ignore-headlines data backend info)
    data))

(when org-scribe--ox-extra-available
  (add-to-list 'org-export-filter-parse-tree-functions
               #'org-scribe--export-filter-ignore-headlines))

(provide 'org-scribe-export)

;;; org-scribe-export.el ends here
