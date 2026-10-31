;;; org-scribe-companion-edits.el --- Visual companion: the Edits tab -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; The data behind the page's Edits tab, served on GET /api/edits, and the
;; operation its entries use:
;;   goto-marker   show a marker's line in Emacs (any .org file of the project)
;;
;; The index is the one F8 6 builds (`org-scribe--edits-collect': every
;; *EDIT* and *NOTE* marker in a comment block, in every .org file below the
;; project root, unsaved edits included).  A marker in the manuscript is
;; placed in its scene and chapter through the snapshot, whose scenes carry
;; their own markers (`:edits', same parser, same buffer text), so the two
;; cannot disagree on which scene a marker belongs to.
;;
;; The snapshot's per-scene markers are what the corkboard and table badges,
;; the detail panel and the "Edit markers" colour criterion read; this file
;; only adds the markers outside scenes and the grouping data the tab needs.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)
(require 'org-scribe-messages)
(require 'org-scribe-core)
(require 'org-scribe-companion-model)
(require 'org-scribe-companion-commands)

(declare-function org-scribe--edits-collect "org-scribe-search")
(defvar org-scribe-companion-raise-frame)

(defun org-scribe-companion-edits--placements (snapshot)
  "Return a hash table from a manuscript line to where SNAPSHOT puts it.
Each value is a plist: `:sceneId', `:sceneTitle', `:chapterId',
`:chapterTitle' (:null where there is none)."
  (let ((table (make-hash-table)))
    (seq-doseq (act (plist-get snapshot :acts))
      (seq-doseq (chapter (plist-get act :chapters))
        (seq-doseq (scene (plist-get chapter :scenes))
          (seq-doseq (marker (plist-get scene :edits))
            (puthash (plist-get marker :line)
                     (list :sceneId (plist-get scene :id)
                           :sceneTitle (plist-get scene :title)
                           ;; a short story's scenes sit straight under the
                           ;; act: there is no chapter to name
                           :chapterId (if (plist-get chapter :passthrough) :null
                                        (plist-get chapter :id))
                           :chapterTitle (if (plist-get chapter :passthrough) :null
                                           (plist-get chapter :title)))
                     table)))))
    table))

(defun org-scribe-companion-edits-get (root)
  "Return the answer to GET /api/edits for the project at ROOT.
\(:ok t :revision REV :markers [...]).  Each marker has `:type',
`:category' (canonical, or :null), `:text', `:file' (relative to ROOT),
`:line', `:heading' and `:manuscript'; one in a manuscript scene also
names the scene and its chapter (see `org-scribe-companion-edits--placements')."
  (require 'org-scribe-search)
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (default-directory root))
    (condition-case err
        (let* ((snapshot (org-scribe-companion--snapshot root))
               (manuscript (plist-get (org-scribe-project-structure) :manuscript-file))
               (placements (org-scribe-companion-edits--placements snapshot))
               (markers
                (mapcar
                 (lambda (marker)
                   (let* ((file (plist-get marker :file))
                          (in-manuscript (and manuscript (file-equal-p file manuscript)))
                          (place (and in-manuscript
                                      (gethash (plist-get marker :line) placements))))
                     (append
                      (list :type (plist-get marker :type)
                            :category (org-scribe-companion--or-null (plist-get marker :category))
                            :text (plist-get marker :text)
                            :file (file-relative-name file root)
                            :line (plist-get marker :line)
                            :heading (org-scribe-companion--or-null (plist-get marker :heading))
                            :manuscript (org-scribe-companion--bool in-manuscript))
                      (or place (list :sceneId :null :sceneTitle :null
                                      :chapterId :null :chapterTitle :null)))))
                 (org-scribe--edits-collect root))))
          (list :ok t
                :revision (plist-get snapshot :revision)
                :markers (vconcat markers)))
      (user-error (org-scribe-companion-commands-error
                   "not-found" (error-message-string err))))))

;;; goto-marker

(defun org-scribe-companion-edits--goto (root args)
  "Show a marker's line in Emacs.  ARGS: `:file' (relative to ROOT, an
.org file inside the project; the manuscript when absent) and `:line'
\(1-based).  Widens when the line is outside a narrowing, reveals it, and
raises the frame when `org-scribe-companion-raise-frame'."
  (let* ((name (or (org-scribe-companion-commands--arg-string args :file)
                   (let ((default-directory root))
                     (when-let* ((manuscript (plist-get (org-scribe-project-structure)
                                                        :manuscript-file)))
                       (file-relative-name manuscript root)))))
         (line (plist-get args :line))
         (file (and name (expand-file-name name root))))
    (unless (and file (string-suffix-p ".org" file)
                 (file-in-directory-p file root)
                 (file-exists-p file))
      (org-scribe-companion-commands--fail "not-found" 'companion-error-no-such-file (or name "")))
    (unless (and (integerp line) (> line 0))
      (org-scribe-companion-commands--fail "invalid" 'companion-error-missing-arg "line"))
    (pop-to-buffer (find-file-noselect file))
    (let ((position (save-restriction
                      (widen)
                      (save-excursion
                        (goto-char (point-min))
                        (forward-line (1- line))
                        (point)))))
      (when (or (< position (point-min)) (> position (point-max))) (widen))
      (goto-char position))
    (when (derived-mode-p 'org-mode)
      (org-fold-show-context 'link-search))
    (recenter)
    (when org-scribe-companion-raise-frame
      (select-frame-set-input-focus (selected-frame)))
    (org-scribe-companion-commands--ok root)))

(provide 'org-scribe-companion-edits)

;;; org-scribe-companion-edits.el ends here
