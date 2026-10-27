;;; org-scribe-companion.el --- Visual companion for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Entry point of the optional visual companion: a browser front end for a
;; novel project, with Emacs as the only program that reads and writes the
;; project files.  Off until `org-scribe-companion-enable' is set.
;;
;; The user-facing checks (switch, project type, per-project gate) live in
;; `org-scribe-companion-open' in core/org-scribe-core.el, so that with the
;; switch off this file is never loaded.  So far (Phase 1) the companion
;; can build the project snapshot and show it as JSON; there is no server
;; yet.

;;; Code:

(require 'org)
(require 'org-id)
(require 'json)
(require 'org-scribe-messages)
(require 'org-scribe-core)
(require 'org-scribe-companion-model)
(require 'org-scribe-companion-commands)
(require 'org-scribe-companion-server)

;;; Headings without an ID

(defun org-scribe-companion--headings-without-id (snapshot)
  "Return how many acts, chapters and scenes of SNAPSHOT have no ID.
Implicit parents (made for headings outside any act or chapter) are not
counted: there is no heading to give an ID to."
  (let ((count 0))
    (cl-flet ((check (heading)
                (when (and (eq (plist-get heading :id) :null)
                           (not (plist-get heading :implicit)))
                  (cl-incf count))))
      (seq-doseq (act (plist-get snapshot :acts))
        (check act)
        (seq-doseq (chapter (plist-get act :chapters))
          (check chapter)
          (seq-doseq (scene (plist-get chapter :scenes))
            (check scene)))))
    count))

(defun org-scribe-companion--add-missing-ids (root)
  "Give an ID to every act, chapter and scene without one at ROOT.
An ordinary edit of the manuscript buffer, left unsaved.  Returns how many
IDs were added."
  (let* ((default-directory (file-name-as-directory root))
         (manuscript (plist-get (org-scribe-project-structure) :manuscript-file))
         (chapter-level (org-scribe-chapter-level))
         (scene-level (org-scribe-scene-level))
         (count 0))
    (with-current-buffer (find-file-noselect manuscript)
      (org-with-wide-buffer
       (goto-char (point-min))
       (org-map-entries
        (lambda ()
          (when (and (memq (org-current-level) (list 1 chapter-level scene-level))
                     (not (member "noexport" (org-get-tags)))
                     (not (org-id-get)))
            (org-id-get-create)
            (cl-incf count)))
        nil 'file)))
    count))

(defun org-scribe-companion--ensure-ids (root)
  "Offer once to add the IDs the project at ROOT lacks.
Returns the number of headings that will stay without an ID."
  (let ((missing (org-scribe-companion--headings-without-id
                  (org-scribe-companion--snapshot root))))
    (when (> missing 0)
      (if (yes-or-no-p (org-scribe-msg 'companion-ask-add-ids missing))
          (message "%s" (org-scribe-msg 'companion-ids-added
                                        (org-scribe-companion--add-missing-ids root)))
        (message "%s" (org-scribe-msg 'companion-ids-missing-note missing))))
    (org-scribe-companion--headings-without-id
     (org-scribe-companion--snapshot root))))

;;; Debugging

(defun org-scribe-companion-debug-snapshot ()
  "Show the current project's snapshot as JSON in a buffer.
Meant for development.  Available once the companion module is loaded
(open the companion first, or `require' it)."
  (interactive)
  (let ((json (org-scribe-companion--snapshot-json))
        (buffer (get-buffer-create (org-scribe-msg 'companion-debug-buffer-name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert json)
        (when (require 'json-ts-mode nil t)
          (ignore-errors (json-ts-mode)))
        (when (and (not (derived-mode-p 'json-ts-mode)) (require 'js nil t))
          (js-json-mode))
        (ignore-errors (json-pretty-print-buffer))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

;;; Opening

(defun org-scribe-companion--open (root)
  "Start the companion for the project at ROOT and show it.
So far this only offers to add missing IDs; the server is not there yet."
  (org-scribe-companion--ensure-ids root)
  (message "%s" (org-scribe-msg 'companion-not-implemented)))

(provide 'org-scribe-companion)

;;; org-scribe-companion.el ends here
