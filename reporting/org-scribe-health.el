;;; org-scribe-health.el --- Project health report for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Aggregated project health report for org-scribe writing projects.
;; Collects word-count progress, scene completion statistics, missing
;; properties, and orphaned entities into a single *org-scribe-health*
;; buffer with clickable ID links.

;;; Code:

(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'org-scribe-core)
(require 'org-scribe-messages)

;; Entity getter functions (from linking modules, loaded before this file)
(declare-function org-scribe--get-all-characters "linking/org-scribe-character-links")
(declare-function org-scribe--get-all-locations "linking/org-scribe-location-links")
(declare-function org-scribe--get-all-plot-threads "linking/org-scribe-plot-links")

;;; Internal Helpers

(defun org-scribe--health-scene-link (heading id)
  "Return a clickable org link string for HEADING using its ID.
Falls back to plain HEADING text when ID is nil."
  (if id
      (format "[[id:%s][%s]]" id heading)
    heading))

(defun org-scribe--health-collect-scene-data (novel-file)
  "Scan NOVEL-FILE and return a list of scene data plists.
Each element is a list:
  (HEADING CHAPTER ID TODO-STATE HAS-POV HAS-CHARS HAS-PLOT HAS-LOCATION)"
  (let (scenes)
    (when (and novel-file (file-exists-p novel-file))
      (with-current-buffer (find-file-noselect novel-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (when (= (org-current-level) 3)
              (let* ((heading (org-get-heading t t t t))
                     (id (org-id-get))
                     (todo (org-get-todo-state))
                     (chapter (save-excursion
                                (org-up-heading-safe)
                                (org-get-heading t t t t)))
                     (pov (org-entry-get nil "PoV"))
                     (chars (org-entry-get nil "Characters"))
                     (plot (org-entry-get nil "Plot"))
                     (loc (org-entry-get nil "Location")))
                (push (list heading chapter id todo
                            (and pov (not (string-empty-p (string-trim pov))))
                            (and chars (not (string-empty-p (string-trim chars))))
                            (and plot (not (string-empty-p (string-trim plot))))
                            (and loc (not (string-empty-p (string-trim loc)))))
                      scenes))))
          nil 'file))))
    (nreverse scenes)))

(defun org-scribe--health-word-totals (novel-file)
  "Return (WORDS . OBJECTIVE) from NOVEL-FILE.
WORDS is the sum of WORDCOUNT from level-3 (scene) headings.
OBJECTIVE is the sum of WORD-OBJECTIVE from level-2 (chapter) headings."
  (let ((words 0) (obj 0))
    (when (and novel-file (file-exists-p novel-file))
      (with-current-buffer (find-file-noselect novel-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (let ((level (org-current-level)))
              (when (= level 3)
                (when-let ((wc (org-entry-get nil "WORDCOUNT")))
                  (setq words (+ words (string-to-number wc)))))
              (when (= level 2)
                (when-let ((wo (org-entry-get nil "WORD-OBJECTIVE")))
                  (setq obj (+ obj (string-to-number wo)))))))
          nil 'file))))
    (cons words obj)))

(defun org-scribe--health-collect-referenced-ids (novel-file)
  "Return a hash table of all entity IDs referenced in scene properties.
Scans PoV, Characters, Location, and Plot properties of all level-3
headings in NOVEL-FILE for [[id:...]] link patterns."
  (let ((ids (make-hash-table :test 'equal)))
    (when (and novel-file (file-exists-p novel-file))
      (with-current-buffer (find-file-noselect novel-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (when (= (org-current-level) 3)
              (dolist (prop '("PoV" "Characters" "Location" "Plot"))
                (when-let ((val (org-entry-get nil prop)))
                  (let ((pos 0))
                    (while (string-match "\\[\\[id:\\([^]]+\\)\\]" val pos)
                      (puthash (match-string 1 val) t ids)
                      (setq pos (match-end 0))))))))
          nil 'file))))
    ids))

(defun org-scribe--health-find-orphans (entities referenced-ids)
  "Return list of entity names not present in REFERENCED-IDS.
ENTITIES is an alist (NAME . (ID . HEADING)) as returned by
`org-scribe--get-all-characters' and similar functions."
  (cl-loop for (name . (id . _)) in entities
           unless (gethash id referenced-ids)
           collect name))

;;; Report Rendering Helpers

(defun org-scribe--health-insert-missing-list (label scenes-list prop-name)
  "Insert a subsection listing SCENES-LIST with missing PROP-NAME under LABEL."
  (insert (format "** Scenes missing %s (%d scene%s)\n\n"
                  label
                  (length scenes-list)
                  (org-scribe-plural (length scenes-list) "")))
  (if (null scenes-list)
      (insert (format "All scenes have =%s= set.\n\n" prop-name))
    (dolist (s scenes-list)
      (insert (format "- %s  (chapter: /%s/)\n"
                      (org-scribe--health-scene-link (nth 0 s) (nth 2 s))
                      (or (nth 1 s) "?"))))
    (insert "\n")))

;;; Main Report Function

;;;###autoload
(defun org-scribe-project-health ()
  "Generate a project health report for the current writing project.
Displays an overview of word-count progress, scene completion by TODO
state, scenes with missing critical properties, orphaned characters and
locations, and a list of all open TODO scenes.

The report opens in the *org-scribe-health* buffer as an Org-mode file
with clickable ID links back to each scene."
  (interactive)
  (let* ((structure (org-scribe-project-structure))
         (novel-file (plist-get structure :novel-file)))

    (unless novel-file
      (user-error "No manuscript file found. Is this an org-scribe novel project?"))

    ;; Collect all data before opening the buffer
    (let* ((scenes         (org-scribe--health-collect-scene-data novel-file))
           (done-keywords  (with-current-buffer (find-file-noselect novel-file)
                             org-done-keywords))
           (word-totals    (org-scribe--health-word-totals novel-file))
           (total-words    (car word-totals))
           (total-obj      (cdr word-totals))
           (progress       (if (> total-obj 0)
                               (* 100.0 (/ (float total-words) total-obj))
                             nil))
           (ref-ids        (org-scribe--health-collect-referenced-ids novel-file))
           (orphan-chars   (when (fboundp 'org-scribe--get-all-characters)
                             (org-scribe--health-find-orphans
                              (org-scribe--get-all-characters) ref-ids)))
           (orphan-locs    (when (fboundp 'org-scribe--get-all-locations)
                             (org-scribe--health-find-orphans
                              (org-scribe--get-all-locations) ref-ids)))
           (scene-count    (length scenes))
           ;; Counts by TODO state
           (todo-counts    (let ((ht (make-hash-table :test 'equal)))
                             (dolist (s scenes)
                               (let ((state (or (nth 3 s) "(none)")))
                                 (puthash state (1+ (gethash state ht 0)) ht)))
                             ht))
           ;; Scenes missing specific properties (only pending scenes)
           (pending        (cl-remove-if
                            (lambda (s) (member (nth 3 s) done-keywords))
                            scenes))
           (miss-pov       (cl-remove-if (lambda (s) (nth 4 s)) pending))
           (miss-plot      (cl-remove-if (lambda (s) (nth 6 s)) pending))
           (miss-location  (cl-remove-if (lambda (s) (nth 7 s)) pending))
           ;; Open (not done) scenes — uses done keywords read from the file
           (open-todos     pending))

      (with-current-buffer (get-buffer-create "*org-scribe-health*")
        (erase-buffer)
        (org-mode)

        ;; Header
        (insert "#+TITLE: Project Health Report\n")
        (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M")))
        (insert "#+STARTUP: overview\n\n")

        ;; ── Overview ─────────────────────────────────────────────────────────
        (insert "* Overview\n\n")
        (insert (format "- Manuscript :: [[file:%s][%s]]\n"
                        novel-file (file-name-nondirectory novel-file)))
        (insert (format "- Total scenes :: %d\n" scene-count))
        (insert (format "- Words written :: %d\n" total-words))
        (if progress
            (progn
              (insert (format "- Word objective :: %d\n" total-obj))
              (insert (format "- Progress :: %.1f%%\n" progress)))
          (insert "- Word objective :: /not set/\n"))
        (insert "\n")

        ;; ── Scene status breakdown ────────────────────────────────────────────
        (insert "* Scenes by Status\n\n")
        (insert "| Status | Count |\n")
        (insert "|--------+-------|\n")
        (maphash (lambda (state count)
                   (insert (format "| %-6s | %5d |\n" state count)))
                 todo-counts)
        (insert "\n")

        ;; ── Missing properties ────────────────────────────────────────────────
        (insert "* Missing Scene Properties\n\n")
        (org-scribe--health-insert-missing-list "PoV" miss-pov "PoV")
        (org-scribe--health-insert-missing-list "Plot" miss-plot "Plot")
        (org-scribe--health-insert-missing-list "Location" miss-location "Location")

        ;; ── Orphaned entities ─────────────────────────────────────────────────
        (insert "* Orphaned Entities\n\n")
        (insert "Entities defined in their database file but not linked in any scene.\n\n")

        (insert (format "** Orphaned Characters (%d)\n\n" (length orphan-chars)))
        (if orphan-chars
            (dolist (name orphan-chars) (insert (format "- %s\n" name)))
          (insert "No orphaned characters.\n"))
        (insert "\n")

        (insert (format "** Orphaned Locations (%d)\n\n" (length orphan-locs)))
        (if orphan-locs
            (dolist (name orphan-locs) (insert (format "- %s\n" name)))
          (insert "No orphaned locations.\n"))
        (insert "\n")

        ;; ── Open TODOs ────────────────────────────────────────────────────────
        (insert (format "* Open TODO Scenes (%d)\n\n" (length open-todos)))
        (if (null open-todos)
            (insert "All scenes are marked DONE.\n")
          (dolist (s open-todos)
            (insert (format "- [%s] %s  (chapter: /%s/)\n"
                            (or (nth 3 s) "?")
                            (org-scribe--health-scene-link (nth 0 s) (nth 2 s))
                            (or (nth 1 s) "?")))))

        (goto-char (point-min))
        (pop-to-buffer (current-buffer))
        (message "Project health report generated.")))))

(provide 'org-scribe-health)

;;; org-scribe-health.el ends here
