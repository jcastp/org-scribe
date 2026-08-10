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
(require 'org-scribe-search)

;; Entity getter functions (from linking modules, loaded before this file)
(declare-function org-scribe--get-all-characters "linking/org-scribe-character-links")
(declare-function org-scribe--get-all-locations "linking/org-scribe-location-links")
(declare-function org-scribe--get-all-plot-threads "linking/org-scribe-plot-links")
(declare-function org-scribe--get-all-plot-points "linking/org-scribe-plot-point-links")

;; Planner struct accessors — only called when (featurep 'org-scribe-planner)
(declare-function org-scribe-plan-title         "planning/org-scribe-planner")
(declare-function org-scribe-plan-total-words   "planning/org-scribe-planner")
(declare-function org-scribe-plan-current-words "planning/org-scribe-planner")
(declare-function org-scribe-plan-daily-words   "planning/org-scribe-planner")
(declare-function org-scribe-plan-start-date    "planning/org-scribe-planner")
(declare-function org-scribe-plan-end-date      "planning/org-scribe-planner")
(declare-function org-scribe-planner--generate-day-schedule "planning/org-scribe-planner")
(declare-function org-scribe-planner--get-today-date         "planning/org-scribe-planner")
(defvar org-scribe-planner--current-plan)

;;; Date Helpers (used by Writing Plan section)

(defun org-scribe--health-days-until (date-string)
  "Return days from today to DATE-STRING (YYYY-MM-DD), minimum 0."
  (let* ((d (parse-time-string date-string))
         (target (encode-time 0 0 0 (nth 3 d) (nth 4 d) (nth 5 d)))
         (now (decode-time))
         (today (encode-time 0 0 0 (nth 3 now) (nth 4 now) (nth 5 now))))
    (max 0 (round (/ (float-time (time-subtract target today)) 86400)))))

(defun org-scribe--health-days-since (date-string)
  "Return days from DATE-STRING (YYYY-MM-DD) to today, minimum 0."
  (let* ((d (parse-time-string date-string))
         (start (encode-time 0 0 0 (nth 3 d) (nth 4 d) (nth 5 d)))
         (now (decode-time))
         (today (encode-time 0 0 0 (nth 3 now) (nth 4 now) (nth 5 now))))
    (max 0 (round (/ (float-time (time-subtract today start)) 86400)))))

(defun org-scribe--health-plan-status (plan)
  "Return 'On track' or 'Behind by N words' for PLAN.
Expected words are computed by walking PLAN's day schedule up to and
including today, skipping spare days — matching
`org-scribe-planner--show-progress-report' — instead of the naive
elapsed-days × daily-words, which overcounts every spare day (e.g.
weekends off) as an expected writing day."
  (let* ((today (org-scribe-planner--get-today-date))
         (expected 0))
    (dolist (day (org-scribe-planner--generate-day-schedule plan))
      (when (and (not (string< today (plist-get day :date)))
                 (not (plist-get day :is-spare-day)))
        (setq expected (+ expected (plist-get day :words)))))
    (let ((delta (- expected (org-scribe-plan-current-words plan))))
      (if (<= delta 0)
          "On track"
        (format "Behind by %d words" delta)))))

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
  (HEADING CHAPTER ID TODO-STATE HAS-POV HAS-CHARS HAS-PLOT HAS-LOCATION
   WORDCOUNT POV-NAME)
WORDCOUNT is the scene's :WORDCOUNT: property as a number (0 if unset).
POV-NAME is the resolved display text of the PoV property (via
`org-scribe--extract-link-text', so an ID link or plain name both
resolve to the character's name), or nil when no PoV is set."
  (let (scenes)
    (when (and novel-file (file-exists-p novel-file))
      (with-current-buffer (find-file-noselect novel-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (when (and (= (org-current-level) 3)
                       (not (member "noexport" (org-get-tags))))
              (let* ((heading (org-get-heading t t t t))
                     (id (org-id-get))
                     (todo (org-get-todo-state))
                     (chapter (save-excursion
                                (org-up-heading-safe)
                                (org-get-heading t t t t)))
                     (pov (org-scribe-scene-property-get 'pov))
                     (chars (org-scribe-scene-property-get 'characters))
                     (plot (org-scribe-scene-property-get 'plot))
                     (loc (org-scribe-scene-property-get 'location))
                     (wordcount (string-to-number (or (org-entry-get nil "WORDCOUNT") "0")))
                     (pov-name (org-scribe--extract-link-text pov)))
                (push (list heading chapter id todo
                            (and pov (not (string-empty-p (string-trim pov))))
                            (and chars (not (string-empty-p (string-trim chars))))
                            (and plot (not (string-empty-p (string-trim plot))))
                            (and loc (not (string-empty-p (string-trim loc))))
                            wordcount
                            pov-name)
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
              (unless (member "noexport" (org-get-tags))
                (when (= level 3)
                  (when-let ((wc (org-entry-get nil "WORDCOUNT")))
                    (setq words (+ words (string-to-number wc)))))
                (when (= level 2)
                  (when-let ((wo (org-entry-get nil "WORD-OBJECTIVE")))
                    (setq obj (+ obj (string-to-number wo))))))))
          nil 'file))))
    (cons words obj)))

;;; Text-level statistics (per-PoV word share, chapter length spread)

(defun org-scribe--health-pov-word-share (scenes)
  "Group SCENES by resolved PoV name and sum their WORDCOUNT.
SCENES is a list as returned by `org-scribe--health-collect-scene-data'.
Returns a list of (POV-NAME SCENE-COUNT WORDS), in order of each PoV's
first appearance in SCENES.  Scenes with no PoV are grouped under the
localized \"(no PoV)\" label, which doubles as a missing-property signal."
  (let ((counts (make-hash-table :test 'equal))
        (words (make-hash-table :test 'equal))
        (order nil)
        (none-label (org-scribe-msg 'msg-health-pov-none-label)))
    (dolist (s scenes)
      (let* ((pov-name (nth 9 s))
             (key (if (and pov-name (not (string-empty-p (string-trim pov-name))))
                      pov-name
                    none-label))
             (wc (or (nth 8 s) 0)))
        (unless (gethash key counts)
          (push key order)
          (puthash key 0 counts)
          (puthash key 0 words))
        (puthash key (1+ (gethash key counts)) counts)
        (puthash key (+ (gethash key words) wc) words)))
    (mapcar (lambda (key)
              (list key (gethash key counts) (gethash key words)))
            (nreverse order))))

(defun org-scribe--health-chapter-word-totals (scenes)
  "Group SCENES by chapter and sum their WORDCOUNT.
SCENES is a list as returned by `org-scribe--health-collect-scene-data'.
Returns a list of (CHAPTER . WORDS), in order of each chapter's first
appearance in SCENES."
  (let ((totals (make-hash-table :test 'equal))
        (order nil))
    (dolist (s scenes)
      (let ((chapter (or (nth 1 s) "?"))
            (wc (or (nth 8 s) 0)))
        (unless (gethash chapter totals)
          (push chapter order)
          (puthash chapter 0 totals))
        (puthash chapter (+ (gethash chapter totals) wc) totals)))
    (mapcar (lambda (chapter) (cons chapter (gethash chapter totals)))
            (nreverse order))))

(defun org-scribe--health-median (numbers)
  "Return the median of NUMBERS (a list of numbers).
Averages the two middle values when NUMBERS has an even length."
  (let* ((sorted (sort (copy-sequence numbers) #'<))
         (n (length sorted)))
    (if (zerop n)
        0
      (if (cl-oddp n)
          (nth (/ n 2) sorted)
        (/ (+ (nth (1- (/ n 2)) sorted) (nth (/ n 2) sorted)) 2.0)))))

(defun org-scribe--health-collect-referenced-ids (novel-file)
  "Return a hash table of all entity IDs referenced in scene properties.
Scans PoV, Characters, Location, Plot and Plot-point properties of all
level-3 headings in NOVEL-FILE for [[id:...]] link patterns.

Every entity type whose orphans are reported must have its scene
property listed here, or all of its entities are reported as orphaned."
  (let ((ids (make-hash-table :test 'equal)))
    (when (and novel-file (file-exists-p novel-file))
      (with-current-buffer (find-file-noselect novel-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (when (= (org-current-level) 3)
              (dolist (prop '(pov characters location plot plot-point))
                (when-let ((val (org-scribe-scene-property-get prop)))
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
           ;; An orphaned plot point is one of the thirteen non-negotiables
           ;; that no scene serves — the method's own check, mechanized.
           (orphan-points  (when (fboundp 'org-scribe--get-all-plot-points)
                             (org-scribe--health-find-orphans
                              (org-scribe--get-all-plot-points) ref-ids)))
           (scene-count    (length scenes))
           ;; Per-PoV word share and chapter length spread (text-level stats)
           (pov-share      (org-scribe--health-pov-word-share scenes))
           (chapter-totals (org-scribe--health-chapter-word-totals scenes))
           (chapter-words  (mapcar #'cdr chapter-totals))
           (chapter-mean   (if chapter-words
                               (/ (float (apply #'+ chapter-words)) (length chapter-words))
                             0.0))
           (chapter-median (org-scribe--health-median chapter-words))
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

        ;; ── Writing Plan ─────────────────────────────────────────────────────
        ;; Skipped entirely when the project has explicitly declined the
        ;; planner (gate = 'no); undecided (nil) keeps prior behavior.
        (when (and (featurep 'org-scribe-planner)
                   (not (eq (org-scribe-planner-gate) 'no)))
          (insert "* Writing Plan\n\n")
          (cond
           ;; Plan is active in memory
           (org-scribe-planner--current-plan
            (let* ((plan  org-scribe-planner--current-plan)
                   (title   (org-scribe-plan-title plan))
                   (target  (org-scribe-plan-total-words plan))
                   (current (org-scribe-plan-current-words plan))
                   (daily   (org-scribe-plan-daily-words plan))
                   (end     (org-scribe-plan-end-date plan))
                   (pct     (if (> target 0)
                                (* 100.0 (/ (float current) target))
                              0.0))
                   (days-rem (org-scribe--health-days-until end))
                   (status  (org-scribe--health-plan-status plan)))
              (insert (format "- Title :: %s\n" title))
              (insert (format "- Target :: %d words\n" target))
              (insert (format "- Current :: %d words (%.1f%%)\n" current pct))
              (insert (format "- Daily target :: %d words\n" daily))
              (insert (format "- End date :: %s\n" end))
              (insert (format "- Days remaining :: %d\n" days-rem))
              (insert (format "- Status :: %s\n" status))))
           ;; Plan file exists but has not been loaded yet
           ((plist-get structure :plan-file)
            (insert (format "Plan file found at =%s= but not loaded.\n"
                            (file-name-nondirectory (plist-get structure :plan-file))))
            (insert "Run =M-x org-scribe-plan= to load it.\n"))
           ;; No plan at all
           (t
            (insert "No active plan.  Create one with =M-x org-scribe-plan=.\n")))
          (insert "\n"))

        ;; ── Scene status breakdown ────────────────────────────────────────────
        (insert "* Scenes by Status\n\n")
        (insert "| Status | Count |\n")
        (insert "|--------+-------|\n")
        (maphash (lambda (state count)
                   (insert (format "| %-6s | %5d |\n" state count)))
                 todo-counts)
        (insert "\n")

        ;; ── Per-PoV word share ───────────────────────────────────────────────
        (insert (format "* %s\n\n" (org-scribe-msg 'msg-health-pov-word-share-heading)))
        (insert (format "%s\n" (org-scribe-msg 'msg-health-pov-word-share-table-header)))
        (insert "|-----+--------+-------+------------|\n")
        (dolist (row pov-share)
          (let* ((pov-name (nth 0 row))
                 (count (nth 1 row))
                 (words (nth 2 row))
                 (pct (if (> total-words 0) (* 100.0 (/ (float words) total-words)) 0.0)))
            (insert (format "| %s | %d | %d | %.1f%% |\n" pov-name count words pct))))
        (insert "\n")

        ;; ── Chapter length spread ────────────────────────────────────────────
        (insert (format "* %s\n\n" (org-scribe-msg 'msg-health-chapter-length-heading)))
        (insert (format "%s\n" (org-scribe-msg 'msg-health-chapter-length-table-header)))
        (insert "|---------+-------|\n")
        (dolist (entry chapter-totals)
          (let* ((chapter (car entry))
                 (words (cdr entry))
                 (outlier (and (> chapter-mean 0)
                              (or (> words (* 2 chapter-mean))
                                  (< words (* 0.5 chapter-mean))))))
            (insert (format "| %s | %d%s |\n" chapter words (if outlier " *" "")))))
        (insert "\n")
        (insert (format "%s\n\n"
                        (org-scribe-msg 'msg-health-chapter-length-summary
                                        (if chapter-words (apply #'min chapter-words) 0)
                                        (if chapter-words (apply #'max chapter-words) 0)
                                        chapter-mean
                                        chapter-median)))
        (when (cl-some (lambda (entry)
                         (and (> chapter-mean 0)
                              (or (> (cdr entry) (* 2 chapter-mean))
                                  (< (cdr entry) (* 0.5 chapter-mean)))))
                       chapter-totals)
          (insert (format "%s\n\n" (org-scribe-msg 'msg-health-chapter-length-outlier-legend))))

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

        ;; Only shown when the project actually defines plot points, so
        ;; projects created from the pre-sistema templates do not grow an
        ;; empty section reporting nothing.
        (when (and (fboundp 'org-scribe--get-all-plot-points)
                   (org-scribe--get-all-plot-points))
          (insert (format "** Plot Points With No Scene (%d)\n\n" (length orphan-points)))
          (insert "Non-negotiables that no scene serves yet.\n\n")
          (if orphan-points
              (dolist (name orphan-points) (insert (format "- %s\n" name)))
            (insert "Every plot point is served by at least one scene.\n"))
          (insert "\n"))

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
