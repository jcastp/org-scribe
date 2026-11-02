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
(require 'org-scribe-chronology)

;; Entity getter functions (from linking modules, loaded before this file)
(declare-function org-scribe--get-all-characters "linking/org-scribe-character-links")
(declare-function org-scribe--get-all-locations "linking/org-scribe-location-links")
(declare-function org-scribe--get-all-objects "linking/org-scribe-object-links")
(declare-function org-scribe--get-all-plot-threads "linking/org-scribe-plot-links")
(declare-function org-scribe--get-all-plot-points "linking/org-scribe-plot-point-links")
(declare-function org-scribe--find-broken-links "linking/org-scribe-link-update")

;; Directory-local spelling dictionary (from templates/org-scribe-project.el,
;; loaded before this file; guarded with `fboundp' at the call site anyway).
(declare-function org-scribe--dir-locals-dictionary "templates/org-scribe-project")
(defvar org-scribe-write-dir-locals)

;; Planner struct accessors — only called when (featurep 'org-scribe-planner)
(declare-function org-scribe-plan-title         "planning/org-scribe-planner")
(declare-function org-scribe-plan-total-words   "planning/org-scribe-planner")
(declare-function org-scribe-plan-current-words "planning/org-scribe-planner")
(declare-function org-scribe-plan-preexisting-words "planning/org-scribe-planner")
(declare-function org-scribe-plan-daily-words   "planning/org-scribe-planner")
(declare-function org-scribe-plan-start-date    "planning/org-scribe-planner")
(declare-function org-scribe-plan-end-date      "planning/org-scribe-planner")
(declare-function org-scribe-planner--ensure-plan-for-current-project "planning/org-scribe-planner")
(declare-function org-scribe-planner--generate-day-schedule "planning/org-scribe-planner")
(declare-function org-scribe-planner--get-today-date         "planning/org-scribe-planner")
(declare-function org-scribe-planner--status                 "planning/org-scribe-planner")
(declare-function org-scribe-planner--outside-total          "planning/org-scribe-planner")
(defvar org-scribe-planner--current-plan)

;;; Date Helpers (used by Writing Plan section)

(defun org-scribe--health-days-until (date-string)
  "Return days from today to DATE-STRING (YYYY-MM-DD), minimum 0."
  (let* ((d (parse-time-string date-string))
         (target (encode-time 0 0 0 (nth 3 d) (nth 4 d) (nth 5 d)))
         (now (decode-time))
         (today (encode-time 0 0 0 (nth 3 now) (nth 4 now) (nth 5 now))))
    (max 0 (round (/ (float-time (time-subtract target today)) 86400)))))

(defun org-scribe--health-plan-status (plan)
  "Return \"On track\", \"Ahead by N words\" or \"Behind by N words\" for PLAN.
Measured as the planner's own status (`org-scribe-planner--status'):
against where the manuscript should stand at the end of today, along the
day schedule (spare days skipped), so the report, the companion and F8 W
agree."
  (let ((status (org-scribe-planner--status plan)))
    (let ((delta (if status (- (plist-get status :difference)) 0)))
      (cond ((> delta 0) (org-scribe-msg 'msg-health-plan-behind delta))
            ((< delta 0) (org-scribe-msg 'msg-health-plan-ahead (- delta)))
            (t (org-scribe-msg 'msg-health-plan-on-track))))))

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
   WORDCOUNT POV-NAME CHAPTER-KEY)
CHAPTER-KEY tells two chapters with the same title apart: the chapter's
ID, or its position when it has none.
WORDCOUNT is the scene's :WORDCOUNT: property as a number (0 if unset).
POV-NAME is the resolved display text of the PoV property (via
`org-scribe--extract-link-text', so an ID link or plain name both
resolve to the character's name), or nil when no PoV is set.

Scenes are identified by `org-scribe-scene-level', the level for the
*current* project type (level 3 for a novel, level 2 for a short story)
-- not a hardcoded 3, which would silently find nothing in a short
story's manuscript."
  (let (scenes)
    (when (and novel-file (file-exists-p novel-file))
      (with-current-buffer (find-file-noselect novel-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (let ((scene-level (org-scribe-scene-level)))
           (org-map-entries
            (lambda ()
              (when (and (= (org-current-level) scene-level)
                         (not (member "noexport" (org-get-tags))))
                (let* ((heading (org-get-heading t t t t))
                       (id (org-id-get))
                       (todo (org-get-todo-state))
                       (chapter (save-excursion
                                  (org-up-heading-safe)
                                  (org-get-heading t t t t)))
                       (chapter-key (save-excursion
                                      (and (org-up-heading-safe)
                                           (or (org-id-get) (point)))))
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
                              pov-name
                              chapter-key)
                        scenes))))
            nil 'file)))))
    (nreverse scenes)))

(defun org-scribe--health-word-totals (novel-file)
  "Return (WORDS . OBJECTIVE) from NOVEL-FILE.
WORDS is the sum of WORDCOUNT from scene headings (the level for the
current project type, per `org-scribe-scene-level' -- level 3 for a
novel, level 2 for a short story).
OBJECTIVE is the sum of the *outermost* WORD-OBJECTIVE values: a heading
counts only when no ancestor carries one.  Objectives roll up, so each
level's figures restate the one above (a novel's acts, then their
chapters, then their scenes); adding levels together would double-count,
and picking a fixed level reported a chapter-level 8,000 against an
80,000-word project.  The top-most level that sets an objective is the
authoritative one -- the acts in a shipped novel, the `Story Content'
container in a shipped short story (scenes added later carry their own
objectives, which roll up into it)."
  (let ((words 0) (obj 0))
    (when (and novel-file (file-exists-p novel-file))
      (with-current-buffer (find-file-noselect novel-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (let ((scene-level (org-scribe-scene-level)))
           (org-map-entries
            (lambda ()
              (when (and (= (org-current-level) scene-level)
                         (not (member "noexport" (org-get-tags))))
                (when-let* ((wc (org-entry-get nil "WORDCOUNT")))
                  (setq words (+ words (string-to-number wc))))))
            nil 'file)))
        (dolist (marker (org-scribe--outermost-objective-markers novel-file))
          (setq obj (+ obj (string-to-number
                            (org-with-point-at marker
                              (org-entry-get nil "WORD-OBJECTIVE")))))
          (set-marker marker nil))))
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
appearance in SCENES.  Chapters are told apart by their key (the last
element of a scene, see `org-scribe--health-collect-scene-data'), not by
title: two chapters both called \"Chapter\" are two rows, the later ones
shown as \"Chapter (2)\".  A scene with no key is grouped by title."
  (let ((totals (make-hash-table :test 'equal))
        (titles (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal))
        (order nil))
    (dolist (s scenes)
      (let* ((title (or (nth 1 s) "?"))
             (key (or (nth 10 s) title))
             (wc (or (nth 8 s) 0)))
        (unless (gethash key totals)
          (push key order)
          (puthash key 0 totals)
          (let ((n (1+ (gethash title seen 0))))
            (puthash title n seen)
            (puthash key (if (> n 1) (format "%s (%d)" title n) title) titles)))
        (puthash key (+ (gethash key totals) wc) totals)))
    (mapcar (lambda (key) (cons (gethash key titles) (gethash key totals)))
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
scene headings (the level for the current project type, per
`org-scribe-scene-level') in NOVEL-FILE for [[id:...]] link patterns.

Every entity type whose orphans are reported must have its scene
property listed here, or all of its entities are reported as orphaned."
  (let ((ids (make-hash-table :test 'equal)))
    (when (and novel-file (file-exists-p novel-file))
      (with-current-buffer (find-file-noselect novel-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (let ((scene-level (org-scribe-scene-level)))
           (org-map-entries
            (lambda ()
              (when (= (org-current-level) scene-level)
                (dolist (prop '(pov characters location objects plot plot-point))
                  (when-let* ((val (org-scribe-scene-property-get prop)))
                    (let ((pos 0))
                      (while (string-match "\\[\\[id:\\([^]]+\\)\\]" val pos)
                        (puthash (match-string 1 val) t ids)
                        (setq pos (match-end 0))))))))
            nil 'file)))))
    ids))

;;; Starting Gate
;;
;; The method's Starting Gate is the single checklist that decides when
;; planning stops and drafting begins.  Six of its eight items live as prose
;; in the design file and can only be self-reported; two are measurable, and
;; measuring them is the point of this section — a checklist that only ever
;; reads back what you asserted cannot tell you that you crossed the gate
;; eleven days ago.
;;
;; Both figures are reported alongside the writer's own tick, and a
;; disagreement is called out rather than silently resolved either way: the
;; report's job is to show the discrepancy, not to decide who is right.

(defun org-scribe--health-gate-items (design-file)
  "Return the Starting Gate checkboxes from DESIGN-FILE.
Each element is (CHECKED-P . LABEL), in document order.  Returns nil when
there is no design file or no gate section — projects created from the
pre-sistema templates have neither, and the caller skips the section."
  (when (and design-file (file-exists-p design-file))
    (with-current-buffer (find-file-noselect design-file)
      (org-with-wide-buffer
       (let ((gate (org-scribe--find-section-heading 'starting-gate))
             (items nil))
         (when gate
           (goto-char gate)
           (save-restriction
             (org-narrow-to-subtree)
             (goto-char (point-min))
             (while (re-search-forward "^[ \t]*- \\[\\([ X-]\\)\\][ \t]+\\(.*\\)$" nil t)
               (let ((checked (string= (match-string 1) "X"))
                     (label (string-trim (match-string 2))))
                 ;; Gate labels wrap: "Protagonista y oponente con Fantasma,
                 ;; Mentira, Debilidad, Deseo y\n  Necesidad".  Absorb the
                 ;; indented continuation lines, or the label is reported
                 ;; truncated mid-sentence.
                 (forward-line 1)
                 (while (and (not (eobp))
                             (looking-at "^[ \t]+\\([^ \t\n-].*\\)$"))
                   (setq label (concat label " " (string-trim (match-string 1))))
                   (forward-line 1))
                 (push (cons checked label) items)))))
         (nreverse items))))))

;; The Starting Gate's two measurable items are identified by matching a
;; substring of their label -- see each pack's `:gate-substrings' section
;; (`lang/org-scribe-lang.el') for the known localized substrings.  Every
;; method's gate (Sistema, Hélice, Matriz — see `org-scribe--methods')
;; phrases its plot-points item and its first-three-scenes item using one
;; of these, in whichever language the project template was written in.
;;
;; This exists so that `org-scribe--health-gate-item' finds the right
;; checkbox by what it says rather than by its position in the list —
;; `(nth 6 gate-items)' and `(nth 7 gate-items)' assumed the Sistema's
;; eight-item gate in the Sistema's own order, which the other methods'
;; gates do not share: their items are fewer, differently grouped, and in
;; a different order.  Matching on the label is also what lets a
;; disagreement note render correctly even if a project's gate has been
;; manually reordered.

(defun org-scribe--health-gate-item (key gate-items)
  "Return the (CHECKED-P . LABEL) entry of GATE-ITEMS measuring KEY.
KEY is `plot-points' or `first-three-scenes' (see the `:gate-substrings'
section of `lang/org-scribe-lang.el').  Returns nil when no item's
label contains any of KEY's known substrings, case-insensitively —
which happens for every project whose design file predates a pack
having an entry for KEY, not only for a genuinely malformed gate."
  (let ((substrings (org-scribe-lang-all :gate-substrings key))
        (case-fold-search t))
    (cl-find-if
     (lambda (item)
       (cl-some (lambda (s) (string-match-p (regexp-quote s) (cdr item)))
                substrings))
     gate-items)))

(defun org-scribe--health-plot-points-with-content (design-file)
  "Return (FILLED . TOTAL) for the thirteen non-negotiables in DESIGN-FILE.
A plot point counts as filled when its body holds real content, as
`org-scribe--body-has-content-p' decides: hints, whether `#' lines or
comment blocks, are not content."
  (when (and design-file (file-exists-p design-file)
             (fboundp 'org-scribe--plot-point-heading-p))
    (with-current-buffer (find-file-noselect design-file)
      (org-with-wide-buffer
       (let ((total 0) (filled 0))
         (org-map-entries
          (lambda ()
            (when (org-scribe--plot-point-heading-p)
              (cl-incf total)
              (when (org-scribe--body-has-content-p)
                (cl-incf filled))))
          nil 'file)
         (when (> total 0) (cons filled total)))))))

(defun org-scribe--health-scenes-written (scenes n)
  "Return how many of the first N SCENES have a non-zero word count."
  (let ((first-n (seq-take scenes n)))
    (seq-count (lambda (s) (> (nth 8 s) 0)) first-n)))

(defun org-scribe--health-dir-locals-hint ()
  "Return a hint string when the project's `.dir-locals.el' needs attention.

The project language lives in `.org-scribe-project'; the spelling
dictionary is derived from it into `.dir-locals.el' at creation time.
The two can drift — a project created before org-scribe generated the
file has none, and a project whose language was edited by hand keeps the
old dictionary — and that drift is invisible until a spell check
disagrees with the prose.  Returns nil when there is nothing to say."
  (when (and org-scribe-write-dir-locals
             (fboundp 'org-scribe--dir-locals-dictionary))
    (when-let* ((root (org-scribe-project-root))
                (expected (org-scribe--dir-locals-dictionary
                           (org-scribe-project-language))))
      (let ((file (expand-file-name ".dir-locals.el" root)))
        (cond
         ((not (file-exists-p file))
          (org-scribe-msg 'msg-health-dir-locals-missing expected))
         (t
          (let ((found (ignore-errors
                         (alist-get 'ispell-local-dictionary
                                    (alist-get nil (with-temp-buffer
                                                     (insert-file-contents file)
                                                     (read (current-buffer))))))))
            (cond
             ((null found)
              (org-scribe-msg 'msg-health-dir-locals-empty expected))
             ((not (equal found expected))
              (org-scribe-msg 'msg-health-dir-locals-differs found expected))
             (t nil)))))))))

(defun org-scribe--health-duplicate-names (entities)
  "Return the names that more than one entity in ENTITIES carries.
ENTITIES is a getter's alist of (NAME . (ID . HEADING)).  Each entry of
the result is (NAME . COUNT), in order of first appearance.  Completion
is keyed on the name, so all but the first of such entities can never
be picked, and every link goes to the first one."
  (let ((counts nil))
    (dolist (entry entities)
      (let ((cell (assoc (car entry) counts)))
        (if cell (setcdr cell (1+ (cdr cell)))
          (push (cons (car entry) 1) counts))))
    (seq-filter (lambda (cell) (> (cdr cell) 1)) (nreverse counts))))

(defun org-scribe--health-find-orphans (entities referenced-ids)
  "Return list of entity names not present in REFERENCED-IDS.
ENTITIES is an alist (NAME . (ID . HEADING)) as returned by
`org-scribe--get-all-characters' and similar functions."
  (mapcar #'car (org-scribe--health-find-orphan-entries entities referenced-ids)))

;;; Report Rendering Helpers

(defun org-scribe--health-insert-missing-list (label scenes-list prop-name)
  "Insert a subsection listing SCENES-LIST with missing PROP-NAME under LABEL.
Each scene is followed by its chapter, in parentheses, only for a
project type with a real chapter level (a novel) -- see
`org-scribe--health-word-totals' for why -- since a short story's
\"chapter\" is really its own single level-1 manuscript wrapper (e.g.
\"Story Content\"), and printing that on every line names nothing
useful."
  (insert (format "** %s\n\n"
                  (org-scribe-msg 'msg-health-missing-scenes
                                  label
                                  (length scenes-list)
                                  (org-scribe-plural (length scenes-list) ""))))
  (if (null scenes-list)
      (insert (org-scribe-msg 'msg-health-missing-none prop-name) "\n\n")
    (dolist (s scenes-list)
      (if (org-scribe-chapter-level)
          (insert (format "- %s  %s\n"
                          (org-scribe--health-scene-link (nth 0 s) (nth 2 s))
                          (org-scribe-msg 'msg-health-chapter-suffix (or (nth 1 s) "?"))))
        (insert (format "- %s\n"
                        (org-scribe--health-scene-link (nth 0 s) (nth 2 s))))))
    (insert "\n")))

;;; Story time

(defun org-scribe--health-story-time-data ()
  "Return the Story Time findings of the current project as a plist.
Keys: `:total' (scenes), `:missing', `:unreadable', `:no-anchor',
`:incomplete' (lists of `org-scribe-chronology-collect' scene entries),
and `:jumps', a list of (ENTRY . DISTANCE-TEXT) for each scene that
jumps back in time."
  (let* ((data (org-scribe-chronology-collect))
         (scenes (plist-get data :scenes))
         (jumps (plist-get data :jumps))
         (by-problem (lambda (problem)
                       (seq-filter (lambda (e) (eq (plist-get e :problem) problem))
                                   scenes))))
    (list :total (length scenes)
          :missing (funcall by-problem 'blank)
          :unreadable (funcall by-problem 'unreadable)
          :no-anchor (funcall by-problem 'no-anchor)
          :incomplete (funcall by-problem 'calendar-incomplete)
          :jumps (delq nil
                       (mapcar (lambda (e)
                                 (when-let* ((jump (assq (plist-get e :index) jumps)))
                                   (cons e (org-scribe-chronology--distance (cdr jump)))))
                               scenes)))))

(defun org-scribe--health-insert-story-time (&optional data)
  "Insert the Story Time section: scenes with no time, unreadable ones, jumps.
DATA is `org-scribe--health-story-time-data''s plist, computed when nil.
Flashbacks are listed, not flagged: a jump back is information, and the
list still lets a typo such as Day 23 for Day 32 stand out.  Inserts
nothing for a project with no scenes."
  (let* ((data (or data (org-scribe--health-story-time-data)))
         (total (plist-get data :total))
         (names (lambda (entries &optional with-text)
                  (mapconcat (lambda (e)
                               (let ((link (org-scribe--health-scene-link
                                            (plist-get e :heading)
                                            (plist-get e :id))))
                                 (if with-text
                                     (format "%s (%s)" link (plist-get e :text))
                                   link)))
                             entries ", ")))
         (missing (plist-get data :missing))
         (unreadable (plist-get data :unreadable))
         (no-anchor (plist-get data :no-anchor))
         (incomplete (plist-get data :incomplete))
         (jumps (plist-get data :jumps)))
    (when (> total 0)
      (insert (format "* %s\n\n" (org-scribe-msg 'msg-health-story-time-heading)))
      (insert (org-scribe-msg 'msg-health-story-time-summary
                              (- total (length missing) (length unreadable)
                                 (length no-anchor) (length incomplete))
                              total)
              "\n\n")
      ;; a project that has not started on story time gets one line, not a
      ;; list of every scene
      (when (= (length missing) total)
        (insert (org-scribe-msg 'msg-health-story-time-none) "\n\n"))
      (when (and missing (< (length missing) total))
        (insert (org-scribe-msg 'msg-health-story-time-missing
                                (length missing) (funcall names missing))
                "\n\n"))
      (when unreadable
        (insert (org-scribe-msg 'msg-health-story-time-unreadable
                                (length unreadable) (funcall names unreadable t))
                "\n\n"))
      (when incomplete
        (insert (org-scribe-msg 'msg-health-story-time-calendar-incomplete
                                (length incomplete) (funcall names incomplete t))
                "\n\n"))
      (when no-anchor
        (insert (org-scribe-msg 'msg-health-story-time-no-anchor
                                (length no-anchor) (funcall names no-anchor t))
                "\n\n"))
      (when jumps
        (insert (org-scribe-msg
                 'msg-health-story-time-flashbacks (length jumps)
                 (mapconcat
                  (lambda (jump)
                    (format "%s %s" (org-scribe--health-scene-link
                                     (plist-get (car jump) :heading)
                                     (plist-get (car jump) :id))
                            (cdr jump)))
                  jumps ", "))
                "\n\n")))))

;;; Collecting the report's data
;;
;; The report is computed once, as data, by `org-scribe--health-data', and
;; drawn by `org-scribe--health-render' into the *org-scribe-health* buffer.
;; The visual companion draws the same data as its Health tab
;; (`org-scribe-companion--health'), so Emacs and the page cannot disagree.

(defun org-scribe--health-find-orphan-entries (entities referenced-ids)
  "Return the (NAME . ID) of each entity in ENTITIES not in REFERENCED-IDS.
ENTITIES is an alist (NAME . (ID . HEADING)), as the entity getters return."
  (cl-loop for (name . (id . _)) in entities
           unless (gethash id referenced-ids)
           collect (cons name id)))

(defun org-scribe--health-plan-data (structure)
  "Return the Writing Plan section's data for the project of STRUCTURE.
Nil when the section is not shown: the planner is not loaded, or the
project declined it.  Otherwise a plist whose `:state' is `active' (with
`:title', `:target', `:current', `:pct', `:daily', `:end',
`:days-remaining', `:status', `:word-target' and `:outside'),
`not-loaded' (with `:file', the plan file's name), or `none'."
  ;; Skipped entirely when the project has explicitly declined the
  ;; planner (gate = 'no); undecided (nil) keeps prior behavior.
  (when (and (featurep 'org-scribe-planner)
             (not (eq (org-scribe-planner-gate) 'no)))
    ;; The active plan is a session global: drop one that belongs to
    ;; another project (and load this project's own) before reading it.
    (org-scribe-planner--ensure-plan-for-current-project)
    (cond
     ;; Plan is active in memory
     (org-scribe-planner--current-plan
      (let* ((plan org-scribe-planner--current-plan)
             (target (org-scribe-plan-total-words plan))
             (current (org-scribe-plan-current-words plan))
             (end (org-scribe-plan-end-date plan)))
        (list :state 'active
              :title (org-scribe-plan-title plan)
              :target target
              :current current
              :pct (if (> target 0) (* 100.0 (/ (float current) target)) 0.0)
              :daily (org-scribe-plan-daily-words plan)
              :end end
              ;; Counting today, as the dashboards do
              :days-remaining (if (and end (not (string< end (org-scribe-planner--get-today-date))))
                                  (1+ (org-scribe--health-days-until end))
                                0)
              :status (org-scribe--health-plan-status plan)
              :word-target (org-scribe-project-word-target)
              :outside (if (fboundp 'org-scribe-planner--outside-total)
                           (org-scribe-planner--outside-total plan)
                         0))))
     ;; Plan file exists but has not been loaded yet
     ((plist-get structure :plan-file)
      (list :state 'not-loaded
            :file (file-name-nondirectory (plist-get structure :plan-file))))
     ;; No plan at all
     (t (list :state 'none)))))

(defun org-scribe--health-gate-data (design-file scenes)
  "Return the Starting Gate section's data, or nil when there is no gate.
DESIGN-FILE holds the gate; SCENES is `org-scribe--health-collect-scene-data''s
list.  A plist: `:items' ((CHECKED-P . LABEL) ...), `:ticked', `:total',
`:points' (FILLED . TOTAL) or nil, `:scenes' (of the first three, how many
have words), `:notes' (where a tick and its measurement disagree, as text)
and `:crossed' (every item ticked)."
  ;; Only for projects that have a design file with a gate section;
  ;; pre-sistema projects have neither and get no empty section.
  (when-let* ((items (org-scribe--health-gate-items design-file)))
    (let* ((points (org-scribe--health-plot-points-with-content design-file))
           (written (org-scribe--health-scenes-written scenes 3))
           (ticked (seq-count #'car items))
           (notes nil)
           (plot-points-item (org-scribe--health-gate-item 'plot-points items))
           (first-scenes-item (org-scribe--health-gate-item 'first-three-scenes items)))
      ;; Disagreements between the tick and the measurement, in both
      ;; directions.  Shown, not resolved.
      (when (and points plot-points-item)
        (let ((ticked-p (car plot-points-item))
              (done-p (= (car points) (cdr points))))
          (cond ((and ticked-p (not done-p))
                 (push (org-scribe-msg 'msg-health-gate-note-points-ticked
                                       (- (cdr points) (car points))
                                       (cdr points))
                       notes))
                ((and (not ticked-p) done-p)
                 (push (org-scribe-msg 'msg-health-gate-note-points-unticked)
                       notes)))))
      (when first-scenes-item
        (let ((ticked-p (car first-scenes-item))
              (done-p (>= written 3)))
          (cond ((and ticked-p (not done-p))
                 (push (org-scribe-msg 'msg-health-gate-note-scenes-ticked written)
                       notes))
                ((and (not ticked-p) done-p)
                 (push (org-scribe-msg 'msg-health-gate-note-scenes-unticked)
                       notes)))))
      (list :items items
            :ticked ticked
            :total (length items)
            :points points
            :scenes written
            :notes (nreverse notes)
            :crossed (= ticked (length items))))))

(defun org-scribe--health-data ()
  "Return the health report of the current project as a plist.
Everything `org-scribe-project-health' shows, computed once; the texts
in it are in the project's language.  Signals a `user-error' outside a
project or when the project has no manuscript.

Keys: `:manuscript-file', `:root', `:scenes' (see
`org-scribe--health-collect-scene-data'), `:scene-count', `:total-words',
`:total-obj', `:progress' (percent, or nil without an objective),
`:word-target', `:spelling' (a hint, or nil), `:plan'
\(`org-scribe--health-plan-data'), `:gate' (`org-scribe--health-gate-data'),
`:todo-counts' ((STATE . COUNT) ..., in order of appearance; a scene with
no state counts as \"(none)\"), `:pov-share', `:chapters-p' (the project
type has a chapter level), `:chapter-totals', `:chapter-mean',
`:chapter-median', `:missing' (((KEY LABEL . SCENES) ...) for pov, plot and
location, pending scenes only), `:story-time', `:broken-links' (see
`org-scribe--find-broken-links'), `:duplicates' ((KIND-MESSAGE (NAME .
COUNT) ...) ...), `:orphans' (a plist by kind, `:character' `:location'
`:object' `:plot' `:plot-point', of (NAME . ID) lists), `:plot-points-p'
\(the project defines plot points) and `:open' (the pending scenes)."
  (let* (;; The report is written in the project's language, whatever
         ;; `org-scribe-message-language' says.
         (org-scribe-message-language (org-scribe-project-language))
         (structure (org-scribe-project-structure))
         (manuscript-file (plist-get structure :manuscript-file))
         (design-file (plist-get structure :design-file))
         (root (org-scribe-project-root)))
    (unless manuscript-file
      ;; Outside a project, say so rather than "no manuscript in this project".
      (org-scribe--require-project)
      (user-error "%s" (org-scribe-msg 'msg-relink-no-novel)))
    (let* ((scenes (org-scribe--health-collect-scene-data manuscript-file))
           (done-keywords (with-current-buffer (find-file-noselect manuscript-file)
                            org-done-keywords))
           (word-totals (org-scribe--health-word-totals manuscript-file))
           (total-words (car word-totals))
           (total-obj (cdr word-totals))
           (ref-ids (org-scribe--health-collect-referenced-ids manuscript-file))
           (orphans (lambda (getter)
                      (when (fboundp getter)
                        (org-scribe--health-find-orphan-entries (funcall getter) ref-ids))))
           (chapter-totals (org-scribe--health-chapter-word-totals scenes))
           (chapter-words (mapcar #'cdr chapter-totals))
           (todo-counts nil)
           ;; Scenes missing specific properties (only pending scenes)
           (pending (cl-remove-if (lambda (s) (member (nth 3 s) done-keywords))
                                  scenes)))
      (dolist (s scenes)
        (let* ((state (or (nth 3 s) "(none)"))
               (cell (assoc state todo-counts)))
          (if cell (setcdr cell (1+ (cdr cell)))
            (push (cons state 1) todo-counts))))
      (list
       :manuscript-file manuscript-file
       :root root
       :scenes scenes
       :scene-count (length scenes)
       :total-words total-words
       :total-obj total-obj
       :progress (and (> total-obj 0) (* 100.0 (/ (float total-words) total-obj)))
       :word-target (org-scribe-project-word-target)
       :spelling (org-scribe--health-dir-locals-hint)
       :plan (org-scribe--health-plan-data structure)
       ;; Starting Gate: the writer's own ticks, plus the two items that
       ;; can be measured rather than asserted.
       :gate (org-scribe--health-gate-data design-file scenes)
       :todo-counts (nreverse todo-counts)
       ;; Per-PoV word share and chapter length spread (text-level stats)
       :pov-share (org-scribe--health-pov-word-share scenes)
       :chapters-p (and (org-scribe-chapter-level) t)
       :chapter-totals chapter-totals
       :chapter-mean (if chapter-words
                         (/ (float (apply #'+ chapter-words)) (length chapter-words))
                       0.0)
       :chapter-median (org-scribe--health-median chapter-words)
       :missing (list (cons 'pov (cons "PoV" (cl-remove-if (lambda (s) (nth 4 s)) pending)))
                      (cons 'plot (cons "Plot" (cl-remove-if (lambda (s) (nth 6 s)) pending)))
                      (cons 'location (cons "Location"
                                            (cl-remove-if (lambda (s) (nth 7 s)) pending))))
       :story-time (org-scribe--health-story-time-data)
       ;; Links whose entity was deleted: nothing else reports them.
       :broken-links (when (fboundp 'org-scribe--find-broken-links)
                       (with-current-buffer (find-file-noselect manuscript-file)
                         (org-scribe--find-broken-links)))
       ;; Names shared by two entities of one kind: (KIND-KEY . DUPLICATES).
       :duplicates (seq-filter
                    #'cdr
                    (mapcar
                     (lambda (spec)
                       (cons (car spec)
                             (when (fboundp (cdr spec))
                               (org-scribe--health-duplicate-names
                                (funcall (cdr spec))))))
                     '((msg-health-duplicate-kind-character
                        . org-scribe--get-all-characters)
                       (msg-health-duplicate-kind-location
                        . org-scribe--get-all-locations)
                       (msg-health-duplicate-kind-object
                        . org-scribe--get-all-objects)
                       (msg-health-duplicate-kind-plot-thread
                        . org-scribe--get-all-plot-threads))))
       :orphans (list :character (funcall orphans 'org-scribe--get-all-characters)
                      :location (funcall orphans 'org-scribe--get-all-locations)
                      :object (funcall orphans 'org-scribe--get-all-objects)
                      :plot (funcall orphans 'org-scribe--get-all-plot-threads)
                      ;; An orphaned plot point is one of the thirteen
                      ;; non-negotiables that no scene serves — the
                      ;; method's own check, mechanized.
                      :plot-point (funcall orphans 'org-scribe--get-all-plot-points))
       :plot-points-p (and (fboundp 'org-scribe--get-all-plot-points)
                           (org-scribe--get-all-plot-points)
                           t)
       ;; Open (not done) scenes — uses done keywords read from the file
       :open pending))))

;;; Drawing the report

(defun org-scribe--health-render (data)
  "Insert the health report for DATA (`org-scribe--health-data') at point.
The current buffer is expected to be in `org-mode'."
  (let* ((org-scribe-message-language (let ((default-directory
                                              (file-name-as-directory
                                               (or (plist-get data :root)
                                                   (file-name-directory
                                                    (plist-get data :manuscript-file))))))
                                        (org-scribe-project-language)))
         (manuscript-file (plist-get data :manuscript-file))
         (total-words (plist-get data :total-words))
         (total-obj (plist-get data :total-obj))
         (progress (plist-get data :progress))
         (word-target (plist-get data :word-target))
         (plan (plist-get data :plan))
         (gate (plist-get data :gate))
         (chapter-totals (plist-get data :chapter-totals))
         (chapter-words (mapcar #'cdr chapter-totals))
         (chapter-mean (plist-get data :chapter-mean))
         (broken-links (plist-get data :broken-links))
         (duplicates (plist-get data :duplicates))
         (orphans (plist-get data :orphans))
         (open-todos (plist-get data :open))
         (orphan-list (lambda (entries heading none)
                        (insert (format "** %s\n\n" (org-scribe-msg heading (length entries))))
                        (if entries
                            (dolist (entry entries) (insert (format "- %s\n" (car entry))))
                          (insert (org-scribe-msg none) "\n")))))

    ;; Header
    (insert "#+TITLE: " (org-scribe-msg 'msg-health-title) "\n")
    (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M")))
    (insert "#+STARTUP: overview\n\n")

    ;; ── Overview ─────────────────────────────────────────────────────────
    (insert (format "* %s\n\n" (org-scribe-msg 'msg-health-overview-heading)))
    (insert (format "- %s :: [[file:%s][%s]]\n"
                    (org-scribe-msg 'msg-health-label-manuscript)
                    manuscript-file (file-name-nondirectory manuscript-file)))
    (insert (format "- %s :: %d\n" (org-scribe-msg 'msg-health-label-total-scenes)
                    (plist-get data :scene-count)))
    (insert (format "- %s :: %d\n" (org-scribe-msg 'msg-health-label-words-written) total-words))
    (when word-target
      (insert (format "- %s\n" (org-scribe-msg 'msg-health-word-target word-target)))
      ;; Reported, not resolved: the parts and the target are the
      ;; writer's to reconcile (F8 t)
      (when (and (> total-obj 0) (/= total-obj word-target))
        (insert (format "- %s\n" (org-scribe-msg 'msg-health-word-target-drift
                                                 total-obj word-target)))))
    (if progress
        (progn
          ;; Same number as the project's word target: one line says it.
          (unless (eql total-obj word-target)
            (insert (format "- %s :: %d\n"
                            (org-scribe-msg 'msg-health-label-word-objective) total-obj)))
          (insert (format "- %s :: %.1f%%\n"
                          (org-scribe-msg 'msg-health-label-progress) progress)))
      (insert (format "- %s :: /%s/\n"
                      (org-scribe-msg 'msg-health-label-word-objective)
                      (org-scribe-msg 'msg-health-not-set))))
    (when-let* ((hint (plist-get data :spelling)))
      (insert (format "- %s :: %s\n" (org-scribe-msg 'msg-health-label-spelling) hint)))
    (insert "\n")

    ;; ── Writing Plan ─────────────────────────────────────────────────────
    (when plan
      (insert (format "* %s\n\n" (org-scribe-msg 'msg-health-plan-heading)))
      (pcase (plist-get plan :state)
        ('active
         (let ((target (plist-get plan :target))
               (plan-word-target (plist-get plan :word-target))
               (outside (plist-get plan :outside)))
           (insert "- " (org-scribe-msg 'msg-health-plan-title (plist-get plan :title)) "\n")
           (insert "- " (org-scribe-msg 'msg-health-plan-target target) "\n")
           (insert "- " (org-scribe-msg 'msg-health-plan-current
                                        (plist-get plan :current) (plist-get plan :pct))
                   "\n")
           (insert "- " (org-scribe-msg 'msg-health-plan-daily (plist-get plan :daily)) "\n")
           (insert "- " (org-scribe-msg 'msg-health-plan-end (plist-get plan :end)) "\n")
           (insert "- " (org-scribe-msg 'msg-health-plan-days-remaining
                                        (plist-get plan :days-remaining))
                   "\n")
           ;; Expected words run through the end of today: say so.
           (insert "- " (org-scribe-msg 'msg-health-plan-status (plist-get plan :status)) "\n")
           (when (and plan-word-target (/= target plan-word-target))
             (insert (format "- %s\n" (org-scribe-msg 'msg-health-plan-target-mismatch
                                                      target plan-word-target))))
           (when (> outside 0)
             (insert (format "- %s\n" (org-scribe-msg 'msg-health-plan-outside outside))))))
        ('not-loaded
         (insert (org-scribe-msg 'msg-health-plan-not-loaded (plist-get plan :file)) "\n")
         (insert (org-scribe-msg 'msg-health-plan-load-hint) "\n"))
        (_ (insert (org-scribe-msg 'msg-health-plan-none) "\n")))
      (insert "\n"))

    ;; ── Starting Gate ─────────────────────────────────────────────────────
    (when gate
      (let ((points (plist-get gate :points))
            (notes (plist-get gate :notes)))
        (insert (format "* %s\n\n" (org-scribe-msg 'msg-health-gate-heading
                                                   (plist-get gate :ticked)
                                                   (plist-get gate :total))))
        (insert (org-scribe-msg 'msg-health-gate-intro) "\n\n")
        (dolist (item (plist-get gate :items))
          (insert (format "- [%s] %s\n" (if (car item) "X" " ") (cdr item))))
        (insert "\n")
        ;; The two measurable items, reported whether or not they are ticked.
        (when points
          (insert (org-scribe-msg 'msg-health-gate-points (car points) (cdr points)) "\n"))
        (insert (org-scribe-msg 'msg-health-gate-scenes (plist-get gate :scenes)) "\n")
        (when notes
          (insert "\n" (org-scribe-msg 'msg-health-gate-worth-a-look
                                       (mapconcat #'identity notes "; "))
                  "\n"))
        (when (plist-get gate :crossed)
          (insert "\n" (org-scribe-msg 'msg-health-gate-crossed) "\n"))
        (insert "\n")))

    ;; ── Scene status breakdown ────────────────────────────────────────────
    (insert (format "* %s\n\n" (org-scribe-msg 'msg-health-status-heading)))
    (insert (org-scribe-msg 'msg-health-status-table-header) "\n")
    (insert "|--------+-------|\n")
    (dolist (cell (plist-get data :todo-counts))
      (insert (format "| %-6s | %5d |\n" (car cell) (cdr cell))))
    (insert "\n")

    ;; ── Per-PoV word share ───────────────────────────────────────────────
    (insert (format "* %s\n\n" (org-scribe-msg 'msg-health-pov-word-share-heading)))
    (insert (format "%s\n" (org-scribe-msg 'msg-health-pov-word-share-table-header)))
    (insert "|-----+--------+-------+------------|\n")
    (dolist (row (plist-get data :pov-share))
      (let* ((words (nth 2 row))
             (pct (if (> total-words 0) (* 100.0 (/ (float words) total-words)) 0.0)))
        (insert (format "| %s | %d | %d | %.1f%% |\n" (nth 0 row) (nth 1 row) words pct))))
    (insert "\n")

    ;; ── Chapter length spread ────────────────────────────────────────────
    ;; Only for a project type with a real chapter level (a novel).  A
    ;; short story has no chapter level at all -- `chapter-totals' would
    ;; still compute one pseudo-"chapter" per scene's level-1 container
    ;; (the manuscript's own wrapper heading), which is not a chapter
    ;; and has nothing to spread.
    (when (plist-get data :chapters-p)
      (insert (format "* %s\n\n" (org-scribe-msg 'msg-health-chapter-length-heading)))
      (insert (format "%s\n" (org-scribe-msg 'msg-health-chapter-length-table-header)))
      (insert "|---------+-------|\n")
      (dolist (entry chapter-totals)
        (insert (format "| %s | %d%s |\n" (car entry) (cdr entry)
                        (if (org-scribe--health-chapter-outlier-p (cdr entry) chapter-mean)
                            " *" ""))))
      (insert "\n")
      (insert (format "%s\n\n"
                      (org-scribe-msg 'msg-health-chapter-length-summary
                                      (if chapter-words (apply #'min chapter-words) 0)
                                      (if chapter-words (apply #'max chapter-words) 0)
                                      chapter-mean
                                      (plist-get data :chapter-median))))
      (when (cl-some (lambda (entry)
                       (org-scribe--health-chapter-outlier-p (cdr entry) chapter-mean))
                     chapter-totals)
        (insert (format "%s\n\n" (org-scribe-msg 'msg-health-chapter-length-outlier-legend)))))

    ;; ── Missing properties ────────────────────────────────────────────────
    (insert (format "* %s\n\n" (org-scribe-msg 'msg-health-missing-heading)))
    (dolist (entry (plist-get data :missing))
      (org-scribe--health-insert-missing-list (cadr entry) (cddr entry) (cadr entry)))

    ;; ── Story time ────────────────────────────────────────────────────────
    (org-scribe--health-insert-story-time (plist-get data :story-time))

    ;; ── Broken links ──────────────────────────────────────────────────────
    (insert (format "* %s\n\n" (org-scribe-msg 'msg-health-broken-links-heading
                                               (length broken-links))))
    (if (null broken-links)
        (insert (org-scribe-msg 'msg-health-broken-links-none) "\n\n")
      (insert (org-scribe-msg 'msg-health-broken-links-intro) "\n\n")
      (dolist (b broken-links)
        (insert (format "- %s :: %s → %s\n"
                        (org-scribe--health-scene-link (plist-get b :heading)
                                                       (plist-get b :scene-id))
                        (plist-get b :property)
                        (plist-get b :name))))
      (insert "\n"))

    ;; ── Duplicate names (only when there are any) ─────────────────────────
    (when duplicates
      (insert (format "* %s\n\n"
                      (org-scribe-msg 'msg-health-duplicate-names-heading
                                      (apply #'+ (mapcar (lambda (d) (length (cdr d)))
                                                         duplicates)))))
      (insert (org-scribe-msg 'msg-health-duplicate-names-intro) "\n\n")
      (dolist (kind duplicates)
        (dolist (dup (cdr kind))
          (insert (format "- %s :: %s (×%d)\n"
                          (org-scribe-msg (car kind)) (car dup) (cdr dup)))))
      (insert "\n"))

    ;; ── Orphaned entities ─────────────────────────────────────────────────
    (insert (format "* %s\n\n" (org-scribe-msg 'msg-health-orphans-heading)))
    (insert (org-scribe-msg 'msg-health-orphans-intro) "\n\n")
    (funcall orphan-list (plist-get orphans :character)
             'msg-health-orphan-characters 'msg-health-orphan-characters-none)
    (insert "\n")
    (funcall orphan-list (plist-get orphans :location)
             'msg-health-orphan-locations 'msg-health-orphan-locations-none)
    (insert "\n")
    (funcall orphan-list (plist-get orphans :object)
             'msg-health-orphan-objects 'msg-health-orphan-objects-none)
    (insert "\n")
    (funcall orphan-list (plist-get orphans :plot)
             'msg-health-orphan-threads 'msg-health-orphan-threads-none)
    (insert "\n")

    ;; Only shown when the project actually defines plot points, so
    ;; projects created from the pre-sistema templates do not grow an
    ;; empty section reporting nothing.
    (when (plist-get data :plot-points-p)
      (let ((points (plist-get orphans :plot-point)))
        (insert (format "** %s\n\n" (org-scribe-msg 'msg-health-orphan-points (length points))))
        (insert (org-scribe-msg 'msg-health-orphan-points-intro) "\n\n")
        (if points
            (dolist (entry points) (insert (format "- %s\n" (car entry))))
          (insert (org-scribe-msg 'msg-health-orphan-points-none) "\n"))
        (insert "\n")))

    ;; ── Open TODOs ────────────────────────────────────────────────────────
    ;; The "(chapter: ...)" suffix only makes sense for a project type
    ;; with a real chapter level; for a short story it would print the
    ;; manuscript's own level-1 container heading (e.g. "Story Content")
    ;; on every single line, which names nothing useful.
    (insert (format "* %s\n\n" (org-scribe-msg 'msg-health-open-heading
                                               (length open-todos))))
    (if (null open-todos)
        (insert (org-scribe-msg 'msg-health-open-none) "\n")
      (dolist (s open-todos)
        (if (plist-get data :chapters-p)
            (insert (format "- [%s] %s  %s\n"
                            (or (nth 3 s) "?")
                            (org-scribe--health-scene-link (nth 0 s) (nth 2 s))
                            (org-scribe-msg 'msg-health-chapter-suffix (or (nth 1 s) "?"))))
          (insert (format "- [%s] %s\n"
                          (or (nth 3 s) "?")
                          (org-scribe--health-scene-link (nth 0 s) (nth 2 s)))))))))

(defun org-scribe--health-chapter-outlier-p (words mean)
  "Non-nil when a chapter of WORDS is over twice MEAN or under half of it."
  (and (> mean 0)
       (or (> words (* 2 mean))
           (< words (* 0.5 mean)))))

;;; Main Report Function

;;;###autoload
(defun org-scribe-project-health ()
  "Generate a project health report for the current writing project.
Displays an overview of word-count progress, scene completion by TODO
state, scenes with missing critical properties, orphaned characters and
locations, and a list of all open TODO scenes.

The report opens in the *org-scribe-health* buffer as an Org-mode file
with clickable ID links back to each scene.  Its data comes from
`org-scribe--health-data', which the visual companion's Health tab
draws too."
  (interactive)
  ;; Collect all data before opening the buffer
  (let* ((data (org-scribe--health-data))
         (root (plist-get data :root)))
    (with-current-buffer (get-buffer-create "*org-scribe-health*")
      (let ((inhibit-read-only t))
        (erase-buffer))
      (org-mode)
      ;; The buffer is reused across projects and keeps the directory it
      ;; was created in; the lookups below (plan, gate, word target, plot
      ;; points) resolve the project from it, so point it at this one.
      (setq default-directory
            (file-name-as-directory
             (or root (file-name-directory (plist-get data :manuscript-file)))))
      (org-scribe--health-render data)
      ;; The tables are inserted row by row; align them once, here.
      (org-table-map-tables #'org-table-align t)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer))
      (let ((org-scribe-message-language (org-scribe-project-language)))
        (message "%s" (org-scribe-msg 'msg-health-generated))))))

(provide 'org-scribe-health)

;;; org-scribe-health.el ends here
