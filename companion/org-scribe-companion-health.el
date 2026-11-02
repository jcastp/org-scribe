;;; org-scribe-companion-health.el --- Visual companion: the Health tab -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; The data behind the page's Health tab, served on GET /api/health (not
;; part of every snapshot: it costs a scan of both files and is wanted only
;; while that tab is open), and the two operations its findings use:
;;   remove-link   drop a link to a deleted entity from one scene property
;;   show-health   open the full report in Emacs (`org-scribe-project-health')
;;
;; The data is `org-scribe--health-data', the same plist the Emacs report is
;; drawn from, turned into JSON: the page and the report cannot disagree.
;; Counts and names travel as data, for the page to word in its own strings;
;; the few sentences the report composes (the Starting Gate notes, the plan's
;; status, the spelling hint, the chapter-length summary) travel as text in
;; the project's language, with their Org markup taken out.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)
(require 'org-scribe-messages)
(require 'org-scribe-core)
(require 'org-scribe-companion-model)
(require 'org-scribe-companion-commands)

(declare-function org-scribe--health-data "org-scribe-health")
(declare-function org-scribe-project-health "org-scribe-health")
(declare-function org-scribe--health-chapter-outlier-p "org-scribe-health")
(defvar org-scribe-companion-raise-frame)

;;; Text

(defun org-scribe-companion-health--plain (text)
  "Return TEXT, a report line, without its Org markup.
Emphasis markers (=code=, *bold*, /italic/, ~verbatim~) go, and a
description-list separator \" :: \" becomes \": \"."
  (when (stringp text)
    (let ((case-fold-search nil))
      (replace-regexp-in-string
       " :: " ": "
       (replace-regexp-in-string
        "\\(^\\|[[:space:](\"']\\)\\([=*/~]\\)\\([^[:space:]]\\|[^[:space:]].*?[^[:space:]]\\)\\2\\($\\|[[:space:].,;:!?)\"']\\)"
        "\\1\\3\\4" text)))))

(defun org-scribe-companion-health--or-null (value)
  "VALUE, or :null when it is nil (`json-serialize' writes nil as {})."
  (if (null value) :null value))

;;; JSON

(defun org-scribe-companion-health--scene (scene)
  "Return a JSON object for SCENE, an element of the report's scene list."
  (list :id (org-scribe-companion-health--or-null (nth 2 scene))
        :title (nth 0 scene)
        :chapter (org-scribe-companion-health--or-null (nth 1 scene))
        :state (org-scribe-companion-health--or-null (nth 3 scene))))

(defun org-scribe-companion-health--time-entry (entry &optional extra)
  "Return a JSON object for the story-time ENTRY, with EXTRA appended."
  (append (list :id (org-scribe-companion-health--or-null (plist-get entry :id))
                :title (or (plist-get entry :heading) "")
                :text (org-scribe-companion-health--or-null (plist-get entry :text)))
          extra))

(defun org-scribe-companion-health--entities (entries)
  "Return a vector of {name, id} for ENTRIES, a list of (NAME . ID)."
  (vconcat (mapcar (lambda (entry) (list :name (car entry) :id (cdr entry))) entries)))

(defun org-scribe-companion-health--plan (plan)
  "Return the Writing Plan as JSON: its state and its lines as plain text."
  (if (null plan)
      :null
    (let ((lines
           (pcase (plist-get plan :state)
             ('active
              (let ((target (plist-get plan :target))
                    (word-target (plist-get plan :word-target))
                    (outside (plist-get plan :outside)))
                (delq nil
                      (list (org-scribe-msg 'msg-health-plan-title (plist-get plan :title))
                            (org-scribe-msg 'msg-health-plan-target target)
                            (org-scribe-msg 'msg-health-plan-current
                                            (plist-get plan :current) (plist-get plan :pct))
                            (org-scribe-msg 'msg-health-plan-daily (plist-get plan :daily))
                            (org-scribe-msg 'msg-health-plan-end (plist-get plan :end))
                            (org-scribe-msg 'msg-health-plan-days-remaining
                                            (plist-get plan :days-remaining))
                            (org-scribe-msg 'msg-health-plan-status (plist-get plan :status))
                            (and word-target (/= target word-target)
                                 (org-scribe-msg 'msg-health-plan-target-mismatch
                                                 target word-target))
                            (and (> outside 0)
                                 (org-scribe-msg 'msg-health-plan-outside outside))))))
             ('not-loaded
              (list (org-scribe-msg 'msg-health-plan-not-loaded (plist-get plan :file))
                    (org-scribe-msg 'msg-health-plan-load-hint)))
             (_ (list (org-scribe-msg 'msg-health-plan-none))))))
      (list :state (symbol-name (plist-get plan :state))
            :lines (vconcat (mapcar #'org-scribe-companion-health--plain lines))))))

(defun org-scribe-companion-health--gate (gate)
  "Return the Starting Gate as JSON, or :null."
  (if (null gate)
      :null
    (let ((points (plist-get gate :points)))
      (list :ticked (plist-get gate :ticked)
            :total (plist-get gate :total)
            :items (vconcat (mapcar (lambda (item)
                                      (list :checked (if (car item) t :false)
                                            :label (cdr item)))
                                    (plist-get gate :items)))
            :points (if points (vector (car points) (cdr points)) :null)
            :scenes (plist-get gate :scenes)
            :notes (vconcat (mapcar #'org-scribe-companion-health--plain
                                    (plist-get gate :notes)))
            :crossed (if (plist-get gate :crossed) t :false)))))

(defun org-scribe-companion-health--chapters (data)
  "Return the chapter length spread of DATA as JSON, or :null."
  (if (not (plist-get data :chapters-p))
      :null
    (let* ((totals (plist-get data :chapter-totals))
           (words (mapcar #'cdr totals))
           (mean (plist-get data :chapter-mean)))
      (list :rows (vconcat
                   (mapcar (lambda (entry)
                             (list :title (car entry) :words (cdr entry)
                                   :outlier (if (org-scribe--health-chapter-outlier-p
                                                 (cdr entry) mean)
                                                t :false)))
                           totals))
            :summary (org-scribe-msg 'msg-health-chapter-length-summary
                                     (if words (apply #'min words) 0)
                                     (if words (apply #'max words) 0)
                                     mean
                                     (plist-get data :chapter-median))))))

(defun org-scribe-companion-health--story-time (time)
  "Return the Story Time findings TIME as JSON."
  (let ((scenes (lambda (key)
                  (vconcat (mapcar #'org-scribe-companion-health--time-entry
                                   (plist-get time key))))))
    (list :total (plist-get time :total)
          :missing (funcall scenes :missing)
          :unreadable (funcall scenes :unreadable)
          :noAnchor (funcall scenes :no-anchor)
          :incomplete (funcall scenes :incomplete)
          :jumps (vconcat (mapcar (lambda (jump)
                                    (org-scribe-companion-health--time-entry
                                     (car jump) (list :distance (cdr jump))))
                                  (plist-get time :jumps))))))

(defun org-scribe-companion-health--json (data)
  "Return the health report DATA (`org-scribe--health-data') as JSON."
  (let ((orphans (plist-get data :orphans))
        (total-obj (plist-get data :total-obj))
        (word-target (plist-get data :word-target)))
    (list
     :overview (list :scenes (plist-get data :scene-count)
                     :words (plist-get data :total-words)
                     :objective total-obj
                     :progress (org-scribe-companion-health--or-null (plist-get data :progress))
                     :wordTarget (org-scribe-companion-health--or-null word-target)
                     ;; Reported, not resolved: the writer reconciles them (F8 t)
                     :drift (if (and word-target (> total-obj 0) (/= total-obj word-target))
                                (org-scribe-companion-health--plain
                                 (org-scribe-msg 'msg-health-word-target-drift
                                                 total-obj word-target))
                              :null)
                     :spelling (org-scribe-companion-health--or-null
                                (org-scribe-companion-health--plain (plist-get data :spelling))))
     :plan (org-scribe-companion-health--plan (plist-get data :plan))
     :gate (org-scribe-companion-health--gate (plist-get data :gate))
     :status (vconcat (mapcar (lambda (cell) (list :state (car cell) :count (cdr cell)))
                              (plist-get data :todo-counts)))
     :povShare (vconcat (mapcar (lambda (row)
                                  (list :name (nth 0 row) :scenes (nth 1 row) :words (nth 2 row)))
                                (plist-get data :pov-share)))
     :chapters (org-scribe-companion-health--chapters data)
     :missing (vconcat (mapcar (lambda (entry)
                                 (list :key (symbol-name (car entry))
                                       :name (cadr entry)
                                       :scenes (vconcat (mapcar #'org-scribe-companion-health--scene
                                                                (cddr entry)))))
                               (plist-get data :missing)))
     :storyTime (org-scribe-companion-health--story-time (plist-get data :story-time))
     :broken (vconcat (mapcar (lambda (b)
                                (list :id (org-scribe-companion-health--or-null (plist-get b :scene-id))
                                      :title (plist-get b :heading)
                                      :key (if (plist-get b :key) (symbol-name (plist-get b :key)) :null)
                                      :property (plist-get b :property)
                                      :name (plist-get b :name)
                                      :target (plist-get b :target)))
                              (plist-get data :broken-links)))
     :duplicates (vconcat (cl-loop for (kind . dups) in (plist-get data :duplicates)
                                   append (mapcar (lambda (dup)
                                                    (list :kind (org-scribe-msg kind)
                                                          :name (car dup) :count (cdr dup)))
                                                  dups)))
     :orphans (list :character (org-scribe-companion-health--entities (plist-get orphans :character))
                    :location (org-scribe-companion-health--entities (plist-get orphans :location))
                    :object (org-scribe-companion-health--entities (plist-get orphans :object))
                    :plot (org-scribe-companion-health--entities (plist-get orphans :plot))
                    :plotPoint (if (plist-get data :plot-points-p)
                                   (org-scribe-companion-health--entities
                                    (plist-get orphans :plot-point))
                                 :null))
     :open (length (plist-get data :open)))))

(defun org-scribe-companion-health-get (root)
  "Return the answer to GET /api/health for the project at ROOT.
\(:ok t :revision REV :health DATA), or a `not-found' error when the
project has no manuscript."
  (require 'org-scribe-health)
  (let* ((default-directory (file-name-as-directory (expand-file-name root)))
         (org-scribe-message-language (org-scribe-project-language)))
    (condition-case err
        ;; the scan visits the files: the revision is read after it
        (let ((health (org-scribe-companion-health--json (org-scribe--health-data))))
          (list :ok t
                :revision (org-scribe-companion--revision-string default-directory)
                :health health))
      (user-error (org-scribe-companion-commands-error
                   "not-found" (error-message-string err)))
      ;; any other failure is still an answer: closing the socket left the
      ;; page with a bare "failed" and the cause only in *Messages*
      (error (org-scribe-companion-commands-error
              "internal" (error-message-string err))))))

;;; remove-link

(defun org-scribe-companion-health--without-link (text target)
  "Return the property TEXT with every link to the ID TARGET taken out.
The comma-separated list that is left keeps its other items as written."
  (let ((case-fold-search nil)
        (link (concat "\\[\\[id:" (regexp-quote target) "\\]\\(?:\\[[^]]*\\]\\)?\\]")))
    (string-trim
     (replace-regexp-in-string
      "\\`[ \t]*,[ \t]*\\|[ \t]*,[ \t]*\\'" ""
      (replace-regexp-in-string
       "[ \t]*,\\(?:[ \t]*,\\)+[ \t]*" ", "
       (replace-regexp-in-string link "" text t t)
       t t)
      t t))))

(defun org-scribe-companion-health--remove-link (root args)
  "Remove a link to a deleted entity from a scene.  ARGS: `:id' (the scene),
`:key' (the canonical property, such as \"characters\"), `:target' (the
dead ID).  One undo step; the property's other items stay as written."
  (let* ((buffer (org-scribe-companion-commands--manuscript-buffer root))
         (id (org-scribe-companion-commands--require-id args :id))
         (target (org-scribe-companion-commands--require-id args :target))
         (key-name (org-scribe-companion-commands--arg-string args :key))
         (key (and key-name (intern-soft key-name)))
         (node (org-scribe-companion-commands--heading root buffer id)))
    (unwind-protect
        (let ((default-directory root))
          (unless (and key (assq key (org-scribe-companion--field-specs)))
            (org-scribe-companion-commands--fail "invalid" 'companion-error-invalid-key
                                                 (or key-name "")))
          (let* ((current (org-scribe-companion-commands--at node
                            (org-scribe-scene-property-get key)))
                 (value (and current
                             (org-scribe-companion-health--without-link current target))))
            (unless (and current (not (equal value current)))
              (org-scribe-companion-commands--fail "conflict" 'companion-error-link-gone target))
            (org-scribe-companion-commands--with-edit buffer
              (org-scribe-companion-commands--at node
                (org-scribe-scene-property-set key value)))
            (org-scribe-companion-commands--finish root)))
      (set-marker node nil))))

;;; show-health

(defun org-scribe-companion-health--show (root _args)
  "Open the full health report in Emacs for the project at ROOT."
  (require 'org-scribe-health)
  (let ((default-directory (file-name-as-directory (expand-file-name root))))
    (org-scribe-project-health)
    (when org-scribe-companion-raise-frame
      (select-frame-set-input-focus (selected-frame)))
    (org-scribe-companion-commands--ok root)))

(provide 'org-scribe-companion-health)

;;; org-scribe-companion-health.el ends here
