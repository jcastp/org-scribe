;;; org-scribe-plot-links.el --- Plot thread linking system for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides ID-based linking system for plot threads in writing projects.
;; Automatically creates unique IDs for plot threads and provides functions
;; to insert plot thread links in scene properties.
;;
;; Features:
;; - Auto-create IDs when capturing new plot threads
;; - Insert plot thread links with completion
;; - Link multiple plot threads at once
;; - Jump to plot thread definition from properties
;; - Update all plot thread links in document

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-scribe-core)
(require 'org-scribe-capture)

;;; Plot Thread ID Management

(defun org-scribe--ensure-plot-thread-has-id ()
  "Ensure the current plot thread heading has a unique ID.
Creates an ID if one doesn't exist. Returns the ID."
  (org-id-get-create))

(defun org-scribe--add-id-to-all-plot-threads ()
  "Add IDs to all plot thread headings in current buffer.
This function scans the plot file and ensures every
plot thread heading has a unique ID property."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (org-map-entries
       (lambda ()
         (let* ((level (org-current-level))
                (parent-props (org-entry-properties nil "TYPE"))
                (type (cdr (assoc "TYPE" parent-props)))
                (heading (org-get-heading t t t t))
                (is-plot-thread-heading
                 (and (>= level 1)  ; Level 1 or deeper
                      (or type  ; Has TYPE property
                          ;; Or is under "Plot" or similar heading
                          ;; Main Plot, Subplots, or has keywords
                          (string-match-p "\\(Main Plot\\|Subplot\\|Thread\\|A-[Pp]lot\\|B-[Pp]lot\\|C-[Pp]lot\\)"
                                         heading)))))
           (when is-plot-thread-heading
             (unless (org-id-get)
               (org-id-get-create)
               (setq count (1+ count))))))
       nil 'file)
      (message "Added IDs to %d plot thread heading%s"
               count (if (= count 1) "" "s")))))

(defun org-scribe--get-plot-thread-name-at-point ()
  "Get the plot thread name from current heading or NAME property."
  (or (org-entry-get nil "NAME")
      (org-get-heading t t t t)))

;;; Plot Thread Database Functions

(defun org-scribe--get-plot-thread-file ()
  "Get the path to the plot threads file for the current project.
For novels, this is objects/plot.org.
For short stories, this is notes.org (Plot section)."
  (let* ((project-root (org-scribe-project-root))
         (project-type (org-scribe-project-type))
         (structure (org-scribe-project-structure)))
    (cond
     ;; Novel project - use objects/plot.org
     ((eq project-type 'novel)
      (let ((plot-file (expand-file-name "objects/plot.org" project-root)))
        (when (file-exists-p plot-file)
          plot-file)))
     ;; Short story - use notes.org
     ((eq project-type 'short-story)
      (plist-get structure :notes-file))
     ;; Unknown - try objects/plot.org
     (t
      (let ((plot-file (expand-file-name "objects/plot.org" project-root)))
        (when (file-exists-p plot-file)
          plot-file))))))

(defun org-scribe--get-all-plot-threads ()
  "Return alist of (THREAD-NAME . (ID . HEADING)) from plot file.
Returns list of (NAME . (ID . HEADING-TEXT)) for all plot threads in the project."
  (let ((plot-file (org-scribe--get-plot-thread-file))
        result)
    (when (and plot-file (file-exists-p plot-file))
      (with-current-buffer (find-file-noselect plot-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (let* ((level (org-current-level))
                   (id (org-id-get))
                   (name (org-scribe--get-plot-thread-name-at-point))
                   (heading (org-get-heading t t t t))
                   ;; Check if this looks like a plot thread heading
                   (is-plot-thread
                    (and (>= level 1)  ; Level 1 or deeper
                         id            ; Has an ID
                         name          ; Has a name
                         ;; Match plot-related headings
                         (or (org-entry-get nil "TYPE")
                             (org-entry-get nil "THREAD-TYPE")
                             (string-match-p "\\(Main Plot\\|Subplot\\|Thread\\|A-[Pp]lot\\|B-[Pp]lot\\|C-[Pp]lot\\)"
                                            heading)))))
              (when is-plot-thread
                (push (cons name (cons id heading)) result))))
          nil 'file))))
    (nreverse result)))

(defun org-scribe--create-plot-thread-link (thread-name id-alist)
  "Create an ID link for THREAD-NAME using ID-ALIST.
ID-ALIST should be in format ((NAME . (ID . HEADING)) ...).
Returns the link string or plain text if no ID found."
  (if-let* ((entry (assoc thread-name id-alist))
            (id (cadr entry)))
      (format "[[id:%s][%s]]" id thread-name)
    thread-name)) ; Fallback to plain text if no ID found

;;; Interactive Functions

;;;###autoload
(defun org-scribe/add-plot-thread-ids ()
  "Add unique IDs to all plot threads in the plot file.
This should be run once on existing projects to set up
the ID-based linking system."
  (interactive)
  (let ((plot-file (org-scribe--get-plot-thread-file)))
    (if (not (file-exists-p plot-file))
        (message "No plot file found. Create plot threads first.")
      (with-current-buffer (find-file-noselect plot-file)
        (org-scribe--add-id-to-all-plot-threads)
        (save-buffer)
        (message "Plot thread IDs updated in %s" plot-file)))))

;;;###autoload
(defun org-scribe/insert-plot-thread-link ()
  "Insert a plot thread link in the current property.
Scans the plot file, presents a completion menu,
and inserts the selected plot thread as an ID link.

Use this function when adding plot threads to scene properties."
  (interactive)
  (let* ((threads (org-scribe--get-all-plot-threads))
         (thread-names (mapcar #'car threads)))
    (if (null thread-names)
        (message "No plot threads found. Create plot threads first or add IDs with org-scribe/add-plot-thread-ids.")
      (let* ((selected (completing-read "Select plot thread: " thread-names nil t))
             (entry (assoc selected threads))
             (id (cadr entry)))
        (if id
            (progn
              (insert (format "[[id:%s][%s]]" id selected))
              (message "Inserted link to %s" selected))
          (message "No ID found for %s" selected))))))

;;;###autoload
(defun org-scribe/insert-multiple-plot-thread-links ()
  "Insert multiple plot thread links separated by commas.
Useful for the :Plot: property which often lists
multiple plot threads in a scene."
  (interactive)
  (let* ((threads (org-scribe--get-all-plot-threads))
         (thread-names (mapcar #'car threads))
         selected-threads
         links)
    (if (null thread-names)
        (message "No plot threads found. Create plot threads first or add IDs with org-scribe/add-plot-thread-ids.")
      ;; Multiple selection loop
      (while (let ((choice (completing-read
                           "Select plot thread (RET to finish): "
                           thread-names nil nil)))
               (when (and choice (not (string-empty-p choice)))
                 (push choice selected-threads)
                 t)))

      ;; Create links for all selected plot threads
      (setq selected-threads (nreverse selected-threads))
      (dolist (name selected-threads)
        (if-let* ((entry (assoc name threads))
                  (id (cadr entry)))
            (push (format "[[id:%s][%s]]" id name) links)
          (push name links)))

      (setq links (nreverse links))
      (if links
          (progn
            (insert (string-join links ", "))
            (message "Inserted %d plot thread link%s"
                    (length links)
                    (if (= (length links) 1) "" "s")))
        (message "No plot threads selected")))))

;;;###autoload
(defun org-scribe/set-scene-plot-threads ()
  "Set the Plot property to multiple plot thread ID links.
Specifically designed for the :Plot: property in scene headings."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (let* ((threads (org-scribe--get-all-plot-threads))
         (thread-names (mapcar #'car threads))
         selected-threads
         links)
    (if (null thread-names)
        (message "No plot threads found. Create plot threads first or add IDs with org-scribe/add-plot-thread-ids.")
      ;; Multiple selection loop
      (while (let ((choice (completing-read
                           "Select plot thread (RET to finish): "
                           thread-names nil nil)))
               (when (and choice (not (string-empty-p choice)))
                 (push choice selected-threads)
                 t)))

      ;; Create links for all selected plot threads
      (setq selected-threads (nreverse selected-threads))
      (dolist (name selected-threads)
        (if-let* ((entry (assoc name threads))
                  (id (cadr entry)))
            (push (format "[[id:%s][%s]]" id name) links)
          (push name links)))

      (setq links (nreverse links))
      (if links
          (progn
            (org-set-property "Plot" (string-join links ", "))
            (message "Set Plot to: %s" (string-join selected-threads ", ")))
        (message "No plot threads selected")))))

;;;###autoload
(defun org-scribe/jump-to-plot-thread ()
  "Jump to plot thread definition from scene.
If Plot property has multiple threads, prompts for selection."
  (interactive)
  (let* ((plot-prop (org-entry-get nil "Plot"))
         (thread-list (when plot-prop
                       (org-scribe--property-to-list plot-prop))))
    (cond
     ((null plot-prop)
      (message "No Plot property in current heading"))
     ((null thread-list)
      (message "No plot threads found in Plot property"))
     ((= (length thread-list) 1)
      ;; Single thread - jump directly
      (let ((thread-name (car thread-list)))
        ;; Extract ID from the property if it's a link
        (if (string-match "\\[\\[id:\\([^]]+\\)\\]\\[" plot-prop)
            (org-id-goto (match-string 1 plot-prop))
          (message "Plot thread '%s' is not an ID link" thread-name))))
     (t
      ;; Multiple threads - prompt for selection
      (let* ((selected (completing-read "Jump to plot thread: " thread-list nil t))
             (threads (org-scribe--get-all-plot-threads))
             (entry (assoc selected threads))
             (id (cadr entry)))
        (if id
            (org-id-goto id)
          (message "No ID found for plot thread '%s'" selected)))))))

;;; Batch Update Functions

(defun org-scribe--link-plot-threads-in-property (property-name)
  "Convert plot thread names to ID links in PROPERTY-NAME of current heading.
Handles both single threads and comma-separated lists."
  (when-let* ((prop-value (org-entry-get nil property-name)))
    (let* ((id-alist (org-scribe--get-all-plot-threads))
           ;; Split on comma, trim whitespace
           (thread-list (mapcar #'string-trim
                                (split-string prop-value "," t)))
           ;; Create links for each plot thread
           (linked-threads (mapcar (lambda (name)
                                     (org-scribe--create-plot-thread-link name id-alist))
                                   thread-list))
           (linked-string (string-join linked-threads ", ")))
      ;; Only update if we actually created links
      (unless (string= prop-value linked-string)
        (org-set-property property-name linked-string)
        t))))

;;;###autoload
(defun org-scribe/link-scene-plot-threads ()
  "Convert plot thread names to ID links in current scene.
Updates :Plot: property."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((updated-plot (org-scribe--link-plot-threads-in-property "Plot")))
      (cond
       (updated-plot
        (message "Updated Plot property"))
       (t
        (message "No Plot property found or already linked"))))))

;;;###autoload
(defun org-scribe/link-all-scene-plot-threads ()
  "Convert plot thread names to ID links in all scenes in current buffer.
Processes all headings with :Plot: properties."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (org-map-entries
       (lambda ()
         ;; Process if has Plot property
         (when (org-entry-get nil "Plot")
           (let ((updated-plot (org-scribe--link-plot-threads-in-property "Plot")))
             (when updated-plot
               (setq count (1+ count))))))
       nil 'file)
      (message "Updated plot thread links in %d scene%s"
               count (if (= count 1) "" "s")))))

;;; Integration with Capture System

(defun org-scribe--capture-finalize-add-plot-thread-id ()
  "Hook function to add ID to newly captured plot threads.
This is called before a plot thread capture is finalized.
Runs in the capture buffer before it's filed.

This is a safety net - plot thread capture templates should include
ID generation via %(org-id-new), but this ensures any plot thread heading
without an ID gets one automatically."
  (when (and (boundp 'org-capture-mode)
             org-capture-mode
             (buffer-file-name))
    ;; Check if we're capturing to a plot file
    (let ((target (org-scribe--get-plot-thread-file)))
      (when (and target
                 (file-exists-p target)
                 ;; Compare the target file with current buffer's file
                 ;; or the file we're capturing to
                 (or (string= (buffer-file-name) target)
                     (string= (buffer-file-name) (expand-file-name target))))
        ;; We're capturing a plot thread, ensure it has an ID
        (save-excursion
          (goto-char (point-min))
          ;; In capture buffer, find the heading we're creating
          (when (re-search-forward "^\\*+ " nil t)
            (org-back-to-heading)
            (unless (org-entry-get nil "ID")
              (org-id-get-create)
              (message "Auto-created ID for new plot thread (via hook)"))))))))

;; Add the hook - use before-finalize to ensure we're still in capture buffer
;; Note: This is redundant with the template's %(org-id-new) but serves as a safety net
(add-hook 'org-capture-before-finalize-hook #'org-scribe--capture-finalize-add-plot-thread-id)

;;;###autoload
(defun org-scribe/setup-plot-thread-links ()
  "Set up plot thread linking system for current project.
This function:
1. Adds IDs to all existing plot threads
2. Ensures the capture hook is active
3. Optionally links existing scenes

Run this once when setting up ID-based plot thread linking
in an existing project."
  (interactive)
  (message "Setting up plot thread linking system...")

  ;; Step 1: Add IDs to plot threads
  (org-scribe/add-plot-thread-ids)

  ;; Step 2: Ask if user wants to link existing scenes
  (when (y-or-n-p "Link plot threads in existing scenes? ")
    (let ((novel-file (plist-get (org-scribe-project-structure) :novel-file)))
      (when (and novel-file (file-exists-p novel-file))
        (with-current-buffer (find-file-noselect novel-file)
          (org-scribe/link-all-scene-plot-threads)
          (save-buffer)))))

  (message "Plot thread linking system setup complete!"))

;;; Phase 2: Timeline and Reporting Functions

;;; Helper Functions for Analysis

(defun org-scribe--get-all-scenes-with-plots ()
  "Return list of all scenes with Plot properties.
Each entry is (SCENE-HEADING CHAPTER-HEADING PLOT-THREADS-LIST)."
  (let ((novel-file (plist-get (org-scribe-project-structure) :novel-file))
        scenes)
    (when (and novel-file (file-exists-p novel-file))
      (with-current-buffer (find-file-noselect novel-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (when (= (org-current-level) 3)  ; Scenes are level 3
              (let* ((heading (org-get-heading t t t t))
                     (chapter (save-excursion
                               (outline-up-heading 1)
                               (org-get-heading t t t t)))
                     (plot-prop (org-entry-get nil "Plot")))
                (when plot-prop
                  ;; Extract thread names from ID links
                  (let ((thread-list (org-scribe--property-to-list plot-prop)))
                    (push (list heading chapter thread-list) scenes))))))
          nil 'file))))
    (nreverse scenes)))

(defun org-scribe--find-thread-in-scenes (thread-name scenes)
  "Find all SCENES containing THREAD-NAME.
Returns list of scene entries."
  (seq-filter (lambda (scene)
                (member thread-name (nth 2 scene)))
              scenes))

(defun org-scribe--calculate-thread-gap (appearances all-scenes)
  "Calculate largest gap in APPEARANCES across ALL-SCENES.
Returns the maximum number of consecutive scenes where thread is absent."
  (when appearances
    (let ((scene-indices nil)
          (max-gap 0))
      ;; Get indices of scenes with this thread
      (dotimes (i (length all-scenes))
        (when (member (nth i all-scenes) appearances)
          (push i scene-indices)))
      (setq scene-indices (nreverse scene-indices))

      ;; Calculate gaps between appearances
      (when (> (length scene-indices) 1)
        (dotimes (i (1- (length scene-indices)))
          (let ((gap (- (nth (1+ i) scene-indices)
                       (nth i scene-indices)
                       1)))  ; Subtract 1 because we count scenes between
            (setq max-gap (max max-gap gap)))))
      max-gap)))

(defun org-scribe--get-plot-thread-weight (thread-name)
  "Get the Weight property for THREAD-NAME from plot file.
Returns the weight as a float, or 999.0 if not found.

THREAD-NAME is matched against the :NAME: property or heading text,
using the same logic as org-scribe--get-plot-thread-name-at-point."
  (let ((plot-file (org-scribe--get-plot-thread-file))
        (weight 999.0))  ; Default for threads without Weight
    (when (and plot-file (file-exists-p plot-file))
      (with-current-buffer (find-file-noselect plot-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         ;; Use org-map-entries to find the plot thread
         (catch 'found
           (org-map-entries
            (lambda ()
              (let* ((level (org-current-level))
                     (id (org-id-get))
                     (name (org-scribe--get-plot-thread-name-at-point))
                     (heading (org-get-heading t t t t))
                     ;; Check if this looks like a plot thread heading
                     (is-plot-thread
                      (and (>= level 1)
                           id
                           name
                           (or (org-entry-get nil "TYPE")
                               (org-entry-get nil "THREAD-TYPE")
                               (string-match-p "\\(Main Plot\\|Subplot\\|Thread\\|A-[Pp]lot\\|B-[Pp]lot\\|C-[Pp]lot\\)"
                                              heading)))))
                ;; If this is a plot thread and the name matches
                (when (and is-plot-thread (string= name thread-name))
                  (when-let ((weight-str (org-entry-get nil "Weight")))
                    (setq weight (string-to-number weight-str)))
                  (throw 'found t))))
            nil 'file)))))
    weight))

;;; Dynamic Block: Plot Thread Timeline

;;;###autoload
(defun org-dblock-write:plot-thread-timeline (params)
  "Generate timeline showing plot thread progression across scenes.
Extracts display text from ID links in :Plot: properties.

The timeline shows all scenes and indicates which threads are present
using symbols:
  ● = Thread is active in this scene
  (blank) = Thread not present

Thread columns are sorted by :Weight: property (ascending).
Threads without a Weight property appear last, sorted alphabetically.

Thread information is extracted from the :Plot: property in scenes.
Thread names are extracted from ID links like [[id:...][Name]]."
  (let* ((threads-set (make-hash-table :test 'equal))
         (scenes (org-scribe--get-all-scenes-with-plots))
         threads)

    ;; Collect unique thread names
    (dolist (scene scenes)
      (let ((thread-list (nth 2 scene)))
        (dolist (thread thread-list)
          (puthash thread t threads-set))))

    ;; Build list of (name . weight) pairs, sort, and extract names
    (let ((thread-weights nil))
      (dolist (thread-name (hash-table-keys threads-set))
        (let ((weight (org-scribe--get-plot-thread-weight thread-name)))
          (push (cons thread-name weight) thread-weights)))

      ;; Sort by weight (ascending), then alphabetically for ties
      (setq thread-weights
            (sort thread-weights
                  (lambda (a b)
                    (let ((weight-a (cdr a))
                          (weight-b (cdr b)))
                      (if (= weight-a weight-b)
                          ;; Weights equal - sort alphabetically
                          (string< (car a) (car b))
                        ;; Different weights - sort by weight
                        (< weight-a weight-b))))))

      ;; Extract sorted thread names
      (setq threads (mapcar #'car thread-weights)))

    (if (null scenes)
        (insert "No scenes with Plot properties found.\n")
      ;; Generate org table (unchanged)
      (insert "| Scene | Chapter |")
      (dolist (thread threads)
        (insert (format " %s |" thread)))
      (insert "\n|-------+---------+")
      (dolist (_ threads)
        (insert "--------+"))
      (insert "\n")

      ;; Table rows (unchanged)
      (dolist (scene scenes)
        (let ((scene-name (nth 0 scene))
              (chapter (nth 1 scene))
              (scene-threads (nth 2 scene)))
          (insert (format "| %s | %s |" scene-name chapter))
          (dolist (thread threads)
            (insert (format " %s |"
                           (if (member thread scene-threads) "●" ""))))
          (insert "\n")))

      ;; Align table
      (org-table-align))))

;;; Plot Thread Health Report

(defun org-scribe--get-thread-status (thread-name appearances all-scenes)
  "Get status symbol and warnings for THREAD-NAME.
APPEARANCES is list of scenes containing thread.
ALL-SCENES is list of all scenes.
Returns (STATUS-SYMBOL . WARNINGS-LIST)."
  (let ((warnings nil)
        (status "✓")
        (scene-count (length appearances))
        (total-scenes (length all-scenes))
        (coverage-pct (if (> (length all-scenes) 0)
                         (/ (* 100.0 scene-count) (length all-scenes))
                       0))
        (gap (org-scribe--calculate-thread-gap appearances all-scenes)))

    ;; Check for issues
    (when (< coverage-pct 30)
      (setq status "⚠️")
      (push (format "Low coverage (%.1f%%)" coverage-pct) warnings))

    (when (and gap (> gap 5))
      (setq status "⚠️")
      (push (format "Gap of %d scenes" gap) warnings))

    (when (= scene-count 1)
      (setq status "⚠️")
      (push "Only in 1 scene" warnings))

    (cons status warnings)))

;;;###autoload
(defun org-scribe/plot-thread-report ()
  "Generate health report for plot threads.
Analyzes thread coverage, gaps, and warnings.
Opens a new buffer with the report."
  (interactive)
  (let ((report-buffer (get-buffer-create "*Plot Thread Health Report*"))
        (threads (org-scribe--get-all-plot-threads))
        (scenes (org-scribe--get-all-scenes-with-plots)))

    (with-current-buffer report-buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Plot Thread Health Report\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d %H:%M") "\n")
      (insert "#+STARTUP: overview\n\n")

      ;; Summary
      (insert "* Summary\n\n")
      (insert (format "- Total plot threads: %d\n" (length threads)))
      (insert (format "- Total scenes: %d\n" (length scenes)))
      (insert (format "- Scenes with plot threads: %d\n" (length scenes)))
      (insert "\n")

      (if (null scenes)
          (insert "** No scenes with Plot properties found.\n\n")
        ;; Per-thread analysis
        (insert "* Thread Details\n\n")
        (dolist (thread threads)
          (let* ((thread-name (car thread))
                 (thread-id (cadr thread))
                 (appearances (org-scribe--find-thread-in-scenes thread-name scenes))
                 (scene-count (length appearances))
                 (coverage-pct (if (> (length scenes) 0)
                                  (/ (* 100.0 scene-count) (length scenes))
                                0))
                 (status-info (org-scribe--get-thread-status thread-name appearances scenes))
                 (status (car status-info))
                 (warnings (cdr status-info)))

            (insert (format "** %s %s\n" status thread-name))
            (insert (format ":PROPERTIES:\n"))
            (insert (format ":ID: %s\n" thread-id))
            (insert (format ":END:\n\n"))
            (insert (format "- Scenes: %d of %d (%.1f%%)\n"
                           scene-count (length scenes) coverage-pct))

            (when appearances
              (insert (format "- First appearance: %s (Chapter: %s)\n"
                             (nth 0 (car appearances))
                             (nth 1 (car appearances))))
              (insert (format "- Last appearance: %s (Chapter: %s)\n"
                             (nth 0 (car (last appearances)))
                             (nth 1 (car (last appearances))))))

            (when warnings
              (insert "\n*Warnings:*\n")
              (dolist (warning warnings)
                (insert (format "- ⚠️ %s\n" warning))))

            (insert "\n"))))

      ;; Scenes without plot threads
      (insert "* Scenes Without Plot Threads\n\n")
      (let ((novel-file (plist-get (org-scribe-project-structure) :novel-file))
            (scenes-without-plot nil))
        (when (and novel-file (file-exists-p novel-file))
          (with-current-buffer (find-file-noselect novel-file)
            (org-map-entries
             (lambda ()
               (when (= (org-current-level) 3)  ; Scenes are level 3
                 (unless (org-entry-get nil "Plot")
                   (let ((heading (org-get-heading t t t t))
                         (chapter (save-excursion
                                   (outline-up-heading 1)
                                   (org-get-heading t t t t))))
                     (push (list heading chapter) scenes-without-plot)))))
             nil 'file)))

        (if scenes-without-plot
            (progn
              (setq scenes-without-plot (nreverse scenes-without-plot))
              (insert (format "Found %d scene(s) without Plot property:\n\n"
                             (length scenes-without-plot)))
              (dolist (scene scenes-without-plot)
                (insert (format "- %s (Chapter: %s)\n" (car scene) (cadr scene)))))
          (insert "All scenes have Plot properties assigned.\n"))))

    (pop-to-buffer report-buffer)
    (goto-char (point-min))
    (message "Plot thread health report generated")))

;;; Plot Thread Statistics

;;;###autoload
(defun org-scribe/plot-thread-stats ()
  "Display quick statistics for plot threads.
Shows one-line summary in the minibuffer."
  (interactive)
  (let* ((threads (org-scribe--get-all-plot-threads))
         (scenes (org-scribe--get-all-scenes-with-plots))
         (thread-count (length threads))
         (scene-count (length scenes))
         (warning-count 0))

    ;; Count warnings
    (dolist (thread threads)
      (let* ((thread-name (car thread))
             (appearances (org-scribe--find-thread-in-scenes thread-name scenes))
             (status-info (org-scribe--get-thread-status thread-name appearances scenes))
             (warnings (cdr status-info)))
        (when warnings
          (setq warning-count (1+ warning-count)))))

    (message "Plot threads: %d | Scenes: %d | Threads with warnings: %d"
             thread-count scene-count warning-count)))

;; Helper function needed from search module
;; This is a forward declaration - the actual function is in org-scribe-search.el
(declare-function org-scribe--property-to-list "search/org-scribe-search")

;;; Update Link Display Names

(require 'org-scribe-link-update)

;;;###autoload
(defun org-scribe/update-plot-link-names ()
  "Update plot thread link display names in current scene.

Refreshes :Plot: property to show current plot thread names from
the plot database.

Use this after renaming a plot thread in plot.org. The ID links
will still work, but this updates the display text to match the
current name.

Example:
  Before: [[id:plot-main-001][Main Plot]]
  After rename in database: [[id:plot-main-001][Primary Storyline]]

Returns t if any updates were made, nil otherwise."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let* ((plots-alist (org-scribe--get-all-plot-threads))
           (id-map (org-scribe--build-id-to-name-map plots-alist))
           (updated-plots (org-scribe--update-links-in-property "Plot" id-map)))
      (if updated-plots
          (message "Updated Plot link names")
        (message "No plot link names needed updating"))
      updated-plots)))

;;;###autoload
(defun org-scribe/update-all-plot-link-names ()
  "Update plot thread link display names in all scenes.

Scans plot database for current names and updates the display
text portion of ID links in :Plot: properties throughout
the entire manuscript.

This is useful after renaming plot threads in plot.org, as ID
links will still work but show the old name. This function refreshes
all display names to match the current database.

Example workflow:
  1. Rename \"Main Plot\" to \"Primary Storyline\" in plot.org
  2. Run this function (M-x org-scribe/update-all-plot-link-names)
  3. All scenes updated automatically!

Returns the number of scenes updated."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((plots-alist (org-scribe--get-all-plot-threads))
           (id-map (org-scribe--build-id-to-name-map plots-alist))
           (count 0))
      (org-map-entries
       (lambda ()
         (when (org-entry-get nil "Plot")
           (let ((updated-plots (org-scribe--update-links-in-property "Plot" id-map)))
             (when updated-plots
               (setq count (1+ count))))))
       nil 'file)
      (message "Updated plot link names in %d scene%s"
               count (if (= count 1) "" "s"))
      count)))

(provide 'org-scribe-plot-links)

;;; org-scribe-plot-links.el ends here
