;;; org-scribe-plot-links.el --- Plot thread linking system for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides ID-based linking system for plot threads in writing projects.
;; Uses the generic linking framework from org-scribe-linking-core.el
;; and adds plot-specific functions (jump, timeline, health reports).
;;
;; Features:
;; - Auto-create IDs when capturing new plot threads
;; - Insert plot thread links with completion
;; - Link multiple plot threads at once
;; - Jump to plot thread definition from properties
;; - Update all plot thread links in document
;; - Plot thread timeline visualization
;; - Plot thread health reports and statistics

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-scribe-core)
(require 'org-scribe-capture)
(require 'org-scribe-messages)
(require 'org-scribe-linking-core)
(require 'org-scribe-link-update)

;;; Plot Thread Heading Predicate

(defun org-scribe--plot-heading-p ()
  "Return non-nil if the heading at point is a plot thread heading."
  (and (>= (org-current-level) 1)
       (or (org-entry-get nil "TYPE")
           (org-entry-get nil "THREAD-TYPE")
           (string-match-p "\\(Main Plot\\|Subplot\\|Thread\\|A-[Pp]lot\\|B-[Pp]lot\\|C-[Pp]lot\\)"
                          (org-get-heading t t t t)))))

;;; Plot Thread File Resolver (custom: inline project-type logic)

(defun org-scribe--get-plot-thread-file ()
  "Get the path to the plot threads file for the current project.
For novels, this is objects/plot.org.
For short stories, this is notes.org (Plot section)."
  (let* ((project-root (org-scribe-project-root))
         (project-type (org-scribe-project-type))
         (structure (org-scribe-project-structure)))
    (cond
     ((eq project-type 'novel)
      (let ((plot-file (expand-file-name "objects/plot.org" project-root)))
        (when (file-exists-p plot-file)
          plot-file)))
     ((eq project-type 'short-story)
      (plist-get structure :notes-file))
     (t
      (let ((plot-file (expand-file-name "objects/plot.org" project-root)))
        (when (file-exists-p plot-file)
          plot-file))))))

;;; Entity Descriptor

(defconst org-scribe--plot-entity
  '(:file-fn org-scribe--get-plot-thread-file
    :heading-predicate org-scribe--plot-heading-p
    :properties ("Plot")
    :msg-added-ids msg-added-plot-ids
    :msg-ids-updated msg-plot-ids-updated
    :error-no-file error-no-plot-file
    :error-none-found error-no-plot-threads-found
    :prompt-select prompt-select-plot-thread
    :prompt-select-multi prompt-select-plot-threads-multi
    :error-no-id error-no-id-for-plot
    :msg-inserted-links msg-inserted-plot-links
    :msg-no-selected msg-no-plot-threads-selected
    :msg-set msg-set-plot-threads
    :msg-updated-single msg-updated-plot
    :msg-no-updates msg-no-plot-updates-needed
    :msg-updated-links msg-updated-plot-links
    :msg-setting-up msg-setting-up-plot-links
    :question-link-existing question-link-existing-plots
    :msg-setup-complete msg-plot-setup-complete
    :msg-updated-link-names msg-updated-plot-link-names
    :msg-no-link-updates-type "plot"
    :msg-updated-all-type "plot")
  "Entity descriptor for plot threads.")

;;; Generated API Functions

(org-scribe-define-entity org-scribe--plot-entity
  :get-all-name          org-scribe--get-all-plot-threads
  :create-link-name      org-scribe--create-plot-thread-link
  :add-ids-to-all-name   org-scribe--add-id-to-all-plot-threads
  :add-ids-name          org-scribe/add-plot-thread-ids
  :insert-link-name      org-scribe/insert-plot-thread-link
  :insert-multi-name     org-scribe/insert-multiple-plot-thread-links
  :set-scene-name        org-scribe/set-scene-plot-threads
  :set-scene-property    "Plot"
  :link-in-prop-name     org-scribe--link-plot-threads-in-property
  :link-scene-name       org-scribe/link-scene-plot-threads
  :link-all-name         org-scribe/link-all-scene-plot-threads
  :setup-name            org-scribe/setup-plot-thread-links
  :setup-add-ids-fn      org-scribe/add-plot-thread-ids
  :setup-link-all-fn     org-scribe/link-all-scene-plot-threads
  :update-names-name     org-scribe/update-plot-link-names
  :update-all-name       org-scribe/update-all-plot-link-names)

;;; Plot-Specific Functions

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
      (message (org-scribe-msg 'msg-no-plot-property)))
     ((null thread-list)
      (message (org-scribe-msg 'msg-no-plot-threads-in-property)))
     ((= (length thread-list) 1)
      (let ((thread-name (car thread-list)))
        (if (string-match "\\[\\[id:\\([^]]+\\)\\]\\[" plot-prop)
            (org-id-goto (match-string 1 plot-prop))
          (message (org-scribe-msg 'msg-plot-not-id-link thread-name)))))
     (t
      (let* ((selected (completing-read (org-scribe-msg 'prompt-jump-to-plot) thread-list nil t))
             (threads (org-scribe--get-all-plot-threads))
             (entry (assoc selected threads))
             (id (cadr entry)))
        (if id
            (org-id-goto id)
          (message (org-scribe-msg 'error-no-id-for-plot selected))))))))

;;; Plot Thread Timeline and Analytics

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
            (when (= (org-current-level) 3)
              (let* ((heading (org-get-heading t t t t))
                     (chapter (save-excursion
                               (outline-up-heading 1)
                               (org-get-heading t t t t)))
                     (plot-prop (org-entry-get nil "Plot")))
                (when plot-prop
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
      (dotimes (i (length all-scenes))
        (when (member (nth i all-scenes) appearances)
          (push i scene-indices)))
      (setq scene-indices (nreverse scene-indices))
      (when (> (length scene-indices) 1)
        (dotimes (i (1- (length scene-indices)))
          (let ((gap (- (nth (1+ i) scene-indices)
                       (nth i scene-indices)
                       1)))
            (setq max-gap (max max-gap gap)))))
      max-gap)))

(defun org-scribe--get-plot-thread-weight (thread-name)
  "Get the Weight property for THREAD-NAME from plot file.
Returns the weight as a float, or 999.0 if not found."
  (let ((plot-file (org-scribe--get-plot-thread-file))
        (weight 999.0))
    (when (and plot-file (file-exists-p plot-file))
      (with-current-buffer (find-file-noselect plot-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (catch 'found
           (org-map-entries
            (lambda ()
              (let ((name (org-scribe--entity-name-at-point)))
                (when (and (org-scribe--plot-heading-p)
                           (org-id-get)
                           (string= name thread-name))
                  (when-let ((weight-str (org-entry-get nil "Weight")))
                    (setq weight (string-to-number weight-str)))
                  (throw 'found t))))
            nil 'file)))))
    weight))

;;;###autoload
(defun org-dblock-write:plot-thread-timeline (params)
  "Generate timeline showing plot thread progression across scenes.
PARAMS are ignored (reserved for future options)."
  (let* ((threads-set (make-hash-table :test 'equal))
         (scenes (org-scribe--get-all-scenes-with-plots))
         threads)
    (dolist (scene scenes)
      (let ((thread-list (nth 2 scene)))
        (dolist (thread thread-list)
          (puthash thread t threads-set))))
    (let ((thread-weights nil))
      (dolist (thread-name (hash-table-keys threads-set))
        (let ((weight (org-scribe--get-plot-thread-weight thread-name)))
          (push (cons thread-name weight) thread-weights)))
      (setq thread-weights
            (sort thread-weights
                  (lambda (a b)
                    (let ((weight-a (cdr a))
                          (weight-b (cdr b)))
                      (if (= weight-a weight-b)
                          (string< (car a) (car b))
                        (< weight-a weight-b))))))
      (setq threads (mapcar #'car thread-weights)))
    (if (null scenes)
        (insert "No scenes with Plot properties found.\n")
      (insert "| Scene | Chapter |")
      (dolist (thread threads)
        (insert (format " %s |" thread)))
      (insert "\n|-------+---------+")
      (dolist (_ threads)
        (insert "--------+"))
      (insert "\n")
      (dolist (scene scenes)
        (let ((scene-name (nth 0 scene))
              (chapter (nth 1 scene))
              (scene-threads (nth 2 scene)))
          (insert (format "| %s | %s |" scene-name chapter))
          (dolist (thread threads)
            (insert (format " %s |"
                           (if (member thread scene-threads) "●" ""))))
          (insert "\n")))
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
        (coverage-pct (if (> (length all-scenes) 0)
                         (/ (* 100.0 scene-count) (length all-scenes))
                       0))
        (gap (org-scribe--calculate-thread-gap appearances all-scenes)))
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
      (insert "* Summary\n\n")
      (insert (format "- Total plot threads: %d\n" (length threads)))
      (insert (format "- Total scenes: %d\n" (length scenes)))
      (insert (format "- Scenes with plot threads: %d\n" (length scenes)))
      (insert "\n")
      (if (null scenes)
          (insert "** No scenes with Plot properties found.\n\n")
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
      (insert "* Scenes Without Plot Threads\n\n")
      (let ((novel-file (plist-get (org-scribe-project-structure) :novel-file))
            (scenes-without-plot nil))
        (when (and novel-file (file-exists-p novel-file))
          (with-current-buffer (find-file-noselect novel-file)
            (org-map-entries
             (lambda ()
               (when (= (org-current-level) 3)
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
    (message (org-scribe-msg 'msg-plot-health-report))))

;;;###autoload
(defun org-scribe/plot-thread-stats ()
  "Display quick statistics for plot threads."
  (interactive)
  (let* ((threads (org-scribe--get-all-plot-threads))
         (scenes (org-scribe--get-all-scenes-with-plots))
         (thread-count (length threads))
         (scene-count (length scenes))
         (warning-count 0))
    (dolist (thread threads)
      (let* ((thread-name (car thread))
             (appearances (org-scribe--find-thread-in-scenes thread-name scenes))
             (status-info (org-scribe--get-thread-status thread-name appearances scenes))
             (warnings (cdr status-info)))
        (when warnings
          (setq warning-count (1+ warning-count)))))
    (message (org-scribe-msg 'msg-plot-stats
                              thread-count scene-count warning-count))))

;;; Legacy Aliases

(defun org-scribe--ensure-plot-thread-has-id ()
  "Ensure the current plot thread heading has a unique ID."
  (org-id-get-create))

(defalias 'org-scribe--get-plot-thread-name-at-point 'org-scribe--entity-name-at-point)

;; Forward declaration for search module dependency
(declare-function org-scribe--property-to-list "search/org-scribe-search")

(provide 'org-scribe-plot-links)

;;; org-scribe-plot-links.el ends here
