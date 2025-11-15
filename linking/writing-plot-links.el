;;; writing-plot-links.el --- Plot thread linking system for emacs-writing -*- lexical-binding: t; -*-

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
(require 'writing-core)
(require 'writing-capture)

;;; Plot Thread ID Management

(defun writing--ensure-plot-thread-has-id ()
  "Ensure the current plot thread heading has a unique ID.
Creates an ID if one doesn't exist. Returns the ID."
  (org-id-get-create))

(defun writing--add-id-to-all-plot-threads ()
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

(defun writing--get-plot-thread-name-at-point ()
  "Get the plot thread name from current heading or NAME property."
  (or (org-entry-get nil "NAME")
      (org-get-heading t t t t)))

;;; Plot Thread Database Functions

(defun writing--get-plot-thread-file ()
  "Get the path to the plot threads file for the current project.
For novels, this is plan/plot.org.
For short stories, this is notes.org (Plot section)."
  (let* ((project-root (writing-project-root))
         (project-type (writing-project-type))
         (structure (writing-project-structure)))
    (cond
     ;; Novel project - use plan/plot.org
     ((eq project-type 'novel)
      (let ((plot-file (expand-file-name "plan/plot.org" project-root)))
        (when (file-exists-p plot-file)
          plot-file)))
     ;; Short story - use notes.org
     ((eq project-type 'short-story)
      (plist-get structure :notes-file))
     ;; Unknown - try plan/plot.org
     (t
      (let ((plot-file (expand-file-name "plan/plot.org" project-root)))
        (when (file-exists-p plot-file)
          plot-file))))))

(defun writing--get-all-plot-threads ()
  "Return alist of (THREAD-NAME . (ID . HEADING)) from plot file.
Returns list of (NAME . (ID . HEADING-TEXT)) for all plot threads in the project."
  (let ((plot-file (writing--get-plot-thread-file))
        result)
    (when (and plot-file (file-exists-p plot-file))
      (with-current-buffer (find-file-noselect plot-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (let* ((level (org-current-level))
                   (id (org-id-get))
                   (name (writing--get-plot-thread-name-at-point))
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

(defun writing--create-plot-thread-link (thread-name id-alist)
  "Create an ID link for THREAD-NAME using ID-ALIST.
ID-ALIST should be in format ((NAME . (ID . HEADING)) ...).
Returns the link string or plain text if no ID found."
  (if-let* ((entry (assoc thread-name id-alist))
            (id (cadr entry)))
      (format "[[id:%s][%s]]" id thread-name)
    thread-name)) ; Fallback to plain text if no ID found

;;; Interactive Functions

;;;###autoload
(defun writing/add-plot-thread-ids ()
  "Add unique IDs to all plot threads in the plot file.
This should be run once on existing projects to set up
the ID-based linking system."
  (interactive)
  (let ((plot-file (writing--get-plot-thread-file)))
    (if (not (file-exists-p plot-file))
        (message "No plot file found. Create plot threads first.")
      (with-current-buffer (find-file-noselect plot-file)
        (writing--add-id-to-all-plot-threads)
        (save-buffer)
        (message "Plot thread IDs updated in %s" plot-file)))))

;;;###autoload
(defun writing/insert-plot-thread-link ()
  "Insert a plot thread link in the current property.
Scans the plot file, presents a completion menu,
and inserts the selected plot thread as an ID link.

Use this function when adding plot threads to scene properties."
  (interactive)
  (let* ((threads (writing--get-all-plot-threads))
         (thread-names (mapcar #'car threads)))
    (if (null thread-names)
        (message "No plot threads found. Create plot threads first or add IDs with writing/add-plot-thread-ids.")
      (let* ((selected (completing-read "Select plot thread: " thread-names nil t))
             (entry (assoc selected threads))
             (id (cadr entry)))
        (if id
            (progn
              (insert (format "[[id:%s][%s]]" id selected))
              (message "Inserted link to %s" selected))
          (message "No ID found for %s" selected))))))

;;;###autoload
(defun writing/insert-multiple-plot-thread-links ()
  "Insert multiple plot thread links separated by commas.
Useful for the :Plot: property which often lists
multiple plot threads in a scene."
  (interactive)
  (let* ((threads (writing--get-all-plot-threads))
         (thread-names (mapcar #'car threads))
         selected-threads
         links)
    (if (null thread-names)
        (message "No plot threads found. Create plot threads first or add IDs with writing/add-plot-thread-ids.")
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
(defun writing/set-scene-plot-threads ()
  "Set the Plot property to multiple plot thread ID links.
Specifically designed for the :Plot: property in scene headings."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (let* ((threads (writing--get-all-plot-threads))
         (thread-names (mapcar #'car threads))
         selected-threads
         links)
    (if (null thread-names)
        (message "No plot threads found. Create plot threads first or add IDs with writing/add-plot-thread-ids.")
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
(defun writing/jump-to-plot-thread ()
  "Jump to plot thread definition from scene.
If Plot property has multiple threads, prompts for selection."
  (interactive)
  (let* ((plot-prop (org-entry-get nil "Plot"))
         (thread-list (when plot-prop
                       (writing--property-to-list plot-prop))))
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
             (threads (writing--get-all-plot-threads))
             (entry (assoc selected threads))
             (id (cadr entry)))
        (if id
            (org-id-goto id)
          (message "No ID found for plot thread '%s'" selected)))))))

;;; Batch Update Functions

(defun writing--link-plot-threads-in-property (property-name)
  "Convert plot thread names to ID links in PROPERTY-NAME of current heading.
Handles both single threads and comma-separated lists."
  (when-let* ((prop-value (org-entry-get nil property-name)))
    (let* ((id-alist (writing--get-all-plot-threads))
           ;; Split on comma, trim whitespace
           (thread-list (mapcar #'string-trim
                                (split-string prop-value "," t)))
           ;; Create links for each plot thread
           (linked-threads (mapcar (lambda (name)
                                     (writing--create-plot-thread-link name id-alist))
                                   thread-list))
           (linked-string (string-join linked-threads ", ")))
      ;; Only update if we actually created links
      (unless (string= prop-value linked-string)
        (org-set-property property-name linked-string)
        t))))

;;;###autoload
(defun writing/link-scene-plot-threads ()
  "Convert plot thread names to ID links in current scene.
Updates :Plot: property."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((updated-plot (writing--link-plot-threads-in-property "Plot")))
      (cond
       (updated-plot
        (message "Updated Plot property"))
       (t
        (message "No Plot property found or already linked"))))))

;;;###autoload
(defun writing/link-all-scene-plot-threads ()
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
           (let ((updated-plot (writing--link-plot-threads-in-property "Plot")))
             (when updated-plot
               (setq count (1+ count))))))
       nil 'file)
      (message "Updated plot thread links in %d scene%s"
               count (if (= count 1) "" "s")))))

;;; Integration with Capture System

(defun writing--capture-finalize-add-plot-id ()
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
    (let ((target (writing--get-plot-thread-file)))
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
(add-hook 'org-capture-before-finalize-hook #'writing--capture-finalize-add-plot-id)

;;;###autoload
(defun writing/setup-plot-thread-links ()
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
  (writing/add-plot-thread-ids)

  ;; Step 2: Ask if user wants to link existing scenes
  (when (y-or-n-p "Link plot threads in existing scenes? ")
    (let ((novel-file (plist-get (writing-project-structure) :novel-file)))
      (when (and novel-file (file-exists-p novel-file))
        (with-current-buffer (find-file-noselect novel-file)
          (writing/link-all-scene-plot-threads)
          (save-buffer)))))

  (message "Plot thread linking system setup complete!"))

;; Helper function needed from search module
;; This is a forward declaration - the actual function is in writing-search.el
(declare-function writing--property-to-list "search/writing-search")

(provide 'writing-plot-links)

;;; writing-plot-links.el ends here
