;;; org-scribe-core.el --- Core utilities for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core utility functions for the org-scribe package.
;; Includes project detection, feature checking, and common helpers.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'project)
(require 'org-scribe-messages)

;;; Project Detection

(defun org-scribe-project-root ()
  "Find root directory of writing project.
Looks for .org-scribe-project file, then falls back to `project-current'."
  (or (locate-dominating-file default-directory ".org-scribe-project")
      (when-let ((proj (project-current)))
        (project-root proj))
      default-directory))

(defvar org-scribe--project-type-cache nil
  "Alist of (PROJECT-ROOT . PROJECT-TYPE) for caching project type detection.
Cleared when changing directories or projects.")

(defun org-scribe-project-type ()
  "Detect the type of writing project.
Returns one of:
  'novel - Novel project (objects/ directory with separate files)
  'short-story - Short story project (consolidated notes.org)
  'unknown - Cannot determine project type

Detection strategy:
1. Check cache for this project root
2. Read .org-scribe-project marker file if it exists (look for Type: line)
3. Check for existence of objects/ directory structure (indicates novel)
4. Check for story.org or cuento.org (indicates short story)
5. Check for novel.org or novela.org (indicates novel)
6. Return 'unknown if none of the above"
  (let* ((root (org-scribe-project-root))
         (cached (alist-get root org-scribe--project-type-cache nil nil #'string=)))
    (if cached
        cached
      ;; Not cached, detect and cache
      (let ((type
             (let ((marker-file (expand-file-name ".org-scribe-project" root)))
               (cond
                ;; Strategy 1: Read marker file
                ((and (file-exists-p marker-file)
                      (with-temp-buffer
                        (insert-file-contents marker-file)
                        (goto-char (point-min))
                        (when (re-search-forward "^# Type: \\(.*\\)$" nil t)
                          (let ((type-str (match-string 1)))
                            (cond
                             ((string= type-str "short-story") 'short-story)
                             ((string= type-str "novel") 'novel)
                             (t nil)))))))

                ;; Strategy 2: Check for objects/ directory (novel indicator)
                ((or (file-directory-p (expand-file-name "objects" root))
                     (file-directory-p (expand-file-name "objects/" root)))
                 'novel)

                ;; Strategy 3: Check for story.org or cuento.org (short story indicator)
                ((or (file-exists-p (expand-file-name "story.org" root))
                     (file-exists-p (expand-file-name "cuento.org" root)))
                 'short-story)

                ;; Strategy 4: Check for novel.org or novela.org (novel indicator)
                ((or (file-exists-p (expand-file-name "novel.org" root))
                     (file-exists-p (expand-file-name "novela.org" root)))
                 'novel)

                ;; Unknown
                (t 'unknown)))))
        ;; Cache the result
        (setq org-scribe--project-type-cache
              (cons (cons root type) org-scribe--project-type-cache))
        type))))

(defun org-scribe--find-existing-file (root &rest relative-paths)
  "Return the first existing file from RELATIVE-PATHS under ROOT, or nil."
  (cl-loop for path in relative-paths
           for full = (expand-file-name path root)
           when (file-exists-p full) return full))

(defun org-scribe--find-existing-dir (root &rest relative-paths)
  "Return the first existing directory from RELATIVE-PATHS under ROOT, or nil."
  (cl-loop for path in relative-paths
           for full = (expand-file-name path root)
           when (file-directory-p full) return full))

(defun org-scribe-project-structure ()
  "Detect project structure and return layout information.
Returns plist with:
  :root          - project root directory
  :novel-file    - main manuscript file (novel.org or novela.org)
  :notes-dir     - notes directory (notes/ or notas/)
  :notes-file    - notes file (notes/notes.org, notas/notas.org, notes.org, or notas.org)
  :characters-file - characters file (objects/characters.org or objects/personajes.org)
  :locations-file  - locations file (objects/locations.org or objects/localizaciones.org)
  :plot-file       - plot file (objects/plot.org or objects/trama.org)
  :timeline-file   - timeline file (objects/timeline.org or objects/cronologia.org)
  :objects-file    - objects file (objects/objects.org or objects/objetos.org)

All file/directory values are nil if the path does not exist."
  (let* ((root (org-scribe-project-root)))
    (list :root root
          :novel-file (org-scribe--find-existing-file root
                        "novel.org" "novela.org")
          :notes-dir (org-scribe--find-existing-dir root
                       "notes" "notas")
          :notes-file (org-scribe--find-existing-file root
                        "notes/notes.org" "notas/notas.org"
                        "notes.org" "notas.org")
          :characters-file (org-scribe--find-existing-file root
                             "objects/characters.org" "objects/personajes.org"
                             "characters.org" "personajes.org")
          :locations-file (org-scribe--find-existing-file root
                            "objects/locations.org" "objects/localizaciones.org"
                            "locations.org" "localizaciones.org")
          :plot-file (org-scribe--find-existing-file root
                       "objects/plot.org" "objects/trama.org"
                       "plot.org" "trama.org")
          :timeline-file (org-scribe--find-existing-file root
                           "objects/timeline.org" "objects/cronologia.org"
                           "timeline.org" "cronologia.org")
          :objects-file (org-scribe--find-existing-file root
                          "objects/objects.org" "objects/objetos.org"
                          "objects.org" "objetos.org"))))

;;; Feature Detection

(defvar org-scribe--available-features nil
  "Alist of (FEATURE . AVAILABLE-P) for optional dependencies.")

(defun org-scribe-check-feature (feature)
  "Check if FEATURE is available and cache result."
  (let ((cached (assq feature org-scribe--available-features)))
    (if cached
        (cdr cached)
      (let ((available (featurep feature)))
        (push (cons feature available) org-scribe--available-features)
        available))))

(defmacro org-scribe-when-feature (feature &rest body)
  "Execute BODY if FEATURE is available, otherwise show message."
  (declare (indent 1))
  `(if (org-scribe-check-feature ',feature)
       (progn ,@body)
     (user-error (org-scribe-msg 'error-feature-not-available ',feature))))

;;; Helper Functions

(defun org-scribe/window-perc (pct)
  "Calculate window width as percentage of frame.

PCT should be a float between 0.0 and 1.0 representing the desired
percentage of `frame-width'.  Returns the floor of the calculation
as an integer suitable for window sizing functions.

Example: (org-scribe/window-perc 0.25) with a 200-char frame returns 50."
  (floor (* (frame-width) pct)))

(defun org-scribe/sanitize-filename (title)
  "Sanitize TITLE for safe use as filename component.
Removes path separators and other problematic characters."
  (let ((safe-title (replace-regexp-in-string "[/\\:]" "_" title)))
    (replace-regexp-in-string "^\\.+" "" safe-title)))

(defun org-scribe/validate-directory (directory &optional create)
  "Validate that DIRECTORY exists.
If CREATE is non-nil and directory doesn't exist, ask user to create it.
Returns t if directory exists or was created, nil otherwise."
  (cond
   ((file-directory-p directory) t)
   (create
    (when (yes-or-no-p (org-scribe-msg 'question-create-directory directory))
      (make-directory directory t)
      t))
   (t nil)))

;;; Error Handling Wrapper

(defmacro org-scribe-with-error-handling (name &rest body)
  "Execute BODY with standard error handling for writing functions.
NAME should be a string identifying the function for error messages."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error
      (message "Error in %s: %s" ,name (error-message-string err))
      nil)))

(provide 'org-scribe-core)

;;; org-scribe-core.el ends here
