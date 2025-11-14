;;; writing-core.el --- Core utilities for emacs-writing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core utility functions for the emacs-writing package.
;; Includes project detection, feature checking, and common helpers.

;;; Code:

(require 'org)
(require 'project)

;;; Project Detection

(defun writing-project-root ()
  "Find root directory of writing project.
Looks for .writing-project file, then falls back to `project-current'."
  (or (locate-dominating-file default-directory ".writing-project")
      (when-let ((proj (project-current)))
        (project-root proj))
      default-directory))

(defvar writing--project-type-cache nil
  "Alist of (PROJECT-ROOT . PROJECT-TYPE) for caching project type detection.
Cleared when changing directories or projects.")

(defun writing-project-type ()
  "Detect the type of writing project.
Returns one of:
  'novel - Novel project (plan/ directory with separate files)
  'short-story - Short story project (consolidated notes.org)
  'unknown - Cannot determine project type

Detection strategy:
1. Check cache for this project root
2. Read .writing-project marker file if it exists (look for Type: line)
3. Check for existence of plan/ directory structure (indicates novel)
4. Check for story.org or cuento.org (indicates short story)
5. Check for novel.org or novela.org (indicates novel)
6. Return 'unknown if none of the above"
  (let* ((root (writing-project-root))
         (cached (alist-get root writing--project-type-cache nil nil #'string=)))
    (if cached
        cached
      ;; Not cached, detect and cache
      (let ((type
             (let ((marker-file (expand-file-name ".writing-project" root)))
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

                ;; Strategy 2: Check for plan/ directory (novel indicator)
                ((or (file-directory-p (expand-file-name "plan" root))
                     (file-directory-p (expand-file-name "plan/" root)))
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
        (setq writing--project-type-cache
              (cons (cons root type) writing--project-type-cache))
        type))))

(defun writing-project-structure ()
  "Detect project structure and return layout information.
Returns plist with :novel-file :notes-dir :characters-dir etc."
  (let* ((root (writing-project-root))
         (novel-en (expand-file-name "novel.org" root))
         (novel-es (expand-file-name "novela.org" root))
         (notes-en (expand-file-name "notes/" root))
         (notes-es (expand-file-name "notas/" root))
         (novel (cond ((file-exists-p novel-en) novel-en)
                      ((file-exists-p novel-es) novel-es)
                      (t nil)))
         (notes (cond ((file-directory-p notes-en) notes-en)
                      ((file-directory-p notes-es) notes-es)
                      (t nil))))
    (list :root root
          :novel-file novel
          :notes-dir notes
          :characters-dir (expand-file-name "characters/" root)
          :research-dir (expand-file-name "research/" root)
          :timeline-file (expand-file-name "timeline.org" root))))

;;; Feature Detection

(defvar writing--available-features nil
  "Alist of (FEATURE . AVAILABLE-P) for optional dependencies.")

(defun writing-check-feature (feature)
  "Check if FEATURE is available and cache result."
  (or (alist-get feature writing--available-features)
      (let ((available (featurep feature)))
        (push (cons feature available) writing--available-features)
        available)))

(defmacro writing-when-feature (feature &rest body)
  "Execute BODY if FEATURE is available, otherwise show message."
  (declare (indent 1))
  `(if (writing-check-feature ',feature)
       (progn ,@body)
     (user-error "Feature %s not available. Install required package" ',feature)))

;;; Helper Functions

(defun writing/window-perc (pct)
  "Calculate window width as percentage of frame.

PCT should be a float between 0.0 and 1.0 representing the desired
percentage of `frame-width'.  Returns the floor of the calculation
as an integer suitable for window sizing functions.

Example: (writing/window-perc 0.25) with a 200-char frame returns 50."
  (floor (* (frame-width) pct)))

(defun writing/sanitize-filename (title)
  "Sanitize TITLE for safe use as filename component.
Removes path separators and other problematic characters."
  (let ((safe-title (replace-regexp-in-string "[/\\:]" "_" title)))
    (replace-regexp-in-string "^\\.+" "" safe-title)))

(defun writing/validate-directory (directory &optional create)
  "Validate that DIRECTORY exists.
If CREATE is non-nil and directory doesn't exist, ask user to create it.
Returns t if directory exists or was created, nil otherwise."
  (cond
   ((file-directory-p directory) t)
   (create
    (when (yes-or-no-p (format "Directory %s does not exist. Create it? " directory))
      (make-directory directory t)
      t))
   (t nil)))

;;; Error Handling Wrapper

(defmacro writing-with-error-handling (name &rest body)
  "Execute BODY with standard error handling for writing functions.
NAME should be a string identifying the function for error messages."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error
      (message "Error in %s: %s" ,name (error-message-string err))
      nil)))

(provide 'writing-core)

;;; writing-core.el ends here
