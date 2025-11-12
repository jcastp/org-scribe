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
