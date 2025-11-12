;;; writing-capture.el --- Capture system for emacs-writing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Project-aware capture system for writing notes.
;; Automatically determines the appropriate notes file based on project structure.

;;; Code:

(require 'org-capture)
(require 'project)
(require 'writing-core)
(require 'writing-config)

;;; Capture Target File Detection

(defun writing/capture-target-file (&optional create-if-missing)
  "Determine the appropriate notes file for org-capture in writing environment.
Uses `project-current' to find the project base directory as reference point.
Returns the file path based on the following priority:
1. notes/notes.org (relative to project root)
2. notas/notas.org - Spanish (relative to project root)
3. novel-notes.org (in project root)
4. notes.org (in project root)
5. current buffer if none of the above exist

If CREATE-IF-MISSING is non-nil, create the first priority notes
file that doesn't exist."
  (let* ((project-dir (if-let ((project (project-current)))
                          (project-root project)
                        (file-name-directory (or (buffer-file-name) default-directory))))
         (notes-subdir-en (expand-file-name "notes/notes.org" project-dir))
         (notes-subdir-es (expand-file-name "notas/notas.org" project-dir))
         (novel-notes (expand-file-name "novel-notes.org" project-dir))
         (notes (expand-file-name "notes.org" project-dir))
         (target (cond
                  ((file-exists-p notes-subdir-en) notes-subdir-en)
                  ((file-exists-p notes-subdir-es) notes-subdir-es)
                  ((file-exists-p novel-notes) novel-notes)
                  ((file-exists-p notes) notes)
                  (t (or (buffer-file-name)
                         (expand-file-name "notes.org" project-dir))))))
    (when (and create-if-missing
               (not (file-exists-p target)))
      (let ((target-dir (file-name-directory target)))
        (unless (file-directory-p target-dir)
          (make-directory target-dir t)))
      (with-temp-file target
        (insert "#+TITLE: Writing Notes\n")
        (insert (format "#+AUTHOR: %s\n" user-full-name))
        (insert (format "#+EMAIL: %s\n\n" user-mail-address))
        (insert "* Notes\n")))
    target))

;;; Capture Templates

(defvar writing/capture-templates
  '(("w" "Writing Note" entry
     (file+headline writing/capture-target-file "Notes")
     "** TODO %?\n  %U\n  %i"
     :empty-lines 1))
  "Capture templates specific to the writing environment.")

;;; Capture Function

;;;###autoload
(defun writing/capture-to-file ()
  "Capture notes to writing project or file.
Automatically determines the appropriate notes file based on project structure."
  (interactive)
  ;; Choose the destination that actually exists
  (let ((target (writing/capture-target-file t)))  ; Create if missing
    ;; Build a temporary capture template that points at TARGET
    (let ((org-capture-templates
           writing/capture-templates))
      ;; Run the capture UI
      (org-capture))))

(provide 'writing-capture)

;;; writing-capture.el ends here
