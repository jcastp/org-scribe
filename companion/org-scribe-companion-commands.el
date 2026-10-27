;;; org-scribe-companion-commands.el --- Visual companion: write operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; The operations the browser can ask for through POST /api/command.
;; Phase 3 has `goto' (show a heading in Emacs) and `save'.  The editing
;; operations arrive in the phases that need them; any other name is
;; `unsupported'.
;;
;; Headings are found by ID, only in the project's manuscript and design
;; files, never by position.
;;
;; A command's answer is a plist: (:ok t :revision REV ...) or
;; (:ok :false :error (:code CODE :message TEXT)), ready for `json-serialize'.
;; Error codes: not-found, invalid, conflict, read-only, unsupported.

;;; Code:

(require 'org)
(require 'org-scribe-messages)
(require 'org-scribe-core)
(require 'org-scribe-companion-model)

(defvar org-scribe-companion-raise-frame)

(defconst org-scribe-companion-commands--operations
  '(("goto" . org-scribe-companion-commands--goto)
    ("save" . org-scribe-companion-commands--save))
  "Alist of (OP-NAME . FUNCTION).  FUNCTION takes ROOT and the args plist.")

(defun org-scribe-companion-commands-error (code message)
  "Return the failure answer with error CODE (a string) and MESSAGE."
  (list :ok :false :error (list :code code :message message)))

(defun org-scribe-companion-commands--project-buffers (root)
  "Return the live buffers visiting ROOT's manuscript and design files."
  (let* ((default-directory (file-name-as-directory root))
         (structure (org-scribe-project-structure)))
    (delq nil (mapcar (lambda (file) (and file (find-buffer-visiting file)))
                      (list (plist-get structure :manuscript-file)
                            (plist-get structure :design-file))))))

(defun org-scribe-companion-commands--find (root id)
  "Return a marker at the heading with ID in ROOT's project files, or nil.
Searches the manuscript and design buffers only (opening them if
needed), ignoring any narrowing."
  (let* ((default-directory (file-name-as-directory root))
         (structure (org-scribe-project-structure)))
    (cl-some
     (lambda (file)
       (when (and file (file-exists-p file))
         (with-current-buffer (find-file-noselect file)
           (org-with-wide-buffer
            (when-let* ((pos (org-find-property "ID" id)))
              (copy-marker pos))))))
     (list (plist-get structure :manuscript-file)
           (plist-get structure :design-file)))))

(defun org-scribe-companion-commands--ok (root)
  "Return the success answer for the project at ROOT."
  (list :ok t :revision (org-scribe-companion--revision-string root)))

(defun org-scribe-companion-commands--arg-string (args key)
  "Return the string argument KEY of the plist ARGS, or nil."
  (let ((value (and (listp args) (plist-get args key))))
    (and (stringp value) (not (string-empty-p value)) value)))

(defun org-scribe-companion-commands--goto (root args)
  "Show the heading ARGS names in Emacs, in the project at ROOT.
ARGS has `:id'.  Widens the buffer if the heading is outside a narrowing,
reveals it, and raises the frame when `org-scribe-companion-raise-frame'."
  (if-let* ((id (org-scribe-companion-commands--arg-string args :id)))
      (if-let* ((marker (org-scribe-companion-commands--find root id)))
          (progn
            (pop-to-buffer (marker-buffer marker))
            (when (or (< marker (point-min)) (> marker (point-max))) (widen))
            (goto-char marker)
            (set-marker marker nil)
            (org-fold-show-context 'link-search)
            (org-fold-show-entry)
            (recenter)
            (when org-scribe-companion-raise-frame
              (select-frame-set-input-focus (selected-frame)))
            (org-scribe-companion-commands--ok root))
        (org-scribe-companion-commands-error
         "not-found" (org-scribe-msg 'companion-error-not-found id)))
    (org-scribe-companion-commands-error
     "invalid" (org-scribe-msg 'companion-error-missing-arg "id"))))

(defun org-scribe-companion-commands--save (root _args)
  "Save the manuscript and design buffers of the project at ROOT, if modified."
  (dolist (buffer (org-scribe-companion-commands--project-buffers root))
    (with-current-buffer buffer
      (when (buffer-modified-p) (save-buffer))))
  (org-scribe-companion-commands--ok root))

(defun org-scribe-companion-commands-run (op args root)
  "Run the operation OP (a string) with the plist ARGS for the project at ROOT.
Returns the answer plist described in the Commentary."
  (let ((function (cdr (assoc op org-scribe-companion-commands--operations))))
    (if function
        (funcall function root args)
      (org-scribe-companion-commands-error
       "unsupported" (org-scribe-msg 'companion-error-unsupported-op (format "%s" op))))))

(provide 'org-scribe-companion-commands)

;;; org-scribe-companion-commands.el ends here
