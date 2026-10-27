;;; org-scribe-companion-commands.el --- Visual companion: write operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; The write operations the browser can ask for through POST /api/command.
;; Phase 2: only the dispatcher exists, and every operation is still
;; `unsupported'.  The operations arrive in the phases that need them.
;;
;; A command's answer is a plist: (:ok t :revision REV ...) or
;; (:ok :false :error (:code CODE :message TEXT)), ready for `json-serialize'.
;; Error codes: not-found, invalid, conflict, read-only, unsupported.

;;; Code:

(require 'org-scribe-messages)

(defconst org-scribe-companion-commands--operations nil
  "Alist of (OP-NAME . FUNCTION).  FUNCTION takes ROOT and the args plist.")

(defun org-scribe-companion-commands-error (code message)
  "Return the failure answer with error CODE (a string) and MESSAGE."
  (list :ok :false :error (list :code code :message message)))

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
