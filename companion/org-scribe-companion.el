;;; org-scribe-companion.el --- Visual companion for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Entry point of the optional visual companion: a browser front end for a
;; novel project, with Emacs as the only program that reads and writes the
;; project files.  Off until `org-scribe-companion-enable' is set.
;;
;; The user-facing checks (switch, project type, per-project gate) live in
;; `org-scribe-companion-open' in core/org-scribe-core.el, so that with the
;; switch off this file is never loaded.  Phase 0: skeleton only.

;;; Code:

(require 'org-scribe-messages)
(require 'org-scribe-companion-model)
(require 'org-scribe-companion-commands)
(require 'org-scribe-companion-server)

(defun org-scribe-companion--open (_root)
  "Start the companion for the project at _ROOT and show it.
Not implemented yet."
  (message "%s" (org-scribe-msg 'companion-not-implemented)))

(provide 'org-scribe-companion)

;;; org-scribe-companion.el ends here
