;;; org-scribe-object-links.el --- Object linking system for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides ID-based linking system for objects in writing projects.
;; Uses the generic linking framework from org-scribe-linking-core.el.
;;
;; Features:
;; - Auto-create IDs when capturing new objects
;; - Insert object links with completion
;; - Link multiple objects at once
;; - Update all object links in document

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-scribe-core)
(require 'org-scribe-capture)
(require 'org-scribe-messages)
(require 'org-scribe-linking-core)
(require 'org-scribe-link-update)

;;; Object Heading Predicate

(defun org-scribe--object-heading-p ()
  "Return non-nil if the heading at point is an object heading.
Objects are the headings directly under the Objects section of the
design notebook, and each carries a `Type' property.  There is no
heading-text fallback: the old layout's subsection headings (\"Symbolic
Objects\") contain the section's own name and would become phantom
objects.  Requiring `Type' also keeps those subsections, and anything
nested below them, out of the entity set."
  (and (org-scribe--heading-parent-section-p 'objects)
       (or (org-entry-get nil "Type")
           (org-entry-get nil "TYPE"))
       t))

;;; Entity Definition

(org-scribe-define-entity object
  ;; ── Config (entity descriptor) ──────────────────────────────────────
  :file-fn                  org-scribe--design-file
  :heading-predicate        org-scribe--object-heading-p
  :properties               (objects)
  :msg-added-ids            msg-added-object-ids
  :msg-ids-updated          msg-object-ids-updated
  :error-no-file            error-no-object-file
  :error-none-found         error-no-objects-found
  :prompt-select            prompt-select-object
  :prompt-select-multi      prompt-select-objects-multi
  :error-no-id              error-no-id-for-object
  :msg-inserted-links       msg-inserted-object-links
  :msg-no-selected          msg-no-objects-selected
  :msg-set                  msg-set-objects
  :msg-updated-single       msg-updated-object
  :msg-no-updates           msg-no-updates-needed
  :msg-updated-links        msg-updated-object-links
  :msg-setting-up           msg-setting-up-object-links
  :question-link-existing   question-link-existing-objects
  :msg-setup-complete       msg-object-setup-complete
  :msg-updated-link-names   msg-updated-link-names
  :msg-no-link-updates-type "object"
  :msg-updated-all-type     "object"
  ;; ── Generated function names ─────────────────────────────────────
  :get-file-name            org-scribe--get-object-file
  :get-all-name             org-scribe--get-all-objects
  :create-link-name         org-scribe--create-object-link
  :add-ids-to-all-name      org-scribe--add-id-to-all-objects
  :add-ids-name             org-scribe-add-object-ids
  :insert-link-name         org-scribe-insert-object-link
  :insert-multi-name        org-scribe-insert-multiple-object-links
  :set-scene-name           org-scribe-set-scene-objects
  :set-scene-property       objects
  :link-in-prop-name        org-scribe--link-objects-in-property
  :link-scene-name          org-scribe-link-scene-objects
  :link-all-name            org-scribe-link-all-scene-objects
  :setup-name               org-scribe-setup-object-links
  :setup-add-ids-fn         org-scribe-add-object-ids
  :setup-link-all-fn        org-scribe-link-all-scene-objects
  :update-names-name        org-scribe-update-object-link-names
  :update-all-name          org-scribe-update-all-object-link-names)

;;; Legacy Aliases

(defun org-scribe--ensure-object-has-id ()
  "Ensure the current object heading has a unique ID."
  (org-id-get-create))

(defalias 'org-scribe--get-object-name-at-point 'org-scribe--entity-name-at-point)

(provide 'org-scribe-object-links)

;;; org-scribe-object-links.el ends here
