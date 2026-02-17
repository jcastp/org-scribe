;;; org-scribe-location-links.el --- Location linking system for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides ID-based linking system for locations in writing projects.
;; Uses the generic linking framework from org-scribe-linking-core.el.
;;
;; Features:
;; - Auto-create IDs when capturing new locations
;; - Insert location links with completion
;; - Link multiple locations at once
;; - Update all location links in document

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-scribe-core)
(require 'org-scribe-capture)
(require 'org-scribe-messages)
(require 'org-scribe-linking-core)
(require 'org-scribe-link-update)

;;; Location Heading Predicate

(defun org-scribe--location-heading-p ()
  "Return non-nil if the heading at point is a location heading."
  (and (>= (org-current-level) 1)
       (or (org-entry-get nil "Type")
           (org-entry-get nil "TYPE")
           (save-excursion
             (ignore-errors
               (org-up-heading-safe)
               (string-match-p "Location\\|Ubicación\\|Setting\\|Place\\|Lugar"
                              (org-get-heading t t t t)))))))

;;; Entity Descriptor

(defconst org-scribe--location-entity
  '(:file-fn org-scribe/capture-location-file
    :heading-predicate org-scribe--location-heading-p
    :properties ("Location")
    :msg-added-ids msg-added-location-ids
    :msg-ids-updated msg-location-ids-updated
    :error-no-file error-no-location-file
    :error-none-found error-no-locations-found
    :prompt-select prompt-select-location
    :prompt-select-multi prompt-select-locations-multi
    :error-no-id error-no-id-for-location
    :msg-inserted-links msg-inserted-location-links
    :msg-no-selected msg-no-locations-selected
    :msg-set msg-set-locations
    :msg-updated-single msg-updated-location
    :msg-no-updates msg-no-updates-needed
    :msg-updated-links msg-updated-location-links
    :msg-setting-up msg-setting-up-location-links
    :question-link-existing question-link-existing-locations
    :msg-setup-complete msg-location-setup-complete
    :msg-updated-link-names msg-updated-link-names
    :msg-no-link-updates-type "location"
    :msg-updated-all-type "location")
  "Entity descriptor for locations.")

;;; Generated API Functions

(org-scribe-define-entity org-scribe--location-entity
  :get-file-name         org-scribe--get-location-file
  :get-all-name          org-scribe--get-all-locations
  :create-link-name      org-scribe--create-location-link
  :add-ids-to-all-name   org-scribe--add-id-to-all-locations
  :add-ids-name          org-scribe/add-location-ids
  :insert-link-name      org-scribe/insert-location-link
  :insert-multi-name     org-scribe/insert-multiple-location-links
  :set-scene-name        org-scribe/set-scene-locations
  :set-scene-property    "Location"
  :link-in-prop-name     org-scribe--link-locations-in-property
  :link-scene-name       org-scribe/link-scene-locations
  :link-all-name         org-scribe/link-all-scene-locations
  :setup-name            org-scribe/setup-location-links
  :setup-add-ids-fn      org-scribe/add-location-ids
  :setup-link-all-fn     org-scribe/link-all-scene-locations
  :update-names-name     org-scribe/update-location-link-names
  :update-all-name       org-scribe/update-all-location-link-names)

;;; Legacy Aliases

(defun org-scribe--ensure-location-has-id ()
  "Ensure the current location heading has a unique ID."
  (org-id-get-create))

(defalias 'org-scribe--get-location-name-at-point 'org-scribe--entity-name-at-point)

(provide 'org-scribe-location-links)

;;; org-scribe-location-links.el ends here
