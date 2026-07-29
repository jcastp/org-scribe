;;; test-location-links.el --- Tests for location linking system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for the location linking module.
;; Tests ID-based linking for locations in writing projects.
;;
;; Note: The helper functions for extracting text from ID links
;; are tested in test-search-links.el and work for all link types.

;;; Code:

(require 'ert)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../search" default-directory))
  (add-to-list 'load-path (expand-file-name "../linking" default-directory))
  (add-to-list 'load-path (expand-file-name "../capture" default-directory)))

(require 'org-scribe-location-links)

;;; Module Loading Tests

(ert-deftest test-location-links-module-loads ()
  "Test that org-scribe-location-links module loads without errors."
  (should (featurep 'org-scribe-location-links)))

;;; Function Availability Tests

(ert-deftest test-location-links-functions-defined ()
  "Test that all public location linking functions are defined."
  ;; Core functions
  (should (fboundp 'org-scribe-add-location-ids))
  (should (fboundp 'org-scribe-insert-location-link))
  (should (fboundp 'org-scribe-insert-multiple-location-links))
  (should (fboundp 'org-scribe-set-scene-locations))

  ;; Batch operations
  (should (fboundp 'org-scribe-link-scene-locations))
  (should (fboundp 'org-scribe-link-all-scene-locations))

  ;; Setup wizard
  (should (fboundp 'org-scribe-setup-location-links)))

;;; Helper Function Tests

(ert-deftest test-location-links-helper-functions-defined ()
  "Test that helper functions are defined."
  (should (fboundp 'org-scribe--ensure-location-has-id))
  (should (fboundp 'org-scribe--add-id-to-all-locations))
  (should (fboundp 'org-scribe--get-location-name-at-point))
  (should (fboundp 'org-scribe--get-location-file))
  (should (fboundp 'org-scribe--get-all-locations))
  (should (fboundp 'org-scribe--create-location-link))
  (should (fboundp 'org-scribe--link-locations-in-property)))

;;; Location Link Creation Tests

(ert-deftest test-location-link-creation ()
  "Test location link creation with ID alist."
  (let* ((id-alist '(("Downtown" . ("loc-downtown-001" . "Downtown"))
                     ("Coffee Shop" . ("loc-coffee-shop-002" . "Coffee Shop: Central Perk"))))
         (link1 (org-scribe--create-location-link "Downtown" id-alist))
         (link2 (org-scribe--create-location-link "Coffee Shop" id-alist))
         (link3 (org-scribe--create-location-link "Unknown" id-alist)))

    ;; Should create ID link for known location
    (should (string= link1 "[[id:loc-downtown-001][Downtown]]"))
    (should (string= link2 "[[id:loc-coffee-shop-002][Coffee Shop]]"))

    ;; Should return plain text for unknown location (fallback)
    (should (string= link3 "Unknown"))))

;;; Location Database Tests

(ert-deftest test-location-database-function-defined ()
  "Test that location database function is defined."
  (should (fboundp 'org-scribe--get-all-locations)))

(ert-deftest test-location-database-structure ()
  "Test that location database returns correct structure."
  ;; The function should return nil if no location file exists
  ;; or a list of (NAME . (ID . HEADING)) tuples
  (let ((result (org-scribe--get-all-locations)))
    ;; Result should be either nil or a list
    (should (or (null result)
                (listp result)))

    ;; If not nil, each element should be a cons cell
    (when result
      (dolist (item result)
        (should (consp item))
        (should (stringp (car item)))  ; Name is a string
        (should (consp (cdr item)))     ; (ID . HEADING) is a cons
        (should (stringp (cadr item)))  ; ID is a string
        ))))

;;; Location Name Extraction Tests

(ert-deftest test-location-name-extraction-function-defined ()
  "Test that location name extraction function is defined."
  (should (fboundp 'org-scribe--get-location-name-at-point)))

;;; ID Ensurement Tests

(ert-deftest test-ensure-location-has-id ()
  "Test ensuring a location heading has an ID."
  ;; This function wraps org-id-get-create
  (should (fboundp 'org-scribe--ensure-location-has-id)))

;;; Batch Update Tests

(ert-deftest test-link-locations-in-property-function-defined ()
  "Test that property linking function is defined."
  (should (fboundp 'org-scribe--link-locations-in-property)))

;;; Hook Integration Tests

(ert-deftest test-location-capture-hook-defined ()
  "Test that unified entity capture hook function is defined."
  (should (fboundp 'org-scribe--capture-finalize-add-entity-id)))

(ert-deftest test-location-capture-hook-registered ()
  "Test that unified entity capture hook is registered."
  (should (memq 'org-scribe--capture-finalize-add-entity-id
                org-capture-before-finalize-hook)))

;;; Location File Detection Tests

(ert-deftest test-location-file-detection ()
  "Test that location file detection works."
  (let ((file (org-scribe--get-location-file)))
    (should (stringp file))
    (should (or (string-match-p "locations\\.org$" file)
                (string-match-p "localizaciones\\.org$" file)
                (string-match-p "notes\\.org$" file)
                (string-match-p "notas\\.org$" file)))))

;;; Integration with Capture System Tests

(ert-deftest test-location-file-uses-capture-system ()
  "Test that location file detection uses capture system."
  ;; org-scribe--get-location-file should delegate to org-scribe-capture-location-file
  (should (equal (org-scribe--get-location-file)
                 (org-scribe-capture-location-file))))

;;; Spanish Heading Detection Tests

(ert-deftest test-location-heading-p-detects-spanish-localizacion ()
  "org-scribe--location-heading-p recognizes the Spanish \"Localización\" heading."
  (with-temp-buffer
    (org-mode)
    (insert "* Localización Principal 1\n** Descripción General\n")
    (goto-char (point-min))
    (org-back-to-heading)
    (should (org-scribe--location-heading-p))))

;;; Subsection Over-Matching Regression Tests

(ert-deftest test-location-heading-p-rejects-subsection ()
  "org-scribe--location-heading-p must not match a subsection of a location heading."
  (with-temp-buffer
    (org-mode)
    (insert "* Old Mill\n** Descripción General\n** Notes\n")
    (goto-char (point-min))
    (search-forward "Descripción General")
    (org-back-to-heading)
    (should-not (org-scribe--location-heading-p))))

;;; Short-story Heading Predicate Tests (H10)

(ert-deftest test-location-heading-p-short-story-matches-level-2-under-setting ()
  "In short-story projects, locations are level-2 headings under
\"* Setting\" (see the shipped notes.org template), not level-1.
Regression test for H10: the predicate previously required level 1
unconditionally, so short-story locations were never found."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Setting\n\n** Main Location(s)\n:PROPERTIES:\n:LOCATION_NAME: Old Mill\n:END:\n")
      (goto-char (point-min))
      (search-forward "Main Location")
      (org-back-to-heading)
      (should (org-scribe--location-heading-p)))))

(ert-deftest test-location-heading-p-short-story-rejects-locations-wrapper ()
  "The \"** Locations\" comment-only placeholder is not itself a location.
Regression test for H10: its own heading text contains \"Location\" and
would otherwise match the regexp fallback, becoming a phantom entity —
it is a landing-zone wrapper documented as \"captures go here as
subheadings\", not a real entry."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Setting\n\n** Locations\n\n** Time Period\n")
      (goto-char (point-min))
      (search-forward "Locations")
      (org-back-to-heading)
      (should-not (org-scribe--location-heading-p)))))

(ert-deftest test-location-heading-p-short-story-rejects-setting-wrapper ()
  "The level-1 \"* Setting\" section header itself is not a location entity."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Setting\n\n** Main Location(s)\n")
      (goto-char (point-min))
      (org-back-to-heading)
      (should-not (org-scribe--location-heading-p)))))

;;; Setup Wizard Tests

(ert-deftest test-setup-wizard-defined ()
  "Test that setup wizard function is defined."
  (should (fboundp 'org-scribe-setup-location-links)))

;;; Run tests

(defun org-scribe-location-links-run-tests ()
  "Run all location linking tests."
  (interactive)
  (ert "^test-location-"))

(provide 'test-location-links)

;;; test-location-links.el ends here
