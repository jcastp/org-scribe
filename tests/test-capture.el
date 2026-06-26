;;; test-capture.el --- Tests for capture system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for the project-aware capture system.
;; Tests capture target file detection, file creation,
;; and template availability.

;;; Code:

(require 'ert)
(require 'org)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../capture" default-directory)))

(require 'org-scribe-core)
(require 'org-scribe-capture)

;;; Module Loading Tests

(ert-deftest test-capture-module-loads ()
  "Test that org-scribe-capture module loads without errors."
  (should (featurep 'org-scribe-capture)))

;;; Function Availability Tests

(ert-deftest test-capture-functions-defined ()
  "Test that all public capture functions are defined."
  ;; Main capture functions
  (should (fboundp 'org-scribe-capture-to-file))
  (should (fboundp 'org-scribe-capture-character))
  (should (fboundp 'org-scribe-capture-location))
  (should (fboundp 'org-scribe-capture-object))
  (should (fboundp 'org-scribe-capture-timeline))

  ;; Target file detection functions
  (should (fboundp 'org-scribe-capture-target-file))
  (should (fboundp 'org-scribe-capture-character-file))
  (should (fboundp 'org-scribe-capture-location-file))
  (should (fboundp 'org-scribe-capture-object-file))
  (should (fboundp 'org-scribe-capture-timeline-file)))

;;; Helper Function Tests

(ert-deftest test-capture-helper-functions-defined ()
  "Test that helper functions are defined."
  (should (fboundp 'org-scribe--create-short-story-notes-file))
  (should (fboundp 'org-scribe--create-novel-capture-file))
  (should (fboundp 'org-scribe--create-capture-file)))

;;; Capture Template Tests

(ert-deftest test-capture-templates-defined ()
  "Test that capture templates are defined."
  (should (boundp 'org-scribe-capture-templates))
  (should (listp org-scribe-capture-templates))
  (should (> (length org-scribe-capture-templates) 0)))

(ert-deftest test-character-capture-templates-defined ()
  "Test that character capture templates are defined."
  (should (boundp 'org-scribe-character-capture-templates))
  (should (listp org-scribe-character-capture-templates))
  (should (> (length org-scribe-character-capture-templates) 0)))

(ert-deftest test-location-capture-templates-defined ()
  "Test that location capture templates are defined."
  (should (boundp 'org-scribe-location-capture-templates))
  (should (listp org-scribe-location-capture-templates))
  (should (> (length org-scribe-location-capture-templates) 0)))

(ert-deftest test-object-capture-templates-defined ()
  "Test that object capture templates are defined."
  (should (boundp 'org-scribe-object-capture-templates))
  (should (listp org-scribe-object-capture-templates))
  (should (> (length org-scribe-object-capture-templates) 0)))

(ert-deftest test-timeline-capture-templates-defined ()
  "Test that timeline capture templates are defined."
  (should (boundp 'org-scribe-timeline-capture-templates))
  (should (listp org-scribe-timeline-capture-templates))
  (should (> (length org-scribe-timeline-capture-templates) 0)))

;;; File Creation Tests

(ert-deftest test-create-short-story-notes-file ()
  "Test creation of short story notes file."
  (let ((temp-file (make-temp-file "test-notes-" nil ".org")))
    (unwind-protect
        (progn
          (org-scribe--create-short-story-notes-file temp-file)

          ;; Verify file was created
          (should (file-exists-p temp-file))

          ;; Verify content has key sections
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              ;; Check has metadata and main sections
              (should (string-match-p "TITLE" content))
              (should (string-match-p "AUTHOR" content))
              (should (string-match-p "Characters" content))
              (should (string-match-p "Plot" content))
              (should (string-match-p "Setting" content)))))

      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-create-novel-capture-file-characters ()
  "Test creation of novel characters file."
  (let ((temp-file (make-temp-file "test-characters-" nil ".org")))
    (unwind-protect
        (progn
          (org-scribe--create-novel-capture-file temp-file 'characters)

          ;; Verify file was created
          (should (file-exists-p temp-file))

          ;; Verify content has expected headers
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "Character" content))
              (should (string-match-p "TITLE" content))
              (should (string-match-p "AUTHOR" content))
              (should (string-match-p "DATE" content)))))

      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-create-novel-capture-file-locations ()
  "Test creation of novel locations file."
  (let ((temp-file (make-temp-file "test-locations-" nil ".org")))
    (unwind-protect
        (progn
          (org-scribe--create-novel-capture-file temp-file 'locations)

          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "Location" content))
              (should (string-match-p "TITLE" content)))))

      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-create-novel-capture-file-objects ()
  "Test creation of novel objects file."
  (let ((temp-file (make-temp-file "test-objects-" nil ".org")))
    (unwind-protect
        (progn
          (org-scribe--create-novel-capture-file temp-file 'objects)

          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "Object" content))
              (should (string-match-p "TITLE" content)))))

      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-create-novel-capture-file-timeline ()
  "Test creation of novel timeline file."
  (let ((temp-file (make-temp-file "test-timeline-" nil ".org")))
    (unwind-protect
        (progn
          (org-scribe--create-novel-capture-file temp-file 'timeline)

          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "Timeline" content))
              (should (string-match-p "TITLE" content)))))

      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-create-novel-capture-file-notes ()
  "Test creation of novel notes file."
  (let ((temp-file (make-temp-file "test-notes-" nil ".org")))
    (unwind-protect
        (progn
          (org-scribe--create-novel-capture-file temp-file 'notes)

          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "Note" content))
              (should (string-match-p "TITLE" content)))))

      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Target File Detection Tests

(ert-deftest test-capture-target-file-detection ()
  "Test that capture target file detection works."
  ;; Without a proper project structure, should return current buffer or default
  (let ((target (org-scribe-capture-target-file)))
    (should (stringp target))
    (should (string-match-p "\\.org$" target))))

(ert-deftest test-capture-character-file-detection ()
  "Test that character file detection works."
  (let ((target (org-scribe-capture-character-file)))
    (should (stringp target))
    (should (or (string-match-p "characters\\.org$" target)
                (string-match-p "personajes\\.org$" target)
                (string-match-p "notes\\.org$" target)
                (string-match-p "notas\\.org$" target)))))

(ert-deftest test-capture-location-file-detection ()
  "Test that location file detection works."
  (let ((target (org-scribe-capture-location-file)))
    (should (stringp target))
    (should (or (string-match-p "locations\\.org$" target)
                (string-match-p "localizaciones\\.org$" target)
                (string-match-p "notes\\.org$" target)
                (string-match-p "notas\\.org$" target)))))

(ert-deftest test-capture-object-file-detection ()
  "Test that object file detection works."
  (let ((target (org-scribe-capture-object-file)))
    (should (stringp target))
    (should (or (string-match-p "objects\\.org$" target)
                (string-match-p "objetos\\.org$" target)
                (string-match-p "notes\\.org$" target)
                (string-match-p "notas\\.org$" target)))))

(ert-deftest test-capture-timeline-file-detection ()
  "Test that timeline file detection works."
  (let ((target (org-scribe-capture-timeline-file)))
    (should (stringp target))
    (should (or (string-match-p "timeline\\.org$" target)
                (string-match-p "cronologia\\.org$" target)
                (string-match-p "notes\\.org$" target)
                (string-match-p "notas\\.org$" target)))))

;;; Template Structure Tests

(ert-deftest test-character-template-has-required-fields ()
  "Test that character template includes all required fields."
  (let ((template-string (nth 4 (car org-scribe-character-capture-templates))))
    (should (string-match-p ":ID:" template-string))
    (should (string-match-p ":Role:" template-string))
    (should (string-match-p ":Age:" template-string))
    (should (string-match-p ":Gender:" template-string))
    (should (string-match-p "Physical Description" template-string))
    (should (string-match-p "Personality" template-string))
    (should (string-match-p "Background" template-string))
    (should (string-match-p "Goal, Motivation, Conflict" template-string))
    (should (string-match-p "Character Arc" template-string))
    (should (string-match-p "Relationships" template-string))))

(ert-deftest test-location-template-has-required-fields ()
  "Test that location template includes all required fields."
  (let ((template-string (nth 4 (car org-scribe-location-capture-templates))))
    (should (string-match-p ":ID:" template-string))
    (should (string-match-p ":Type:" template-string))
    (should (string-match-p ":Importance:" template-string))
    (should (string-match-p "General Description" template-string))
    (should (string-match-p "Geography" template-string))
    (should (string-match-p "Cultural Aspects" template-string))
    (should (string-match-p "History" template-string))
    (should (string-match-p "Notable Features" template-string))
    (should (string-match-p "Atmosphere & Mood" template-string))))

(ert-deftest test-object-template-has-required-fields ()
  "Test that object template includes all required fields."
  (let ((template-string (nth 4 (car org-scribe-object-capture-templates))))
    (should (string-match-p ":Type:" template-string))
    (should (string-match-p ":Owner:" template-string))
    (should (string-match-p ":Status:" template-string))
    (should (string-match-p "Physical Description" template-string))
    (should (string-match-p "Origin" template-string))
    (should (string-match-p "Properties" template-string))
    (should (string-match-p "Plot" template-string))
    (should (string-match-p "Symbolism" template-string))))

(ert-deftest test-timeline-template-has-required-fields ()
  "Test that timeline template includes all required fields."
  (let ((template-string (nth 4 (car org-scribe-timeline-capture-templates))))
    (should (string-match-p ":ID:" template-string))
    (should (string-match-p ":Type:" template-string))
    (should (string-match-p ":Date:" template-string))
    (should (string-match-p ":Characters:" template-string))
    (should (string-match-p ":Location:" template-string))
    (should (string-match-p "Description" template-string))
    (should (string-match-p "Consequences" template-string))
    (should (string-match-p "Connections" template-string))))

;;; Auto-ID Generation Tests

(ert-deftest test-character-template-generates-id ()
  "Test that character template includes auto-ID generation."
  (let ((template-string (nth 4 (car org-scribe-character-capture-templates))))
    ;; Should have %(org-id-new) in ID property
    (should (string-match-p ":ID: %(org-id-new)" template-string))))

(ert-deftest test-location-template-generates-id ()
  "Test that location template includes auto-ID generation."
  (let ((template-string (nth 4 (car org-scribe-location-capture-templates))))
    (should (string-match-p ":ID: %(org-id-new)" template-string))))

(ert-deftest test-timeline-template-generates-id ()
  "Test that timeline template includes auto-ID generation."
  (let ((template-string (nth 4 (car org-scribe-timeline-capture-templates))))
    (should (string-match-p ":ID: %(org-id-new)" template-string))))

;;; ─────────────────────────────────────────────
;;; File Creation Routing — both project types
;;; ─────────────────────────────────────────────

(ert-deftest test-capture-create-file-dispatcher-short-story ()
  "Test that the file dispatcher creates a short-story notes file."
  (let ((temp-file (make-temp-file "test-notes-ss-" nil ".org")))
    (unwind-protect
        (progn
          (delete-file temp-file)  ; start clean
          (org-scribe--create-capture-file temp-file 'short-story 'characters)
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            ;; Short-story notes file contains Characters section
            (should (string-match-p "Characters" (buffer-string)))
            (should (string-match-p "Plot" (buffer-string)))
            (should (string-match-p "Setting" (buffer-string)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-capture-create-file-dispatcher-novel ()
  "Test that the file dispatcher creates an individual novel capture file."
  (let ((temp-file (make-temp-file "test-chars-novel-" nil ".org")))
    (unwind-protect
        (progn
          (delete-file temp-file)
          (org-scribe--create-capture-file temp-file 'novel 'characters)
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            ;; Novel individual file has the entity-type title
            (should (string-match-p "Character" (buffer-string)))
            (should (string-match-p "TITLE" (buffer-string)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-capture-create-file-dispatcher-plot-any-type ()
  "Test that the plot dispatcher creates a plot file regardless of project type."
  (let ((temp-file (make-temp-file "test-plot-" nil ".org")))
    (unwind-protect
        (progn
          (delete-file temp-file)
          ;; Plot gets its own structure even for short-story projects
          (org-scribe--create-capture-file temp-file 'short-story 'plot)
          (should (file-exists-p temp-file))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (should (string-match-p "Plot" (buffer-string)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; ─────────────────────────────────────────────
;;; create-if-missing — auto-creates target file
;;; ─────────────────────────────────────────────

(ert-deftest test-capture-create-if-missing-novel-creates-objects-dir ()
  "Test that character-file with create-if-missing creates file for a novel project."
  (let* ((temp-dir (make-temp-file "test-novel-proj-" t))
         (expected-file (expand-file-name "objects/characters.org" temp-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'org-scribe-project-root)
                   (lambda () temp-dir))
                  ((symbol-function 'org-scribe-project-type)
                   (lambda () 'novel)))
          ;; File must not exist before the call
          (should-not (file-exists-p expected-file))
          (let ((result (org-scribe-capture-character-file t)))
            (should (file-exists-p expected-file))
            (should (string-match-p "characters\\.org$" result))))
      (delete-directory temp-dir t))))

(ert-deftest test-capture-create-if-missing-short-story-creates-notes ()
  "Test that character-file with create-if-missing creates notes.org for short stories."
  (let* ((temp-dir (make-temp-file "test-ss-proj-" t))
         (expected-file (expand-file-name "notes.org" temp-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'org-scribe-project-root)
                   (lambda () temp-dir))
                  ((symbol-function 'org-scribe-project-type)
                   (lambda () 'short-story)))
          (should-not (file-exists-p expected-file))
          (let ((result (org-scribe-capture-character-file t)))
            (should (file-exists-p expected-file))
            (should (string-match-p "notes\\.org$" result))))
      (delete-directory temp-dir t))))

;;; ─────────────────────────────────────────────
;;; Project-type routing — without filesystem
;;; ─────────────────────────────────────────────

(ert-deftest test-capture-character-file-routes-short-story ()
  "Test that short-story projects route character capture to notes.org."
  (let* ((temp-dir (make-temp-file "test-route-ss-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'org-scribe-project-root)
                   (lambda () temp-dir))
                  ((symbol-function 'org-scribe-project-type)
                   (lambda () 'short-story)))
          (let ((result (org-scribe-capture-character-file)))
            ;; No notes.org exists yet, so it falls back to the default name
            (should (string-match-p "notes\\.org$" result))))
      (delete-directory temp-dir t))))

(ert-deftest test-capture-character-file-routes-novel ()
  "Test that novel projects route character capture to objects/characters.org."
  (let* ((temp-dir (make-temp-file "test-route-novel-" t))
         (char-file (expand-file-name "objects/characters.org" temp-dir)))
    (unwind-protect
        (progn
          ;; Pre-create the file so cl-find-if can find it
          (make-directory (expand-file-name "objects" temp-dir) t)
          (write-region "" nil char-file)
          (cl-letf (((symbol-function 'org-scribe-project-root)
                     (lambda () temp-dir))
                    ((symbol-function 'org-scribe-project-type)
                     (lambda () 'novel)))
            (let ((result (org-scribe-capture-character-file)))
              (should (string-match-p "characters\\.org$" result)))))
      (delete-directory temp-dir t))))

;;; ─────────────────────────────────────────────
;;; ID assignment on finalisation
;;; ─────────────────────────────────────────────

(ert-deftest test-capture-finalize-hook-adds-id-to-heading ()
  "Test that the finalize hook adds an ID to a heading that lacks one."
  (let ((temp-file (make-temp-file "test-finalize-" nil ".org")))
    (unwind-protect
        (progn
          ;; Create a minimal entity file with one heading and no ID
          (with-temp-file temp-file
            (insert "* Alice\n:PROPERTIES:\n:Role: Protagonist\n:END:\n\n"))
          (with-current-buffer (find-file-noselect temp-file)
            (unwind-protect
                (let ((org-capture-mode t))
                  (cl-letf (((symbol-function 'org-scribe-capture-character-file)
                             (lambda (&rest _) temp-file))
                            ((symbol-function 'org-scribe-capture-location-file)
                             (lambda (&rest _) "/nonexistent-loc.org"))
                            ((symbol-function 'org-scribe-capture-plot-thread-file)
                             (lambda (&rest _) "/nonexistent-plot.org")))
                    (org-scribe--capture-finalize-add-entity-id)
                    ;; The hook must have added an :ID: property
                    (should (string-match-p ":ID:" (buffer-string)))))
              (kill-buffer (current-buffer)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-capture-finalize-hook-skips-existing-id ()
  "Test that the finalize hook does not overwrite an existing ID."
  (let ((temp-file (make-temp-file "test-finalize-existing-" nil ".org"))
        (fixed-id "test-existing-id-12345"))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert (format "* Bob\n:PROPERTIES:\n:ID: %s\n:Role: Mentor\n:END:\n\n"
                            fixed-id)))
          (with-current-buffer (find-file-noselect temp-file)
            (unwind-protect
                (let ((org-capture-mode t))
                  (cl-letf (((symbol-function 'org-scribe-capture-character-file)
                             (lambda (&rest _) temp-file))
                            ((symbol-function 'org-scribe-capture-location-file)
                             (lambda (&rest _) "/nonexistent-loc.org"))
                            ((symbol-function 'org-scribe-capture-plot-thread-file)
                             (lambda (&rest _) "/nonexistent-plot.org")))
                    (org-scribe--capture-finalize-add-entity-id)
                    ;; The original ID must still be present and unchanged
                    (should (string-match-p fixed-id (buffer-string)))))
              (kill-buffer (current-buffer)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-capture-finalize-hook-inactive-outside-capture ()
  "Test that the finalize hook is a no-op when org-capture-mode is nil."
  (let ((temp-file (make-temp-file "test-finalize-noop-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Charlie\n:PROPERTIES:\n:Role: Ally\n:END:\n\n"))
          (with-current-buffer (find-file-noselect temp-file)
            (unwind-protect
                (let ((org-capture-mode nil))   ; simulate being outside capture
                  (org-scribe--capture-finalize-add-entity-id)
                  ;; No ID should have been added
                  (should-not (string-match-p ":ID:" (buffer-string))))
              (kill-buffer (current-buffer)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; ─────────────────────────────────────────────
;;; Run tests
;;; ─────────────────────────────────────────────

(defun org-scribe-capture-run-tests ()
  "Run all capture system tests."
  (interactive)
  (ert "^test-capture-\\|^test-create-\\|^test-character-\\|^test-location-\\|^test-object-\\|^test-timeline-"))

(provide 'test-capture)

;;; test-capture.el ends here
