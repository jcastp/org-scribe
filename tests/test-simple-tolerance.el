;;; test-simple-tolerance.el --- The rest of org-scribe on a simple project -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; A simple project has no plot points, no Starting Gate, no parts and no
;; method vocabulary.  These tests create real ones (novel and short story,
;; English and Spanish) and run the features that read the notebook against
;; them: the health report, the companion snapshot, the plot-point commands,
;; the calendar, scene snapshots and the word target.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (dolist (dir '("../core" "../search" "../linking" "../capture" "../templates"
                 "../reporting" "../revision" "../companion"))
    (add-to-list 'load-path (expand-file-name dir default-directory))))

(require 'org-scribe-project)
(require 'org-scribe-health)
(require 'org-scribe-plot-point-links)
(require 'org-scribe-snapshot)
(require 'org-scribe-companion)

(defmacro test-simple-tolerance--each-project (&rest body)
  "Run BODY in each of four created simple projects.
Binds `kind', `lang', `root', `manuscript' and `notebook'."
  (declare (indent 0))
  `(dolist (spec '((novel . en) (novel . es) (short-story . en) (short-story . es)))
     (let* ((kind (car spec)) (lang (cdr spec))
            (base (make-temp-file "test-simple-tolerance-" t))
            (root (expand-file-name "P" base))
            (default-directory (file-name-as-directory root)))
       (unwind-protect
           (progn
             (cl-letf (((symbol-function 'find-file) #'ignore)
                       ((symbol-function 'project-remember-project) #'ignore))
               (if (eq kind 'novel)
                   (org-scribe-create-novel-project base "P" lang 'simple)
                 (org-scribe-create-short-story-project base "P" lang nil 'simple)))
             (org-scribe-project-type-cache-clear)
             (let ((manuscript (plist-get (org-scribe-project-structure) :manuscript-file))
                   (notebook (plist-get (org-scribe-project-structure) :design-file)))
               (ignore manuscript notebook kind lang root)
               ,@body))
         (dolist (b (buffer-list))
           (when (and (buffer-file-name b) (string-prefix-p base (buffer-file-name b)))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b)))
         (delete-directory base t)))))

(ert-deftest test-simple-tolerance-health-has-no-method-sections ()
  "The health report runs, shows no gate and no plot points, and flags nothing
about them."
  (test-simple-tolerance--each-project
    (let ((data (org-scribe--health-data)))
      (should (null (plist-get data :gate)))
      (should-not (plist-get data :plot-points-p))
      (should (null (plist-get (plist-get data :orphans) :plot-point))))
    (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (set-buffer b))))
      (org-scribe-project-health))
    (with-current-buffer "*org-scribe-health*"
      (let ((text (buffer-string)))
        (should (string-match-p "Overview\\|Resumen" text))
        (should-not (string-match-p "Starting Gate\\|Puerta de salida" text))
        (should-not (string-match-p "non-negotiable\\|irrenunciable" text))))))

(ert-deftest test-simple-tolerance-plot-point-commands-refuse-politely ()
  "With no plot points the commands signal a `user-error', worded for
the simple method as well as a short story."
  (test-simple-tolerance--each-project
    (should (null (org-scribe--get-all-plot-points)))
    (dolist (command '(org-scribe-setup-plot-point-links
                       org-scribe-set-scene-plot-points
                       org-scribe-jump-to-plot-point
                       org-scribe-add-plot-point-ids
                       org-scribe-insert-plot-point-link
                       org-scribe-insert-multiple-plot-point-links
                       org-scribe-link-scene-plot-points
                       org-scribe-link-all-scene-plot-points
                       org-scribe-update-plot-point-link-names
                       org-scribe-update-all-plot-point-link-names))
      (let ((err (should-error (funcall command) :type 'user-error)))
        (should (string-match-p
                 "simple\\|cuento"
                 (let ((org-scribe-message-language lang))
                   (cadr err))))))))

(ert-deftest test-simple-tolerance-companion-forms-offer-the-usual-choices ()
  "The simple method offers the same Role, Type and Status lists as the others."
  (test-simple-tolerance--each-project
    (let ((choices (org-scribe-companion--entity-choices)))
      (should (plist-get choices :character))
      (should (plist-get choices :plot))
      (should (plist-get choices :object)))
    (let ((json (json-parse-string (org-scribe-companion--snapshot-json)
                                   :object-type 'alist)))
      (should (assq 'entities json))
      (should (alist-get 'entityChoices json)))))

(ert-deftest test-simple-tolerance-notebook-sections-are-added-flat ()
  "A snapshot and a calendar make their sections at level 1, as in a short story."
  (test-simple-tolerance--each-project
    (org-scribe-calendar-create "Era" "Year 1")
    (with-current-buffer (find-file-noselect manuscript)
      (goto-char (point-min))
      (re-search-forward (if (eq lang 'es) "^\\*+ .*Escena" "^\\*+ .*Scene"))
      (org-scribe-snapshot-scene))
    (with-temp-buffer
      (insert-file-contents notebook)
      (org-mode)
      (let ((levels (org-map-entries
                     (lambda () (cons (org-get-heading t t t t) (org-current-level))))))
        (should (equal 1 (cdr (assoc (org-scribe-lang-heading 'calendar lang) levels))))
        (should (equal 1 (cdr (assoc (org-scribe-lang-heading 'versions lang) levels))))))))

(ert-deftest test-simple-tolerance-word-target-can-be-changed ()
  "`org-scribe-set-word-target' works: the manuscript is the shared one."
  (test-simple-tolerance--each-project
    (org-scribe-set-word-target 60000 (if (eq kind 'novel) 'reset 'keep) t)
    (should (= 60000 (org-scribe-project-word-target)))))

(provide 'test-simple-tolerance)

;;; test-simple-tolerance.el ends here
