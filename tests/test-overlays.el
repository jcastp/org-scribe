;;; test-overlays.el --- Tests for org-scribe entity tooltip system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for linking/org-scribe-overlays.el.
;; Covers tooltip formatting, the post-command deduplication logic,
;; minor-mode hook registration, and the auto-enable guard.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path (expand-file-name "core" parent-dir))
  (add-to-list 'load-path (expand-file-name "linking" parent-dir)))

(require 'org-scribe-messages)
(require 'org-scribe-overlays)

;;; Module loading

(ert-deftest test-overlays-module-loads ()
  "org-scribe-overlays feature is available after require."
  (should (featurep 'org-scribe-overlays)))

(ert-deftest test-overlays-functions-defined ()
  "All public and internal functions are defined."
  (should (fboundp 'org-scribe-overlays-mode))
  (should (fboundp 'org-scribe--overlays-id-at-point))
  (should (fboundp 'org-scribe--overlays-format-tooltip))
  (should (fboundp 'org-scribe--overlays-post-command))
  (should (fboundp 'org-scribe--overlays-maybe-enable)))

(ert-deftest test-overlays-variables-defined ()
  "Buffer-local state variable is defined."
  (should (boundp 'org-scribe--overlays-last-id)))

;;; Tooltip formatting

(ert-deftest test-overlays-format-tooltip-full-properties ()
  "format-tooltip returns name + all four character properties when all are set."
  (let ((temp-file (make-temp-file "test-overlays-char-" nil ".org"))
        heading-pos)
    (unwind-protect
        (progn
          (with-current-buffer (find-file-noselect temp-file)
            (erase-buffer)
            (insert (concat "* Alice\n"
                            ":PROPERTIES:\n"
                            ":ID: char-test-001\n"
                            ":Role: Protagonist\n"
                            ":Age: 28\n"
                            ":Goal: Find peace\n"
                            ":Motivation: Family\n"
                            ":END:\n\n"))
            (save-buffer)
            (setq heading-pos (point-min)))
          (cl-letf (((symbol-function 'org-id-find)
                     (lambda (_id) (cons temp-file heading-pos))))
            (let ((result (org-scribe--overlays-format-tooltip "char-test-001")))
              (should (stringp result))
              (should (string-match-p "Alice" result))
              (should (string-match-p "Role: Protagonist" result))
              (should (string-match-p "Age: 28" result))
              (should (string-match-p "Goal: Find peace" result))
              (should (string-match-p "Motivation: Family" result)))))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

(ert-deftest test-overlays-format-tooltip-partial-properties ()
  "format-tooltip includes only the properties that are set."
  (let ((temp-file (make-temp-file "test-overlays-partial-" nil ".org"))
        heading-pos)
    (unwind-protect
        (progn
          (with-current-buffer (find-file-noselect temp-file)
            (erase-buffer)
            (insert (concat "* Bob\n"
                            ":PROPERTIES:\n"
                            ":ID: char-test-002\n"
                            ":Role: Antagonist\n"
                            ":END:\n\n"))
            (save-buffer)
            (setq heading-pos (point-min)))
          (cl-letf (((symbol-function 'org-id-find)
                     (lambda (_id) (cons temp-file heading-pos))))
            (let ((result (org-scribe--overlays-format-tooltip "char-test-002")))
              (should (string-match-p "Bob" result))
              (should (string-match-p "Role: Antagonist" result))
              (should-not (string-match-p "Age:" result))
              (should-not (string-match-p "Goal:" result)))))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

(ert-deftest test-overlays-format-tooltip-name-only ()
  "format-tooltip returns just [Name] when no character properties are present."
  (let ((temp-file (make-temp-file "test-overlays-loc-" nil ".org"))
        heading-pos)
    (unwind-protect
        (progn
          (with-current-buffer (find-file-noselect temp-file)
            (erase-buffer)
            (insert (concat "* London\n"
                            ":PROPERTIES:\n"
                            ":ID: loc-test-001\n"
                            ":Type: City\n"
                            ":END:\n\n"))
            (save-buffer)
            (setq heading-pos (point-min)))
          (cl-letf (((symbol-function 'org-id-find)
                     (lambda (_id) (cons temp-file heading-pos))))
            (let ((result (org-scribe--overlays-format-tooltip "loc-test-001")))
              (should (stringp result))
              (should (string-match-p "London" result))
              (should-not (string-match-p "Role:" result))
              ;; Name-only format has no "|" separator
              (should-not (string-match-p " | " result)))))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

(ert-deftest test-overlays-format-tooltip-returns-nil-for-unknown-id ()
  "format-tooltip returns nil when org-id-find cannot locate the entity."
  (cl-letf (((symbol-function 'org-id-find) (lambda (_id) nil)))
    (should (null (org-scribe--overlays-format-tooltip "nonexistent-id")))))

;;; Minor-mode hook registration

(ert-deftest test-overlays-mode-adds-hook-on-enable ()
  "Enabling org-scribe-overlays-mode registers the post-command hook buffer-locally."
  (with-temp-buffer
    (org-mode)
    (org-scribe-overlays-mode 1)
    (should (memq 'org-scribe--overlays-post-command post-command-hook))
    (org-scribe-overlays-mode -1)))

(ert-deftest test-overlays-mode-removes-hook-on-disable ()
  "Disabling org-scribe-overlays-mode removes the post-command hook."
  (with-temp-buffer
    (org-mode)
    (org-scribe-overlays-mode 1)
    (org-scribe-overlays-mode -1)
    (should-not (memq 'org-scribe--overlays-post-command post-command-hook))))

(ert-deftest test-overlays-mode-resets-last-id-on-disable ()
  "Disabling the mode resets the last-id deduplication state."
  (with-temp-buffer
    (org-mode)
    (org-scribe-overlays-mode 1)
    (setq org-scribe--overlays-last-id "some-entity-id")
    (org-scribe-overlays-mode -1)
    (should (null org-scribe--overlays-last-id))))

;;; ID-at-point detection

(ert-deftest test-overlays-id-at-point-in-body-text ()
  "id-at-point returns the ID for a link in paragraph body text (element API path)."
  (with-temp-buffer
    (org-mode)
    (insert "See [[id:entity-body-001][Alice]] for details.\n")
    (goto-char (point-min))
    (forward-char 7)  ; inside the link
    (should (equal (org-scribe--overlays-id-at-point) "entity-body-001"))))

(ert-deftest test-overlays-id-at-point-in-property-value ()
  "id-at-point returns the ID for a link inside a property value (regex fallback).
This is the primary use-case for org-scribe: the PoV/Characters/Location
properties in scene headings all contain [[id:...]] links.  The element
API cannot detect them there because org-mode does not parse property values
for link objects — org-element-context returns \\='node-property, not \\='link."
  (with-temp-buffer
    (org-mode)
    (insert (concat "* Scene One\n"
                    ":PROPERTIES:\n"
                    ":PoV: [[id:char-pov-001][Alex]]\n"
                    ":END:\n\n"))
    (goto-char (point-min))
    (re-search-forward ":PoV:")
    (forward-char 5)  ; now inside [[id:char-pov-001][Alex]]
    (should (equal (org-scribe--overlays-id-at-point) "char-pov-001"))))

(ert-deftest test-overlays-id-at-point-multiple-links-in-property ()
  "id-at-point correctly identifies which ID the cursor is on among several."
  (with-temp-buffer
    (org-mode)
    (insert (concat "* Scene\n"
                    ":PROPERTIES:\n"
                    ":Characters: [[id:char-aaa][Alice]], [[id:char-bbb][Bob]]\n"
                    ":END:\n\n"))
    (goto-char (point-min))
    ;; Position on the second link (Bob)
    (re-search-forward "\\[\\[id:char-bbb\\]")
    (forward-char 2)
    (should (equal (org-scribe--overlays-id-at-point) "char-bbb"))))

(ert-deftest test-overlays-id-at-point-returns-nil-outside-link ()
  "id-at-point returns nil when point is on plain text."
  (with-temp-buffer
    (org-mode)
    (insert "Just some plain text here.\n")
    (goto-char (point-min))
    (should (null (org-scribe--overlays-id-at-point)))))

;;; Post-command hook behaviour

(ert-deftest test-overlays-post-command-sets-last-id-on-id-link ()
  "post-command updates last-id when point moves onto an [[id:...]] link."
  (cl-letf (((symbol-function 'org-scribe--overlays-format-tooltip)
             (lambda (_id) nil)))
    (with-temp-buffer
      (org-mode)
      (insert "[[id:test-entity-001][Alice]]\n")
      (goto-char (point-min))
      (forward-char 5)
      (org-scribe--overlays-post-command)
      (should (equal org-scribe--overlays-last-id "test-entity-001")))))

(ert-deftest test-overlays-post-command-skips-duplicate-id ()
  "post-command does not call format-tooltip again when the ID hasn't changed."
  (let ((calls 0))
    (cl-letf (((symbol-function 'org-scribe--overlays-format-tooltip)
               (lambda (_id) (cl-incf calls) "tooltip")))
      (with-temp-buffer
        (org-mode)
        (insert "[[id:test-entity-002][Bob]]\n")
        (goto-char (point-min))
        (forward-char 5)
        (org-scribe--overlays-post-command)   ; first: sets last-id, calls format-tooltip
        (should (= calls 1))
        (org-scribe--overlays-post-command)   ; second: same ID — no re-call
        (should (= calls 1))))))

(ert-deftest test-overlays-post-command-sets-last-id-in-property-drawer ()
  "post-command works for links inside property values, not only body text.
Regression: previously the element API returned \\='node-property instead of
\\='link for property values, so no tooltip was ever shown for scene properties."
  (cl-letf (((symbol-function 'org-scribe--overlays-format-tooltip)
             (lambda (_id) nil)))
    (with-temp-buffer
      (org-mode)
      (insert (concat "* Scene\n"
                      ":PROPERTIES:\n"
                      ":PoV: [[id:char-prop-test][Alex]]\n"
                      ":END:\n\n"))
      (goto-char (point-min))
      (re-search-forward ":PoV:")
      (forward-char 5)
      (org-scribe--overlays-post-command)
      (should (equal org-scribe--overlays-last-id "char-prop-test")))))

(ert-deftest test-overlays-post-command-clears-last-id-when-not-on-link ()
  "post-command resets last-id when point is not on an ID link."
  (with-temp-buffer
    (org-mode)
    (setq org-scribe--overlays-last-id "previous-id")
    (insert "Plain text with no links.\n")
    (goto-char (point-min))
    (org-scribe--overlays-post-command)
    (should (null org-scribe--overlays-last-id))))

;;; Auto-enable guard

(ert-deftest test-overlays-maybe-enable-activates-when-flag-set ()
  "maybe-enable turns on the mode when org-scribe-overlays-enable is non-nil."
  (with-temp-buffer
    (org-mode)
    (let ((org-scribe-overlays-enable t))
      (org-scribe--overlays-maybe-enable)
      (should org-scribe-overlays-mode))
    (org-scribe-overlays-mode -1)))

(ert-deftest test-overlays-maybe-enable-noop-when-flag-nil ()
  "maybe-enable does nothing when org-scribe-overlays-enable is nil."
  (with-temp-buffer
    (org-mode)
    (let ((org-scribe-overlays-enable nil))
      (org-scribe--overlays-maybe-enable)
      (should-not org-scribe-overlays-mode))))

(provide 'test-overlays)

;;; test-overlays.el ends here
