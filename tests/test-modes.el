;;; test-modes.el --- Tests for writing environment modes -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for org-scribe-modes.el covering:
;; - Function and variable availability
;; - State save/restore round-trips (editing mode)
;; - Mode conflict / mutual exclusivity handling
;; - Focus mode narrowing round-trip
;; - Graceful absence of optional packages (writeroom, etc.)

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

;;; Load paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../modes" default-directory)))

(require 'org-scribe-core)
(require 'org-scribe-config)
(require 'org-scribe-messages)
(require 'org-scribe-modes)

;;; ─────────────────────────────────────────────
;;; Module Loading
;;; ─────────────────────────────────────────────

(ert-deftest test-modes-module-loads ()
  "Test that org-scribe-modes loads without errors."
  (should (featurep 'org-scribe-modes)))

;;; ─────────────────────────────────────────────
;;; Function Availability
;;; ─────────────────────────────────────────────

(ert-deftest test-modes-public-functions-defined ()
  "Test that all four public writing-mode toggles are defined."
  (should (fboundp 'org-scribe/writing-env-mode))
  (should (fboundp 'org-scribe/writing-env-mode-focus))
  (should (fboundp 'org-scribe/project-mode))
  (should (fboundp 'org-scribe/editing-mode)))

(ert-deftest test-modes-internal-functions-defined ()
  "Test that internal activate/deactivate helpers are defined."
  (should (fboundp 'org-scribe--deactivate-other-modes))
  (should (fboundp 'org-scribe-env--activate))
  (should (fboundp 'org-scribe-env--deactivate))
  (should (fboundp 'org-scribe-env-focus--activate))
  (should (fboundp 'org-scribe-env-focus--deactivate))
  (should (fboundp 'org-scribe-editing--setup))
  (should (fboundp 'org-scribe-editing--teardown))
  (should (fboundp 'org-scribe-env--cleanup))
  (should (fboundp 'org-scribe/file-notes-filename))
  (should (fboundp 'org-scribe/editing-profile))
  (should (fboundp 'org-scribe/resize-margins)))

;;; ─────────────────────────────────────────────
;;; Variable Availability
;;; ─────────────────────────────────────────────

(ert-deftest test-modes-customization-vars-defined ()
  "Test that all customization variables are defined."
  (should (boundp 'org-scribe-env-work-theme))
  (should (boundp 'org-scribe-env-normal-theme))
  (should (boundp 'org-scribe-env-work-font))
  (should (boundp 'org-scribe-env-normal-font))
  (should (boundp 'org-scribe-env-work-width))
  (should (boundp 'org-scribe-env-normal-width))
  (should (boundp 'org-scribe-editing-theme))
  (should (boundp 'org-scribe-editing-fill-column-width))
  (should (boundp 'org-scribe-editing-fontaine-preset))
  (should (boundp 'org-scribe-editing-left-width-percent))
  (should (boundp 'org-scribe-editing-right-width-percent)))

(ert-deftest test-modes-state-vars-are-buffer-local ()
  "Test that all mode state variables are declared buffer-local."
  ;; local-variable-if-set-p returns t for defvar-local variables
  (should (local-variable-if-set-p 'org-scribe-env--writeroom-active))
  (should (local-variable-if-set-p 'org-scribe-env--narrowed))
  (should (local-variable-if-set-p 'org-scribe-editing--saved-config))
  (should (local-variable-if-set-p 'org-scribe-editing--saved-theme))
  (should (local-variable-if-set-p 'org-scribe-editing--saved-fill-column-width))
  (should (local-variable-if-set-p 'org-scribe-editing--saved-fontaine-preset)))

(ert-deftest test-modes-state-vars-isolated-between-buffers ()
  "Test that mode state variables do not bleed between buffers."
  (let ((buf-a (generate-new-buffer " *test-modes-a*"))
        (buf-b (generate-new-buffer " *test-modes-b*")))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (setq org-scribe-env--writeroom-active t))
          (with-current-buffer buf-b
            (should-not org-scribe-env--writeroom-active)))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

;;; ─────────────────────────────────────────────
;;; Exclusive Modes List
;;; ─────────────────────────────────────────────

(ert-deftest test-modes-exclusive-list-is-defined ()
  "Test that the exclusive modes list is defined."
  (should (boundp 'org-scribe-exclusive-modes))
  (should (listp org-scribe-exclusive-modes))
  (should (= 4 (length org-scribe-exclusive-modes))))

(ert-deftest test-modes-exclusive-list-contains-all-four-modes ()
  "Test that all four writing modes appear in the exclusive list."
  (should (memq 'org-scribe/writing-env-mode      org-scribe-exclusive-modes))
  (should (memq 'org-scribe/writing-env-mode-focus org-scribe-exclusive-modes))
  (should (memq 'org-scribe/project-mode           org-scribe-exclusive-modes))
  (should (memq 'org-scribe/editing-mode           org-scribe-exclusive-modes)))

;;; ─────────────────────────────────────────────
;;; Mode Conflict — Mutual Exclusivity
;;; ─────────────────────────────────────────────

(ert-deftest test-modes-deactivate-other-calls-active-modes-with-minus-one ()
  "Test that deactivate-other-modes passes -1 to all active modes except the current one."
  (let ((calls nil))
    (cl-letf (((symbol-value 'org-scribe/writing-env-mode) t)
              ((symbol-value 'org-scribe/writing-env-mode-focus) t)
              ((symbol-value 'org-scribe/project-mode) t)
              ((symbol-value 'org-scribe/editing-mode) t)
              ((symbol-function 'org-scribe/writing-env-mode)
               (lambda (n) (push (cons 'writing-env n) calls)))
              ((symbol-function 'org-scribe/writing-env-mode-focus)
               (lambda (n) (push (cons 'focus n) calls)))
              ((symbol-function 'org-scribe/project-mode)
               (lambda (n) (push (cons 'project n) calls)))
              ((symbol-function 'org-scribe/editing-mode)
               (lambda (n) (push (cons 'editing n) calls))))
      ;; Keep writing-env-mode, deactivate the rest
      (org-scribe--deactivate-other-modes 'org-scribe/writing-env-mode)
      ;; Current mode must NOT be in the call list
      (should-not (assq 'writing-env calls))
      ;; All others must have been called with -1
      (should (equal (cdr (assq 'focus   calls)) -1))
      (should (equal (cdr (assq 'project calls)) -1))
      (should (equal (cdr (assq 'editing calls)) -1)))))

(ert-deftest test-modes-deactivate-other-skips-already-inactive-modes ()
  "Test that deactivate-other-modes does not call modes that are already off."
  (let ((calls nil))
    (cl-letf (((symbol-value 'org-scribe/writing-env-mode) nil)      ; already off
              ((symbol-value 'org-scribe/writing-env-mode-focus) nil) ; already off
              ((symbol-value 'org-scribe/project-mode) t)
              ((symbol-value 'org-scribe/editing-mode) t)
              ((symbol-function 'org-scribe/writing-env-mode)
               (lambda (n) (push 'writing-env calls)))
              ((symbol-function 'org-scribe/writing-env-mode-focus)
               (lambda (n) (push 'focus calls)))
              ((symbol-function 'org-scribe/project-mode)
               (lambda (n) (push 'project calls)))
              ((symbol-function 'org-scribe/editing-mode)
               (lambda (n) (push 'editing calls))))
      ;; Keep editing-mode; only project-mode should be deactivated
      (org-scribe--deactivate-other-modes 'org-scribe/editing-mode)
      (should-not (memq 'editing   calls))  ; current — not touched
      (should-not (memq 'writing-env calls)) ; was nil — not touched
      (should-not (memq 'focus     calls))   ; was nil — not touched
      (should     (memq 'project   calls))))) ; was t — should be called

;;; ─────────────────────────────────────────────
;;; Utility: file-notes-filename
;;; ─────────────────────────────────────────────

(ert-deftest test-modes-file-notes-filename-simple ()
  "Test file-notes-filename with a bare filename."
  (should (string= "novel-notes.org"
                   (org-scribe/file-notes-filename "novel.org"))))

(ert-deftest test-modes-file-notes-filename-strips-directory ()
  "Test that file-notes-filename strips the directory component."
  (should (string= "story-notes.org"
                   (org-scribe/file-notes-filename "/home/user/writing/story.org"))))

(ert-deftest test-modes-file-notes-filename-tilde-path ()
  "Test file-notes-filename with a ~ path."
  (should (string= "manuscript-notes.org"
                   (org-scribe/file-notes-filename "~/projects/novel/manuscript.org"))))

;;; ─────────────────────────────────────────────
;;; Writing Environment: absence of writeroom
;;; ─────────────────────────────────────────────

(ert-deftest test-modes-writeroom-required-signals-user-error ()
  "Test that activate signals user-error when writeroom-mode is not installed."
  ;; Only meaningful when writeroom is not installed in the test environment
  (skip-unless (not (fboundp 'writeroom-mode)))
  (with-temp-buffer
    (cl-letf (((symbol-function 'display-line-numbers-mode) #'ignore)
              ((symbol-function 'load-theme) #'ignore))
      ;; consult-theme and fontaine-set-preset are also absent in clean test env,
      ;; so the code naturally falls through to the writeroom guard.
      (should-error (org-scribe-env--activate) :type 'user-error))))

(ert-deftest test-modes-deactivate-clears-writeroom-flag ()
  "Test that deactivate clears org-scribe-env--writeroom-active."
  (with-temp-buffer
    (setq org-scribe-env--writeroom-active t)
    (cl-letf (((symbol-function 'display-line-numbers-mode) #'ignore)
              ((symbol-function 'load-theme) #'ignore)
              ;; Give writeroom-mode a no-op body so the when-block can run
              ((symbol-function 'writeroom-mode) #'ignore))
      (org-scribe-env--deactivate))
    (should-not org-scribe-env--writeroom-active)))

;;; ─────────────────────────────────────────────
;;; Focus Mode: narrowing round-trip
;;; ─────────────────────────────────────────────

(ert-deftest test-modes-focus-activate-narrows-buffer ()
  "Test that focus mode activate narrows the buffer to the current subtree."
  (with-temp-buffer
    (org-mode)
    (insert "* Chapter 1\n** Scene A\nContent here.\n** Scene B\nMore.\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)   ; point at "* Chapter 1"
    ;; Mock the base env so we don't need GUI / writeroom
    (cl-letf (((symbol-function 'org-scribe-env--activate) #'ignore))
      (org-scribe-env-focus--activate)
      (should org-scribe-env--narrowed)
      (should (buffer-narrowed-p)))))

(ert-deftest test-modes-focus-deactivate-widens-buffer ()
  "Test that focus mode deactivate widens the buffer and clears the flag."
  (with-temp-buffer
    (org-mode)
    (insert "* Chapter 1\n** Scene A\nContent.\n** Scene B\nMore.\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (cl-letf (((symbol-function 'org-scribe-env--activate) #'ignore)
              ((symbol-function 'org-scribe-env--deactivate) #'ignore))
      (org-scribe-env-focus--activate)
      (should (buffer-narrowed-p))
      (org-scribe-env-focus--deactivate)
      (should-not org-scribe-env--narrowed)
      (should-not (buffer-narrowed-p)))))

(ert-deftest test-modes-focus-not-narrowed-outside-org-mode ()
  "Test that focus activate does not narrow in a non-org-mode buffer."
  (with-temp-buffer
    ;; fundamental-mode — derived-mode-p 'org-mode is nil
    (cl-letf (((symbol-function 'org-scribe-env--activate) #'ignore))
      (org-scribe-env-focus--activate)
      ;; Flag must remain nil because derived-mode-p check fails
      (should-not org-scribe-env--narrowed)
      (should-not (buffer-narrowed-p)))))

;;; ─────────────────────────────────────────────
;;; Editing Mode: state save/restore
;;; ─────────────────────────────────────────────

(ert-deftest test-modes-editing-teardown-clears-saved-state ()
  "Test that editing mode teardown always clears all saved state variables."
  (with-temp-buffer
    ;; Simulate saved state (no real window config — teardown skips nil configs)
    (setq org-scribe-editing--saved-config           nil
          org-scribe-editing--saved-theme            nil
          org-scribe-editing--saved-fill-column-width 80
          org-scribe-editing--saved-fontaine-preset  'regular)
    ;; Teardown with nil config/theme skips GUI calls; fontaine/fill skipped
    ;; because fontaine-current-preset / visual-fill-column-width are unbound
    ;; in the test environment.
    (org-scribe-editing--teardown)
    (should (null org-scribe-editing--saved-config))
    (should (null org-scribe-editing--saved-theme))
    (should (null org-scribe-editing--saved-fill-column-width))
    (should (null org-scribe-editing--saved-fontaine-preset))))

(ert-deftest test-modes-editing-teardown-restores-window-config ()
  "Test that teardown restores a previously saved window configuration."
  (skip-unless (display-graphic-p))  ; window configs require a display
  (let ((saved-config (current-window-configuration)))
    (with-temp-buffer
      (setq org-scribe-editing--saved-config saved-config
            org-scribe-editing--saved-theme nil
            org-scribe-editing--saved-fill-column-width nil
            org-scribe-editing--saved-fontaine-preset nil)
      (org-scribe-editing--teardown)
      ;; After teardown the saved config is cleared
      (should (null org-scribe-editing--saved-config)))))

;;; ─────────────────────────────────────────────
;;; Kill-buffer cleanup hook
;;; ─────────────────────────────────────────────

(ert-deftest test-modes-kill-buffer-cleanup-clears-state ()
  "Test that the kill-buffer hook clears all mode state in the dying buffer."
  (let ((buf (generate-new-buffer " *test-modes-cleanup*")))
    (unwind-protect
        (with-current-buffer buf
          (setq org-scribe-env--writeroom-active    t
                org-scribe-env--narrowed            t
                org-scribe-editing--saved-config    'fake-config)
          (org-scribe-env--cleanup)
          (should-not org-scribe-env--writeroom-active)
          (should-not org-scribe-env--narrowed)
          (should-not org-scribe-editing--saved-config))
      (kill-buffer buf))))

;;; ─────────────────────────────────────────────
;;; Test runner
;;; ─────────────────────────────────────────────

(defun org-scribe-modes-run-tests ()
  "Run all writing-modes tests interactively."
  (interactive)
  (ert "^test-modes-"))

(provide 'test-modes)

;;; test-modes.el ends here
