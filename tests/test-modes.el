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
;;; Function Availability
;;; ─────────────────────────────────────────────

(ert-deftest test-modes-public-functions-defined ()
  "Test that all four public writing-mode toggles are defined."
  (should (fboundp 'org-scribe-writing-env-mode))
  (should (fboundp 'org-scribe-writing-env-mode-focus))
  (should (fboundp 'org-scribe-project-mode))
  (should (fboundp 'org-scribe-editing-mode)))

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
  (should (fboundp 'org-scribe--editing-right-panel-file))
  (should (fboundp 'org-scribe-editing-profile))
  (should (fboundp 'org-scribe-resize-margins)))

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
  (should (memq 'org-scribe-writing-env-mode      org-scribe-exclusive-modes))
  (should (memq 'org-scribe-writing-env-mode-focus org-scribe-exclusive-modes))
  (should (memq 'org-scribe-project-mode           org-scribe-exclusive-modes))
  (should (memq 'org-scribe-editing-mode           org-scribe-exclusive-modes)))

;;; ─────────────────────────────────────────────
;;; Mode Conflict — Mutual Exclusivity
;;; ─────────────────────────────────────────────

(ert-deftest test-modes-deactivate-other-calls-active-modes-with-minus-one ()
  "Test that deactivate-other-modes passes -1 to all active modes except the current one."
  (let ((calls nil))
    (cl-letf (((symbol-value 'org-scribe-writing-env-mode) t)
              ((symbol-value 'org-scribe-writing-env-mode-focus) t)
              ((symbol-value 'org-scribe-project-mode) t)
              ((symbol-value 'org-scribe-editing-mode) t)
              ((symbol-function 'org-scribe-writing-env-mode)
               (lambda (n) (push (cons 'writing-env n) calls)))
              ((symbol-function 'org-scribe-writing-env-mode-focus)
               (lambda (n) (push (cons 'focus n) calls)))
              ((symbol-function 'org-scribe-project-mode)
               (lambda (n) (push (cons 'project n) calls)))
              ((symbol-function 'org-scribe-editing-mode)
               (lambda (n) (push (cons 'editing n) calls))))
      ;; Keep writing-env-mode, deactivate the rest
      (org-scribe--deactivate-other-modes 'org-scribe-writing-env-mode)
      ;; Current mode must NOT be in the call list
      (should-not (assq 'writing-env calls))
      ;; All others must have been called with -1
      (should (equal (cdr (assq 'focus   calls)) -1))
      (should (equal (cdr (assq 'project calls)) -1))
      (should (equal (cdr (assq 'editing calls)) -1)))))

(ert-deftest test-modes-deactivate-other-skips-already-inactive-modes ()
  "Test that deactivate-other-modes does not call modes that are already off."
  (let ((calls nil))
    (cl-letf (((symbol-value 'org-scribe-writing-env-mode) nil)      ; already off
              ((symbol-value 'org-scribe-writing-env-mode-focus) nil) ; already off
              ((symbol-value 'org-scribe-project-mode) t)
              ((symbol-value 'org-scribe-editing-mode) t)
              ((symbol-function 'org-scribe-writing-env-mode)
               (lambda (n) (push 'writing-env calls)))
              ((symbol-function 'org-scribe-writing-env-mode-focus)
               (lambda (n) (push 'focus calls)))
              ((symbol-function 'org-scribe-project-mode)
               (lambda (n) (push 'project calls)))
              ((symbol-function 'org-scribe-editing-mode)
               (lambda (n) (push 'editing calls))))
      ;; Keep editing-mode; only project-mode should be deactivated
      (org-scribe--deactivate-other-modes 'org-scribe-editing-mode)
      (should-not (memq 'editing   calls))  ; current — not touched
      (should-not (memq 'writing-env calls)) ; was nil — not touched
      (should-not (memq 'focus     calls))   ; was nil — not touched
      (should     (memq 'project   calls))))) ; was t — should be called

;;; ─────────────────────────────────────────────
;;; Utility: editing right-panel resolution
;;; ─────────────────────────────────────────────
;;
;; The pane used to show a per-manuscript companion file
;; ("novel.org" -> "novel-notes.org") that existed only as an org-remark
;; annotation sink.  org-remark support was removed, so the pane now
;; resolves through `org-scribe-editing-right-panel'.

(defmacro org-scribe-test--with-project (dir-var &rest body)
  "Run BODY in a temporary novel project rooted at DIR-VAR."
  (declare (indent 1))
  `(let ((,dir-var (make-temp-file "org-scribe-panel-" t)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".org-scribe-project" ,dir-var)
             (insert "Type: novel\n"))
           (make-directory (expand-file-name "notes" ,dir-var) t)
           (with-temp-file (expand-file-name "notes/notes.org" ,dir-var)
             (insert "#+TITLE: Notes\n"))
           (let ((default-directory ,dir-var))
             ,@body))
       (delete-directory ,dir-var t))))

(ert-deftest test-modes-right-panel-defaults-to-notes ()
  "The default `notes' value resolves to the project notes file."
  (org-scribe-test--with-project dir
    (let ((org-scribe-editing-right-panel 'notes))
      (should (string= (expand-file-name "notes/notes.org" dir)
                       (org-scribe--editing-right-panel-file
                        (expand-file-name "novel.org" dir)))))))

(ert-deftest test-modes-right-panel-string-is-project-relative ()
  "A string value is expanded relative to the project root."
  (org-scribe-test--with-project dir
    (let ((org-scribe-editing-right-panel "notes/research.org"))
      (should (string= (expand-file-name "notes/research.org" dir)
                       (org-scribe--editing-right-panel-file
                        (expand-file-name "novel.org" dir)))))))

(ert-deftest test-modes-right-panel-function-receives-manuscript ()
  "A function value is called with the manuscript file name."
  (org-scribe-test--with-project dir
    (let* ((seen nil)
           (org-scribe-editing-right-panel
            (lambda (src) (setq seen src) "/tmp/custom-panel.org")))
      (should (string= "/tmp/custom-panel.org"
                       (org-scribe--editing-right-panel-file
                        (expand-file-name "novel.org" dir))))
      (should (string= (expand-file-name "novel.org" dir) seen)))))

(ert-deftest test-modes-right-panel-revision-falls-back-when-absent ()
  "`revision' falls back to the notes file when no revision.org exists."
  (org-scribe-test--with-project dir
    (let ((org-scribe-editing-right-panel 'revision))
      (should (string= (expand-file-name "notes/notes.org" dir)
                       (org-scribe--editing-right-panel-file
                        (expand-file-name "novel.org" dir)))))))

(ert-deftest test-modes-right-panel-revision-found-when-present ()
  "`revision' resolves to revision.org when the project has one."
  (org-scribe-test--with-project dir
    (with-temp-file (expand-file-name "revision.org" dir)
      (insert "#+TITLE: Revision\n"))
    (let ((org-scribe-editing-right-panel 'revision))
      (should (string= (expand-file-name "revision.org" dir)
                       (org-scribe--editing-right-panel-file
                        (expand-file-name "novel.org" dir)))))))

(ert-deftest test-modes-right-panel-unknown-value-falls-back ()
  "An unrecognised value falls back to the notes file, never nil."
  (org-scribe-test--with-project dir
    (let ((org-scribe-editing-right-panel 'nonsense))
      (should (string= (expand-file-name "notes/notes.org" dir)
                       (org-scribe--editing-right-panel-file
                        (expand-file-name "novel.org" dir)))))))

;;; ─────────────────────────────────────────────
;;; Safe theme application
;;; ─────────────────────────────────────────────

(ert-deftest test-modes-theme-defaults-are-nil ()
  "Test that the writing-mode themes default to nil.
A non-nil default means the *first* press of the writing-mode key on a
fresh install tries to load a theme the user may not have."
  (should (null (default-value 'org-scribe-env-work-theme)))
  (should (null (default-value 'org-scribe-env-normal-theme))))

(ert-deftest test-modes-apply-theme-nil-is-a-no-op ()
  "Test that a nil theme touches neither `load-theme' nor `consult-theme'."
  (let (called)
    (cl-letf (((symbol-function 'load-theme)
               (lambda (&rest _) (setq called 'load-theme)))
              ((symbol-function 'consult-theme)
               (lambda (&rest _) (setq called 'consult-theme)))
              ((symbol-function 'disable-theme) #'ignore))
      (org-scribe--apply-theme nil))
    (should-not called)))

(ert-deftest test-modes-apply-theme-missing-theme-messages-instead-of-erroring ()
  "Test that a theme that is not installed degrades to a message.
This is the fresh-install failure BAD-7 describes: `load-theme' on an
absent theme signals, so the package's own writing mode errored out."
  (let (called (msg nil))
    (cl-letf (((symbol-function 'load-theme)
               (lambda (&rest _) (setq called 'load-theme)))
              ((symbol-function 'disable-theme) #'ignore)
              ((symbol-function 'custom-available-themes) (lambda () nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      ;; `consult-theme' may or may not be defined in the test environment;
      ;; the guard must fire before either branch is reached.
      (cl-letf (((symbol-function 'consult-theme)
                 (lambda (&rest _) (setq called 'consult-theme))))
        (org-scribe--apply-theme 'org-scribe-no-such-theme)))
    (should-not called)
    (should (string-match-p "org-scribe-no-such-theme" msg))))

(ert-deftest test-modes-apply-theme-installed-theme-is-loaded ()
  "Test that an available theme is still applied."
  (let (loaded)
    (cl-letf (((symbol-function 'load-theme)
               (lambda (theme &rest _) (setq loaded theme)))
              ((symbol-function 'disable-theme) #'ignore)
              ((symbol-function 'custom-available-themes) (lambda () '(leuven)))
              ((symbol-function 'consult-theme) nil))
      (org-scribe--apply-theme 'leuven))
    (should (eq loaded 'leuven))))

;;; ─────────────────────────────────────────────
;;; Writing Environment: absence of writeroom
;;; ─────────────────────────────────────────────

(ert-deftest test-modes-writeroom-required-signals-user-error ()
  "Test that activate signals user-error when writeroom-mode is not installed.
Writeroom absence is simulated by emptying its function cell, which is
what `org-scribe-env--activate' checks with `fboundp'.  This used to be
a `skip-unless' on writeroom being absent, which meant the test never
ran anywhere writeroom is installed — that is, anywhere the rest of the
suite runs."
  (with-temp-buffer
    (cl-letf (((symbol-function 'writeroom-mode) nil)
              ;; Not reached while the guard holds (it fires before any of
              ;; this), but stubbed so a future reordering fails on the
              ;; missing user-error rather than on a live theme change.
              ((symbol-function 'display-line-numbers-mode) #'ignore)
              ((symbol-function 'load-theme) #'ignore))
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
  "Test that teardown restores a previously saved window configuration.
The restore call is captured rather than performed, so this runs in
batch: it used to be `skip-unless' on `display-graphic-p', which meant
it never ran in the batch suite at all.  Capturing also lets us assert
the stronger fact — that teardown passed the *saved* configuration to
`set-window-configuration' — instead of only that the variable was
cleared afterwards."
  (let ((saved-config (current-window-configuration))
        (restored nil))
    (with-temp-buffer
      (setq org-scribe-editing--saved-config saved-config
            org-scribe-editing--saved-theme nil
            org-scribe-editing--saved-fill-column-width nil
            org-scribe-editing--saved-fontaine-preset nil)
      (cl-letf (((symbol-function 'set-window-configuration)
                 (lambda (config &rest _) (setq restored config))))
        (org-scribe-editing--teardown))
      (should (eq restored saved-config))
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
