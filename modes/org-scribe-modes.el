;;; org-scribe-modes.el --- Writing environment modes for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Four mutually exclusive writing modes for different workflows:
;; - org-scribe/writing-env-mode: Distraction-free writing environment
;; - org-scribe/writing-env-mode-focus: Focus mode with narrowing to current section
;; - org-scribe/project-mode: Project navigation with treemacs and imenu-list
;; - org-scribe/editing-mode: Three-pane editing layout with notes

;;; Code:

(require 'org)
(require 'org-scribe-core)
(require 'org-scribe-config)

;; Declare external functions
(declare-function writeroom-mode "writeroom-mode")
(declare-function consult-theme "consult")
(declare-function fontaine-set-preset "fontaine")
(declare-function treemacs-add-and-display-current-project-exclusively "treemacs")
(declare-function treemacs-get-local-window "treemacs")
(declare-function imenu-list-smart-toggle "imenu-list")

;;; Mutual Exclusivity System

(defvar my-org-scribe-exclusive-modes
  '(org-scribe/writing-env-mode
    org-scribe/writing-env-mode-focus
    org-scribe/project-mode
    org-scribe/editing-mode)
  "List of writing minor modes that should be mutually exclusive.")

(defun my-org-scribe--deactivate-other-modes (current-mode)
  "Deactivate all writing modes except CURRENT-MODE.
This function is called when activating any of the mutually exclusive
writing modes to ensure only one is active at a time."
  (dolist (mode my-org-scribe-exclusive-modes)
    (unless (eq mode current-mode)
      (when (and (boundp mode) (symbol-value mode))
        (funcall mode -1)))))

;;; Writing Environment Mode (Base)

(defvar-local my-org-scribe-env--writeroom-active nil
  "Track if writeroom was activated by org-scribe-env mode.")

(defun my-org-scribe-env--activate ()
  "Activate writing environment with theme, font, and writeroom."
  (display-line-numbers-mode 1)
  ;; Check if consult-theme is available
  (if (fboundp 'consult-theme)
      (consult-theme my-org-scribe-env-work-theme)
    (load-theme my-org-scribe-env-work-theme t))
  ;; Check if fontaine is available
  (when (fboundp 'fontaine-set-preset)
    (fontaine-set-preset my-org-scribe-env-work-font))
  ;; Check if writeroom-mode is available
  (if (fboundp 'writeroom-mode)
      (progn
        (setq writeroom-width my-org-scribe-env-work-width)
        (writeroom-mode 1)
        (setq my-org-scribe-env--writeroom-active t))
    (user-error "writeroom-mode is required for writing environment modes")))

(defun my-org-scribe-env--deactivate ()
  "Deactivate writing environment and restore previous settings."
  (display-line-numbers-mode -1)
  ;; Restore theme
  (if (fboundp 'consult-theme)
      (consult-theme my-org-scribe-env-normal-theme)
    (load-theme my-org-scribe-env-normal-theme t))
  ;; Restore font
  (when (fboundp 'fontaine-set-preset)
    (fontaine-set-preset my-org-scribe-env-normal-font))
  ;; Deactivate writeroom if we activated it
  (when my-org-scribe-env--writeroom-active
    (writeroom-mode -1)
    (setq my-org-scribe-env--writeroom-active nil)))

;;;###autoload
(define-minor-mode org-scribe/writing-env-mode
  "Toggle a distraction-free writing environment.

This mode changes the theme, font, and enables writeroom-mode
with customized settings optimized for focused writing."
  :lighter " ✍"
  :global nil
  (if org-scribe/writing-env-mode
      (progn
        (my-org-scribe--deactivate-other-modes 'org-scribe/writing-env-mode)
        (my-org-scribe-env--activate))
    (my-org-scribe-env--deactivate)))

;;; Focus Writing Mode (with narrowing)

(defvar-local my-org-scribe-env--narrowed nil
  "Track if buffer was narrowed by org-scribe-env-mode-focus.")

(defun my-org-scribe-env-focus--activate ()
  "Activate writing environment and narrow to current org section."
  ;; First activate the base writing environment
  (my-org-scribe-env--activate)
  ;; Then add narrowing if in org-mode
  (when (derived-mode-p 'org-mode)
    (org-narrow-to-subtree)
    (setq my-org-scribe-env--narrowed t)))

(defun my-org-scribe-env-focus--deactivate ()
  "Deactivate writing environment and restore buffer view."
  ;; First widen if we narrowed
  (when my-org-scribe-env--narrowed
    (widen)
    (setq my-org-scribe-env--narrowed nil))
  ;; Then deactivate the base writing environment
  (my-org-scribe-env--deactivate))

;;;###autoload
(define-minor-mode org-scribe/writing-env-mode-focus
  "Toggle a distraction-free writing environment focused on current section.

This mode changes the theme, font, and enables writeroom-mode
with customized settings optimized for focused writing.  Additionally,
it narrows the buffer to the current org section at point."
  :lighter " ✍🔍"
  :global nil
  (if org-scribe/writing-env-mode-focus
      (progn
        (my-org-scribe--deactivate-other-modes 'org-scribe/writing-env-mode-focus)
        (my-org-scribe-env-focus--activate))
    (my-org-scribe-env-focus--deactivate)))

;;; Project Writing Mode (treemacs + imenu-list)

;;;###autoload
(define-minor-mode org-scribe/project-mode
  "Toggle treemacs and imenu-list together for a focused writing environment.

When enabled, opens treemacs with the current project exclusively
and activates imenu-list.  When disabled, closes both windows.
Focus always returns to the original buffer for seamless transitions."
  :lighter " ProjWrt"
  :global nil
  (let ((original-window (selected-window)))
    (if org-scribe/project-mode
        ;; Enable: Open both windows, then return focus
        (progn
          (my-org-scribe--deactivate-other-modes 'org-scribe/project-mode)
          ;; Check for treemacs
          (if (fboundp 'treemacs-add-and-display-current-project-exclusively)
              (treemacs-add-and-display-current-project-exclusively)
            (warn "treemacs package not available"))
          ;; Check for imenu-list
          (if (fboundp 'imenu-list-smart-toggle)
              (unless (get-buffer-window imenu-list-buffer-name)
                (imenu-list-smart-toggle))
            (warn "imenu-list package not available"))
          ;; Return focus to original window
          (when (window-live-p original-window)
            (select-window original-window)))
      ;; Disable: Close both windows, focus stays in current buffer
      (progn
        ;; Close treemacs
        (when (fboundp 'treemacs-get-local-window)
          (when-let ((treemacs-window (treemacs-get-local-window)))
            (delete-window treemacs-window)))
        ;; Close imenu-list
        (when (and (fboundp 'imenu-list-smart-toggle)
                   (get-buffer-window imenu-list-buffer-name))
          (imenu-list-smart-toggle))
        ;; Ensure focus is on original window
        (when (window-live-p original-window)
          (select-window original-window))))))

;;; Editing Mode (three-pane layout)

;; Helper functions for editing mode
(defun org-scribe/file-notes-filename (file)
  "Return the org-remark notes filename for FILE.
E.g., \"~/tmp/foo.org\" → \"foo-notes.org\"."
  (let* ((base (file-name-sans-extension (file-name-nondirectory file)))
         (ext  (file-name-extension (file-name-nondirectory file))))
    (concat base "-notes." ext)))

(defun org-scribe/resize-margins ()
  "Center the current buffer according to `visual-fill-column-width'.
If the desired column width exceeds the window width, do nothing
instead of passing a negative margin to `set-window-margins'."
  (when (and (boundp 'visual-fill-column-width)
             visual-fill-column-width
             (> (window-width) visual-fill-column-width))
    (let ((margin (/ (- (window-width) visual-fill-column-width) 2)))
      (set-window-margins (selected-window) margin margin))))

(defun org-scribe/editing-profile ()
  "Apply the visual style for editing sessions.
Applies theme, column width, and font preset."
  ;; Theme
  (when (fboundp 'consult-theme)
    (consult-theme org-scribe-editing-theme))
  ;; Column width for visual-fill-column
  (when (boundp 'visual-fill-column-width)
    (setq visual-fill-column-width org-scribe-editing-fill-column-width))
  ;; Font preset
  (when (fboundp 'fontaine-set-preset)
    (fontaine-set-preset org-scribe-editing-fontaine-preset)))

;; State-saving variables
(defvar-local org-scribe-editing--saved-config nil
  "Window configuration saved before `org-scribe-editing-mode' was enabled.")
(defvar-local org-scribe-editing--saved-theme nil
  "List of themes that were enabled before `org-scribe-editing-mode' was turned on.")
(defvar-local org-scribe-editing--saved-fill-column-width nil
  "Value of `visual-fill-column-width' before the mode was enabled.")
(defvar-local org-scribe-editing--saved-fontaine-preset nil
  "Current Fontaine preset before the mode was enabled.")

(defun org-scribe-editing--setup ()
  "Create the three-pane layout and apply the editing visual profile."
  (let* ((left-perc   org-scribe-editing-left-width-percent)
         (right-perc  org-scribe-editing-right-width-percent)
         (src-file
          (or (buffer-file-name)
              (user-error "Current buffer is not visiting a file; cannot enable `org-scribe-editing-mode'")))
         (notes-file (org-scribe/file-notes-filename src-file))
         (frame-w    (frame-width))
         (right-w    (org-scribe/window-perc right-perc))
         (left-w     (org-scribe/window-perc left-perc)))

    ;; Save visual state
    (setq org-scribe-editing--saved-config (current-window-configuration)
          org-scribe-editing--saved-theme
          (when (boundp 'custom-enabled-themes) custom-enabled-themes)
          org-scribe-editing--saved-fill-column-width
          (when (boundp 'visual-fill-column-width) visual-fill-column-width)
          org-scribe-editing--saved-fontaine-preset
          (when (boundp 'fontaine-current-preset) fontaine-current-preset))

    ;; Create layout
    (delete-other-windows)
    ;; Left panel: imenu-list
    (when (require 'imenu-list nil t)
      (setq imenu-list-size left-w
            imenu-list-focus-after-activation nil)
      (imenu-list-smart-toggle))
    ;; Center: current buffer
    (switch-to-buffer (current-buffer))
    ;; Right panel: notes file
    (let ((left-size (- (window-total-width) right-w)))
      (split-window-right left-size)
      (other-window 1)
      (find-file notes-file))
    (other-window -1)

    ;; Apply visual profile
    (org-scribe/editing-profile)

    ;; Resize margins if visual-fill-column is available
    (org-scribe/resize-margins)))

(defun org-scribe-editing--teardown ()
  "Restore the previous window configuration and visual settings."
  ;; Windows
  (when (window-configuration-p org-scribe-editing--saved-config)
    (set-window-configuration org-scribe-editing--saved-config))

  ;; Themes
  (when (and (boundp 'custom-enabled-themes) org-scribe-editing--saved-theme)
    (dolist (th custom-enabled-themes)
      (unless (member th org-scribe-editing--saved-theme)
        (disable-theme th)))
    (dolist (th org-scribe-editing--saved-theme)
      (unless (member th custom-enabled-themes)
        (load-theme th t))))

  ;; visual-fill-column-width
  (when (boundp 'visual-fill-column-width)
    (setq visual-fill-column-width org-scribe-editing--saved-fill-column-width))

  ;; Fontaine preset
  (when (and (boundp 'fontaine-current-preset)
             org-scribe-editing--saved-fontaine-preset)
    (fontaine-set-preset org-scribe-editing--saved-fontaine-preset))

  ;; Clean up saved vars
  (setq org-scribe-editing--saved-config nil
        org-scribe-editing--saved-theme nil
        org-scribe-editing--saved-fill-column-width nil
        org-scribe-editing--saved-fontaine-preset nil))

;;;###autoload
(define-minor-mode org-scribe/editing-mode
  "Minor mode that sets up a three-pane editing layout for the current file.

When enabled the current frame is split into:
  - left: `imenu-list' (configurable percentage)
  - centre: the buffer you are currently editing
  - right: a matching org-remark notes file (configurable percentage)

Disabling the mode restores the previous window configuration and the
visual settings (theme, column width, font preset)."
  :init-value nil
  :lighter " Edit"
  :global nil
  (if org-scribe/editing-mode
      (progn
        (my-org-scribe--deactivate-other-modes 'org-scribe/editing-mode)
        (org-scribe-editing--setup))
    (org-scribe-editing--teardown)))

;;; Cleanup on kill buffer

(defun my-org-scribe-env--cleanup ()
  "Emergency cleanup of writing environment state.
This is called on kill-buffer-hook to ensure state is cleaned up."
  (setq my-org-scribe-env--writeroom-active nil
        my-org-scribe-env--narrowed nil
        org-scribe-editing--saved-config nil))

(add-hook 'kill-buffer-hook #'my-org-scribe-env--cleanup)

(provide 'org-scribe-modes)

;;; org-scribe-modes.el ends here
