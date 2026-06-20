;;; org-scribe-modes.el --- Writing environment modes for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Four mutually exclusive writing layouts for different workflows:
;; - write    (org-scribe-writing-env-mode): Distraction-free writing environment
;; - focus    (org-scribe-writing-env-mode-focus): Focus mode with narrowing
;; - edit     (org-scribe-editing-mode): Three-pane editing layout with notes
;; - navigate (org-scribe-project-mode): Project navigation (treemacs + imenu-list)
;;
;; `org-scribe-workspace' is the single unified entry point: it switches
;; between the named layouts (with completion and toggle-off), driving the
;; minor modes below.  The layouts are listed once in
;; `org-scribe-workspace-layouts', from which the mutual-exclusivity list
;; and the dispatcher are derived.

;;; Code:

(require 'org)
(require 'seq)
(require 'org-scribe-core)
(require 'org-scribe-config)
(require 'org-scribe-messages)

;; Declare external functions
(declare-function writeroom-mode "writeroom-mode")
(declare-function consult-theme "consult")
(declare-function fontaine-set-preset "fontaine")
(declare-function treemacs-add-and-display-current-project-exclusively "treemacs")
(declare-function treemacs-get-local-window "treemacs")
(declare-function imenu-list-smart-toggle "imenu-list")

;;; Workspace Layout Table (single source of truth)

(defvar org-scribe-workspace-layouts
  '((write    . org-scribe-writing-env-mode)
    (focus    . org-scribe-writing-env-mode-focus)
    (edit     . org-scribe-editing-mode)
    (navigate . org-scribe-project-mode))
  "Alist mapping a workspace layout name to the minor mode implementing it.
This is the single source of truth for the writing environment: both
`org-scribe-exclusive-modes' (mutual exclusivity) and the unified
`org-scribe-workspace' command are derived from it.  Adding a layout here
wires it into both with no other changes.")

;;; Mutual Exclusivity System

(defvar org-scribe-exclusive-modes
  (mapcar #'cdr org-scribe-workspace-layouts)
  "List of writing minor modes that should be mutually exclusive.
Derived from `org-scribe-workspace-layouts'.")

(defun org-scribe--deactivate-other-modes (current-mode)
  "Deactivate all writing modes except CURRENT-MODE.
This function is called when activating any of the mutually exclusive
writing modes to ensure only one is active at a time."
  (dolist (mode org-scribe-exclusive-modes)
    (unless (eq mode current-mode)
      (when (and (boundp mode) (symbol-value mode))
        (funcall mode -1)))))

;;; Unified Workspace Command

(defun org-scribe--workspace-mode (layout)
  "Return the minor-mode symbol implementing workspace LAYOUT, or nil."
  (cdr (assq layout org-scribe-workspace-layouts)))

(defun org-scribe-workspace-current ()
  "Return the name of the active workspace layout, or nil if none is active."
  (car (seq-find (lambda (cell)
                   (let ((mode (cdr cell)))
                     (and (boundp mode) (symbol-value mode))))
                 org-scribe-workspace-layouts)))

;;;###autoload
(defun org-scribe-workspace (&optional layout)
  "Switch the writing workspace to LAYOUT.

LAYOUT is one of the names in `org-scribe-workspace-layouts' (by default
`write', `focus', `edit', `navigate'), or `normal' / nil to turn the
active layout off and return to the ordinary editing view.

Interactively, prompt for the layout with completion, offering the
currently active one (if any) as the default.  Choosing the layout that
is already active turns it off.

This single command is the recommended entry point; it replaces having to
remember four separate toggles.  The underlying minor modes
\(`org-scribe-writing-env-mode' and friends) remain available and are what
this command drives."
  (interactive
   (list (let* ((current (org-scribe-workspace-current))
                (names (append (mapcar (lambda (cell) (symbol-name (car cell)))
                                       org-scribe-workspace-layouts)
                               '("normal"))))
           (intern (completing-read
                    (org-scribe-msg 'prompt-select-workspace)
                    names nil t nil nil
                    (when current (symbol-name current)))))))
  (let ((current (org-scribe-workspace-current))
        (target  (unless (memq layout '(normal nil)) layout)))
    (cond
     ;; Turn the active layout (if any) off.
     ((null target)
      (when current (funcall (org-scribe--workspace-mode current) -1))
      (message (org-scribe-msg 'msg-workspace-normal)))
     ;; Reject unknown layout names.
     ((not (org-scribe--workspace-mode target))
      (user-error (org-scribe-msg 'error-unknown-workspace target)))
     ;; Selecting the active layout toggles it off.
     ((eq target current)
      (funcall (org-scribe--workspace-mode target) -1)
      (message (org-scribe-msg 'msg-workspace-normal)))
     ;; Switch to the requested layout.  Activating it deactivates any
     ;; other layout via `org-scribe--deactivate-other-modes'.
     (t
      (funcall (org-scribe--workspace-mode target) 1)
      (message (org-scribe-msg 'msg-workspace-set (symbol-name target)))))))

;;; Writing Environment Mode (Base)

(defvar-local org-scribe-env--writeroom-active nil
  "Track if writeroom was activated by org-scribe-env mode.")

(defun org-scribe-env--activate ()
  "Activate writing environment with theme, font, and writeroom."
  (display-line-numbers-mode 1)
  ;; Check if consult-theme is available
  (if (fboundp 'consult-theme)
      (consult-theme org-scribe-env-work-theme)
    (load-theme org-scribe-env-work-theme t))
  ;; Check if fontaine is available
  (when (fboundp 'fontaine-set-preset)
    (fontaine-set-preset org-scribe-env-work-font))
  ;; Check if writeroom-mode is available
  (if (fboundp 'writeroom-mode)
      (progn
        (setq writeroom-width org-scribe-env-work-width)
        (writeroom-mode 1)
        (setq org-scribe-env--writeroom-active t))
    (user-error (org-scribe-msg 'error-writeroom-required))))

(defun org-scribe-env--deactivate ()
  "Deactivate writing environment and restore previous settings."
  (display-line-numbers-mode -1)
  ;; Restore theme
  (if (fboundp 'consult-theme)
      (consult-theme org-scribe-env-normal-theme)
    (load-theme org-scribe-env-normal-theme t))
  ;; Restore font
  (when (fboundp 'fontaine-set-preset)
    (fontaine-set-preset org-scribe-env-normal-font))
  ;; Deactivate writeroom if we activated it
  (when org-scribe-env--writeroom-active
    (writeroom-mode -1)
    (setq org-scribe-env--writeroom-active nil)))

;;;###autoload
(define-minor-mode org-scribe-writing-env-mode
  "Toggle a distraction-free writing environment.

This mode changes the theme, font, and enables writeroom-mode
with customized settings optimized for focused writing."
  :lighter " ✍"
  :global nil
  (if org-scribe-writing-env-mode
      (progn
        (org-scribe--deactivate-other-modes 'org-scribe-writing-env-mode)
        (org-scribe-env--activate))
    (org-scribe-env--deactivate)))

;;; Focus Writing Mode (with narrowing)

(defvar-local org-scribe-env--narrowed nil
  "Track if buffer was narrowed by org-scribe-env-mode-focus.")

(defun org-scribe-env-focus--activate ()
  "Activate writing environment and narrow to current org section."
  ;; First activate the base writing environment
  (org-scribe-env--activate)
  ;; Then add narrowing if in org-mode
  (when (derived-mode-p 'org-mode)
    (org-narrow-to-subtree)
    (setq org-scribe-env--narrowed t)))

(defun org-scribe-env-focus--deactivate ()
  "Deactivate writing environment and restore buffer view."
  ;; First widen if we narrowed
  (when org-scribe-env--narrowed
    (widen)
    (setq org-scribe-env--narrowed nil))
  ;; Then deactivate the base writing environment
  (org-scribe-env--deactivate))

;;;###autoload
(define-minor-mode org-scribe-writing-env-mode-focus
  "Toggle a distraction-free writing environment focused on current section.

This mode changes the theme, font, and enables writeroom-mode
with customized settings optimized for focused writing.  Additionally,
it narrows the buffer to the current org section at point."
  :lighter " ✍🔍"
  :global nil
  (if org-scribe-writing-env-mode-focus
      (progn
        (org-scribe--deactivate-other-modes 'org-scribe-writing-env-mode-focus)
        (org-scribe-env-focus--activate))
    (org-scribe-env-focus--deactivate)))

;;; Project Writing Mode (treemacs + imenu-list)

;;;###autoload
(define-minor-mode org-scribe-project-mode
  "Toggle treemacs and imenu-list together for a focused writing environment.

When enabled, opens treemacs with the current project exclusively
and activates imenu-list.  When disabled, closes both windows.
Focus always returns to the original buffer for seamless transitions."
  :lighter " ProjWrt"
  :global nil
  (let ((original-window (selected-window)))
    (if org-scribe-project-mode
        ;; Enable: Open both windows, then return focus
        (progn
          (org-scribe--deactivate-other-modes 'org-scribe-project-mode)
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
(defun org-scribe-file-notes-filename (file)
  "Return the org-remark notes filename for FILE.
E.g., \"~/tmp/foo.org\" → \"foo-notes.org\"."
  (let* ((base (file-name-sans-extension (file-name-nondirectory file)))
         (ext  (file-name-extension (file-name-nondirectory file))))
    (concat base "-notes." ext)))

(defun org-scribe-resize-margins ()
  "Center the current buffer according to `visual-fill-column-width'.
If the desired column width exceeds the window width, do nothing
instead of passing a negative margin to `set-window-margins'."
  (when (and (boundp 'visual-fill-column-width)
             visual-fill-column-width
             (> (window-width) visual-fill-column-width))
    (let ((margin (/ (- (window-width) visual-fill-column-width) 2)))
      (set-window-margins (selected-window) margin margin))))

(defun org-scribe-editing-profile ()
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
              (user-error (org-scribe-msg 'error-no-org-file))))
         (notes-file (org-scribe-file-notes-filename src-file))
         (frame-w    (frame-width))
         (right-w    (org-scribe-window-perc right-perc))
         (left-w     (org-scribe-window-perc left-perc)))

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
    (org-scribe-editing-profile)

    ;; Resize margins if visual-fill-column is available
    (org-scribe-resize-margins)))

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
(define-minor-mode org-scribe-editing-mode
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
  (if org-scribe-editing-mode
      (progn
        (org-scribe--deactivate-other-modes 'org-scribe-editing-mode)
        (org-scribe-editing--setup))
    (org-scribe-editing--teardown)))

;;; Cleanup on kill buffer

(defun org-scribe-env--cleanup ()
  "Emergency cleanup of writing environment state.
This is called on kill-buffer-hook to ensure state is cleaned up."
  (setq org-scribe-env--writeroom-active nil
        org-scribe-env--narrowed nil
        org-scribe-editing--saved-config nil))

(add-hook 'kill-buffer-hook #'org-scribe-env--cleanup)

(provide 'org-scribe-modes)

;;; org-scribe-modes.el ends here
