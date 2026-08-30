;;; org-scribe-modes.el --- Writing environment modes for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Four mutually exclusive writing layouts for different workflows:
;; - write    (org-scribe-writing-env-mode): Distraction-free writing environment
;; - focus    (org-scribe-writing-env-mode-focus): Focus mode with narrowing
;; - edit     (org-scribe-editing-mode): Three-pane editing layout with the
;;            project notes file in the right pane (see
;;            `org-scribe-editing-right-panel')
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

;; Forward declarations for imenu-list's own variables.  These are not
;; cosmetic: `imenu-list-position' is `let'-bound below, and in a
;; `lexical-binding' file the byte compiler binds a symbol lexically
;; unless it knows the symbol is special.  Compiled without imenu-list
;; loaded, that `let' becomes a lexical variable the library never reads
;; — the compiler says so outright ("Unused lexical variable") — and the
;; docking side is silently ignored.  A bare `defvar' marks the symbol
;; special without giving it a value, so the library's own definition
;; still wins.
(defvar imenu-list-buffer-name)
(defvar imenu-list-position)
(defvar imenu-list-size)
(defvar imenu-list-focus-after-activation)
;; Loaded after this module (see the load order in org-scribe.el), but
;; only called at runtime from `org-scribe--editing-right-panel-file'.
(declare-function org-scribe-capture-target-file "org-scribe-capture")
(declare-function org-scribe--find-existing-file "org-scribe-core")

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

(defvar-local org-scribe-env--saved-writeroom-width nil
  "Global value of `writeroom-width' saved before org-scribe-env overrode it.")

(defun org-scribe--apply-theme (theme)
  "Switch to THEME, degrading gracefully instead of erroring.

Does nothing when THEME is nil — the default for the writing-mode
themes, so a fresh install never touches the user's colours.  When
THEME names a theme that is not installed, says so and leaves the
current theme alone: a bare `load-theme' on a missing theme would
signal, turning the first press of the writing-mode key on a fresh
install into an error."
  (when theme
    (if (not (or (custom-theme-p theme)
                 (memq theme (custom-available-themes))))
        (message (org-scribe-msg 'msg-theme-unavailable (symbol-name theme)))
      (if (fboundp 'consult-theme)
          (consult-theme theme)
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme theme t)))))

(defun org-scribe-env--activate ()
  "Activate writing environment with theme, font, and writeroom."
  ;; Fail fast, before touching theme/font, if writeroom-mode is absent.
  (unless (fboundp 'writeroom-mode)
    (user-error (org-scribe-msg 'error-writeroom-required)))
  (display-line-numbers-mode 1)
  (org-scribe--apply-theme org-scribe-env-work-theme)
  ;; Check if fontaine is available
  (when (fboundp 'fontaine-set-preset)
    (fontaine-set-preset org-scribe-env-work-font))
  (setq org-scribe-env--saved-writeroom-width writeroom-width)
  (setq writeroom-width org-scribe-env-work-width)
  (writeroom-mode 1)
  (setq org-scribe-env--writeroom-active t))

(defun org-scribe-env--deactivate ()
  "Deactivate writing environment and restore previous settings."
  (display-line-numbers-mode -1)
  ;; Restore theme
  (org-scribe--apply-theme org-scribe-env-normal-theme)
  ;; Restore font
  (when (fboundp 'fontaine-set-preset)
    (fontaine-set-preset org-scribe-env-normal-font))
  ;; Deactivate writeroom if we activated it
  (when org-scribe-env--writeroom-active
    (writeroom-mode -1)
    (setq org-scribe-env--writeroom-active nil)
    (setq writeroom-width org-scribe-env--saved-writeroom-width)))

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
        (condition-case err
            (org-scribe-env--activate)
          (error
           (org-scribe-writing-env-mode -1)
           (signal (car err) (cdr err)))))
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
        (condition-case err
            (org-scribe-env-focus--activate)
          (error
           (org-scribe-writing-env-mode-focus -1)
           (signal (car err) (cdr err)))))
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
          ;; Check for imenu-list.  Load it rather than testing
          ;; `fboundp': `imenu-list-smart-toggle' is autoloaded, so it is
          ;; bound as soon as the package is *installed*, while
          ;; `imenu-list-buffer-name' is a plain `defconst' that does not
          ;; exist until the library is actually loaded.  Guarding on the
          ;; function and then reading the variable passed the check and
          ;; signalled `void-variable' on the next line, in a fresh
          ;; session where nothing else had pulled imenu-list in.
          (if (require 'imenu-list nil t)
              (unless (get-buffer-window imenu-list-buffer-name)
                ;; Treemacs already occupies the left edge, so dock
                ;; imenu-list on the right.  The package has defaulted
                ;; both ways across versions, so state it rather than
                ;; relying on the default.
                (let ((imenu-list-position 'right))
                  (imenu-list-smart-toggle)))
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
        ;; Close imenu-list.  Same trap as the enable branch, and it bit
        ;; harder here: with the mode stuck on after a failed enable, the
        ;; way out also signalled, so treemacs could not be dismissed.
        ;; `featurep' rather than `require' — if the library was never
        ;; loaded there is no imenu-list window to close, and loading it
        ;; just to decide that would be backwards.
        (when (and (featurep 'imenu-list)
                   (get-buffer-window imenu-list-buffer-name))
          (imenu-list-smart-toggle))
        ;; Ensure focus is on original window
        (when (window-live-p original-window)
          (select-window original-window))))))

;;; Editing Mode (three-pane layout)

;; Helper functions for editing mode
(defun org-scribe--editing-display-right-panel (src-file)
  "Display the editing-mode right pane for SRC-FILE in the current window.
Most values of `org-scribe-editing-right-panel' name a file; `edits'
instead names a generated buffer, so it cannot go through `find-file'."
  (if (eq org-scribe-editing-right-panel 'edits)
      (switch-to-buffer
       (org-scribe--edits-build
        (or (org-scribe-project-root)
            (file-name-directory src-file))))
    (find-file (org-scribe--editing-right-panel-file src-file))))

(defun org-scribe--editing-right-panel-file (src-file)
  "Return the file to show in the editing-mode right pane for SRC-FILE.
Dispatches on `org-scribe-editing-right-panel'; see that variable for
the accepted values.  Falls back to the project notes file when a
requested file cannot be located, so the pane always has something to
display."
  (let ((root (org-scribe-project-root)))
    (or (pcase org-scribe-editing-right-panel
          ('notes (org-scribe-capture-target-file t))
          ;; `edits' is a buffer, not a file; it is handled by
          ;; `org-scribe--editing-display-right-panel' before reaching
          ;; here.  Fall through to the notes file for any caller that
          ;; asks this function for a file name regardless.
          ('edits (org-scribe-capture-target-file t))
          ('revision (and root (org-scribe--find-existing-file root "revision.org")))
          ((and (pred stringp) path) (and root (expand-file-name path root)))
          ((and (pred functionp) fn) (funcall fn src-file)))
        ;; Unknown value, or a file that could not be located: fall back
        ;; to the notes file rather than leaving the pane empty.
        (org-scribe-capture-target-file t))))

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
  (org-scribe--apply-theme org-scribe-editing-theme)
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
      (org-scribe--editing-display-right-panel src-file))
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
  - right: the project notes file (configurable percentage); see
    `org-scribe-editing-right-panel' to show something else there

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
