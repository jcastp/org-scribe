;;; org-scribe-overlays.el --- Entity tooltips for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides `org-scribe-overlays-mode', a buffer-local minor mode that displays
;; entity information when point moves onto an [[id:...]] link in a scene
;; property or in body text.
;;
;; For characters the tooltip shows Role, Age, Occupation, Goal, Motivation,
;; and Conflict.  For other entity types it shows the heading name only.
;;
;; Three display styles are available via `org-scribe-overlays-display':
;;
;;   inline   — styled text appended after the link in the buffer (default)
;;   posframe — floating child frame near point (requires posframe package)
;;   echo     — echo area (original behaviour)
;;
;; This feature is disabled by default.  To enable it automatically whenever
;; `org-scribe-mode' is active, add the following to your init file:
;;
;;   (setq org-scribe-overlays-enable t)
;;
;; You can also toggle it manually at any time with:
;;
;;   M-x org-scribe-overlays-mode

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-scribe-messages)

;;; Face

(defface org-scribe-overlays-face
  '((t :inherit shadow :slant italic))
  "Face used for inline entity tooltip overlays."
  :group 'org-scribe)

;;; Internal state

(defvar-local org-scribe--overlays-last-id nil
  "ID of the last entity whose tooltip was shown, used to avoid redundant lookups.")

(defvar-local org-scribe--overlays-overlay nil
  "Active inline tooltip overlay, or nil when no tooltip is displayed.")

(defconst org-scribe--overlays-posframe-buffer " *org-scribe-tooltip*"
  "Buffer name used for the posframe tooltip child frame.")

;;; Link detection

(defun org-scribe--overlays-link-at-point ()
  "Return (ID . END-POS) for the [[id:...]] link at point, or nil.
Tries the org-element API first (reliable for links in paragraphs and body
text), then falls back to a regex scan of the current line.  The fallback
is needed because org-mode does not parse property values for link objects:
`org-element-context' returns \\='node-property there, not \\='link."
  (or
   ;; Primary: element API — works for links in body text / paragraphs
   (let ((context (ignore-errors (org-element-context))))
     (when (and context
                (eq (org-element-type context) 'link)
                (string= (org-element-property :type context) "id"))
       (cons (org-element-property :path context)
             (save-excursion
               (goto-char (org-element-property :begin context))
               (search-forward "]]" (org-element-property :end context) t)
               (point)))))
   ;; Fallback: regex — catches [[id:...]] inside property drawer values
   (let* ((line-start (line-beginning-position))
          (col        (- (point) line-start))
          (line       (buffer-substring-no-properties line-start (line-end-position)))
          (pos 0)
          found)
     (while (and (not found)
                 (string-match "\\[\\[id:\\([^]\n]+\\)\\]\\[[^]\n]*\\]\\]" line pos))
       (when (and (>= col (match-beginning 0))
                  (<= col (match-end 0)))
         (setq found (cons (match-string 1 line)
                           (+ line-start (match-end 0)))))
       (setq pos (match-end 0)))
     found)))

(defun org-scribe--overlays-id-at-point ()
  "Return the ID of an [[id:...]] link at point, or nil.
Convenience wrapper around `org-scribe--overlays-link-at-point'."
  (car (org-scribe--overlays-link-at-point)))

;;; Tooltip formatting

(defconst org-scribe--overlays-plot-type-re
  "A-[Pp]lot\\|B-[Pp]lot\\|C-[Pp]lot\\|Subplot\\|Main.*[Pp]lot\\|Thread"
  "Regexp matching known plot-thread TYPE values.
Used to distinguish plot headings (TYPE: A-plot) from location headings
that also carry a :Type: property (e.g. Type: City).")

(defun org-scribe--overlays-format-tooltip (id)
  "Return a tooltip string for the entity with ID, or nil if not found.
Dispatches on entity type detected from heading properties:
  - Plot thread (THREAD-TYPE, or TYPE matching a plot pattern):
      thread-type  |  Status  |  Weight  |  From
  - Character (Role property):
      Role  |  Age  |  Occupation  |  Goal  |  Motivation  |  Conflict
  - Location / other:
      heading name only"
  (when-let* ((location (org-id-find id))
              (file (car location))
              (pos  (cdr location)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char pos)
        (let* ((name        (org-get-heading t t t t))
               ;; Plot thread: explicit THREAD-TYPE, or TYPE whose value looks
               ;; like a plot category (guards against location :Type: City).
               (thread-type (or (org-entry-get nil "THREAD-TYPE")
                                (let ((tp (org-entry-get nil "TYPE")))
                                  (when (and tp (string-match-p
                                                 org-scribe--overlays-plot-type-re tp))
                                    tp)))))
          (cond
           ;; ── Plot thread ──────────────────────────────────────────────────
           (thread-type
            (let* ((status       (org-entry-get nil "STATUS"))
                   (weight       (org-entry-get nil "Weight"))
                   (first-appear (org-entry-get nil "FIRST-APPEARANCE"))
                   (parts        (delq nil
                                       (list thread-type
                                             (when status       (format "Status: %s" status))
                                             (when weight       (format "Weight: %s" weight))
                                             (when first-appear (format "From: %s" first-appear))))))
              (format "[%s]  %s" name (string-join parts "  |  "))))
           ;; ── Character ───────────────────────────────────────────────────
           ((org-entry-get nil "Role")
            (let* ((role       (org-entry-get nil "Role"))
                   (age        (org-entry-get nil "Age"))
                   (occupation (org-entry-get nil "Occupation"))
                   (goal       (org-entry-get nil "Goal"))
                   (motivation (org-entry-get nil "Motivation"))
                   (conflict   (org-entry-get nil "Conflict"))
                   (parts      (delq nil
                                     (list (when role       (format "Role: %s" role))
                                           (when age        (format "Age: %s" age))
                                           (when occupation (format "Occupation: %s" occupation))
                                           (when goal       (format "Goal: %s" goal))
                                           (when motivation (format "Motivation: %s" motivation))
                                           (when conflict   (format "Conflict: %s" conflict))))))
              (if parts
                  (format "[%s]  %s" name (string-join parts "  |  "))
                (format "[%s]" name))))
           ;; ── Location / other ────────────────────────────────────────────
           (t (format "[%s]" name))))))))

;;; Inline display

(defun org-scribe--overlays-show-inline (tooltip end-pos)
  "Display TOOLTIP as styled text after the link ending at END-POS."
  (org-scribe--overlays-hide-inline)
  (let ((ov (make-overlay end-pos end-pos nil t nil)))
    (overlay-put ov 'after-string
                 (propertize (concat "  " tooltip)
                             'face 'org-scribe-overlays-face))
    (overlay-put ov 'org-scribe-overlay t)
    (setq org-scribe--overlays-overlay ov)))

(defun org-scribe--overlays-hide-inline ()
  "Delete the active inline tooltip overlay, if any."
  (when org-scribe--overlays-overlay
    (delete-overlay org-scribe--overlays-overlay)
    (setq org-scribe--overlays-overlay nil)))

;;; Posframe display

(defun org-scribe--overlays-show-posframe (tooltip)
  "Display TOOLTIP in a posframe child frame near point."
  (posframe-show org-scribe--overlays-posframe-buffer
                 :string (concat " " tooltip " ")
                 :position (point)
                 :poshandler #'posframe-poshandler-point-bottom-left-corner
                 :border-width 1
                 :border-color (face-foreground 'shadow nil t)
                 :background-color (face-background 'tooltip nil t)
                 :foreground-color (face-foreground 'tooltip nil t)
                 :accept-focus nil))

(defun org-scribe--overlays-hide-posframe ()
  "Hide the posframe tooltip child frame."
  (when (featurep 'posframe)
    (posframe-hide org-scribe--overlays-posframe-buffer)))

;;; Unified show / clear

(defun org-scribe--overlays-show (tooltip end-pos)
  "Display TOOLTIP using the style set by `org-scribe-overlays-display'.
END-POS is the buffer position just after the ID link, used for inline style."
  (pcase (bound-and-true-p org-scribe-overlays-display)
    ('posframe
     (if (and (require 'posframe nil t) (posframe-workable-p))
         (org-scribe--overlays-show-posframe tooltip)
       (org-scribe--overlays-show-inline tooltip end-pos)))
    ('echo
     (message "%s" tooltip))
    (_
     (org-scribe--overlays-show-inline tooltip end-pos))))

(defun org-scribe--overlays-clear ()
  "Remove any active tooltip display."
  (org-scribe--overlays-hide-inline)
  (org-scribe--overlays-hide-posframe))

;;; Post-command hook

(defun org-scribe--overlays-post-command ()
  "Show an entity tooltip when point is on an ID link.
Registered on `post-command-hook' by `org-scribe-overlays-mode'."
  (when (derived-mode-p 'org-mode)
    (let* ((link   (org-scribe--overlays-link-at-point))
           (id     (car link))
           (endpos (cdr link)))
      (cond
       ((null id)
        (org-scribe--overlays-clear)
        (setq org-scribe--overlays-last-id nil))
       ((not (equal id org-scribe--overlays-last-id))
        (setq org-scribe--overlays-last-id id)
        (when-let ((tooltip (org-scribe--overlays-format-tooltip id)))
          (org-scribe--overlays-show tooltip endpos)))))))

;;; Minor mode

;;;###autoload
(define-minor-mode org-scribe-overlays-mode
  "Show entity information when point is on an [[id:...]] link.

When active, moving point onto any ID link in a scene property or body text
displays a tooltip with the entity's key properties (Role, Age, Occupation,
Goal, Motivation, Conflict for characters; heading name for other types).

The display style is controlled by `org-scribe-overlays-display':
  inline   — styled text after the link (default)
  posframe — floating frame near point (requires posframe)
  echo     — echo area

This mode is buffer-local and disabled by default.  To enable it
automatically for all `org-scribe-mode' buffers, set:

  (setq org-scribe-overlays-enable t)"
  :lighter " Tip"
  :group 'org-scribe
  (if org-scribe-overlays-mode
      (progn
        (add-hook 'post-command-hook #'org-scribe--overlays-post-command nil t)
        (message (org-scribe-msg 'msg-overlays-enabled)))
    (remove-hook 'post-command-hook #'org-scribe--overlays-post-command t)
    (org-scribe--overlays-clear)
    (setq org-scribe--overlays-last-id nil)
    (message (org-scribe-msg 'msg-overlays-disabled))))

;;; Auto-enable integration

(defun org-scribe--overlays-maybe-enable ()
  "Enable `org-scribe-overlays-mode' when `org-scribe-overlays-enable' is non-nil.
Added to `org-scribe-mode-hook'."
  (when (bound-and-true-p org-scribe-overlays-enable)
    (org-scribe-overlays-mode 1)))

(add-hook 'org-scribe-mode-hook #'org-scribe--overlays-maybe-enable)

(provide 'org-scribe-overlays)

;;; org-scribe-overlays.el ends here
