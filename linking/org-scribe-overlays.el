;;; org-scribe-overlays.el --- Entity tooltips for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides `org-scribe-overlays-mode', a buffer-local minor mode that displays
;; entity information in the echo area when point moves onto an ID link in a
;; scene property.
;;
;; For characters the tooltip shows Role, Age, Goal, and Motivation.
;; For other entity types it shows the heading name only.
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

;;; Internal state

(defvar-local org-scribe--overlays-last-id nil
  "ID of the last entity whose tooltip was shown, used to avoid redundant lookups.")

;;; Tooltip formatting

(defun org-scribe--overlays-format-tooltip (id)
  "Return a tooltip string for the entity with ID, or nil if the ID is not found.
Reads Role, Age, Goal, and Motivation from the entity heading's properties."
  (when-let* ((location (org-id-find id))
              (file (car location))
              (pos  (cdr location)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char pos)
        (let* ((name       (org-get-heading t t t t))
               (role       (org-entry-get nil "Role"))
               (age        (org-entry-get nil "Age"))
               (goal       (org-entry-get nil "Goal"))
               (motivation (org-entry-get nil "Motivation"))
               (parts      (delq nil
                                 (list (when role       (format "Role: %s" role))
                                       (when age        (format "Age: %s" age))
                                       (when goal       (format "Goal: %s" goal))
                                       (when motivation (format "Motivation: %s" motivation))))))
          (if parts
              (format "[%s]  %s" name (string-join parts "  |  "))
            (format "[%s]" name)))))))

;;; Post-command hook

(defun org-scribe--overlays-post-command ()
  "Show an entity tooltip in the echo area when point is on an ID link.
Registered on `post-command-hook' by `org-scribe-overlays-mode'."
  (when (derived-mode-p 'org-mode)
    (let* ((context (ignore-errors (org-element-context)))
           (id (when (and context
                          (eq (org-element-type context) 'link)
                          (string= (org-element-property :type context) "id"))
                 (org-element-property :path context))))
      (cond
       ((null id)
        (setq org-scribe--overlays-last-id nil))
       ((not (equal id org-scribe--overlays-last-id))
        (setq org-scribe--overlays-last-id id)
        (when-let ((tooltip (org-scribe--overlays-format-tooltip id)))
          (message "%s" tooltip)))))))

;;; Minor mode

;;;###autoload
(define-minor-mode org-scribe-overlays-mode
  "Show entity information in the echo area when point is on an ID link.

When active, moving point onto any `[[id:...][...]]' link inside a scene
property displays a one-line tooltip with the entity's key properties
(Role, Age, Goal, Motivation for characters).

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
