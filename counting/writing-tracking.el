;;; writing-tracking.el --- Progress tracking for emacs-writing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Daily progress tracking using org-tracktable.
;; This module provides integration with the org-tracktable package
;; for tracking writing progress over time.

;;; Code:

(require 'writing-core)

;; org-tracktable should be loaded if available
(declare-function org-tracktable-write "org-tracktable")

;;;###autoload
(defun writing/track-progress ()
  "Update the writing progress track table.
Requires org-tracktable package to be installed."
  (interactive)
  (writing-when-feature org-tracktable
    (org-tracktable-write)))

(provide 'writing-tracking)

;;; writing-tracking.el ends here
