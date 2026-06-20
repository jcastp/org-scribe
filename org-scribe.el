;;; org-scribe.el --- Comprehensive toolkit for creative writing in Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; Author: Javier Castilla <jcastp@pm.me>
;; Version: 0.4.0
;; Package-Requires: ((emacs "29.1") (org "9.6") (org-ql "0.8") (writeroom-mode "3.7") (hydra "0.15.0"))
;; Keywords: writing, org-mode, novel, fiction, project
;; URL: https://codeberg.org/jcastp/org-scribe

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; org-scribe provides a comprehensive toolkit for creative writers
;; working in Org-mode.  Features include:
;;
;; - Project creation from customizable templates
;; - Word counting and progress tracking
;; - Character database and timeline management
;; - Writing modes (distraction-free, focus, editing, project)
;; - Dictionary integration (RAE, synonyms, translation)
;; - Scene and chapter insertion templates
;; - Analysis tools (dialogue, tension, word frequency)
;; - Export presets for various formats
;; - Research linking and note capture
;; - Name generator and writing prompts
;;
;; Version 0.2.0 merges the functionality of emacs-writing-template,
;; providing a complete batteries-included writing environment.

;;; Code:

(require 'org)
(require 'org-element)

(defconst org-scribe--source-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding the org-scribe source tree.")

(defun org-scribe--require (feature relative-path)
  "Require FEATURE, loading it from RELATIVE-PATH under the source tree.
RELATIVE-PATH is resolved against `org-scribe--source-directory'."
  (require feature (expand-file-name relative-path org-scribe--source-directory)))

;; Modules are loaded in dependency order.  Each entry is
;; (FEATURE . RELATIVE-PATH); see CLAUDE.md for the rationale behind the
;; ordering.  Keep this list grouped by concern.
(dolist (module
         '(;; Core (always first — everything else depends on messages/core/config)
           (org-scribe-messages    . "core/org-scribe-messages")
           (org-scribe-core        . "core/org-scribe-core")
           (org-scribe-config      . "core/org-scribe-config")
           ;; Project creation and templates
           (org-scribe-project     . "templates/org-scribe-project")
           ;; Feature modules
           (org-scribe-modes       . "modes/org-scribe-modes")
           (org-scribe-wordcount   . "counting/org-scribe-wordcount")
           (org-scribe-tracking    . "counting/org-scribe-tracking")
           (org-scribe-search      . "search/org-scribe-search")
           (org-scribe-dictionary  . "language/org-scribe-dictionary")
           (org-scribe-capture     . "capture/org-scribe-capture")
           ;; Linking system (core framework + entity modules)
           (org-scribe-linking-core          . "linking/org-scribe-linking-core")
           (org-scribe-link-update           . "linking/org-scribe-link-update")
           (org-scribe-character-links       . "linking/org-scribe-character-links")
           (org-scribe-character-relationships . "linking/org-scribe-character-relationships")
           (org-scribe-location-links        . "linking/org-scribe-location-links")
           (org-scribe-plot-links            . "linking/org-scribe-plot-links")
           (org-scribe-column-view           . "linking/org-scribe-column-view")
           ;; Overlay tooltips (opt-in via org-scribe-overlays-enable)
           (org-scribe-overlays    . "linking/org-scribe-overlays")
           ;; Project health report (depends on the linking getters above)
           (org-scribe-health      . "reporting/org-scribe-health")
           ;; Export and UI
           (org-scribe-export      . "export/org-scribe-export")
           (org-scribe-hydra       . "ui/org-scribe-hydra")))
  (org-scribe--require (car module) (cdr module)))

;;;###autoload
(defun org-scribe-version ()
  "Display org-scribe version."
  (interactive)
  (message "org-scribe version 0.2.0 (includes project templates)"))

;;;###autoload
(define-minor-mode org-scribe-mode
  "Minor mode for creative writing features.
Provides keybindings and menu for all org-scribe functions."
  :lighter " Write"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<f8> <f8>") 'hydra-org-scribe/body)
            (define-key map (kbd "C-c W") 'org-scribe/capture-to-file)
            map)
  :group 'org-scribe)

;;;###autoload
(defun org-scribe-setup ()
  "Set up org-scribe for use.
This function can be called from your init file to set up
recommended keybindings and hooks."
  (interactive)
  ;; Add hook to enable in org-mode
  (add-hook 'org-mode-hook #'org-scribe-mode))

(provide 'org-scribe)

;;; org-scribe.el ends here
