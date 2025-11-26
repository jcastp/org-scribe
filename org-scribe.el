;;; org-scribe.el --- Comprehensive toolkit for creative writing in Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; Author: Javier Castilla <jcastp@pm.me>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (org "9.6") (org-ql "0.8") (writeroom-mode "3.7"))
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

;; Core modules (always loaded)
;; Load messages first - other modules depend on it
(require 'org-scribe-messages (expand-file-name "core/org-scribe-messages" (file-name-directory load-file-name)))
(require 'org-scribe-core (expand-file-name "core/org-scribe-core" (file-name-directory load-file-name)))
(require 'org-scribe-config (expand-file-name "core/org-scribe-config" (file-name-directory load-file-name)))

;; Project creation and templates (merged from emacs-org-scribe-template)
(require 'org-scribe-project (expand-file-name "templates/org-scribe-project" (file-name-directory load-file-name)))

;; Load all feature modules
;; main writing modes
(require 'org-scribe-modes (expand-file-name "modes/org-scribe-modes" (file-name-directory load-file-name)))
;; word counting and tracking
(require 'org-scribe-wordcount (expand-file-name "counting/org-scribe-wordcount" (file-name-directory load-file-name)))
(require 'org-scribe-tracking (expand-file-name "counting/org-scribe-tracking" (file-name-directory load-file-name)))
;; novel related searches
(require 'org-scribe-search (expand-file-name "search/org-scribe-search" (file-name-directory load-file-name)))
;; dictionary searches
(require 'org-scribe-dictionary (expand-file-name "language/org-scribe-dictionary" (file-name-directory load-file-name)))
;; org capture to the writing project
(require 'org-scribe-capture (expand-file-name "capture/org-scribe-capture" (file-name-directory load-file-name)))
;; character linking system
(require 'org-scribe-link-update (expand-file-name "linking/org-scribe-link-update" (file-name-directory load-file-name)))
(require 'org-scribe-character-links (expand-file-name "linking/org-scribe-character-links" (file-name-directory load-file-name)))
(require 'org-scribe-character-relationships (expand-file-name "linking/org-scribe-character-relationships" (file-name-directory load-file-name)))
(require 'org-scribe-location-links (expand-file-name "linking/org-scribe-location-links" (file-name-directory load-file-name)))
(require 'org-scribe-plot-links (expand-file-name "linking/org-scribe-plot-links" (file-name-directory load-file-name)))
;; column view enhancements for ID links
(require 'org-scribe-column-view (expand-file-name "linking/org-scribe-column-view" (file-name-directory load-file-name)))
;; writing - export
(require 'org-scribe-export (expand-file-name "export/org-scribe-export" (file-name-directory load-file-name)))
;; hydra for better access to common functions
(require 'org-scribe-hydra (expand-file-name "ui/org-scribe-hydra" (file-name-directory load-file-name)))

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
