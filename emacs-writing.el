;;; emacs-writing.el --- Comprehensive toolkit for creative writing in Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; Author: Javier Castilla <jcastp@pm.me>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (org "9.6") (org-ql "0.8") (writeroom-mode "3.7"))
;; Keywords: writing, org-mode, novel, fiction, project
;; URL: https://codeberg.org/jcastp/emacs-writing

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

;; emacs-writing provides a comprehensive toolkit for creative writers
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
(require 'writing-core (expand-file-name "core/writing-core" (file-name-directory load-file-name)))
(require 'writing-config (expand-file-name "core/writing-config" (file-name-directory load-file-name)))

;; Project creation and templates (merged from emacs-writing-template)
(require 'writing-project (expand-file-name "templates/writing-project" (file-name-directory load-file-name)))

;; Load all feature modules
;; main writing modes
(require 'writing-modes (expand-file-name "modes/writing-modes" (file-name-directory load-file-name)))
;; word counting and tracking
(require 'writing-wordcount (expand-file-name "counting/writing-wordcount" (file-name-directory load-file-name)))
(require 'writing-tracking (expand-file-name "counting/writing-tracking" (file-name-directory load-file-name)))
;; novel related searches
(require 'writing-search (expand-file-name "search/writing-search" (file-name-directory load-file-name)))
;; dictionary searches
(require 'writing-dictionary (expand-file-name "language/writing-dictionary" (file-name-directory load-file-name)))
;; org capture to the writing project
(require 'writing-capture (expand-file-name "capture/writing-capture" (file-name-directory load-file-name)))
;; writing - export
(require 'writing-export (expand-file-name "export/writing-export" (file-name-directory load-file-name)))
;; hydra for better access to common functions
(require 'writing-hydra (expand-file-name "ui/writing-hydra" (file-name-directory load-file-name)))

;;;###autoload
(defun writing-version ()
  "Display emacs-writing version."
  (interactive)
  (message "emacs-writing version 0.2.0 (includes project templates)"))

;;;###autoload
(define-minor-mode emacs-writing-mode
  "Minor mode for creative writing features.
Provides keybindings and menu for all emacs-writing functions."
  :lighter " Write"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<f8> <f8>") 'hydra-writing/body)
            (define-key map (kbd "C-c W") 'writing/capture-to-file)
            map)
  :group 'writing)

;;;###autoload
(defun emacs-writing-setup ()
  "Set up emacs-writing for use.
This function can be called from your init file to set up
recommended keybindings and hooks."
  (interactive)
  ;; Add hook to enable in org-mode
  (add-hook 'org-mode-hook #'emacs-writing-mode))

(provide 'emacs-writing)

;;; emacs-writing.el ends here
