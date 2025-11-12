;;; emacs-writing.el --- Comprehensive toolkit for creative writing in Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; Author: Javier Castilla <jcastp@pm.me>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6") (org-ql "0.8") (writeroom-mode "3.7"))
;; Keywords: writing, org-mode, novel, fiction
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
;; - Word counting and progress tracking
;; - Character database and timeline management
;; - Writing modes (distraction-free, focus, editing, project)
;; - Dictionary integration (RAE, synonyms, translation)
;; - Scene management and templates
;; - Analysis tools (dialogue, tension, word frequency)
;; - Export presets for various formats
;; - Research linking and note capture
;; - Name generator and writing prompts
;;
;; Designed to work seamlessly with emacs-writing-template for
;; project scaffolding, but can be used independently.

;;; Code:

(require 'org)
(require 'org-element)

;; Core modules (always loaded)
(require 'writing-core (expand-file-name "core/writing-core" (file-name-directory load-file-name)))
(require 'writing-config (expand-file-name "core/writing-config" (file-name-directory load-file-name)))

;; Feature detection
(defvar writing-template-available-p (featurep 'writing-template)
  "Non-nil if emacs-writing-template is installed.")

;; Autoloads for word counting
;;;###autoload (autoload 'writing/ews-org-count-words "counting/writing-wordcount" nil t)
;;;###autoload (autoload 'org-context-count-words "counting/writing-wordcount" nil t)

;; Autoloads for writing modes
;;;###autoload (autoload 'my-writing-env-mode "modes/writing-modes" nil t)
;;;###autoload (autoload 'my-writing-env-mode-focus "modes/writing-modes" nil t)
;;;###autoload (autoload 'project-writing-mode "modes/writing-modes" nil t)
;;;###autoload (autoload 'writing/editing-mode "modes/writing-modes" nil t)

;; Autoloads for search functions
;;;###autoload (autoload 'writing/org-find-pov "search/writing-search" nil t)
;;;###autoload (autoload 'writing/org-find-character "search/writing-search" nil t)
;;;###autoload (autoload 'writing/org-find-plot "search/writing-search" nil t)
;;;###autoload (autoload 'writing/org-find-location "search/writing-search" nil t)
;;;###autoload (autoload 'writing/search-todos-recursive "search/writing-search" nil t)

;; Autoloads for dictionary
;;;###autoload (autoload 'writing/rae-api-lookup "language/writing-dictionary" nil t)
;;;###autoload (autoload 'writing/rae-api-random "language/writing-dictionary" nil t)
;;;###autoload (autoload 'writing/sinonimo "language/writing-dictionary" nil t)

;; Autoloads for capture
;;;###autoload (autoload 'writing/capture-to-file "capture/writing-capture" nil t)

;; Autoloads for hydra
;;;###autoload (autoload 'hydra-writing/body "ui/writing-hydra" nil t)

;;;###autoload
(defun writing-version ()
  "Display emacs-writing version."
  (interactive)
  (message "emacs-writing version 0.1.0"))

;;;###autoload
(define-minor-mode emacs-writing-mode
  "Minor mode for creative writing features.
Provides keybindings and menu for all emacs-writing functions."
  :lighter " Write"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "F8 F8") 'hydra-writing/body)
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
