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
           (org-scribe-hydra       . "ui/org-scribe-hydra")
           ;; Backward-compatibility aliases (loaded last: every replacement
           ;; symbol must already exist before the obsolete aliases bind)
           (org-scribe-compat      . "core/org-scribe-compat")))
  (org-scribe--require (car module) (cdr module)))

;; The writing planner (planning/org-scribe-planner.el) is NOT loaded here.
;; All its public commands carry ;;;###autoload cookies and are available
;; immediately after install without any explicit require.  When called,
;; they load planning/org-scribe-planner.el on demand, which in turn wires
;; itself into org-scribe via (with-eval-after-load 'org-scribe ...).
;; To disable individual integration features before the planner loads:
;;   (setq org-scribe-planner-auto-load-plan nil)   ; no auto-load on project open
;;   (setq org-scribe-planner-auto-push-wordcount nil) ; no push after word count
;;   (setq org-scribe-planner-offer-plan-on-create nil) ; no prompt on new project
;;   (setq org-scribe-planner-show-mode-line nil)   ; no [W:n/n] in mode line

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
            (define-key map (kbd "C-c W") 'org-scribe-capture-to-file)
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

;;; Setup self-check (onboarding)

(defconst org-scribe--dependencies
  '((:required
     (org            . "Org mode (built in)")
     (org-ql         . "Searches: find scenes by PoV / character / plot / location")
     (writeroom-mode . "Distraction-free writing workspaces")
     (hydra          . "The F8 F8 command menu"))
    (:recommended
     (org-context-extended . "Accurate word counts that exclude Org metadata")
     (org-tracktable       . "Daily writing-progress tracking"))
    (:optional
     (consult        . "Theme switching in writing workspaces")
     (fontaine       . "Font preset management")
     (treemacs       . "File tree in the navigate workspace")
     (imenu-list     . "Document outline in the edit workspace")
     (guess-language . "Automatic language detection")
     (gt             . "Translation support")
     (powerthesaurus . "English thesaurus")
     (org-remark     . "Text annotations")))
  "org-scribe dependencies grouped by importance, each with a description.
Only the `:required' group is needed for the package to load; the
`:recommended' group degrades gracefully when absent and the `:optional'
group simply leaves the corresponding feature inert.")

(defconst org-scribe--package-urls
  '((org-context-extended . "https://codeberg.org/jcastp/org-context-extended")
    (org-tracktable       . "https://codeberg.org/jcastp/org-tracktable"))
  "Source URLs for dependencies that are not on a package archive.")

(defun org-scribe--feature-available-p (feature)
  "Return non-nil when FEATURE is loaded or installed on the `load-path'."
  (or (featurep feature)
      (and (locate-library (symbol-name feature)) t)))

;;;###autoload
(defun org-scribe-setup-check ()
  "Report which org-scribe features are active and which are missing.
Opens a buffer listing every dependency grouped by importance, marking
each as available or missing, with a one-line description of what it
enables and an install hint for missing non-archive packages.  Nothing is
installed or changed — this is purely a diagnostic report to make
onboarding a single, self-explanatory step."
  (interactive)
  (with-current-buffer (get-buffer-create "*org-scribe setup*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when (fboundp 'org-mode) (org-mode))
      (insert "#+TITLE: org-scribe setup check\n"
              "# [X] available   [ ] missing\n\n")
      (dolist (group '((:required    . "Required (must be present)")
                       (:recommended . "Recommended (graceful fallback if missing)")
                       (:optional    . "Optional (feature inert if missing)")))
        (insert (format "* %s\n" (cdr group)))
        (dolist (cell (cdr (assq (car group) org-scribe--dependencies)))
          (let* ((feature (car cell))
                 (desc (cdr cell))
                 (ok (org-scribe--feature-available-p feature)))
            (insert (format "- [%s] =%s= — %s\n"
                            (if ok "X" " ") feature desc))
            (when-let* (((not ok))
                        (url (alist-get feature org-scribe--package-urls)))
              (insert (format "  install: %s\n" url)))))
        (insert "\n"))
      (goto-char (point-min)))
    (display-buffer (current-buffer))))

(provide 'org-scribe)

;;; org-scribe.el ends here
