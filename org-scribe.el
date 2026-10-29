;;; org-scribe.el --- Comprehensive toolkit for creative writing in Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; Author: Javier Castilla <jcastp@pm.me>
;; Version: 0.9.0
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
;; - Word counting
;; - Character/location/plot ID-based linking, with timeline visualization
;; - Writing modes (distraction-free, focus, editing, project)
;; - Dictionary integration (RAE, synonyms)
;; - Scene and chapter insertion templates
;; - Scene break export filters
;; - Note capture
;;
;; Bundles a writing planner (see planning/).

;;; Code:

(require 'org)
(require 'org-element)
(require 'lisp-mnt)

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
         '(;; Language pack registry, then the shipped packs themselves
           ;; (before everything else: messages, core and config all read
           ;; language packs).  Each pack self-registers on `require' (see
           ;; lang/org-scribe-lang-en.el's Commentary for why); listed here
           ;; too only so the load order reads top-to-bottom -- core.el
           ;; also requires both packs directly, since several test files
           ;; require it standalone without ever loading this file.
           (org-scribe-lang         . "lang/org-scribe-lang")
           (org-scribe-lang-en      . "lang/org-scribe-lang-en")
           (org-scribe-lang-es      . "lang/org-scribe-lang-es")
           ;; Core (always first — everything else depends on messages/core/config)
           (org-scribe-messages    . "core/org-scribe-messages")
           (org-scribe-core        . "core/org-scribe-core")
           (org-scribe-config      . "core/org-scribe-config")
           ;; Story-time parser (pure; reads only the language packs and core)
           (org-scribe-story-time  . "core/org-scribe-story-time")
           ;; Project creation and templates
           (org-scribe-project     . "templates/org-scribe-project")
           ;; Feature modules
           (org-scribe-modes       . "modes/org-scribe-modes")
           (org-scribe-wordcount   . "counting/org-scribe-wordcount")
           (org-scribe-search      . "search/org-scribe-search")
           ;; `mythes' first: `dictionary' requires it for the offline backend.
           ;; `lemma' need only precede a lookup, not `mythes' itself, which
           ;; resolves the lemmatizer lazily rather than requiring it.
           (org-scribe-mythes      . "language/org-scribe-mythes")
           (org-scribe-lemma       . "language/org-scribe-lemma")
           (org-scribe-wikcionario . "language/org-scribe-wikcionario")
           (org-scribe-dictionary  . "language/org-scribe-dictionary")
           (org-scribe-capture     . "capture/org-scribe-capture")
           ;; Linking system (core framework + entity modules)
           (org-scribe-linking-core          . "linking/org-scribe-linking-core")
           (org-scribe-link-update           . "linking/org-scribe-link-update")
           (org-scribe-character-links       . "linking/org-scribe-character-links")
           (org-scribe-character-relationships . "linking/org-scribe-character-relationships")
           (org-scribe-location-links        . "linking/org-scribe-location-links")
           (org-scribe-object-links          . "linking/org-scribe-object-links")
           (org-scribe-plot-links            . "linking/org-scribe-plot-links")
           ;; Plot points load after plot threads: they share the plot file
           ;; resolver, and org-scribe--get-plot-point-file forwards to it.
           (org-scribe-plot-point-links      . "linking/org-scribe-plot-point-links")
           (org-scribe-column-view           . "linking/org-scribe-column-view")
           ;; Overlay tooltips (opt-in via org-scribe-overlays-enable)
           (org-scribe-overlays    . "linking/org-scribe-overlays")
           ;; Project health report (depends on the linking getters above)
           (org-scribe-chronology  . "reporting/org-scribe-chronology")
           (org-scribe-health      . "reporting/org-scribe-health")
           ;; Export and UI
           (org-scribe-export      . "export/org-scribe-export")
           ;; Compile builds on the export filters' scoping helpers and on
           ;; org-scribe-project-type; it loads after export, before the UI
           ;; that binds its command.
           (org-scribe-compile     . "export/org-scribe-compile")
           (org-scribe-hydra       . "ui/org-scribe-hydra")))
  (org-scribe--require (car module) (cdr module)))

;; Writing planner (planning/org-scribe-planner.el) — lazy-loaded via autoloads.
;; Adding planning/ to load-path lets the autoload entries (which use bare
;; feature names like "org-scribe-planner") resolve correctly.  Loading the
;; autoloads file registers every ;;;###autoload command so they appear in
;; M-x and are callable without an explicit (require 'org-scribe-planner).
(let ((planning-dir (expand-file-name "planning" org-scribe--source-directory)))
  (add-to-list 'load-path planning-dir)
  (load (expand-file-name "org-scribe-planner-autoloads" planning-dir) nil :nomessage))

;; Visual companion (companion/) — off until `org-scribe-companion-enable'.
;; Only the load path and the (cheap) autoloads file are set up here; the
;; module itself is required by `org-scribe-companion-open' after its checks.
(let ((companion-dir (expand-file-name "companion" org-scribe--source-directory)))
  (add-to-list 'load-path companion-dir)
  (load (expand-file-name "org-scribe-companion-autoloads" companion-dir) nil :nomessage))

;; To disable individual integration features before the planner loads:
;;   (setq org-scribe-planner-auto-load-plan nil)   ; no auto-load on project open
;;   (setq org-scribe-planner-auto-push-wordcount nil) ; no push after word count
;;   (setq org-scribe-planner-offer-plan-on-create nil) ; no prompt on new project
;;   (setq org-scribe-planner-show-mode-line nil)   ; no [W:n/n] in mode line

;;;###autoload
(defun org-scribe-version ()
  "Display org-scribe version, read from the \"Version:\" package header.
Avoids drifting out of sync with the header, unlike a hardcoded string."
  (interactive)
  (message "org-scribe version %s"
           (or (lm-version (expand-file-name "org-scribe.el" org-scribe--source-directory))
               "unknown")))

;;;###autoload
(define-minor-mode org-scribe-mode
  "Minor mode for creative writing features.
Provides keybindings and menu for all org-scribe functions.

Also installs `org-scribe--auto-wordcount-before-save' and
`org-scribe--auto-update-links-after-save' as buffer-local save hooks
(removed again on disable).  Both remain individually opt-in via
`org-scribe-auto-wordcount' / `org-scribe-auto-relink' and no-op unless the
buffer is an org-scribe manuscript/entity file; installing them
buffer-locally here, rather than unconditionally on the global
`before-save-hook' / `after-save-hook' at module load time, means buffers
where org-scribe-mode is never turned on never pay the cost of running
project detection on every save (L3)."
  :lighter " Write"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<f8>") 'hydra-org-scribe/body)
            (define-key map (kbd "C-c W") 'org-scribe-capture-to-file)
            map)
  :group 'org-scribe
  (if org-scribe-mode
      (progn
        (add-hook 'before-save-hook #'org-scribe--auto-wordcount-before-save nil t)
        (add-hook 'after-save-hook #'org-scribe--auto-update-links-after-save nil t))
    (remove-hook 'before-save-hook #'org-scribe--auto-wordcount-before-save t)
    (remove-hook 'after-save-hook #'org-scribe--auto-update-links-after-save t)))


;;; Planner auto-load

(defvar org-scribe-planner-auto-load-plan)
(declare-function org-scribe-planner--auto-load-plan "org-scribe-planner")

(defun org-scribe--planner-maybe-autoload ()
  "Load the writing planner when the current project has opted into it.
Runs on `org-scribe-mode-hook'.  The planner is autoloaded, and its own
integrations — the auto-load-plan hook, the word-count bridge in
`org-scribe-ews-org-count-words', save-time syncing — are only wired up
once it loads.  Without this, a fresh session never loaded it until the
writer happened to open a planner view, and every word count until then
silently missed the ledger.

Only a project whose gate is \\='yes (see `org-scribe-planner-gate') pays
the load; undecided and \\='no projects are left alone.  The plan itself
is loaded through `org-scribe-planner--auto-load-plan' directly, rather
than left to the hook the planner adds while loading: that hook is added
in the middle of this very `org-scribe-mode-hook' run and would not fire
until the next buffer.  Calling it twice is harmless — it does nothing
once a plan is active.  `org-scribe-planner-auto-load-plan' set to nil
still keeps the plan from loading; it does not keep the planner (and so
the ledger) from working."
  (when (and org-scribe-mode
             (not (featurep 'org-scribe-planner))
             (org-scribe-project-root)
             (eq (org-scribe-planner-gate) 'yes))
    (with-demoted-errors "org-scribe: could not load the planner: %S"
      (when (and (require 'org-scribe-planner nil t)
                 org-scribe-planner-auto-load-plan)
        (org-scribe-planner--auto-load-plan)))))

(add-hook 'org-scribe-mode-hook #'org-scribe--planner-maybe-autoload)

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
     (hydra          . "The F8 command menu"))
    (:recommended
     (org-context-extended . "Accurate word counts that exclude Org metadata"))
    (:optional
     (consult        . "Theme switching in writing workspaces")
     (fontaine       . "Font preset management")
     (treemacs       . "File tree in the navigate workspace")
     (imenu-list     . "Document outline in the edit workspace")
     (guess-language . "Automatic language detection")
     (gt             . "Translation support")
     (powerthesaurus . "English thesaurus")
     (tempel         . "Snippets for inline edit markers (org-scribe-tempel-setup)")
     (org-remark     . "Text annotations")
     (ox-extra       . "Makes the :ignore: tag on scene/chapter headings actually drop the title from export while keeping its body; part of the org-contrib package. Without it, exported manuscripts show raw TODO keywords and heading titles.")
     ("pdflatex"     . "PDF output from `org-scribe-compile'; needs a LaTeX distribution (TeX Live, MacTeX, or MiKTeX). Without it, `org-scribe-compile' reports the missing tool by name rather than failing partway through.")
     ("pandoc"       . "DOCX output from `org-scribe-compile'; needs the pandoc command-line tool (https://pandoc.org). Without it, `org-scribe-compile' reports the missing tool by name rather than failing partway through.")
     (ox-epub        . "EPUB output from `org-scribe-compile'. Not on a package archive by default on all systems; available via MELPA (M-x package-install RET ox-epub RET). Without it, `org-scribe-compile' reports it by name rather than failing partway through.")))
  "org-scribe dependencies grouped by importance, each with a description.
Only the `:required' group is needed for the package to load; the
`:recommended' group degrades gracefully when absent and the `:optional'
group simply leaves the corresponding feature inert.")

(defconst org-scribe--package-urls
  '((org-context-extended . "https://codeberg.org/jcastp/org-context-extended"))
  "Source URLs for dependencies that are not on a package archive.")

(defun org-scribe--feature-available-p (feature)
  "Return non-nil when FEATURE is available.
FEATURE is either a symbol naming an Elisp feature -- checked via
`featurep' or `locate-library', as before -- or a string naming an
external executable, checked via `executable-find'.  The string case
exists for dependencies `org-scribe-compile' shells out to (pandoc,
pdflatex): there is no Elisp library to require for those, so the usual
check does not apply."
  (if (stringp feature)
      (and (executable-find feature) t)
    (or (featurep feature)
        (and (locate-library (symbol-name feature)) t))))

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
      (org-scribe--setup-check-companion)
      (goto-char (point-min)))
    (display-buffer (current-buffer))))

(defun org-scribe--setup-check-companion ()
  "Insert the visual companion's state: the global switch, this project's
gate and the server.  Reads the switch and the gate only; never loads the
companion module."
  (insert "* Visual companion\n"
          (format "- [%s] =org-scribe-companion-enable= — global switch (off: the module is never loaded)\n"
                  (if org-scribe-companion-enable "X" " ")))
  (let ((gate (and (not (eq (org-scribe-project-type) 'unknown))
                   (org-scribe-companion-gate))))
    (insert (format "- [%s] this project — %s\n"
                    (if (eq gate 'yes) "X" " ")
                    (cond ((eq (org-scribe-project-type) 'unknown) "not inside an org-scribe project")
                          ((eq gate 'yes) "opted in (# Companion: yes)")
                          ((eq gate 'no) "opted out (# Companion: no)")
                          (t "undecided: asked the first time it is opened")))))
  (insert (format "- [%s] server — %s\n\n"
                  (if (and (featurep 'org-scribe-companion)
                           (org-scribe-companion-server-running-p))
                      "X" " ")
                  "running now (F8 B opens it; F8 B again after stopping restarts it)")))

(provide 'org-scribe)

;;; org-scribe.el ends here
