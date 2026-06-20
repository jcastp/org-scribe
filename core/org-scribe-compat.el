;;; org-scribe-compat.el --- Backward-compatibility aliases -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; org-scribe historically used a slash separator (`org-scribe/foo') for
;; public commands.  As of 0.5.0 the public prefix is `org-scribe-foo',
;; matching the Emacs Lisp convention and the package's own variable
;; naming.  This file provides obsolete aliases from every old slash name
;; to its hyphenated replacement so existing keybindings and configuration
;; keep working for one release.  Byte-compilation will emit deprecation
;; warnings pointing at the new names.
;;
;; This file is loaded last so that every replacement symbol already
;; exists.  It can be deleted in a future release once users have migrated.

;;; Code:

(define-obsolete-function-alias 'org-scribe/add-character-ids 'org-scribe-add-character-ids "0.5.0")
(define-obsolete-function-alias 'org-scribe/add-location-ids 'org-scribe-add-location-ids "0.5.0")
(define-obsolete-function-alias 'org-scribe/add-plot-thread-ids 'org-scribe-add-plot-thread-ids "0.5.0")
(define-obsolete-function-alias 'org-scribe/add-relationship 'org-scribe-add-relationship "0.5.0")
(define-obsolete-function-alias 'org-scribe/capture-character 'org-scribe-capture-character "0.5.0")
(define-obsolete-function-alias 'org-scribe/capture-character-file 'org-scribe-capture-character-file "0.5.0")
(define-obsolete-function-alias 'org-scribe/capture-location 'org-scribe-capture-location "0.5.0")
(define-obsolete-function-alias 'org-scribe/capture-location-file 'org-scribe-capture-location-file "0.5.0")
(define-obsolete-function-alias 'org-scribe/capture-object 'org-scribe-capture-object "0.5.0")
(define-obsolete-function-alias 'org-scribe/capture-object-file 'org-scribe-capture-object-file "0.5.0")
(define-obsolete-function-alias 'org-scribe/capture-plot-thread 'org-scribe-capture-plot-thread "0.5.0")
(define-obsolete-function-alias 'org-scribe/capture-plot-thread-file 'org-scribe-capture-plot-thread-file "0.5.0")
(define-obsolete-function-alias 'org-scribe/capture-target-file 'org-scribe-capture-target-file "0.5.0")
(define-obsolete-variable-alias 'org-scribe/capture-templates 'org-scribe-capture-templates "0.5.0")
(define-obsolete-function-alias 'org-scribe/capture-timeline 'org-scribe-capture-timeline "0.5.0")
(define-obsolete-function-alias 'org-scribe/capture-timeline-file 'org-scribe-capture-timeline-file "0.5.0")
(define-obsolete-function-alias 'org-scribe/capture-to-file 'org-scribe-capture-to-file "0.5.0")
(define-obsolete-variable-alias 'org-scribe/character-capture-templates 'org-scribe-character-capture-templates "0.5.0")
(define-obsolete-function-alias 'org-scribe/editing-mode 'org-scribe-editing-mode "0.5.0")
(define-obsolete-variable-alias 'org-scribe/editing-mode 'org-scribe-editing-mode "0.5.0")
(define-obsolete-function-alias 'org-scribe/editing-profile 'org-scribe-editing-profile "0.5.0")
(define-obsolete-function-alias 'org-scribe/ews-org-count-words 'org-scribe-ews-org-count-words "0.5.0")
(define-obsolete-variable-alias 'org-scribe/exercise-templates 'org-scribe-exercise-templates "0.5.0")
(define-obsolete-function-alias 'org-scribe/file-notes-filename 'org-scribe-file-notes-filename "0.5.0")
(define-obsolete-function-alias 'org-scribe/insert-character-link 'org-scribe-insert-character-link "0.5.0")
(define-obsolete-function-alias 'org-scribe/insert-location-link 'org-scribe-insert-location-link "0.5.0")
(define-obsolete-function-alias 'org-scribe/insert-multiple-character-links 'org-scribe-insert-multiple-character-links "0.5.0")
(define-obsolete-function-alias 'org-scribe/insert-multiple-location-links 'org-scribe-insert-multiple-location-links "0.5.0")
(define-obsolete-function-alias 'org-scribe/insert-multiple-plot-thread-links 'org-scribe-insert-multiple-plot-thread-links "0.5.0")
(define-obsolete-function-alias 'org-scribe/insert-plot-thread-link 'org-scribe-insert-plot-thread-link "0.5.0")
(define-obsolete-function-alias 'org-scribe/insert-relationship-block 'org-scribe-insert-relationship-block "0.5.0")
(define-obsolete-function-alias 'org-scribe/jump-to-plot-thread 'org-scribe-jump-to-plot-thread "0.5.0")
(define-obsolete-function-alias 'org-scribe/jump-to-pov-character 'org-scribe-jump-to-pov-character "0.5.0")
(define-obsolete-function-alias 'org-scribe/link-all-scene-characters 'org-scribe-link-all-scene-characters "0.5.0")
(define-obsolete-function-alias 'org-scribe/link-all-scene-locations 'org-scribe-link-all-scene-locations "0.5.0")
(define-obsolete-function-alias 'org-scribe/link-all-scene-plot-threads 'org-scribe-link-all-scene-plot-threads "0.5.0")
(define-obsolete-function-alias 'org-scribe/link-scene-characters 'org-scribe-link-scene-characters "0.5.0")
(define-obsolete-function-alias 'org-scribe/link-scene-locations 'org-scribe-link-scene-locations "0.5.0")
(define-obsolete-function-alias 'org-scribe/link-scene-plot-threads 'org-scribe-link-scene-plot-threads "0.5.0")
(define-obsolete-variable-alias 'org-scribe/location-capture-templates 'org-scribe-location-capture-templates "0.5.0")
(define-obsolete-variable-alias 'org-scribe/object-capture-templates 'org-scribe-object-capture-templates "0.5.0")
(define-obsolete-function-alias 'org-scribe/org-find-character 'org-scribe-org-find-character "0.5.0")
(define-obsolete-function-alias 'org-scribe/org-find-location 'org-scribe-org-find-location "0.5.0")
(define-obsolete-function-alias 'org-scribe/org-find-plot 'org-scribe-org-find-plot "0.5.0")
(define-obsolete-function-alias 'org-scribe/org-find-pov 'org-scribe-org-find-pov "0.5.0")
(define-obsolete-variable-alias 'org-scribe/plot-thread-capture-templates 'org-scribe-plot-thread-capture-templates "0.5.0")
(define-obsolete-function-alias 'org-scribe/plot-thread-report 'org-scribe-plot-thread-report "0.5.0")
(define-obsolete-function-alias 'org-scribe/plot-thread-stats 'org-scribe-plot-thread-stats "0.5.0")
(define-obsolete-function-alias 'org-scribe/project-health 'org-scribe-project-health "0.5.0")
(define-obsolete-function-alias 'org-scribe/project-mode 'org-scribe-project-mode "0.5.0")
(define-obsolete-variable-alias 'org-scribe/project-mode 'org-scribe-project-mode "0.5.0")
(define-obsolete-function-alias 'org-scribe/rae-api-lookup 'org-scribe-rae-api-lookup "0.5.0")
(define-obsolete-function-alias 'org-scribe/rae-api-random 'org-scribe-rae-api-random "0.5.0")
(define-obsolete-function-alias 'org-scribe/rae-format-conjugations 'org-scribe-rae-format-conjugations "0.5.0")
(define-obsolete-function-alias 'org-scribe/rae-format-result 'org-scribe-rae-format-result "0.5.0")
(define-obsolete-function-alias 'org-scribe/relink-project 'org-scribe-relink-project "0.5.0")
(define-obsolete-function-alias 'org-scribe/remove-relationship 'org-scribe-remove-relationship "0.5.0")
(define-obsolete-function-alias 'org-scribe/resize-margins 'org-scribe-resize-margins "0.5.0")
(define-obsolete-function-alias 'org-scribe/sanitize-filename 'org-scribe-sanitize-filename "0.5.0")
(define-obsolete-variable-alias 'org-scribe/scene-break-replacements 'org-scribe-scene-break-replacements "0.5.0")
(define-obsolete-function-alias 'org-scribe/search-edits-recursive 'org-scribe-search-edits-recursive "0.5.0")
(define-obsolete-function-alias 'org-scribe/search-todos-recursive 'org-scribe-search-todos-recursive "0.5.0")
(define-obsolete-function-alias 'org-scribe/set-pov-character 'org-scribe-set-pov-character "0.5.0")
(define-obsolete-function-alias 'org-scribe/set-scene-characters 'org-scribe-set-scene-characters "0.5.0")
(define-obsolete-function-alias 'org-scribe/set-scene-locations 'org-scribe-set-scene-locations "0.5.0")
(define-obsolete-function-alias 'org-scribe/set-scene-plot-threads 'org-scribe-set-scene-plot-threads "0.5.0")
(define-obsolete-function-alias 'org-scribe/setup-character-links 'org-scribe-setup-character-links "0.5.0")
(define-obsolete-function-alias 'org-scribe/setup-character-relationships 'org-scribe-setup-character-relationships "0.5.0")
(define-obsolete-function-alias 'org-scribe/setup-location-links 'org-scribe-setup-location-links "0.5.0")
(define-obsolete-function-alias 'org-scribe/setup-plot-thread-links 'org-scribe-setup-plot-thread-links "0.5.0")
(define-obsolete-function-alias 'org-scribe/show-all-relationships 'org-scribe-show-all-relationships "0.5.0")
(define-obsolete-function-alias 'org-scribe/show-character-relationships 'org-scribe-show-character-relationships "0.5.0")
(define-obsolete-function-alias 'org-scribe/sinonimo 'org-scribe-sinonimo "0.5.0")
(define-obsolete-variable-alias 'org-scribe/sinonimo-window-width 'org-scribe-sinonimo-window-width "0.5.0")
(define-obsolete-variable-alias 'org-scribe/timeline-capture-templates 'org-scribe-timeline-capture-templates "0.5.0")
(define-obsolete-function-alias 'org-scribe/track-progress 'org-scribe-track-progress "0.5.0")
(define-obsolete-function-alias 'org-scribe/update-all-character-link-names 'org-scribe-update-all-character-link-names "0.5.0")
(define-obsolete-function-alias 'org-scribe/update-all-link-names 'org-scribe-update-all-link-names "0.5.0")
(define-obsolete-function-alias 'org-scribe/update-all-location-link-names 'org-scribe-update-all-location-link-names "0.5.0")
(define-obsolete-function-alias 'org-scribe/update-all-plot-link-names 'org-scribe-update-all-plot-link-names "0.5.0")
(define-obsolete-function-alias 'org-scribe/update-character-link-names 'org-scribe-update-character-link-names "0.5.0")
(define-obsolete-function-alias 'org-scribe/update-location-link-names 'org-scribe-update-location-link-names "0.5.0")
(define-obsolete-function-alias 'org-scribe/update-plot-link-names 'org-scribe-update-plot-link-names "0.5.0")
(define-obsolete-function-alias 'org-scribe/update-scene-wordcounts 'org-scribe-update-scene-wordcounts "0.5.0")
(define-obsolete-function-alias 'org-scribe/validate-directory 'org-scribe-validate-directory "0.5.0")
(define-obsolete-function-alias 'org-scribe/window-perc 'org-scribe-window-perc "0.5.0")
(define-obsolete-function-alias 'org-scribe/wordcount 'org-scribe-wordcount "0.5.0")
(define-obsolete-function-alias 'org-scribe/workspace 'org-scribe-workspace "0.5.0")
(define-obsolete-function-alias 'org-scribe/writing-env-mode 'org-scribe-writing-env-mode "0.5.0")
(define-obsolete-variable-alias 'org-scribe/writing-env-mode 'org-scribe-writing-env-mode "0.5.0")
(define-obsolete-function-alias 'org-scribe/writing-env-mode-focus 'org-scribe-writing-env-mode-focus "0.5.0")
(define-obsolete-variable-alias 'org-scribe/writing-env-mode-focus 'org-scribe-writing-env-mode-focus "0.5.0")

(provide 'org-scribe-compat)

;;; org-scribe-compat.el ends here
