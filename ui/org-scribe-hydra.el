;;; org-scribe-hydra.el --- Hydra menu for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unified hydra menu providing quick access to all writing functions.

;;; Code:

(require 'hydra)

;; Declare functions from other modules
(declare-function org-scribe/project-mode "modes/org-scribe-modes")
(declare-function org-scribe/writing-env-mode "modes/org-scribe-modes")
(declare-function org-scribe/writing-env-mode-focus "modes/org-scribe-modes")
(declare-function org-scribe/editing-mode "modes/org-scribe-modes")
(declare-function org-scribe/rae-api-lookup "language/org-scribe-dictionary")
(declare-function org-scribe/sinonimo "language/org-scribe-dictionary")
(declare-function powerthesaurus-lookup-dwim "powerthesaurus")
(declare-function org-scribe/capture-to-file "capture/org-scribe-capture")
(declare-function org-scribe/capture-character "capture/org-scribe-capture")
(declare-function org-scribe/capture-location "capture/org-scribe-capture")
(declare-function org-scribe/capture-object "capture/org-scribe-capture")
(declare-function org-scribe/capture-timeline "capture/org-scribe-capture")
(declare-function org-scribe/capture-plot-thread "capture/org-scribe-capture")
(declare-function org-context-count-words "org-context-extended")
(declare-function org-scribe/ews-org-count-words "counting/org-scribe-wordcount")
(declare-function org-tracktable-write "org-tracktable")
(declare-function org-scribe/org-find-pov "search/org-scribe-search")
(declare-function org-scribe/org-find-character "search/org-scribe-search")
(declare-function org-scribe/org-find-plot "search/org-scribe-search")
(declare-function org-scribe/org-find-location "search/org-scribe-search")
(declare-function org-scribe/search-todos-recursive "search/org-scribe-search")
(declare-function org-scribe-create-novel-project "templates/org-scribe-project")
(declare-function org-scribe-create-short-story-project "templates/org-scribe-project")
(declare-function org-scribe-insert-scene "templates/org-scribe-project")
(declare-function org-scribe-insert-chapter "templates/org-scribe-project")
(declare-function org-scribe-open-project-file "templates/org-scribe-project")
;; manage character related links
(declare-function org-scribe/set-pov-character "linking/org-scribe-character-links")
(declare-function org-scribe/set-scene-characters "linking/org-scribe-character-links")
(declare-function org-scribe/jump-to-pov-character "linking/org-scribe-character-links")
(declare-function org-scribe/link-scene-characters "linking/org-scribe-character-links")
(declare-function org-scribe/link-all-scene-characters "linking/org-scribe-character-links")
(declare-function org-scribe/add-character-ids "linking/org-scribe-character-links")
(declare-function org-scribe/setup-character-links "linking/org-scribe-character-links")
;; manage location related links
(declare-function org-scribe/set-scene-locations "linking/org-scribe-location-links")
(declare-function org-scribe/link-scene-locations "linking/org-scribe-location-links")
(declare-function org-scribe/link-all-scene-locations "linking/org-scribe-location-links")
(declare-function org-scribe/add-location-ids "linking/org-scribe-location-links")
(declare-function org-scribe/setup-location-links "linking/org-scribe-location-links")
;; manage plot thread related links
(declare-function org-scribe/set-scene-plot-threads "linking/org-scribe-plot-links")
(declare-function org-scribe/jump-to-plot-thread "linking/org-scribe-plot-links")
(declare-function org-scribe/link-scene-plot-threads "linking/org-scribe-plot-links")
(declare-function org-scribe/link-all-scene-plot-threads "linking/org-scribe-plot-links")
(declare-function org-scribe/add-plot-thread-ids "linking/org-scribe-plot-links")
(declare-function org-scribe/setup-plot-thread-links "linking/org-scribe-plot-links")
(declare-function org-scribe/plot-thread-report "linking/org-scribe-plot-links")
(declare-function org-scribe/plot-thread-stats "linking/org-scribe-plot-links")

;;;###autoload (autoload 'hydra-org-scribe-characters/body "ui/org-scribe-hydra" nil t)
(defhydra hydra-org-scribe-characters (:color blue :hint nil)
  "
^Character Linking^
^^^^^^^^------------------------------------------------------------
_p_: Set PoV character      _l_: Link scene characters
_c_: Set scene characters   _L_: Link all scenes
_j_: Jump to PoV char       _i_: Add IDs to characters
_u_: Update link names      _U_: Update all link names
_s_: Setup linking system   _q_: Back to main menu
"
  ("p" org-scribe/set-pov-character "set PoV")
  ("c" org-scribe/set-scene-characters "set characters")
  ("j" org-scribe/jump-to-pov-character "jump to PoV")
  ("l" org-scribe/link-scene-characters "link scene")
  ("L" org-scribe/link-all-scene-characters "link all")
  ("i" org-scribe/add-character-ids "add IDs")
  ("u" org-scribe/update-character-link-names "update names")
  ("U" org-scribe/update-all-character-link-names "update all names")
  ("s" org-scribe/setup-character-links "setup system")
  ("q" hydra-org-scribe/body "back")
  ("Q" nil "quit"))

;;;###autoload (autoload 'hydra-org-scribe-locations/body "ui/org-scribe-hydra" nil t)
(defhydra hydra-org-scribe-locations (:color blue :hint nil)
  "
^Location Linking^
^^^^^^^^------------------------------------------------------------
_c_: Set scene locations   _l_: Link scene locations
_i_: Add IDs to locations  _L_: Link all scenes
_u_: Update link names     _U_: Update all link names
_s_: Setup linking system  _q_: Back to main menu
"
  ("c" org-scribe/set-scene-locations "set locations")
  ("l" org-scribe/link-scene-locations "link scene")
  ("L" org-scribe/link-all-scene-locations "link all")
  ("i" org-scribe/add-location-ids "add IDs")
  ("u" org-scribe/update-location-link-names "update names")
  ("U" org-scribe/update-all-location-link-names "update all names")
  ("s" org-scribe/setup-location-links "setup system")
  ("q" hydra-org-scribe/body "back")
  ("Q" nil "quit"))

;;;###autoload (autoload 'hydra-org-scribe-plot-threads/body "ui/org-scribe-hydra" nil t)
(defhydra hydra-org-scribe-plot-threads (:color blue :hint nil)
  "
^Plot Thread Linking^       ^Analysis^
^^^^^^^^------------------------------------------------------------
_p_: Set scene plot threads   _t_: Timeline table
_j_: Jump to plot thread      _r_: Health report
_l_: Link scene plot threads  _S_: Statistics
_L_: Link all scenes          _i_: Add IDs to threads
_u_: Update link names        _U_: Update all link names
_s_: Setup linking system     _q_: Back to main menu
"
  ("p" org-scribe/set-scene-plot-threads "set plot threads")
  ("j" org-scribe/jump-to-plot-thread "jump to thread")
  ("l" org-scribe/link-scene-plot-threads "link scene")
  ("L" org-scribe/link-all-scene-plot-threads "link all")
  ("i" org-scribe/add-plot-thread-ids "add IDs")
  ("u" org-scribe/update-plot-link-names "update names")
  ("U" org-scribe/update-all-plot-link-names "update all names")
  ("s" org-scribe/setup-plot-thread-links "setup system")
  ("t" (lambda () (interactive) (message "Insert '#+BEGIN: plot-thread-timeline' then press C-c C-c")) "timeline")
  ("r" org-scribe/plot-thread-report "health report")
  ("S" org-scribe/plot-thread-stats "statistics")
  ("q" hydra-org-scribe/body "back")
  ("Q" nil "quit"))

;;;###autoload (autoload 'hydra-org-scribe/body "ui/org-scribe-hydra" nil t)
(defhydra hydra-org-scribe (:color blue :hint nil)
  "
^Insert^           ^Modes^            ^Capture^          ^Tools^            ^Search^           ^Manage^
^^^^^^^^------------------------------------------------------------------------------------------------------------------
_s_: Scene         _m_: Mode (write)  _n_: Note          _w_: Words count   _1_: POV           _C_: Characters
_c_: Chapter       _p_: Project mode  _h_: cHaracter     _r_: tRack table   _2_: Character     _L_: Locations
_o_: Open file     _f_: Focus mode    _l_: Location      _a_: Add WC props  _3_: Plot          _P_: Plot threads
                 _e_: Editing mode  _b_: oBject        _d_: Dictionary    _4_: Location      _U_: Update links
                                  _t_: Timeline      _y_: sYnonyms      _5_: TODOs
                                  _g_: plot thread                                         _q_: Quit
"
  ;; Insert (most frequent actions get best keys)
  ("s" org-scribe-insert-scene "insert scene")
  ("c" org-scribe-insert-chapter "insert chapter")
  ("o" org-scribe-open-project-file "open project file")

  ;; Modes
  ("m" org-scribe/writing-env-mode "writing mode")
  ("p" org-scribe/project-mode "project mode")
  ("f" org-scribe/writing-env-mode-focus "focus mode")
  ("e" org-scribe/editing-mode "editing mode")

  ;; Capture functions
  ("n" org-scribe/capture-to-file "capture note")
  ("h" org-scribe/capture-character "capture character")
  ("l" org-scribe/capture-location "capture location")
  ("b" org-scribe/capture-object "capture object")
  ("t" org-scribe/capture-timeline "capture timeline event")
  ("g" org-scribe/capture-plot-thread "capture plot thread")

  ;; Tools
  ("w" org-context-count-words "count words")
  ("r" org-tracktable-write "track table")
  ("a" org-scribe/ews-org-count-words "add word properties")
  ("d" org-scribe/rae-api-lookup "RAE dictionary")
  ("y" org-scribe/sinonimo "synonyms")

  ;; Search (numbered for consistency)
  ("1" org-scribe/org-find-pov "find by POV")
  ("2" org-scribe/org-find-character "find by character")
  ("3" org-scribe/org-find-plot "find by plot")
  ("4" org-scribe/org-find-location "find by location")
  ("5" org-scribe/search-todos-recursive "find TODOs")

  ;; Manage (linking submenus - capitals only)
  ("C" hydra-org-scribe-characters/body "character links")
  ("L" hydra-org-scribe-locations/body "location links")
  ("P" hydra-org-scribe-plot-threads/body "plot thread links")
  ("U" org-scribe/update-all-link-names "update all link names")

  ;; Exit
  ("q" nil "quit"))

(provide 'org-scribe-hydra)

;;; org-scribe-hydra.el ends here
