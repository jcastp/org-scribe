;;; writing-hydra.el --- Hydra menu for emacs-writing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unified hydra menu providing quick access to all writing functions.

;;; Code:

(require 'hydra)

;; Declare functions from other modules
(declare-function project-writing-mode "modes/writing-modes")
(declare-function my-writing-env-mode "modes/writing-modes")
(declare-function my-writing-env-mode-focus "modes/writing-modes")
(declare-function writing/editing-mode "modes/writing-modes")
(declare-function writing/rae-api-lookup "language/writing-dictionary")
(declare-function writing/sinonimo "language/writing-dictionary")
(declare-function powerthesaurus-lookup-dwim "powerthesaurus")
(declare-function writing/capture-to-file "capture/writing-capture")
(declare-function writing/capture-character "capture/writing-capture")
(declare-function writing/capture-location "capture/writing-capture")
(declare-function writing/capture-object "capture/writing-capture")
(declare-function writing/capture-timeline "capture/writing-capture")
(declare-function writing/capture-plot-thread "capture/writing-capture")
(declare-function org-context-count-words "org-context-extended")
(declare-function writing/ews-org-count-words "counting/writing-wordcount")
(declare-function org-tracktable-write "org-tracktable")
(declare-function writing/org-find-pov "search/writing-search")
(declare-function writing/org-find-character "search/writing-search")
(declare-function writing/org-find-plot "search/writing-search")
(declare-function writing/org-find-location "search/writing-search")
(declare-function writing/search-todos-recursive "search/writing-search")
(declare-function writing-create-novel-project "templates/writing-project")
(declare-function writing-create-short-story-project "templates/writing-project")
(declare-function writing-insert-scene "templates/writing-project")
(declare-function writing-insert-chapter "templates/writing-project")
(declare-function writing-open-project-file "templates/writing-project")
;; manage character related links
(declare-function writing/set-pov-character "linking/writing-character-links")
(declare-function writing/set-scene-characters "linking/writing-character-links")
(declare-function writing/jump-to-pov-character "linking/writing-character-links")
(declare-function writing/link-scene-characters "linking/writing-character-links")
(declare-function writing/link-all-scene-characters "linking/writing-character-links")
(declare-function writing/add-character-ids "linking/writing-character-links")
(declare-function writing/setup-character-links "linking/writing-character-links")
;; manage location related links
(declare-function writing/set-scene-locations "linking/writing-location-links")
(declare-function writing/link-scene-locations "linking/writing-location-links")
(declare-function writing/link-all-scene-locations "linking/writing-location-links")
(declare-function writing/add-location-ids "linking/writing-location-links")
(declare-function writing/setup-location-links "linking/writing-location-links")
;; manage plot thread related links
(declare-function writing/set-scene-plot-threads "linking/writing-plot-links")
(declare-function writing/jump-to-plot-thread "linking/writing-plot-links")
(declare-function writing/link-scene-plot-threads "linking/writing-plot-links")
(declare-function writing/link-all-scene-plot-threads "linking/writing-plot-links")
(declare-function writing/add-plot-thread-ids "linking/writing-plot-links")
(declare-function writing/setup-plot-thread-links "linking/writing-plot-links")
(declare-function writing/plot-thread-report "linking/writing-plot-links")
(declare-function writing/plot-thread-stats "linking/writing-plot-links")

;;;###autoload (autoload 'hydra-writing-characters/body "ui/writing-hydra" nil t)
(defhydra hydra-writing-characters (:color blue :hint nil)
  "
^Character Linking^
^^^^^^^^------------------------------------------------------------
_p_: Set PoV character      _l_: Link scene characters
_c_: Set scene characters   _L_: Link all scenes
_j_: Jump to PoV char       _i_: Add IDs to characters
_u_: Update link names      _U_: Update all link names
_s_: Setup linking system   _q_: Back to main menu
"
  ("p" writing/set-pov-character "set PoV")
  ("c" writing/set-scene-characters "set characters")
  ("j" writing/jump-to-pov-character "jump to PoV")
  ("l" writing/link-scene-characters "link scene")
  ("L" writing/link-all-scene-characters "link all")
  ("i" writing/add-character-ids "add IDs")
  ("u" writing/update-character-link-names "update names")
  ("U" writing/update-all-character-link-names "update all names")
  ("s" writing/setup-character-links "setup system")
  ("q" hydra-writing/body "back")
  ("Q" nil "quit"))

;;;###autoload (autoload 'hydra-writing-locations/body "ui/writing-hydra" nil t)
(defhydra hydra-writing-locations (:color blue :hint nil)
  "
^Location Linking^
^^^^^^^^------------------------------------------------------------
_c_: Set scene locations   _l_: Link scene locations
_i_: Add IDs to locations  _L_: Link all scenes
_u_: Update link names     _U_: Update all link names
_s_: Setup linking system  _q_: Back to main menu
"
  ("c" writing/set-scene-locations "set locations")
  ("l" writing/link-scene-locations "link scene")
  ("L" writing/link-all-scene-locations "link all")
  ("i" writing/add-location-ids "add IDs")
  ("u" writing/update-location-link-names "update names")
  ("U" writing/update-all-location-link-names "update all names")
  ("s" writing/setup-location-links "setup system")
  ("q" hydra-writing/body "back")
  ("Q" nil "quit"))

;;;###autoload (autoload 'hydra-writing-plot-threads/body "ui/writing-hydra" nil t)
(defhydra hydra-writing-plot-threads (:color blue :hint nil)
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
  ("p" writing/set-scene-plot-threads "set plot threads")
  ("j" writing/jump-to-plot-thread "jump to thread")
  ("l" writing/link-scene-plot-threads "link scene")
  ("L" writing/link-all-scene-plot-threads "link all")
  ("i" writing/add-plot-thread-ids "add IDs")
  ("u" writing/update-plot-link-names "update names")
  ("U" writing/update-all-plot-link-names "update all names")
  ("s" writing/setup-plot-thread-links "setup system")
  ("t" (lambda () (interactive) (message "Insert '#+BEGIN: plot-thread-timeline' then press C-c C-c")) "timeline")
  ("r" writing/plot-thread-report "health report")
  ("S" writing/plot-thread-stats "statistics")
  ("q" hydra-writing/body "back")
  ("Q" nil "quit"))

;;;###autoload (autoload 'hydra-writing/body "ui/writing-hydra" nil t)
(defhydra hydra-writing (:color blue :hint nil)
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
  ("s" writing-insert-scene "insert scene")
  ("c" writing-insert-chapter "insert chapter")
  ("o" writing-open-project-file "open project file")

  ;; Modes
  ("m" my-writing-env-mode "writing mode")
  ("p" project-writing-mode "project mode")
  ("f" my-writing-env-mode-focus "focus mode")
  ("e" writing/editing-mode "editing mode")

  ;; Capture functions
  ("n" writing/capture-to-file "capture note")
  ("h" writing/capture-character "capture character")
  ("l" writing/capture-location "capture location")
  ("b" writing/capture-object "capture object")
  ("t" writing/capture-timeline "capture timeline event")
  ("g" writing/capture-plot-thread "capture plot thread")

  ;; Tools
  ("w" org-context-count-words "count words")
  ("r" org-tracktable-write "track table")
  ("a" writing/ews-org-count-words "add word properties")
  ("d" writing/rae-api-lookup "RAE dictionary")
  ("y" writing/sinonimo "synonyms")

  ;; Search (numbered for consistency)
  ("1" writing/org-find-pov "find by POV")
  ("2" writing/org-find-character "find by character")
  ("3" writing/org-find-plot "find by plot")
  ("4" writing/org-find-location "find by location")
  ("5" writing/search-todos-recursive "find TODOs")

  ;; Manage (linking submenus - capitals only)
  ("C" hydra-writing-characters/body "character links")
  ("L" hydra-writing-locations/body "location links")
  ("P" hydra-writing-plot-threads/body "plot thread links")
  ("U" writing/update-all-link-names "update all link names")

  ;; Exit
  ("q" nil "quit"))

(provide 'writing-hydra)

;;; writing-hydra.el ends here
