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
(declare-function org-context-count-words "org-context-extended")
(declare-function writing/ews-org-count-words "counting/writing-wordcount")
(declare-function org-tracktable-write "org-tracktable")
(declare-function writing/org-find-pov "search/writing-search")
(declare-function writing/org-find-character "search/writing-search")
(declare-function writing/org-find-plot "search/writing-search")
(declare-function writing/org-find-location "search/writing-search")
(declare-function writing/search-todos-recursive "search/writing-search")
(declare-function writing-create-project "templates/writing-project")
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
_s_: Setup linking system   _q_: Back to main menu
"
  ("p" writing/set-pov-character "set PoV")
  ("c" writing/set-scene-characters "set characters")
  ("j" writing/jump-to-pov-character "jump to PoV")
  ("l" writing/link-scene-characters "link scene")
  ("L" writing/link-all-scene-characters "link all")
  ("i" writing/add-character-ids "add IDs")
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
_s_: Setup linking system
_q_: Back to main menu
"
  ("c" writing/set-scene-locations "set locations")
  ("l" writing/link-scene-locations "link scene")
  ("L" writing/link-all-scene-locations "link all")
  ("i" writing/add-location-ids "add IDs")
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
_s_: Setup linking system     _q_: Back to main menu
"
  ("p" writing/set-scene-plot-threads "set plot threads")
  ("j" writing/jump-to-plot-thread "jump to thread")
  ("l" writing/link-scene-plot-threads "link scene")
  ("L" writing/link-all-scene-plot-threads "link all")
  ("i" writing/add-plot-thread-ids "add IDs")
  ("s" writing/setup-plot-thread-links "setup system")
  ("t" (lambda () (interactive) (message "Insert '#+BEGIN: plot-thread-timeline' then press C-c C-c")) "timeline")
  ("r" writing/plot-thread-report "health report")
  ("S" writing/plot-thread-stats "statistics")
  ("q" hydra-writing/body "back")
  ("Q" nil "quit"))

;;;###autoload (autoload 'hydra-writing/body "ui/writing-hydra" nil t)
(defhydra hydra-writing (:color blue :hint nil)
  "
^Project^          ^Modes^            ^Capture^             ^Dictionary^          ^Count^            ^Search^
^^^^^^^^------------------------------------------------------------------------------------------------------------------
_C_: New chapter   _p_: Project mode  _c_: Note             _r_: RAE dictionary   _n_: Count words   _u_: Find POV
_S_: New scene     _w_: Writing mode  _h_: Character        _s_: Synonyms         _m_: Add WC props  _i_: Find character
_O_: Open file     _f_: Focus mode    _l_: Location         _d_: Thesaurus        _t_: Track table   _o_: Find plot
_P_: Char links    _e_: Editing mode  _j_: Object                                                _y_: Find location
_L_: Loc links                      _k_: Timeline                                              _a_: Find TODOs
_T_: Plot threads
                                                                               _q_: Quit
"
  ;; Project management
  ;;("P" writing-create-project "new novel project")
  ;;("T" writing-create-short-story-project "new short story project")
  ("C" writing-insert-chapter "insert chapter")
  ("S" writing-insert-scene "insert scene")
  ("O" writing-open-project-file "open project file")

  ;; Modes
  ("p" project-writing-mode "project mode")
  ("w" my-writing-env-mode "writing mode")
  ("f" my-writing-env-mode-focus "focus mode")
  ("e" writing/editing-mode "editing mode")

  ;; Capture functions
  ("c" writing/capture-to-file "capture note")
  ("h" writing/capture-character "capture character")
  ("l" writing/capture-location "capture location")
  ("j" writing/capture-object "capture object")
  ("k" writing/capture-timeline "capture timeline event")

  ;; Dictionary tools
  ("r" writing/rae-api-lookup "RAE dictionary")
  ("s" writing/sinonimo "synonyms")
  ("d" powerthesaurus-lookup-dwim "thesaurus")

  ;; Count words
  ("n" org-context-count-words "count words")
  ("m" writing/ews-org-count-words "add word properties")
  ("t" org-tracktable-write "track table")

  ;; Searches
  ("u" writing/org-find-pov "find POV")
  ("i" writing/org-find-character "find character")
  ("o" writing/org-find-plot "find plot")
  ("y" writing/org-find-location "find location")
  ("a" writing/search-todos-recursive "find TODOs")

  ;; Properties linking
  ("P" hydra-writing-characters/body "character links")
  ("L" hydra-writing-locations/body "location links")
  ("T" hydra-writing-plot-threads/body "plot thread links")

  ;; Exit
  ("q" nil "quit"))

(provide 'writing-hydra)

;;; writing-hydra.el ends here
