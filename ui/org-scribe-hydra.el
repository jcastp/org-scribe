;;; org-scribe-hydra.el --- Hydra menu for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unified hydra menu providing quick access to all writing functions.
;;
;; The menu is organised in two tiers so the day-to-day writing surface
;; stays small:
;;
;;   `hydra-org-scribe/body' (F8 F8) — the main menu.  Holds only frequent
;;   actions: insert, writing modes, *tagging the current scene* with
;;   characters/locations/plot (the everyday linking verbs), word counting,
;;   search, capture-note, and health.
;;
;;   `hydra-org-scribe-capture/body' (k) — capture new entities.
;;
;;   `hydra-org-scribe-links/body' (L) — the linking *upkeep* menu.  The
;;   headline command here is `org-scribe-relink-project', a single
;;   registry-driven pass that adds missing IDs, links plain names, and
;;   refreshes display names across every entity type — replacing the old
;;   per-entity add-ids / link / update commands.  Set-up, relationships,
;;   plot analysis, and display toggles live here too.

;;; Code:

(require 'hydra)

;; Declare functions from other modules
(declare-function org-scribe-workspace "modes/org-scribe-modes")
(declare-function org-scribe-rae-api-lookup "language/org-scribe-dictionary")
(declare-function org-scribe-sinonimo "language/org-scribe-dictionary")
(declare-function powerthesaurus-lookup-dwim "powerthesaurus")
(declare-function org-scribe-capture-to-file "capture/org-scribe-capture")
(declare-function org-scribe-capture-character "capture/org-scribe-capture")
(declare-function org-scribe-capture-location "capture/org-scribe-capture")
(declare-function org-scribe-capture-object "capture/org-scribe-capture")
(declare-function org-scribe-capture-timeline "capture/org-scribe-capture")
(declare-function org-scribe-capture-plot-thread "capture/org-scribe-capture")
(declare-function org-scribe-wordcount "counting/org-scribe-wordcount")
(declare-function org-tracktable-write "org-tracktable")
(declare-function org-scribe-org-find-pov "search/org-scribe-search")
(declare-function org-scribe-org-find-character "search/org-scribe-search")
(declare-function org-scribe-org-find-plot "search/org-scribe-search")
(declare-function org-scribe-org-find-location "search/org-scribe-search")
(declare-function org-scribe-search-todos-recursive "search/org-scribe-search")
(declare-function org-scribe-search-edits-recursive "search/org-scribe-search")
(declare-function org-scribe-create-novel-project "templates/org-scribe-project")
(declare-function org-scribe-create-short-story-project "templates/org-scribe-project")
(declare-function org-scribe-insert-scene "templates/org-scribe-project")
(declare-function org-scribe-insert-chapter "templates/org-scribe-project")
(declare-function org-scribe-open-project-file "templates/org-scribe-project")
;; everyday scene-tagging verbs (promoted to the main menu)
(declare-function org-scribe-set-pov-character "linking/org-scribe-character-links")
(declare-function org-scribe-set-scene-characters "linking/org-scribe-character-links")
(declare-function org-scribe-jump-to-pov-character "linking/org-scribe-character-links")
(declare-function org-scribe-set-scene-locations "linking/org-scribe-location-links")
(declare-function org-scribe-set-scene-plot-threads "linking/org-scribe-plot-links")
(declare-function org-scribe-jump-to-plot-thread "linking/org-scribe-plot-links")
;; linking upkeep
(declare-function org-scribe-relink-project "linking/org-scribe-link-update")
(declare-function org-scribe-setup-character-links "linking/org-scribe-character-links")
(declare-function org-scribe-setup-location-links "linking/org-scribe-location-links")
(declare-function org-scribe-setup-plot-thread-links "linking/org-scribe-plot-links")
(declare-function org-scribe-plot-thread-report "linking/org-scribe-plot-links")
(declare-function org-scribe-plot-thread-stats "linking/org-scribe-plot-links")
;; character relationships
(declare-function org-scribe-add-relationship "linking/org-scribe-character-relationships")
(declare-function org-scribe-remove-relationship "linking/org-scribe-character-relationships")
(declare-function org-scribe-show-character-relationships "linking/org-scribe-character-relationships")
(declare-function org-scribe-show-all-relationships "linking/org-scribe-character-relationships")
(declare-function org-scribe-setup-character-relationships "linking/org-scribe-character-relationships")
(declare-function org-scribe-insert-relationship-block "linking/org-scribe-character-relationships")
;; display toggles
(declare-function org-scribe-column-view-toggle "linking/org-scribe-column-view")
(declare-function org-scribe-overlays-mode "linking/org-scribe-overlays")
;; project health report
(declare-function org-scribe-project-health "reporting/org-scribe-health")
;; planner submenu (optional — only present when org-scribe-planner is loaded)
(declare-function hydra-org-scribe-planner/body "planning/org-scribe-planner")

;;;###autoload (autoload 'hydra-org-scribe-capture/body "ui/org-scribe-hydra" nil t)
(defhydra hydra-org-scribe-capture (:color blue :hint nil)
  "
^Capture into project^
^^^^----------------------------------------------------------
_h_: Character   _b_: Object      _g_: Plot thread
_l_: Location    _t_: Timeline    _n_: Note

_q_: Back to main menu            _Q_: Quit
"
  ("h" org-scribe-capture-character "character")
  ("l" org-scribe-capture-location "location")
  ("b" org-scribe-capture-object "object")
  ("t" org-scribe-capture-timeline "timeline")
  ("g" org-scribe-capture-plot-thread "plot thread")
  ("n" org-scribe-capture-to-file "note")
  ("q" hydra-org-scribe/body "back")
  ("Q" nil "quit"))

;;;###autoload (autoload 'hydra-org-scribe-links/body "ui/org-scribe-hydra" nil t)
(defhydra hydra-org-scribe-links (:color blue :hint nil)
  "
^Whole project^          ^Set up linking^       ^Relationships^             ^Analysis & Display^
^^^^------------------------------------------------------------------------------------------------------------
_R_: Relink everything    _c_: Characters        _a_: Add relationship       _t_: Plot health report
_j_: Jump to plot thread  _o_: Locations         _x_: Remove relationship    _S_: Plot statistics
^^                        _p_: Plot threads      _v_: View relationships     _C_: Column view toggle
^^                        _r_: Relationships     _V_: View all relationships _T_: Tooltips toggle
^^                        ^^                     _g_: Insert graph block
_q_: Back to main menu    _Q_: Quit
"
  ;; Whole-project upkeep (the headline action)
  ("R" org-scribe-relink-project "relink project")
  ("j" org-scribe-jump-to-plot-thread "jump to plot")
  ;; One-time set-up per entity type
  ("c" org-scribe-setup-character-links "char setup")
  ("o" org-scribe-setup-location-links "loc setup")
  ("p" org-scribe-setup-plot-thread-links "plot setup")
  ("r" org-scribe-setup-character-relationships "rel setup")
  ;; Relationships
  ("a" org-scribe-add-relationship "add relationship")
  ("x" org-scribe-remove-relationship "remove relationship")
  ("v" org-scribe-show-character-relationships "view relationships")
  ("V" org-scribe-show-all-relationships "view all")
  ("g" org-scribe-insert-relationship-block "insert graph block")
  ;; Analysis and display
  ("t" org-scribe-plot-thread-report "plot report")
  ("S" org-scribe-plot-thread-stats "plot stats")
  ("C" org-scribe-column-view-toggle "column view")
  ("T" org-scribe-overlays-mode "tooltips")
  ("q" hydra-org-scribe/body "back")
  ("Q" nil "quit"))

;;;###autoload (autoload 'hydra-org-scribe/body "ui/org-scribe-hydra" nil t)
(defhydra hydra-org-scribe (:color blue :hint nil)
  "
^Insert^         ^Workspace^      ^Tag Scene^          ^Tools^             ^Search^          ^Manage^
^^^^--------------------------------------------------------------------------------------------------------------
_s_: Scene       _m_: Write       _p_: PoV             _w_: Count words    _1_: by PoV       _n_: Capture note
_c_: Chapter     _f_: Focus       _h_: Characters      _r_: Track table    _2_: Character    _k_: Capture entity…
_o_: Open file   _e_: Edit        _l_: Locations       _d_: Dictionary     _3_: Plot         _L_: Links & upkeep…
^^               _v_: Navigate    _g_: Plot threads    _y_: Synonyms       _4_: Location     _R_: Relink project
^^               _0_: Normal      _j_: Jump to PoV     _x_: Thesaurus      _5_: TODOs        _H_: Health report
^^               ^^               ^^                   ^^                  _6_: Edits        _W_: Planner…
^^               ^^               ^^                   ^^                  ^^                _q_: Quit
"
  ;; Insert (most frequent)
  ("s" org-scribe-insert-scene "insert scene")
  ("c" org-scribe-insert-chapter "insert chapter")
  ("o" org-scribe-open-project-file "open project file")

  ;; Workspace (one unified command; each key selects a named layout,
  ;; re-selecting the active one turns it off)
  ("m" (org-scribe-workspace 'write) "write")
  ("f" (org-scribe-workspace 'focus) "focus")
  ("e" (org-scribe-workspace 'edit) "edit")
  ("v" (org-scribe-workspace 'navigate) "navigate")
  ("0" (org-scribe-workspace 'normal) "normal")

  ;; Tag the current scene (everyday linking verbs — these write proper
  ;; ID links directly, so plain-name maintenance is rarely needed)
  ("p" org-scribe-set-pov-character "set PoV")
  ("h" org-scribe-set-scene-characters "set characters")
  ("l" org-scribe-set-scene-locations "set locations")
  ("g" org-scribe-set-scene-plot-threads "set plot threads")
  ("j" org-scribe-jump-to-pov-character "jump to PoV")

  ;; Tools
  ;; "w" is the unified word-count dispatcher: plain = count now,
  ;; C-u = refresh all WORDCOUNT properties, C-u C-u = refresh scenes.
  ("w" org-scribe-wordcount "count words")
  ("r" org-tracktable-write "track table")
  ("d" org-scribe-rae-api-lookup "RAE dictionary")
  ("y" org-scribe-sinonimo "synonyms")
  ("x" powerthesaurus-lookup-dwim "thesaurus")

  ;; Search
  ("1" org-scribe-org-find-pov "find by PoV")
  ("2" org-scribe-org-find-character "find by character")
  ("3" org-scribe-org-find-plot "find by plot")
  ("4" org-scribe-org-find-location "find by location")
  ("5" org-scribe-search-todos-recursive "find TODOs")
  ("6" org-scribe-search-edits-recursive "find edits and notes")

  ;; Manage
  ("n" org-scribe-capture-to-file "capture note")
  ("k" hydra-org-scribe-capture/body "capture entity")
  ("L" hydra-org-scribe-links/body "links & upkeep")
  ("R" org-scribe-relink-project "relink project")
  ("H" org-scribe-project-health "project health report")
  ("W" (lambda ()
         (interactive)
         (require 'org-scribe-planner)
         (hydra-org-scribe-planner/body))
   "planner")

  ;; Exit
  ("q" nil "quit"))

(provide 'org-scribe-hydra)

;;; org-scribe-hydra.el ends here
