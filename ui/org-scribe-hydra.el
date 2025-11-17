;;; org-scribe-hydra.el --- Hydra menu for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unified hydra menu providing quick access to all writing functions.

;;; Code:

(require 'hydra)
(require 'org-scribe-core)  ; Needed for org-scribe-project-root
(require 'org-scribe-i18n)

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

;;; Helper functions for building i18n menu text

(defun org-scribe-hydra--char-menu-text ()
  "Build character linking submenu text with i18n."
  (format "
^%s^
^^^^^^^^------------------------------------------------------------
_p_: %s      _l_: %s
_c_: %s   _L_: %s
_j_: %s       _i_: %s
_u_: %s      _U_: %s
_s_: %s   _q_: %s
"
          (org-scribe-i18n hydra-char-title)
          (org-scribe-i18n hydra-char-set-pov)
          (org-scribe-i18n hydra-char-link-scene)
          (org-scribe-i18n hydra-char-set-characters)
          (org-scribe-i18n hydra-char-link-all)
          (org-scribe-i18n hydra-char-jump-pov)
          (org-scribe-i18n hydra-char-add-ids)
          (org-scribe-i18n hydra-char-update-names)
          (org-scribe-i18n hydra-char-update-all-names)
          (org-scribe-i18n hydra-char-setup)
          (org-scribe-i18n hydra-back)))

(defun org-scribe-hydra--loc-menu-text ()
  "Build location linking submenu text with i18n."
  (format "
^%s^
^^^^^^^^------------------------------------------------------------
_c_: %s   _l_: %s
_i_: %s  _L_: %s
_u_: %s     _U_: %s
_s_: %s  _q_: %s
"
          (org-scribe-i18n hydra-loc-title)
          (org-scribe-i18n hydra-loc-set-locations)
          (org-scribe-i18n hydra-loc-link-scene)
          (org-scribe-i18n hydra-loc-add-ids)
          (org-scribe-i18n hydra-loc-link-all)
          (org-scribe-i18n hydra-loc-update-names)
          (org-scribe-i18n hydra-loc-update-all-names)
          (org-scribe-i18n hydra-loc-setup)
          (org-scribe-i18n hydra-back)))

(defun org-scribe-hydra--plot-menu-text ()
  "Build plot thread linking submenu text with i18n."
  (format "
^%s^       ^%s^
^^^^^^^^------------------------------------------------------------
_p_: %s   _t_: %s
_j_: %s      _r_: %s
_l_: %s  _S_: %s
_L_: %s          _i_: %s
_u_: %s        _U_: %s
_s_: %s     _q_: %s
"
          (org-scribe-i18n hydra-plot-title)
          (org-scribe-i18n hydra-plot-analysis)
          (org-scribe-i18n hydra-plot-set-threads)
          (org-scribe-i18n hydra-plot-timeline)
          (org-scribe-i18n hydra-plot-jump)
          (org-scribe-i18n hydra-plot-report)
          (org-scribe-i18n hydra-plot-link-scene)
          (org-scribe-i18n hydra-plot-stats)
          (org-scribe-i18n hydra-plot-link-all)
          (org-scribe-i18n hydra-plot-add-ids)
          (org-scribe-i18n hydra-plot-update-names)
          (org-scribe-i18n hydra-plot-update-all-names)
          (org-scribe-i18n hydra-plot-setup)
          (org-scribe-i18n hydra-back)))

(defun org-scribe-hydra--main-menu-text ()
  "Build main hydra menu text with i18n."
  (format "
^%s^           ^%s^            ^%s^          ^%s^            ^%s^           ^%s^
^^^^^^^^------------------------------------------------------------------------------------------------------------------
_s_: %s         _m_: %s  _n_: %s          _w_: %s   _1_: %s           _C_: %s
_c_: %s       _p_: %s  _h_: %s     _r_: %s   _2_: %s     _L_: %s
_o_: %s     _f_: %s    _l_: %s      _a_: %s  _3_: %s          _P_: %s
                 _e_: %s  _b_: %s        _d_: %s    _4_: %s      _U_: %s
                                  _t_: %s      _y_: %s      _5_: %s
                                  _g_: %s                                         _q_: %s
"
          (org-scribe-i18n hydra-section-insert)
          (org-scribe-i18n hydra-section-modes)
          (org-scribe-i18n hydra-section-capture)
          (org-scribe-i18n hydra-section-tools)
          (org-scribe-i18n hydra-section-search)
          (org-scribe-i18n hydra-section-manage)
          ;; Insert
          (org-scribe-i18n hydra-insert-scene)
          (org-scribe-i18n hydra-mode-write)
          (org-scribe-i18n hydra-capture-note)
          (org-scribe-i18n hydra-tool-wordcount)
          (org-scribe-i18n hydra-search-pov)
          (org-scribe-i18n hydra-manage-characters)
          ;; Row 2
          (org-scribe-i18n hydra-insert-chapter)
          (org-scribe-i18n hydra-mode-project)
          (org-scribe-i18n hydra-capture-character)
          (org-scribe-i18n hydra-tool-track)
          (org-scribe-i18n hydra-search-character)
          (org-scribe-i18n hydra-manage-locations)
          ;; Row 3
          (org-scribe-i18n hydra-insert-open-file)
          (org-scribe-i18n hydra-mode-focus)
          (org-scribe-i18n hydra-capture-location)
          (org-scribe-i18n hydra-tool-add-wc)
          (org-scribe-i18n hydra-search-plot)
          (org-scribe-i18n hydra-manage-plot-threads)
          ;; Row 4
          (org-scribe-i18n hydra-mode-editing)
          (org-scribe-i18n hydra-capture-object)
          (org-scribe-i18n hydra-tool-dictionary)
          (org-scribe-i18n hydra-search-location)
          (org-scribe-i18n hydra-manage-update-links)
          ;; Row 5
          (org-scribe-i18n hydra-capture-timeline)
          (org-scribe-i18n hydra-tool-synonyms)
          (org-scribe-i18n hydra-search-todos)
          ;; Row 6
          (org-scribe-i18n hydra-capture-plot)
          (org-scribe-i18n hydra-quit)))

;;;###autoload (autoload 'hydra-org-scribe-characters/body "ui/org-scribe-hydra" nil t)
(defhydra hydra-org-scribe-characters (:color blue :hint nil
                                        :body-pre (message "%s" (org-scribe-hydra--char-menu-text)))
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
(defhydra hydra-org-scribe-locations (:color blue :hint nil
                                       :body-pre (message "%s" (org-scribe-hydra--loc-menu-text)))
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
(defhydra hydra-org-scribe-plot-threads (:color blue :hint nil
                                          :body-pre (message "%s" (org-scribe-hydra--plot-menu-text)))
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
(defhydra hydra-org-scribe (:color blue :hint nil
                             :body-pre (message "%s" (org-scribe-hydra--main-menu-text)))
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
