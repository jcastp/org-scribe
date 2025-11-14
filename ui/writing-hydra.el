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

;;;###autoload (autoload 'hydra-writing/body "ui/writing-hydra" nil t)
(defhydra hydra-writing (:color blue :hint nil)
  "
^Project^          ^Modes^            ^Capture^             ^Dictionary^          ^Count^            ^Search^
^^^^^^^^------------------------------------------------------------------------------------------------------------------
_P_: New novel     _p_: Project mode  _c_: Note             _r_: RAE dictionary   _n_: Count words   _u_: Find POV
_T_: New story     _w_: Writing mode  _h_: Character        _s_: Synonyms         _m_: Add WC props  _i_: Find character
_C_: New chapter   _f_: Focus mode    _l_: Location         _d_: Thesaurus        _t_: Track table   _o_: Find plot
_S_: New scene     _e_: Editing mode  _j_: Object                                                _y_: Find location
_O_: Open file                      _k_: Timeline                                              _a_: Find TODOs
_q_: Quit
"
  ;; Project management
  ("P" writing-create-project "new novel project")
  ("T" writing-create-short-story-project "new short story project")
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

  ;; Exit
  ("q" nil "quit"))

(provide 'writing-hydra)

;;; writing-hydra.el ends here
