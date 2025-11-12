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
(declare-function org-context-count-words "org-context-extended")
(declare-function writing/ews-org-count-words "counting/writing-wordcount")
(declare-function org-tracktable-write "org-tracktable")
(declare-function writing/org-find-pov "search/writing-search")
(declare-function writing/org-find-character "search/writing-search")
(declare-function writing/org-find-plot "search/writing-search")
(declare-function writing/org-find-location "search/writing-search")
(declare-function writing/search-todos-recursive "search/writing-search")

;;;###autoload (autoload 'hydra-writing/body "ui/writing-hydra" nil t)
(defhydra hydra-writing (:color blue :hint nil)
  "
^Modes^                   ^Writing Tools^       ^Count words^         ^Searches^
^^^^^^^^------------------------------------------------------------------------
_p_: Project mode         _r_: RAE dictionary   _n_: Count words      _u_: Find POV
_w_: Writing mode         _s_: Synonyms         _m_: EWS Word count   _i_: Find character
_f_: Focus writing mode   _d_: Thesaurus        _t_: Track table      _o_: Find plot
_e_: Editing mode         _c_: writing note                           _y_: Find location
                                                                      _a_: Find TODOs
_q_: Quit
"
  ;; Modes
  ("p" project-writing-mode "project mode")
  ("w" my-writing-env-mode "writing mode")
  ("f" my-writing-env-mode-focus "focus mode")
  ("e" writing/editing-mode "editing mode")

  ;; Writing tools
  ("r" writing/rae-api-lookup "RAE dictionary")
  ("s" writing/sinonimo "synonyms")
  ;; Note: writing/translate function doesn't exist in the codebase
  ;; ("l" writing/translate "translate")
  ("d" powerthesaurus-lookup-dwim "thesaurus")
  ("c" writing/capture-to-file "capture writing note")

  ;; Count words
  ("n" org-context-count-words "count words")
  ("m" writing/ews-org-count-words "add word properties")
  ("t" org-tracktable-write "track table")

  ;; Novel searches
  ("u" writing/org-find-pov "find POV")
  ("i" writing/org-find-character "find character")
  ("o" writing/org-find-plot "find plot")
  ("y" writing/org-find-location "find location")
  ("a" writing/search-todos-recursive "find TODOs")

  ;; Exit
  ("q" nil "quit"))

(provide 'writing-hydra)

;;; writing-hydra.el ends here
