;;; org-scribe-tempel.eld --- Tempel snippets for org-scribe -*- mode: lisp-data; -*-

;; Inline edit markers for the org-scribe revision workflow.
;;
;; Add this file to `tempel-path' to use them:
;;
;;   (with-eval-after-load 'tempel
;;     (add-to-list 'tempel-path (org-scribe-tempel-snippets-file)))
;;
;; or call `org-scribe-tempel-setup', which does the same thing.
;;
;; `tempel-path' takes a list of files, so adding this one never touches
;; or overrides your own templates file.  If you already define `edit' or
;; `note' snippets of your own, whichever file comes first in
;; `tempel-path' wins — org-scribe does not try to detect or resolve
;; that.
;;
;; Both snippets wrap the marker in a comment block, which is what keeps
;; markers out of every export backend and out of word counts.  See the
;; "Marker Convention" section of manual lesson 14.
;;
;; There is deliberately no `summary' snippet here.  *SUMMARY* is a note
;; about a scene not yet written: it belongs under a TODO heading and is
;; found with the TODO search, not with the edit index.  Keeping it out
;; of the shipped set keeps the two kinds of note distinct.

org-mode

;; *EDIT*: <category> - <text>
;;
;; The category list comes from `org-scribe-edit-categories', so
;; customizing that variable updates both this prompt and the grouping in
;; `org-scribe-search-edits' — the prompt and the index cannot drift
;; apart.  The prompt accepts anything you type; an unknown or empty
;; category simply groups the marker under "other".
(edit
 "#+begin_comment" n
 "*EDIT*: " (p (concat (completing-read "Type: " org-scribe-edit-categories)
                       " - ")
               type)
 p n
 "#+end_comment" n
 :ann "edit marker"
 :doc "Mark existing prose as needing a change, by category")

;; *NOTE*: <text>
;;
;; No category: a note is a reminder, not a classified piece of work.
(note
 "#+begin_comment" n
 "*NOTE*: " p n
 "#+end_comment" n
 :ann "note marker"
 :doc "Leave a reminder about existing prose")
