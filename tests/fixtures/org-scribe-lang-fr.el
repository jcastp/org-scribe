;;; org-scribe-lang-fr.el --- Test fixture: a minimal French language pack -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A test fixture, not a shipped language pack -- there is no real French
;; template set, and this file is never `require'd by `org-scribe.el' or
;; listed in any module load order.  It exists solely for
;; `tests/test-lang.el''s `test-lang-third-language-resolves', which is
;; the acceptance test for the whole language-pack refactor described in
;; "i18n-extended.org": if a third language, registered from nothing but
;; this file plus a couple of pack accessor calls, resolves a real
;; project tree end to end, the seams are real.
;;
;; Deliberately minimal -- only the keys that test actually exercises
;; (`:files' manuscript-novel and characters, `:dirs' objects, `:headings'
;; characters, and two `:messages' entries), not a full pack shaped like
;; `lang/org-scribe-lang-en.el'.  Unlike the shipped packs, this file does
;; NOT self-register on `require': the test registers it explicitly, and
;; unregisters it again in an `unwind-protect', so no other test in the
;; suite -- run before or after this one, in the same Emacs process --
;; ever sees a "fr" entry in the registry.

;;; Code:

(defconst org-scribe-lang-fr
  (list
   :code "fr"
   :name "Français"
   :files
   '((manuscript-novel . "roman.org")
     (characters       . "objets/personnages.org"))
   :dirs
   '((objects . "objets"))
   :headings
   '((characters . "Personnages"))
   :messages
   '((default-scene-name . "Nouvelle scène")
     (default-chapter-name . "Nouveau chapitre")))
  "A minimal French pack, for tests only.  See this file's Commentary.")

(provide 'org-scribe-lang-fr)

;;; org-scribe-lang-fr.el ends here
