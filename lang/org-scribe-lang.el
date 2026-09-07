;;; org-scribe-lang.el --- Language pack registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A language pack is a single plist (see `lang/org-scribe-lang-en.el' and
;; `lang/org-scribe-lang-es.el' for the shipped examples) holding every
;; string that varies by language: project file names, directory names,
;; section headings, scene property names, *EDIT* category spellings,
;; Starting Gate label substrings, template set names, plotting-method
;; labels, the spelling-checker dictionary code, and the full set of
;; user-facing messages.
;;
;; This file is the registry and the only public API for reading a pack.
;; Nothing outside `lang/' should ever read a pack plist directly with
;; `plist-get' — every read goes through one of the accessors below, so
;; that adding a language means adding a pack file and nothing else (see
;; "i18n-extended.org", section "The acceptance criterion").
;;
;; Two accessor families, matching the read/write asymmetry the package's
;; older alias tables already got right:
;;
;;   - `org-scribe-lang-file' / `-dir' / `-heading' / `-property' /
;;     `-template' / `-message' each resolve for ONE language (the
;;     project's, by default) -- this is what WRITING a new file, heading,
;;     property or message must use, because writing has to commit to
;;     exactly one spelling.
;;
;;   - `org-scribe-lang-all' returns every registered language's value for
;;     a key, project language first -- this is what READING must use, so
;;     a file or property written in a different language than the
;;     project's current setting still resolves.
;;
;; This file loads before `core/org-scribe-core.el' and
;; `core/org-scribe-config.el' (see org-scribe.el's module list), so it
;; must not `require' either -- `org-scribe-project-language' is reached
;; through `fboundp', not a hard dependency, exactly like
;; `core/org-scribe-messages.el' already does for
;; `org-scribe-message-language'.

;;; Code:

(defvar org-scribe--language-packs nil
  "Alist of LANGUAGE-SYMBOL -> pack plist, in registration order.
Populated by `org-scribe-register-language'.  Each pack plist is
documented in `lang/org-scribe-lang-en.el'.")

;; `org-scribe-fallback-language' is a `defcustom' in
;; `core/org-scribe-config.el' (per the project's convention that all
;; defcustoms live there), but this file loads before config.el in the
;; module load order (see org-scribe.el) and the accessors below need the
;; variable at load time.  A plain `defvar' here supplies the default
;; ('en) until config.el's `defcustom' runs and adds the customize
;; metadata on top -- `defcustom', like `defvar', does not override an
;; already-bound value, so this is safe and the two forms cooperate
;; rather than conflict.  Mirrors the identical arrangement for
;; `org-scribe-message-language' in `core/org-scribe-messages.el'.
(defvar org-scribe-fallback-language 'en
  "Language to fall back to when a message key is missing from the
requested language's pack.  The real `defcustom' lives in
`core/org-scribe-config.el'; see this file's Commentary for why a
forward `defvar' is needed here too.

Only `org-scribe-lang-message' consults this.  A missing STRUCTURAL key
\(a file name, a directory name, a heading, a property, a template set\)
is a packaging bug in that language's pack, not a translation gap, and
signals rather than silently falling back to another language's
spelling -- see `org-scribe-lang-file' and friends.")

;; `org-scribe-project-language' lives in `core/org-scribe-core.el', which
;; loads after this file.  Declared so the byte-compiler does not warn
;; about the `fboundp'-guarded call below.
(declare-function org-scribe-project-language "org-scribe-core")

;;; Registration

(defun org-scribe-register-language (symbol pack)
  "Register PACK under language SYMBOL, replacing any existing registration.
PACK is a plist; see `lang/org-scribe-lang-en.el' for its shape.

Re-registering an already-known SYMBOL replaces its pack in place,
keeping that language's position in `org-scribe-languages'.  A first
registration is appended, not prepended: `(setf (alist-get ...))'
conses new keys onto the front of the list, which would silently
reverse registration order (the second language registered would sort
before the first) -- exactly the kind of positional assumption this
whole package has been bitten by before."
  (if (assq symbol org-scribe--language-packs)
      (setf (alist-get symbol org-scribe--language-packs) pack)
    (setq org-scribe--language-packs
          (append org-scribe--language-packs (list (cons symbol pack))))))

(defun org-scribe-languages ()
  "Return the list of registered language symbols, in registration order."
  (mapcar #'car org-scribe--language-packs))

(defun org-scribe--lang-default ()
  "Return the language to use when no LANGUAGE argument is given.
The current project's language when `org-scribe-project-language' is
loaded and a project can be found; `org-scribe-fallback-language'
otherwise.  Never calls `org-scribe-project-language' unless it is
already loaded -- this file loads before core/org-scribe-core.el and
must not force it to load."
  (if (fboundp 'org-scribe-project-language)
      (org-scribe-project-language)
    org-scribe-fallback-language))

;;; Pack access

(defun org-scribe-lang-pack (&optional language)
  "Return the registered pack plist for LANGUAGE.
LANGUAGE defaults to `org-scribe--lang-default'.  Signals an error if
LANGUAGE is not a registered language."
  (let ((language (or language (org-scribe--lang-default))))
    (or (alist-get language org-scribe--language-packs)
        (error "org-scribe: no language pack registered for `%s'" language))))

(defun org-scribe--lang-struct (section key language)
  "Return the LANGUAGE pack's SECTION value for KEY, signalling if absent.
SECTION is a pack plist key such as `:files' or `:headings'; KEY is a
key within that section's own alist.  Used by every single-language
structural accessor (`org-scribe-lang-file' and friends) -- a missing
structural key is a packaging bug in that language's pack, not a
translation gap, so this signals rather than falling back to another
language (see `org-scribe-fallback-language')."
  (let* ((pack (org-scribe-lang-pack language))
         (table (plist-get pack section))
         (cell (assq key table)))
    (if cell
        (cdr cell)
      (error "org-scribe: language pack `%s' has no %s entry for `%s'"
             (or language (org-scribe--lang-default)) section key))))

(defun org-scribe-lang-file (key &optional language)
  "Return the project-relative file path for concept KEY, in LANGUAGE.
KEY is a symbol such as `characters' or `manuscript-novel' (see the
`:files' section of `lang/org-scribe-lang-en.el' for the full list).
LANGUAGE defaults to `org-scribe--lang-default'.  Signals an error if
KEY is not a known concept in that language's pack -- writing a file
must commit to exactly one language's spelling; see
`org-scribe-lang-all' for the read-tolerant equivalent."
  (org-scribe--lang-struct :files key language))

(defun org-scribe-lang-dir (key &optional language)
  "Return the directory name for concept KEY, in LANGUAGE.
KEY is a symbol such as `objects' or `notes' (see the `:dirs' section
of `lang/org-scribe-lang-en.el').  LANGUAGE defaults to
`org-scribe--lang-default'.  Signals an error if KEY is unknown."
  (org-scribe--lang-struct :dirs key language))

(defun org-scribe-lang-heading (key &optional language)
  "Return the level-1 section heading text for concept KEY, in LANGUAGE.
KEY is a symbol such as `characters' or `plot-threads' (see the
`:headings' section of `lang/org-scribe-lang-en.el').  LANGUAGE
defaults to `org-scribe--lang-default'.  Signals an error if KEY is
unknown."
  (org-scribe--lang-struct :headings key language))

(defun org-scribe-lang-property (key &optional language)
  "Return the literal Org property name for scene property KEY, in LANGUAGE.
KEY is a symbol such as `characters' or `plot-point' (see the
`:properties' section of `lang/org-scribe-lang-en.el').  LANGUAGE
defaults to `org-scribe--lang-default'.  Signals an error if KEY is
unknown."
  (org-scribe--lang-struct :properties key language))

(defun org-scribe-lang-template (kind &optional language)
  "Return the template set directory name for KIND, in LANGUAGE.
KIND is `novel' or `short-story' (see the `:templates' section of
`lang/org-scribe-lang-en.el').  LANGUAGE defaults to
`org-scribe--lang-default'.  Signals an error if KIND is unknown."
  (org-scribe--lang-struct :templates kind language))

(defun org-scribe-lang-message (key &optional language)
  "Return the user-facing message template for KEY, in LANGUAGE.
KEY is a message key such as `default-scene-name' (see the
`:messages' section of `lang/org-scribe-lang-en.el').  LANGUAGE
defaults to `org-scribe--lang-default'.

Unlike the other single-language accessors, this never signals: it
falls back to `org-scribe-fallback-language' when KEY is missing from
LANGUAGE's pack, and to KEY's own symbol name as a last resort.  A
missing message is a translation gap, not a packaging bug, and must
never turn into a Lisp error surfaced to a writer mid-sentence."
  (let* ((language (or language (org-scribe--lang-default)))
         (pack (ignore-errors (org-scribe-lang-pack language)))
         (found (and pack (assq key (plist-get pack :messages)))))
    (or (cdr found)
        (and (not (eq language org-scribe-fallback-language))
             (let* ((fallback-pack (ignore-errors
                                     (org-scribe-lang-pack org-scribe-fallback-language)))
                    (fallback-found (and fallback-pack
                                         (assq key (plist-get fallback-pack :messages)))))
               (cdr fallback-found)))
        (symbol-name key))))

;;; Cross-language access (read-tolerant)

(defconst org-scribe--lang-variants-sections
  '((:headings . :heading-variants))
  "Pack plist section -> its extra-spellings section, if any.
`:headings' is currently the only section that ships variant spellings
\(e.g. accented and unaccented Spanish headings\); a future section that
needs the same treatment adds an entry here rather than `org-scribe-lang-all'
guessing a name from SECTION.")

(defun org-scribe-lang-all (section key)
  "Return every registered language's value for KEY in SECTION.
SECTION is a pack plist key such as `:files', `:headings', or
`:properties'.  The project language's value comes first (see
`org-scribe--lang-default'), followed by every other registered
language's value, followed by each language's variant spellings for
KEY (see `org-scribe--lang-variants-sections'), if any.  Duplicates
are dropped, preserving first occurrence.

Returns nil if KEY is not found in SECTION for any registered
language, rather than signalling -- callers of this function are the
read-tolerant side of the read/write split (see this file's
Commentary) and already fall back to KEY itself when nil comes back,
exactly as the alias tables this function replaces always did.

This is the ONLY accessor that reads across every registered language
at once; `org-scribe-lang-file' and friends read one language only,
because writing a file, heading or property must commit to exactly
one spelling."
  (let* ((default-lang (org-scribe--lang-default))
         (variants-section (alist-get section org-scribe--lang-variants-sections))
         (ordered-langs (cons default-lang
                              (remq default-lang (org-scribe-languages))))
         (values nil))
    (dolist (lang ordered-langs)
      (let* ((pack (alist-get lang org-scribe--language-packs))
             (cell (and pack (assq key (plist-get pack section)))))
        (when cell
          (push (cdr cell) values))))
    (when variants-section
      (dolist (lang ordered-langs)
        (let* ((pack (alist-get lang org-scribe--language-packs))
               (cell (and pack (assq key (plist-get pack variants-section)))))
          (when cell
            (dolist (variant (cdr cell))
              (push variant values))))))
    (delete-dups (nreverse values))))

(provide 'org-scribe-lang)

;;; org-scribe-lang.el ends here
