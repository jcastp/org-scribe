;;; org-scribe-messages.el --- Centralized user-facing messages -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Centralized repository for all user-facing strings in org-scribe,
;; in two languages: English (`org-scribe-messages-en') and Spanish
;; (`org-scribe-messages-es').  Their DATA now lives in the registered
;; language packs (`lang/org-scribe-lang-en.el', `lang/org-scribe-lang-es.el')
;; -- these two variables are a one-line derivation from those packs'
;; `:messages' section, kept as directly-bound variables (see their own
;; docstrings for why) rather than replaced outright by
;; `org-scribe-lang-message'.  `org-scribe-msg' looks up the key in
;; whichever language `org-scribe-message-language' selects, falls back
;; to English when the key is missing from that language's alist, and
;; falls back to the symbol's own name as a last resort — it never
;; signals an error for a genuinely unregistered key.
;;
;; Every message key must be present in BOTH alists; `tests/test-messages.el'
;; enforces this (key-set parity, and matching counts of %s/%d format
;; specifiers between the two languages for the same key).  Since both
;; alists are derived from the packs, keeping this parity is really a
;; matter of keeping the packs' own `:messages' sections in parity --
;; see `lang/org-scribe-lang.el'.
;;
;; Usage:
;;   (org-scribe-msg 'default-scene-name)
;;   => "New scene"                (English, the default)
;;
;;   (let ((org-scribe-message-language 'es))
;;     (org-scribe-msg 'default-scene-name))
;;   => "Escena nueva"
;;
;;   (org-scribe-msg 'msg-inserted-link "Alex")
;;   => "Inserted link to Alex"

;;; Code:

;; `org-scribe-messages-en'/`-es' below derive from the registered
;; language packs at THIS file's own load time, so the packs must be
;; loaded -- and self-registered, which each does on `require' -- before
;; that point.  Required directly here, not left to `org-scribe.el',
;; because several test files `require' this file standalone without
;; ever loading that one (see `lang/org-scribe-lang-en.el''s Commentary).
(require 'org-scribe-lang)
(require 'org-scribe-lang-en)
(require 'org-scribe-lang-es)

;; `org-scribe-message-language' is a `defcustom' in
;; core/org-scribe-config.el (per the project's convention that all
;; defcustoms live there), but this file loads before config.el in the
;; module load order (see org-scribe.el) and `org-scribe-msg' needs the
;; variable at load time.  A plain `defvar' here supplies the default
;; ('en) until config.el's `defcustom' runs and adds the customize
;; metadata on top — `defcustom', like `defvar', does not override an
;; already-bound value, so this is safe and the two forms cooperate
;; rather than conflict.
(defvar org-scribe-message-language 'en
  "Language for user-facing messages: `en' (English) or `es' (Spanish).
The real `defcustom' lives in `core/org-scribe-config.el'; see this
file's Commentary for why a forward `defvar' is needed here too.")

(defconst org-scribe-messages-en
  (plist-get (org-scribe-lang-pack 'en) :messages)
  "English messages, derived from the `en' language pack
(`lang/org-scribe-lang-en.el').  Kept as a directly-bound variable,
rather than only reachable through `org-scribe-lang-message', because
`org-scribe-msg' below reads it (and `org-scribe-messages-es') by
name, and `tests/test-messages.el' dynamically `let'-binds
`org-scribe-messages-es' to a modified copy to test the
missing-key-falls-back-to-English path -- a binding
`org-scribe-lang-message' (which reads the registered pack, not this
variable) would never see.  See this file's Commentary and
`org-scribe-msg'.  Each entry is (KEY . MESSAGE-TEMPLATE) where
MESSAGE-TEMPLATE can include printf-style format specifiers (%s, %d,
etc.) for dynamic content.")

(defconst org-scribe-messages-es
  (plist-get (org-scribe-lang-pack 'es) :messages)
  "Spanish messages, derived from the `es' language pack
(`lang/org-scribe-lang-es.el').  See `org-scribe-messages-en'.")

(defun org-scribe-msg (key &rest args)
  "Get user-facing message for KEY and format with ARGS.

KEY is a symbol that identifies the message in the alist selected by
`org-scribe-message-language' (`org-scribe-messages-en' or
`org-scribe-messages-es').  ARGS are optional format arguments to
substitute into the message template.

Falls back to `org-scribe-messages-en' when KEY is missing from the
selected language, and to KEY's own symbol name when it is missing from
English too — this function never signals an error for an unregistered
key.

Examples:
  (org-scribe-msg 'default-scene-name)
  => \"New scene\"

  (org-scribe-msg 'msg-inserted-link \"Alex\")
  => \"Inserted link to Alex\"

  (org-scribe-msg 'msg-updated-links 5 \"s\")
  => \"Updated character links in 5 scenes\""
  (let* ((table (if (eq org-scribe-message-language 'es)
                    org-scribe-messages-es
                  org-scribe-messages-en))
         (template (or (alist-get key table)
                       (alist-get key org-scribe-messages-en)
                       (symbol-name key))))
    (if args
        (apply #'format template args)
      template)))

(defun org-scribe-plural (count singular-suffix)
  "Return appropriate plural suffix based on COUNT.
SINGULAR-SUFFIX is the suffix to use when COUNT is 1 (usually empty string).
Otherwise returns 's'.

Examples:
  (org-scribe-plural 1 \"\") => \"\"
  (org-scribe-plural 5 \"\") => \"s\"
  (org-scribe-plural 0 \"\") => \"s\"

This is a helper for constructing grammatically correct messages.
Every message in `org-scribe-messages-es' that uses this suffix
attaches it to a noun whose Spanish plural is also formed by adding
just \"s\" (e.g. \"enlace\"/\"enlaces\", \"escena\"/\"escenas\",
\"encabezado\"/\"encabezados\") — nouns needing \"-es\" (e.g.
\"localización\"/\"localizaciones\") are rephrased so the pluralized
word is always one of the safe \"-s\" nouns instead."
  (if (= count 1) singular-suffix "s"))

(provide 'org-scribe-messages)

;;; org-scribe-messages.el ends here
