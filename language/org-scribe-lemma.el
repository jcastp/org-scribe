;;; org-scribe-lemma.el --- Word lemmatization via hunspell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Reduces an inflected word to its dictionary form using hunspell's
;; morphological analysis, so that a lookup for a word as it appears in the
;; manuscript can fall back to the form a reference work is indexed under.
;;
;; This matters because both org-scribe's offline sources — the MyThes
;; thesaurus and (later) Wikcionario — are indexed by lemma, while manuscripts
;; contain inflected forms.  Without this, looking up `corriendo' or `corrió'
;; simply misses, which reads to the writer as "this word has no synonyms"
;; rather than "this tool cannot conjugate".
;;
;; No new dependency: hunspell and its Spanish dictionaries are already
;; required for spell checking.  Everything here degrades to nil when hunspell
;; or the dictionary is absent, per the project's optional-dependency
;; convention — it never signals.
;;
;; OUTPUT FORMAT
;;
;; `hunspell -m' emits one analysis line per reading, then a blank line:
;;
;;     $ echo corriendo | hunspell -d es_ES -m
;;     corriendo  st:correr fl:R
;;
;; The stem is the `st:' field.  A word with no analysis (not in the
;; dictionary) yields just the word, with no `st:' — hence nil.
;;
;; AMBIGUITY: several readings are common and the first is not always the one
;; wanted.  `casas' analyses as both `casa' (noun) and `casar' (verb); `vino'
;; as both `vino' (wine) and `venir' (came).  Rather than pick one and hope,
;; `org-scribe-lemmas' returns *every* candidate in hunspell's order and lets
;; the caller try each against its index — if `vino' has no thesaurus entry but
;; `venir' does, the writer still gets an answer.  Building a disambiguation UI
;; would need context this module does not have.
;;
;; ENCODING AND LOCALE
;;
;; The Spanish hunspell dictionary is UTF-8 (`SET UTF-8' in es_ES.aff) — note
;; this is the opposite of the MyThes data, which is ISO8859-1; the two are
;; unrelated files and neither encoding may be assumed from the other.
;;
;; hunspell converts its output to the *locale's* encoding, so under a C locale
;; it mangles accented lemmas: `canción' comes back analysed as `st:canci',
;; silently truncated at the first non-ASCII byte, alongside `error - iconv'
;; noise on stdout.  Passing `-i UTF-8' is not sufficient — it fixes the input
;; side only.  The subprocess is therefore run with `LC_ALL' forced to a UTF-8
;; locale, which eliminates both the truncation and the noise.  Parsing only
;; `st:' lines means any residual noise is ignored regardless.

;;; Code:

(require 'org-scribe-config)

(defconst org-scribe--lemma-locale "C.UTF-8"
  "Locale forced on the hunspell subprocess.
Chosen over a language-specific locale because it is present on any modern
glibc system without the user having generated it.")

(defvar org-scribe--lemma-dictionary-cache nil
  "Cons of (REQUESTED . RESOLVED) for the last dictionary lookup.
RESOLVED is the dictionary name that worked, or the symbol `none'.  Caching
this avoids re-paying for a failed first candidate on every single call.")

;;;###autoload
(defun org-scribe-lemma-available-p ()
  "Return non-nil when hunspell is available for morphological analysis."
  (and (executable-find "hunspell") t))

(defun org-scribe--lemma-dictionary-candidates ()
  "Return the hunspell dictionaries to try, most specific first.
When `org-scribe-lemma-dictionary' is nil the language is taken from
`org-scribe-mythes-language', so one setting drives both the thesaurus and
the morphology.  A regional variant falls back to the base language's
conventional dictionary, letting an `es_MX' user who only installed `es_ES'
still get lemmas."
  (let* ((lang (or org-scribe-lemma-dictionary org-scribe-mythes-language))
         (base (car (split-string lang "[_-]")))
         (conventional (concat base "_" (upcase base))))
    (delete-dups (list lang conventional))))

(defun org-scribe--lemma-call (word dictionary)
  "Run hunspell -m on WORD with DICTIONARY.
Return the output string, or nil when the run failed."
  (with-temp-buffer
    (let* ((process-environment
            (append (list (concat "LC_ALL=" org-scribe--lemma-locale)
                          (concat "LANG=" org-scribe--lemma-locale))
                    process-environment))
           (coding-system-for-read 'utf-8)
           (coding-system-for-write 'utf-8)
           (exit (condition-case nil
                     (call-process-region word nil "hunspell" nil t nil
                                          "-d" dictionary "-i" "UTF-8" "-m")
                   ;; hunspell absent: `call-process-region' signals rather
                   ;; than returning an exit code.
                   (error nil))))
      (when (eq exit 0)
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun org-scribe--lemma-parse (output)
  "Extract the `st:' stems from hunspell OUTPUT, in order, deduplicated."
  (let ((stems nil)
        (start 0))
    (while (string-match "^[^ \t\n]+[ \t]+\\(?:.*[ \t]\\)?st:\\([^ \t\n]+\\)"
                         output start)
      (push (match-string 1 output) stems)
      (setq start (match-end 0)))
    (delete-dups (nreverse stems))))

;;;###autoload
(defun org-scribe-lemmas (word)
  "Return the candidate dictionary forms of WORD, most likely first.

Returns nil when WORD is unknown, when hunspell is unavailable, or when WORD
contains whitespace — hunspell analyses a phrase token by token, so a
multi-word entry such as \"a bocajarro\" would yield per-token lemmas that
mean nothing as a phrase.

Several readings are normal: \"casas\" returns (\"casa\" \"casar\").  Callers
should try each candidate in turn rather than assuming the first is right."
  (let ((w (string-trim (or word ""))))
    (when (and (not (string-empty-p w))
               (not (string-match-p "[ \t\n]" w))
               (org-scribe-lemma-available-p))
      (let ((requested (org-scribe--lemma-dictionary-candidates))
            (result nil))
        ;; Reuse the previously resolved dictionary when the request is
        ;; unchanged, including a remembered failure.
        (if (and org-scribe--lemma-dictionary-cache
                 (equal (car org-scribe--lemma-dictionary-cache) requested))
            (let ((resolved (cdr org-scribe--lemma-dictionary-cache)))
              (unless (eq resolved 'none)
                (setq result (org-scribe--lemma-parse
                              (or (org-scribe--lemma-call w resolved) "")))))
          (let ((resolved 'none))
            (catch 'found
              (dolist (dict requested)
                (when-let* ((output (org-scribe--lemma-call w dict)))
                  (setq resolved dict
                        result (org-scribe--lemma-parse output))
                  (throw 'found t))))
            (setq org-scribe--lemma-dictionary-cache (cons requested resolved))))
        result))))

;;;###autoload
(defun org-scribe-lemma (word)
  "Return the most likely dictionary form of WORD, or nil.
See `org-scribe-lemmas' when more than one reading matters."
  (car (org-scribe-lemmas word)))

;;;###autoload
(defun org-scribe-lemma-clear-cache ()
  "Forget the resolved hunspell dictionary, forcing rediscovery."
  (interactive)
  (setq org-scribe--lemma-dictionary-cache nil))

(provide 'org-scribe-lemma)

;;; org-scribe-lemma.el ends here
