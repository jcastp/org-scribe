;;; test-lemma.el --- Tests for hunspell lemmatization -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for language/org-scribe-lemma.el.
;;
;; Tests that run hunspell skip when it or the Spanish dictionary is absent,
;; so the suite passes on a machine without them.  The output parser is tested
;; against captured strings and always runs, since that is where the logic is.

;;; Code:

(require 'ert)
(require 'cl-lib)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../language" default-directory)))

(require 'org-scribe-lemma)

(defun test-lemma-hunspell-usable-p ()
  "Return non-nil when hunspell can actually analyse Spanish here."
  (and (org-scribe-lemma-available-p)
       (org-scribe--lemma-call "correr" "es_ES")))

(defmacro test-lemma-with-clean-cache (&rest body)
  "Run BODY with the dictionary cache cleared before and after."
  (declare (indent 0))
  `(let ((org-scribe--lemma-dictionary-cache nil))
     ,@body))

;;; Output parsing (no subprocess)

(ert-deftest test-lemma-parses-single-stem ()
  "The `st:' field is extracted from an analysis line."
  (should (equal '("correr")
                 (org-scribe--lemma-parse "corriendo  st:correr fl:R\n\n"))))

(ert-deftest test-lemma-parses-stem-without-flags ()
  "A word that is already a lemma analyses to itself."
  (should (equal '("correr")
                 (org-scribe--lemma-parse "correr  st:correr\n\n"))))

(ert-deftest test-lemma-parses-multiple-readings-in-order ()
  "Every reading is returned, in hunspell's order.
Ambiguity is normal: \"casas\" is both a noun plural and a verb form, and the
caller decides which one its index can answer."
  (should (equal '("casa" "casar")
                 (org-scribe--lemma-parse
                  "casas  st:casa fl:S\ncasas  st:casar fl:E\n\n"))))

(ert-deftest test-lemma-parse-deduplicates ()
  "Repeated stems collapse to one entry."
  (should (equal '("vino")
                 (org-scribe--lemma-parse
                  "vino  st:vino\nvino  st:vino fl:X\n\n"))))

(ert-deftest test-lemma-parse-unknown-word-yields-nil ()
  "A word with no analysis line has no stem."
  (should-not (org-scribe--lemma-parse "xyzzy\n\n"))
  (should-not (org-scribe--lemma-parse "")))

(ert-deftest test-lemma-parse-ignores-iconv-noise ()
  "Diagnostic lines on stdout do not become lemmas.
hunspell emits `error - iconv' lines under a mismatched locale; parsing only
`st:' lines means such noise can never be mistaken for a result."
  (should (equal '("correr")
                 (org-scribe--lemma-parse
                  (concat "error - iconv: UTF-8 -> ANSI_X3.4-1968\n"
                          "error - iconv: UTF-8 -> ANSI_X3.4-1968\n"
                          "corrió  st:correr fl:R\n\n")))))

(ert-deftest test-lemma-parse-handles-extra-fields ()
  "Fields before `st:' do not defeat the match."
  (should (equal '("casa")
                 (org-scribe--lemma-parse "casas  po:noun st:casa fl:S\n\n"))))

;;; Input guards (no subprocess)

(ert-deftest test-lemma-rejects-multiword-input ()
  "A phrase is never lemmatized.
hunspell analyses token by token, so \"a bocajarro\" would yield lemmas for
\"a\" and \"bocajarro\" separately — meaningless for a phrase that is itself a
thesaurus headword."
  (cl-letf (((symbol-function 'org-scribe--lemma-call)
             (lambda (&rest _) (error "should not be called"))))
    (should-not (org-scribe-lemmas "a bocajarro"))
    (should-not (org-scribe-lemmas "de vez en cuando"))))

(ert-deftest test-lemma-rejects-empty-input ()
  "Empty or whitespace-only input returns nil without running hunspell."
  (cl-letf (((symbol-function 'org-scribe--lemma-call)
             (lambda (&rest _) (error "should not be called"))))
    (should-not (org-scribe-lemmas ""))
    (should-not (org-scribe-lemmas "   "))
    (should-not (org-scribe-lemmas nil))))

(ert-deftest test-lemma-degrades-when-hunspell-absent ()
  "With no hunspell on PATH the module returns nil rather than signalling."
  (cl-letf (((symbol-function 'org-scribe-lemma-available-p) (lambda () nil)))
    (should-not (org-scribe-lemmas "corriendo"))
    (should-not (org-scribe-lemma "corriendo"))))

;;; Dictionary selection

(ert-deftest test-lemma-dictionary-follows-thesaurus-language ()
  "With no explicit dictionary the thesaurus language is used."
  (let ((org-scribe-lemma-dictionary nil)
        (org-scribe-mythes-language "es_ES"))
    (should (equal '("es_ES") (org-scribe--lemma-dictionary-candidates)))))

(ert-deftest test-lemma-dictionary-falls-back-to-base-language ()
  "A regional variant offers the base language's conventional dictionary too.
An es_MX user who installed only es_ES still gets lemmas."
  (let ((org-scribe-lemma-dictionary nil)
        (org-scribe-mythes-language "es_MX"))
    (should (equal '("es_MX" "es_ES") (org-scribe--lemma-dictionary-candidates)))))

(ert-deftest test-lemma-dictionary-explicit-setting-wins ()
  "An explicit dictionary overrides the thesaurus language."
  (let ((org-scribe-lemma-dictionary "en_US")
        (org-scribe-mythes-language "es_ES"))
    (should (equal '("en_US" "en_EN") (org-scribe--lemma-dictionary-candidates)))))

(ert-deftest test-lemma-remembers-failed-dictionary ()
  "A dictionary that cannot be opened is not retried on every call.
Without this the failing candidate costs a process spawn per lookup."
  (test-lemma-with-clean-cache
    (let ((calls 0)
          (org-scribe-lemma-dictionary "zz_ZZ"))
      (cl-letf (((symbol-function 'org-scribe-lemma-available-p) (lambda () t))
                ((symbol-function 'org-scribe--lemma-call)
                 (lambda (&rest _) (cl-incf calls) nil)))
        (should-not (org-scribe-lemmas "corriendo"))
        (let ((after-first calls))
          (should (> after-first 0))
          (should-not (org-scribe-lemmas "andando"))
          (should (= after-first calls)))))))

(ert-deftest test-lemma-reuses-resolved-dictionary ()
  "Once a dictionary works, later calls go straight to it."
  (test-lemma-with-clean-cache
    (let ((tried nil)
          (org-scribe-lemma-dictionary nil)
          (org-scribe-mythes-language "es_MX"))
      (cl-letf (((symbol-function 'org-scribe-lemma-available-p) (lambda () t))
                ((symbol-function 'org-scribe--lemma-call)
                 (lambda (_w dict)
                   (push dict tried)
                   (when (equal dict "es_ES") "correr  st:correr\n\n"))))
        (should (equal '("correr") (org-scribe-lemmas "correr")))
        (should (equal '("es_ES" "es_MX") tried))   ; both tried the first time
        (setq tried nil)
        (should (equal '("correr") (org-scribe-lemmas "correr")))
        (should (equal '("es_ES") tried))))))      ; only the resolved one after

(ert-deftest test-lemma-clear-cache ()
  "`org-scribe-lemma-clear-cache' forces dictionary rediscovery."
  (setq org-scribe--lemma-dictionary-cache '(("es_ES") . "es_ES"))
  (org-scribe-lemma-clear-cache)
  (should-not org-scribe--lemma-dictionary-cache))

;;; Real hunspell

(ert-deftest test-lemma-real-inflected-verb ()
  "A conjugated verb reduces to its infinitive."
  (skip-unless (test-lemma-hunspell-usable-p))
  (test-lemma-with-clean-cache
    (let ((org-scribe-lemma-dictionary "es_ES"))
      (should (equal "correr" (org-scribe-lemma "corriendo")))
      (should (equal "correr" (org-scribe-lemma "corrió"))))))

(ert-deftest test-lemma-real-plural-noun ()
  "A plural noun reduces to its singular."
  (skip-unless (test-lemma-hunspell-usable-p))
  (test-lemma-with-clean-cache
    (let ((org-scribe-lemma-dictionary "es_ES"))
      (should (member "casa" (org-scribe-lemmas "casas"))))))

(ert-deftest test-lemma-real-word-already-lemma ()
  "A dictionary form analyses to itself."
  (skip-unless (test-lemma-hunspell-usable-p))
  (test-lemma-with-clean-cache
    (let ((org-scribe-lemma-dictionary "es_ES"))
      (should (equal "correr" (org-scribe-lemma "correr"))))))

(ert-deftest test-lemma-real-unknown-word ()
  "A word absent from the dictionary yields nil."
  (skip-unless (test-lemma-hunspell-usable-p))
  (test-lemma-with-clean-cache
    (let ((org-scribe-lemma-dictionary "es_ES"))
      (should-not (org-scribe-lemma "xyzzyqwerty")))))

(ert-deftest test-lemma-real-capitalized-word ()
  "A capitalized word reduces to its lowercase lemma."
  (skip-unless (test-lemma-hunspell-usable-p))
  (test-lemma-with-clean-cache
    (let ((org-scribe-lemma-dictionary "es_ES"))
      (should (equal "miedo" (org-scribe-lemma "Miedo"))))))

(ert-deftest test-lemma-real-accented-lemma-survives-hostile-locale ()
  "An accented lemma is not truncated, whatever the ambient locale.

This is the locale regression guard.  hunspell encodes its output for the
locale, so under LC_ALL=C it returns \"st:canci\" — silently cut at the first
non-ASCII byte.  The module forces a UTF-8 locale on the subprocess; without
that, this test fails while every ASCII-lemma test above still passes."
  (skip-unless (test-lemma-hunspell-usable-p))
  (test-lemma-with-clean-cache
    (let ((org-scribe-lemma-dictionary "es_ES")
          (process-environment (append '("LC_ALL=C" "LANG=C") process-environment)))
      (should (equal "canción" (org-scribe-lemma "canción"))))))

(ert-deftest test-lemma-real-ambiguous-word-returns-all-readings ()
  "An ambiguous form returns every reading, not just the first.
\"vino\" is both a noun and a form of `venir'; a caller whose index lacks one
can still answer with the other."
  (skip-unless (test-lemma-hunspell-usable-p))
  (test-lemma-with-clean-cache
    (let* ((org-scribe-lemma-dictionary "es_ES")
           (lemmas (org-scribe-lemmas "vino")))
      (should (member "vino" lemmas))
      (should (member "venir" lemmas)))))

(provide 'test-lemma)

;;; test-lemma.el ends here
