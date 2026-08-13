;;; test-mythes.el --- Tests for the offline MyThes reader -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for language/org-scribe-mythes.el and the thesaurus backend
;; dispatch in language/org-scribe-dictionary.el.
;;
;; Most tests run against generated fixtures in fixtures/mythes/ rather than
;; the system thesaurus, so the suite passes on a machine with no `mythes-es'
;; installed.  Regenerate them with
;;   emacs -Q --batch -l tests/fixtures/make-mythes-fixtures.el
;;
;; A handful of tests exercise the real installed data and skip when it is
;; absent; they guard against the shipped files differing from the format the
;; reader assumes.

;;; Code:

(require 'ert)
(require 'cl-lib)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../language" default-directory)))

(require 'org-scribe-mythes)
(require 'org-scribe-dictionary)

(defconst test-mythes-fixture-dir
  (expand-file-name "fixtures/mythes/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory holding the generated MyThes fixtures.")

(defmacro test-mythes-with-fixture (lang &rest body)
  "Run BODY with the fixture thesaurus LANG selected and the cache cleared.

Lemmatization is disabled unless BODY rebinds
`org-scribe-mythes-lemma-function': the reader consults `org-scribe-lemmas'
whenever it is loaded, and leaving that live would make these tests spawn
hunspell and depend on its dictionaries."
  (declare (indent 1))
  `(let ((org-scribe-mythes-directory test-mythes-fixture-dir)
         (org-scribe-mythes-language ,lang)
         (org-scribe-mythes-lemma-function #'ignore)
         (org-scribe--mythes-index-cache nil))
     ,@body))

;;; File discovery

(ert-deftest test-mythes-finds-fixture-pair ()
  "Both fixture files are located for an exact language match."
  (test-mythes-with-fixture "xx_L1"
    (let ((files (org-scribe-mythes-files)))
      (should files)
      (should (string-suffix-p "th_xx_L1_v2.dat" (car files)))
      (should (string-suffix-p "th_xx_L1_v2.idx" (cdr files))))))

(ert-deftest test-mythes-available-p-reflects-data-presence ()
  "`org-scribe-mythes-available-p' is nil when no thesaurus is installed."
  (test-mythes-with-fixture "xx_L1"
    (should (org-scribe-mythes-available-p)))
  (let ((org-scribe-mythes-directory "/nonexistent/mythes/")
        (org-scribe-mythes-language "es_ES"))
    (should-not (org-scribe-mythes-available-p))))

(ert-deftest test-mythes-falls-back-to-same-base-language ()
  "A missing exact match resolves to another variant of the base language.
Debian ships every regional Spanish thesaurus as a symlink to th_es_ES_v2,
so `es_MX' must not report \"no thesaurus installed\"."
  (test-mythes-with-fixture "xx_ZZ"
    (let ((files (org-scribe-mythes-files)))
      (should files)
      (should (string-match-p "th_xx_\\(L1\\|UTF\\)_v2\\.dat\\'" (car files))))))

;;; Encoding

(ert-deftest test-mythes-reads-declared-encoding ()
  "The encoding is taken from line 1 of the file, not assumed."
  (should (eq 'latin-1
              (org-scribe--mythes-coding-system
               (expand-file-name "th_xx_L1_v2.dat" test-mythes-fixture-dir))))
  (should (eq 'utf-8
              (org-scribe--mythes-coding-system
               (expand-file-name "th_xx_UTF_v2.dat" test-mythes-fixture-dir)))))

;;; Lookup

(ert-deftest test-mythes-lookup-returns-grouped-senses ()
  "A lookup returns one (POS . SYNONYMS) group per meaning line."
  (test-mythes-with-fixture "xx_L1"
    (let ((groups (org-scribe-mythes-lookup "abandonar")))
      (should (= 3 (length groups)))
      (should (equal "tr." (car (nth 0 groups))))
      (should (equal '("dejar" "desamparar" "desatender") (cdr (nth 0 groups))))
      (should (equal "prnl." (car (nth 1 groups)))))))

(ert-deftest test-mythes-pos-placeholder-becomes-nil ()
  "The bare \"-\" placeholder is reported as nil, not as a label.
Most Spanish entries carry no grammatical label, so this is the common case."
  (test-mythes-with-fixture "xx_L1"
    (let ((group (nth 2 (org-scribe-mythes-lookup "abandonar"))))
      (should-not (car group))
      (should (equal '("renunciar" "dimitir" "cesar") (cdr group))))))

(ert-deftest test-mythes-lookup-after-accented-entry ()
  "A headword stored after an accented one still resolves correctly.

This is the byte-offset regression guard.  The .idx holds byte offsets while
Emacs point counts characters; a reader that conflates them drifts by one
position per extra byte earlier in the file, so `zapato' — which follows
`canción' in the fixture — comes back as a fragment of another entry
\(\"to|1\" instead of \"zapato|1\").

Both fixtures are checked and the UTF-8 one is what does the work: Latin-1 is
single-byte, so byte and character offsets coincide there and a broken reader
passes.  Do not drop the UTF-8 case from this loop."
  (dolist (lang '("xx_L1" "xx_UTF"))
    (test-mythes-with-fixture lang
      (let ((groups (org-scribe-mythes-lookup "zapato")))
        (should (= 1 (length groups)))
        (should (equal "m." (car (nth 0 groups))))
        (should (equal '("calzado" "bota" "chanclo") (cdr (nth 0 groups))))))))

(ert-deftest test-mythes-lookup-accented-headword ()
  "An accented headword is found and its synonyms decode correctly."
  (dolist (lang '("xx_L1" "xx_UTF"))
    (test-mythes-with-fixture lang
      (let ((groups (org-scribe-mythes-lookup "canción")))
        (should (= 1 (length groups)))
        (should (member "melodía" (cdr (nth 0 groups))))))))

(ert-deftest test-mythes-both-encodings-agree ()
  "The Latin-1 and UTF-8 fixtures yield identical decoded results."
  (should (equal (test-mythes-with-fixture "xx_L1"
                   (org-scribe-mythes-lookup "canción"))
                 (test-mythes-with-fixture "xx_UTF"
                   (org-scribe-mythes-lookup "canción")))))

(ert-deftest test-mythes-lookup-miss-returns-nil ()
  "A word with no entry returns nil rather than signalling."
  (test-mythes-with-fixture "xx_L1"
    (should-not (org-scribe-mythes-lookup "inexistente"))))

(ert-deftest test-mythes-lookup-falls-back-to-downcase ()
  "A capitalized word resolves against the lowercase headword.
Words at the start of a sentence are capitalized in the manuscript."
  (test-mythes-with-fixture "xx_L1"
    (should (org-scribe-mythes-lookup "Madrid"))
    (should (equal (org-scribe-mythes-lookup "Madrid")
                   (org-scribe-mythes-lookup "madrid")))))

(ert-deftest test-mythes-lookup-trims-whitespace ()
  "Surrounding whitespace does not defeat a lookup."
  (test-mythes-with-fixture "xx_L1"
    (should (org-scribe-mythes-lookup "  abandonar  "))))

(ert-deftest test-mythes-dedupes-reordered-senses ()
  "Meaning groups holding the same synonyms in a different order collapse.
The shipped Spanish data contains such pairs; rendering both reads as a bug."
  (test-mythes-with-fixture "xx_L1"
    (should (= 1 (length (org-scribe-mythes-lookup "alegre"))))))

;;; Lemma fallback (Phase 2)

(ert-deftest test-mythes-lookup-entry-reports-direct-hit ()
  "A direct hit reports the word itself as the answering headword."
  (test-mythes-with-fixture "xx_L1"
    (should (equal "abandonar" (car (org-scribe-mythes-lookup-entry "abandonar"))))))

(ert-deftest test-mythes-lookup-entry-reports-downcased-headword ()
  "A capitalized word reports the lowercase headword that answered."
  (test-mythes-with-fixture "xx_L1"
    (should (equal "madrid" (car (org-scribe-mythes-lookup-entry "Madrid"))))))

(ert-deftest test-mythes-lookup-falls-back-to-lemma ()
  "An inflected form resolves through its lemma.
This is the whole point of Phase 2: the thesaurus is indexed by dictionary
form while the manuscript contains conjugations."
  (test-mythes-with-fixture "xx_L1"
    (let ((org-scribe-mythes-lemma-function
           (lambda (w) (when (equal w "abandonando") '("abandonar")))))
      (let ((entry (org-scribe-mythes-lookup-entry "abandonando")))
        (should entry)
        (should (equal "abandonar" (car entry)))
        (should (= 3 (length (cdr entry))))))))

(ert-deftest test-mythes-lemma-tried-only-after-literal-forms ()
  "The literal word wins over any lemma.
A word that is itself a headword must never be silently replaced."
  (test-mythes-with-fixture "xx_L1"
    (let ((org-scribe-mythes-lemma-function (lambda (_) '("zapato"))))
      (should (equal "abandonar"
                     (car (org-scribe-mythes-lookup-entry "abandonar")))))))

(ert-deftest test-mythes-tries-every-lemma-candidate ()
  "Each candidate lemma is tried until one has an entry.
Ambiguous forms yield several readings and only some are in the thesaurus."
  (test-mythes-with-fixture "xx_L1"
    (let ((org-scribe-mythes-lemma-function
           (lambda (_) '("nosuchword" "alsonothere" "zapato"))))
      (should (equal "zapato" (car (org-scribe-mythes-lookup-entry "zapatos")))))))

(ert-deftest test-mythes-lemma-candidates-are-also-downcased ()
  "A capitalized lemma still resolves against a lowercase headword."
  (test-mythes-with-fixture "xx_L1"
    (let ((org-scribe-mythes-lemma-function (lambda (_) '("Madrid"))))
      (should (equal "madrid" (car (org-scribe-mythes-lookup-entry "Madrileño")))))))

(ert-deftest test-mythes-lookup-works-without-lemmatizer ()
  "With no lemmatizer available, direct lookups are unaffected."
  (test-mythes-with-fixture "xx_L1"
    (let ((org-scribe-mythes-lemma-function nil))
      ;; Unbinding via `cl-letf' so the definition is restored afterwards;
      ;; `fmakunbound' here would break every later test in the file.
      (cl-letf (((symbol-function 'org-scribe-lemmas) nil))
        (should-not (fboundp 'org-scribe-lemmas))
        (should (org-scribe-mythes-lookup-entry "abandonar"))
        (should-not (org-scribe-mythes-lookup-entry "abandonando"))))))

(ert-deftest test-mythes-real-data-resolves-inflected-form ()
  "A conjugated verb resolves against the installed thesaurus via hunspell.
Skipped unless both the thesaurus and hunspell are installed."
  (let ((org-scribe-mythes-directory "/usr/share/mythes/")
        (org-scribe-mythes-language "es_ES")
        (org-scribe-mythes-lemma-function nil)
        (org-scribe--mythes-index-cache nil))
    (skip-unless (and (org-scribe-mythes-available-p)
                      (fboundp 'org-scribe-lemmas)
                      (org-scribe-lemma "corriendo")))
    (let ((entry (org-scribe-mythes-lookup-entry "corriendo")))
      (should entry)
      (should (equal "correr" (car entry)))
      (should (cdr entry)))))

;;; Index cache

(ert-deftest test-mythes-index-cache-is-reused ()
  "A second lookup reuses the cached index instead of re-reading the file."
  (test-mythes-with-fixture "xx_L1"
    (org-scribe-mythes-lookup "abandonar")
    (should org-scribe--mythes-index-cache)
    (let ((calls 0))
      (cl-letf* ((original (symbol-function 'org-scribe--mythes-load-index))
                 ((symbol-function 'org-scribe--mythes-load-index)
                  (lambda (&rest args) (cl-incf calls) (apply original args))))
        (org-scribe-mythes-lookup "zapato")
        (should (= 0 calls))))))

(ert-deftest test-mythes-index-cache-invalidated-on-mtime-change ()
  "Touching the index file forces a reload, so an upgrade is picked up."
  (test-mythes-with-fixture "xx_L1"
    (org-scribe-mythes-lookup "abandonar")
    (setcar (cdr org-scribe--mythes-index-cache) '(0 0))  ; fake a stale mtime
    (let ((calls 0))
      (cl-letf* ((original (symbol-function 'org-scribe--mythes-load-index))
                 ((symbol-function 'org-scribe--mythes-load-index)
                  (lambda (&rest args) (cl-incf calls) (apply original args))))
        (org-scribe-mythes-lookup "zapato")
        (should (= 1 calls))))))

(ert-deftest test-mythes-clear-cache-forces-reload ()
  "`org-scribe-mythes-clear-cache' discards the cached index."
  (test-mythes-with-fixture "xx_L1"
    (org-scribe-mythes-lookup "abandonar")
    (should org-scribe--mythes-index-cache)
    (org-scribe-mythes-clear-cache)
    (should-not org-scribe--mythes-index-cache)))

;;; Backend dispatch

;; These stub `org-scribe-mythes-lookup-entry' — the function the dispatcher
;; actually calls.  Stubbing `org-scribe-mythes-lookup' instead leaves the real
;; lookup running against the system thesaurus, and the assertions stop meaning
;; anything.

(ert-deftest test-mythes-backend-wordreference-skips-local-lookup ()
  "With the backend forced online, the local thesaurus is never consulted."
  (let ((org-scribe-thesaurus-backend 'wordreference)
        (looked-up nil)
        (opened nil))
    (cl-letf (((symbol-function 'org-scribe-mythes-lookup-entry)
               (lambda (&rest _) (setq looked-up t) nil))
              ((symbol-function 'org-scribe--sinonimo-wordreference)
               (lambda (&rest _) (setq opened t))))
      (org-scribe-sinonimo "abandonar")
      (should-not looked-up)
      (should opened))))

(ert-deftest test-mythes-backend-auto-falls-back-online-on-miss ()
  "Under `auto', a word absent from the local data still opens WordReference."
  (let ((org-scribe-thesaurus-backend 'auto)
        (opened nil))
    (cl-letf (((symbol-function 'org-scribe-mythes-lookup-entry) (lambda (&rest _) nil))
              ((symbol-function 'org-scribe-mythes-available-p) (lambda () t))
              ((symbol-function 'org-scribe--sinonimo-wordreference)
               (lambda (&rest _) (setq opened t))))
      (org-scribe-sinonimo "inexistente")
      (should opened))))

(ert-deftest test-mythes-backend-mythes-never-goes-online ()
  "With the backend forced offline, a miss reports rather than opening eww."
  (let ((org-scribe-thesaurus-backend 'mythes)
        (opened nil))
    (cl-letf (((symbol-function 'org-scribe-mythes-lookup-entry) (lambda (&rest _) nil))
              ((symbol-function 'org-scribe-mythes-available-p) (lambda () t))
              ((symbol-function 'org-scribe--sinonimo-wordreference)
               (lambda (&rest _) (setq opened t))))
      (org-scribe-sinonimo "inexistente")
      (should-not opened))))

(ert-deftest test-mythes-backend-auto-renders-local-hit ()
  "A local hit is rendered locally and does not touch the network path."
  (let ((org-scribe-thesaurus-backend 'auto)
        (rendered nil)
        (opened nil))
    (cl-letf (((symbol-function 'org-scribe-mythes-lookup-entry)
               (lambda (&rest _) '("abandonar" ("tr." "dejar" "soltar"))))
              ((symbol-function 'org-scribe--sinonimo-render-mythes)
               (lambda (&rest _) (setq rendered t)))
              ((symbol-function 'org-scribe--sinonimo-wordreference)
               (lambda (&rest _) (setq opened t))))
      (org-scribe-sinonimo "abandonar")
      (should rendered)
      (should-not opened))))

(ert-deftest test-mythes-sinonimo-rejects-empty-input ()
  "An empty word is rejected before any backend is consulted."
  (should-error (org-scribe-sinonimo "   ") :type 'user-error))

;;; Rendering

(ert-deftest test-mythes-render-produces-org-buffer ()
  "The renderer writes an Org buffer listing every sense."
  (cl-letf (((symbol-function 'org-scribe--side-window) #'ignore))
    (org-scribe--sinonimo-render-mythes
     "abandonar" '(("tr." "dejar" "soltar") (nil "renunciar" "cesar")))
    (with-current-buffer (format "*%s*" (org-scribe-msg 'msg-thesaurus-title "abandonar"))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (eq major-mode 'org-mode))
        (should (string-match-p "dejar, soltar" text))
        (should (string-match-p "renunciar, cesar" text))
        (should (string-match-p "/tr\\./" text))
        ;; Both senses are headings, so an unlabelled one is still countable.
        (should (= 2 (cl-count ?* text)))))))

(ert-deftest test-mythes-render-discloses-lemma-substitution ()
  "When a lemma answered, the buffer says so.
A writer who asked about \"corriendo\" and is shown \"correr\" must see that
a substitution happened, or the tool looks like it misread the question."
  (cl-letf (((symbol-function 'org-scribe--side-window) #'ignore))
    (org-scribe--sinonimo-render-mythes
     "corriendo" '((nil "huir" "escapar")) "correr")
    (with-current-buffer (format "*%s*" (org-scribe-msg 'msg-thesaurus-title "corriendo"))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "corriendo" text))
        (should (string-match-p "correr" text))
        ;; The notice is an Org comment: never exported, never word-counted.
        (should (string-match-p "^# " text))))))

(ert-deftest test-mythes-render-silent-when-no-substitution ()
  "A direct hit carries no substitution notice."
  (cl-letf (((symbol-function 'org-scribe--side-window) #'ignore))
    (org-scribe--sinonimo-render-mythes
     "abandonar" '((nil "dejar" "soltar")) "abandonar")
    (with-current-buffer (format "*%s*" (org-scribe-msg 'msg-thesaurus-title "abandonar"))
      (should-not (string-match-p
                   "^# " (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest test-mythes-render-case-difference-is-not-a-substitution ()
  "Resolving `Miedo' to the headword `miedo' is not worth announcing."
  (cl-letf (((symbol-function 'org-scribe--side-window) #'ignore))
    (org-scribe--sinonimo-render-mythes "Miedo" '((nil "temor")) "miedo")
    (with-current-buffer (format "*%s*" (org-scribe-msg 'msg-thesaurus-title "Miedo"))
      (should-not (string-match-p
                   "^# " (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest test-mythes-dispatch-passes-headword-to-renderer ()
  "The dispatcher forwards the answering headword, not just the groups."
  (let ((org-scribe-thesaurus-backend 'auto)
        (received nil))
    (cl-letf (((symbol-function 'org-scribe-mythes-lookup-entry)
               (lambda (&rest _) '("correr" (nil "huir"))))
              ((symbol-function 'org-scribe--sinonimo-render-mythes)
               (lambda (word _groups &optional headword)
                 (setq received (list word headword)))))
      (org-scribe-sinonimo "corriendo")
      (should (equal '("corriendo" "correr") received)))))

;;; Real installed data

(ert-deftest test-mythes-real-data-parses ()
  "The installed system thesaurus parses under the assumed format.
Skipped when no thesaurus is installed."
  (let ((org-scribe-mythes-directory "/usr/share/mythes/")
        (org-scribe-mythes-language "es_ES")
        (org-scribe--mythes-index-cache nil))
    (skip-unless (org-scribe-mythes-available-p))
    (let ((groups (org-scribe-mythes-lookup "abandonar")))
      (should groups)
      (should (cl-every (lambda (g) (and (listp g) (cdr g))) groups))
      (should (member "renunciar" (apply #'append (mapcar #'cdr groups)))))))

(ert-deftest test-mythes-real-data-handles-accents ()
  "An accented headword in the installed data decodes correctly.
Skipped when no thesaurus is installed."
  (let ((org-scribe-mythes-directory "/usr/share/mythes/")
        (org-scribe-mythes-language "es_ES")
        (org-scribe--mythes-index-cache nil))
    (skip-unless (org-scribe-mythes-available-p))
    (let ((groups (org-scribe-mythes-lookup "canción")))
      (should groups)
      ;; Every synonym must be decoded text, never raw undecoded bytes.
      (dolist (syn (apply #'append (mapcar #'cdr groups)))
        (should (stringp syn))
        (should-not (string-match-p "[\200-\377]" syn))))))

(provide 'test-mythes)

;;; test-mythes.el ends here
