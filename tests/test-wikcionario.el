;;; test-wikcionario.el --- Tests for the local Wikcionario backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for language/org-scribe-wikcionario.el and the definition backend
;; dispatch in language/org-scribe-dictionary.el.
;;
;; NO TEST HERE TOUCHES THE NETWORK, and none requires a running kiwix-serve.
;; `org-scribe--wikcionario-get' is stubbed throughout; the JSON and XML
;; fixtures below are real responses captured from kiwix-serve 3.7 serving
;; wiktionary_es_all_nopic, so the parsers are tested against what the server
;; actually sends rather than what it ought to.
;;
;; A final group exercises a live server and skips when none is running.

;;; Code:

(require 'ert)
(require 'cl-lib)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../language" default-directory)))

(require 'org-scribe-wikcionario)
(require 'org-scribe-dictionary)

;;; Captured fixtures

(defconst test-wikcionario-catalog
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <id>be68237f-a6cb-20ed-b2c0-41bd3fa34458</id>
  <link rel=\"self\" href=\"/catalog/v2/entries\"
        type=\"application/atom+xml;profile=opds-catalog;kind=acquisition\"/>
  <title>All Entries</title>
  <entry>
    <title>Wikcionario</title>
    <language>spa</language>
    <name>wiktionary_es_all</name>
    <flavour>nopic</flavour>
    <articleCount>948486</articleCount>
    <link rel=\"http://opds-spec.org/image/thumbnail\"
          href=\"/catalog/v2/illustration/d9fe7d67/?size=48\"
          type=\"image/png;width=48;height=48;scale=1\"/>
    <link type=\"text/html\" href=\"/content/wiktionary_es_all_nopic\" />
  </entry>
</feed>"
  "Real OPDS response.  Note <name> and the content href disagree.")

(defconst test-wikcionario-suggest-hits
  "[
  { \"value\" : \"oscurece\", \"label\" : \"&lt;b&gt;oscurece&lt;/b&gt;\",
    \"kind\" : \"path\" , \"path\" : \"oscurece\" },
  { \"value\" : \"oscurecer\", \"label\" : \"&lt;b&gt;oscurecer&lt;/b&gt;\",
    \"kind\" : \"path\" , \"path\" : \"oscurecer\" },
  { \"value\" : \"oscurec \", \"label\" : \"containing &apos;oscurec&apos;...\",
    \"kind\" : \"pattern\" }
]"
  "Real /suggest response: two articles plus the synthetic pattern entry.")

(defconst test-wikcionario-suggest-miss
  "[
  { \"value\" : \"xyzzyqwerty \", \"label\" : \"containing &apos;xyzzyqwerty&apos;...\",
    \"kind\" : \"pattern\" }
]"
  "Real /suggest response for a word with no articles at all.")

(defmacro test-wikcionario-with-server (responses &rest body)
  "Run BODY with `org-scribe--wikcionario-get' answering from RESPONSES.
RESPONSES is an alist of (PATH-REGEXP . (STATUS . BODY)); the first match
wins and an unmatched path returns nil, i.e. unreachable."
  (declare (indent 1))
  `(let ((org-scribe-wikcionario-url "http://localhost:8080")
         (org-scribe-wikcionario-book nil)
         (org-scribe--wikcionario-book nil)
         (org-scribe--wikcionario-available 'unknown))
     (cl-letf (((symbol-function 'org-scribe--wikcionario-get)
                (lambda (path)
                  ;; `case-fold-search' defaults to t, which would make this
                  ;; stub match /content/x/Casa against the route for `casa'
                  ;; — the opposite of the real server, whose entries are
                  ;; case-sensitive.  A case-folding stub silently passes the
                  ;; downcase-fallback test whether or not the code works.
                  (let ((case-fold-search nil))
                    (cdr (seq-find (lambda (pair) (string-match-p (car pair) path))
                                   ,responses))))))
       ,@body)))

(defconst test-wikcionario-basic-server
  '(("\\`/catalog/v2/entries" . (200 . nil))
    ("\\`/content/[^/]+/casa\\'" . (200 . "<html/>"))
    ("\\`/content/[^/]+/correr\\'" . (200 . "<html/>"))
    ("\\`/content/[^/]+/corriendo\\'" . (200 . "<html/>"))
    ("\\`/content/" . (404 . "not found")))
  "A server with articles for casa, correr and corriendo only.")

;; The catalog body must be the real XML; splice it in at run time.
(defun test-wikcionario-server (&rest extra)
  "Return a stub server alist with the real catalogue plus EXTRA routes."
  (append extra
          (list (cons "\\`/catalog/v2/entries" (cons 200 test-wikcionario-catalog)))
          (list (cons "\\`/content/" (cons 404 "not found")))))

;;; Book discovery

(ert-deftest test-wikcionario-discovers-book-from-content-href ()
  "The book name comes from the content href, not the <name> element.

The catalogue reports <name>wiktionary_es_all</name> while URLs require
wiktionary_es_all_nopic — the flavour suffix is missing from <name>.  Reading
<name> 404s every article and breaks /suggest, so this is pinned."
  (test-wikcionario-with-server (test-wikcionario-server)
    (should (equal "wiktionary_es_all_nopic" (org-scribe-wikcionario-book)))))

(ert-deftest test-wikcionario-parse-book-ignores-thumbnail-link ()
  "Discovery skips non-HTML links.
The catalogue's first <link> is a PNG thumbnail; taking the first link
outright would yield an illustration path."
  (should (equal "wiktionary_es_all_nopic"
                 (org-scribe--wikcionario-parse-book test-wikcionario-catalog))))

(ert-deftest test-wikcionario-configured-book-wins ()
  "An explicit book name is used without querying the catalogue."
  (test-wikcionario-with-server (test-wikcionario-server)
    (let ((org-scribe-wikcionario-book "my_own_zim"))
      (should (equal "my_own_zim" (org-scribe-wikcionario-book))))))

(ert-deftest test-wikcionario-no-book-when-unreachable ()
  "An unreachable server yields no book rather than signalling."
  (test-wikcionario-with-server nil
    (should-not (org-scribe-wikcionario-book))))

;;; Availability and caching

(ert-deftest test-wikcionario-available-when-serving-a-book ()
  "Availability follows book discovery."
  (test-wikcionario-with-server (test-wikcionario-server)
    (should (org-scribe-wikcionario-available-p)))
  (test-wikcionario-with-server nil
    (should-not (org-scribe-wikcionario-available-p))))

(ert-deftest test-wikcionario-probes-once-per-session ()
  "Availability is probed once and then trusted, so lookups cost no latency."
  (let ((probes 0))
    (test-wikcionario-with-server (test-wikcionario-server)
      (cl-letf* ((original (symbol-function 'org-scribe--wikcionario-get))
                 ((symbol-function 'org-scribe--wikcionario-get)
                  (lambda (path)
                    (when (string-prefix-p "/catalog" path) (cl-incf probes))
                    (funcall original path))))
        (org-scribe-wikcionario-available-p)
        (org-scribe-wikcionario-available-p)
        (org-scribe-wikcionario-available-p)
        (should (= 1 probes))))))

(ert-deftest test-wikcionario-force-reprobes ()
  "A forced check re-probes, for when kiwix-serve is started mid-session."
  (let ((probes 0))
    (test-wikcionario-with-server (test-wikcionario-server)
      (cl-letf* ((original (symbol-function 'org-scribe--wikcionario-get))
                 ((symbol-function 'org-scribe--wikcionario-get)
                  (lambda (path)
                    (when (string-prefix-p "/catalog" path) (cl-incf probes))
                    (funcall original path))))
        (org-scribe-wikcionario-available-p)
        (org-scribe-wikcionario-available-p t)
        (should (= 2 probes))))))

(ert-deftest test-wikcionario-clear-cache-resets-state ()
  "Clearing the cache forgets both availability and the book name."
  (test-wikcionario-with-server (test-wikcionario-server)
    (org-scribe-wikcionario-available-p)
    (should org-scribe--wikcionario-book)
    (org-scribe-wikcionario-clear-cache)
    (should-not org-scribe--wikcionario-book)
    (should (eq 'unknown org-scribe--wikcionario-available))))

;;; URL construction

(ert-deftest test-wikcionario-article-url-uses-content-prefix ()
  "Article URLs use the canonical /content/ form with no extension.
The legacy /<BOOK>/A/<WORD> form only 302-redirects, and adding a .html
extension 404s."
  (test-wikcionario-with-server (test-wikcionario-server)
    (should (equal "http://localhost:8080/content/wiktionary_es_all_nopic/casa"
                   (org-scribe-wikcionario-article-url "casa")))))

(ert-deftest test-wikcionario-article-url-percent-encodes ()
  "Accented and spaced words are percent-encoded."
  (test-wikcionario-with-server (test-wikcionario-server)
    (should (string-suffix-p "/canci%C3%B3n"
                             (org-scribe-wikcionario-article-url "canción")))))

(ert-deftest test-wikcionario-article-url-preserves-case ()
  "Case is preserved: entries are case-sensitive, and proper nouns need it."
  (test-wikcionario-with-server (test-wikcionario-server)
    (should (string-suffix-p "/Madrid"
                             (org-scribe-wikcionario-article-url "Madrid")))))

(ert-deftest test-wikcionario-trailing-slash-in-url-is-tolerated ()
  "A configured URL with a trailing slash does not produce a double slash."
  (test-wikcionario-with-server (test-wikcionario-server)
    (let ((org-scribe-wikcionario-url "http://localhost:8080/"))
      (should (equal "http://localhost:8080/content/wiktionary_es_all_nopic/casa"
                     (org-scribe-wikcionario-article-url "casa"))))))

;;; Suggestions

(ert-deftest test-wikcionario-suggestions-drop-the-pattern-entry ()
  "The synthetic \"containing '...'\" entry is not a suggestion.

kiwix-serve always appends an entry with kind \"pattern\" and no path — an
offer to run a full-text search.  Reporting it as a word would be wrong, and
on a total miss it is the only element returned, so without this filter a
nonsense word would never produce an empty result."
  (should (equal '("oscurece" "oscurecer")
                 (org-scribe--wikcionario-parse-suggestions
                  test-wikcionario-suggest-hits))))

(ert-deftest test-wikcionario-suggestions-empty-on-total-miss ()
  "A word with no articles yields no suggestions at all."
  (should-not (org-scribe--wikcionario-parse-suggestions
               test-wikcionario-suggest-miss)))

(ert-deftest test-wikcionario-suggestions-survive-malformed-json ()
  "Unparseable JSON yields nil rather than signalling."
  (should-not (org-scribe--wikcionario-parse-suggestions "not json at all"))
  (should-not (org-scribe--wikcionario-parse-suggestions "")))

(ert-deftest test-wikcionario-suggest-queries-content-parameter ()
  "The /suggest query uses `content=', which is mandatory, not `book='."
  (let (seen)
    (test-wikcionario-with-server (test-wikcionario-server)
      (org-scribe-wikcionario-book)          ; resolve first
      (cl-letf (((symbol-function 'org-scribe--wikcionario-get)
                 (lambda (path)
                   (setq seen path)
                   (cons 200 test-wikcionario-suggest-hits))))
        (org-scribe-wikcionario-suggest "oscurec")
        (should (string-match-p "\\`/suggest\\?" seen))
        (should (string-match-p "content=wiktionary_es_all_nopic" seen))
        (should (string-match-p "term=oscurec" seen))
        (should-not (string-match-p "book=" seen))))))

;;; Resolution — the inflected-form rule

(ert-deftest test-wikcionario-resolves-literal-when-word-is-its-own-lemma ()
  "A dictionary form resolves to itself."
  (test-wikcionario-with-server
      (test-wikcionario-server '("\\`/content/[^/]+/casa\\'" . (200 . "<html/>")))
    (cl-letf (((symbol-function 'org-scribe-lemmas) (lambda (_) '("casa"))))
      (should (equal "casa" (car (org-scribe-wikcionario-resolve "casa")))))))

(ert-deftest test-wikcionario-prefers-lemma-for-inflected-form ()
  "An inflected form resolves to its lemma even though its own article exists.

This is the rule that distinguishes this module from the thesaurus one.
Wikcionario has an article for every inflected form, but they are stubs: the
whole content of `corriendo' is \"Gerundio de correr\", 7.8 KB against 105 KB
for `correr'.  Literal-first would therefore never consult the lemma, and
would land the writer on a stub for every conjugated verb."
  (test-wikcionario-with-server
      (test-wikcionario-server
       '("\\`/content/[^/]+/corriendo\\'" . (200 . "<html/>"))
       '("\\`/content/[^/]+/correr\\'" . (200 . "<html/>")))
    (cl-letf (((symbol-function 'org-scribe-lemmas)
               (lambda (w) (when (equal w "corriendo") '("correr")))))
      (should (equal "correr" (car (org-scribe-wikcionario-resolve "corriendo")))))))

(ert-deftest test-wikcionario-falls-back-to-literal-when-lemma-missing ()
  "When the lemma has no article, the word's own article is used."
  (test-wikcionario-with-server
      (test-wikcionario-server
       '("\\`/content/[^/]+/corriendo\\'" . (200 . "<html/>")))
    (cl-letf (((symbol-function 'org-scribe-lemmas) (lambda (_) '("correr"))))
      (should (equal "corriendo"
                     (car (org-scribe-wikcionario-resolve "corriendo")))))))

(ert-deftest test-wikcionario-resolves-without-lemmatizer ()
  "With no lemmatizer available, the literal word is used."
  (test-wikcionario-with-server
      (test-wikcionario-server '("\\`/content/[^/]+/casa\\'" . (200 . "<html/>")))
    (cl-letf (((symbol-function 'org-scribe-lemmas) nil))
      (should (equal "casa" (car (org-scribe-wikcionario-resolve "casa")))))))

(ert-deftest test-wikcionario-resolves-via-downcase ()
  "A capitalized word resolves against the lowercase entry."
  (test-wikcionario-with-server
      (test-wikcionario-server '("\\`/content/[^/]+/casa\\'" . (200 . "<html/>")))
    (cl-letf (((symbol-function 'org-scribe-lemmas) (lambda (_) nil)))
      (should (equal "casa" (car (org-scribe-wikcionario-resolve "Casa")))))))

(ert-deftest test-wikcionario-resolve-returns-nil-on-miss ()
  "A word with no article anywhere resolves to nil."
  (test-wikcionario-with-server (test-wikcionario-server)
    (cl-letf (((symbol-function 'org-scribe-lemmas) (lambda (_) nil)))
      (should-not (org-scribe-wikcionario-resolve "xyzzyqwerty")))))

;;; Backend dispatch

(ert-deftest test-wikcionario-backend-rae-skips-local ()
  "With the backend forced online, the local server is never consulted."
  (let ((org-scribe-dictionary-backend 'rae-api)
        (probed nil) (rae nil))
    (cl-letf (((symbol-function 'org-scribe-wikcionario-available-p)
               (lambda (&rest _) (setq probed t) nil))
              ((symbol-function 'org-scribe-rae-api-lookup)
               (lambda (&rest _) (setq rae t))))
      (org-scribe-dictionary-lookup "casa")
      (should-not probed)
      (should rae))))

(ert-deftest test-wikcionario-backend-auto-falls-back-to-rae ()
  "With no local server, `auto' uses the online RAE."
  (let ((org-scribe-dictionary-backend 'auto)
        (rae nil))
    (cl-letf (((symbol-function 'org-scribe-wikcionario-available-p)
               (lambda (&rest _) nil))
              ((symbol-function 'org-scribe-rae-api-lookup)
               (lambda (&rest _) (setq rae t))))
      (org-scribe-dictionary-lookup "casa")
      (should rae))))

(ert-deftest test-wikcionario-backend-local-miss-does-not-go-online ()
  "A running server that lacks the word reports it instead of going online.
Silently hopping to the RAE would make a coverage gap look like a network
round trip, and the writer could not tell the two apart."
  (let ((org-scribe-dictionary-backend 'auto)
        (rae nil) (reported nil))
    (cl-letf (((symbol-function 'org-scribe-wikcionario-available-p)
               (lambda (&rest _) t))
              ((symbol-function 'org-scribe-wikcionario-resolve)
               (lambda (&rest _) nil))
              ((symbol-function 'org-scribe--wikcionario-report-miss)
               (lambda (&rest _) (setq reported t)))
              ((symbol-function 'org-scribe-rae-api-lookup)
               (lambda (&rest _) (setq rae t))))
      (org-scribe-dictionary-lookup "xyzzyqwerty")
      (should reported)
      (should-not rae))))

(ert-deftest test-wikcionario-backend-offline-only-never-goes-online ()
  "With the backend forced offline, an unreachable server reports, not falls back."
  (let ((org-scribe-dictionary-backend 'wikcionario)
        (rae nil))
    (cl-letf (((symbol-function 'org-scribe-wikcionario-available-p)
               (lambda (&rest _) nil))
              ((symbol-function 'org-scribe-rae-api-lookup)
               (lambda (&rest _) (setq rae t))))
      (org-scribe-dictionary-lookup "casa")
      (should-not rae))))

(ert-deftest test-wikcionario-prefix-argument-forces-reprobe ()
  "The command's prefix argument reaches the availability probe."
  (let ((org-scribe-dictionary-backend 'auto)
        (forced 'unset))
    (cl-letf (((symbol-function 'org-scribe-wikcionario-available-p)
               (lambda (&optional force) (setq forced force) nil))
              ((symbol-function 'org-scribe-rae-api-lookup) #'ignore))
      (org-scribe-dictionary-lookup "casa" t)
      (should (eq t forced))
      (org-scribe-dictionary-lookup "casa")
      (should-not forced))))

(ert-deftest test-wikcionario-lookup-rejects-empty-input ()
  "Empty input is rejected before any backend is consulted."
  (should-error (org-scribe-dictionary-lookup "   ") :type 'user-error))

;;; Live server (skipped when none is running)

(defun test-wikcionario-live-p ()
  "Return non-nil when a local kiwix-serve is actually reachable."
  (let ((org-scribe--wikcionario-available 'unknown)
        (org-scribe--wikcionario-book nil))
    (org-scribe-wikcionario-available-p)))

(ert-deftest test-wikcionario-live-book-name-has-flavour ()
  "Against a live server, the discovered book name is the URL-usable one."
  (skip-unless (test-wikcionario-live-p))
  (org-scribe-wikcionario-clear-cache)
  (let ((book (org-scribe-wikcionario-book)))
    (should book)
    ;; The catalogue's <name> lacks the flavour; the usable name must work.
    (should (org-scribe-wikcionario-entry-exists-p "casa"))))

(ert-deftest test-wikcionario-live-case-sensitivity ()
  "Against a live server, entries are case-sensitive as assumed."
  (skip-unless (test-wikcionario-live-p))
  (org-scribe-wikcionario-clear-cache)
  (should (org-scribe-wikcionario-entry-exists-p "casa"))
  (should-not (org-scribe-wikcionario-entry-exists-p "CASA")))

(ert-deftest test-wikcionario-live-inflected-form-resolves-to-lemma ()
  "Against a live server and hunspell, `corriendo' resolves to `correr'."
  (skip-unless (and (test-wikcionario-live-p)
                    (fboundp 'org-scribe-lemmas)
                    (equal "correr" (car (org-scribe-lemmas "corriendo")))))
  (org-scribe-wikcionario-clear-cache)
  (should (equal "correr" (car (org-scribe-wikcionario-resolve "corriendo")))))

(ert-deftest test-wikcionario-live-suggestions-for-a-miss ()
  "Against a live server, a partial word yields real article suggestions."
  (skip-unless (test-wikcionario-live-p))
  (org-scribe-wikcionario-clear-cache)
  (let ((suggestions (org-scribe-wikcionario-suggest "oscurec")))
    (should suggestions)
    (should (member "oscurecer" suggestions))
    ;; The synthetic pattern entry must never appear.
    (should-not (cl-some (lambda (s) (string-match-p "containing" s)) suggestions))))

(provide 'test-wikcionario)

;;; test-wikcionario.el ends here
