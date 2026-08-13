;;; org-scribe-wikcionario.el --- Offline definitions via kiwix-serve -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Looks up Spanish definitions in a local Wikcionario (Spanish Wiktionary)
;; ZIM archive served by kiwix-serve, so definitions work with no network and
;; no dependence on the unofficial rae-api.com scraper.
;;
;; There is no free offline Spanish *monolingual* dictionary to package: the
;; RAE's Diccionario de la lengua española is copyrighted and has never been
;; released under a free licence.  Wikcionario is the realistic offline
;; source, and covers what the RAE endpoint returns — definitions, etymology,
;; conjugation tables — plus per-sense synonyms and antonyms.
;;
;; THIS MODULE NEVER MANAGES THE SERVER
;;
;; No `start-process', no sentinels, no port-conflict handling, no "shall I
;; start the server?" prompt.  Running kiwix-serve is the writer's business,
;; documented as a systemd user unit in INSTALL.org.  This module only probes
;; a URL and reports what it finds — and when it finds nothing, the message
;; names the service, because "cannot reach http://localhost:8080" is not
;; actionable while "is kiwix-serve running?" is.
;;
;; URL SCHEME
;;
;; Verified against kiwix-serve 3.7 serving wiktionary_es_all_nopic:
;;
;;   /content/<BOOK>/<WORD>              article        (200 / 404)
;;   /suggest?content=<BOOK>&term=<W>    suggestions    (JSON)
;;   /catalog/v2/entries                 served books   (OPDS XML)
;;
;; The `/content/' prefix is required; the older `/<BOOK>/A/<WORD>' form only
;; 302-redirects to it.  Entries carry no extension (`casa', never
;; `casa.html') and are case-sensitive (`casa' 200, `Casa' 404).
;;
;; THE BOOK NAME IS NOT THE OPDS <name>
;;
;; The catalogue reports the same book under two different names:
;;
;;   <name>wiktionary_es_all</name>                                  ← WRONG
;;   <link type="text/html" href="/content/wiktionary_es_all_nopic"/> ← RIGHT
;;
;; The <name> element omits the flavour (`nopic'), and using it 404s every
;; article and breaks /suggest.  Discovery therefore reads the href of the
;; text/html link.  Do not "simplify" this to the obvious element.
;;
;; WHY THE LOOKUP CHAIN IS NOT THE THESAURUS CHAIN
;;
;; MyThes has no entry for an inflected form, so `org-scribe-mythes-lookup'
;; tries the literal word first and only then its lemma.  Wikcionario is the
;; opposite: it has an article for *every* inflected form, but they are stubs.
;; The complete content of `corriendo' is "Forma verbal — Gerundio de correr",
;; 7.8 KB against 105 KB for `correr'.
;;
;; Literal-first would therefore never reach the lemma step — the literal
;; always exists — and would land the writer on a stub every time they looked
;; up a conjugated verb.  So when hunspell says a word's lemma differs from
;; the word, this module treats it as an inflected form and goes to the lemma
;; article, disclosing the substitution.  The literal is still used when the
;; word is its own lemma, and remains the fallback if the lemma article is
;; missing.
;;
;; This is a linguistic test rather than a size heuristic on purpose: the
;; hermosas/hermoso ratio is only 2.5x, so any byte threshold would be
;; guesswork, while "is this an inflected form?" is precisely the question.

;;; Code:

(require 'json)
(require 'url)
(require 'url-util)
(require 'dom)
(require 'org-scribe-config)
(require 'org-scribe-messages)

(declare-function org-scribe-lemmas "org-scribe-lemma" (word))

(defvar org-scribe--wikcionario-available 'unknown
  "Cached availability of the local kiwix-serve.
`unknown' means never probed; otherwise t or nil.  Probed once per session so
lookups pay no latency, and invalidated by
`org-scribe-wikcionario-clear-cache' or a prefix argument to the lookup
command.")

(defvar org-scribe--wikcionario-book nil
  "Cached ZIM book name discovered from the server, or nil.")

;;;###autoload
(defun org-scribe-wikcionario-clear-cache ()
  "Forget the cached availability and book name, forcing a re-probe.
Use after starting kiwix-serve in a session where it was not running."
  (interactive)
  (setq org-scribe--wikcionario-available 'unknown
        org-scribe--wikcionario-book nil))

;;; HTTP

(defun org-scribe--wikcionario-get (path)
  "GET PATH from the configured server.
Return (STATUS . BODY) with STATUS an integer, or nil when unreachable.
Never signals: an unreachable server is an expected condition here."
  (let ((url (concat (string-trim-right org-scribe-wikcionario-url "/") path))
        (url-request-method "GET"))
    (condition-case nil
        (with-current-buffer
            (url-retrieve-synchronously url t t org-scribe-wikcionario-timeout)
          (unwind-protect
              (progn
                (goto-char (point-min))
                (let ((status (when (re-search-forward "\\`HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                                (string-to-number (match-string 1)))))
                  (goto-char (point-min))
                  (when (re-search-forward "^\r?$" nil t)
                    ;; Step over the blank line itself.  Leaving it in makes
                    ;; the body start with a newline, which is harmless for
                    ;; JSON but puts whitespace before the XML declaration —
                    ;; and `libxml-parse-xml-region' then returns nil for a
                    ;; perfectly good document.
                    (forward-line 1)
                    (cons status
                          (decode-coding-string
                           (buffer-substring-no-properties (point) (point-max))
                           'utf-8)))))
            (kill-buffer (current-buffer))))
      (error nil))))

(defun org-scribe--wikcionario-status (path)
  "Return the HTTP status integer for PATH, or nil when unreachable."
  (car (org-scribe--wikcionario-get path)))

;;; Book discovery

(defun org-scribe--wikcionario-parse-book (xml)
  "Extract the ZIM book name from the OPDS catalogue XML.

Reads the href of the text/html link, NOT the <name> element: the catalogue
reports `wiktionary_es_all' there while URLs need `wiktionary_es_all_nopic'.
Using <name> 404s every article."
  (when-let* ((dom (with-temp-buffer
                     (insert xml)
                     (libxml-parse-xml-region (point-min) (point-max)))))
    (catch 'found
      (dolist (link (dom-by-tag dom 'link))
        (let ((type (dom-attr link 'type))
              (href (dom-attr link 'href)))
          (when (and type href
                     (string-prefix-p "text/html" type)
                     (string-match "/content/\\([^/?#]+\\)" href))
            (throw 'found (match-string 1 href)))))
      nil)))

(defun org-scribe-wikcionario-book ()
  "Return the ZIM book name to query, discovering it when not configured."
  (or org-scribe-wikcionario-book
      org-scribe--wikcionario-book
      (setq org-scribe--wikcionario-book
            (when-let* ((response (org-scribe--wikcionario-get "/catalog/v2/entries"))
                        ((eq (car response) 200)))
              (org-scribe--wikcionario-parse-book (cdr response))))))

;;; Availability

;;;###autoload
(defun org-scribe-wikcionario-available-p (&optional force)
  "Return non-nil when a local Wikcionario is reachable and serving a book.
The result is cached for the session; FORCE re-probes."
  (when force (org-scribe-wikcionario-clear-cache))
  (if (eq org-scribe--wikcionario-available 'unknown)
      (setq org-scribe--wikcionario-available
            (and (org-scribe-wikcionario-book) t))
    org-scribe--wikcionario-available))

;;; URLs

(defun org-scribe-wikcionario-article-url (word)
  "Return the URL of WORD's article, or nil when no book is known."
  (when-let* ((book (org-scribe-wikcionario-book)))
    (format "%s/content/%s/%s"
            (string-trim-right org-scribe-wikcionario-url "/")
            book
            ;; Entries carry no extension and are case-sensitive; only the
            ;; percent-encoding is ours to add.
            (url-hexify-string word))))

(defun org-scribe-wikcionario-entry-exists-p (word)
  "Return non-nil when WORD has an article."
  (when-let* ((book (org-scribe-wikcionario-book)))
    (eq 200 (org-scribe--wikcionario-status
             (format "/content/%s/%s" book (url-hexify-string word))))))

;;; Suggestions

(defun org-scribe--wikcionario-parse-suggestions (json-string)
  "Extract real article names from a /suggest JSON-STRING response.

Entries whose `kind' is not \"path\" are dropped.  The server always appends
a synthetic \"containing '...'\" entry with kind \"pattern\" and no path — an
offer to run a full-text search, not a word — and on a total miss that is the
only element returned.  Without this filter a nonsense word would produce a
suggestion instead of an honest empty result."
  (condition-case nil
      (let* ((json-array-type 'list)
             (json-object-type 'alist)
             (json-key-type 'string)
             (items (json-read-from-string json-string)))
        (delq nil
              (mapcar (lambda (item)
                        (when (equal "path" (cdr (assoc "kind" item)))
                          ;; `value' is the display form; `path' is the URL
                          ;; component.  `label' carries HTML markup and is
                          ;; never used.
                          (or (cdr (assoc "value" item))
                              (cdr (assoc "path" item)))))
                      items)))
    (error nil)))

(defun org-scribe-wikcionario-suggest (word &optional count)
  "Return up to COUNT article names suggested for WORD, or nil."
  (when-let* ((book (org-scribe-wikcionario-book))
              (response (org-scribe--wikcionario-get
                         (format "/suggest?content=%s&term=%s&count=%d"
                                 (url-hexify-string book)
                                 (url-hexify-string word)
                                 (or count 10))))
              ((eq (car response) 200)))
    (mapcar #'string-trim
            (org-scribe--wikcionario-parse-suggestions (cdr response)))))

;;; Resolution

(defun org-scribe--wikcionario-lemmas (word)
  "Return candidate lemmas for WORD, or nil when lemmatization is unavailable."
  (when (fboundp 'org-scribe-lemmas)
    (org-scribe-lemmas word)))

(defun org-scribe-wikcionario-resolve (word)
  "Return (HEADWORD . URL) for WORD, or nil when it has no article.

HEADWORD may differ from WORD: Wikcionario stores every inflected form as a
stub whose whole content is a pointer to the dictionary form, so an inflected
word resolves to its lemma's article.  Callers must disclose that.

The order is deliberately not the thesaurus module's.  See the commentary."
  (let* ((w (string-trim word))
         (lemmas (org-scribe--wikcionario-lemmas w))
         (lemma (car lemmas))
         (candidates
          (delete-dups
           (delq nil
                 (append
                  ;; An inflected form: its own article is a stub, so prefer
                  ;; the lemma. Only when hunspell says the lemma differs.
                  (when (and lemma (not (string-equal-ignore-case lemma w)))
                    (list lemma))
                  ;; The word itself, then its lowercase form (entries are
                  ;; case-sensitive and mostly lowercase).
                  (list w (downcase w))
                  ;; Remaining readings of an ambiguous form.
                  (cdr lemmas))))))
    (catch 'found
      (dolist (candidate candidates)
        (when (org-scribe-wikcionario-entry-exists-p candidate)
          (throw 'found (cons candidate
                              (org-scribe-wikcionario-article-url candidate)))))
      nil)))

(provide 'org-scribe-wikcionario)

;;; org-scribe-wikcionario.el ends here
