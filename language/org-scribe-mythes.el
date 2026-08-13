;;; org-scribe-mythes.el --- Offline MyThes thesaurus reader -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Reads the MyThes thesaurus data files shipped by LibreOffice language
;; packs (Debian/Ubuntu: `mythes-es', `mythes-en-us', ...), giving org-scribe
;; an offline synonym source with no network dependency.
;;
;; This module is a *pure data reader*: it takes a word and returns a data
;; structure.  All buffer, window and rendering work lives in
;; `org-scribe-dictionary.el'.  Keeping the split means this file is testable
;; in batch mode with no display.
;;
;; FILE FORMAT
;;
;; A MyThes thesaurus is a pair of files, e.g. th_es_ES_v2.idx and
;; th_es_ES_v2.dat.  Line 1 of each declares the encoding; the Spanish data
;; ships as ISO8859-1, so the encoding must be read, never assumed.
;;
;; The .idx is line 1 encoding, line 2 the entry count, then one line per
;; headword mapping it to a position in the .dat:
;;
;;     ISO8859-1
;;     21846
;;     a bocajarro|212
;;     a excepción de|425
;;
;; The .dat holds variable-length entries.  Each begins with
;; HEADWORD|MEANING-COUNT followed by exactly that many meaning lines of the
;; form POS|SYNONYM|SYNONYM|...  The POS field is a bare "-" when absent —
;; which it is for the vast majority of Spanish entries — and otherwise an
;; abbreviated grammatical label in parentheses, "(tr.)", "(prnl.)", "(m.)":
;;
;;     abandonar|4
;;     (prnl.)|abandonarse|dejarse llevar|confiarse|darse|entregarse
;;     (tr.)|desistir|renunciar|marcharse|desasistir|dejar|ceder
;;     -|aparcar|posponer|retrasar|atrasar|arrinconar|diferir
;;
;; Unlike the English MyThes data, Spanish meaning lines do not repeat the
;; headword among its own synonyms.
;;
;; THE BYTE-OFFSET CONSTRAINT
;;
;; The .idx positions are *byte* offsets, but Emacs point positions are
;; *character* positions.  The .dat is therefore read into a unibyte buffer
;; with `insert-file-contents-literally' and only the extracted region is
;; decoded.  Both of those are load-bearing: the literal read also suppresses
;; EOL translation, which would otherwise shift every offset in a file with
;; CRLF line endings.
;;
;; The trap here is that the obvious wrong implementation — decode the whole
;; file, treat the offset as a character position — *works perfectly on the
;; Spanish data*.  ISO8859-1 is a single-byte encoding, so byte and character
;; positions coincide, and every test against th_es_ES_v2 would pass.  It
;; breaks only on a UTF-8 thesaurus, where the two diverge at the first
;; accented headword and every later lookup returns a fragment of an unrelated
;; entry: reading `zapato' at a byte offset yields "to|1".  Since MyThes
;; declares its encoding per file and newer packs ship UTF-8, the reader must
;; be correct for both.  `test-mythes-lookup-after-accented-entry' runs
;; against a Latin-1 *and* a UTF-8 fixture for exactly this reason — against
;; the Latin-1 one alone it would pass no matter how the offsets were handled.

;;; Code:

(require 'cl-lib)
(require 'org-scribe-config)
(require 'org-scribe-messages)

(defconst org-scribe--mythes-coding-systems
  '(("ISO8859-1"  . latin-1)
    ("ISO-8859-1" . latin-1)
    ("ISO8859-15" . iso-8859-15)
    ("UTF-8"      . utf-8)
    ("UTF8"       . utf-8))
  "Map MyThes encoding names to Emacs coding systems.")

(defvar org-scribe--mythes-index-cache nil
  "Cached MyThes index, as (IDX-FILE MTIME . HASH-TABLE), or nil.
HASH-TABLE maps a headword string to its byte offset in the .dat file.")

;;; File discovery

(defun org-scribe--mythes-file-pair (dir base)
  "Return (DAT . IDX) for BASE in DIR when both files are readable, else nil."
  (let ((dat (expand-file-name (concat base ".dat") dir))
        (idx (expand-file-name (concat base ".idx") dir)))
    (when (and (file-readable-p dat) (file-readable-p idx))
      (cons dat idx))))

(defun org-scribe-mythes-files ()
  "Return (DAT . IDX) for the configured thesaurus, or nil when absent.
Tries the exact language first, then any thesaurus for the same base
language — Debian ships every regional Spanish variant as a symlink to
th_es_ES_v2, so `es_MX' resolves without a separate download."
  (let* ((dir (file-name-as-directory org-scribe-mythes-directory))
         (lang org-scribe-mythes-language)
         (base-lang (car (split-string lang "[_-]"))))
    (when (file-directory-p dir)
      (or (org-scribe--mythes-file-pair dir (format "th_%s_v2" lang))
          ;; Fall back to any variant of the same base language.
          (let ((found nil))
            (dolist (dat (file-expand-wildcards
                          (expand-file-name (format "th_%s_*_v2.dat" base-lang) dir))
                         found)
              (unless found
                (setq found (org-scribe--mythes-file-pair
                             dir (file-name-base dat))))))))))

;;;###autoload
(defun org-scribe-mythes-available-p ()
  "Return non-nil when MyThes thesaurus data is installed and readable."
  (and (org-scribe-mythes-files) t))

;;; Encoding

(defun org-scribe--mythes-coding-system (file)
  "Return the Emacs coding system declared on line 1 of FILE.
Falls back to `latin-1', the encoding the Spanish data actually ships in."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 64)
    (goto-char (point-min))
    (let* ((name (string-trim (buffer-substring-no-properties
                               (point) (line-end-position))))
           (mapped (cdr (assoc-string name org-scribe--mythes-coding-systems t)))
           (guess (intern (downcase name))))
      (cond (mapped mapped)
            ((coding-system-p guess) guess)
            (t 'latin-1)))))

;;; Index

(defun org-scribe--mythes-load-index (idx)
  "Load IDX into a hash table mapping headword to byte offset in the .dat."
  (let ((table (make-hash-table :test #'equal :size 24000))
        (coding (org-scribe--mythes-coding-system idx)))
    (with-temp-buffer
      (let ((coding-system-for-read coding))
        (insert-file-contents idx))
      (goto-char (point-min))
      (forward-line 2)                  ; skip the encoding and count lines
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (point) (line-end-position))))
          ;; Split on the *last* "|": a headword may not contain one, but
          ;; splitting from the right costs nothing and cannot misparse.
          (when-let* ((sep (string-match-p "|[0-9]+$" line)))
            (puthash (substring line 0 sep)
                     (string-to-number (substring line (1+ sep)))
                     table)))
        (forward-line 1)))
    table))

(defun org-scribe--mythes-index (idx)
  "Return the hash table for IDX, loading and caching it when needed.
The cache is invalidated when the file's modification time changes, so a
package upgrade is picked up without restarting Emacs."
  (let ((mtime (file-attribute-modification-time (file-attributes idx))))
    (unless (and org-scribe--mythes-index-cache
                 (equal (nth 0 org-scribe--mythes-index-cache) idx)
                 (equal (nth 1 org-scribe--mythes-index-cache) mtime))
      (setq org-scribe--mythes-index-cache
            (cons idx (cons mtime (org-scribe--mythes-load-index idx)))))
    (cddr org-scribe--mythes-index-cache)))

;;;###autoload
(defun org-scribe-mythes-clear-cache ()
  "Discard the cached MyThes index, forcing a reload on the next lookup."
  (interactive)
  (setq org-scribe--mythes-index-cache nil))

;;; Entry reading

(defun org-scribe--mythes-parse-meaning (line)
  "Parse a MyThes meaning LINE into (POS . SYNONYMS).
POS is nil when the field is the placeholder \"-\"; otherwise it is the
label with its surrounding parentheses stripped."
  (let* ((fields (split-string line "|"))
         (pos (car fields))
         (syns (delete "" (cdr fields))))
    (cons (unless (member pos '("-" ""))
            (if (string-match "\\`(\\(.*\\))\\'" pos)
                (match-string 1 pos)
              pos))
          syns)))

(defun org-scribe--mythes-read-entry (dat offset coding)
  "Read the MyThes entry at byte OFFSET in DAT, decoding with CODING.
Return a list of (POS . SYNONYMS) meaning groups, or nil."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally dat)
    ;; Emacs point is 1-based; the stored offsets are 0-based byte counts.
    (goto-char (min (1+ offset) (point-max)))
    (cl-flet ((line ()
                (let ((raw (buffer-substring-no-properties
                            (point) (line-end-position))))
                  ;; Defensive: a literal read leaves CR in place on CRLF files.
                  (string-trim-right (decode-coding-string raw coding) "\r"))))
      (let* ((head (line))
             (sep (string-match-p "|[0-9]+\\'" head))
             (count (and sep (string-to-number (substring head (1+ sep)))))
             (groups nil))
        (when (and count (> count 0))
          (dotimes (_ count)
            (forward-line 1)
            (unless (eobp)
              (let ((group (org-scribe--mythes-parse-meaning (line))))
                (when (cdr group)
                  (push group groups)))))
          (nreverse groups))))))

(defun org-scribe--mythes-dedupe (groups)
  "Remove GROUPS whose synonym sets duplicate an earlier group.
The Spanish data contains meaning lines that differ only in the order of
their members; showing them twice reads as a bug in the reader."
  (let ((seen nil) (result nil))
    (dolist (group groups (nreverse result))
      (let ((key (sort (copy-sequence (cdr group)) #'string<)))
        (unless (member key seen)
          (push key seen)
          (push group result))))))

;;; Public lookup

(defvar org-scribe-mythes-lemma-function nil
  "Function returning candidate dictionary forms of a word, or nil.
Called with one string, expected to return an ordered list of lemmas.

When nil, `org-scribe-lemmas' is used if it has been loaded.  Resolving the
function this way rather than requiring the lemma module keeps this file free
of a hard dependency on a subprocess-spawning one, and gives tests a seam to
bind.")

(defun org-scribe--mythes-lemma-function ()
  "Return the lemmatizer to use, or nil when none is available."
  (or org-scribe-mythes-lemma-function
      (and (fboundp 'org-scribe-lemmas) #'org-scribe-lemmas)))

(defun org-scribe--mythes-candidates (word)
  "Return the headwords to try for WORD, in order.

The word as written comes first, then its lowercase form — MyThes headwords
are lowercase, so a word capitalized at the start of a sentence still
resolves.  Lemmas follow, since the thesaurus is indexed by dictionary form
while manuscripts contain inflected ones: without them \"corriendo\" simply
misses.  Every lemma is offered in its own case and lowercased too."
  (let* ((w (string-trim word))
         (candidates (list w (downcase w))))
    (when-let* ((fn (org-scribe--mythes-lemma-function)))
      (dolist (lemma (funcall fn w))
        (setq candidates (append candidates (list lemma (downcase lemma))))))
    (delete-dups (delete "" candidates))))

;;;###autoload
(defun org-scribe-mythes-lookup-entry (word)
  "Look up WORD in the offline MyThes thesaurus.

Return (HEADWORD . GROUPS), where HEADWORD is the entry that actually
answered — which may differ from WORD when a lemma was used — and GROUPS is
a list of (POS . SYNONYMS) meaning groups.  Return nil when nothing matched
or no thesaurus is installed.

Callers should disclose HEADWORD when it differs from WORD: a writer who
asked about \"corriendo\" and is shown synonyms for \"correr\" must be able
to see that a substitution happened, or the tool looks like it misread the
question."
  (when-let* ((files (org-scribe-mythes-files))
              (dat (car files))
              (idx (cdr files))
              (table (org-scribe--mythes-index idx))
              (coding (org-scribe--mythes-coding-system dat)))
    (catch 'found
      (dolist (key (org-scribe--mythes-candidates word))
        (when-let* ((offset (gethash key table))
                    (groups (org-scribe--mythes-dedupe
                             (org-scribe--mythes-read-entry dat offset coding))))
          (throw 'found (cons key groups))))
      nil)))

;;;###autoload
(defun org-scribe-mythes-lookup (word)
  "Look up WORD in the offline MyThes thesaurus.
Return a list of (POS . SYNONYMS) meaning groups, or nil.  POS is nil for
entries carrying no grammatical label, which is the common case in the
Spanish data.  Use `org-scribe-mythes-lookup-entry' when it matters which
headword answered."
  (cdr (org-scribe-mythes-lookup-entry word)))

(provide 'org-scribe-mythes)

;;; org-scribe-mythes.el ends here
