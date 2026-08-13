;;; make-mythes-fixtures.el --- regenerate the MyThes test fixtures -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Regenerates tests/fixtures/mythes/th_xx_{L1,UTF}_v2.{dat,idx}.
;;
;; The fixtures are generated rather than hand-written because their .idx
;; holds *byte* offsets into the .dat, and a hand-maintained pair drifts the
;; moment anyone edits an entry — producing a broken fixture that fails the
;; tests for a reason unrelated to the code under test.  Computing the offsets
;; here keeps them correct by construction.
;;
;; Run with:
;;   emacs -Q --batch -l tests/fixtures/make-mythes-fixtures.el
;;
;; The entry set deliberately includes:
;;   - `canción', an accented headword placed *before* later entries, so a
;;     reader that confuses byte and character offsets fails on `zapato';
;;   - `abandonar', with several meaning groups and a mix of labelled and
;;     unlabelled ones;
;;   - `alegre', whose two groups hold the same synonyms in a different order,
;;     to exercise deduplication;
;;   - `Madrid', capitalised, for the downcase fallback.
;; `inexistente' is deliberately absent, for the miss path.

;;; Code:

(defconst make-mythes--entries
  '(("abandonar"
     ("(tr.)" "dejar" "desamparar" "desatender")
     ("(prnl.)" "abandonarse" "entregarse" "confiarse")
     ("-" "renunciar" "dimitir" "cesar"))
    ("canción"
     ("(f.)" "copla" "tonada" "melodía"))
    ("alegre"
     ("(adj.)" "contento" "risueño" "jovial")
     ("(adj.)" "jovial" "contento" "risueño"))
    ("madrid"
     ("-" "capital" "villa y corte"))
    ("zapato"
     ("(m.)" "calzado" "bota" "chanclo")))
  "Fixture entries as (HEADWORD (POS SYN...) ...).")

(defun make-mythes--write (dir lang coding declared)
  "Write a fixture pair for LANG into DIR encoded with CODING.
DECLARED is the encoding name written on line 1 of each file."
  (let ((dat (expand-file-name (format "th_%s_v2.dat" lang) dir))
        (idx (expand-file-name (format "th_%s_v2.idx" lang) dir))
        (offsets nil))
    ;; Build the .dat in a unibyte buffer so `point' counts bytes, which is
    ;; exactly what the .idx must record.
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert (encode-coding-string (concat declared "\n") coding))
      (dolist (entry make-mythes--entries)
        (push (cons (car entry) (1- (point))) offsets)
        (insert (encode-coding-string
                 (format "%s|%d\n" (car entry) (length (cdr entry)))
                 coding))
        (dolist (meaning (cdr entry))
          (insert (encode-coding-string
                   (concat (string-join meaning "|") "\n")
                   coding))))
      (let ((coding-system-for-write 'binary))
        (write-region (point-min) (point-max) dat nil 'quiet)))
    (setq offsets (nreverse offsets))
    ;; The .idx is sorted by headword, deliberately *not* by offset, matching
    ;; the shipped data.
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert (encode-coding-string
               (format "%s\n%d\n" declared (length offsets)) coding))
      (dolist (pair (sort (copy-sequence offsets)
                          (lambda (a b) (string< (car a) (car b)))))
        (insert (encode-coding-string
                 (format "%s|%d\n" (car pair) (cdr pair)) coding)))
      (let ((coding-system-for-write 'binary))
        (write-region (point-min) (point-max) idx nil 'quiet)))
    (message "wrote %s and %s" dat idx)))

(let ((dir (expand-file-name
            "mythes" (file-name-directory (or load-file-name buffer-file-name)))))
  (make-directory dir t)
  ;; Latin-1, as the real Spanish data ships.
  (make-mythes--write dir "xx_L1" 'latin-1 "ISO8859-1")
  ;; UTF-8, as newer thesauri ship — the encoding line must drive the decode.
  (make-mythes--write dir "xx_UTF" 'utf-8 "UTF-8"))

;;; make-mythes-fixtures.el ends here
