;;; test-character-relationships.el --- Tests for character relationship system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for org-scribe-character-relationships.el.
;; Covers the pure functions that handle relationship data:
;;   - Parsing relationship strings from :RelationshipsData: properties
;;   - Formatting relationships back to strings
;;   - ASCII tree visualization
;;   - Relationship filtering by strength
;;   - Sentiment-to-color mapping for DOT graphs
;;   - Table formatting
;;
;; Interactive functions (add/remove/show) require a live project
;; and are not tested here.

;;; Code:

(require 'ert)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../search" default-directory))
  (add-to-list 'load-path (expand-file-name "../linking" default-directory))
  (add-to-list 'load-path (expand-file-name "../capture" default-directory)))

(require 'org-scribe-character-relationships)

;;; Module Loading Tests

(ert-deftest test-relationships-module-loads ()
  "Test that org-scribe-character-relationships module loads without errors."
  (should (featurep 'org-scribe-character-relationships)))

;;; Function Availability Tests

(ert-deftest test-relationships-functions-defined ()
  "Test that all relationship functions are defined."
  ;; Parsing
  (should (fboundp 'org-scribe--parse-single-relationship))
  (should (fboundp 'org-scribe--parse-relationships))
  ;; Formatting
  (should (fboundp 'org-scribe--format-relationship))
  (should (fboundp 'org-scribe--relationships-to-string))
  ;; Visualization
  (should (fboundp 'org-scribe--ascii-relationship-tree))
  (should (fboundp 'org-scribe--format-relationship-table))
  ;; Filtering
  (should (fboundp 'org-scribe--filter-relationships-by-strength))
  ;; DOT graph
  (should (fboundp 'org-scribe--sentiment-to-color))
  (should (fboundp 'org-scribe--generate-dot-code))
  ;; Interactive
  (should (fboundp 'org-scribe/add-relationship))
  (should (fboundp 'org-scribe/remove-relationship))
  (should (fboundp 'org-scribe/show-character-relationships))
  (should (fboundp 'org-scribe/show-all-relationships))
  (should (fboundp 'org-scribe/setup-character-relationships))
  (should (fboundp 'org-dblock-write:character-relationships)))

(ert-deftest test-relationships-variables-defined ()
  "Test that relationship type variables are defined."
  (should (boundp 'org-scribe-relationship-types))
  (should (boundp 'org-scribe-relationship-sentiments))
  (should (listp org-scribe-relationship-types))
  (should (listp org-scribe-relationship-sentiments))
  (should (> (length org-scribe-relationship-types) 0))
  (should (> (length org-scribe-relationship-sentiments) 0)))

;;; org-scribe--parse-single-relationship Tests

(ert-deftest test-relationships-parse-single-basic ()
  "Test parsing a basic relationship string."
  (let ((result (org-scribe--parse-single-relationship
                 "[[id:char-bob-001][Bob]](friend,3,positive)")))
    (should result)
    (should (string= "char-bob-001" (nth 0 result)))  ; ID
    (should (string= "Bob" (nth 1 result)))            ; Name
    (should (string= "friend" (nth 2 result)))         ; Type
    (should (= 3 (nth 3 result)))                      ; Strength
    (should (string= "positive" (nth 4 result)))))     ; Sentiment

(ert-deftest test-relationships-parse-single-full ()
  "Test parsing relationship with all metadata."
  (let ((result (org-scribe--parse-single-relationship
                 "[[id:char-alice-001][Alice Rivera]](mentor,5,positive)")))
    (should result)
    (should (string= "char-alice-001" (nth 0 result)))
    (should (string= "Alice Rivera" (nth 1 result)))
    (should (string= "mentor" (nth 2 result)))
    (should (= 5 (nth 3 result)))
    (should (string= "positive" (nth 4 result)))))

(ert-deftest test-relationships-parse-single-different-types ()
  "Test parsing relationships with various types."
  (dolist (type '("enemy" "rival" "family" "lover" "ally" "colleague"))
    (let* ((rel-string (format "[[id:char-001][Person]](%s,2,neutral)" type))
           (result (org-scribe--parse-single-relationship rel-string)))
      (should result)
      (should (string= type (nth 2 result))))))

(ert-deftest test-relationships-parse-single-all-sentiments ()
  "Test parsing relationships with all sentiment values."
  (dolist (sentiment '("positive" "negative" "neutral" "complex"))
    (let* ((rel-string (format "[[id:char-001][Person]](friend,3,%s)" sentiment))
           (result (org-scribe--parse-single-relationship rel-string)))
      (should result)
      (should (string= sentiment (nth 4 result))))))

(ert-deftest test-relationships-parse-single-nil ()
  "Test parsing nil returns nil."
  (should (null (org-scribe--parse-single-relationship nil))))

(ert-deftest test-relationships-parse-single-empty-string ()
  "Test parsing empty string returns nil."
  (should (null (org-scribe--parse-single-relationship ""))))

(ert-deftest test-relationships-parse-single-malformed ()
  "Test parsing malformed string returns nil."
  (should (null (org-scribe--parse-single-relationship "not a relationship")))
  (should (null (org-scribe--parse-single-relationship "[[id:x][Name]]")))
  (should (null (org-scribe--parse-single-relationship "[[id:x][Name]](friend)"))))

(ert-deftest test-relationships-parse-single-strength-as-integer ()
  "Test that strength is parsed as an integer."
  (let ((result (org-scribe--parse-single-relationship
                 "[[id:char-001][Bob]](friend,4,positive)")))
    (should (integerp (nth 3 result)))
    (should (= 4 (nth 3 result)))))

;;; org-scribe--parse-relationships Tests

(ert-deftest test-relationships-parse-multi-empty ()
  "Test parsing empty/nil relationship property."
  (should (null (org-scribe--parse-relationships nil)))
  (should (null (org-scribe--parse-relationships ""))))

(ert-deftest test-relationships-parse-multi-single ()
  "Test parsing property with one relationship."
  (let ((result (org-scribe--parse-relationships
                 "[[id:char-bob-001][Bob]](friend,3,positive)")))
    (should result)
    (should (= 1 (length result)))
    (should (string= "Bob" (nth 1 (car result))))))

(ert-deftest test-relationships-parse-multi-multiple ()
  "Test parsing property with multiple relationships."
  (let ((result (org-scribe--parse-relationships
                 "[[id:char-bob-001][Bob]](friend,3,positive); [[id:char-carol-001][Carol]](rival,2,negative)")))
    (should result)
    (should (= 2 (length result)))
    (should (string= "Bob" (nth 1 (nth 0 result))))
    (should (string= "Carol" (nth 1 (nth 1 result))))))

(ert-deftest test-relationships-parse-multi-preserves-order ()
  "Test that parsing preserves relationship order."
  (let* ((rel-string (concat
                      "[[id:char-001][Alice]](friend,5,positive); "
                      "[[id:char-002][Bob]](rival,3,negative); "
                      "[[id:char-003][Carol]](mentor,4,neutral)"))
         (result (org-scribe--parse-relationships rel-string)))
    (should (= 3 (length result)))
    (should (string= "Alice" (nth 1 (nth 0 result))))
    (should (string= "Bob" (nth 1 (nth 1 result))))
    (should (string= "Carol" (nth 1 (nth 2 result))))))

;;; org-scribe--format-relationship Tests

(ert-deftest test-relationships-format-basic ()
  "Test formatting a relationship to string."
  (should (string= "[[id:char-bob-001][Bob]](friend,3,positive)"
                   (org-scribe--format-relationship
                    "char-bob-001" "Bob" "friend" 3 "positive"))))

(ert-deftest test-relationships-format-round-trip ()
  "Test that format and parse are inverse operations."
  (let* ((id "char-alice-001")
         (name "Alice Rivera")
         (type "mentor")
         (strength 5)
         (sentiment "complex")
         (formatted (org-scribe--format-relationship
                     id name type strength sentiment))
         (parsed (org-scribe--parse-single-relationship formatted)))
    (should parsed)
    (should (string= id (nth 0 parsed)))
    (should (string= name (nth 1 parsed)))
    (should (string= type (nth 2 parsed)))
    (should (= strength (nth 3 parsed)))
    (should (string= sentiment (nth 4 parsed)))))

;;; org-scribe--relationships-to-string Tests

(ert-deftest test-relationships-to-string-single ()
  "Test converting single relationship to string."
  (let ((rels '(("char-bob-001" "Bob" "friend" 3 "positive"))))
    (should (string= "[[id:char-bob-001][Bob]](friend,3,positive)"
                     (org-scribe--relationships-to-string rels)))))

(ert-deftest test-relationships-to-string-multiple ()
  "Test converting multiple relationships to string."
  (let ((rels '(("char-bob-001" "Bob" "friend" 3 "positive")
                ("char-carol-001" "Carol" "rival" 2 "negative"))))
    (let ((result (org-scribe--relationships-to-string rels)))
      (should (string-match-p "Bob" result))
      (should (string-match-p "Carol" result))
      ;; Should be semicolon-separated
      (should (string-match-p "; " result)))))

(ert-deftest test-relationships-to-string-round-trip ()
  "Test that to-string and parse-relationships are inverse."
  (let* ((original '(("char-alice-001" "Alice" "mentor" 5 "positive")
                     ("char-bob-001" "Bob" "rival" 2 "negative")))
         (string (org-scribe--relationships-to-string original))
         (parsed (org-scribe--parse-relationships string)))
    (should (= (length original) (length parsed)))
    (dotimes (i (length original))
      (should (string= (nth 0 (nth i original))
                       (nth 0 (nth i parsed))))
      (should (string= (nth 1 (nth i original))
                       (nth 1 (nth i parsed)))))))

;;; org-scribe--ascii-relationship-tree Tests

(ert-deftest test-relationships-ascii-tree-basic ()
  "Test basic ASCII tree generation."
  (let* ((rels '(("char-bob-001" "Bob" "friend" 3 "positive")))
         (tree (org-scribe--ascii-relationship-tree "Alice" rels)))
    (should (stringp tree))
    (should (string-match-p "Alice" tree))
    (should (string-match-p "Bob" tree))
    (should (string-match-p "friend" tree))))

(ert-deftest test-relationships-ascii-tree-last-element ()
  "Test that last element uses └─ prefix."
  (let* ((rels '(("char-bob-001" "Bob" "friend" 3 "positive")))
         (tree (org-scribe--ascii-relationship-tree "Alice" rels)))
    (should (string-match-p "└─" tree))))

(ert-deftest test-relationships-ascii-tree-non-last-element ()
  "Test that non-last elements use ├─ prefix."
  (let* ((rels '(("char-bob-001" "Bob" "friend" 3 "positive")
                 ("char-carol-001" "Carol" "rival" 2 "negative")))
         (tree (org-scribe--ascii-relationship-tree "Alice" rels)))
    (should (string-match-p "├─" tree))  ; Bob (not last)
    (should (string-match-p "└─" tree)))) ; Carol (last)

(ert-deftest test-relationships-ascii-tree-sentiment-symbols ()
  "Test that sentiment is shown as correct symbol."
  ;; positive → +
  (let* ((rels '(("char-bob-001" "Bob" "friend" 3 "positive")))
         (tree (org-scribe--ascii-relationship-tree "Alice" rels)))
    (should (string-match-p "+" tree)))

  ;; negative → -
  (let* ((rels '(("char-bob-001" "Bob" "enemy" 4 "negative")))
         (tree (org-scribe--ascii-relationship-tree "Alice" rels)))
    (should (string-match-p "-" tree)))

  ;; complex → ~
  (let* ((rels '(("char-bob-001" "Bob" "rival" 3 "complex")))
         (tree (org-scribe--ascii-relationship-tree "Alice" rels)))
    (should (string-match-p "~" tree))))

(ert-deftest test-relationships-ascii-tree-empty ()
  "Test ASCII tree with no relationships."
  (let ((tree (org-scribe--ascii-relationship-tree "Alice" '())))
    (should (stringp tree))
    (should (string-match-p "Alice" tree))))

;;; org-scribe--sentiment-to-color Tests

(ert-deftest test-relationships-sentiment-positive ()
  "Test positive sentiment maps to green."
  (should (string= "forestgreen" (org-scribe--sentiment-to-color "positive"))))

(ert-deftest test-relationships-sentiment-negative ()
  "Test negative sentiment maps to red."
  (should (string= "crimson" (org-scribe--sentiment-to-color "negative"))))

(ert-deftest test-relationships-sentiment-complex ()
  "Test complex sentiment maps to purple."
  (should (string= "purple" (org-scribe--sentiment-to-color "complex"))))

(ert-deftest test-relationships-sentiment-neutral ()
  "Test neutral/unknown sentiment maps to gray."
  (should (string= "gray50" (org-scribe--sentiment-to-color "neutral")))
  (should (string= "gray50" (org-scribe--sentiment-to-color "unknown")))
  (should (string= "gray50" (org-scribe--sentiment-to-color ""))))

;;; org-scribe--filter-relationships-by-strength Tests

(ert-deftest test-relationships-filter-by-strength-basic ()
  "Test filtering relationships by minimum strength."
  (let* ((all-rels '(("Alice" .
                      (("char-bob-001" "Bob" "friend" 3 "positive")
                       ("char-carol-001" "Carol" "rival" 1 "negative")
                       ("char-dave-001" "Dave" "ally" 5 "neutral")))
                     ("Bob" .
                      (("char-alice-001" "Alice" "friend" 2 "positive")))))
         (filtered (org-scribe--filter-relationships-by-strength all-rels 3)))
    ;; Alice should have 2 rels (strength 3 and 5)
    (let ((alice-rels (cdr (assoc "Alice" filtered))))
      (should (= 2 (length alice-rels)))
      ;; Carol (strength 1) should be filtered out
      (should (null (cl-find "Carol" alice-rels :key (lambda (r) (nth 1 r))
                             :test #'string=)))
      ;; Bob (strength 3) and Dave (strength 5) should remain
      (should (cl-find "Bob" alice-rels :key (lambda (r) (nth 1 r))
                       :test #'string=)))))

(ert-deftest test-relationships-filter-all-above ()
  "Test filter with threshold that passes all relationships."
  (let* ((all-rels '(("Alice" .
                      (("char-bob-001" "Bob" "friend" 5 "positive")
                       ("char-carol-001" "Carol" "rival" 5 "negative")))))
         (filtered (org-scribe--filter-relationships-by-strength all-rels 1)))
    (should (= 2 (length (cdr (assoc "Alice" filtered)))))))

(ert-deftest test-relationships-filter-all-below ()
  "Test filter with threshold that excludes all relationships."
  (let* ((all-rels '(("Alice" .
                      (("char-bob-001" "Bob" "friend" 1 "positive")
                       ("char-carol-001" "Carol" "rival" 2 "negative")))))
         (filtered (org-scribe--filter-relationships-by-strength all-rels 5)))
    ;; Alice's entry should be removed (all rels below threshold)
    (should (null (assoc "Alice" filtered)))))

;;; org-scribe--format-relationship-table Tests

(ert-deftest test-relationships-format-table-basic ()
  "Test basic table format."
  (let* ((all-rels '(("Alice" .
                      (("char-bob-001" "Bob" "friend" 3 "positive")))))
         (table (org-scribe--format-relationship-table all-rels)))
    (should (stringp table))
    (should (string-match-p "Alice" table))
    (should (string-match-p "Bob" table))
    (should (string-match-p "friend" table))
    ;; Should be org table format with | separators
    (should (string-match-p "|" table))))

(ert-deftest test-relationships-format-table-header ()
  "Test that table includes header row."
  (let* ((all-rels '(("Alice" .
                      (("char-bob-001" "Bob" "friend" 3 "positive")))))
         (table (org-scribe--format-relationship-table all-rels)))
    (should (string-match-p "Character" table))
    (should (string-match-p "Related To" table))
    (should (string-match-p "Type" table))
    (should (string-match-p "Strength" table))
    (should (string-match-p "Sentiment" table))))

(ert-deftest test-relationships-format-table-empty ()
  "Test table format with empty relationships."
  (let ((table (org-scribe--format-relationship-table '())))
    (should (stringp table))))

;;; Run tests

(defun org-scribe-relationships-run-tests ()
  "Run all character relationship tests."
  (interactive)
  (ert "^test-relationships-"))

(provide 'test-character-relationships)

;;; test-character-relationships.el ends here
