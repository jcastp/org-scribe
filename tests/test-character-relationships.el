;;; test-character-relationships.el --- Tests for character relationship system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for org-scribe-character-relationships.el.
;; Covers:
;;   - Parsing relationship strings from :RelationshipsData: properties
;;   - Formatting relationships back to strings
;;   - Plain-list and table display formatting
;;   - The interactive add/remove/show/get-all commands, against a
;;     temp-file fixture with `org-scribe-capture-character-file' and
;;     `org-scribe-project-type' stubbed so the entity-lookup machinery
;;     resolves to the fixture instead of a real project.

;;; Code:

(require 'ert)
(require 'cl-lib)

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
  "Test that all surviving relationship functions are defined."
  ;; Parsing
  (should (fboundp 'org-scribe--parse-single-relationship))
  (should (fboundp 'org-scribe--parse-relationships))
  ;; Formatting
  (should (fboundp 'org-scribe--format-relationship))
  (should (fboundp 'org-scribe--relationships-to-string))
  ;; Display
  (should (fboundp 'org-scribe--format-relationship-list))
  (should (fboundp 'org-scribe--format-relationship-table))
  ;; Database
  (should (fboundp 'org-scribe--get-character-relationships))
  (should (fboundp 'org-scribe--find-character-by-name))
  (should (fboundp 'org-scribe--update-character-relationships))
  (should (fboundp 'org-scribe--get-character-relationships-by-name))
  (should (fboundp 'org-scribe--get-all-relationships))
  ;; Interactive
  (should (fboundp 'org-scribe-add-relationship))
  (should (fboundp 'org-scribe-remove-relationship))
  (should (fboundp 'org-scribe-show-character-relationships))
  (should (fboundp 'org-scribe-show-all-relationships))
  (should (fboundp 'org-scribe-setup-character-relationships)))

(ert-deftest test-relationships-variables-defined ()
  "Test that the relationship type variable is defined."
  (should (boundp 'org-scribe-relationship-types))
  (should (listp org-scribe-relationship-types))
  (should (> (length org-scribe-relationship-types) 0)))

;;; org-scribe--parse-single-relationship Tests

(ert-deftest test-relationships-parse-single-basic ()
  "Test parsing a basic relationship string."
  (let ((result (org-scribe--parse-single-relationship
                 "[[id:char-bob-001][Bob]]|friend")))
    (should result)
    (should (string= "char-bob-001" (nth 0 result)))  ; ID
    (should (string= "Bob" (nth 1 result)))            ; Name
    (should (string= "friend" (nth 2 result)))))        ; Type

(ert-deftest test-relationships-parse-single-full ()
  "Test parsing relationship with a multi-word name."
  (let ((result (org-scribe--parse-single-relationship
                 "[[id:char-alice-001][Alice Rivera]]|mentor")))
    (should result)
    (should (string= "char-alice-001" (nth 0 result)))
    (should (string= "Alice Rivera" (nth 1 result)))
    (should (string= "mentor" (nth 2 result)))))

(ert-deftest test-relationships-parse-single-different-types ()
  "Test parsing relationships with various types."
  (dolist (type '("enemy" "rival" "family" "lover" "ally" "colleague"))
    (let* ((rel-string (format "[[id:char-001][Person]]|%s" type))
           (result (org-scribe--parse-single-relationship rel-string)))
      (should result)
      (should (string= type (nth 2 result))))))

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

;;; org-scribe--parse-relationships Tests

(ert-deftest test-relationships-parse-multi-empty ()
  "Test parsing empty/nil relationship property."
  (should (null (org-scribe--parse-relationships nil)))
  (should (null (org-scribe--parse-relationships ""))))

(ert-deftest test-relationships-parse-multi-single ()
  "Test parsing property with one relationship."
  (let ((result (org-scribe--parse-relationships
                 "[[id:char-bob-001][Bob]]|friend")))
    (should result)
    (should (= 1 (length result)))
    (should (string= "Bob" (nth 1 (car result))))))

(ert-deftest test-relationships-parse-multi-multiple ()
  "Test parsing property with multiple relationships."
  (let ((result (org-scribe--parse-relationships
                 "[[id:char-bob-001][Bob]]|friend; [[id:char-carol-001][Carol]]|rival")))
    (should result)
    (should (= 2 (length result)))
    (should (string= "Bob" (nth 1 (nth 0 result))))
    (should (string= "Carol" (nth 1 (nth 1 result))))))

(ert-deftest test-relationships-parse-multi-preserves-order ()
  "Test that parsing preserves relationship order."
  (let* ((rel-string (concat
                      "[[id:char-001][Alice]]|friend; "
                      "[[id:char-002][Bob]]|rival; "
                      "[[id:char-003][Carol]]|mentor"))
         (result (org-scribe--parse-relationships rel-string)))
    (should (= 3 (length result)))
    (should (string= "Alice" (nth 1 (nth 0 result))))
    (should (string= "Bob" (nth 1 (nth 1 result))))
    (should (string= "Carol" (nth 1 (nth 2 result))))))

;;; org-scribe--format-relationship Tests

(ert-deftest test-relationships-format-basic ()
  "Test formatting a relationship to string."
  (should (string= "[[id:char-bob-001][Bob]]|friend"
                   (org-scribe--format-relationship
                    "char-bob-001" "Bob" "friend"))))

(ert-deftest test-relationships-format-round-trip ()
  "Test that format and parse are inverse operations."
  (let* ((id "char-alice-001")
         (name "Alice Rivera")
         (type "mentor")
         (formatted (org-scribe--format-relationship id name type))
         (parsed (org-scribe--parse-single-relationship formatted)))
    (should parsed)
    (should (string= id (nth 0 parsed)))
    (should (string= name (nth 1 parsed)))
    (should (string= type (nth 2 parsed)))))

;;; org-scribe--relationships-to-string Tests

(ert-deftest test-relationships-to-string-single ()
  "Test converting single relationship to string."
  (let ((rels '(("char-bob-001" "Bob" "friend"))))
    (should (string= "[[id:char-bob-001][Bob]]|friend"
                     (org-scribe--relationships-to-string rels)))))

(ert-deftest test-relationships-to-string-multiple ()
  "Test converting multiple relationships to string."
  (let ((rels '(("char-bob-001" "Bob" "friend")
                ("char-carol-001" "Carol" "rival"))))
    (let ((result (org-scribe--relationships-to-string rels)))
      (should (string-match-p "Bob" result))
      (should (string-match-p "Carol" result))
      ;; Should be semicolon-separated
      (should (string-match-p "; " result)))))

(ert-deftest test-relationships-to-string-round-trip ()
  "Test that to-string and parse-relationships are inverse."
  (let* ((original '(("char-alice-001" "Alice" "mentor")
                     ("char-bob-001" "Bob" "rival")))
         (string (org-scribe--relationships-to-string original))
         (parsed (org-scribe--parse-relationships string)))
    (should (= (length original) (length parsed)))
    (dotimes (i (length original))
      (should (string= (nth 0 (nth i original))
                       (nth 0 (nth i parsed))))
      (should (string= (nth 1 (nth i original))
                       (nth 1 (nth i parsed))))
      (should (string= (nth 2 (nth i original))
                       (nth 2 (nth i parsed)))))))

;;; org-scribe--format-relationship-list Tests

(ert-deftest test-relationships-format-list-basic ()
  "Test that the plain-list formatter names the character and each relation."
  (let* ((rels '(("char-bob-001" "Bob" "friend")))
         (text (org-scribe--format-relationship-list "Alice" rels)))
    (should (stringp text))
    (should (string-match-p "Alice" text))
    (should (string-match-p "Bob" text))
    (should (string-match-p "friend" text))))

(ert-deftest test-relationships-format-list-multiple-lines ()
  "Test that each relationship gets its own line."
  (let* ((rels '(("char-bob-001" "Bob" "friend")
                 ("char-carol-001" "Carol" "rival")))
         (text (org-scribe--format-relationship-list "Alice" rels)))
    (should (string-match-p "- friend: Bob" text))
    (should (string-match-p "- rival: Carol" text))))

(ert-deftest test-relationships-format-list-empty ()
  "Test the plain-list formatter with no relationships."
  (let ((text (org-scribe--format-relationship-list "Alice" '())))
    (should (stringp text))
    (should (string-match-p "Alice" text))))

;;; org-scribe--format-relationship-table Tests

(ert-deftest test-relationships-format-table-basic ()
  "Test basic table format."
  (let* ((all-rels '(("Alice" .
                      (("char-bob-001" "Bob" "friend")))))
         (table (org-scribe--format-relationship-table all-rels)))
    (should (stringp table))
    (should (string-match-p "Alice" table))
    (should (string-match-p "Bob" table))
    (should (string-match-p "friend" table))
    ;; Should be org table format with | separators
    (should (string-match-p "|" table))))

(ert-deftest test-relationships-format-table-header ()
  "Test that table includes header row, with no Strength/Sentiment columns."
  (let* ((all-rels '(("Alice" .
                      (("char-bob-001" "Bob" "friend")))))
         (table (org-scribe--format-relationship-table all-rels)))
    (should (string-match-p "Character" table))
    (should (string-match-p "Related To" table))
    (should (string-match-p "Type" table))
    (should-not (string-match-p "Strength" table))
    (should-not (string-match-p "Sentiment" table))))

(ert-deftest test-relationships-format-table-empty ()
  "Test table format with empty relationships."
  (let ((table (org-scribe--format-relationship-table '())))
    (should (stringp table))))

;;; Interactive-command fixture
;;
;; `org-scribe--get-character-file' and `org-scribe--get-all-characters'
;; both resolve through `org-scribe-capture-character-file' (see
;; org-scribe-linking-core.el's :file-fn slot), so stubbing that one
;; function is enough to point the whole entity-lookup chain at a fixture
;; file instead of a real project.

(defmacro org-scribe-relationships-test--with-fixture (&rest body)
  "Run BODY with a two-character temp file wired in as the character DB."
  (declare (indent 0))
  `(let* ((temp-file (make-temp-file "test-rel-chars-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file temp-file
             (insert "* Alice\n")
             (insert ":PROPERTIES:\n:ID: char-alice-001\n:Role: Protagonist\n:END:\n\n")
             (insert "* Bob\n")
             (insert ":PROPERTIES:\n:ID: char-bob-001\n:Role: Ally\n:END:\n\n")
             (insert "* Carol\n")
             (insert ":PROPERTIES:\n:ID: char-carol-001\n:Role: Rival\n:END:\n\n"))
           (cl-letf (((symbol-function 'org-scribe-capture-character-file)
                      (lambda (&optional _create) temp-file))
                     ((symbol-function 'org-scribe-project-type)
                      (lambda () 'novel)))
             ,@body))
       (let ((buf (find-buffer-visiting temp-file)))
         (when buf (kill-buffer buf)))
       (delete-file temp-file))))

(ert-deftest test-relationships-add-writes-property ()
  "org-scribe-add-relationship stores a new SOURCE -> (TARGET, TYPE) entry."
  (org-scribe-relationships-test--with-fixture
    (cl-letf (((symbol-function 'completing-read)
               (let ((answers '("Alice" "Bob" "mentor")))
                 (lambda (&rest _args)
                   (pop answers)))))
      (org-scribe-add-relationship))
    (let ((rels (org-scribe--get-character-relationships-by-name "Alice")))
      (should (= 1 (length rels)))
      (should (string= "char-bob-001" (nth 0 (car rels))))
      (should (string= "Bob" (nth 1 (car rels))))
      (should (string= "mentor" (nth 2 (car rels)))))))

(ert-deftest test-relationships-add-then-remove ()
  "org-scribe-remove-relationship deletes the selected entry only."
  (org-scribe-relationships-test--with-fixture
    (org-scribe--update-character-relationships
     "Alice" '(("char-bob-001" "Bob" "mentor")
               ("char-carol-001" "Carol" "rival")))
    (cl-letf (((symbol-function 'completing-read)
               (let ((answers '("Alice" "Bob (mentor)")))
                 (lambda (&rest _args)
                   (pop answers)))))
      (org-scribe-remove-relationship))
    (let ((rels (org-scribe--get-character-relationships-by-name "Alice")))
      (should (= 1 (length rels)))
      (should (string= "Carol" (nth 1 (car rels)))))))

(ert-deftest test-relationships-get-all-across-project ()
  "org-scribe--get-all-relationships collects every character's relationships."
  (org-scribe-relationships-test--with-fixture
    (org-scribe--update-character-relationships
     "Alice" '(("char-bob-001" "Bob" "friend")))
    (org-scribe--update-character-relationships
     "Bob" '(("char-carol-001" "Carol" "rival")))
    (let ((all (org-scribe--get-all-relationships)))
      (should (= 2 (length all)))
      (should (assoc "Alice" all))
      (should (assoc "Bob" all))
      (should-not (assoc "Carol" all)))))

(ert-deftest test-relationships-show-character-lists-relationships ()
  "org-scribe-show-character-relationships renders the plain list for the selection."
  (org-scribe-relationships-test--with-fixture
    (org-scribe--update-character-relationships
     "Alice" '(("char-bob-001" "Bob" "friend")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "Alice")))
      (org-scribe-show-character-relationships))
    (unwind-protect
        (with-current-buffer "*Relationships: Alice*"
          (should (string-match-p "Alice" (buffer-string)))
          (should (string-match-p "friend: Bob" (buffer-string))))
      (kill-buffer "*Relationships: Alice*"))))

;;; Run tests

(defun org-scribe-relationships-run-tests ()
  "Run all character relationship tests."
  (interactive)
  (ert "^test-relationships-"))

(provide 'test-character-relationships)

;;; test-character-relationships.el ends here
