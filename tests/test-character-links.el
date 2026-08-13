;;; test-character-links.el --- Tests for character linking system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for the character linking module.
;; Tests character timeline functionality, helper functions,
;; and link creation.
;;
;; Note: The helper functions for extracting text from ID links
;; are tested in test-search-links.el and work for all link types
;; (characters, locations, and plot threads).

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

(require 'org-scribe-character-links)

;;; Function Availability Tests

(ert-deftest test-character-links-functions-defined ()
  "The hand-written half of the character API is defined.

The character entity deliberately does not generate these four through
`org-scribe-define-entity' — PoV needs its own single-value handling, so
they are written out in linking/org-scribe-character-links.el.  The
generated half is covered for every entity at once by
`test-entity-registry-api-is-generated' in test-sistema-templates.el, so
listing it again here would only duplicate that, and go stale."
  (should (fboundp 'org-scribe-set-pov-character))
  (should (fboundp 'org-scribe-jump-to-pov-character))
  (should (fboundp 'org-scribe-link-scene-characters))
  (should (fboundp 'org-scribe-link-all-scene-characters)))

;;; org-scribe--entity-name-at-point
;;
;; Tested here rather than per entity: every entity type resolves its name
;; through this one function in linking/org-scribe-linking-core.el, and the
;; per-entity `org-scribe--get-*-name-at-point' names are defaliases for it.

(ert-deftest test-entity-name-at-point-uses-heading ()
  "With no NAME property, the heading text is the entity name.
This is what lets the template ship placeholder headings the writer
overwrites with the real name."
  (with-temp-buffer
    (org-mode)
    (insert "* Alice Moreau\n")
    (goto-char (point-min))
    (should (equal (org-scribe--entity-name-at-point) "Alice Moreau"))))

(ert-deftest test-entity-name-at-point-prefers-name-property ()
  "An explicit NAME property overrides the heading text.
It is the escape hatch for an entity whose heading is not its name."
  (with-temp-buffer
    (org-mode)
    (insert "* The Antagonist\n:PROPERTIES:\n:NAME: Victor Sarraute\n:END:\n")
    (goto-char (point-min))
    (should (equal (org-scribe--entity-name-at-point) "Victor Sarraute"))))

(ert-deftest test-entity-name-at-point-strips-heading-decoration ()
  "TODO keywords, priorities and tags are not part of the entity name.
A name carrying a tag would not match the same entity elsewhere, so link
display names and completion would silently split in two."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO [#A] Alice Moreau :draft:\n")
    (goto-char (point-min))
    (should (equal (org-scribe--entity-name-at-point) "Alice Moreau"))))

;;; org-scribe--link-entity-in-property Tests (L4)

(ert-deftest test-character-link-entity-in-property-comma-in-name ()
  "A known character name containing a comma is linked as one entity, not split (L4)."
  (let ((id-alist '(("Smith, John" . ("char-smith-001" . "Smith, John"))
                    ("Sam" . ("char-sam-002" . "Sam")))))
    (cl-letf (((symbol-function 'org-scribe--get-all-entities)
               (lambda (_entity) id-alist)))
      (with-temp-buffer
        (org-mode)
        (insert "* Scene One\n:PROPERTIES:\n:Characters: Smith, John, Sam\n:END:\n")
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (let ((changed (org-scribe--link-entity-in-property
                        org-scribe--character-entity 'characters)))
          (should changed)
          (let ((updated (org-entry-get nil "Characters")))
            (should (string-match-p (regexp-quote "[[id:char-smith-001][Smith, John]]") updated))
            (should (string-match-p (regexp-quote "[[id:char-sam-002][Sam]]") updated))))))))

;;; org-scribe--entity-completion-items Tests (L5)

(ert-deftest test-entity-completion-items-unique-names-unchanged ()
  "Unique names are used as-is for the completion label."
  (let* ((items '(("Alex" . ("char-alex-001" . "Alex"))
                  ("Sam" . ("char-sam-002" . "Sam"))))
         (result (org-scribe--entity-completion-items items)))
    (should (equal (mapcar #'car result) '("Alex" "Sam")))
    (should (equal (assoc "Alex" result) '("Alex" "char-alex-001" . "Alex")))))

(ert-deftest test-entity-completion-items-duplicate-names-disambiguated ()
  "Duplicate names get an ID suffix so completion can tell them apart (L5)."
  (let* ((items '(("Alex" . ("char-alex-001" . "Alex"))
                  ("Alex" . ("char-alex-002" . "Alex"))
                  ("Sam" . ("char-sam-003" . "Sam"))))
         (result (org-scribe--entity-completion-items items)))
    ;; Labels are now unique.
    (should (= (length (delete-dups (mapcar #'car result))) 3))
    ;; Each disambiguated label still resolves to its own ID and the true name.
    (should (equal (assoc "Alex (char-alex-001)" result)
                   '("Alex (char-alex-001)" "char-alex-001" . "Alex")))
    (should (equal (assoc "Alex (char-alex-002)" result)
                   '("Alex (char-alex-002)" "char-alex-002" . "Alex")))
    ;; The unrelated unique name is untouched.
    (should (equal (assoc "Sam" result) '("Sam" "char-sam-003" . "Sam")))))

;;; org-scribe--insert-entity-link Tests (L5)

(ert-deftest test-insert-entity-link-picks-correct-duplicate ()
  "Selecting a disambiguated label inserts a link to that entity's own ID (L5).
Regression: previously `assoc' on the raw (possibly duplicate) name always
resolved to whichever entry came first, so picking the second \"Alex\"
still linked to the first Alex's ID."
  (let ((entity (list :error-none-found 'error-none-found
                      :prompt-select 'prompt-select
                      :error-no-id 'error-no-id)))
    (cl-letf (((symbol-function 'org-scribe--get-all-entities)
               (lambda (_entity) '(("Alex" . ("char-alex-001" . "Alex"))
                                   ("Alex" . ("char-alex-002" . "Alex")))))
              ((symbol-function 'completing-read)
               (lambda (&rest _) "Alex (char-alex-002)"))
              ((symbol-function 'org-scribe-msg) (lambda (&rest _) "")))
      (with-temp-buffer
        (org-scribe--insert-entity-link entity)
        (should (string= (buffer-string) "[[id:char-alex-002][Alex]]"))))))

;;; Character Timeline Tests

(ert-deftest test-character-timeline-helper-functions-defined ()
  "Test that the character timeline dblock and its helpers are defined."
  (should (fboundp 'org-dblock-write:character-timeline))
  (should (fboundp 'org-scribe--get-all-scenes-with-characters))
  (should (fboundp 'org-scribe--collect-unique-characters))
  (should (fboundp 'org-scribe--character-symbol)))

(ert-deftest test-character-symbol-pov ()
  "Test character symbol for PoV character."
  (should (string= "◆"
                   (org-scribe--character-symbol "Alice" "Alice" '("Bob" "Charlie")))))

(ert-deftest test-character-symbol-present ()
  "Test character symbol for present (non-PoV) character."
  (should (string= "●"
                   (org-scribe--character-symbol "Bob" "Alice" '("Bob" "Charlie")))))

(ert-deftest test-character-symbol-absent ()
  "Test character symbol for absent character."
  (should (string= ""
                   (org-scribe--character-symbol "David" "Alice" '("Bob" "Charlie")))))

(ert-deftest test-character-symbol-pov-precedence ()
  "Test that PoV takes precedence when character in both properties."
  ;; If Alice is PoV and also in Characters list, should show ◆ (not both)
  (should (string= "◆"
                   (org-scribe--character-symbol "Alice" "Alice" '("Alice" "Bob")))))

(ert-deftest test-character-symbol-no-pov ()
  "Test character symbol when no PoV (nil PoV)."
  (should (string= "●"
                   (org-scribe--character-symbol "Alice" nil '("Alice" "Bob"))))
  (should (string= ""
                   (org-scribe--character-symbol "Charlie" nil '("Alice" "Bob")))))

(ert-deftest test-character-symbol-no-characters ()
  "Test character symbol when no Characters list (nil or empty)."
  (should (string= "◆"
                   (org-scribe--character-symbol "Alice" "Alice" nil)))
  (should (string= ""
                   (org-scribe--character-symbol "Bob" "Alice" nil)))
  (should (string= ""
                   (org-scribe--character-symbol "Charlie" "Alice" '()))))

(ert-deftest test-collect-unique-characters ()
  "Test collecting unique character names from scenes."
  (let ((scenes '(("Scene 1" "Ch 1" "Alice" ("Alice" "Bob"))
                  ("Scene 2" "Ch 1" "Bob" ("Bob" "Charlie"))
                  ("Scene 3" "Ch 2" "Alice" ("Alice" "Bob" "Charlie")))))
    ;; Should return unique characters, sorted alphabetically
    (should (equal '("Alice" "Bob" "Charlie")
                   (org-scribe--collect-unique-characters scenes)))))

(ert-deftest test-collect-unique-characters-with-nil ()
  "Test collecting characters when some scenes have nil PoV or Characters."
  (let ((scenes '(("Scene 1" "Ch 1" "Alice" nil)            ; PoV only
                  ("Scene 2" "Ch 1" nil ("Bob" "Charlie"))  ; Characters only
                  ("Scene 3" "Ch 2" "Alice" ("Alice" "Bob")))))
    (should (equal '("Alice" "Bob" "Charlie")
                   (org-scribe--collect-unique-characters scenes)))))

(ert-deftest test-collect-unique-characters-empty-strings ()
  "Test that empty strings are filtered out."
  (let ((scenes '(("Scene 1" "Ch 1" "Alice" ("Alice" ""))
                  ("Scene 2" "Ch 1" "" ("Bob"))
                  ("Scene 3" "Ch 2" "Charlie" ("Charlie")))))
    ;; Empty strings should be filtered
    (should (equal '("Alice" "Bob" "Charlie")
                   (org-scribe--collect-unique-characters scenes)))))

(ert-deftest test-collect-unique-characters-duplicates ()
  "Test that duplicate characters are deduplicated."
  (let ((scenes '(("Scene 1" "Ch 1" "Alice" ("Alice" "Bob"))
                  ("Scene 2" "Ch 1" "Alice" ("Alice" "Bob"))
                  ("Scene 3" "Ch 2" "Alice" ("Alice" "Bob")))))
    ;; Should have each character only once
    (should (equal '("Alice" "Bob")
                   (org-scribe--collect-unique-characters scenes)))))

(ert-deftest test-collect-unique-characters-sorting ()
  "Test that characters are sorted alphabetically."
  (let ((scenes '(("Scene 1" "Ch 1" "Zoe" ("Zoe" "Bob"))
                  ("Scene 2" "Ch 1" "Alice" ("Alice" "Charlie")))))
    ;; Should be alphabetically sorted
    (should (equal '("Alice" "Bob" "Charlie" "Zoe")
                   (org-scribe--collect-unique-characters scenes)))))

(ert-deftest test-collect-unique-characters-empty ()
  "Test collecting characters from empty scenes list."
  (should (equal '() (org-scribe--collect-unique-characters '()))))

;;; Hidden Weight Tests (linking-core helpers + character timeline)

(ert-deftest test-entity-hidden-weight-p-boundaries ()
  "Only negative weights hide; 0 and the 999.0 default do not.
The default matters most: an entity with no Weight property at all must
still appear in the table, merely sorted last."
  (should (org-scribe--entity-hidden-weight-p -1))
  (should (org-scribe--entity-hidden-weight-p -1.0))
  (should (org-scribe--entity-hidden-weight-p -2))
  (should (org-scribe--entity-hidden-weight-p -0.5))
  (should-not (org-scribe--entity-hidden-weight-p 0))
  (should-not (org-scribe--entity-hidden-weight-p 1.0))
  (should-not (org-scribe--entity-hidden-weight-p 999.0))
  (should-not (org-scribe--entity-hidden-weight-p nil)))

(ert-deftest test-parse-weight-rejects-blank-and-nonsense ()
  "A blank or non-numeric Weight parses to nil, never to 0.
`string-to-number' answers 0 for both, and 0 is a real weight that sorts
ahead of everything — so the templates' blank `:Weight:' would otherwise
pin every shipped character to the first columns of the timeline."
  (should (null (org-scribe--parse-weight "")))
  (should (null (org-scribe--parse-weight " ")))
  (should (null (org-scribe--parse-weight "high")))
  (should (null (org-scribe--parse-weight "1-2")))
  (should (null (org-scribe--parse-weight nil)))
  (should (= 0 (org-scribe--parse-weight "0")))
  (should (= 3.0 (org-scribe--parse-weight "3.0")))
  (should (= 3.0 (org-scribe--parse-weight " 3.0 ")))
  (should (= -1 (org-scribe--parse-weight "-1")))
  (should (= -1.0 (org-scribe--parse-weight "-1.0"))))

(ert-deftest test-blank-weight-property-reads-as-unweighted ()
  "A heading with a blank `:Weight:' is not hidden and is not weight 0.
Goes through `org-entry-get' rather than testing the parser alone,
because it is Org's empty-string answer that makes this a live hazard."
  (with-temp-buffer
    (org-mode)
    (insert "* Alice\n:PROPERTIES:\n:Role: Protagonist\n:Weight:\n:END:\n")
    (goto-char (point-min))
    (let ((parsed (org-scribe--parse-weight (org-entry-get nil "Weight"))))
      (should (null parsed))
      (should-not (org-scribe--entity-hidden-weight-p parsed)))))

(ert-deftest test-partition-entities-by-weight ()
  "Partition splits on negative weight and sorts both halves."
  (let* ((weights '(("Alice" . 1.0) ("Zoe" . 2.0)
                    ("Bob" . -1.0) ("Carla" . -1.0) ("Dan" . 999.0)))
         (weight-fn (lambda (name) (alist-get name weights nil nil #'string=)))
         (split (org-scribe--partition-entities-by-weight
                 '("Zoe" "Carla" "Alice" "Dan" "Bob") weight-fn)))
    (should (equal '("Alice" "Zoe" "Dan") (car split)))
    ;; Equal weights fall back to alphabetical, same as the visible half.
    (should (equal '("Bob" "Carla") (cdr split)))))

(ert-deftest test-sort-entities-by-weight-drops-hidden ()
  "`org-scribe--sort-entities-by-weight' returns the visible half only."
  (let* ((weights '(("Alice" . 1.0) ("Bob" . -1.0)))
         (weight-fn (lambda (name) (alist-get name weights nil nil #'string=))))
    (should (equal '("Alice")
                   (org-scribe--sort-entities-by-weight '("Alice" "Bob") weight-fn)))))

(defmacro org-scribe-test--with-character-weights (weights &rest body)
  "Run BODY with `org-scribe--get-character-weight' stubbed from WEIGHTS.
WEIGHTS is an alist of (NAME . WEIGHT); names absent from it get the
999.0 no-property default."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'org-scribe--get-character-weight)
              (lambda (name) (or (alist-get name ,weights nil nil #'string=) 999.0))))
     ,@body))

(ert-deftest test-collect-unique-characters-excludes-hidden ()
  "A character with Weight -1 is left out of the visible column list."
  (let ((scenes '(("Scene 1" "Ch 1" "Alice" ("Alice" "Extra"))
                  ("Scene 2" "Ch 1" "Bob" ("Bob" "Extra")))))
    (org-scribe-test--with-character-weights '(("Extra" . -1.0))
      (should (equal '("Alice" "Bob")
                     (org-scribe--collect-unique-characters scenes))))))

(ert-deftest test-collect-unique-characters-with-hidden-reports-both ()
  "The partitioned collector reports the hidden characters separately."
  (let ((scenes '(("Scene 1" "Ch 1" "Alice" ("Alice" "Extra" "Spear Carrier")))))
    (org-scribe-test--with-character-weights '(("Extra" . -1.0)
                                               ("Spear Carrier" . -2.0))
      (let ((split (org-scribe--collect-unique-characters-with-hidden scenes)))
        (should (equal '("Alice") (car split)))
        (should (equal '("Spear Carrier" "Extra") (cdr split)))))))

(ert-deftest test-collect-unique-characters-all-hidden ()
  "Hiding every character yields an empty column list, not an error."
  (let ((scenes '(("Scene 1" "Ch 1" "Extra" ("Extra")))))
    (org-scribe-test--with-character-weights '(("Extra" . -1.0))
      (let ((split (org-scribe--collect-unique-characters-with-hidden scenes)))
        (should (equal '() (car split)))
        (should (equal '("Extra") (cdr split)))))))

(ert-deftest test-character-timeline-omits-hidden-and-notes-them ()
  "The dblock drops hidden columns and names them in a comment line."
  (let ((scenes '(("Scene 1" "Ch 1" "Alice" ("Alice" "Extra")))))
    (cl-letf (((symbol-function 'org-scribe--get-all-scenes-with-characters)
               (lambda () scenes)))
      (org-scribe-test--with-character-weights '(("Extra" . -1.0))
        (with-temp-buffer
          (org-mode)
          (org-dblock-write:character-timeline nil)
          (let ((text (buffer-string)))
            (should (string-match-p "Alice" text))
            (should-not (string-match-p "| Extra" text))
            ;; Named in a comment line, so it is neither exported nor counted.
            (should (string-match-p "^# .*Extra" text))))))))

(ert-deftest test-character-timeline-show-hidden-restores-columns ()
  "The :show-hidden dblock parameter puts hidden characters back."
  (let ((scenes '(("Scene 1" "Ch 1" "Alice" ("Alice" "Extra")))))
    (cl-letf (((symbol-function 'org-scribe--get-all-scenes-with-characters)
               (lambda () scenes)))
      (org-scribe-test--with-character-weights '(("Extra" . -1.0))
        (with-temp-buffer
          (org-mode)
          (org-dblock-write:character-timeline '(:show-hidden t))
          (let ((text (buffer-string)))
            (should (string-match-p "Extra" text))
            ;; No note: nothing was withheld.
            (should-not (string-match-p "^# " text))))))))

;;; Heading Predicate Tests

(ert-deftest test-character-heading-p-detects-top-level ()
  "org-scribe--character-heading-p matches a level-1 character heading."
  (with-temp-buffer
    (org-mode)
    (insert "* Protagonist\n** Physical Description\n")
    (goto-char (point-min))
    (org-back-to-heading)
    (should (org-scribe--character-heading-p))))

(ert-deftest test-character-heading-p-rejects-subsection ()
  "org-scribe--character-heading-p must not match a subsection under a
character heading (e.g. Physical Description, Personality, Background)."
  (with-temp-buffer
    (org-mode)
    (insert "* Protagonist\n** Physical Description\n** Personality\n** Background\n")
    (goto-char (point-min))
    (dolist (heading '("Physical Description" "Personality" "Background"))
      (goto-char (point-min))
      (search-forward heading)
      (org-back-to-heading)
      (should-not (org-scribe--character-heading-p)))))

(ert-deftest test-character-heading-p-rejects-nested-subsection ()
  "org-scribe--character-heading-p must not match a level-3 subsection."
  (with-temp-buffer
    (org-mode)
    (insert "* Protagonist\n** Goal, Motivation, Conflict\n*** Internal\n")
    (goto-char (point-min))
    (search-forward "Internal")
    (org-back-to-heading)
    (should-not (org-scribe--character-heading-p))))

;;; Short-story Heading Predicate Tests (H10)

(ert-deftest test-character-heading-p-short-story-matches-level-2-under-characters ()
  "In short-story projects, characters are level-2 headings under
\"* Characters\" (see the shipped notes.org template), not level-1.
Regression test for H10: the predicate previously required level 1
unconditionally, so short-story characters were never found."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Characters\n\n** Protagonist: Alice\n:PROPERTIES:\n:TYPE: Protagonist\n:END:\n")
      (goto-char (point-min))
      (search-forward "Alice")
      (org-back-to-heading)
      (should (org-scribe--character-heading-p)))))

(ert-deftest test-character-heading-p-short-story-rejects-characters-wrapper ()
  "The level-1 \"* Characters\" section header itself is not an entity.
Regression test for H10: without a level check, the wrapper heading's own
text (which may match the regexp fallback) could become a phantom
character entity."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Characters\n\n** Protagonist: Alice\n:PROPERTIES:\n:TYPE: Protagonist\n:END:\n")
      (goto-char (point-min))
      (org-back-to-heading)
      (should-not (org-scribe--character-heading-p)))))

(ert-deftest test-character-heading-p-short-story-rejects-level-2-outside-characters ()
  "A level-2 heading under an unrelated section is not a character, even
when its text matches the regexp fallback (e.g. contains \"Antagonist\").
Confirms the parent-section check is load-bearing, not redundant with
the existing property/regexp checks."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Research & References\n\n** Antagonist's real-world inspiration\n")
      (goto-char (point-min))
      (search-forward "Antagonist")
      (org-back-to-heading)
      (should-not (org-scribe--character-heading-p)))))

;;; Run tests

(defun org-scribe-character-links-run-tests ()
  "Run all character linking tests."
  (interactive)
  (ert "^test-character-"))

(provide 'test-character-links)

;;; test-character-links.el ends here
