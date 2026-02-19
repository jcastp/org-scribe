;;; test-link-update.el --- Tests for link display name updates -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for org-scribe-link-update.el.
;; Covers link display name updating logic:
;;   - org-scribe--update-link-display-name (string manipulation)
;;   - org-scribe--build-id-to-name-map (data structure construction)
;;   - org-scribe--update-links-in-property (org property updating)
;;
;; These are the core helpers used when renaming characters, locations,
;; or plot threads in database files.

;;; Code:

(require 'ert)
(require 'org)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../linking" default-directory)))

(require 'org-scribe-link-update)

;;; Module Loading Tests

(ert-deftest test-link-update-module-loads ()
  "Test that org-scribe-link-update module loads without errors."
  (should (featurep 'org-scribe-link-update)))

(ert-deftest test-link-update-functions-defined ()
  "Test that all link update functions are defined."
  (should (fboundp 'org-scribe--update-link-display-name))
  (should (fboundp 'org-scribe--build-id-to-name-map))
  (should (fboundp 'org-scribe--update-links-in-property))
  (should (fboundp 'org-scribe/update-all-link-names)))

;;; org-scribe--update-link-display-name Tests

(defun test-link-update--make-map (&rest pairs)
  "Build a hash table from PAIRS (id1 name1 id2 name2 ...)."
  (let ((map (make-hash-table :test 'equal)))
    (while pairs
      (puthash (car pairs) (cadr pairs) map)
      (setq pairs (cddr pairs)))
    map))

(ert-deftest test-link-update-name-unchanged ()
  "Test that link is unchanged when name already matches."
  (let ((map (test-link-update--make-map "char-alex-001" "Alex Rivera")))
    (should (string= "[[id:char-alex-001][Alex Rivera]]"
                     (org-scribe--update-link-display-name
                      "[[id:char-alex-001][Alex Rivera]]" map)))))

(ert-deftest test-link-update-name-changed ()
  "Test that link display name is updated when name changed in DB."
  (let ((map (test-link-update--make-map "char-alex-001" "Alexandra Rivera")))
    (should (string= "[[id:char-alex-001][Alexandra Rivera]]"
                     (org-scribe--update-link-display-name
                      "[[id:char-alex-001][Alex Rivera]]" map)))))

(ert-deftest test-link-update-id-not-in-map ()
  "Test that link is unchanged when ID not found in map."
  (let ((map (test-link-update--make-map "char-other-001" "Other")))
    ;; ID char-alex-001 not in map - keep as is
    (should (string= "[[id:char-alex-001][Alex]]"
                     (org-scribe--update-link-display-name
                      "[[id:char-alex-001][Alex]]" map)))))

(ert-deftest test-link-update-plain-text-unchanged ()
  "Test that plain text (not a link) is returned unchanged."
  (let ((map (test-link-update--make-map "char-alex-001" "Alex")))
    (should (string= "Plain Text"
                     (org-scribe--update-link-display-name "Plain Text" map)))
    (should (string= "Alex"
                     (org-scribe--update-link-display-name "Alex" map)))))

(ert-deftest test-link-update-empty-map ()
  "Test link update with empty map returns original."
  (let ((map (make-hash-table :test 'equal)))
    (should (string= "[[id:char-alex-001][Alex]]"
                     (org-scribe--update-link-display-name
                      "[[id:char-alex-001][Alex]]" map)))))

(ert-deftest test-link-update-preserves-link-format ()
  "Test that updated link preserves the [[id:...][...]] format."
  (let ((map (test-link-update--make-map "loc-001" "New York City")))
    (let ((result (org-scribe--update-link-display-name
                   "[[id:loc-001][NYC]]" map)))
      ;; Should have proper format
      (should (string-prefix-p "[[id:" result))
      (should (string-match-p "\\[\\[id:loc-001\\]\\[New York City\\]\\]" result)))))

(ert-deftest test-link-update-non-id-link-unchanged ()
  "Test that non-ID links (file:, http:, etc.) are unchanged."
  (let ((map (test-link-update--make-map "test" "Something")))
    ;; File link - not an ID link
    (should (string= "[[file:test.org][Link Text]]"
                     (org-scribe--update-link-display-name
                      "[[file:test.org][Link Text]]" map)))
    ;; HTTP link - not an ID link
    (should (string= "[[https://example.com][Website]]"
                     (org-scribe--update-link-display-name
                      "[[https://example.com][Website]]" map)))))

;;; org-scribe--build-id-to-name-map Tests

(ert-deftest test-link-update-build-map-basic ()
  "Test building ID-to-name map from basic alist."
  (let* ((items '(("Alex Rivera" . ("char-alex-001" . "Alex Rivera"))
                  ("Sam Chen" . ("char-sam-002" . "Sam Chen"))))
         (map (org-scribe--build-id-to-name-map items)))
    (should (hash-table-p map))
    (should (string= "Alex Rivera" (gethash "char-alex-001" map)))
    (should (string= "Sam Chen" (gethash "char-sam-002" map)))))

(ert-deftest test-link-update-build-map-empty ()
  "Test building map from empty alist."
  (let ((map (org-scribe--build-id-to-name-map '())))
    (should (hash-table-p map))
    (should (= 0 (hash-table-count map)))))

(ert-deftest test-link-update-build-map-nil-id-filtered ()
  "Test that entries with nil ID are filtered out."
  (let* ((items '(("Alice" . (nil . "Alice"))
                  ("Bob" . ("char-bob-001" . "Bob"))))
         (map (org-scribe--build-id-to-name-map items)))
    ;; Alice has nil ID, should not be in map
    (should (null (gethash nil map)))
    ;; Bob should be in map
    (should (string= "Bob" (gethash "char-bob-001" map)))))

(ert-deftest test-link-update-build-map-multiple-entries ()
  "Test building map with many entries."
  (let* ((items (list (cons "Alice" (cons "id-001" "Alice"))
                      (cons "Bob" (cons "id-002" "Bob"))
                      (cons "Carol" (cons "id-003" "Carol"))
                      (cons "Dave" (cons "id-004" "Dave"))))
         (map (org-scribe--build-id-to-name-map items)))
    (should (= 4 (hash-table-count map)))
    (should (string= "Alice" (gethash "id-001" map)))
    (should (string= "Dave" (gethash "id-004" map)))))

;;; org-scribe--update-links-in-property Tests

(defmacro test-link-update--with-org-heading (heading-text &rest body)
  "Execute BODY in a temp org buffer at HEADING-TEXT."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,heading-text)
     (goto-char (point-min))
     (org-next-visible-heading 1)
     ,@body))

(ert-deftest test-link-update-property-single-link ()
  "Test updating a property with a single ID link."
  (let ((map (test-link-update--make-map "char-alex-001" "Alexandra Rivera")))
    (test-link-update--with-org-heading
        "* Scene One\n:PROPERTIES:\n:Characters: [[id:char-alex-001][Alex Rivera]]\n:END:\n"
      (let ((changed (org-scribe--update-links-in-property "Characters" map)))
        (should changed)
        (should (string= "[[id:char-alex-001][Alexandra Rivera]]"
                         (org-entry-get nil "Characters")))))))

(ert-deftest test-link-update-property-no-change-needed ()
  "Test that property is not changed when names already match."
  (let ((map (test-link-update--make-map "char-alex-001" "Alex Rivera")))
    (test-link-update--with-org-heading
        "* Scene One\n:PROPERTIES:\n:Characters: [[id:char-alex-001][Alex Rivera]]\n:END:\n"
      (let ((changed (org-scribe--update-links-in-property "Characters" map)))
        (should (null changed))))))

(ert-deftest test-link-update-property-multiple-links ()
  "Test updating a property with multiple comma-separated links."
  (let ((map (test-link-update--make-map
              "char-alex-001" "Alexandra Rivera"
              "char-sam-002" "Samuel Chen")))
    (test-link-update--with-org-heading
        "* Scene One\n:PROPERTIES:\n:Characters: [[id:char-alex-001][Alex]], [[id:char-sam-002][Sam]]\n:END:\n"
      (let ((changed (org-scribe--update-links-in-property "Characters" map)))
        (should changed)
        (let ((updated (org-entry-get nil "Characters")))
          (should (string-match-p "Alexandra Rivera" updated))
          (should (string-match-p "Samuel Chen" updated)))))))

(ert-deftest test-link-update-property-nil-when-missing ()
  "Test that update returns nil when property doesn't exist."
  (let ((map (test-link-update--make-map "char-alex-001" "Alex")))
    (test-link-update--with-org-heading
        "* Scene One\n"
      (let ((changed (org-scribe--update-links-in-property "Characters" map)))
        (should (null changed))))))

;;; Run tests

(defun org-scribe-link-update-run-tests ()
  "Run all link update tests."
  (interactive)
  (ert "^test-link-update-"))

(provide 'test-link-update)

;;; test-link-update.el ends here
