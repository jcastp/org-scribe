;;; test-messages.el --- Tests for org-scribe messages -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for the centralized message system in org-scribe-messages.el.
;; Covers message retrieval, format argument substitution, pluralization,
;; fallback behavior for missing keys, and English/Spanish parity.

;;; Code:

(require 'ert)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory)))

(require 'org-scribe-messages)

;;; Function Availability Tests

(ert-deftest test-messages-functions-defined ()
  "Test that message functions and both language alists are defined."
  (should (fboundp 'org-scribe-msg))
  (should (fboundp 'org-scribe-plural))
  (should (boundp 'org-scribe-messages-en))
  (should (boundp 'org-scribe-messages-es))
  (should (boundp 'org-scribe-message-language)))

;;; org-scribe-msg Tests

(ert-deftest test-messages-msg-simple ()
  "Test retrieving a message without format arguments."
  (should (string= "New scene" (org-scribe-msg 'default-scene-name)))
  (should (string= "New chapter" (org-scribe-msg 'default-chapter-name))))

(ert-deftest test-messages-msg-with-single-arg ()
  "Test retrieving a message with one format argument."
  (should (string= "Inserted link to Alice"
                   (org-scribe-msg 'msg-inserted-link "Alice")))
  (should (string= "Set PoV to Bob"
                   (org-scribe-msg 'msg-set-pov "Bob"))))

(ert-deftest test-messages-msg-with-multiple-args ()
  "Test retrieving a message with multiple format arguments."
  (should (string= "Updated character links in 5 scenes"
                   (org-scribe-msg 'msg-updated-links 5 "s")))
  (should (string= "Updated character links in 1 scene"
                   (org-scribe-msg 'msg-updated-links 1 ""))))

(ert-deftest test-messages-msg-with-string-and-number ()
  "Test message formatting with mixed arg types."
  (should (string= "Novel project 'My Novel' created successfully at /tmp/my-novel"
                   (org-scribe-msg 'project-creation-success-novel
                                   "My Novel" "/tmp/my-novel"))))

(ert-deftest test-messages-msg-unknown-key-falls-back-to-symbol-name ()
  "An unregistered key never signals an error — it falls back to its own
symbol name (Phase 8: no hard error, matching the English → symbol-name
fallback chain used for keys missing from a single language)."
  (should (string= "this-key-does-not-exist"
                   (org-scribe-msg 'this-key-does-not-exist))))

(ert-deftest test-messages-msg-returns-string ()
  "Test that org-scribe-msg always returns a string."
  (should (stringp (org-scribe-msg 'default-scene-name)))
  (should (stringp (org-scribe-msg 'msg-inserted-link "X")))
  (should (stringp (org-scribe-msg 'error-empty-title))))

(ert-deftest test-messages-msg-all-error-keys-exist ()
  "Test that commonly used error keys are defined."
  (should (org-scribe-msg 'error-empty-title))
  (should (org-scribe-msg 'error-path-separator))
  (should (org-scribe-msg 'error-no-characters-found))
  (should (org-scribe-msg 'error-no-locations-found))
  (should (org-scribe-msg 'error-no-plot-threads-found)))

(ert-deftest test-messages-msg-all-prompt-keys-exist ()
  "Test that search/prompt keys are defined."
  (should (org-scribe-msg 'search-pov-prompt))
  (should (org-scribe-msg 'search-char-prompt))
  (should (org-scribe-msg 'search-plot-prompt))
  (should (org-scribe-msg 'search-loc-prompt))
  (should (org-scribe-msg 'prompt-select-character))
  (should (org-scribe-msg 'prompt-select-location))
  (should (org-scribe-msg 'prompt-select-plot-thread)))

;;; org-scribe-plural Tests

(ert-deftest test-messages-plural-singular ()
  "Test plural returns singular suffix when count is 1."
  (should (string= "" (org-scribe-plural 1 "")))
  (should (string= "x" (org-scribe-plural 1 "x"))))

(ert-deftest test-messages-plural-plural ()
  "Test plural returns 's' when count is not 1."
  (should (string= "s" (org-scribe-plural 0 "")))
  (should (string= "s" (org-scribe-plural 2 "")))
  (should (string= "s" (org-scribe-plural 5 "")))
  (should (string= "s" (org-scribe-plural 100 ""))))

(ert-deftest test-messages-plural-negative ()
  "Test plural with negative count returns 's'."
  (should (string= "s" (org-scribe-plural -1 ""))))

(ert-deftest test-messages-plural-in-message ()
  "Test using plural inside a message."
  (should (string= "Updated character links in 1 scene"
                   (org-scribe-msg 'msg-updated-links 1 (org-scribe-plural 1 ""))))
  (should (string= "Updated character links in 3 scenes"
                   (org-scribe-msg 'msg-updated-links 3 (org-scribe-plural 3 "")))))

;;; Message Repository Tests

(ert-deftest test-messages-repository-is-alist ()
  "Every registered language pack's `:messages' section is a proper alist.
Generalized over `org-scribe-languages' rather than the two shipped
`org-scribe-messages-en'/`-es' variables, so a third registered
language pack is covered automatically -- see \"i18n-extended.org\",
Step 8."
  (dolist (lang (org-scribe-languages))
    (let ((table (plist-get (org-scribe-lang-pack lang) :messages)))
      (should (listp table))
      (should (> (length table) 0))
      (dolist (entry table)
        (should (consp entry))
        (should (symbolp (car entry)))
        (should (stringp (cdr entry)))))))

;;; Cross-Language Parity Tests

(defun test-messages--format-spec-count (template)
  "Return the number of %s/%d specifiers in TEMPLATE."
  (let ((count 0) (pos 0))
    (while (string-match "%[sd]" template pos)
      (setq count (1+ count))
      (setq pos (match-end 0)))
    count))

(ert-deftest test-messages-parity-same-key-sets ()
  "Every registered language pack defines exactly the same set of
message keys, each compared against the first-registered language
\(English, in registration order -- see `org-scribe-languages').  With
exactly the two shipped packs this is the original English/Spanish
check; a third registered pack is covered by the same loop, with no
test edit needed to add it."
  (let* ((languages (org-scribe-languages))
         (reference-keys (sort (mapcar #'car (plist-get (org-scribe-lang-pack (car languages)) :messages))
                               #'string<)))
    (dolist (lang (cdr languages))
      (should (equal reference-keys
                     (sort (mapcar #'car (plist-get (org-scribe-lang-pack lang) :messages))
                          #'string<))))))

(ert-deftest test-messages-parity-no-duplicate-keys ()
  "No registered language pack's `:messages' table defines the same key twice."
  (dolist (lang (org-scribe-languages))
    (let ((keys (mapcar #'car (plist-get (org-scribe-lang-pack lang) :messages))))
      (should (= (length keys) (length (delete-dups (copy-sequence keys))))))))

(ert-deftest test-messages-parity-format-spec-counts-match ()
  "For every key, the number of %s/%d specifiers matches across every
registered language pack, each compared against the first-registered
language.  This does not require the same order or types swapped, only
the same count — `format' has no positional specifiers in Elisp, so the
specifier sequence (not just its length) must actually match for a
translation to be correct, but a mismatched *count* is always a
translation bug and is what this test catches mechanically."
  (let* ((languages (org-scribe-languages))
         (reference-table (plist-get (org-scribe-lang-pack (car languages)) :messages)))
    (dolist (lang (cdr languages))
      (let ((table (plist-get (org-scribe-lang-pack lang) :messages)))
        (dolist (entry reference-table)
          (let* ((key (car entry))
                 (reference-template (cdr entry))
                 (template (alist-get key table)))
            (should template)
            (should (= (test-messages--format-spec-count reference-template)
                      (test-messages--format-spec-count template)))))))))

(ert-deftest test-messages-fallback-to-english-when-es-key-missing ()
  "Lookup falls back to English when a key is deliberately absent from
a (temporary, local) Spanish table."
  (let* ((org-scribe-messages-es
          (assoc-delete-all 'default-scene-name
                            (copy-alist org-scribe-messages-es)))
         (org-scribe-message-language 'es))
    (should (string= "New scene" (org-scribe-msg 'default-scene-name)))))

(ert-deftest test-messages-language-switch-changes-output ()
  "Switching `org-scribe-message-language' switches which alist is used."
  (let ((org-scribe-message-language 'en))
    (should (string= "New scene" (org-scribe-msg 'default-scene-name))))
  (let ((org-scribe-message-language 'es))
    (should (string= "Escena nueva" (org-scribe-msg 'default-scene-name)))))

(ert-deftest test-messages-spanish-lookup-basic ()
  "Spot-check a few Spanish translations directly, including format args."
  (let ((org-scribe-message-language 'es))
    (should (string= "Escena nueva" (org-scribe-msg 'default-scene-name)))
    (should (string= "Seleccionar personaje: " (org-scribe-msg 'prompt-select-character)))
    (should (string= "Enlace insertado a Alicia"
                     (org-scribe-msg 'msg-inserted-link "Alicia")))))

;;; Run tests

(defun org-scribe-messages-run-tests ()
  "Run all message tests."
  (interactive)
  (ert "^test-messages-"))

(provide 'test-messages)

;;; test-messages.el ends here
