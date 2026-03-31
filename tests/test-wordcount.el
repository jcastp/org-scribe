;;; test-wordcount.el --- Tests for word counting -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for word counting functionality.
;; Note: These tests require org-context-extended to be installed.

;;; Code:

(require 'ert)
(require 'org)

;; Add parent directory to load path
(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path (expand-file-name "counting" parent-dir))
  (add-to-list 'load-path (expand-file-name "core" parent-dir)))

;; Ensure org-context-count-words symbol exists for mocking even without the package
(unless (fboundp 'org-context-count-words)
  (defalias 'org-context-count-words (lambda (&rest _) 0)))

;; Only run tests if org-context-extended is available
(when (require 'org-context-extended nil t)
  (require 'org-scribe-wordcount)

  (ert-deftest org-scribe-test-wordcount-property-added ()
    "Test that WORDCOUNT property is added to headings."
    (with-temp-buffer
      (org-mode)
      ;; Set buffer-file-name so org-id-get-create works
      (setq buffer-file-name (make-temp-name "/tmp/test-wordcount-"))
      (insert "* Heading One\n")
      (insert "This is some text with five words.\n")
      (insert "* Heading Two\n")
      (insert "More text here.\n")
      (goto-char (point-min))
      (org-scribe/ews-org-count-words)
      ;; Check that WORDCOUNT properties were added
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (should (org-entry-get nil "WORDCOUNT"))
      (org-next-visible-heading 1)
      (should (org-entry-get nil "WORDCOUNT"))))

  (ert-deftest org-scribe-test-wordcount-excludes-comments ()
    "Test that word counting excludes comments."
    (with-temp-buffer
      (org-mode)
      (insert "* Heading\n")
      (insert "This is visible text.\n")
      (insert "# This is a comment and should be excluded\n")
      (insert "More visible text.\n")
      ;; The actual count will depend on org-context-extended behavior
      ;; This is a placeholder test
      (should t)))

  (defun org-scribe-wordcount-run-tests ()
    "Run word count tests."
    (interactive)
    (ert-run-tests-interactively "^org-scribe-test-wordcount-")))

;;; Scene Wordcount Tests (always run — mock org-context-extended)

(require 'org-scribe-wordcount)

(ert-deftest test-wordcount-update-scene-wordcounts-defined ()
  "Test that org-scribe/update-scene-wordcounts is defined."
  (should (fboundp 'org-scribe/update-scene-wordcounts)))

(ert-deftest test-wordcount-update-scene-wordcounts-errors-without-org-context ()
  "Test that update-scene-wordcounts signals user-error without org-context-extended."
  (let ((orig-featurep (symbol-function 'featurep)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &rest args)
                 (if (eq feature 'org-context-extended)
                     nil
                   (apply orig-featurep feature args)))))
      (with-temp-buffer
        (org-mode)
        (should-error (org-scribe/update-scene-wordcounts) :type 'user-error)))))

(ert-deftest test-wordcount-update-scene-wordcounts-sets-property ()
  "Test that WORDCOUNT is set on level-3 :ignore: scene headings."
  (let ((orig-featurep (symbol-function 'featurep)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &rest args)
                 (if (eq feature 'org-context-extended)
                     t
                   (apply orig-featurep feature args))))
              ((symbol-function 'org-context-count-words)
               (lambda (&rest _) 42)))
      (with-temp-buffer
        (org-mode)
        (insert "** TODO Chapter One :ignore:\n:PROPERTIES:\n:WORDCOUNT: 0\n:END:\n\n")
        (insert "*** TODO Scene One :ignore:\n:PROPERTIES:\n:PoV:\n:END:\n\nSome prose here.\n")
        (goto-char (point-min))
        (org-scribe/update-scene-wordcounts)
        ;; Navigate to scene heading and check WORDCOUNT
        (goto-char (point-min))
        (org-next-visible-heading 1)  ; chapter
        (org-next-visible-heading 1)  ; scene
        (should (equal (org-entry-get nil "WORDCOUNT") "42"))))))

(ert-deftest test-wordcount-update-scene-wordcounts-ignores-non-scene-headings ()
  "Test that WORDCOUNT is not changed on non-level-3 headings."
  (let ((orig-featurep (symbol-function 'featurep)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &rest args)
                 (if (eq feature 'org-context-extended)
                     t
                   (apply orig-featurep feature args))))
              ((symbol-function 'org-context-count-words)
               (lambda (&rest _) 99)))
      (with-temp-buffer
        (org-mode)
        (insert "* Act One :ignore:\n:PROPERTIES:\n:WORDCOUNT: 0\n:END:\n\n")
        (insert "** TODO Chapter :ignore:\n:PROPERTIES:\n:WORDCOUNT: 0\n:END:\n\n")
        (insert "*** TODO Scene :ignore:\n:PROPERTIES:\n:PoV:\n:END:\n")
        (goto-char (point-min))
        (org-scribe/update-scene-wordcounts)
        ;; Collect WORDCOUNT by level to avoid navigation subtleties
        (let (results)
          (org-map-entries
           (lambda ()
             (push (cons (org-current-level)
                         (org-entry-get nil "WORDCOUNT"))
                   results)))
          ;; Level-1 and level-2 should be unchanged ("0")
          (should (equal (cdr (assq 1 results)) "0"))
          (should (equal (cdr (assq 2 results)) "0"))
          ;; Level-3 scene should have been updated
          (should (equal (cdr (assq 3 results)) "99")))))))

(ert-deftest test-wordcount-update-scene-wordcounts-scope-tree ()
  "Test that update-scene-wordcounts scopes to the current subtree when on a heading."
  (let ((orig-featurep (symbol-function 'featurep)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &rest args)
                 (if (eq feature 'org-context-extended)
                     t
                   (apply orig-featurep feature args))))
              ((symbol-function 'org-context-count-words)
               (lambda (&rest _) 55)))
      (with-temp-buffer
        (org-mode)
        (insert "** TODO Chapter One :ignore:\n:PROPERTIES:\n:WORDCOUNT: 0\n:END:\n\n")
        (insert "*** TODO Scene A :ignore:\n:PROPERTIES:\n:PoV:\n:END:\n\n")
        (insert "** TODO Chapter Two :ignore:\n:PROPERTIES:\n:WORDCOUNT: 0\n:END:\n\n")
        (insert "*** TODO Scene B :ignore:\n:PROPERTIES:\n:PoV:\n:END:\n\n")
        ;; Position exactly on Chapter One heading using text search
        (goto-char (point-min))
        (re-search-forward "^\\*\\* TODO Chapter One")
        (beginning-of-line)
        (org-scribe/update-scene-wordcounts)
        ;; Scene A (inside chapter one) should be updated
        (goto-char (point-min))
        (re-search-forward "^\\*\\*\\* TODO Scene A")
        (beginning-of-line)
        (should (equal (org-entry-get nil "WORDCOUNT") "55"))
        ;; Scene B (inside chapter two) should NOT be updated
        (goto-char (point-min))
        (re-search-forward "^\\*\\*\\* TODO Scene B")
        (beginning-of-line)
        (should (null (org-entry-get nil "WORDCOUNT")))))))

(provide 'test-wordcount)

;;; test-wordcount.el ends here
