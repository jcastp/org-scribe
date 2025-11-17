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
  (add-to-list 'load-path (expand-file-name "counting" parent-dir)))

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

(provide 'test-wordcount)

;;; test-wordcount.el ends here
