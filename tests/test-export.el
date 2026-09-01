;;; test-export.el --- Tests for export filters -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for export filters and scene break replacement.
;; Tests scene break replacement for various export backends.

;;; Code:

(require 'ert)
(require 'org)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../export" default-directory)))

(require 'org-scribe-config)
(require 'org-scribe-export)

;; `org-scribe-mode' itself lives in org-scribe.el (the package entry
;; point), which this test file does not load. Declare it here so the
;; `buffer-local-value' check in `org-scribe--export-in-scribe-context-p'
;; can be exercised in isolation.
(defvar-local org-scribe-mode nil)

;;; Function Availability Tests

(ert-deftest test-export-functions-defined ()
  "Test that export functions are defined."
  (should (fboundp 'org-scribe--export-replace-scene-breaks)))

;;; Scene Break Configuration Tests

(ert-deftest test-scene-break-replacements-configured ()
  "Test that scene break replacements are configured."
  (should (boundp 'org-scribe-scene-break-replacements))
  (should (listp org-scribe-scene-break-replacements))
  (should (> (length org-scribe-scene-break-replacements) 0)))

(ert-deftest test-scene-break-has-backends ()
  "Test that scene break replacements include common backends."
  ;; Should have at least ascii, html, latex, and default (t)
  (should (assoc 'ascii org-scribe-scene-break-replacements))
  (should (assoc 'html org-scribe-scene-break-replacements))
  (should (assoc 'latex org-scribe-scene-break-replacements))
  (should (assoc t org-scribe-scene-break-replacements)))

;;; Scene Break Replacement Tests

(ert-deftest test-scene-break-replacement-is-case-sensitive ()
  "A case-insensitive occurrence of the macro text is left untouched.
The macro always expands to exactly \"SCENE-BREAK\" (uppercase); a
lowercase or mixed-case occurrence elsewhere in the document -- prose
discussing a \"scene break\", or a CSS class name like
\"org-scribe-scene-break\" (the bug this pins: found corrupting
org-scribe-compile's EPUB output, because this filter runs on the
*entire* rendered document under Emacs's usual case-insensitive
default, not just at macro-expansion sites) -- must not be replaced."
  (let ((text "See the org-scribe-scene-break CSS class.\nSCENE-BREAK\nDone.")
        (backend 'ascii))
    (let ((result (org-scribe--export-replace-scene-breaks text backend nil))
          (case-fold-search nil))
      ;; The lowercase occurrence survives verbatim...
      (should (string-match-p "org-scribe-scene-break CSS class" result))
      ;; ...while the real, uppercase macro expansion is gone.
      (should-not (string-match-p "SCENE-BREAK" result)))))

(ert-deftest test-scene-break-replacement-ascii ()
  "Test scene break replacement for ASCII backend."
  (let ((text "Some text before.\nSCENE-BREAK\nSome text after.")
        (backend 'ascii)
        (expected-replacement (alist-get 'ascii org-scribe-scene-break-replacements)))
    (let ((result (org-scribe--export-replace-scene-breaks text backend nil)))
      (should (string-match-p expected-replacement result))
      (should-not (string-match-p "SCENE-BREAK" result)))))

(ert-deftest test-scene-break-replacement-html ()
  "Test scene break replacement for HTML backend."
  (let ((text "Some text before.\nSCENE-BREAK\nSome text after.")
        (backend 'html)
        (expected-replacement (alist-get 'html org-scribe-scene-break-replacements)))
    (let ((result (org-scribe--export-replace-scene-breaks text backend nil)))
      (should (string-match-p expected-replacement result))
      (should-not (string-match-p "SCENE-BREAK" result)))))

(ert-deftest test-scene-break-replacement-latex ()
  "Test scene break replacement for LaTeX backend."
  (let ((text "Some text before.\nSCENE-BREAK\nSome text after.")
        (backend 'latex)
        (expected-replacement (alist-get 'latex org-scribe-scene-break-replacements)))
    (let ((result (org-scribe--export-replace-scene-breaks text backend nil)))
      (should (string-match-p (regexp-quote expected-replacement) result))
      (should-not (string-match-p "SCENE-BREAK" result)))))

(ert-deftest test-scene-break-replacement-odt ()
  "Test scene break replacement for ODT backend."
  (let ((text "Some text before.\nSCENE-BREAK\nSome text after.")
        (backend 'odt)
        (expected-replacement (or (alist-get 'odt org-scribe-scene-break-replacements)
                                 (alist-get t org-scribe-scene-break-replacements))))
    (let ((result (org-scribe--export-replace-scene-breaks text backend nil)))
      (should (string-match-p (regexp-quote expected-replacement) result))
      (should-not (string-match-p "SCENE-BREAK" result)))))

(ert-deftest test-scene-break-replacement-default ()
  "Test scene break replacement for unknown backend uses default."
  (let ((text "Some text before.\nSCENE-BREAK\nSome text after.")
        (backend 'unknown-backend)
        (expected-replacement (alist-get t org-scribe-scene-break-replacements)))
    (let ((result (org-scribe--export-replace-scene-breaks text backend nil)))
      (should (string-match-p (regexp-quote expected-replacement) result))
      (should-not (string-match-p "SCENE-BREAK" result)))))

(ert-deftest test-scene-break-replacement-multiple ()
  "Test that multiple scene breaks are all replaced."
  (let ((text "Text 1.\nSCENE-BREAK\nText 2.\nSCENE-BREAK\nText 3.")
        (backend 'ascii))
    (let ((result (org-scribe--export-replace-scene-breaks text backend nil)))
      ;; Should not contain any SCENE-BREAK markers
      (should-not (string-match-p "SCENE-BREAK" result)))))

(ert-deftest test-scene-break-no-replacement-needed ()
  "Test that text without scene breaks is unchanged."
  (let ((text "Some text without any scene breaks.")
        (backend 'ascii))
    (let ((result (org-scribe--export-replace-scene-breaks text backend nil)))
      (should (string= text result)))))

(ert-deftest test-scene-break-empty-text ()
  "Test scene break replacement with empty text."
  (let ((text "")
        (backend 'ascii))
    (let ((result (org-scribe--export-replace-scene-breaks text backend nil)))
      (should (string= "" result)))))

;;; Filter Integration Tests

(ert-deftest test-scene-break-filter-registered ()
  "Test that the scene break filter wrapper is registered in org-export."
  (should (boundp 'org-export-filter-final-output-functions))
  (should (memq 'org-scribe--export-filter-scene-breaks
                org-export-filter-final-output-functions)))

;;; Scoping to org-scribe documents (L2)

(ert-deftest test-scene-break-in-scribe-context-nil-info ()
  "in-scribe-context-p is nil when INFO carries no file or buffer info."
  (should-not (org-scribe--export-in-scribe-context-p nil)))

(ert-deftest test-scene-break-in-scribe-context-project-file ()
  "in-scribe-context-p is t when :input-file lives under an org-scribe project."
  (let* ((project-dir (make-temp-file "test-export-project-" t))
         (file (expand-file-name "novel.org" project-dir)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".org-scribe-project" project-dir)
            (insert "# Writing project: Test\n# Type: novel\n"))
          (with-temp-file file (insert "* Chapter\n"))
          (should (org-scribe--export-in-scribe-context-p (list :input-file file))))
      (delete-directory project-dir t))))

(ert-deftest test-scene-break-in-scribe-context-non-project-file ()
  "in-scribe-context-p is nil for a file outside any org-scribe project."
  (let ((file (make-temp-file "test-export-unrelated-" nil ".org")))
    (unwind-protect
        (should-not (org-scribe--export-in-scribe-context-p (list :input-file file)))
      (delete-file file))))

(ert-deftest test-scene-break-in-scribe-context-buffer-with-org-scribe-mode ()
  "in-scribe-context-p is t when the source buffer has org-scribe-mode on."
  (with-temp-buffer
    (rename-buffer "*test-export-scribe-buf*" t)
    (setq-local org-scribe-mode t)
    (should (org-scribe--export-in-scribe-context-p
             (list :input-buffer (buffer-name))))))

(ert-deftest test-scene-break-in-scribe-context-buffer-without-org-scribe-mode ()
  "in-scribe-context-p is nil when the source buffer has org-scribe-mode off."
  (with-temp-buffer
    (rename-buffer "*test-export-non-scribe-buf*" t)
    (setq-local org-scribe-mode nil)
    (should-not (org-scribe--export-in-scribe-context-p
                 (list :input-buffer (buffer-name))))))

(ert-deftest test-scene-break-filter-skips-non-scribe-document ()
  "The registered filter leaves SCENE-BREAK untouched outside org-scribe."
  (let ((text "Before\nSCENE-BREAK\nAfter"))
    (should (string= text
                      (org-scribe--export-filter-scene-breaks text 'ascii nil)))))

(ert-deftest test-scene-break-filter-replaces-in-scribe-document ()
  "The registered filter replaces SCENE-BREAK inside an org-scribe project."
  (let* ((project-dir (make-temp-file "test-export-project-" t))
         (file (expand-file-name "novel.org" project-dir))
         (text "Before\nSCENE-BREAK\nAfter"))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".org-scribe-project" project-dir)
            (insert "# Writing project: Test\n# Type: novel\n"))
          (let ((result (org-scribe--export-filter-scene-breaks
                         text 'ascii (list :input-file file))))
            (should-not (string-match-p "SCENE-BREAK" result))))
      (delete-directory project-dir t))))

;;; :ignore: Tag Activation (ox-extra, PROP-8)

(ert-deftest test-ignore-headlines-filter-registered-when-ox-extra-available ()
  "The ignore-headlines filter is registered exactly when ox-extra loaded."
  (should (eq (and (memq 'org-scribe--export-filter-ignore-headlines
                        org-export-filter-parse-tree-functions)
                   t)
              (and org-scribe--ox-extra-available t))))

(ert-deftest test-ignore-headlines-drops-title-keeps-body-in-scribe-document ()
  "Exporting an org-scribe document drops an :ignore: title but keeps its body.
This is the design lesson 11 documents (title excluded, content
included) -- confirmed against ox-extra's actual behavior rather than
plain Org, which has no built-in handling for a tag named \"ignore\"."
  (skip-unless org-scribe--ox-extra-available)
  (let* ((project-dir (make-temp-file "test-export-project-" t))
         (file (expand-file-name "novel.org" project-dir)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".org-scribe-project" project-dir)
            (insert "# Writing project: Test\n# Type: novel\n"))
          (with-temp-file file
            (insert "#+TODO: TODO | DONE\n* TODO Scene 1 :ignore:\nThe door creaked open.\n"))
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buf
                  (let ((output (org-export-as 'ascii nil nil t)))
                    (should-not (string-match-p "TODO" output))
                    (should-not (string-match-p "Scene 1" output))
                    (should (string-match-p "door creaked open" output))))
              (kill-buffer buf))))
      (delete-directory project-dir t))))

(ert-deftest test-ignore-headlines-keeps-title-outside-scribe-document ()
  "An :ignore:-tagged heading in a non-org-scribe file is left untouched.
Loading org-scribe-export must not change the export behavior of
unrelated Org files just because ox-extra happens to be on the
load-path."
  (skip-unless org-scribe--ox-extra-available)
  (let ((file (make-temp-file "test-export-unrelated-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TODO: TODO | DONE\n* TODO Scene 1 :ignore:\nThe door creaked open.\n"))
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buf
                  (let ((output (org-export-as 'ascii nil nil t)))
                    (should (string-match-p "Scene 1" output))))
              (kill-buffer buf))))
      (delete-file file))))

;;; Configuration Customization Tests

(ert-deftest test-scene-break-customization ()
  "Test that scene break replacements can be customized."
  (let ((original-replacements org-scribe-scene-break-replacements)
        (custom-replacement "*** CUSTOM BREAK ***"))
    (unwind-protect
        (progn
          ;; Temporarily customize
          (setq org-scribe-scene-break-replacements
                (cons (cons 'test-backend custom-replacement)
                      org-scribe-scene-break-replacements))

          ;; Test custom replacement
          (let ((text "Before\nSCENE-BREAK\nAfter"))
            (let ((result (org-scribe--export-replace-scene-breaks text 'test-backend nil)))
              (should (string-match-p (regexp-quote custom-replacement) result))
              (should-not (string-match-p "SCENE-BREAK" result)))))

      ;; Restore original
      (setq org-scribe-scene-break-replacements original-replacements))))

;;; Run tests

(defun org-scribe-export-run-tests ()
  "Run all export filter tests."
  (interactive)
  (ert "^test-export-\\|^test-scene-break-"))

(provide 'test-export)

;;; test-export.el ends here
