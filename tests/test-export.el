;;; test-export.el --- Tests for export filters -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for export filters and scene break replacement.
;; Tests scene break replacement for various export backends.

;;; Code:

(require 'ert)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../export" default-directory)))

(require 'writing-config)
(require 'writing-export)

;;; Module Loading Tests

(ert-deftest test-export-module-loads ()
  "Test that writing-export module loads without errors."
  (should (featurep 'writing-export)))

;;; Function Availability Tests

(ert-deftest test-export-functions-defined ()
  "Test that export functions are defined."
  (should (fboundp 'org-export-replace-scene-breaks)))

;;; Scene Break Configuration Tests

(ert-deftest test-scene-break-replacements-configured ()
  "Test that scene break replacements are configured."
  (should (boundp 'writing/scene-break-replacements))
  (should (listp writing/scene-break-replacements))
  (should (> (length writing/scene-break-replacements) 0)))

(ert-deftest test-scene-break-has-backends ()
  "Test that scene break replacements include common backends."
  ;; Should have at least ascii, html, latex, and default (t)
  (should (assoc 'ascii writing/scene-break-replacements))
  (should (assoc 'html writing/scene-break-replacements))
  (should (assoc 'latex writing/scene-break-replacements))
  (should (assoc t writing/scene-break-replacements)))

;;; Scene Break Replacement Tests

(ert-deftest test-scene-break-replacement-ascii ()
  "Test scene break replacement for ASCII backend."
  (let ((text "Some text before.\nSCENE-BREAK\nSome text after.")
        (backend 'ascii)
        (expected-replacement (alist-get 'ascii writing/scene-break-replacements)))
    (let ((result (org-export-replace-scene-breaks text backend nil)))
      (should (string-match-p expected-replacement result))
      (should-not (string-match-p "SCENE-BREAK" result)))))

(ert-deftest test-scene-break-replacement-html ()
  "Test scene break replacement for HTML backend."
  (let ((text "Some text before.\nSCENE-BREAK\nSome text after.")
        (backend 'html)
        (expected-replacement (alist-get 'html writing/scene-break-replacements)))
    (let ((result (org-export-replace-scene-breaks text backend nil)))
      (should (string-match-p expected-replacement result))
      (should-not (string-match-p "SCENE-BREAK" result)))))

(ert-deftest test-scene-break-replacement-latex ()
  "Test scene break replacement for LaTeX backend."
  (let ((text "Some text before.\nSCENE-BREAK\nSome text after.")
        (backend 'latex)
        (expected-replacement (alist-get 'latex writing/scene-break-replacements)))
    (let ((result (org-export-replace-scene-breaks text backend nil)))
      (should (string-match-p (regexp-quote expected-replacement) result))
      (should-not (string-match-p "SCENE-BREAK" result)))))

(ert-deftest test-scene-break-replacement-odt ()
  "Test scene break replacement for ODT backend."
  (let ((text "Some text before.\nSCENE-BREAK\nSome text after.")
        (backend 'odt)
        (expected-replacement (or (alist-get 'odt writing/scene-break-replacements)
                                 (alist-get t writing/scene-break-replacements))))
    (let ((result (org-export-replace-scene-breaks text backend nil)))
      (should (string-match-p (regexp-quote expected-replacement) result))
      (should-not (string-match-p "SCENE-BREAK" result)))))

(ert-deftest test-scene-break-replacement-default ()
  "Test scene break replacement for unknown backend uses default."
  (let ((text "Some text before.\nSCENE-BREAK\nSome text after.")
        (backend 'unknown-backend)
        (expected-replacement (alist-get t writing/scene-break-replacements)))
    (let ((result (org-export-replace-scene-breaks text backend nil)))
      (should (string-match-p (regexp-quote expected-replacement) result))
      (should-not (string-match-p "SCENE-BREAK" result)))))

(ert-deftest test-scene-break-replacement-multiple ()
  "Test that multiple scene breaks are all replaced."
  (let ((text "Text 1.\nSCENE-BREAK\nText 2.\nSCENE-BREAK\nText 3.")
        (backend 'ascii))
    (let ((result (org-export-replace-scene-breaks text backend nil)))
      ;; Should not contain any SCENE-BREAK markers
      (should-not (string-match-p "SCENE-BREAK" result)))))

(ert-deftest test-scene-break-no-replacement-needed ()
  "Test that text without scene breaks is unchanged."
  (let ((text "Some text without any scene breaks.")
        (backend 'ascii))
    (let ((result (org-export-replace-scene-breaks text backend nil)))
      (should (string= text result)))))

(ert-deftest test-scene-break-empty-text ()
  "Test scene break replacement with empty text."
  (let ((text "")
        (backend 'ascii))
    (let ((result (org-export-replace-scene-breaks text backend nil)))
      (should (string= "" result)))))

;;; Filter Integration Tests

(ert-deftest test-scene-break-filter-registered ()
  "Test that scene break filter is registered in org-export."
  (should (boundp 'org-export-filter-final-output-functions))
  (should (memq 'org-export-replace-scene-breaks
                org-export-filter-final-output-functions)))

;;; Configuration Customization Tests

(ert-deftest test-scene-break-customization ()
  "Test that scene break replacements can be customized."
  (let ((original-replacements writing/scene-break-replacements)
        (custom-replacement "*** CUSTOM BREAK ***"))
    (unwind-protect
        (progn
          ;; Temporarily customize
          (setq writing/scene-break-replacements
                (cons (cons 'test-backend custom-replacement)
                      writing/scene-break-replacements))

          ;; Test custom replacement
          (let ((text "Before\nSCENE-BREAK\nAfter"))
            (let ((result (org-export-replace-scene-breaks text 'test-backend nil)))
              (should (string-match-p (regexp-quote custom-replacement) result))
              (should-not (string-match-p "SCENE-BREAK" result)))))

      ;; Restore original
      (setq writing/scene-break-replacements original-replacements))))

;;; Run tests

(defun writing-export-run-tests ()
  "Run all export filter tests."
  (interactive)
  (ert "^test-export-\\|^test-scene-break-"))

(provide 'test-export)

;;; test-export.el ends here
