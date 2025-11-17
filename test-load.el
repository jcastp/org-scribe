;;; test-load.el --- Quick test to verify org-scribe loads correctly -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Quick test script to verify that org-scribe can be loaded without errors.
;; Run with: emacs -Q -l test-load.el

;;; Code:

;; Add current directory to load path
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path default-directory)
  (add-to-list 'load-path (expand-file-name "core" default-directory))
  (add-to-list 'load-path (expand-file-name "templates" default-directory))
  (add-to-list 'load-path (expand-file-name "counting" default-directory))
  (add-to-list 'load-path (expand-file-name "modes" default-directory))
  (add-to-list 'load-path (expand-file-name "search" default-directory))
  (add-to-list 'load-path (expand-file-name "language" default-directory))
  (add-to-list 'load-path (expand-file-name "capture" default-directory))
  (add-to-list 'load-path (expand-file-name "linking" default-directory))
  (add-to-list 'load-path (expand-file-name "export" default-directory))
  (add-to-list 'load-path (expand-file-name "ui" default-directory)))

;; Try to load the package
(message "Testing org-scribe package load...")

(condition-case err
    (progn
      (require 'org-scribe-core)
      (message "✓ org-scribe-core loaded successfully")

      (require 'org-scribe-config)
      (message "✓ org-scribe-config loaded successfully")

      (require 'org-scribe-i18n)
      (message "✓ org-scribe-i18n loaded successfully")

      (require 'org-scribe-project)
      (message "✓ org-scribe-project loaded successfully")

      (require 'org-scribe-wordcount)
      (message "✓ org-scribe-wordcount loaded successfully")

      (require 'org-scribe-tracking)
      (message "✓ org-scribe-tracking loaded successfully")

      (require 'org-scribe-modes)
      (message "✓ org-scribe-modes loaded successfully")

      (require 'org-scribe-search)
      (message "✓ org-scribe-search loaded successfully")

      (require 'org-scribe-dictionary)
      (message "✓ org-scribe-dictionary loaded successfully")

      (require 'org-scribe-capture)
      (message "✓ org-scribe-capture loaded successfully")

      (require 'org-scribe-character-links)
      (message "✓ org-scribe-character-links loaded successfully")

      (require 'org-scribe-location-links)
      (message "✓ org-scribe-location-links loaded successfully")

      (require 'org-scribe-plot-links)
      (message "✓ org-scribe-plot-links loaded successfully")

      (require 'org-scribe-column-view)
      (message "✓ org-scribe-column-view loaded successfully")

      (require 'org-scribe-export)
      (message "✓ org-scribe-export loaded successfully")

      ;; Hydra is optional
      (when (require 'hydra nil t)
        (require 'org-scribe-hydra)
        (message "✓ org-scribe-hydra loaded successfully"))

      (require 'org-scribe)
      (message "✓ org-scribe loaded successfully")

      (message "\n=================================")
      (message "SUCCESS: All modules loaded!")
      (message "=================================")
      (message "\nVersion: %s" (org-scribe-version))
      (message "\nAvailable commands:")
      (message "- M-x org-scribe-mode")
      (message "- M-x org-scribe-create-novel-project (NEW in 0.2.0)")
      (message "- M-x org-scribe-insert-scene (NEW in 0.2.0)")
      (message "- M-x org-scribe-insert-chapter (NEW in 0.2.0)")
      (message "- M-x org-scribe/writing-env-mode")
      (message "- M-x org-scribe/ews-org-count-words")
      (when (featurep 'hydra)
        (message "- M-x hydra-org-scribe/body (includes project functions)"))
      (message "\nConfiguration variables:")
      (message "- M-x customize-group RET org-scribe RET")
      (message "\n"))

  (error
   (message "\n=================================")
   (message "ERROR: Failed to load package")
   (message "=================================")
   (message "Error: %s" (error-message-string err))
   (message "\nThis may be normal if you don't have all dependencies installed.")
   (message "Required: org-mode, org-ql, writeroom-mode")
   (message "Optional: hydra, consult, fontaine, treemacs, imenu-list")))

;;; test-load.el ends here
