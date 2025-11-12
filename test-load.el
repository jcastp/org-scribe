;;; test-load.el --- Quick test to verify emacs-writing loads correctly -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Quick test script to verify that emacs-writing can be loaded without errors.
;; Run with: emacs -Q -l test-load.el

;;; Code:

;; Add current directory to load path
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path default-directory)
  (add-to-list 'load-path (expand-file-name "core" default-directory))
  (add-to-list 'load-path (expand-file-name "counting" default-directory))
  (add-to-list 'load-path (expand-file-name "modes" default-directory))
  (add-to-list 'load-path (expand-file-name "search" default-directory))
  (add-to-list 'load-path (expand-file-name "language" default-directory))
  (add-to-list 'load-path (expand-file-name "capture" default-directory))
  (add-to-list 'load-path (expand-file-name "export" default-directory))
  (add-to-list 'load-path (expand-file-name "ui" default-directory)))

;; Try to load the package
(message "Testing emacs-writing package load...")

(condition-case err
    (progn
      (require 'writing-core)
      (message "✓ writing-core loaded successfully")

      (require 'writing-config)
      (message "✓ writing-config loaded successfully")

      (require 'writing-wordcount)
      (message "✓ writing-wordcount loaded successfully")

      (require 'writing-tracking)
      (message "✓ writing-tracking loaded successfully")

      (require 'writing-modes)
      (message "✓ writing-modes loaded successfully")

      (require 'writing-search)
      (message "✓ writing-search loaded successfully")

      (require 'writing-dictionary)
      (message "✓ writing-dictionary loaded successfully")

      (require 'writing-capture)
      (message "✓ writing-capture loaded successfully")

      (require 'writing-export)
      (message "✓ writing-export loaded successfully")

      ;; Hydra is optional
      (when (require 'hydra nil t)
        (require 'writing-hydra)
        (message "✓ writing-hydra loaded successfully"))

      (require 'emacs-writing)
      (message "✓ emacs-writing loaded successfully")

      (message "\n=================================")
      (message "SUCCESS: All modules loaded!")
      (message "=================================")
      (message "\nVersion: %s" (writing-version))
      (message "\nAvailable commands:")
      (message "- M-x emacs-writing-mode")
      (message "- M-x my-writing-env-mode")
      (message "- M-x writing/ews-org-count-words")
      (when (featurep 'hydra)
        (message "- M-x hydra-writing/body"))
      (message "\nConfiguration variables:")
      (message "- M-x customize-group RET writing RET")
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
