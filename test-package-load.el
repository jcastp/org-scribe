;;; test-package-load.el --- Test org-scribe package loading -*- lexical-binding: t; -*-

;; This file tests that org-scribe can be loaded correctly with package managers
;; that only add the root directory to load-path.

;;; Commentary:

;; Usage:
;;   emacs -Q --batch -l test-package-load.el
;;
;; This simulates how package managers load the package and verifies that:
;; 1. Subdirectories are correctly added to load-path
;; 2. Cross-module requires work (e.g., org-scribe-character-links requiring org-scribe-core)
;; 3. All main modules load without errors

;;; Code:

(message "Testing org-scribe package loading...")
(message "========================================")

;; Simulate package manager behavior: only add root directory
(let ((pkg-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path pkg-dir)
  (message "\nPackage root added to load-path: %s" pkg-dir)

  ;; This is what org-scribe.el does to support subdirectories
  (message "Adding subdirectories to load-path (as org-scribe.el does)...")
  (dolist (subdir '("core" "templates" "linking" "counting" "search"
                    "capture" "modes" "language" "export" "ui"))
    (add-to-list 'load-path (expand-file-name subdir pkg-dir)))
  (message "✓ Subdirectories added"))

;; Test 1: Load core modules
(message "\nTest 1: Loading core modules...")
(require 'org-scribe-messages)
(require 'org-scribe-core)
(require 'org-scribe-config)
(message "✓ Core modules loaded")

;; Test 2: Load linking modules (these require core modules)
(message "\nTest 2: Loading linking modules (cross-module requires)...")
(require 'org-scribe-character-links)
(require 'org-scribe-location-links)
(require 'org-scribe-plot-links)
(message "✓ Linking modules loaded (cross-module requires work!)")

;; Test 3: Load other feature modules
(message "\nTest 3: Loading feature modules...")
(require 'org-scribe-project)
(require 'org-scribe-search)
(require 'org-scribe-wordcount)
(require 'org-scribe-capture)
(message "✓ Feature modules loaded")

;; Test 4: Load main package file
(message "\nTest 4: Loading main package...")
;; Note: This will fail if optional dependencies (hydra, writeroom-mode) aren't installed
;; But the load-path mechanism will have already been tested
(condition-case err
    (progn
      (require 'org-scribe)
      (message "✓ Main package loaded successfully"))
  (file-missing
   (message "⚠ Main package load failed (expected if optional dependencies missing)")
   (message "  Error: %s" (error-message-string err))
   (message "  This is OK - the load-path mechanism works!")))

(message "\n========================================")
(message "Package loading test complete!")
(message "All critical cross-module requires work correctly.")

;;; test-package-load.el ends here
