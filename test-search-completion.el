#!/usr/bin/env emacs --script
;;; test-search-completion.el --- Test search function completion menus

;; Load path setup
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (expand-file-name "core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "search" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "linking" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "capture" (file-name-directory load-file-name)))

;; Load required modules
(require 'org-scribe-core)
(require 'org-scribe-capture)
(require 'org-scribe-search)

(defvar test-results nil
  "List of test results (name . pass/fail).")

(defun test-run (name test-fn)
  "Run a test and record result."
  (condition-case err
      (progn
        (funcall test-fn)
        (push (cons name 'pass) test-results)
        (message "✓ %s" name))
    (error
     (push (cons name 'fail) test-results)
     (message "✗ %s: %s" name (error-message-string err)))))

;; Test 1: Verify search functions are defined
(test-run "org-scribe/org-find-pov is defined"
          (lambda ()
            (unless (fboundp 'org-scribe/org-find-pov)
              (error "Function not defined"))))

(test-run "org-scribe/org-find-character is defined"
          (lambda ()
            (unless (fboundp 'org-scribe/org-find-character)
              (error "Function not defined"))))

(test-run "org-scribe/org-find-plot is defined"
          (lambda ()
            (unless (fboundp 'org-scribe/org-find-plot)
              (error "Function not defined"))))

(test-run "org-scribe/org-find-location is defined"
          (lambda ()
            (unless (fboundp 'org-scribe/org-find-location)
              (error "Function not defined"))))

;; Test 2: Verify linking module functions can be loaded
(test-run "org-scribe--get-all-characters is available"
          (lambda ()
            (require 'org-scribe-character-links)
            (unless (fboundp 'org-scribe--get-all-characters)
              (error "Function not defined"))))

(test-run "org-scribe--get-all-locations is available"
          (lambda ()
            (require 'org-scribe-location-links)
            (unless (fboundp 'org-scribe--get-all-locations)
              (error "Function not defined"))))

(test-run "org-scribe--get-all-plot-threads is available"
          (lambda ()
            (require 'org-scribe-plot-links)
            (unless (fboundp 'org-scribe--get-all-plot-threads)
              (error "Function not defined"))))

;; Test 3: Test database access with demo project
(let ((demo-dir (expand-file-name "examples/demo-novel" (file-name-directory load-file-name))))
  (when (file-directory-p demo-dir)
    (cd demo-dir)

    (test-run "Can read characters from demo project"
              (lambda ()
                (require 'org-scribe-character-links)
                (let ((chars (org-scribe--get-all-characters)))
                  (unless chars
                    (error "No characters found"))
                  (unless (> (length chars) 0)
                    (error "Character list empty"))
                  (message "  Found %d characters" (length chars)))))

    (test-run "Can read locations from demo project"
              (lambda ()
                (require 'org-scribe-location-links)
                (let ((locs (org-scribe--get-all-locations)))
                  (unless locs
                    (error "No locations found"))
                  (unless (> (length locs) 0)
                    (error "Location list empty"))
                  (message "  Found %d locations" (length locs)))))

    (test-run "Can read plot threads from demo project"
              (lambda ()
                (require 'org-scribe-plot-links)
                (let ((threads (org-scribe--get-all-plot-threads)))
                  (unless threads
                    (error "No plot threads found"))
                  (unless (> (length threads) 0)
                    (error "Plot thread list empty"))
                  (message "  Found %d plot threads" (length threads)))))))

;; Test 4: Verify substring matching helper
(test-run "org-scribe--property-contains-p works with substrings"
          (lambda ()
            (unless (org-scribe--property-contains-p "Alex Rivera" "Alex")
              (error "Substring match failed"))
            (unless (org-scribe--property-contains-p "Alex Rivera" "Rivera")
              (error "Substring match failed"))
            (when (org-scribe--property-contains-p "Alex Rivera" "Sam")
              (error "Should not match non-substring"))))

(test-run "org-scribe--property-contains-p works with ID links"
          (lambda ()
            (unless (org-scribe--property-contains-p "[[id:abc123][Alex Rivera]]" "Alex")
              (error "ID link substring match failed"))
            (unless (org-scribe--property-contains-p "[[id:abc123][Alex Rivera]]" "Rivera")
              (error "ID link substring match failed"))))

;; Summary
(message "\n=== Test Summary ===")
(let ((passed (cl-count-if (lambda (r) (eq (cdr r) 'pass)) test-results))
      (total (length test-results)))
  (message "Passed: %d/%d tests" passed total)
  (when (< passed total)
    (message "\nFailed tests:")
    (dolist (result test-results)
      (when (eq (cdr result) 'fail)
        (message "  - %s" (car result)))))
  (kill-emacs (if (= passed total) 0 1)))
