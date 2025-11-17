#!/usr/bin/env emacs --script
;;; test-link-update.el --- Test link name update functions

;; Load path setup
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (expand-file-name "core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "linking" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "capture" (file-name-directory load-file-name)))

;; Load required modules
(require 'org-scribe-core)
(require 'org-scribe-capture)
(require 'org-scribe-link-update)
(require 'org-scribe-character-links)

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

;; Test 1: Module loading
(test-run "org-scribe-link-update module loads"
          (lambda ()
            (unless (featurep 'org-scribe-link-update)
              (error "Module not loaded"))))

;; Test 2: Core functions defined
(test-run "org-scribe--update-link-display-name is defined"
          (lambda ()
            (unless (fboundp 'org-scribe--update-link-display-name)
              (error "Function not defined"))))

(test-run "org-scribe--build-id-to-name-map is defined"
          (lambda ()
            (unless (fboundp 'org-scribe--build-id-to-name-map)
              (error "Function not defined"))))

(test-run "org-scribe--update-links-in-property is defined"
          (lambda ()
            (unless (fboundp 'org-scribe--update-links-in-property)
              (error "Function not defined"))))

;; Test 3: Update functions defined
(test-run "org-scribe/update-character-link-names is defined"
          (lambda ()
            (unless (fboundp 'org-scribe/update-character-link-names)
              (error "Function not defined"))))

(test-run "org-scribe/update-all-character-link-names is defined"
          (lambda ()
            (unless (fboundp 'org-scribe/update-all-character-link-names)
              (error "Function not defined"))))

(test-run "org-scribe/update-all-link-names is defined"
          (lambda ()
            (unless (fboundp 'org-scribe/update-all-link-names)
              (error "Function not defined"))))

;; Test 4: Link update logic
(test-run "Update link with new name"
          (lambda ()
            (let ((map (make-hash-table :test 'equal))
                  (link "[[id:test-123][Old Name]]"))
              (puthash "test-123" "New Name" map)
              (let ((result (org-scribe--update-link-display-name link map)))
                (unless (string= result "[[id:test-123][New Name]]")
                  (error "Expected '[[id:test-123][New Name]]' but got '%s'" result))))))

(test-run "Keep link unchanged when name matches"
          (lambda ()
            (let ((map (make-hash-table :test 'equal))
                  (link "[[id:test-123][Same Name]]"))
              (puthash "test-123" "Same Name" map)
              (let ((result (org-scribe--update-link-display-name link map)))
                (unless (string= result "[[id:test-123][Same Name]]")
                  (error "Link should not change"))))))

(test-run "Keep link unchanged when ID not in map"
          (lambda ()
            (let ((map (make-hash-table :test 'equal))
                  (link "[[id:unknown-999][Orphan]]"))
              (let ((result (org-scribe--update-link-display-name link map)))
                (unless (string= result "[[id:unknown-999][Orphan]]")
                  (error "Link should not change when ID not found"))))))

(test-run "Keep plain text unchanged"
          (lambda ()
            (let ((map (make-hash-table :test 'equal))
                  (text "Plain Text"))
              (let ((result (org-scribe--update-link-display-name text map)))
                (unless (string= result "Plain Text")
                  (error "Plain text should not change"))))))

;; Test 5: ID to name map building
(test-run "Build ID-to-name map from alist"
          (lambda ()
            (let ((alist '(("Alice" . ("id-alice" . "Alice"))
                          ("Bob" . ("id-bob" . "Bob")))))
              (let ((map (org-scribe--build-id-to-name-map alist)))
                (unless (string= (gethash "id-alice" map) "Alice")
                  (error "Expected Alice for id-alice"))
                (unless (string= (gethash "id-bob" map) "Bob")
                  (error "Expected Bob for id-bob"))))))

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
