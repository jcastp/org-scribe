;;; test-word-target.el --- Tests for the project word target -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for the project's total word target: the `# Target-words:' marker
;; accessor, the exact split among the outermost parts, and the thousands
;; separator (core/org-scribe-core.el).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path (expand-file-name "core" parent-dir)))

(require 'org-scribe-core)
(require 'org-scribe-config)

;;; Fixture

(defmacro test-word-target--with-project (root-var marker &rest body)
  "Bind ROOT-VAR to a temp project dir whose marker file holds MARKER; run BODY."
  (declare (indent 2))
  `(let ((,root-var (make-temp-file "test-word-target-" t)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".org-scribe-project" ,root-var)
             (insert ,marker))
           ,@body)
       (delete-directory ,root-var t))))

(defun test-word-target--read (marker-line)
  "Return `org-scribe-project-word-target' for a marker holding MARKER-LINE."
  (test-word-target--with-project root (concat "# Type: novel\n" marker-line "\n")
    (org-scribe-project-word-target root)))

;;; Split

(ert-deftest test-word-target-split-examples ()
  (should (equal (org-scribe--split-word-target 80000 '(25 50 25)) '(20000 40000 20000)))
  (should (equal (org-scribe--split-word-target 50000 '(25 50 25)) '(12500 25000 12500)))
  (should (equal (org-scribe--split-word-target 5000 '(25 50 25)) '(1250 2500 1250)))
  (should (equal (org-scribe--split-word-target 10 '(1 1 1)) '(4 3 3)))
  (should (equal (org-scribe--split-word-target 7 '(25 50 25)) '(2 3 2))))

(ert-deftest test-word-target-split-sums-exactly ()
  (dolist (total '(1 2 3 7 99 1001 49999 80001))
    (dolist (weights '((25 50 25) (1 1 1) (1 2) (3) (1 0 1) (0.5 1.5 1)))
      (let ((parts (org-scribe--split-word-target total weights)))
        (should (= (length parts) (length weights)))
        (should (= (apply #'+ parts) total))))))

(ert-deftest test-word-target-split-ties-go-to-the-earlier-part ()
  (should (equal (org-scribe--split-word-target 2 '(1 1 1)) '(1 1 0)))
  (should (equal (org-scribe--split-word-target 1 '(1 1)) '(1 0))))

(ert-deftest test-word-target-split-scale-invariant ()
  (dolist (total '(7 5000 80001))
    (should (equal (org-scribe--split-word-target total '(1 2 1))
                   (org-scribe--split-word-target total '(25 50 25))))))

(ert-deftest test-word-target-split-errors ()
  (should-error (org-scribe--split-word-target 0 '(1 1)) :type 'user-error)
  (should-error (org-scribe--split-word-target -5 '(1 1)) :type 'user-error)
  (should-error (org-scribe--split-word-target 10.5 '(1 1)) :type 'user-error)
  (should-error (org-scribe--split-word-target nil '(1 1)) :type 'user-error)
  (should-error (org-scribe--split-word-target 10 nil) :type 'user-error)
  (should-error (org-scribe--split-word-target 10 '(1 -1 1)) :type 'user-error)
  (should-error (org-scribe--split-word-target 10 '(0 0)) :type 'user-error))

;;; Marker accessor

(ert-deftest test-word-target-marker-missing-file ()
  (let ((root (make-temp-file "test-word-target-" t)))
    (unwind-protect (should-not (org-scribe-project-word-target root))
      (delete-directory root t))))

(ert-deftest test-word-target-marker-missing-line ()
  (test-word-target--with-project root "# Type: novel\n"
    (should-not (org-scribe-project-word-target root))))

(ert-deftest test-word-target-marker-values ()
  (should (= (test-word-target--read "# Target-words: 50000") 50000))
  (should (= (test-word-target--read "# Target-words:  50000  ") 50000))
  (should (= (test-word-target--read "# target-words: 1200") 1200))
  (should-not (test-word-target--read "# Target-words: abc"))
  (should-not (test-word-target--read "# Target-words: 50k"))
  (should-not (test-word-target--read "# Target-words: 0"))
  (should-not (test-word-target--read "# Target-words: -5"))
  (should-not (test-word-target--read "# Target-words: 12.5"))
  (should-not (test-word-target--read "# Target-words:")))

(ert-deftest test-word-target-marker-no-project-ignores-default-directory ()
  "With no project root, a marker in `default-directory' is never read."
  (test-word-target--with-project dir "# Type: novel\n# Target-words: 777\n"
    (let ((default-directory (file-name-as-directory dir)))
      (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () nil)))
        (should-not (org-scribe-project-word-target))))))

(ert-deftest test-word-target-setter-round-trip-keeps-other-lines ()
  (test-word-target--with-project root "# Type: novel\n# Language: en\n"
    (org-scribe--set-project-word-target root 60000)
    (should (= (org-scribe-project-word-target root) 60000))
    (org-scribe--set-project-word-target root 55000)
    (should (= (org-scribe-project-word-target root) 55000))
    (should (equal (org-scribe--project-marker-get root "Type") "novel"))
    (should (equal (org-scribe--project-marker-get root "Language") "en"))
    (with-temp-buffer
      (insert-file-contents (expand-file-name ".org-scribe-project" root))
      (should (= (how-many "Target-words" (point-min) (point-max)) 1)))))

(ert-deftest test-word-target-setter-validates ()
  (test-word-target--with-project root "# Type: novel\n"
    (dolist (bad '(0 -3 5.5 "5000" nil))
      (should-error (org-scribe--set-project-word-target root bad) :type 'user-error))
    (should-not (org-scribe-project-word-target root))))

;;; Defaults

(ert-deftest test-word-target-defaults-by-type ()
  (let ((org-scribe-default-word-target '((novel . 80000) (short-story . 5000))))
    (should (= (org-scribe-default-word-target-for 'novel) 80000))
    (should (= (org-scribe-default-word-target-for 'short-story) 5000))
    (should (= (org-scribe-default-word-target-for 'unknown) 80000))))

;;; Formatting

(ert-deftest test-word-target-format-by-language ()
  (should (equal (org-scribe--format-word-count 80000 'en) "80,000"))
  (should (equal (org-scribe--format-word-count 80000 'es) "80.000"))
  (should (equal (org-scribe--format-word-count 999 'en) "999"))
  (should (equal (org-scribe--format-word-count 1000 'en) "1,000"))
  (should (equal (org-scribe--format-word-count 1000000 'en) "1,000,000"))
  (should (equal (org-scribe--format-word-count 0 'es) "0")))

(provide 'test-word-target)
;;; test-word-target.el ends here
