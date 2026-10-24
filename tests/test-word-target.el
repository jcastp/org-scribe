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
  (add-to-list 'load-path (expand-file-name "core" parent-dir))
  (add-to-list 'load-path (expand-file-name "templates" parent-dir))
  (add-to-list 'load-path (expand-file-name "reporting" parent-dir)))

(require 'org-scribe-core)
(require 'org-scribe-config)
(require 'org-scribe-project)
(require 'org-scribe-health)

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

;;; org-scribe-set-word-target

(defmacro test-word-target--with-project (dir-var kind language target &rest body)
  "Create a KIND project in LANGUAGE with TARGET words; run BODY inside it.
DIR-VAR is bound to the project directory, which is also `default-directory'."
  (declare (indent 4))
  `(let* ((base (make-temp-file "test-set-word-target-" t))
          (,dir-var (file-name-as-directory (expand-file-name "P" base))))
     (unwind-protect
         (progn
           (cl-letf (((symbol-function 'message) #'ignore))
             (if (eq ,kind 'novel)
                 (org-scribe-create-novel-project base "P" ,language nil ,target)
               (org-scribe-create-short-story-project base "P" ,language ,target)))
           (let ((default-directory ,dir-var))
             (cl-letf (((symbol-function 'message) #'ignore))
               ,@body)))
       (dolist (buf (buffer-list))
         (when (and (buffer-file-name buf) (string-prefix-p base (buffer-file-name buf)))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory base t))))

(defun test-word-target--text (dir name)
  "Return the contents of NAME under DIR."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name dir))
    (buffer-string)))

(defun test-word-target--objectives (text)
  "Return the WORD-OBJECTIVE values in TEXT, in order."
  (let (values (start 0))
    (while (string-match ":WORD-OBJECTIVE: +\\([0-9]+\\)" text start)
      (push (string-to-number (match-string 1 text)) values)
      (setq start (match-end 0)))
    (nreverse values)))

(defun test-word-target--count-ids (dir)
  "Count :ID: lines across every .org file under DIR."
  (cl-loop for file in (directory-files-recursively dir "\\.org\\'")
           sum (with-temp-buffer
                 (insert-file-contents file)
                 (how-many "^:ID:" (point-min) (point-max)))))

(ert-deftest test-word-target-set-rescales-everything-with-reset ()
  (test-word-target--with-project dir 'novel 'en 80000
    (let ((ids (test-word-target--count-ids dir)))
      (org-scribe-set-word-target 50000 'reset t)
      (should (= (org-scribe-project-word-target dir) 50000))
      (let ((novel (test-word-target--text dir "novel.org")))
        (should (equal (test-word-target--objectives novel) '(12500 3000 500 25000 12500)))
        (should (string-match-p "^- Total word target :: 50,000$" novel)))
      (should (string-match-p "^:TARGET-WORDCOUNT: 50000$"
                              (test-word-target--text dir "README.org")))
      (should (= ids (test-word-target--count-ids dir))))))

(ert-deftest test-word-target-set-keeps-hand-edited-proportions ()
  (test-word-target--with-project dir 'novel 'en 80000
    (let ((novel (expand-file-name "novel.org" dir)))
      (with-temp-buffer
        (insert-file-contents novel)
        (goto-char (point-min))
        (re-search-forward ":WORD-OBJECTIVE: 40000")
        (replace-match ":WORD-OBJECTIVE: 35000")
        (goto-char (point-min))
        (while (re-search-forward ":CUSTOM_ID: act-3" nil t)
          (re-search-forward ":WORD-OBJECTIVE: 20000")
          (replace-match ":WORD-OBJECTIVE: 25000"))
        (write-region (point-min) (point-max) novel))
      (org-scribe-set-word-target 40000 'keep t)
      (should (equal (test-word-target--objectives (test-word-target--text dir "novel.org"))
                     '(10000 3000 500 17500 12500))))))

(ert-deftest test-word-target-set-reset-refused-for-other-part-counts ()
  (test-word-target--with-project dir 'novel 'en 80000
    (let ((novel (expand-file-name "novel.org" dir)))
      (with-temp-buffer
        (insert-file-contents novel)
        (goto-char (point-max))
        (insert "\n* Act IV :ignore:\n:PROPERTIES:\n:WORD-OBJECTIVE: 10000\n:END:\n")
        (write-region (point-min) (point-max) novel))
      (should-error (org-scribe-set-word-target 50000 'reset t) :type 'user-error)
      (should-not (equal (org-scribe-project-word-target dir) 50000))
      (org-scribe-set-word-target 90000 'keep t)
      (should (= (length (test-word-target--objectives
                          (test-word-target--text dir "novel.org")))
                 6)))))

(ert-deftest test-word-target-set-skips-heading-without-objective ()
  (test-word-target--with-project dir 'novel 'en 80000
    (let ((novel (expand-file-name "novel.org" dir)))
      (with-temp-buffer
        (insert-file-contents novel)
        (goto-char (point-max))
        (insert "\n* Epilogue :ignore:\nSome words.\n")
        (write-region (point-min) (point-max) novel))
      (org-scribe-set-word-target 50000 'reset t)
      (with-temp-buffer
        (insert-file-contents novel)
        (goto-char (point-min))
        (re-search-forward "^\\* Epilogue")
        (org-mode)
        (should-not (org-entry-get nil "WORD-OBJECTIVE"))))))

(ert-deftest test-word-target-set-old-project-offers-the-current-sum ()
  "With no marker, the default offered is the current objectives' sum."
  (test-word-target--with-project dir 'novel 'en 80000
    (let ((marker (expand-file-name ".org-scribe-project" dir))
          (offered nil))
      (with-temp-buffer
        (insert-file-contents marker)
        (goto-char (point-min))
        (flush-lines "^# Target-words:")
        (write-region (point-min) (point-max) marker))
      (should-not (org-scribe-project-word-target dir))
      (cl-letf (((symbol-function 'read-number)
                 (lambda (_prompt &optional default) (setq offered default) default))
                ((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _) (car choices)))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (call-interactively #'org-scribe-set-word-target))
      (should (= offered 80000))
      (should (equal (test-word-target--objectives (test-word-target--text dir "novel.org"))
                     '(20000 3000 500 40000 20000))))))

(ert-deftest test-word-target-set-short-story ()
  (test-word-target--with-project dir 'short-story 'en 5000
    (org-scribe-set-word-target 8000 'reset t)
    (should (equal (test-word-target--objectives (test-word-target--text dir "story.org"))
                   '(2000 4000 2000)))
    (should (string-match-p "^:TARGET_WORDCOUNT: 8000$" (test-word-target--text dir "story.org")))
    (should (string-match-p "^:TARGET_WORDCOUNT: 8000$" (test-word-target--text dir "README.org")))))

(ert-deftest test-word-target-set-spanish-goals-line ()
  (test-word-target--with-project dir 'novel 'es 80000
    (org-scribe-set-word-target 50000 'reset t)
    (should (string-match-p
             "^- Objetivo total de palabras :: 50\\.000$"
             (test-word-target--text dir (org-scribe-lang-file 'manuscript-novel 'es))))))

(ert-deftest test-word-target-set-outside-a-project-refuses-before-prompting ()
  (let* ((dir (file-name-as-directory (make-temp-file "test-no-project-" t)))
         (default-directory dir)
         (prompted nil))
    (unwind-protect
        (cl-letf (((symbol-function 'read-number) (lambda (&rest _) (setq prompted t) 1))
                  ((symbol-function 'completing-read) (lambda (&rest _) (setq prompted t) ""))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) (setq prompted t) t)))
          (should-error (call-interactively #'org-scribe-set-word-target) :type 'user-error)
          (should-not prompted))
      (delete-directory dir t))))

(ert-deftest test-word-target-set-leaves-an-unsaved-manuscript-modified ()
  (test-word-target--with-project dir 'novel 'en 80000
    (let ((buffer (find-file-noselect (expand-file-name "novel.org" dir))))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "\nUnsaved words.\n"))
      (org-scribe-set-word-target 50000 'reset t)
      (with-current-buffer buffer
        (should (buffer-modified-p))
        (should (string-match-p "Unsaved words" (buffer-string)))
        (should (equal (test-word-target--objectives (buffer-string))
                       '(12500 3000 500 25000 12500))))
      ;; The file on disk is the unsaved-edits-free one
      (should-not (string-match-p "Unsaved words" (test-word-target--text dir "novel.org"))))))

(ert-deftest test-word-target-health-objective-sum-follows-the-shared-helper ()
  (test-word-target--with-project dir 'novel 'en 80000
    (let ((novel (expand-file-name "novel.org" dir)))
      (should (= (cdr (org-scribe--health-word-totals novel)) 80000))
      (org-scribe-set-word-target 50000 'reset t)
      (should (= (cdr (org-scribe--health-word-totals novel)) 50000)))))

(provide 'test-word-target)
;;; test-word-target.el ends here
