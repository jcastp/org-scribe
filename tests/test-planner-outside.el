;;; test-planner-outside.el --- Tests for words outside the manuscript -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for words that count toward the goal but aren't in the manuscript:
;; a typed number (OUTSIDE_WORDS) and files counted once or on every sync
;; (OUTSIDE_FILES).  They are kept and shown, never counted: the written
;; total — ledger and CURRENT_WORDS — is the manuscript's count.  Covers the
;; syncs, onboarding an empty manuscript, the new-plan wizard, the "What
;; changed?" option, the conversion of plans saved when they were counted,
;; and the plan file.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-scribe-planner)

;;; Helpers

(defmacro test-outside--in-dir (dir &rest body)
  "Run BODY with DIR bound to a fresh temporary directory, deleted after."
  (declare (indent 1))
  `(let ((,dir (file-name-as-directory (make-temp-file "org-scribe-outside-" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,dir t))))

(defun test-outside--write (file text)
  "Write TEXT to FILE."
  (with-temp-file file (insert text)))

(defun test-outside--plan (&rest args)
  "80,000 words over 70 days from Oct 12, 2026, with ARGS."
  (let ((plan (apply #'make-org-scribe-plan
                     (append args (list :title "P" :total-words 80000 :days 70
                                        :start-date "2026-10-12")))))
    (org-scribe-planner--calculate-missing-variable plan)
    plan))

(defmacro test-outside--quietly (&rest body)
  "Run BODY on Oct 12, 2026, with saving, display and messages stubbed."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'org-scribe-planner--get-today-date) (lambda () "2026-10-12"))
             ((symbol-function 'org-scribe-planner--save-plan) #'ignore)
             ((symbol-function 'org-scribe-planner--refresh-agenda-if-synced) #'ignore)
             ((symbol-function 'org-scribe-planner-show-calendar) #'ignore)
             ((symbol-function 'org-scribe-planner--show-progress-report) #'ignore)
             ((symbol-function 'message) #'ignore))
     ,@body))

(defun test-outside--choose (&rest prefixes)
  "A `completing-read' stub answering with PREFIXES, one per call."
  (lambda (_prompt collection &optional predicate &rest _)
    (let ((prefix (pop prefixes)))
      (or (cl-find-if (lambda (c) (string-prefix-p prefix c))
                      (all-completions "" collection predicate))
          (error "No candidate starts with %S" prefix)))))

;;; Counting files

(ert-deftest test-planner-outside-count-plain-file ()
  "A text file is counted by words."
  (test-outside--in-dir dir
    (test-outside--write (expand-file-name "draft.txt" dir) "One two three four five.\nSix.")
    (should (= (org-scribe-planner--count-file-words (expand-file-name "draft.txt" dir)) 6))
    (should-not (org-scribe-planner--count-file-words (expand-file-name "missing.txt" dir)))))

(ert-deftest test-planner-outside-directory-is-not-a-countable-file ()
  "A directory has no words: nil, not a \"Is a directory\" error."
  (test-outside--in-dir dir
    (should-not (org-scribe-planner--count-file-words dir))
    (should-not (org-scribe-planner--count-file-words
                 (directory-file-name dir)))))

(ert-deftest test-planner-outside-accepting-the-directory-default-is-refused-not-fatal ()
  "Answering the file prompt with its default (the directory) must not abort.
It used to signal from `insert-file-contents', unwinding the whole
new-plan wizard and losing every answer typed so far."
  (test-outside--in-dir dir
    (let ((plan (make-org-scribe-plan))
          (told nil))
      (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) dir))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq told (apply #'format fmt args)))))
        (should-not (org-scribe-planner--add-outside-file plan dir)))
      (should (string-match-p "Can't read" told))
      (should-not (org-scribe-plan-outside-files plan)))))

(ert-deftest test-planner-outside-count-org-file-like-the-manuscript ()
  "An Org file is counted without its headings, drawers and comments."
  (skip-unless (and (fboundp 'org-scribe-accurate-wordcount-p)
                    (org-scribe-accurate-wordcount-p)))
  (test-outside--in-dir dir
    (test-outside--write (expand-file-name "draft.org" dir)
                         "* A heading of five words\n:PROPERTIES:\n:ID: abc\n:END:\n# a comment line\nOne two three.\n")
    (should (= (org-scribe-planner--count-file-words (expand-file-name "draft.org" dir)) 3))))

(ert-deftest test-planner-outside-total-typed-once-and-live ()
  "Typed words, a file counted once and a file counted on every sync."
  (test-outside--in-dir dir
    (test-outside--write (expand-file-name "live.txt" dir) "a b c")
    (let ((plan (make-org-scribe-plan
                 :outside-words 100
                 :outside-files (list (list :path "old.txt" :count 50)
                                      (list :path "live.txt" :live t)))))
      (should (= (org-scribe-planner--outside-total plan dir) 153))
      ;; The live file grows; the file counted once doesn't change
      (test-outside--write (expand-file-name "live.txt" dir) "a b c d e")
      (should (= (org-scribe-planner--outside-total plan dir) 155))
      (should (= (org-scribe-planner--written-total plan 1000 dir) 1000)))))

;;; Syncs

(ert-deftest test-planner-outside-record-total-ignores-outside-words ()
  "The automatic sync records the manuscript's count, outside words aside.
A sync of 500 manuscript words is a day of 500 whatever is written elsewhere."
  (test-outside--quietly
    (let* ((plan (test-outside--plan :preexisting-words 3672 :current-words 3672
                                     :outside-words 2000))
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file "/tmp/unused-plan.org")
           (org-scribe-planner-auto-push-wordcount t)
           (org-scribe-planner-auto-track-daily t)
           (org-scribe-planner-after-progress-update-hook nil))
      (cl-letf (((symbol-function 'org-scribe-planner--ensure-plan-for-current-project) #'ignore))
        (org-scribe-planner-record-total 4172))
      (should (= (org-scribe-plan-current-words plan) 4172))
      (should (= (org-scribe-planner--entry-delta
                  (org-scribe-plan-daily-word-counts plan) "2026-10-12" 3672)
                 500))
      (should-not (eq (plist-get (org-scribe-planner--status plan) :status) 'behind)))))

(ert-deftest test-planner-outside-update-progress-ignores-empty-manuscript ()
  "`u' with a counted manuscript of 0 changes nothing, like the automatic sync.
Regression: it stored 0, so a plan with pre-existing words fell behind
by all of them."
  (test-outside--quietly
    (let* ((plan (test-outside--plan :preexisting-words 3672 :current-words 3672))
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file "/tmp/unused-plan.org")
           (org-scribe-planner-wordcount-function (lambda () 0)))
      (org-scribe-planner-update-progress)
      (should (= (org-scribe-plan-current-words plan) 3672)))))

(ert-deftest test-planner-outside-update-progress-ignores-outside-words ()
  "`u' stores the count it reads, outside words aside."
  (let ((dir (file-name-as-directory (make-temp-file "org-scribe-outside-" t))))
    (unwind-protect
        (test-outside--quietly
          (let* ((file (expand-file-name "plan.org" dir))
                 (plan (test-outside--plan :preexisting-words 3672 :current-words 3672
                                           :outside-words 3672))
                 (org-scribe-planner--current-plan plan)
                 (org-scribe-planner--current-plan-file file)
                 (org-scribe-planner-wordcount-function (lambda () 800)))
            (test-outside--write file "* P\n:PROPERTIES:\n:TOTAL_WORDS: 80000\n:END:\n")
            (org-scribe-planner-update-progress)
            (should (= (org-scribe-plan-current-words plan) 800))))
      (dolist (buffer (buffer-list))
        (when (and (buffer-file-name buffer) (string-prefix-p dir (buffer-file-name buffer)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest test-planner-outside-warns-below-start ()
  "A written total below the starting words is reported."
  (let ((shown nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
      (org-scribe-planner--warn-if-below-start
       (make-org-scribe-plan :preexisting-words 3672 :current-words 500)))
    (should (string-match-p "fewer than the 3672" shown))))

(ert-deftest test-planner-outside-day-delta-is-manuscript-only ()
  "Manuscript 10,000 -> 10,500 while an outside file grows 2,000 -> 2,800: 500."
  (test-outside--in-dir dir
    (test-outside--write (expand-file-name "research.txt" dir)
                         (string-join (make-list 2800 "w") " "))
    (test-outside--quietly
      (let* ((plan (test-outside--plan
                    :preexisting-words 10000 :current-words 10000
                    :words-basis 'manuscript
                    :outside-files (list (list :path "research.txt" :live t))))
             (org-scribe-planner--current-plan plan)
             (org-scribe-planner--current-plan-file (expand-file-name "plan.org" dir))
             (org-scribe-planner-auto-push-wordcount t)
             (org-scribe-planner-auto-track-daily t)
             (org-scribe-planner-after-progress-update-hook nil))
        (cl-letf (((symbol-function 'org-scribe-planner--ensure-plan-for-current-project) #'ignore))
          (org-scribe-planner-record-total 10500))
        (should (= (org-scribe-plan-current-words plan) 10500))
        (should (= (org-scribe-planner--entry-delta
                    (org-scribe-plan-daily-word-counts plan) "2026-10-12" 10000)
                   500))
        (should (string-match-p
                 "Outside the manuscript: 2800 words (not counted)"
                 (org-scribe-planner--outside-line plan dir)))))))

;;; Creating plans

(ert-deftest test-planner-outside-onboard-empty-manuscript ()
  "Onboarding an empty manuscript starts from the manuscript's 0 words.
Words written elsewhere are not asked about, nor counted."
  (test-outside--quietly
    (let ((org-scribe-planner-after-plan-load-hook nil)
          (org-scribe-planner--current-plan nil)
          (org-scribe-planner--current-plan-file nil)
          (org-scribe-planner-wordcount-function (lambda () 0))
          (numbers (list 80000)))
      (cl-letf (((symbol-function 'org-scribe-planner--read-positive-number)
                 (lambda (&rest _) (pop numbers)))
                ((symbol-function 'read-string) (lambda (&rest _) "P"))
                ((symbol-function 'org-scribe-planner--read-date)
                 (lambda (prompt &rest _)
                   (if (string-match-p "start" prompt) "2026-10-12" "2026-12-20")))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "End date → calculate daily words"))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                ((symbol-function 'read-file-name) (lambda (&rest _) "/tmp/unused-plan.org"))
                ((symbol-function 'make-directory) #'ignore))
        (org-scribe-planner-onboard-existing-project))
      (let ((plan org-scribe-planner--current-plan))
        (should (eq (org-scribe-plan-words-basis plan) 'manuscript))
        (should (= (org-scribe-plan-outside-words plan) 0))
        (should (= (org-scribe-plan-preexisting-words plan) 0))
        (should (= (org-scribe-plan-current-words plan) 0))
        ;; The first sync of 500 manuscript words is 500 written that day
        (let ((org-scribe-planner--current-plan plan)
              (org-scribe-planner--current-plan-file "/tmp/unused-plan.org")
              (org-scribe-planner-auto-push-wordcount t)
              (org-scribe-planner-auto-track-daily t)
              (org-scribe-planner-after-progress-update-hook nil))
          (cl-letf (((symbol-function 'org-scribe-planner--ensure-plan-for-current-project) #'ignore))
            (org-scribe-planner-record-total 500))
          (should (= (org-scribe-plan-current-words plan) 500)))))))

(ert-deftest test-planner-outside-onboard-offers-the-project-target ()
  "Onboarding offers the project's recorded target as the total."
  (test-outside--quietly
    (let ((org-scribe-planner-after-plan-load-hook nil)
          (org-scribe-planner--current-plan nil)
          (org-scribe-planner--current-plan-file nil)
          (org-scribe-planner-wordcount-function (lambda () 0))
          (offered 'unset))
      (cl-letf (((symbol-function 'org-scribe-planner--project-word-target) (lambda () 50000))
                ((symbol-function 'org-scribe-planner--read-positive-number)
                 (lambda (prompt &optional default)
                   (when (string-match-p "Total word target" prompt) (setq offered default))
                   (or default 80000)))
                ((symbol-function 'read-string) (lambda (&rest _) "P"))
                ((symbol-function 'org-scribe-planner--read-date)
                 (lambda (prompt &rest _)
                   (if (string-match-p "start" prompt) "2026-10-12" "2026-12-20")))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "End date → calculate daily words"))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                ((symbol-function 'read-file-name) (lambda (&rest _) "/tmp/unused-plan.org"))
                ((symbol-function 'make-directory) #'ignore))
        (org-scribe-planner-onboard-existing-project))
      (should (= offered 50000))
      (should (= (org-scribe-plan-total-words org-scribe-planner--current-plan) 50000)))))

(ert-deftest test-planner-outside-wizard-in-project ()
  "New plan in a project starts from the manuscript count; nothing is asked about words elsewhere."
  (test-outside--quietly
    (let ((org-scribe-planner-after-plan-load-hook nil)
          (org-scribe-planner--current-plan nil)
          (org-scribe-planner--current-plan-file nil)
          (org-scribe-planner-wordcount-function (lambda () 1000)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt &rest _)
                   (if (string-prefix-p "Start from" prompt)
                       "From scratch"
                     "Total words + Days → Calculate daily words")))
                ((symbol-function 'org-scribe-planner--read-positive-number) (lambda (&rest _) 30000))
                ((symbol-function 'org-scribe-planner--read-days) (lambda (&rest _) 10))
                ((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) (error "Asked about words outside the manuscript")))
                ((symbol-function 'org-scribe-planner--read-date) (lambda (&rest _) "2026-10-12"))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (org-scribe-planner-new-plan "P" "/tmp/unused-plan.org"))
      (let ((plan org-scribe-planner--current-plan))
        (should (= (org-scribe-plan-outside-words plan) 0))
        (should (= (org-scribe-plan-preexisting-words plan) 1000))
        (should (= (org-scribe-plan-daily-words plan) 2900))))))

;;; What changed? → Words outside the manuscript

(defmacro test-outside--change (plan answers &rest stubs)
  "Run \"What changed?\" on PLAN answering ANSWERS, with STUBS."
  (declare (indent 2))
  `(test-outside--quietly
     (let ((org-scribe-planner--current-plan ,plan)
           (org-scribe-planner--current-plan-file "/tmp/unused-plan.org"))
       (cl-letf (((symbol-function 'completing-read) (apply #'test-outside--choose ,answers))
                 ,@stubs)
         (org-scribe-planner-recalculate)))))

(defun test-outside--old-style-plan ()
  "A plan made before: 3,672 declared, not in the manuscript; 500 synced today."
  (let ((plan (test-outside--plan :preexisting-words 3672)))
    (setf (org-scribe-plan-daily-word-counts plan)
          (list (cons "2026-10-12" (list :words 500 :note ""))))
    (setf (org-scribe-plan-current-words plan) 500)
    plan))

(ert-deftest test-planner-outside-changing-outside-words-moves-no-record ()
  "Typed words outside the manuscript are recorded; no count moves."
  (let ((plan (test-outside--old-style-plan)))
    (test-outside--change plan '("Words outside the manuscript" "Set the typed number")
      ((symbol-function 'org-scribe-planner--read-non-negative-number) (lambda (&rest _) 3672)))
    (should (= (org-scribe-plan-outside-words plan) 3672))
    (should (= (org-scribe-plan-current-words plan) 500))
    (should (= (org-scribe-plan-preexisting-words plan) 3672))
    (should (= (plist-get (cdr (assoc "2026-10-12" (org-scribe-plan-daily-word-counts plan))) :words)
               500))))

(ert-deftest test-planner-outside-add-and-stop-counting-a-file ()
  "A file added is recorded and shown, moving no count; stopping removes it."
  (test-outside--in-dir dir
    (test-outside--write (expand-file-name "draft.txt" dir) (string-join (make-list 300 "word") " "))
    (let ((plan (test-outside--old-style-plan)))
      (cl-letf (((symbol-function 'org-scribe-planner--outside-base) (lambda (&rest _) dir))
                ((symbol-function 'read-file-name)
                 (lambda (&rest _) (expand-file-name "draft.txt" dir))))
        (test-outside--change plan '("Words outside the manuscript" "Add a file" "Once"))
        (should (equal (org-scribe-plan-outside-files plan) '((:path "draft.txt" :count 300))))
        (should (= (org-scribe-plan-current-words plan) 500))
        (test-outside--change plan '("Words outside the manuscript" "Stop counting" "draft.txt"))
        (should-not (org-scribe-plan-outside-files plan))
        (should (= (org-scribe-plan-current-words plan) 500))))))

;;; Conversion of plans saved when outside words were counted

(defun test-outside--load-file (dir text)
  "Write TEXT to a plan file in DIR and return the plan loaded from it."
  (let ((file (expand-file-name "plan.org" dir)))
    (test-outside--write file text)
    (org-scribe-planner--load-plan file)))

(defconst test-outside--old-plan-text
  (concat "* P\n:PROPERTIES:\n:TOTAL_WORDS: 50000\n:DAILY_WORDS: 500\n:DAYS: 100\n"
          ":START_DATE: 2026-10-10\n:END_DATE: 2027-01-17\n:CURRENT_WORDS: 13300\n"
          ":PREEXISTING_WORDS: 2000\n:OUTSIDE_WORDS: 2000\n"
          ":DAILY_WORD_COUNTS: 2026-10-11:12000,2026-10-12:13300\n:END:\n")
  "A plan saved before the ledger stopped counting outside words.")

(defmacro test-outside--with-loaded (var text &rest body)
  "Bind VAR to the plan loaded from TEXT in a temporary directory; run BODY."
  (declare (indent 2))
  `(test-outside--in-dir dir
     (let ((,var nil))
       (unwind-protect
           (progn (cl-letf (((symbol-function 'message) #'ignore))
                    (setq ,var (test-outside--load-file dir ,text)))
                  ,@body)
         (dolist (buffer (buffer-list))
           (when (and (buffer-file-name buffer) (string-prefix-p dir (buffer-file-name buffer)))
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))))))

(ert-deftest test-planner-outside-convert-on-load-shifts-everything ()
  "Outside words come off every ledger entry, CURRENT_WORDS and the start."
  (test-outside--with-loaded plan test-outside--old-plan-text
    (should (equal (mapcar (lambda (e) (plist-get (cdr e) :words))
                           (org-scribe-planner--sorted-daily-counts
                            (org-scribe-plan-daily-word-counts plan)))
                   '(10000 11300)))
    (should (= (org-scribe-plan-current-words plan) 11300))
    (should (= (org-scribe-plan-preexisting-words plan) 0))
    (should (eq (org-scribe-plan-words-basis plan) 'manuscript))))

(ert-deftest test-planner-outside-convert-is-idempotent ()
  "A plan already marked WORDS_BASIS: manuscript is not shifted."
  (test-outside--with-loaded plan
      (replace-regexp-in-string ":OUTSIDE_WORDS: 2000"
                                ":OUTSIDE_WORDS: 2000\n:WORDS_BASIS: manuscript"
                                test-outside--old-plan-text t t)
    (should (= (org-scribe-plan-current-words plan) 13300))
    (should (= (org-scribe-plan-preexisting-words plan) 2000))))

(ert-deftest test-planner-outside-convert-without-outside-words ()
  "No outside words: nothing moves, the basis is set, nothing is said."
  (test-outside--in-dir dir
    (let ((told nil) (plan nil))
      (unwind-protect
          (cl-letf (((symbol-function 'message) (lambda (&rest _) (setq told t))))
            (setq plan (test-outside--load-file
                        dir (replace-regexp-in-string ":OUTSIDE_WORDS: 2000\n" ""
                                                      test-outside--old-plan-text)))
            (should (= (org-scribe-plan-current-words plan) 13300))
            (should (eq (org-scribe-plan-words-basis plan) 'manuscript))
            (should-not told))
        (dolist (buffer (buffer-list))
          (when (and (buffer-file-name buffer) (string-prefix-p dir (buffer-file-name buffer)))
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

(ert-deftest test-planner-outside-convert-roundtrip ()
  "Load, save, reload: same numbers, and WORDS_BASIS is in the file."
  (test-outside--with-loaded plan test-outside--old-plan-text
    (let ((file (expand-file-name "plan.org" dir)))
      (cl-letf (((symbol-function 'message) #'ignore))
        (org-scribe-planner--save-plan plan file)
        (with-temp-buffer
          (insert-file-contents file)
          (should (string-match-p "^:WORDS_BASIS: +manuscript" (buffer-string))))
        (dolist (buffer (buffer-list))
          (when (equal (buffer-file-name buffer) file)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))
        (let ((again (org-scribe-planner--load-plan file)))
          (should (= (org-scribe-plan-current-words again) 11300))
          (should (= (org-scribe-plan-preexisting-words again) 0)))))))

(ert-deftest test-planner-outside-shown-not-counted ()
  "The plan view says outside words are not counted, only when there are some."
  (test-outside--in-dir dir
    (cl-letf (((symbol-function 'org-scribe-planner--get-today-date) (lambda () "2026-10-12")))
      (let ((with (test-outside--plan :outside-words 2800 :words-basis 'manuscript))
            (without (test-outside--plan :words-basis 'manuscript))
            (file (expand-file-name "plan.org" dir)))
        (org-scribe-planner-show-calendar with file)
        (with-current-buffer org-scribe-planner-calendar-buffer
          (should (string-match-p "Outside the manuscript: 2800 words (not counted)"
                                  (buffer-string))))
        (org-scribe-planner-show-calendar without file)
        (with-current-buffer org-scribe-planner-calendar-buffer
          (should-not (string-match-p "not counted" (buffer-string))))))))

(ert-deftest test-planner-outside-file-default-mode-by-location ()
  "Files inside the project default to every sync; others to once."
  (test-outside--in-dir dir
    (test-outside--write (expand-file-name "inside.txt" dir) "a b")
    (let ((plan (make-org-scribe-plan))
          (default-offered nil))
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (&rest _) (expand-file-name "inside.txt" dir)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt _collection _pred _req _init _hist default)
                   (setq default-offered default)
                   default)))
        (org-scribe-planner--add-outside-file plan dir))
      (should (string-prefix-p "On every sync" default-offered))
      (should (equal (org-scribe-plan-outside-files plan) '((:path "inside.txt" :live t)))))))

(ert-deftest test-planner-outside-refuses-the-manuscript ()
  "The manuscript can't be counted as outside it."
  (test-outside--in-dir dir
    (let ((manuscript (expand-file-name "novela.org" dir))
          (plan (make-org-scribe-plan)))
      (test-outside--write manuscript "Some words.")
      (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) manuscript))
                ((symbol-function 'org-scribe-planner--manuscript-file) (lambda () manuscript))
                ((symbol-function 'message) #'ignore))
        (should-not (org-scribe-planner--add-outside-file plan dir)))
      (should-not (org-scribe-plan-outside-files plan)))))

(ert-deftest test-planner-outside-manuscript-count-excludes-noexport-apparatus ()
  "The manuscript total is the scenes' prose, not Project Goals and the like."
  (skip-unless (and (fboundp 'org-scribe-accurate-wordcount-p)
                    (org-scribe-accurate-wordcount-p)))
  (test-outside--in-dir base
    (let* ((project (expand-file-name "Novel" base))
           (file (progn (cl-letf (((symbol-function 'message) #'ignore))
                          (org-scribe-create-novel-project base "Novel" 'en))
                        (expand-file-name "novel.org" project)))
           (default-directory project))
      (unwind-protect
          (with-current-buffer (find-file-noselect file)
            (org-with-wide-buffer
             (goto-char (point-min))
             (re-search-forward "^\\*\\*\\* .*:ignore:")
             (org-back-to-heading t)
             (org-end-of-meta-data t)
             (insert "One two three four five.\n")
             (goto-char (point-min))
             (re-search-forward "^\\* Project Goals")
             (org-end-of-meta-data t)
             (insert "Words of apparatus that are never prose.\n"))
            (cl-letf (((symbol-function 'message) #'ignore))
              (org-scribe-ews-org-count-words))
            (should (= (org-scribe-planner--sum-wordcounts file) 5)))
        (dolist (buffer (buffer-list))
          (when (and (buffer-file-name buffer) (string-prefix-p base (buffer-file-name buffer)))
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))))))

;;; Plan file

(ert-deftest test-planner-outside-roundtrip ()
  "OUTSIDE_WORDS and OUTSIDE_FILES survive save→load."
  (test-outside--in-dir dir
    (let ((file (expand-file-name "plan.org" dir))
          (plan (test-outside--plan :outside-words 3672
                                    :outside-files (list (list :path "notas/draft.org" :live t)
                                                         (list :path "~/old.txt" :count 900)))))
      (unwind-protect
          (progn
            (org-scribe-planner--save-plan plan file)
            (let ((loaded (org-scribe-planner--load-plan file)))
              (should (= (org-scribe-plan-outside-words loaded) 3672))
              (should (equal (org-scribe-plan-outside-files loaded)
                             '((:path "notas/draft.org" :live t) (:path "~/old.txt" :count 900))))))
        (dolist (buffer (buffer-list))
          (when (equal (buffer-file-name buffer) file) (kill-buffer buffer)))))))

(ert-deftest test-planner-outside-malformed-files-property-ignored ()
  "A hand-edited OUTSIDE_FILES that isn't a list of plists is ignored."
  (test-outside--in-dir dir
    (let ((file (expand-file-name "plan.org" dir)))
      (test-outside--write file (concat "* P\n:PROPERTIES:\n:TOTAL_WORDS: 1000\n:DAILY_WORDS: 100\n"
                                        ":DAYS: 10\n:START_DATE: 2026-10-12\n:END_DATE: 2026-10-21\n"
                                        ":CURRENT_WORDS: 0\n:OUTSIDE_FILES: (oops\n:END:\n"))
      (unwind-protect
          (should-not (org-scribe-plan-outside-files (org-scribe-planner--load-plan file)))
        (dolist (buffer (buffer-list))
          (when (equal (buffer-file-name buffer) file) (kill-buffer buffer)))))))

(ert-deftest test-planner-outside-paths-follow-the-plan ()
  "Paths are re-anchored when the plan is saved elsewhere.
Relative when the file is inside the plan's directory, full otherwise."
  (let ((plan (make-org-scribe-plan :outside-files (list (list :path "../notas/a.org" :live t)))))
    (org-scribe-planner--rebase-outside-paths plan "/srv/u/project/plans/" "/srv/u/project/")
    (should (equal (plist-get (car (org-scribe-plan-outside-files plan)) :path) "notas/a.org"))
    (org-scribe-planner--rebase-outside-paths plan "/srv/u/project/" "/srv/u/elsewhere/")
    (should (equal (plist-get (car (org-scribe-plan-outside-files plan)) :path)
                   "/srv/u/project/notas/a.org"))))

(provide 'test-planner-outside)
;;; test-planner-outside.el ends here
