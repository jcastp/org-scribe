;;; test-planner-outside.el --- Tests for words outside the manuscript -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for words that count toward the goal but aren't in the manuscript:
;; a typed number (OUTSIDE_WORDS) and files counted once or on every sync
;; (OUTSIDE_FILES).  The written total — ledger and CURRENT_WORDS — is the
;; manuscript's count plus these.  Covers the syncs, onboarding an empty
;; manuscript, the new-plan wizard, the "What changed?" option and its
;; repair of plans made before, and the plan file.

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
      (should (= (org-scribe-planner--written-total plan 1000 dir) 1155)))))

;;; Syncs

(ert-deftest test-planner-outside-record-total-adds-outside-words ()
  "The automatic sync records the manuscript plus the words outside it.
Regression: with 3,672 words declared but not in the manuscript, a sync of
500 manuscript words gave a day of -3,172 and a plan 3,672 words behind."
  (test-outside--quietly
    (let* ((plan (test-outside--plan :preexisting-words 3672 :current-words 3672
                                     :outside-words 3672))
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file "/tmp/unused-plan.org")
           (org-scribe-planner-auto-push-wordcount t)
           (org-scribe-planner-auto-track-daily t)
           (org-scribe-planner-after-progress-update-hook nil))
      (cl-letf (((symbol-function 'org-scribe-planner--ensure-plan-for-current-project) #'ignore))
        (org-scribe-planner-record-total 500))
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

(ert-deftest test-planner-outside-update-progress-adds-outside-words ()
  "`u' adds the words outside the manuscript to the count it reads."
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
            (should (= (org-scribe-plan-current-words plan) 4472))))
      (dolist (buffer (buffer-list))
        (when (and (buffer-file-name buffer) (string-prefix-p dir (buffer-file-name buffer)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest test-planner-outside-warns-below-start ()
  "A written total below the starting words says how to fix it."
  (let ((shown nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
      (org-scribe-planner--warn-if-below-start
       (make-org-scribe-plan :preexisting-words 3672 :current-words 500)))
    (should (string-match-p "Words outside the manuscript" shown))))

;;; Creating plans

(ert-deftest test-planner-outside-onboard-empty-manuscript ()
  "Onboarding an empty manuscript with words written elsewhere.
The report: an empty manuscript and 3,672 words declared ended up 3,672
words behind on the first sync."
  (test-outside--quietly
    (let ((org-scribe-planner-after-plan-load-hook nil)
          (org-scribe-planner--current-plan nil)
          (org-scribe-planner--current-plan-file nil)
          (org-scribe-planner-wordcount-function (lambda () 0))
          (numbers (list 80000)))
      (cl-letf (((symbol-function 'org-scribe-planner--read-positive-number)
                 (lambda (&rest _) (pop numbers)))
                ((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) 3672))
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
        (should (= (org-scribe-plan-outside-words plan) 3672))
        (should (= (org-scribe-plan-preexisting-words plan) 3672))
        (should (= (org-scribe-plan-current-words plan) 3672))
        ;; The first sync of 500 manuscript words is 500 written that day
        (let ((org-scribe-planner--current-plan plan)
              (org-scribe-planner--current-plan-file "/tmp/unused-plan.org")
              (org-scribe-planner-auto-push-wordcount t)
              (org-scribe-planner-auto-track-daily t)
              (org-scribe-planner-after-progress-update-hook nil))
          (cl-letf (((symbol-function 'org-scribe-planner--ensure-plan-for-current-project) #'ignore))
            (org-scribe-planner-record-total 500))
          (should (= (org-scribe-plan-current-words plan) 4172)))))))

(ert-deftest test-planner-outside-wizard-in-project ()
  "New plan in a project: manuscript count plus words elsewhere."
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
                ((symbol-function 'org-scribe-planner--read-non-negative-number) (lambda (&rest _) 500))
                ((symbol-function 'org-scribe-planner--read-date) (lambda (&rest _) "2026-10-12"))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (org-scribe-planner-new-plan "P" "/tmp/unused-plan.org"))
      (let ((plan org-scribe-planner--current-plan))
        (should (= (org-scribe-plan-outside-words plan) 500))
        (should (= (org-scribe-plan-preexisting-words plan) 1500))
        (should (= (org-scribe-plan-daily-words plan) 2850))))))

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

(ert-deftest test-planner-outside-repair-declared-words ()
  "Words already declared as starting words: the record is corrected."
  (let ((plan (test-outside--old-style-plan)))
    (should (eq (plist-get (test-outside--quietly (org-scribe-planner--status plan)) :status)
                'behind))
    (test-outside--change plan '("Words outside the manuscript" "Set the typed number"
                                 "Are among the starting words")
      ((symbol-function 'org-scribe-planner--read-non-negative-number) (lambda (&rest _) 3672)))
    (should (= (org-scribe-plan-outside-words plan) 3672))
    (should (= (org-scribe-plan-current-words plan) 4172))
    (should (= (org-scribe-plan-preexisting-words plan) 3672))
    (should (= (plist-get (cdr (assoc "2026-10-12" (org-scribe-plan-daily-word-counts plan))) :words)
               4172))))

(ert-deftest test-planner-outside-repair-new-starting-words ()
  "Words new to the plan: the starting words rise with them, progress doesn't."
  (let ((plan (test-outside--old-style-plan)))
    (test-outside--change plan '("Words outside the manuscript" "Set the typed number"
                                 "Are new to the plan")
      ((symbol-function 'org-scribe-planner--read-non-negative-number) (lambda (&rest _) 1000)))
    (should (= (org-scribe-plan-preexisting-words plan) 4672))
    (should (= (org-scribe-plan-current-words plan) 1500))
    (should (= (org-scribe-planner--entry-delta
                (org-scribe-plan-daily-word-counts plan) "2026-10-12"
                (org-scribe-plan-preexisting-words plan))
               (org-scribe-planner--entry-delta
                (list (cons "2026-10-12" (list :words 500))) "2026-10-12" 3672)))))

(ert-deftest test-planner-outside-repair-previews-both ()
  "Both ways are previewed with the written total and the status."
  (let ((plan (test-outside--old-style-plan))
        (offered nil)
        (answers (list "Words outside the manuscript" "Set the typed number")))
    (test-outside--quietly
      (let ((org-scribe-planner--current-plan plan)
            (org-scribe-planner--current-plan-file "/tmp/unused-plan.org"))
        (cl-letf (((symbol-function 'org-scribe-planner--read-non-negative-number)
                   (lambda (&rest _) 3672))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt collection &optional predicate &rest _)
                     (let ((candidates (all-completions "" collection predicate)))
                       (if answers
                           (let ((prefix (pop answers)))
                             (cl-find-if (lambda (c) (string-prefix-p prefix c)) candidates))
                         (setq offered candidates)
                         (car candidates))))))
          (org-scribe-planner-recalculate))))
    (should (= (length offered) 2))
    ;; Corrected: today's 500 words put the plan ahead, nothing behind
    (should (string-match-p "→ written 4172, 3672 at the start; Ahead: \\+500 words" (car offered)))))

(ert-deftest test-planner-outside-add-and-stop-counting-a-file ()
  "A file added counts toward the total; stopping counting takes it off."
  (test-outside--in-dir dir
    (test-outside--write (expand-file-name "draft.txt" dir) (string-join (make-list 300 "word") " "))
    (let ((plan (test-outside--old-style-plan))
          (file (expand-file-name "plan.org" dir)))
      (cl-letf (((symbol-function 'org-scribe-planner--outside-base) (lambda (&rest _) dir))
                ((symbol-function 'read-file-name)
                 (lambda (&rest _) (expand-file-name "draft.txt" dir))))
        (test-outside--change plan '("Words outside the manuscript" "Add a file"
                                     "Once" "Are new to the plan"))
        (should (equal (org-scribe-plan-outside-files plan) '((:path "draft.txt" :count 300))))
        (should (= (org-scribe-plan-current-words plan) 800))
        (test-outside--change plan '("Words outside the manuscript" "Stop counting"
                                     "draft.txt" "Are new to the plan"))
        (should-not (org-scribe-plan-outside-files plan))
        (should (= (org-scribe-plan-current-words plan) 500))
        (ignore file)))))

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
