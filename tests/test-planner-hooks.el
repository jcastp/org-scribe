;;; test-hooks.el --- Tests for integration hooks and pluggable fn-vars -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests verifying that integration hooks and pluggable function variables
;; work correctly in isolation, without requiring org-scribe to be present.

;;; Code:

(require 'ert)
(require 'org-scribe-planner)

;;; Helpers

(defmacro test-hooks--with-plan-file (plan-var file-var &rest body)
  "Bind PLAN-VAR and FILE-VAR to a temporary plan and file, then run BODY.
Cleans up the temp file unconditionally."
  (declare (indent 2))
  `(let* ((,file-var (make-temp-file "test-hooks-plan-" nil ".org"))
          (,plan-var (make-org-scribe-plan
                      :title "Hook Test Plan"
                      :total-words 10000
                      :daily-words 500
                      :days 20
                      :start-date "2026-01-01"
                      :end-date "2026-01-20"
                      :current-words 0)))
     (unwind-protect
         (progn
           (org-scribe-planner--save-plan ,plan-var ,file-var)
           ,@body)
       (ignore-errors (delete-file ,file-var)))))

;;; Tests for after-plan-load-hook

(ert-deftest test-planner-hooks-after-plan-load-hook-fires-on-load-plan ()
  "after-plan-load-hook is called with (plan file) when load-plan succeeds."
  (test-hooks--with-plan-file plan file
    (let* ((hook-calls nil)
           (org-scribe-planner-after-plan-load-hook
            (list (lambda (p f) (push (list p f) hook-calls))))
           (org-scribe-planner--current-plan nil)
           (org-scribe-planner--current-plan-file nil)
           (org-scribe-planner-project-root-function nil))
      ;; Simulate load-plan by calling it non-interactively after stubbing
      ;; read-file-name so it returns our temp file without prompting.
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (&rest _) file))
                ((symbol-function 'org-scribe-planner-show-calendar)
                 (lambda (&rest _) nil)))
        (org-scribe-planner-load-plan))
      (should (= (length hook-calls) 1))
      (let ((args (car hook-calls)))
        (should (org-scribe-plan-p (nth 0 args)))
        (should (string= (nth 1 args) file))))))

(ert-deftest test-planner-hooks-after-plan-load-hook-fires-on-new-plan ()
  "after-plan-load-hook is called when new-plan activates a plan."
  (let* ((hook-calls nil)
         (org-scribe-planner-after-plan-load-hook
          (list (lambda (p f) (push (list p f) hook-calls))))
         (org-scribe-planner--current-plan nil)
         (org-scribe-planner--current-plan-file nil)
         (temp-dir (make-temp-file "test-hooks-newplan-" t))
         (temp-file (expand-file-name "hook-new-plan.org" temp-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'read-string)
                   (lambda (prompt &rest _)
                     (if (string-match-p "[Tt]itle" prompt) "Hook Plan" "")))
                  ((symbol-function 'completing-read)
                   (lambda (_ choices &rest _)
                     ;; Choose "Total words + Days → Calculate daily words"
                     (car choices)))
                  ((symbol-function 'org-scribe-planner--read-positive-number)
                   (lambda (&rest _) 10000))
                  ((symbol-function 'org-scribe-planner--read-days)
                   (lambda (&rest _) 20))
                  ((symbol-function 'org-scribe-planner--read-date)
                   (lambda (&rest _) "2026-01-01"))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                  ((symbol-function 'read-file-name)
                   (lambda (&rest _) temp-file))
                  ((symbol-function 'org-scribe-planner-show-calendar)
                   (lambda (&rest _) nil)))
          (org-scribe-planner-new-plan))
      (delete-directory temp-dir t))
    (should (>= (length hook-calls) 1))
    (let ((args (car hook-calls)))
      (should (org-scribe-plan-p (nth 0 args))))))

;;; Tests for after-progress-update-hook

(ert-deftest test-planner-hooks-after-progress-update-hook-fires-cumulative ()
  "after-progress-update-hook fires with (plan count nil) on update-progress."
  (test-hooks--with-plan-file plan file
    (let* ((hook-calls nil)
           (org-scribe-planner-after-progress-update-hook
            (list (lambda (p c d) (push (list p c d) hook-calls))))
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file file)
           (org-scribe-planner-wordcount-function nil))
      (cl-letf (((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) 5000))
                ((symbol-function 'org-scribe-planner-show-calendar)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-scribe-planner--show-progress-report)
                 (lambda (&rest _) nil)))
        (org-scribe-planner-update-progress))
      (should (= (length hook-calls) 1))
      (let ((args (car hook-calls)))
        (should (org-scribe-plan-p (nth 0 args)))
        (should (= (nth 1 args) 5000))
        (should (null (nth 2 args)))))))  ; date is nil for cumulative update

(ert-deftest test-planner-hooks-after-progress-update-hook-fires-daily ()
  "after-progress-update-hook fires with (plan count date) on update-daily."
  (test-hooks--with-plan-file plan file
    (let* ((hook-calls nil)
           (org-scribe-planner-after-progress-update-hook
            (list (lambda (p c d) (push (list p c d) hook-calls))))
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file file)
           (org-scribe-planner--schedule-cache nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "2026-01-01"))
                ((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) 750))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "good session"))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (org-scribe-planner-update-daily-word-count))
      (should (= (length hook-calls) 1))
      (let ((args (car hook-calls)))
        (should (org-scribe-plan-p (nth 0 args)))
        (should (= (nth 1 args) 750))
        (should (string= (nth 2 args) "2026-01-01"))))))

;;; Tests for wordcount-function

(ert-deftest test-planner-hooks-wordcount-function-used-when-set ()
  "update-progress uses wordcount-function return value without prompting."
  (test-hooks--with-plan-file plan file
    (let* ((prompt-called nil)
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file file)
           (org-scribe-planner-after-progress-update-hook nil)
           (org-scribe-planner-wordcount-function (lambda () 12345)))
      (cl-letf (((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) (setq prompt-called t) 0))
                ((symbol-function 'org-scribe-planner-show-calendar)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-scribe-planner--show-progress-report)
                 (lambda (&rest _) nil)))
        (org-scribe-planner-update-progress))
      ;; Prompt must NOT have been called
      (should-not prompt-called)
      ;; Plan must reflect the value from wordcount-function
      (should (= (org-scribe-plan-current-words org-scribe-planner--current-plan)
                 12345)))))

(ert-deftest test-planner-hooks-wordcount-function-nil-falls-back-to-prompt ()
  "update-progress falls back to read-number when wordcount-function returns nil."
  (test-hooks--with-plan-file plan file
    (let* ((prompt-called nil)
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file file)
           (org-scribe-planner-after-progress-update-hook nil)
           ;; Function that returns nil → should trigger the prompt
           (org-scribe-planner-wordcount-function (lambda () nil)))
      (cl-letf (((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) (setq prompt-called t) 9999))
                ((symbol-function 'org-scribe-planner-show-calendar)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-scribe-planner--show-progress-report)
                 (lambda (&rest _) nil)))
        (org-scribe-planner-update-progress))
      ;; Prompt MUST have been called since function returned nil
      (should prompt-called)
      (should (= (org-scribe-plan-current-words org-scribe-planner--current-plan)
                 9999)))))

;;; Tests for project-root-function

(ert-deftest test-planner-hooks-project-root-function-used-by-load-plan ()
  "load-plan passes project-root-function result to the file picker."
  (test-hooks--with-plan-file plan file
    (let* ((picker-dir nil)
           (org-scribe-planner--current-plan nil)
           (org-scribe-planner--current-plan-file nil)
           (org-scribe-planner-after-plan-load-hook nil)
           (org-scribe-planner-project-root-function
            (lambda () "/tmp/my-writing-project")))
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (_ dir &rest _)
                   (setq picker-dir dir)
                   file))
                ((symbol-function 'org-scribe-planner-show-calendar)
                 (lambda (&rest _) nil)))
        (org-scribe-planner-load-plan))
      (should (string= picker-dir "/tmp/my-writing-project")))))

;;; Tests for org-scribe-planner-today

(ert-deftest test-planner-today-logs-todays-date ()
  "org-scribe-planner-today records words for today without a date picker."
  (test-hooks--with-plan-file plan file
    (let* ((org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file file)
           (org-scribe-planner-after-progress-update-hook nil)
           (org-scribe-planner-wordcount-function nil)
           (org-scribe-planner--schedule-cache nil))
      (cl-letf (((symbol-function 'org-scribe-planner--get-today-date)
                 (lambda () "2026-01-05"))
                ((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) 600))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "")))
        (org-scribe-planner-today))
      (let ((entry (assoc "2026-01-05"
                          (org-scribe-plan-daily-word-counts
                           org-scribe-planner--current-plan))))
        (should entry)
        (should (= (plist-get (cdr entry) :words) 600))))))

(ert-deftest test-planner-today-wordcount-function-skips-prompt ()
  "org-scribe-planner-today uses wordcount-function and skips the word-count prompt."
  (test-hooks--with-plan-file plan file
    (let* ((prompt-called nil)
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file file)
           (org-scribe-planner-after-progress-update-hook nil)
           (org-scribe-planner-wordcount-function (lambda () 999))
           (org-scribe-planner--schedule-cache nil))
      (cl-letf (((symbol-function 'org-scribe-planner--get-today-date)
                 (lambda () "2026-01-05"))
                ((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) (setq prompt-called t) 0))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "")))
        (org-scribe-planner-today))
      (should-not prompt-called)
      (let ((entry (assoc "2026-01-05"
                          (org-scribe-plan-daily-word-counts
                           org-scribe-planner--current-plan))))
        (should (= (plist-get (cdr entry) :words) 999))))))

(ert-deftest test-planner-today-hook-fires-with-todays-date ()
  "org-scribe-planner-today fires after-progress-update-hook with today's date."
  (test-hooks--with-plan-file plan file
    (let* ((hook-calls nil)
           (org-scribe-planner-after-progress-update-hook
            (list (lambda (p c d) (push (list p c d) hook-calls))))
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file file)
           (org-scribe-planner-wordcount-function nil)
           (org-scribe-planner--schedule-cache nil))
      (cl-letf (((symbol-function 'org-scribe-planner--get-today-date)
                 (lambda () "2026-01-05"))
                ((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) 300))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "")))
        (org-scribe-planner-today))
      (should (= (length hook-calls) 1))
      (let ((args (car hook-calls)))
        (should (org-scribe-plan-p (nth 0 args)))
        (should (= (nth 1 args) 300))
        (should (string= (nth 2 args) "2026-01-05"))))))

(ert-deftest test-planner-today-no-recalculate-prompt ()
  "org-scribe-planner-today does not ask about recalculation."
  (test-hooks--with-plan-file plan file
    (let* ((y-or-n-called nil)
           (org-scribe-planner--current-plan plan)
           (org-scribe-planner--current-plan-file file)
           (org-scribe-planner-after-progress-update-hook nil)
           (org-scribe-planner-wordcount-function nil)
           (org-scribe-planner--schedule-cache nil))
      (cl-letf (((symbol-function 'org-scribe-planner--get-today-date)
                 (lambda () "2026-01-05"))
                ((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) 400))
                ((symbol-function 'read-string)
                 (lambda (&rest _) ""))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq y-or-n-called t) nil)))
        (org-scribe-planner-today))
      (should-not y-or-n-called))))

;;; Tests for org-scribe-planner--load-plan-project-aware

(ert-deftest test-planner-hooks-load-plan-skips-picker-in-project ()
  "load-plan loads plan.org directly when inside a project — no file picker."
  (test-hooks--with-plan-file plan plan-file
    (let* ((temp-dir (make-temp-file "test-planner-lp-" t))
           (project-plan (expand-file-name "plan.org" temp-dir))
           (picker-called nil)
           (org-scribe-planner--current-plan nil)
           (org-scribe-planner--current-plan-file nil)
           (org-scribe-planner-after-plan-load-hook nil))
      (unwind-protect
          (progn
            ;; Write marker file and copy the plan into the project dir
            (with-temp-file (expand-file-name ".org-scribe-project" temp-dir)
              (insert "# Writing project: Test Novel\n# Type: novel\n"))
            (copy-file plan-file project-plan)
            (cl-letf (((symbol-function 'org-scribe-project-root)
                       (lambda () temp-dir))
                      ((symbol-function 'org-scribe-planner--select-plan-file)
                       (lambda (&rest _) (setq picker-called t) nil))
                      ((symbol-function 'org-scribe-planner-show-calendar)
                       (lambda (&rest _) nil)))
              (org-scribe-planner-load-plan))
            ;; The advice must have intercepted --select-plan-file
            (should-not picker-called)
            ;; The plan must now be active
            (should org-scribe-planner--current-plan)
            (should (string= org-scribe-planner--current-plan-file project-plan)))
        (delete-directory temp-dir t)))))

(ert-deftest test-planner-hooks-load-plan-uses-picker-outside-project ()
  "load-plan uses the file picker when not inside an org-scribe project."
  (test-hooks--with-plan-file _plan plan-file
    (let ((picker-called nil)
          (org-scribe-planner--current-plan nil)
          (org-scribe-planner--current-plan-file nil)
          (org-scribe-planner-after-plan-load-hook nil))
      (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () nil))
                ((symbol-function 'org-scribe-planner--select-plan-file)
                 (lambda (&rest _) (setq picker-called t) nil))
                ((symbol-function 'org-scribe-planner-show-calendar)
                 (lambda (&rest _) nil)))
        (org-scribe-planner-load-plan))
      (should picker-called))))

(ert-deftest test-planner-hooks-load-plan-uses-picker-when-no-plan-file ()
  "load-plan uses the file picker when in a project that has no plan.org."
  (let* ((temp-dir (make-temp-file "test-planner-lp-nf-" t))
         (picker-called nil)
         (org-scribe-planner--current-plan nil)
         (org-scribe-planner--current-plan-file nil)
         (org-scribe-planner-after-plan-load-hook nil))
    (unwind-protect
        (progn
          ;; Marker file present but no plan.org
          (with-temp-file (expand-file-name ".org-scribe-project" temp-dir)
            (insert "# Writing project: Test Novel\n# Type: novel\n"))
          (cl-letf (((symbol-function 'org-scribe-project-root)
                     (lambda () temp-dir))
                    ((symbol-function 'org-scribe-planner--select-plan-file)
                     (lambda (&rest _) (setq picker-called t) nil))
                    ((symbol-function 'org-scribe-planner-show-calendar)
                     (lambda (&rest _) nil)))
            (org-scribe-planner-load-plan))
          (should picker-called))
      (delete-directory temp-dir t))))

;;; Tests for org-scribe-planner--plan-file-valid-p

(ert-deftest test-planner-hooks-plan-file-valid-p-nil-for-comment-placeholder ()
  "plan-file-valid-p returns nil for a comment-only placeholder."
  (let ((f (make-temp-file "test-plan-valid-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file f
            (insert "# plan.org — writing plan placeholder\n")
            (insert "# Run M-x org-scribe-plan to create your plan.\n"))
          (should-not (org-scribe-planner--plan-file-valid-p f)))
      (delete-file f))))

(ert-deftest test-planner-hooks-plan-file-valid-p-nil-for-empty-file ()
  "plan-file-valid-p returns nil for an empty file."
  (let ((f (make-temp-file "test-plan-valid-" nil ".org")))
    (unwind-protect
        (should-not (org-scribe-planner--plan-file-valid-p f))
      (delete-file f))))

(ert-deftest test-planner-hooks-plan-file-valid-p-nil-for-missing-file ()
  "plan-file-valid-p returns nil when the file does not exist."
  (should-not (org-scribe-planner--plan-file-valid-p "/nonexistent/plan.org")))

(ert-deftest test-planner-hooks-plan-file-valid-p-t-for-real-plan ()
  "plan-file-valid-p returns t for a file saved by the planner."
  (test-hooks--with-plan-file plan file
    (should (org-scribe-planner--plan-file-valid-p file))))

;;; Tests for org-scribe-planner--project-title

(ert-deftest test-planner-hooks-project-title-reads-from-marker ()
  "org-scribe-planner--project-title returns title from .org-scribe-project."
  (let ((temp-dir (make-temp-file "test-planner-title-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".org-scribe-project" temp-dir)
            (insert "# Writing project: My Test Novel\n# Type: novel\n"))
          (should (string= (org-scribe-planner--project-title temp-dir)
                           "My Test Novel")))
      (delete-directory temp-dir t))))

(ert-deftest test-planner-hooks-project-title-nil-without-marker ()
  "org-scribe-planner--project-title returns nil when no marker file exists."
  (let ((temp-dir (make-temp-file "test-planner-title-" t)))
    (unwind-protect
        (should (null (org-scribe-planner--project-title temp-dir)))
      (delete-directory temp-dir t))))

(ert-deftest test-planner-hooks-project-title-nil-without-title-line ()
  "org-scribe-planner--project-title returns nil when marker has no title line."
  (let ((temp-dir (make-temp-file "test-planner-title-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".org-scribe-project" temp-dir)
            (insert "# Type: novel\n# Created: 2026-01-01\n"))
          (should (null (org-scribe-planner--project-title temp-dir))))
      (delete-directory temp-dir t))))

;;; Tests for org-scribe-plan

(ert-deftest test-planner-hooks-org-scribe-plan-shows-active-plan ()
  "org-scribe-plan shows calendar when a plan is already active."
  (test-hooks--with-plan-file plan file
    (let ((calendar-called nil)
          (org-scribe-planner--current-plan plan)
          (org-scribe-planner--current-plan-file file))
      (cl-letf (((symbol-function 'org-scribe-planner-show-calendar)
                 (lambda (&rest _) (setq calendar-called t))))
        (org-scribe-plan))
      (should calendar-called))))

(ert-deftest test-planner-hooks-org-scribe-plan-loads-and-shows-existing-plan ()
  "org-scribe-plan silently loads plan.org and shows calendar when no plan is active."
  (test-hooks--with-plan-file plan plan-file
    (let* ((temp-dir (make-temp-file "test-planner-ep-" t))
           (project-plan (expand-file-name "plan.org" temp-dir))
           (calendar-called nil)
           (hook-calls 0)
           (org-scribe-planner--current-plan nil)
           (org-scribe-planner--current-plan-file nil)
           (org-scribe-planner-after-plan-load-hook
            (list (lambda (&rest _) (setq hook-calls (1+ hook-calls))))))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name ".org-scribe-project" temp-dir)
              (insert "# Writing project: Test Novel\n# Type: novel\n"))
            (copy-file plan-file project-plan)
            (cl-letf (((symbol-function 'org-scribe-project-root)
                       (lambda () temp-dir))
                      ((symbol-function 'org-scribe-planner-show-calendar)
                       (lambda (&rest _) (setq calendar-called t))))
              (org-scribe-plan))
            (should calendar-called)
            (should (= hook-calls 1))
            (should org-scribe-planner--current-plan)
            (should (string= org-scribe-planner--current-plan-file project-plan)))
        (delete-directory temp-dir t)))))

(ert-deftest test-planner-hooks-org-scribe-plan-offers-create-when-no-plan ()
  "org-scribe-plan offers to create a plan when in project with no plan file."
  (let* ((temp-dir (make-temp-file "test-planner-ep-np-" t))
         (new-plan-called nil)
         (org-scribe-planner--current-plan nil)
         (org-scribe-planner--current-plan-file nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".org-scribe-project" temp-dir)
            (insert "# Writing project: Test Novel\n# Type: novel\n"))
          (cl-letf (((symbol-function 'org-scribe-project-root)
                     (lambda () temp-dir))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'org-scribe-planner-new-plan)
                     (lambda () (setq new-plan-called t))))
            (org-scribe-plan))
          (should new-plan-called))
      (delete-directory temp-dir t))))

(ert-deftest test-planner-hooks-org-scribe-plan-falls-back-outside-project ()
  "org-scribe-plan falls back to show-current-plan when not in a project."
  (let ((show-called nil)
        (org-scribe-planner--current-plan nil)
        (org-scribe-planner--current-plan-file nil))
    (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () nil))
              ((symbol-function 'org-scribe-planner-show-current-plan)
               (lambda () (setq show-called t))))
      (org-scribe-plan))
    (should show-called)))

;;; Regression: placeholder plan.org does not cause load errors

(ert-deftest test-planner-hooks-org-scribe-plan-treats-placeholder-as-no-plan ()
  "org-scribe-plan offers to create when plan.org is a comment-only placeholder."
  (let* ((temp-dir (make-temp-file "test-planner-stub-" t))
         (new-plan-called nil)
         (org-scribe-planner--current-plan nil)
         (org-scribe-planner--current-plan-file nil))
    (unwind-protect
        (progn
          ;; Create project marker and a comment-only placeholder
          (with-temp-file (expand-file-name ".org-scribe-project" temp-dir)
            (insert "# Writing project: Stub Test\n# Type: novel\n"))
          (with-temp-file (expand-file-name "plan.org" temp-dir)
            (insert "# plan.org — placeholder\n"))
          (cl-letf (((symbol-function 'org-scribe-project-root)
                     (lambda () temp-dir))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'org-scribe-planner-new-plan)
                     (lambda () (setq new-plan-called t))))
            (org-scribe-plan))
          ;; new-plan must have been called, not a load attempt
          (should new-plan-called))
      (delete-directory temp-dir t))))

(ert-deftest test-planner-hooks-load-plan-falls-through-for-placeholder ()
  "load-plan uses the file picker when plan.org is a comment-only placeholder."
  (let* ((temp-dir (make-temp-file "test-planner-stub-lp-" t))
         (picker-called nil)
         (org-scribe-planner--current-plan nil)
         (org-scribe-planner--current-plan-file nil)
         (org-scribe-planner-after-plan-load-hook nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".org-scribe-project" temp-dir)
            (insert "# Writing project: Stub Test\n# Type: novel\n"))
          (with-temp-file (expand-file-name "plan.org" temp-dir)
            (insert "# plan.org — placeholder\n"))
          (cl-letf (((symbol-function 'org-scribe-project-root)
                     (lambda () temp-dir))
                    ((symbol-function 'org-scribe-planner--select-plan-file)
                     (lambda (&rest _) (setq picker-called t) nil))
                    ((symbol-function 'org-scribe-planner-show-calendar)
                     (lambda (&rest _) nil)))
            (org-scribe-planner-load-plan))
          ;; Picker must have been shown (advice fell through)
          (should picker-called))
      (delete-directory temp-dir t))))

;;; Regression: update-daily-word-count refreshes calendar even when user declines recalculation

(ert-deftest test-planner-hooks-update-daily-count-shows-calendar-on-no ()
  "update-daily-word-count refreshes the calendar even when user declines recalculation.
Regression: count was saved but calendar not refreshed, making it appear lost."
  (test-hooks--with-plan-file plan file
    (let ((calendar-calls 0)
          (recalc-calls 0)
          (org-scribe-planner--current-plan plan)
          (org-scribe-planner--current-plan-file file))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "2026-01-05"))
                ((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) 300))
                ((symbol-function 'read-string) (lambda (&rest _) ""))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)) ; user says NO
                ((symbol-function 'org-scribe-planner-recalculate-remaining-days)
                 (lambda (&rest _) (cl-incf recalc-calls)))
                ((symbol-function 'org-scribe-planner-show-calendar)
                 (lambda (&rest _) (cl-incf calendar-calls))))
        (org-scribe-planner-update-daily-word-count))
      ;; Count must be registered
      (should (assoc "2026-01-05" (org-scribe-plan-daily-word-counts
                                   org-scribe-planner--current-plan)))
      ;; Calendar must refresh even though recalculation was declined
      (should (= 1 calendar-calls))
      (should (= 0 recalc-calls)))))

(ert-deftest test-planner-hooks-add-session-note-never-recalculates ()
  "add-session-note always shows the calendar; it has no recalculation prompt.
The old update-daily-word-count alias must exhibit the same behaviour."
  (test-hooks--with-plan-file plan file
    (let ((recalc-calls 0)
          (calendar-calls 0)
          (org-scribe-planner--current-plan plan)
          (org-scribe-planner--current-plan-file file))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "2026-01-05"))
                ((symbol-function 'org-scribe-planner--read-non-negative-number)
                 (lambda (&rest _) 300))
                ((symbol-function 'read-string) (lambda (&rest _) ""))
                ((symbol-function 'org-scribe-planner-recalculate-remaining-days)
                 (lambda (&rest _) (cl-incf recalc-calls)))
                ((symbol-function 'org-scribe-planner-show-calendar)
                 (lambda (&rest _) (cl-incf calendar-calls))))
        (org-scribe-planner-add-session-note))
      ;; No recalculation prompt — always zero
      (should (= 0 recalc-calls))
      ;; Calendar is always refreshed
      (should (= 1 calendar-calls)))))

;;; Tests for --sum-wordcounts (double-counting regression)

(defun test-hooks--make-org-with-wordcounts (content)
  "Write CONTENT to a temp file and return the file path."
  (let ((f (make-temp-file "test-wc-" nil ".org")))
    (with-temp-file f (insert content))
    f))

(ert-deftest test-planner-sum-wordcounts-flat ()
  "sum-wordcounts returns the correct total when every heading is a leaf."
  (let ((f (test-hooks--make-org-with-wordcounts
            "* Scene 1\n:PROPERTIES:\n:WORDCOUNT: 300\n:END:\n\n* Scene 2\n:PROPERTIES:\n:WORDCOUNT: 200\n:END:\n")))
    (unwind-protect
        (should (= 500 (org-scribe-planner--sum-wordcounts f)))
      (delete-file f))))

(ert-deftest test-planner-sum-wordcounts-hierarchical-no-double-count ()
  "sum-wordcounts counts only the topmost WORDCOUNT in each subtree.
Regression: previously summed every level, giving 3× the real count when
WORDCOUNT is set on act, chapter, and scene headings simultaneously."
  (let ((f (test-hooks--make-org-with-wordcounts
            (concat "* Act I\n:PROPERTIES:\n:WORDCOUNT: 500\n:END:\n"
                    "** Chapter 1\n:PROPERTIES:\n:WORDCOUNT: 300\n:END:\n"
                    "*** Scene 1\n:PROPERTIES:\n:WORDCOUNT: 300\n:END:\n"
                    "** Chapter 2\n:PROPERTIES:\n:WORDCOUNT: 200\n:END:\n"
                    "*** Scene 2\n:PROPERTIES:\n:WORDCOUNT: 200\n:END:\n"
                    "* Act II\n:PROPERTIES:\n:WORDCOUNT: 400\n:END:\n"
                    "** Chapter 3\n:PROPERTIES:\n:WORDCOUNT: 400\n:END:\n"))))
    (unwind-protect
        ;; Only Act I (500) and Act II (400) should be counted, not their children.
        (should (= 900 (org-scribe-planner--sum-wordcounts f)))
      (delete-file f))))

(ert-deftest test-planner-sum-wordcounts-mixed-levels ()
  "sum-wordcounts handles subtrees where only some levels have WORDCOUNT."
  (let ((f (test-hooks--make-org-with-wordcounts
            (concat "* Act I\n:PROPERTIES:\n:WORDCOUNT: 600\n:END:\n"
                    "** Chapter 1\n:PROPERTIES:\n:WORDCOUNT: 600\n:END:\n"
                    ;; No WORDCOUNT on Act II — leaf chapters counted directly
                    "* Act II\n"
                    "** Chapter 2\n:PROPERTIES:\n:WORDCOUNT: 350\n:END:\n"
                    "** Chapter 3\n:PROPERTIES:\n:WORDCOUNT: 250\n:END:\n"))))
    (unwind-protect
        ;; Act I (600) + Chapter 2 (350) + Chapter 3 (250) = 1200
        (should (= 1200 (org-scribe-planner--sum-wordcounts f)))
      (delete-file f))))

(ert-deftest test-planner-sum-wordcounts-zero-parents-skipped ()
  "sum-wordcounts skips child headings even when the parent WORDCOUNT is zero."
  (let ((f (test-hooks--make-org-with-wordcounts
            (concat "* Meta\n:PROPERTIES:\n:WORDCOUNT: 0\n:END:\n"
                    "** Notes\n:PROPERTIES:\n:WORDCOUNT: 0\n:END:\n"
                    "* Content\n:PROPERTIES:\n:WORDCOUNT: 800\n:END:\n"))))
    (unwind-protect
        ;; Meta (0) + Content (800); Notes is skipped (parent Meta has WORDCOUNT).
        (should (= 800 (org-scribe-planner--sum-wordcounts f)))
      (delete-file f))))

;;; test-hooks.el ends here
