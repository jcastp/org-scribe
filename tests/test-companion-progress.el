;;; test-companion-progress.el --- Tests for the companion's progress data -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for N3 (`companion/org-scribe-companion-progress.el', GET
;; /api/progress): the manuscript against its objective, the writing plan as
;; the planner computes it, and the rule that the companion never loads the
;; planner on its own (checked in a fresh `emacs -Q', since the test runner
;; may have loaded it already).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (dolist (dir '("core" "lang" "linking" "search" "capture" "templates" "reporting"
                 "planning" "companion"))
    (add-to-list 'load-path (expand-file-name dir parent-dir))))

(require 'org-scribe-config)
(require 'org-scribe-core)
(require 'org-scribe-character-links)
(require 'org-scribe-location-links)
(require 'org-scribe-plot-links)
(require 'org-scribe-plot-point-links)
(require 'org-scribe-health)
(require 'org-scribe-companion)

(defvar org-scribe-planner--current-plan)
(defvar org-scribe-planner--current-plan-file)
(declare-function org-scribe-planner--get-today-target "org-scribe-planner-dashboards")
(declare-function org-scribe-planner--generate-day-schedule "org-scribe-planner")

(defconst test-companion-progress--repo
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the org-scribe checkout.")

(defun test-companion-progress--set-gate (root value)
  "Set ROOT's `# Planner:' line to VALUE, or remove it when VALUE is nil."
  (let ((file (expand-file-name ".org-scribe-project" root)))
    (with-temp-file file
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward "^# Planner: .*\n" nil t) (replace-match ""))
      (when value (goto-char (point-max)) (insert (format "# Planner: %s\n" value))))))

(defmacro test-companion-progress--with-project (root-var demo &rest body)
  "Run BODY with a copy of the example DEMO at ROOT-VAR, no plan active."
  (declare (indent 2))
  `(let* ((,root-var (file-name-as-directory (make-temp-file "test-companion-progress-" t)))
          (default-directory ,root-var)
          (org-id-locations-file (expand-file-name "ids" ,root-var))
          (make-backup-files nil)
          (create-lockfiles nil))
     (unwind-protect
         (progn
           (copy-directory (expand-file-name (concat "examples/" ,demo) test-companion-progress--repo)
                           ,root-var t t t)
           (setq org-scribe-planner--current-plan nil
                 org-scribe-planner--current-plan-file nil)
           (org-scribe-project-type-cache-clear)
           (org-scribe-companion--clear-cache)
           ,@body)
       (setq org-scribe-planner--current-plan nil
             org-scribe-planner--current-plan-file nil)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (string-prefix-p ,root-var file)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (org-scribe-project-type-cache-clear)
       (org-scribe-companion--clear-cache)
       (delete-directory ,root-var t))))

(defmacro test-companion-progress--on (day &rest body)
  "Run BODY with the planner's today being DAY (YYYY-MM-DD)."
  (declare (indent 1))
  `(progn
     (require 'org-scribe-planner)
     (cl-letf (((symbol-function 'org-scribe-planner--get-today-date) (lambda () ,day)))
       ,@body)))

(ert-deftest test-companion-progress-plan-during ()
  "The plan's figures are the planner's own."
  (test-companion-progress--with-project root "demo-novel"
    (test-companion-progress--on "2026-05-08"
      (let* ((answer (org-scribe-companion-progress-get root))
             (plan (plist-get answer :plan))
             (days (plist-get plan :days)))
        (should (eq t (plist-get answer :ok)))
        (should (equal "active" (plist-get plan :state)))
        (should (equal "during" (plist-get plan :phase)))
        (should (= 80000 (plist-get plan :total)))
        (should (equal (org-scribe-planner--get-today-target org-scribe-planner--current-plan)
                       (plist-get plan :todayTarget)))
        (should (= (length (org-scribe-planner--generate-day-schedule org-scribe-planner--current-plan))
                   (length days)))
        ;; written up to today, nothing after
        (let ((today (cl-position "2026-05-08" days :key (lambda (d) (plist-get d :date)) :test #'equal)))
          (should (numberp (plist-get (aref days today) :actual)))
          (should (eq :null (plist-get (aref days (1+ today)) :actual))))
        (should (member (plist-get plan :status) '("ahead" "behind" "on-track")))
        (should (stringp (json-serialize answer)))))))

;; Reported 2026-11-01: after a recalculation on day one the burndown
;; started at 49,333 words left instead of 50,000.
(ert-deftest test-companion-progress-recalculated-plan ()
  "Planned totals before the recalculation are the plan as it stood, and
the step is sent for the page to draw."
  (require 'org-scribe-planner)
  (require 'org-scribe-planner-dashboards)
  (test-companion-progress--on "2026-11-01"
    ;; The planner loads at run time: no `setf' on its struct here
    (let ((plan (make-org-scribe-plan
                 :title "P" :total-words 50000 :days 30 :start-date "2026-11-01"
                 :current-words 2334
                 :daily-word-counts (list (cons "2026-11-01" (list :words 2334 :note ""))))))
      (org-scribe-planner--calculate-missing-variable plan)
      (cl-letf (((symbol-function 'org-scribe-planner--save-plan) #'ignore)
                ((symbol-function 'org-scribe-planner--refresh-agenda-if-synced) #'ignore)
                ((symbol-function 'org-scribe-planner-show-calendar) #'ignore))
        (org-scribe-planner-recalculate-remaining-days plan "/tmp/unused-plan.org"))
      (let* ((json (org-scribe-companion-progress--plan-json plan))
             (days (plist-get json :days)))
        (should (= 0 (plist-get json :preexisting)))
        (should (= 1667 (plist-get (aref days 0) :planned)))
        (should (= 3978 (plist-get (aref days 1) :planned)))
        (should (equal (plist-get json :rebase)
                       '(:date "2026-11-02" :from 1667 :to 2334)))
        (should (stringp (json-serialize json)))))))

(ert-deftest test-companion-progress-plan-phases ()
  (test-companion-progress--with-project root "demo-novel"
    (test-companion-progress--on "2026-09-01"
      (should (equal "after" (plist-get (plist-get (org-scribe-companion-progress-get root) :plan)
                                        :phase))))
    (test-companion-progress--on "2026-04-01"
      (let ((plan (plist-get (org-scribe-companion-progress-get root) :plan)))
        (should (equal "before" (plist-get plan :phase)))
        (should (eq :null (plist-get plan :todayTarget)))))))

(ert-deftest test-companion-progress-manuscript-matches-the-health-report ()
  (test-companion-progress--with-project root "demo-novel"
    (let ((totals (org-scribe--health-word-totals (expand-file-name "novel.org" root)))
          (manuscript (plist-get (org-scribe-companion-progress-get root) :manuscript)))
      (should (= (car totals) (plist-get manuscript :words)))
      (should (= (cdr totals) (plist-get manuscript :objective))))))

(ert-deftest test-companion-progress-no-plan-states ()
  (test-companion-progress--with-project root "demo-novel"
    (test-companion-progress--set-gate root "no")
    (should (equal "off" (plist-get (plist-get (org-scribe-companion-progress-get root) :plan)
                                    :state)))
    (test-companion-progress--set-gate root nil)
    (should (equal "none" (plist-get (plist-get (org-scribe-companion-progress-get root) :plan)
                                     :state)))
    ;; opted in, but no plan file anywhere
    (test-companion-progress--set-gate root "yes")
    (delete-file (expand-file-name "plan.org" root))
    (should (equal "none" (plist-get (plist-get (org-scribe-companion-progress-get root) :plan)
                                     :state)))))

(ert-deftest test-companion-progress-never-loads-the-planner-on-its-own ()
  "In a fresh session, no plan means no planner; a plan loads it."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (base (file-name-as-directory (make-temp-file "test-companion-progress-fresh-" t)))
         (declined (expand-file-name "declined/" base))
         (planless (expand-file-name "planless/" base))
         (planned (expand-file-name "planned/" base)))
    (unwind-protect
        (progn
          (dolist (dir (list declined planless planned))
            (copy-directory (expand-file-name "examples/demo-novel" test-companion-progress--repo)
                            dir nil t t))
          (test-companion-progress--set-gate declined "no")
          (delete-file (expand-file-name "plan.org" planless))
          (let* ((form (format "(progn
  (setq org-id-locations-file %S make-backup-files nil create-lockfiles nil)
  (load %S nil t)
  (require 'org-scribe-companion)
  (let (out)
    (dolist (root (list %S %S))
      (push (list (plist-get (plist-get (org-scribe-companion-progress-get root) :plan) :state)
                  (featurep 'org-scribe-planner))
            out))
    (push (list (plist-get (plist-get (org-scribe-companion-progress-get %S) :plan) :state)
                (featurep 'org-scribe-planner))
          out)
    (princ (format \"RESULT:%%S\" (nreverse out)))))"
                               (expand-file-name "ids" base)
                               (expand-file-name "test-load.el" test-companion-progress--repo)
                               declined planless planned))
                 (output (with-temp-buffer
                           (call-process emacs nil t nil "-Q" "--batch" "--eval" form)
                           (buffer-string)))
                 (result (and (string-match "RESULT:\\(.*\\)\\'" output)
                              (car (read-from-string (match-string 1 output))))))
            (should (equal '(("off" nil) ("none" nil) ("active" t)) result))))
      (delete-directory base t))))

(provide 'test-companion-progress)

;;; test-companion-progress.el ends here
