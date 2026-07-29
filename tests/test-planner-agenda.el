;;; test-planner-agenda.el --- Tests for org-agenda sync -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `org-scribe-planner--add-agenda-entries' and interaction with
;; `org-scribe-planner--save-plan' (H6): saving a plan must not destroy a
;; previously synced "** Agenda Entries" section, and re-syncing must not
;; duplicate entries.

;;; Code:

(require 'ert)
(require 'org-scribe-planner)

(defmacro test-agenda--with-plan-file (plan-var file-var &rest body)
  "Run BODY with PLAN-VAR / FILE-VAR bound to a small saved plan.
Cleans up the temp file unconditionally."
  (declare (indent 2))
  `(let* ((,file-var (make-temp-file "test-planner-agenda-" nil ".org"))
          (,plan-var (make-org-scribe-plan
                      :title "Agenda Test Plan"
                      :total-words 3000
                      :daily-words 1000
                      :days 3
                      :start-date "2026-02-01"
                      :end-date "2026-02-03"
                      :current-words 0)))
     (unwind-protect
         (progn
           (org-scribe-planner--save-plan ,plan-var ,file-var)
           ,@body)
       (ignore-errors (delete-file ,file-var)))))

(ert-deftest test-planner-agenda-survives-plan-save ()
  "Saving a plan preserves a previously synced Agenda Entries section."
  (test-agenda--with-plan-file plan file
    (let ((org-scribe-planner-sync-to-agenda t))
      (org-scribe-planner--add-agenda-entries plan file)
      ;; Now re-save the plan (as every word-count sync does)
      (org-scribe-planner--save-plan plan file)
      (with-temp-buffer
        (insert-file-contents file)
        (should (string-match-p "\\*\\* Agenda Entries" (buffer-string)))
        (should (string-match-p "TODO Write 1000 words" (buffer-string)))))))

(ert-deftest test-planner-agenda-sync-is-idempotent ()
  "Calling add-agenda-entries twice does not duplicate TODOs."
  (test-agenda--with-plan-file plan file
    (let ((org-scribe-planner-sync-to-agenda t))
      (org-scribe-planner--add-agenda-entries plan file)
      (org-scribe-planner--add-agenda-entries plan file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((count 0))
          (while (re-search-forward "TODO Write 1000 words" nil t)
            (setq count (1+ count)))
          (should (= count 3)))))))

(ert-deftest test-planner-agenda-file-list-string-does-not-error ()
  "org-agenda-files as a string (file-of-files) does not signal an error."
  (test-agenda--with-plan-file plan file
    (let ((org-agenda-files "/tmp/does-not-matter-agenda-files.txt"))
      (should-not (condition-case err
                      (progn
                        (org-scribe-planner--update-agenda-file-list file)
                        nil)
                    (error err))))))

(provide 'test-planner-agenda)

;;; test-planner-agenda.el ends here
