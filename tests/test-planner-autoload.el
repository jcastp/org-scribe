;;; test-planner-autoload.el --- The planner loads itself for opted-in projects -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for `org-scribe--planner-maybe-autoload' (org-scribe.el): turning
;; `org-scribe-mode' on in a "# Planner: yes" project loads the planner and
;; activates the project's plan, so the very first word count of a session
;; reaches the ledger.
;;
;; The bug this guards against only exists while the planner is *not*
;; loaded — every word count silently skipped the ledger until the writer
;; happened to open a planner view.  The test runner loads the planner long
;; before these tests run, so an in-process test could never see it.  The
;; real check therefore runs in a fresh `emacs -Q' subprocess, which starts
;; exactly where a writer's session does: org-scribe loaded, planner only
;; autoloaded.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path (expand-file-name "core" parent-dir))
  (add-to-list 'load-path (expand-file-name "planning" parent-dir)))

(require 'org-scribe-planner)

(defconst test-planner-autoload--repo
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the org-scribe checkout, for the subprocess's load path.")

(defun test-planner-autoload--make-project (base name gate &optional plan)
  "Create project NAME under BASE with planner GATE (\"yes\", \"no\" or nil).
Writes a one-scene manuscript.  When PLAN is non-nil, also saves a plan
to plan.org.  Returns the manuscript file name."
  (let ((root (expand-file-name name base)))
    (make-directory root t)
    (with-temp-file (expand-file-name ".org-scribe-project" root)
      (insert (format "# Writing project: %s\n# Type: novel\n# Language: en\n" name))
      (when gate (insert (format "# Planner: %s\n" gate))))
    (make-directory (expand-file-name "objects" root) t)
    (with-temp-file (expand-file-name "novel.org" root)
      (insert "#+TITLE: " name "\n#+MACRO: scene-break SCENE-BREAK\n"
              "* Act I :ignore:\n** TODO Chapter 1 :ignore:\n"
              "*** TODO Scene 1 :ignore:\n:PROPERTIES:\n:PoV:\n:END:\n\n"
              "Mara found the letter under the door.\n"))
    (when plan
      (org-scribe-planner--save-plan plan (expand-file-name "plan.org" root)))
    (expand-file-name "novel.org" root)))

(defconst test-planner-autoload--script
  "(progn
  (setq org-id-locations-file (expand-file-name \"ids\" base)
        make-backup-files nil create-lockfiles nil)
  (load (expand-file-name \"test-load.el\" repo) nil t)
  (let (out)
    (dolist (f (list plain undecided declined))
      (with-current-buffer (find-file-noselect f)
        (org-scribe-mode 1)
        (push (cons (file-name-nondirectory (directory-file-name (file-name-directory f)))
                    (featurep 'org-scribe-planner))
              out)))
    (with-current-buffer (find-file-noselect opted-in)
      (org-scribe-mode 1)
      (push (cons 'loaded (featurep 'org-scribe-planner)) out)
      (push (cons 'plan (and (boundp 'org-scribe-planner--current-plan)
                             org-scribe-planner--current-plan
                             (org-scribe-plan-title org-scribe-planner--current-plan)))
            out)
      (goto-char (point-max))
      (insert \"The water had reached the third step by then.\\n\")
      (save-buffer)
      (org-scribe-ews-org-count-words)
      (push (cons 'ledger (and org-scribe-planner--current-plan
                               (org-scribe-plan-daily-word-counts
                                org-scribe-planner--current-plan)
                               t))
            out))
    (princ (format \"RESULT:%S\" out))))"
  "Form run in the subprocess.
Expects REPO, BASE, PLAIN, UNDECIDED, DECLINED and OPTED-IN to be bound.
Prints an alist after \"RESULT:\".")

(ert-deftest test-planner-autoload-loads-only-for-opted-in-projects ()
  "A fresh session loads the planner for a gate-yes project, and only then.
Undecided, declined and non-project buffers must leave it unloaded; the
gate-yes project must get its plan activated and its first word count
recorded in the ledger."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (base (make-temp-file "test-planner-autoload-" t))
         (plan (make-org-scribe-plan
                :title "Autoload Test Plan" :total-words 10000 :daily-words 500
                :days 20 :start-date (format-time-string "%Y-%m-%d")
                :end-date (format-time-string
                           "%Y-%m-%d" (time-add nil (* 19 24 3600)))
                :current-words 0)))
    (unwind-protect
        (let* ((plain (expand-file-name "loose/notes.org" base))
               (undecided (test-planner-autoload--make-project base "undecided" nil))
               (declined (test-planner-autoload--make-project base "declined" "no"))
               (opted-in (test-planner-autoload--make-project base "opted-in" "yes" plan))
               (form (format "(let ((repo %S) (base %S) (plain %S) (undecided %S) (declined %S) (opted-in %S)) %s)"
                             test-planner-autoload--repo base plain undecided
                             declined opted-in test-planner-autoload--script))
               (output (progn
                         (make-directory (file-name-directory plain) t)
                         (with-temp-file plain (insert "* Shopping\n"))
                         (with-temp-buffer
                           (call-process emacs nil t nil "-Q" "--batch" "--eval" form)
                           (buffer-string))))
               (result (and (string-match "RESULT:\\(.*\\)\\'" output)
                            (car (read-from-string (match-string 1 output))))))
          (should result)
          (should-not (alist-get "loose" result nil nil #'equal))
          (should-not (alist-get "undecided" result nil nil #'equal))
          (should-not (alist-get "declined" result nil nil #'equal))
          (should (alist-get 'loaded result))
          (should (equal "Autoload Test Plan" (alist-get 'plan result)))
          (should (alist-get 'ledger result)))
      (delete-directory base t))))

(ert-deftest test-planner-autoload-offers-a-plan-on-interactive-create ()
  "In a fresh session, creating a project interactively offers a plan.
The offer was advice the planner installed when it loaded, so before any
planner command nothing was offered; and a programmatic call, which used
to prompt once the planner was loaded, must never prompt."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (base (make-temp-file "test-planner-offer-" t))
         (form (format "(let ((repo %S) (base %S))
  (setq org-id-locations-file (expand-file-name \"ids\" base)
        make-backup-files nil create-lockfiles nil)
  (load (expand-file-name \"test-load.el\" repo) nil t)
  (let ((asked nil))
    (fset 'yes-or-no-p (lambda (p) (push p asked) nil))
    (org-scribe-create-short-story-project base \"Quiet\" 'en)
    (let ((quiet asked))
      (funcall-interactively #'org-scribe-create-short-story-project base \"Asked\" 'en)
      (princ (format \"RESULT:%%S\"
                     (list :quiet quiet
                           :asked (length asked)
                           :gate (org-scribe-planner-gate (expand-file-name \"Asked\" base))))))))"
                       test-planner-autoload--repo base)))
    (unwind-protect
        (let* ((output (with-temp-buffer
                         (call-process emacs nil t nil "-Q" "--batch" "--eval" form)
                         (buffer-string)))
               (result (and (string-match "RESULT:\\(.*\\)\\'" output)
                            (car (read-from-string (match-string 1 output))))))
          (should result)
          (should-not (plist-get result :quiet))
          (should (= 1 (plist-get result :asked)))
          (should (eq 'no (plist-get result :gate))))
      (delete-directory base t))))

(provide 'test-planner-autoload)
;;; test-planner-autoload.el ends here
