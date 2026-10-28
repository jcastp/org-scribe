;;; test-companion-gate.el --- Tests for the visual companion's switches -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for the companion's two switches (Phase 0): the global
;; `org-scribe-companion-enable', the per-project gate
;; `org-scribe-companion-gate', and `org-scribe-companion-open''s checks.
;; The "module stays unloaded" check runs in a fresh `emacs -Q' subprocess,
;; because the test runner may already have loaded the module.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path (expand-file-name "core" parent-dir))
  (add-to-list 'load-path (expand-file-name "lang" parent-dir))
  (add-to-list 'load-path (expand-file-name "linking" parent-dir))
  (add-to-list 'load-path (expand-file-name "search" parent-dir))
  (add-to-list 'load-path (expand-file-name "companion" parent-dir)))

(require 'org-scribe-config)
(require 'org-scribe-core)
(require 'org-scribe-companion)

(defconst test-companion-gate--repo
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the org-scribe checkout, for the subprocess's load path.")

(defmacro test-companion-gate--with-project (type root-var &rest body)
  "Run BODY in a temp project of TYPE, with ROOT-VAR bound to its root.
`default-directory' is the project root; the companion switch is on, and
`org-scribe-companion--open' is stubbed so that no server starts."
  (declare (indent 2))
  `(let* ((,root-var (file-name-as-directory (make-temp-file "test-companion-gate-" t)))
          (default-directory ,root-var)
          (org-scribe-companion-enable t))
     (unwind-protect
         (cl-letf (((symbol-function 'org-scribe-companion--open) #'ignore))
           (with-temp-file (expand-file-name ".org-scribe-project" ,root-var)
             (insert "# Writing project: Gate Test\n# Type: " ,type "\n"))
           (org-scribe-project-type-cache-clear)
           ,@body)
       (org-scribe-project-type-cache-clear)
       (delete-directory ,root-var t))))

;;; Gate read and write

(ert-deftest test-companion-gate-nil-when-undecided ()
  (test-companion-gate--with-project "novel" root
    (should (null (org-scribe-companion-gate root)))))

(ert-deftest test-companion-gate-enable-and-disable-for-project ()
  (test-companion-gate--with-project "novel" root
    (org-scribe-companion-enable-for-project)
    (should (eq 'yes (org-scribe-companion-gate root)))
    (org-scribe-companion-disable-for-project)
    (should (eq 'no (org-scribe-companion-gate root)))
    (should (null (org-scribe-planner-gate root)))))

;;; org-scribe-companion-open

(ert-deftest test-companion-gate-switch-off-is-a-user-error ()
  (test-companion-gate--with-project "novel" root
    (let ((org-scribe-companion-enable nil)
          (asked nil))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) (setq asked t) t)))
        (should-error (org-scribe-companion-open) :type 'user-error))
      (should-not asked)
      (should (null (org-scribe-companion-gate root))))))

(ert-deftest test-companion-gate-short-story-asks-like-a-novel ()
  (test-companion-gate--with-project "short-story" root
    (let ((asked 0))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) (cl-incf asked) t)))
        (org-scribe-companion-open)
        (should (eq 'yes (org-scribe-companion-gate root)))
        (should (= 1 asked))))))

(ert-deftest test-companion-gate-undecided-asks-once-and-records-yes ()
  (test-companion-gate--with-project "novel" root
    (let ((asked 0))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) (cl-incf asked) t)))
        (org-scribe-companion-open)
        (should (eq 'yes (org-scribe-companion-gate root)))
        (org-scribe-companion-open)
        (should (= 1 asked))))))

(ert-deftest test-companion-gate-undecided-records-no-when-declined ()
  (test-companion-gate--with-project "novel" root
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (org-scribe-companion-open)
      (should (eq 'no (org-scribe-companion-gate root))))))

(ert-deftest test-companion-gate-declined-project-offers-to-enable ()
  (test-companion-gate--with-project "novel" root
    (org-scribe--project-marker-set root "Companion" "no")
    (let ((prompt nil))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (p) (setq prompt p) t)))
        (org-scribe-companion-open))
      (should (equal prompt (org-scribe-msg 'companion-ask-reenable)))
      (should (eq 'yes (org-scribe-companion-gate root))))))

;;; Messages exist in both languages

(ert-deftest test-companion-gate-messages-in-both-languages ()
  (dolist (key '(companion-error-disabled
                 companion-ask-enable companion-ask-reenable
                 companion-staying-disabled companion-enabled-for-project
                 companion-disabled-for-project companion-opened companion-stopped
                 companion-not-running))
    (should (alist-get key org-scribe-messages-en))
    (should (alist-get key org-scribe-messages-es))))

;;; The module stays unloaded while the switch is off

(ert-deftest test-companion-gate-module-not-loaded-while-switch-off ()
  "In a fresh session, the switch off leaves the module unloaded.
Turning the switch on and answering the gate then loads it."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (base (make-temp-file "test-companion-load-" t))
         (form (format "(let ((repo %S) (base %S))
  (setq org-id-locations-file (expand-file-name \"ids\" base)
        make-backup-files nil create-lockfiles nil)
  (load (expand-file-name \"test-load.el\" repo) nil t)
  (with-temp-file (expand-file-name \".org-scribe-project\" base)
    (insert \"# Writing project: Probe\\n# Type: novel\\n\"))
  (with-temp-file (expand-file-name \"novel.org\" base)
    (insert \"* Act\\n:PROPERTIES:\\n:ID: probe-act\\n:END:\\n\"))
  (let ((default-directory (file-name-as-directory base)) off-error on)
    (setq off-error (condition-case e (org-scribe-companion-open) (user-error (cadr e))))
    (let ((before (featurep 'org-scribe-companion)))
      (fset 'yes-or-no-p (lambda (_) t))
      (setq org-scribe-companion-enable t
            org-scribe-companion-port 0
            org-scribe-companion-browser-function #'ignore)
      (org-scribe-companion-open)
      (org-scribe-companion-stop)
      (princ (format \"RESULT:%%S\"
                     (list :error off-error :before before
                           :after (featurep 'org-scribe-companion)
                           :gate (org-scribe-companion-gate base)))))))"
                       test-companion-gate--repo base)))
    (unwind-protect
        (let* ((output (with-temp-buffer
                         (call-process emacs nil t nil "-Q" "--batch" "--eval" form)
                         (buffer-string)))
               (result (and (string-match "RESULT:\\(.*\\)\\'" output)
                            (car (read-from-string (match-string 1 output))))))
          (should result)
          (should (stringp (plist-get result :error)))
          (should-not (plist-get result :before))
          (should (plist-get result :after))
          (should (eq 'yes (plist-get result :gate))))
      (delete-directory base t))))

(provide 'test-companion-gate)
;;; test-companion-gate.el ends here

(ert-deftest test-companion-gate-setup-check-reports-the-companion ()
  (test-companion-gate--with-project "novel" root
    (let ((org-scribe-companion-enable nil))
      (org-scribe-setup-check)
      (with-current-buffer "*org-scribe setup*"
        (let ((text (buffer-string)))
          (should (string-match-p "^\\* Visual companion" text))
          (should (string-match-p "\\[ \\] =org-scribe-companion-enable=" text))
          (should (string-match-p "this project" text))
          (should (string-match-p "\\[ \\] server" text))))
      (kill-buffer "*org-scribe setup*"))))
