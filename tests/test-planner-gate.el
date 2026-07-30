;;; test-planner-gate.el --- Tests for the per-project planner gate -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for the per-project planner opt-in gate (Phase 4 of the
;; hygiene/slimming roadmap): the marker-file accessors in
;; core/org-scribe-core.el (`org-scribe--project-marker-get',
;; `org-scribe--project-marker-set', `org-scribe-planner-gate'), the
;; gate-writing/checking behavior in planning/org-scribe-planner.el, and
;; the health report's gate check.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add module directories to load path
(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path (expand-file-name "core" parent-dir))
  (add-to-list 'load-path (expand-file-name "templates" parent-dir))
  (add-to-list 'load-path (expand-file-name "counting" parent-dir))
  (add-to-list 'load-path (expand-file-name "linking" parent-dir))
  (add-to-list 'load-path (expand-file-name "reporting" parent-dir))
  (add-to-list 'load-path (expand-file-name "planning" parent-dir)))

(require 'org-scribe-core)
(require 'org-scribe-health)
(require 'org-scribe-planner)

;;; Fixture

(defmacro test-planner-gate--with-project (root-var &rest body)
  "Bind ROOT-VAR to a fresh temp project dir with a marker file, run BODY."
  (declare (indent 1))
  `(let ((,root-var (make-temp-file "test-planner-gate-" t)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".org-scribe-project" ,root-var)
             (insert "# Writing project: Gate Test Novel\n# Type: novel\n"))
           ,@body)
       (delete-directory ,root-var t))))

;;; org-scribe--project-marker-get / -set round trip

(ert-deftest test-planner-gate-marker-set-then-get-round-trip ()
  "Setting a key and reading it back returns the same value."
  (test-planner-gate--with-project root
    (org-scribe--project-marker-set root "Planner" "yes")
    (should (string= "yes" (org-scribe--project-marker-get root "Planner")))))

(ert-deftest test-planner-gate-marker-get-nil-when-absent ()
  "Reading a key never written returns nil."
  (test-planner-gate--with-project root
    (should (null (org-scribe--project-marker-get root "Planner")))))

(ert-deftest test-planner-gate-marker-get-nil-without-marker-file ()
  "Reading from a directory with no marker file at all returns nil."
  (let ((root (make-temp-file "test-planner-gate-nomarker-" t)))
    (unwind-protect
        (should (null (org-scribe--project-marker-get root "Planner")))
      (delete-directory root t))))

(ert-deftest test-planner-gate-marker-set-replaces-existing-line ()
  "Setting a key twice replaces the line rather than duplicating it."
  (test-planner-gate--with-project root
    (org-scribe--project-marker-set root "Planner" "no")
    (org-scribe--project-marker-set root "Planner" "yes")
    (should (string= "yes" (org-scribe--project-marker-get root "Planner")))
    (with-temp-buffer
      (insert-file-contents (expand-file-name ".org-scribe-project" root))
      (should (= 1 (how-many "^# Planner:" (point-min) (point-max)))))))

(ert-deftest test-planner-gate-marker-set-preserves-other-lines ()
  "Setting one key does not disturb other marker-file lines."
  (test-planner-gate--with-project root
    (org-scribe--project-marker-set root "Planner" "yes")
    (should (string= "Gate Test Novel"
                     (org-scribe--project-marker-get root "Writing project")))
    (should (string= "novel" (org-scribe--project-marker-get root "Type")))))

(ert-deftest test-planner-gate-marker-get-case-insensitive-key ()
  "KEY matching against the marker line is case-insensitive."
  (test-planner-gate--with-project root
    (org-scribe--project-marker-set root "Planner" "yes")
    (should (string= "yes" (org-scribe--project-marker-get root "planner")))
    (should (string= "yes" (org-scribe--project-marker-get root "PLANNER")))))

(ert-deftest test-planner-gate-marker-set-noop-without-marker-file ()
  "Setting a key in a directory with no marker file does nothing (no crash)."
  (let ((root (make-temp-file "test-planner-gate-nomarker-set-" t)))
    (unwind-protect
        (progn
          (org-scribe--project-marker-set root "Planner" "yes")
          (should-not (file-exists-p (expand-file-name ".org-scribe-project" root))))
      (delete-directory root t))))

;;; org-scribe-planner-gate

(ert-deftest test-planner-gate-returns-yes ()
  "Gate returns 'yes when the marker says so."
  (test-planner-gate--with-project root
    (org-scribe--project-marker-set root "Planner" "yes")
    (should (eq 'yes (org-scribe-planner-gate root)))))

(ert-deftest test-planner-gate-returns-no ()
  "Gate returns 'no when the marker says so."
  (test-planner-gate--with-project root
    (org-scribe--project-marker-set root "Planner" "no")
    (should (eq 'no (org-scribe-planner-gate root)))))

(ert-deftest test-planner-gate-returns-nil-when-undecided ()
  "Gate returns nil when the marker has no Planner line."
  (test-planner-gate--with-project root
    (should (null (org-scribe-planner-gate root)))))

(ert-deftest test-planner-gate-uses-current-project-root-by-default ()
  "Gate without a ROOT argument resolves via `org-scribe-project-root'."
  (test-planner-gate--with-project root
    (org-scribe--project-marker-set root "Planner" "yes")
    (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () root)))
      (should (eq 'yes (org-scribe-planner-gate))))))

;;; --offer-plan-on-create writes the gate

(ert-deftest test-planner-gate-offer-on-create-accept-writes-yes ()
  "Accepting the offer-to-create-a-plan prompt writes '# Planner: yes'."
  (let* ((base-dir (make-temp-file "test-planner-gate-offer-" t))
         (title "Gated Novel")
         (project-dir (expand-file-name title base-dir)))
    (unwind-protect
        (progn
          (make-directory project-dir t)
          (with-temp-file (expand-file-name ".org-scribe-project" project-dir)
            (insert "# Writing project: Gated Novel\n# Type: novel\n"))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'org-scribe-planner-new-plan) (lambda (&rest _) nil)))
            (org-scribe-planner--offer-plan-on-create base-dir title))
          (should (eq 'yes (org-scribe-planner-gate project-dir))))
      (delete-directory base-dir t))))

(ert-deftest test-planner-gate-offer-on-create-decline-writes-no ()
  "Declining the offer-to-create-a-plan prompt writes '# Planner: no'
and never calls `org-scribe-planner-new-plan'."
  (let* ((base-dir (make-temp-file "test-planner-gate-offer-" t))
         (title "Declined Novel")
         (project-dir (expand-file-name title base-dir))
         (new-plan-called nil))
    (unwind-protect
        (progn
          (make-directory project-dir t)
          (with-temp-file (expand-file-name ".org-scribe-project" project-dir)
            (insert "# Writing project: Declined Novel\n# Type: novel\n"))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                    ((symbol-function 'org-scribe-planner-new-plan)
                     (lambda (&rest _) (setq new-plan-called t))))
            (org-scribe-planner--offer-plan-on-create base-dir title))
          (should-not new-plan-called)
          (should (eq 'no (org-scribe-planner-gate project-dir))))
      (delete-directory base-dir t))))

;;; Auto-load no-ops when gated off

(ert-deftest test-planner-gate-auto-load-noop-when-gate-no ()
  "--auto-load-plan does not load a plan when the project's gate is 'no,
even if a valid plan.org sits right there in the project root."
  (test-planner-gate--with-project root
    (org-scribe--project-marker-set root "Planner" "no")
    (let* ((plan-file (expand-file-name "plan.org" root))
           (org-scribe-planner--current-plan nil)
           (org-scribe-planner--current-plan-file nil)
           (test-planner-gate--faked-org-scribe (not (featurep 'org-scribe))))
      (unwind-protect
          (progn
            (when test-planner-gate--faked-org-scribe (push 'org-scribe features))
            (org-scribe-planner--save-plan
             (make-org-scribe-plan :title "Gated Off" :total-words 1000
                                   :daily-words 100 :days 10
                                   :start-date "2026-01-01" :end-date "2026-01-10"
                                   :current-words 0)
             plan-file)
            (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () root)))
              (org-scribe-planner--auto-load-plan))
            (should-not org-scribe-planner--current-plan))
        (when test-planner-gate--faked-org-scribe
          (setq features (delq 'org-scribe features)))))))

(ert-deftest test-planner-gate-auto-load-noop-when-gate-undecided ()
  "--auto-load-plan does not load a plan when the gate has never been set."
  (test-planner-gate--with-project root
    (let* ((plan-file (expand-file-name "plan.org" root))
           (org-scribe-planner--current-plan nil)
           (org-scribe-planner--current-plan-file nil)
           (test-planner-gate--faked-org-scribe (not (featurep 'org-scribe))))
      (unwind-protect
          (progn
            (when test-planner-gate--faked-org-scribe (push 'org-scribe features))
            (org-scribe-planner--save-plan
             (make-org-scribe-plan :title "Undecided" :total-words 1000
                                   :daily-words 100 :days 10
                                   :start-date "2026-01-01" :end-date "2026-01-10"
                                   :current-words 0)
             plan-file)
            (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () root)))
              (org-scribe-planner--auto-load-plan))
            (should-not org-scribe-planner--current-plan))
        (when test-planner-gate--faked-org-scribe
          (setq features (delq 'org-scribe features)))))))

(ert-deftest test-planner-gate-auto-load-loads-when-gate-yes ()
  "--auto-load-plan loads the plan normally when the gate is 'yes."
  (test-planner-gate--with-project root
    (org-scribe--project-marker-set root "Planner" "yes")
    (let* ((plan-file (expand-file-name "plan.org" root))
           (org-scribe-planner--current-plan nil)
           (org-scribe-planner--current-plan-file nil)
           (org-scribe-planner-after-plan-load-hook nil)
           (test-planner-gate--faked-org-scribe (not (featurep 'org-scribe))))
      (unwind-protect
          (progn
            (when test-planner-gate--faked-org-scribe (push 'org-scribe features))
            (org-scribe-planner--save-plan
             (make-org-scribe-plan :title "Gate Open" :total-words 1000
                                   :daily-words 100 :days 10
                                   :start-date "2026-01-01" :end-date "2026-01-10"
                                   :current-words 0)
             plan-file)
            (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () root)))
              (org-scribe-planner--auto-load-plan))
            (should org-scribe-planner--current-plan)
            (should (string= "Gate Open" (org-scribe-plan-title org-scribe-planner--current-plan))))
        (when test-planner-gate--faked-org-scribe
          (setq features (delq 'org-scribe features)))))))

;;; Enable/disable commands flip the marker line

(ert-deftest test-planner-gate-enable-for-project-writes-yes ()
  "org-scribe-planner-enable-for-project sets '# Planner: yes'."
  (test-planner-gate--with-project root
    (org-scribe--project-marker-set root "Planner" "no")
    (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () root)))
      (org-scribe-planner-enable-for-project))
    (should (eq 'yes (org-scribe-planner-gate root)))))

(ert-deftest test-planner-gate-disable-for-project-writes-no ()
  "org-scribe-planner-disable-for-project sets '# Planner: no'."
  (test-planner-gate--with-project root
    (org-scribe--project-marker-set root "Planner" "yes")
    (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () root)))
      (org-scribe-planner-disable-for-project))
    (should (eq 'no (org-scribe-planner-gate root)))))

(ert-deftest test-planner-gate-enable-errors-outside-project ()
  "org-scribe-planner-enable-for-project errors when not in an org-scribe project."
  (let ((root (make-temp-file "test-planner-gate-noproj-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () root)))
          (should-error (org-scribe-planner-enable-for-project) :type 'user-error))
      (delete-directory root t))))

;;; Health report omits the Writing Plan section when gated off

(ert-deftest test-planner-gate-health-omits-plan-section-when-no ()
  "org-scribe-project-health omits the * Writing Plan section entirely
when the project's gate is 'no."
  (let ((temp-novel (make-temp-file "test-planner-gate-health-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-novel
            (insert "** TODO Chapter :ignore:\n*** TODO Scene :ignore:\n:PROPERTIES:\n:PoV: Alice\n:WORDCOUNT: 100\n:END:\n\n"))
          (cl-letf (((symbol-function 'org-scribe-project-structure)
                     (lambda ()
                       (list :novel-file temp-novel
                             :plan-file nil
                             :characters-file nil
                             :locations-file nil)))
                    ((symbol-function 'org-scribe-planner-gate) (lambda (&rest _) 'no)))
            (org-scribe-project-health)
            (with-current-buffer (get-buffer "*org-scribe-health*")
              (should-not (string-match-p "Writing Plan" (buffer-string))))))
      (delete-file temp-novel))))

(ert-deftest test-planner-gate-health-shows-plan-section-when-undecided ()
  "org-scribe-project-health still shows the section when the gate is nil (undecided)."
  (let ((temp-novel (make-temp-file "test-planner-gate-health-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-novel
            (insert "** TODO Chapter :ignore:\n*** TODO Scene :ignore:\n:PROPERTIES:\n:PoV: Alice\n:WORDCOUNT: 100\n:END:\n\n"))
          (cl-letf (((symbol-function 'org-scribe-project-structure)
                     (lambda ()
                       (list :novel-file temp-novel
                             :plan-file nil
                             :characters-file nil
                             :locations-file nil)))
                    ((symbol-function 'org-scribe-planner-gate) (lambda (&rest _) nil)))
            (org-scribe-project-health)
            (with-current-buffer (get-buffer "*org-scribe-health*")
              (should (string-match-p "Writing Plan" (buffer-string))))))
      (delete-file temp-novel))))

(provide 'test-planner-gate)

;;; test-planner-gate.el ends here
