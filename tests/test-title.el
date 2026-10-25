;;; test-title.el --- Tests for changing the project title -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for `org-scribe-set-title' (templates/org-scribe-project.el) and
;; the `org-scribe-project-title' accessor (core/org-scribe-core.el).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (dolist (dir '("core" "templates" "reporting" "planning" "capture"))
    (add-to-list 'load-path (expand-file-name dir parent-dir))))

(require 'org-scribe-core)
(require 'org-scribe-config)
(require 'org-scribe-project)
(require 'org-scribe-planner)

;;; Fixtures

(defmacro test-title--with-project (spec &rest body)
  "Create a project and run BODY with `default-directory' inside it.
SPEC is (ROOT-VAR TITLE KIND LANGUAGE), KIND being `novel' or `short'."
  (declare (indent 1))
  (let ((base (make-symbol "base")))
    `(let* ((,base (make-temp-file "test-title-" t))
            (,(nth 0 spec) (expand-file-name ,(nth 1 spec) ,base)))
       (unwind-protect
           (progn
             (cl-letf (((symbol-function 'message) #'ignore))
               (if (eq ,(nth 2 spec) 'novel)
                   (org-scribe-create-novel-project ,base ,(nth 1 spec) ,(nth 3 spec))
                 (org-scribe-create-short-story-project ,base ,(nth 1 spec) ,(nth 3 spec))))
             (let ((default-directory (file-name-as-directory ,(nth 0 spec))))
               ,@body))
         (dolist (buf (buffer-list))
           (when-let* ((file (buffer-file-name buf)))
             (when (string-prefix-p ,base file)
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf))))
         (delete-directory ,base t)))))

(defun test-title--set (title)
  "Run `org-scribe-set-title' to TITLE, confirmed, silently."
  (cl-letf (((symbol-function 'message) #'ignore))
    (org-scribe-set-title title t)))

(defun test-title--titles (root)
  "Return an alist of (RELATIVE-FILE . TITLE-VALUE) for ROOT's Org files."
  (mapcar (lambda (f)
            (cons (file-relative-name f root)
                  (org-scribe--file-title-keyword f)))
          (seq-remove (lambda (f) (string-prefix-p (expand-file-name "export" root) f))
                      (mapcar (lambda (r) (expand-file-name r root))
                              (org-scribe--project-file-candidates root)))))

(defun test-title--file (root name)
  "Return the contents of ROOT's NAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name root))
    (buffer-string)))

;;; Accessor

(ert-deftest test-title-accessor-marker-wins ()
  (test-title--with-project (root "Old" 'novel 'en)
    (org-scribe--project-marker-set root "Title" "Marker")
    (should (equal (org-scribe-project-title root) "Marker"))))

(ert-deftest test-title-accessor-creation-records-both-lines ()
  (test-title--with-project (root "Old" 'novel 'en)
    (should (equal (org-scribe--project-marker-get root "Title") "Old"))
    (should (equal (org-scribe--project-marker-get root "Writing project") "Old"))))

(ert-deftest test-title-accessor-falls-back-to-manuscript ()
  (let ((root (make-temp-file "test-title-" t)))
    (unwind-protect
        (progn
          (write-region "# Type: novel\n" nil (expand-file-name ".org-scribe-project" root))
          (write-region "#+TITLE: From Manuscript\n" nil (expand-file-name "novel.org" root))
          (should (equal (org-scribe-project-title root) "From Manuscript")))
      (delete-directory root t))))

(ert-deftest test-title-accessor-falls-back-to-writing-project ()
  (let ((root (make-temp-file "test-title-" t)))
    (unwind-protect
        (progn
          (write-region "# Writing project: At Creation\n" nil
                        (expand-file-name ".org-scribe-project" root))
          (should (equal (org-scribe-project-title root) "At Creation")))
      (delete-directory root t))))

(ert-deftest test-title-accessor-falls-back-to-directory-name ()
  (let ((root (make-temp-file "test-title-folder-" t)))
    (unwind-protect
        (should (equal (org-scribe-project-title root)
                       (file-name-nondirectory root)))
      (delete-directory root t))))

;;; Validation

(ert-deftest test-title-validator-allows-what-a-directory-name-cannot ()
  (dolist (title '("Why Not?" "Mother's Hands" "Part I: Arrival" "A/B" "Dot."))
    (should-not (org-scribe--validate-title-text title))))

(ert-deftest test-title-validator-rejects-empty-and-newline ()
  (should (stringp (org-scribe--validate-title-text "  ")))
  (should (stringp (org-scribe--validate-title-text "Two\nLines"))))

;;; Rewrite rule

(ert-deftest test-title-rewrite-value-rules ()
  (should (equal (org-scribe--title-rewrite-value "Dune" "Dune" "Arrakis") "Arrakis"))
  (should (equal (org-scribe--title-rewrite-value "Dune - Design" "Dune" "Arrakis")
                 "Arrakis - Design"))
  (should (equal (org-scribe--title-rewrite-value "Dune - Design (The Matrix)" "Dune" "X")
                 "X - Design (The Matrix)"))
  (should-not (org-scribe--title-rewrite-value "Dunes - Design" "Dune" "Arrakis"))
  (should-not (org-scribe--title-rewrite-value "Dune: Design" "Dune" "Arrakis"))
  (should-not (org-scribe--title-rewrite-value "Other" "Dune" "Arrakis")))

;;; The command

(ert-deftest test-title-set-rewrites-every-title-and-keeps-suffixes ()
  (dolist (case '((novel en) (novel es) (short en) (short es)))
    (test-title--with-project (root "Old Name" (car case) (cadr case))
      (let ((before (test-title--titles root)))
        (test-title--set "New Name")
        (let ((after (test-title--titles root)))
          (should (equal (mapcar #'car before) (mapcar #'car after)))
          (cl-loop for (file . old) in before
                   for new = (cdr (assoc file after))
                   do (when old
                        (should (string-prefix-p "New Name" new))
                        (should (equal (substring old (length "Old Name"))
                                       (substring new (length "New Name"))))))
          (should (equal (org-scribe-project-title root) "New Name")))))))

(ert-deftest test-title-set-leaves-writing-project-and-folder-alone ()
  (test-title--with-project (root "Old Name" 'novel 'en)
    (test-title--set "New Name")
    (should (equal (org-scribe--project-marker-get root "Writing project") "Old Name"))
    (should (equal (org-scribe--project-marker-get root "Title") "New Name"))
    (should (file-directory-p root))
    (should (equal (file-name-nondirectory root) "Old Name"))))

(ert-deftest test-title-set-keeps-other-marker-lines ()
  (test-title--with-project (root "Old Name" 'novel 'en)
    (let ((lines (lambda ()
                   (seq-remove (lambda (l) (string-prefix-p "# Title:" l))
                               (split-string (test-title--file root ".org-scribe-project") "\n")))))
      (let ((before (funcall lines)))
        (test-title--set "New Name")
        (should (equal before (funcall lines)))))))

(ert-deftest test-title-set-prefix-collision-is-left-alone ()
  (test-title--with-project (root "Dune" 'novel 'en)
    (let ((file (expand-file-name "notes/notes.org" root)))
      (with-temp-file file (insert "#+TITLE: Dunes - Notes\n"))
      (test-title--set "Arrakis")
      (should (equal (org-scribe--file-title-keyword file) "Dunes - Notes")))))

(ert-deftest test-title-set-hand-edited-title-is-left-alone-and-reported ()
  (test-title--with-project (root "Old Name" 'novel 'en)
    (let ((file (expand-file-name "notes/notes.org" root))
          (messages nil))
      (with-temp-file file (insert "#+TITLE: My own heading\n"))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
        (org-scribe-set-title "New Name" t))
      (should (equal (org-scribe--file-title-keyword file) "My own heading"))
      (should (seq-some (lambda (m) (string-match-p "notes/notes.org" m)) messages)))))

(ert-deftest test-title-set-handles-regexp-characters-and-apostrophes ()
  (test-title--with-project (root "Old (v.2)" 'novel 'en)
    (test-title--set "Mother's [Hands]? *")
    (dolist (entry (test-title--titles root))
      (when (cdr entry)
        (should (string-prefix-p "Mother's [Hands]? *" (cdr entry)))))
    (test-title--set "Back")
    (dolist (entry (test-title--titles root))
      (when (cdr entry)
        (should (string-prefix-p "Back" (cdr entry)))))))

(ert-deftest test-title-set-unsaved-buffer-stays-modified ()
  (test-title--with-project (root "Old Name" 'novel 'en)
    (let* ((file (expand-file-name "README.org" root))
           (buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "\nunsaved\n"))
      (test-title--set "New Name")
      (with-current-buffer buffer
        (should (buffer-modified-p))
        (should (equal (org-scribe--buffer-title-keyword) "New Name - Project Overview")))
      (should (equal (org-scribe--file-title-keyword file) "New Name - Project Overview"))
      (should-not (string-match-p "New Name" (let ((s (with-temp-buffer
                                                         (insert-file-contents file)
                                                         (buffer-string))))
                                               (car (split-string s "\n"))))))))

(ert-deftest test-title-set-refuses-empty-and-newline-and-unchanged ()
  (test-title--with-project (root "Old Name" 'novel 'en)
    (should-error (test-title--set "  ") :type 'user-error)
    (should-error (test-title--set "A\nB") :type 'user-error)
    (should-error (test-title--set "Old Name") :type 'user-error)))

(ert-deftest test-title-set-declined-preview-changes-nothing ()
  (test-title--with-project (root "Old Name" 'novel 'en)
    (let ((before (test-title--titles root)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                ((symbol-function 'message) #'ignore))
        (should-error (org-scribe-set-title "New Name") :type 'user-error))
      (should (equal before (test-title--titles root)))
      (should (equal (org-scribe--project-marker-get root "Title") "Old Name")))))

(ert-deftest test-title-set-refuses-outside-a-project ()
  (let ((default-directory (make-temp-file "test-title-none-" t)))
    (unwind-protect
        (should-error (org-scribe-set-title "X" t) :type 'user-error)
      (delete-directory default-directory t))))

(ert-deftest test-title-set-old-project-without-title-marker ()
  "A project made before `# Title:' existed is renamed from its manuscript."
  (test-title--with-project (root "Old Name" 'novel 'en)
    (let ((marker (expand-file-name ".org-scribe-project" root)))
      (write-region (replace-regexp-in-string
                     "^# Title: .*\n" "" (test-title--file root ".org-scribe-project"))
                    nil marker)
      (should (equal (org-scribe-project-title root) "Old Name"))
      (test-title--set "New Name")
      (should (equal (org-scribe--project-marker-get root "Title") "New Name"))
      (should (equal (org-scribe--file-title-keyword (expand-file-name "novel.org" root))
                     "New Name")))))

(ert-deftest test-title-set-compiled-title-follows ()
  (test-title--with-project (root "Old Name" 'novel 'en)
    (test-title--set "New Name")
    (let ((keywords (with-temp-buffer
                      (insert-file-contents (expand-file-name "novel.org" root))
                      (org-mode)
                      (org-collect-keywords '("TITLE")))))
      (should (equal (cadr (assoc "TITLE" keywords)) "New Name")))))

;;; The plan

(ert-deftest test-title-set-renames-an-unloaded-plan-heading ()
  (test-title--with-project (root "Old Name" 'novel 'en)
    (let ((plan (expand-file-name "plan.org" root)))
      (with-temp-file plan (insert "* Old Name\n:PROPERTIES:\n:TOTAL_WORDS: 100\n:END:\n"))
      (org-scribe--project-marker-set root "Planner" "yes")
      (cl-letf (((symbol-function 'featurep)
                 (let ((orig (symbol-function 'featurep)))
                   (lambda (f &rest r) (and (not (eq f 'org-scribe-planner))
                                            (apply orig f r))))))
        (test-title--set "New Name"))
      (should (string-prefix-p "* New Name\n:PROPERTIES:" (test-title--file root "plan.org"))))))

(ert-deftest test-title-set-leaves-a-differently-named-plan-alone ()
  (test-title--with-project (root "Old Name" 'novel 'en)
    (let ((plan (expand-file-name "plan.org" root)))
      (with-temp-file plan (insert "* My plan\n"))
      (test-title--set "New Name")
      (should (equal (test-title--file root "plan.org") "* My plan\n")))))

(ert-deftest test-title-set-leaves-plan-alone-when-gate-is-no ()
  (test-title--with-project (root "Old Name" 'novel 'en)
    (let ((plan (expand-file-name "plan.org" root)))
      (with-temp-file plan (insert "* Old Name\n"))
      (org-scribe--project-marker-set root "Planner" "no")
      (test-title--set "New Name")
      (should (equal (test-title--file root "plan.org") "* Old Name\n")))))

(ert-deftest test-title-set-offers-the-loaded-plan-through-the-planner ()
  (test-title--with-project (root "Old Name" 'novel 'en)
    (let* ((plan-file (expand-file-name "plan.org" root))
           (org-scribe-planner--current-plan (make-org-scribe-plan :title "Old Name"))
           (org-scribe-planner--current-plan-file plan-file)
           (renamed nil))
      (with-temp-file plan-file (insert "* Old Name\n"))
      (org-scribe--project-marker-set root "Planner" "yes")
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'org-scribe-planner-set-title)
                 (lambda (title) (setq renamed title))))
        (test-title--set "New Name"))
      (should (equal renamed "New Name"))
      ;; The file is the planner's to write; the command did not touch it.
      (should (equal (test-title--file root "plan.org") "* Old Name\n")))))

(ert-deftest test-title-planner-set-title-renames-and-saves ()
  (test-title--with-project (root "Old Name" 'novel 'en)
    (let* ((plan-file (expand-file-name "plan.org" root))
           (plan (make-org-scribe-plan :title "Old Name" :total-words 1000
                                       :start-date "2026-01-01" :days 10
                                       :daily-words 100 :end-date "2026-01-10"
                                       :writing-days 10)))
      (org-scribe-planner--save-plan plan plan-file)
      (cl-letf (((symbol-function 'org-scribe-planner--get-current-plan)
                 (lambda (&rest _) (cons plan plan-file)))
                ((symbol-function 'org-scribe-planner--lock-past-goals) #'ignore)
                ((symbol-function 'message) #'ignore))
        (should (org-scribe-planner-set-title "New Name")))
      (should (equal (org-scribe-plan-title plan) "New Name"))
      (should (string-prefix-p "* New Name\n" (test-title--file root "plan.org"))))))

(ert-deftest test-title-planner-discovery-finds-plan-named-at-creation ()
  (test-title--with-project (root "Old Name" 'novel 'en)
    (let ((legacy (expand-file-name "old-name-plan.org" root)))
      (with-temp-file legacy (insert "* Old Name\n"))
      (test-title--set "New Name")
      (should (equal (file-truename (org-scribe-planner--find-plan-file root))
                     (file-truename legacy))))))

(ert-deftest test-title-planner-project-title-prefers-title-marker ()
  (test-title--with-project (root "Old Name" 'novel 'en)
    (test-title--set "New Name")
    (should (equal (org-scribe-planner--project-title root) "New Name"))))

(provide 'test-title)
;;; test-title.el ends here
