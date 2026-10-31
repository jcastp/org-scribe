;;; test-companion-collections.el --- Tests for the companion's saved filters -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for `companion/org-scribe-companion-collections.el': the
;; "# Companion-collection:" lines, the snapshot's `collections' entry, the
;; revision part they add, and the `set-collection' command.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (dolist (dir '("core" "lang" "linking" "search" "capture" "templates" "companion"))
    (add-to-list 'load-path (expand-file-name dir parent-dir))))

(require 'org-scribe-config)
(require 'org-scribe-core)
(require 'org-scribe-character-links)
(require 'org-scribe-location-links)
(require 'org-scribe-plot-links)
(require 'org-scribe-plot-point-links)
(require 'org-scribe-companion)

(defconst test-companion-collections--repo
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the org-scribe checkout.")

(defconst test-companion-collections--alex "char-alex-rivera-001")
(defconst test-companion-collections--sam "char-sam-chen-001")
(defconst test-companion-collections--plot "plot-main-line-001")

(defmacro test-companion-collections--with-project (root-var &rest body)
  "Run BODY with a copy of the demo novel at ROOT-VAR (not serving)."
  (declare (indent 1))
  `(let* ((,root-var (file-name-as-directory (make-temp-file "test-companion-coll-" t)))
          (default-directory ,root-var)
          (org-id-locations-file (expand-file-name "ids" ,root-var))
          (make-backup-files nil)
          (create-lockfiles nil)
          (org-scribe-companion-raise-frame nil))
     (unwind-protect
         (save-window-excursion
           (copy-directory (expand-file-name "examples/demo-novel" test-companion-collections--repo)
                           ,root-var t t t)
           (org-scribe-project-type-cache-clear)
           (org-scribe-companion--clear-cache)
           ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (string-prefix-p ,root-var file)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (org-scribe-project-type-cache-clear)
       (org-scribe-companion--clear-cache)
       (delete-directory ,root-var t))))

(defun test-companion-collections--run (root &rest args)
  (org-scribe-companion-commands-run "set-collection" args root))

(defun test-companion-collections--ok-p (answer)
  (eq t (plist-get answer :ok)))

(defun test-companion-collections--code (answer)
  (plist-get (plist-get answer :error) :code))

(defun test-companion-collections--lines (root)
  (org-scribe--project-marker-get-all root "Companion-collection"))

;;; Reading

(ert-deftest test-companion-collections-parse-a-hand-written-line ()
  (should (equal '("Ana's open" ("pov" . "id-1") ("todo" . "!FINISHED"))
                 (org-scribe-companion-collections--parse
                  "Ana's open |pov=id-1|  todo = !FINISHED  ")))
  ;; unknown keys, empty values, repeats and junk are ignored when reading
  (should (equal '("X" ("plot" . "p"))
                 (org-scribe-companion-collections--parse
                  "X | colour=red | pov= | plot=p | plot=q | nonsense")))
  (should-not (org-scribe-companion-collections--parse " | pov=id-1")))

(ert-deftest test-companion-collections-in-the-snapshot ()
  (test-companion-collections--with-project root
    (org-scribe--project-marker-set-all
     root "Companion-collection"
     (list (format "Alex | pov=%s | todo=-" test-companion-collections--alex)
           "Nameless filter |"))
    (let ((collections (plist-get (org-scribe-companion--snapshot root) :collections)))
      (should (= 2 (length collections)))
      (should (equal "Alex" (plist-get (aref collections 0) :name)))
      (should (equal (list :pov test-companion-collections--alex :todo "-")
                     (plist-get (aref collections 0) :filters))))
    ;; and it serializes: a collection without filters is {}
    (let ((data (json-parse-string (org-scribe-companion--snapshot-json root)
                                   :object-type 'alist)))
      (should (equal "-" (alist-get 'todo (alist-get 'filters (aref (alist-get 'collections data) 0))))))))

(ert-deftest test-companion-collections-change-the-revision ()
  (test-companion-collections--with-project root
    (let ((before (org-scribe-companion--revision-string root)))
      (should (string-match-p "-k[0-9a-f]\\{8\\}\\'" before))
      (should (test-companion-collections--ok-p
               (test-companion-collections--run root :name "Mine" :filters '(:todo "TODO"))))
      (should-not (equal before (org-scribe-companion--revision-string root))))))

;;; set-collection

(ert-deftest test-companion-collections-create-writes-one-line-in-key-order ()
  (test-companion-collections--with-project root
    (let ((answer (test-companion-collections--run
                   root :name "  Alex,  open  "
                   :filters (list :todo "!FINISHED" :pov test-companion-collections--alex
                                  :plot "" :location "-"))))
      (should (test-companion-collections--ok-p answer))
      (should (equal "Alex, open" (plist-get answer :name))))
    (should (equal (list (format "Alex, open | pov=%s | location=- | todo=!FINISHED"
                                 test-companion-collections--alex))
                   (test-companion-collections--lines root)))))

(ert-deftest test-companion-collections-new-ones-go-at-the-end ()
  (test-companion-collections--with-project root
    (test-companion-collections--run root :name "B" :filters '(:todo "TODO"))
    (test-companion-collections--run root :name "A" :filters '(:todo "-"))
    (should (equal '("B" "A")
                   (mapcar #'car (org-scribe-companion-collections-read root))))))

(ert-deftest test-companion-collections-refusals ()
  (test-companion-collections--with-project root
    (test-companion-collections--run root :name "Taken" :filters '(:todo "TODO"))
    (dolist (case `(((:name "" :filters (:todo "TODO")) "invalid")
                    ((:name "a | b" :filters (:todo "TODO")) "invalid")
                    ((:name ,(make-string 81 ?x) :filters (:todo "TODO")) "invalid")
                    ((:name "taken" :filters (:todo "-")) "invalid") ; case is ignored
                    ((:name "New" :filters (:colour "red")) "invalid")
                    ((:name "New" :filters (:pov "no-such-id")) "invalid")
                    ;; a plot thread's ID is not a character
                    ((:name "New" :filters (:pov ,test-companion-collections--plot)) "invalid")
                    ((:name "New" :filters (:todo "DONE")) "invalid")
                    ((:name "New" :filters (:todo "!DONE")) "invalid")
                    ((:name "New" :filters (:pov "" :todo "")) "invalid")
                    ((:name "New") "invalid")
                    ((:old "Gone" :name "New" :filters (:todo "TODO")) "conflict")
                    ((:old "Gone" :delete t) "conflict")
                    ((:delete t) "invalid")))
      (should (equal (cadr case)
                     (test-companion-collections--code
                      (apply #'test-companion-collections--run root (car case))))))
    ;; nothing was written by the refusals
    (should (equal '("Taken | todo=TODO") (test-companion-collections--lines root)))))

(ert-deftest test-companion-collections-update-rename-and-delete ()
  (test-companion-collections--with-project root
    (test-companion-collections--run root :name "One" :filters '(:todo "TODO"))
    (test-companion-collections--run root :name "Two" :filters '(:todo "-"))
    ;; new filters, same place
    (should (test-companion-collections--ok-p
             (test-companion-collections--run
              root :old "one" :name "One"
              :filters (list :character test-companion-collections--sam))))
    (should (equal (list (format "One | character=%s" test-companion-collections--sam)
                         "Two | todo=-")
                   (test-companion-collections--lines root)))
    ;; renaming to its own name in another case is allowed
    (should (test-companion-collections--ok-p
             (test-companion-collections--run root :old "Two" :name "TWO")))
    ;; renaming onto another collection is not
    (should (equal "invalid" (test-companion-collections--code
                              (test-companion-collections--run root :old "TWO" :name "one"))))
    (should (test-companion-collections--ok-p
             (test-companion-collections--run root :old "One" :delete t)))
    (should (equal '("TWO | todo=-") (test-companion-collections--lines root)))))

(ert-deftest test-companion-collections-rename-keeps-the-line-as-written ()
  "A part that names a deleted entity, or that the code does not know, survives a rename."
  (test-companion-collections--with-project root
    (org-scribe--project-marker-set-all
     root "Companion-collection" '("Old name |pov=deleted-id| mood=tense"))
    (should (test-companion-collections--ok-p
             (test-companion-collections--run root :old "Old name" :name "New name")))
    (should (equal '("New name |pov=deleted-id| mood=tense")
                   (test-companion-collections--lines root)))))

(ert-deftest test-companion-collections-other-marker-lines-are-kept ()
  (test-companion-collections--with-project root
    (let ((before (org-scribe--project-marker-get root "Language")))
      (test-companion-collections--run root :name "A" :filters '(:todo "TODO"))
      (test-companion-collections--run root :old "A" :delete t)
      (should (equal before (org-scribe--project-marker-get root "Language")))
      (should-not (test-companion-collections--lines root)))))

(ert-deftest test-companion-collections-need-the-marker-file ()
  (test-companion-collections--with-project root
    (delete-file (expand-file-name ".org-scribe-project" root))
    (org-scribe-project-type-cache-clear)
    (should (equal "invalid" (test-companion-collections--code
                              (test-companion-collections--run
                               root :name "A" :filters '(:todo "TODO")))))))

(ert-deftest test-companion-collections-hidden-entities-count ()
  "A character with a negative Weight can still be a collection's filter."
  (test-companion-collections--with-project root
    (with-current-buffer (find-file-noselect (expand-file-name "design.org" root))
      (goto-char (org-find-property "ID" test-companion-collections--sam))
      (org-entry-put nil "Weight" "-1"))
    (should (test-companion-collections--ok-p
             (test-companion-collections--run
              root :name "Sam" :filters (list :pov test-companion-collections--sam))))))

(provide 'test-companion-collections)

;;; test-companion-collections.el ends here
