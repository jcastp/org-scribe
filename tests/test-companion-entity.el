;;; test-companion-entity.el --- Tests for the companion's entity forms -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for `companion/org-scribe-companion-entity.el': the entity data the
;; forms read, and the commands that write properties and section bodies of
;; the design notebook.  They run on a temporary copy of `examples/demo-novel'.

;;; Code:

(require 'ert)
(require 'cl-lib)

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

(defconst test-companion-entity--repo
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defconst test-companion-entity--alex "char-alex-rivera-001")

(defmacro test-companion-entity--with-project (root-var &rest body)
  "Run BODY with a copy of the demo novel at ROOT-VAR."
  (declare (indent 1))
  `(let* ((,root-var (file-name-as-directory (make-temp-file "test-companion-ent-" t)))
          (default-directory ,root-var)
          (org-id-locations-file (expand-file-name "ids" ,root-var))
          (make-backup-files nil)
          (create-lockfiles nil)
          (org-scribe-companion-raise-frame nil))
     (unwind-protect
         (save-window-excursion
           (copy-directory (expand-file-name "examples/demo-novel" test-companion-entity--repo)
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

(defun test-companion-entity--run (root op &rest args)
  (org-scribe-companion-commands-run op args root))

(defun test-companion-entity--design (root)
  (with-current-buffer (find-file-noselect (expand-file-name "design.org" root))
    (org-with-wide-buffer (buffer-string))))

(defun test-companion-entity--code (answer)
  (plist-get (plist-get answer :error) :code))

(defun test-companion-entity--section (data title)
  (cl-find title (append (plist-get data :sections) nil)
           :key (lambda (s) (plist-get s :title)) :test #'equal))

(defun test-companion-entity--changed-lines (before after)
  "Return the list of (OLD . NEW) line pairs that differ, same line count assumed."
  (cl-remove-if #'null
                (cl-mapcar (lambda (a b) (and (not (equal a b)) (cons a b)))
                           (split-string before "\n") (split-string after "\n"))))

;;; Reading

(ert-deftest test-companion-entity-get-returns-properties-sections-and-scenes ()
  (test-companion-entity--with-project root
    (let ((data (org-scribe-companion-entity-get root test-companion-entity--alex)))
      (should (eq t (plist-get data :ok)))
      (should (equal "character" (plist-get data :kind)))
      (should (equal "Alex Rivera" (plist-get data :name)))
      (let ((role (cl-find "Role" (append (plist-get data :properties) nil)
                           :key (lambda (p) (plist-get p :name)) :test #'equal)))
        (should (equal "Protagonist" (plist-get role :value))))
      (should (member "Protagonist" (append (plist-get (plist-get data :choices) :Role) nil)))
      (let ((sections (plist-get data :sections)))
        (should (equal 0 (plist-get (aref sections 0) :index)))
        (should (string-match-p "Concept ::" (plist-get (aref sections 0) :body)))
        (should (cl-every (lambda (title) (test-companion-entity--section data title))
                          '("Desire" "Need" "Plan" "Backstory" "Notes"))))
      (should (>= (length (plist-get data :relationships)) 2))
      (should (> (length (plist-get data :appearsIn)) 3))
      (should (stringp (plist-get (aref (plist-get data :appearsIn) 0) :id))))))

(ert-deftest test-companion-entity-get-unknown-and-plot-points ()
  (test-companion-entity--with-project root
    (should (equal "not-found"
                   (test-companion-entity--code
                    (org-scribe-companion-entity-get root "nope"))))
    (let* ((points (plist-get (plist-get (org-scribe-companion--snapshot root) :entities)
                              :plot-point))
           (data (org-scribe-companion-entity-get root (plist-get (aref points 0) :id))))
      (should (eq t (plist-get data :ok)))
      (should (eq t (plist-get data :readOnly))))))

;;; set-entity-property

(ert-deftest test-companion-entity-set-property-changes-one-line ()
  (test-companion-entity--with-project root
    (let ((before (test-companion-entity--design root)))
      (should (eq t (plist-get (test-companion-entity--run
                                root "set-entity-property" :id test-companion-entity--alex
                                :key "Role" :value "Ally")
                               :ok)))
      (let ((changed (test-companion-entity--changed-lines
                      before (test-companion-entity--design root))))
        (should (= 1 (length changed)))
        (should (string-match-p "\\`:Role: +Protagonist\\'" (car (car changed))))
        (should (string-match-p "\\`:Role: +Ally\\'" (cdr (car changed))))))))

(ert-deftest test-companion-entity-set-property-validates ()
  (test-companion-entity--with-project root
    (let ((before (test-companion-entity--design root)))
      (dolist (case '((:key "ID" :value "x" "read-only")
                      (:key "RelationshipsData" :value "" "read-only")
                      (:key "SCHEDULED" :value "FINISHED" "read-only")
                      (:key "category" :value "x" "read-only")
                      (:key "ARCHIVE" :value "x" "read-only")
                      (:key "Weight" :value "heavy" "invalid")
                      (:key "bad key" :value "x" "invalid")
                      (:key nil :value "x" "invalid")))
        (should (equal (car (last case))
                       (test-companion-entity--code
                        (apply #'test-companion-entity--run root "set-entity-property"
                               :id test-companion-entity--alex (butlast case))))))
      (should (equal "not-found"
                     (test-companion-entity--code
                      (test-companion-entity--run root "set-entity-property" :id "nope"
                                                  :key "Role" :value "x"))))
      (should (equal before (test-companion-entity--design root)))
      ;; a negative weight is allowed: it hides the entity
      (should (eq t (plist-get (test-companion-entity--run
                                root "set-entity-property" :id test-companion-entity--alex
                                :key "Weight" :value "-1")
                               :ok))))))

(ert-deftest test-companion-entity-plot-points-and-scenes-are-not-editable-here ()
  (test-companion-entity--with-project root
    (let* ((points (plist-get (plist-get (org-scribe-companion--snapshot root) :entities)
                              :plot-point))
           (id (plist-get (aref points 0) :id)))
      (should (equal "read-only"
                     (test-companion-entity--code
                      (test-companion-entity--run root "set-entity-property" :id id
                                                  :key "Role" :value "x"))))
      (should (equal "not-found"
                     (test-companion-entity--code
                      (test-companion-entity--run root "set-entity-property"
                                                  :id "a6aaa600-35b3-4193-9510-a4bb2ef0000f"
                                                  :key "Role" :value "x")))))))

;;; set-entity-section

(ert-deftest test-companion-entity-set-section-replaces-only-that-body ()
  (test-companion-entity--with-project root
    (let* ((before (test-companion-entity--design root))
           (data (org-scribe-companion-entity-get root test-companion-entity--alex))
           (section (test-companion-entity--section data "Need"))
           (answer (test-companion-entity--run
                    root "set-entity-section" :id test-companion-entity--alex
                    :index (plist-get section :index) :title "Need"
                    :hash (plist-get section :hash) :body "\n- Goal :: A new need.\n\n")))
      (should (eq t (plist-get answer :ok)))
      (let ((after (test-companion-entity--design root)))
        (should (equal (replace-regexp-in-string
                        (regexp-quote (plist-get section :body)) "\n- Goal :: A new need.\n\n"
                        before t t)
                       after))
        (should (string-match-p "^\\*\\*\\*\\* Need$" after))
        (should (string-match-p "^\\*\\*\\*\\* Plan$" after)))
      ;; the other sections' hashes still hold
      (let ((again (org-scribe-companion-entity-get root test-companion-entity--alex)))
        (should (equal (plist-get (test-companion-entity--section data "Plan") :hash)
                       (plist-get (test-companion-entity--section again "Plan") :hash)))))))

(ert-deftest test-companion-entity-set-section-zero-leaves-deeper-headings-untouched ()
  (test-companion-entity--with-project root
    (let* ((before (test-companion-entity--design root))
           (data (org-scribe-companion-entity-get root test-companion-entity--alex))
           (intro (aref (plist-get data :sections) 0)))
      (test-companion-entity--run
       root "set-entity-section" :id test-companion-entity--alex :index 0
       :title "Alex Rivera" :hash (plist-get intro :hash) :body "\nJust one line.\n\n")
      (let ((after (test-companion-entity--design root)))
        (should (string-match-p "Just one line" after))
        (should-not (string-match-p "A designer who has never once" after))
        ;; everything from the first sub-heading on is identical
        (should (equal (substring before (string-search "\n**** Desire\n" before))
                       (substring after (string-search "\n**** Desire\n" after))))))))

(ert-deftest test-companion-entity-set-section-conflict-leaves-the-buffer-alone ()
  (test-companion-entity--with-project root
    (let* ((data (org-scribe-companion-entity-get root test-companion-entity--alex))
           (section (test-companion-entity--section data "Notes"))
           (before (test-companion-entity--design root)))
      ;; someone edits that section in Emacs after the form loaded it
      (with-current-buffer (find-file-noselect (expand-file-name "design.org" root))
        (org-with-wide-buffer
         (goto-char (point-min))
         (re-search-forward "^The counting is the voice\\.")
         (replace-match "The counting is the voice, edited in Emacs.")))
      (let* ((edited (test-companion-entity--design root))
             (answer (test-companion-entity--run
                      root "set-entity-section" :id test-companion-entity--alex
                      :index (plist-get section :index) :title "Notes"
                      :hash (plist-get section :hash) :body "Mine.\n")))
        (should-not (equal before edited))
        (should (equal "conflict" (test-companion-entity--code answer)))
        (should (string-match-p "edited in Emacs"
                                (plist-get (plist-get answer :error) :current)))
        (should (equal edited (test-companion-entity--design root)))))))

(ert-deftest test-companion-entity-set-section-refuses-headings-and-wrong-titles ()
  (test-companion-entity--with-project root
    (let* ((data (org-scribe-companion-entity-get root test-companion-entity--alex))
           (section (test-companion-entity--section data "Notes"))
           (before (test-companion-entity--design root)))
      (should (equal "invalid"
                     (test-companion-entity--code
                      (test-companion-entity--run
                       root "set-entity-section" :id test-companion-entity--alex
                       :index (plist-get section :index) :title "Notes"
                       :hash (plist-get section :hash) :body "text\n* A heading\n"))))
      (should (equal "conflict"
                     (test-companion-entity--code
                      (test-companion-entity--run
                       root "set-entity-section" :id test-companion-entity--alex
                       :index (plist-get section :index) :title "Another title"
                       :hash (plist-get section :hash) :body "x\n"))))
      (should (equal "conflict"
                     (test-companion-entity--code
                      (test-companion-entity--run
                       root "set-entity-section" :id test-companion-entity--alex
                       :index 99 :hash "x" :body "x\n"))))
      (should (equal before (test-companion-entity--design root))))))

(ert-deftest test-companion-entity-edits-are-one-undo-step ()
  (test-companion-entity--with-project root
    (let* ((before (test-companion-entity--design root))
           (data (org-scribe-companion-entity-get root test-companion-entity--alex))
           (section (test-companion-entity--section data "Plan")))
      (test-companion-entity--run
       root "set-entity-section" :id test-companion-entity--alex
       :index (plist-get section :index) :title "Plan"
       :hash (plist-get section :hash) :body "New plan.\n")
      (with-current-buffer (find-file-noselect (expand-file-name "design.org" root))
        (undo-boundary) (undo-start) (undo-more 1) (undo-more 1))
      (should (equal before (test-companion-entity--design root))))))

;;; capture-entity

(ert-deftest test-companion-entity-capture-runs-the-existing-command ()
  (test-companion-entity--with-project root
    (require 'org-scribe-capture)
    (let (called)
      (cl-letf (((symbol-function 'org-scribe-capture-character)
                 (lambda () (interactive) (push 'character called)))
                ((symbol-function 'org-scribe-capture-location)
                 (lambda () (interactive) (push 'location called)))
                ((symbol-function 'org-scribe-capture-plot-thread)
                 (lambda () (interactive) (push 'plot called))))
        (dolist (kind '("character" "location" "plot"))
          (should (eq t (plist-get (test-companion-entity--run
                                    root "capture-entity" :kind kind)
                                   :ok))))
        (should-not called)             ; run from a timer, not inline
        (let ((deadline (+ (float-time) 3)))
          (while (and (< (length called) 3) (< (float-time) deadline))
            (accept-process-output nil 0.05)))
        (should (equal '(character location plot) (sort (copy-sequence called)
                                                        (lambda (a b) (string< a b)))))
        (should (equal "invalid"
                       (test-companion-entity--code
                        (test-companion-entity--run root "capture-entity" :kind "plot-point"))))))))

(ert-deftest test-companion-entity-messages-in-both-languages ()
  (dolist (key '(companion-error-invalid-weight companion-error-body-heading
                 companion-error-section-gone companion-error-section-conflict))
    (should (alist-get key org-scribe-messages-en))
    (should (alist-get key org-scribe-messages-es))))

(provide 'test-companion-entity)
;;; test-companion-entity.el ends here
