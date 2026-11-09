;;; test-entity-ids.el --- Tests for automatic entity IDs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; An entity is found by its ID, so a heading typed by hand was invisible
;; until a setup command ran.  `org-scribe--ensure-entity-ids' (linking/
;; org-scribe-linking-core.el) mints the IDs the first time the entities are
;; asked for.  These tests build a real `simple' project and type headings
;; into its notebook, as a writer would.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (dolist (dir '("../core" "../search" "../linking" "../capture" "../templates"))
    (add-to-list 'load-path (expand-file-name dir default-directory))))

(require 'org-scribe-character-links)
(require 'org-scribe-location-links)
(require 'org-scribe-object-links)
(require 'org-scribe-plot-links)
(require 'org-scribe-project)

(defmacro test-entity-ids--with-project (spec &rest body)
  "Create a project, bind SPEC's (DIR NOTEBOOK) and run BODY, then clean up.
SPEC is (DIR-VAR NOTEBOOK-VAR &key KIND METHOD LANGUAGE)."
  (declare (indent 1))
  (let ((dir (nth 0 spec)) (notebook (nth 1 spec))
        (kind (or (plist-get (nthcdr 2 spec) :kind) ''short-story))
        (method (plist-get (nthcdr 2 spec) :method))
        (lang (or (plist-get (nthcdr 2 spec) :language) ''en)))
    `(let* ((base (make-temp-file "test-entity-ids-" t))
            (,dir (expand-file-name "P" base))
            (default-directory (file-name-as-directory ,dir)))
       (unwind-protect
           (progn
             (cl-letf (((symbol-function 'find-file) #'ignore)
                       ((symbol-function 'project-remember-project) #'ignore))
               (if (eq ,kind 'novel)
                   (org-scribe-create-novel-project base "P" ,lang ,method)
                 (org-scribe-create-short-story-project base "P" ,lang nil ,method)))
             (org-scribe-project-type-cache-clear)
             (let ((,notebook (expand-file-name (org-scribe-lang-file 'design ,lang)
                                                ,dir)))
               ,@body))
         (dolist (b (buffer-list))
           (when (and (buffer-file-name b)
                      (string-prefix-p base (buffer-file-name b)))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b)))
         (delete-directory base t)))))

(defun test-entity-ids--type-under (file section text)
  "Insert TEXT at the end of SECTION's heading line's subtree in FILE, on disk."
  ;; Creation left a buffer visiting FILE; drop it so the write is not a
  ;; conflict and the next read goes back to the disk.
  (when-let* ((b (find-buffer-visiting file)))
    (with-current-buffer b (set-buffer-modified-p nil))
    (kill-buffer b))
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (re-search-forward (format "^\\* %s$" (regexp-quote section)))
    (org-end-of-subtree t t)
    (unless (bolp) (insert "\n"))
    (insert text)
    (write-region (point-min) (point-max) file nil 'silent)))

(defun test-entity-ids--disk-has-id (file name)
  "Return non-nil when NAME's heading in FILE (read from disk) has an ID."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (when (re-search-forward (format "^\\*+ %s$" (regexp-quote name)) nil t)
      (org-id-get))))

(ert-deftest test-entity-ids-typed-character-is-found-and-saved ()
  "A character typed by hand, with a blank Role, is listed and gets an ID on disk."
  (test-entity-ids--with-project (dir notebook :method 'simple)
    (test-entity-ids--type-under
     notebook "Characters" "** Fiodor, the black cat\n:PROPERTIES:\n:Role:\n:END:\n")
    (should-not (test-entity-ids--disk-has-id notebook "Fiodor, the black cat"))
    (let ((found (org-scribe--get-all-characters)))
      (should (assoc "Fiodor, the black cat" found))
      (should (stringp (cadr (assoc "Fiodor, the black cat" found)))))
    (should (test-entity-ids--disk-has-id notebook "Fiodor, the black cat"))))

(ert-deftest test-entity-ids-every-entity-kind ()
  "Locations, objects and plot threads typed by hand are found as well."
  (test-entity-ids--with-project (dir notebook :method 'simple)
    (test-entity-ids--type-under
     notebook "Setting" "** The attic\n:PROPERTIES:\n:Type:\n:END:\n")
    (test-entity-ids--type-under
     notebook "Objects" "** The brass key\n:PROPERTIES:\n:Type:\n:END:\n")
    (test-entity-ids--type-under
     notebook "Plot Threads" "** The missing will\n:PROPERTIES:\n:THREAD-TYPE:\n:Weight:\n:END:\n")
    (should (assoc "The attic" (org-scribe--get-all-locations)))
    (should (assoc "The brass key" (org-scribe--get-all-objects)))
    (should (assoc "The missing will" (org-scribe--get-all-plot-threads)))))

(ert-deftest test-entity-ids-minting-is-idempotent ()
  "A second pass adds nothing and does not change the ID."
  (test-entity-ids--with-project (dir notebook :method 'simple)
    (test-entity-ids--type-under
     notebook "Characters" "** Fiodor\n:PROPERTIES:\n:Role:\n:END:\n")
    (let ((first (cadr (assoc "Fiodor" (org-scribe--get-all-characters)))))
      (should (stringp first))
      (should (= 0 (org-scribe--ensure-entity-ids org-scribe--character-entity)))
      (with-current-buffer (find-file-noselect notebook)
        (setq org-scribe--entity-ids-tick nil)
        (should (= 0 (org-scribe--mint-entity-ids))))
      (should (equal first (cadr (assoc "Fiodor" (org-scribe--get-all-characters))))))))

(ert-deftest test-entity-ids-non-entities-get-none ()
  "Section headings and Scratchpad notes are not entities and get no ID."
  (test-entity-ids--with-project (dir notebook :method 'simple)
    (test-entity-ids--type-under notebook "Scratchpad" "** A loose idea\nSome text.\n")
    (test-entity-ids--type-under notebook "Characters" "** Fiodor\n:PROPERTIES:\n:Role:\n:END:\n")
    (org-scribe--get-all-characters)
    (should (test-entity-ids--disk-has-id notebook "Fiodor"))
    (should-not (test-entity-ids--disk-has-id notebook "A loose idea"))
    (dolist (section '("Characters" "Setting" "Plot Threads" "Objects" "Timeline" "Scratchpad"))
      (should-not (test-entity-ids--disk-has-id notebook section)))))

(ert-deftest test-entity-ids-unsaved-buffer-is-not-written ()
  "With unsaved edits in the notebook, IDs are minted in the buffer only."
  (test-entity-ids--with-project (dir notebook :method 'simple)
    (test-entity-ids--type-under notebook "Characters" "** Fiodor\n:PROPERTIES:\n:Role:\n:END:\n")
    (with-current-buffer (find-file-noselect notebook)
      (goto-char (point-max))
      (insert "\nunsaved words\n")
      (should (buffer-modified-p)))
    (should (assoc "Fiodor" (org-scribe--get-all-characters)))
    (with-current-buffer (find-file-noselect notebook)
      (should (buffer-modified-p))
      (should (org-id-get (progn (goto-char (point-min))
                                 (re-search-forward "^\\*\\* Fiodor")
                                 (point)))))
    (should-not (test-entity-ids--disk-has-id notebook "Fiodor"))))

(ert-deftest test-entity-ids-read-only-buffer-is-left-alone ()
  "A read-only notebook buffer is not modified; its entity is just not listed."
  (test-entity-ids--with-project (dir notebook :method 'simple)
    (test-entity-ids--type-under notebook "Characters" "** Fiodor\n:PROPERTIES:\n:Role:\n:END:\n")
    (with-current-buffer (find-file-noselect notebook)
      (setq buffer-read-only t))
    (should-not (assoc "Fiodor" (org-scribe--get-all-characters)))
    (with-current-buffer (find-file-noselect notebook)
      (should-not (buffer-modified-p)))))

(ert-deftest test-entity-ids-narrowing-and-point-are-preserved ()
  "Minting leaves the notebook buffer's narrowing and point where they were."
  (test-entity-ids--with-project (dir notebook :method 'simple)
    (test-entity-ids--type-under notebook "Characters" "** Fiodor\n:PROPERTIES:\n:Role:\n:END:\n")
    (with-current-buffer (find-file-noselect notebook)
      (goto-char (point-min))
      (re-search-forward "^\\* Objects")
      (org-narrow-to-subtree)
      ;; The new ID is written before the narrowed region, so compare by
      ;; marker, not by position.
      (let ((point (copy-marker (point)))
            (beg (copy-marker (point-min)))
            (end (copy-marker (point-max) t)))
        (org-scribe--get-all-characters)
        (should (buffer-narrowed-p))
        (should (= beg (point-min)))
        (should (= end (point-max)))
        (should (= point (point)))))))

(ert-deftest test-entity-ids-shipped-templates-need-no-minting ()
  "Creation already mints every template entity: nothing is added later.
Guards against the getter giving an ID to a heading creation did not."
  (dolist (spec '((novel nil en) (novel helice es) (short-story nil en)
                  (short-story nil es) (novel simple en) (short-story simple es)))
    (test-entity-ids--with-project (dir notebook :kind (nth 0 spec)
                                        :method (nth 1 spec)
                                        :language (nth 2 spec))
      (let ((before (with-temp-buffer (insert-file-contents notebook) (buffer-string))))
        (dolist (entry org-scribe-entity-registry)
          (should (= 0 (org-scribe--ensure-entity-ids (cdr entry)))))
        (org-scribe--get-all-characters)
        (should (equal before
                       (with-temp-buffer (insert-file-contents notebook)
                                         (buffer-string))))))))

(provide 'test-entity-ids)

;;; test-entity-ids.el ends here
