;;; test-simple-capture.el --- Tests for the simple method's capture -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Under `# Method: simple' the capture commands ask for a name only and file
;; a bare heading: an ID and the blank properties the entity needs.  Every
;; other project keeps its full templates.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (dolist (dir '("../core" "../search" "../linking" "../capture" "../templates"
                 "../companion"))
    (add-to-list 'load-path (expand-file-name dir default-directory))))

(require 'org-scribe-capture)
(require 'org-scribe-character-links)
(require 'org-scribe-location-links)
(require 'org-scribe-object-links)
(require 'org-scribe-plot-links)

(defconst test-simple-capture--kinds
  ;; KIND BUILDER KEY SECTION-EN EXPECTED-PROPERTIES
  '((character org-scribe-character-capture-templates "c" "Characters"
               ("ID" "Role" "Weight"))
    (location org-scribe-location-capture-templates "l" "Setting" ("ID" "Type"))
    (object org-scribe-object-capture-templates "o" "Objects" ("ID" "Type"))
    (timeline org-scribe-timeline-capture-templates "t" "Timeline"
              ("ID" "Date" "Time"))
    (plot-thread org-scribe-plot-thread-capture-templates "p" "Plot Threads"
                 ("ID" "THREAD-TYPE" "Weight"))))

(defmacro test-simple-capture--with-project (method type &rest body)
  "Run BODY in a temp project (marker METHOD, TYPE) with a flat bare notebook.
Binds `notebook'."
  (declare (indent 2))
  `(let* ((root (make-temp-file "test-simple-capture-" t))
          (notebook (expand-file-name "design.org" root)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".org-scribe-project" root)
             (insert (format "# Writing project: T\n# Type: %s\n# Language: en\n%s"
                             ,type (if ,method (format "# Method: %s\n" ,method) ""))))
           (with-temp-file (expand-file-name (if (equal ,type "novel") "novel.org" "story.org")
                                             root)
             (insert "* Act\n"))
           (with-temp-file notebook
             (insert "* Characters\n\n* Setting\n\n* Plot Threads\n\n* Objects\n\n* Timeline\n\n* Scratchpad\n"))
           (let ((default-directory (file-name-as-directory root)))
             (org-scribe-project-type-cache-clear root)
             ,@body))
       (when (get-file-buffer notebook)
         (with-current-buffer (get-file-buffer notebook) (set-buffer-modified-p nil))
         (kill-buffer (get-file-buffer notebook)))
       (delete-directory root t))))

(defun test-simple-capture--capture (builder key name)
  "Capture through BUILDER's template KEY naming it NAME; return the prompt count."
  (let ((prompts 0))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) (cl-incf prompts) name))
              ((symbol-function 'completing-read)
               (lambda (&rest _) (cl-incf prompts) name)))
      (let ((org-capture-templates (funcall builder)))
        (org-capture nil key)
        (org-capture-finalize)))
    prompts))

(defun test-simple-capture--entry (notebook name)
  "Return (PROPERTY-NAMES BODY-TEXT PROPERTY-ALIST) of NAME's heading in NOTEBOOK."
  (with-current-buffer (find-file-noselect notebook)
    (revert-buffer t t)
    (goto-char (point-min))
    (re-search-forward (format "^\\*+ %s$" (regexp-quote name)))
    (let* ((props (org-entry-properties nil 'standard))
           (end (save-excursion (org-end-of-subtree t t) (point)))
           (body (save-excursion
                   (org-end-of-meta-data t)
                   (string-trim (buffer-substring (point) end)))))
      (list (mapcar #'car props) body props))))

(ert-deftest test-simple-capture-asks-only-for-the-name ()
  "Each simple capture prompts exactly once, for the name."
  (dolist (kind test-simple-capture--kinds)
    (test-simple-capture--with-project "simple" "novel"
      (should (= 1 (test-simple-capture--capture
                    (nth 1 kind) (nth 2 kind) "Fiodor, the black cat"))))))

(ert-deftest test-simple-capture-files-a-bare-heading ()
  "The heading carries the ID and the blank properties, and no body."
  (dolist (type '("novel" "short-story"))
    (dolist (kind test-simple-capture--kinds)
      (test-simple-capture--with-project "simple" type
        (test-simple-capture--capture (nth 1 kind) (nth 2 kind) "Fiodor")
        (pcase-let ((`(,names ,body ,props) (test-simple-capture--entry notebook "Fiodor")))
          (should (equal (sort (mapcar #'upcase (nth 4 kind)) #'string<)
                         (sort (remove "CATEGORY" (copy-sequence names)) #'string<)))
          (should (string-empty-p body))
          (should (cdr (assoc "ID" props)))
          (dolist (name (remove "ID" (nth 4 kind)))
            (should (equal "" (cdr (assoc (upcase name) props))))))))))

(ert-deftest test-simple-capture-files-under-its-section-at-level-two ()
  "A flat notebook: the entity is a level-2 child of its section."
  (dolist (kind test-simple-capture--kinds)
    (test-simple-capture--with-project "simple" "short-story"
      (test-simple-capture--capture (nth 1 kind) (nth 2 kind) "Fiodor")
      (with-current-buffer (find-file-noselect notebook)
        (goto-char (point-min))
        (re-search-forward "^\\*+ Fiodor$")
        (should (= 2 (org-current-level)))
        (should (equal (nth 3 kind)
                       (save-excursion (org-up-heading-safe)
                                       (org-get-heading t t t t))))))))

(ert-deftest test-simple-capture-entities-are-found-with-their-id ()
  "A captured character, location, object and thread is an entity at once."
  (test-simple-capture--with-project "simple" "novel"
    (dolist (kind test-simple-capture--kinds)
      (test-simple-capture--capture (nth 1 kind) (nth 2 kind) (format "N-%s" (car kind))))
    (let ((id-of (lambda (alist name) (cadr (assoc name alist)))))
      (should (funcall id-of (org-scribe--get-all-characters) "N-character"))
      (should (funcall id-of (org-scribe--get-all-locations) "N-location"))
      (should (funcall id-of (org-scribe--get-all-objects) "N-object"))
      (should (funcall id-of (org-scribe--get-all-plot-threads) "N-plot-thread")))
    ;; nothing was left for the automatic pass to mint
    (with-current-buffer (find-file-noselect notebook)
      (should (= 0 (org-scribe--mint-entity-ids))))))

(ert-deftest test-simple-capture-works-in-spanish ()
  "The prompt and menu label come from the project language."
  (test-simple-capture--with-project "simple" "novel"
    (with-temp-file (expand-file-name ".org-scribe-project" root)
      (insert "# Writing project: T\n# Type: novel\n# Language: es\n# Method: simple\n"))
    (org-scribe-project-type-cache-clear root)
    (let ((template (car (org-scribe-character-capture-templates))))
      (should (equal (let ((org-scribe-message-language 'es))
                       (org-scribe-msg 'capture-char-name))
                     (nth 1 template)))
      (should (string-match-p "Nombre del personaje" (nth 4 template))))))

(ert-deftest test-simple-capture-leaves-other-methods-alone ()
  "Without `Method: simple' the builders return their full templates."
  (dolist (method '(nil "sistema" "helice"))
    (dolist (type '("novel" "short-story"))
      (dolist (kind test-simple-capture--kinds)
        (test-simple-capture--with-project method type
          (let ((template (car (funcall (nth 1 kind)))))
            (should (equal (nth 2 kind) (nth 0 template)))
            ;; full templates carry more than the bare ID and marker lines
            (should (> (length (split-string (nth 4 template) "\n")) 8))))))))

(ert-deftest test-simple-capture-timeline-event-is-found-by-the-chronology ()
  "A captured timeline event gets its Date, which is how the chronology reads it."
  (test-simple-capture--with-project "simple" "novel"
    (test-simple-capture--capture #'org-scribe-timeline-capture-templates "t" "The fall")
    (with-current-buffer (find-file-noselect notebook)
      (revert-buffer t t)
      (goto-char (point-min))
      (re-search-forward "^\\*+ The fall$")
      (org-entry-put nil "Date" "Day 3")
      (should (equal "Day 3" (org-entry-get nil "Date"))))))

(ert-deftest test-simple-capture-companion-fill-takes-the-role-value ()
  "The companion's filler needs no prompt on the marker line to place a value."
  (require 'org-scribe-companion-entity)
  (test-simple-capture--with-project "simple" "novel"
    (let ((template (nth 4 (car (org-scribe-character-capture-templates)))))
      (let ((text (org-scribe-companion-entity--expand
                   template "Fiodor" "Role" "Cat" "ID-1")))
        (should (string-match-p "^\\* Fiodor$" text))
        (should (string-match-p "^:ID: ID-1$" text))
        (should (string-match-p "^:Role: Cat$" text))
        (should (string-match-p "^:Weight:$" text)))
      (let ((text (org-scribe-companion-entity--expand
                   template "Fiodor" "Role" "" "ID-1")))
        (should (string-match-p "^:Role:$" text))))))

(ert-deftest test-simple-capture-companion-creates-a-bare-entity ()
  "The companion's create-entity files the same bare heading, with the role."
  (require 'org-scribe-companion-entity)
  (test-simple-capture--with-project "simple" "novel"
    (let ((answer (org-scribe-companion-entity--create
                   root '(:kind "character" :name "Fiodor" :value "Cat"))))
      (should (plist-get answer :id)))
    (with-current-buffer (org-scribe-companion--visit notebook)
      (save-buffer))
    (pcase-let ((`(,_names ,body ,props) (test-simple-capture--entry notebook "Fiodor")))
      (should (string-empty-p body))
      (should (equal "Cat" (cdr (assoc "ROLE" props))))
      (should (equal "" (cdr (assoc "WEIGHT" props)))))))

(provide 'test-simple-capture)

;;; test-simple-capture.el ends here
