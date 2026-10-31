;;; test-companion-health.el --- Tests for the companion's Health tab -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for `companion/org-scribe-companion-health.el' (N1): the health
;; report as JSON (the same data the Emacs report is drawn from), the
;; `remove-link' and `show-health' operations, and the split of
;; `reporting/org-scribe-health.el' into data and drawing.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (dolist (dir '("core" "lang" "linking" "search" "capture" "templates" "reporting" "companion"))
    (add-to-list 'load-path (expand-file-name dir parent-dir))))

(require 'org-scribe-config)
(require 'org-scribe-core)
(require 'org-scribe-character-links)
(require 'org-scribe-location-links)
(require 'org-scribe-object-links)
(require 'org-scribe-plot-links)
(require 'org-scribe-plot-point-links)
(require 'org-scribe-link-update)
(require 'org-scribe-health)
(require 'org-scribe-companion)

(defconst test-companion-health--repo
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the org-scribe checkout.")

(defconst test-companion-health--handover "d3a8b898-d081-4aa1-aa8e-4a4a161f19f5"
  "ID of \"Scene 2 - The Handover\" (TOWRITE) in the demo novel.")

(defmacro test-companion-health--with-project (root-var demo &rest body)
  "Run BODY with a copy of the example DEMO at ROOT-VAR (not serving)."
  (declare (indent 2))
  `(let* ((,root-var (file-name-as-directory (make-temp-file "test-companion-health-" t)))
          (default-directory ,root-var)
          (org-id-locations-file (expand-file-name "ids" ,root-var))
          (make-backup-files nil)
          (create-lockfiles nil)
          (org-scribe-companion-raise-frame nil))
     (unwind-protect
         (save-window-excursion
           (copy-directory (expand-file-name (concat "examples/" ,demo) test-companion-health--repo)
                           ,root-var t t t)
           (org-scribe-project-type-cache-clear)
           (org-scribe-companion--clear-cache)
           ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (string-prefix-p ,root-var file)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (when (get-buffer "*org-scribe-health*") (kill-buffer "*org-scribe-health*"))
       (org-scribe-project-type-cache-clear)
       (org-scribe-companion--clear-cache)
       (delete-directory ,root-var t))))

(defun test-companion-health--json (root)
  "The /api/health answer for ROOT, through JSON and back, as an alist."
  (json-parse-string (json-serialize (org-scribe-companion-health-get root))
                     :object-type 'alist :array-type 'list
                     :null-object nil :false-object :false))

(defun test-companion-health--manuscript (root)
  (find-file-noselect (expand-file-name "novel.org" root)))

(defun test-companion-health--at-scene (root id)
  "Move point to the scene ID in the manuscript of ROOT; return the buffer."
  (let ((buffer (test-companion-health--manuscript root)))
    (with-current-buffer buffer
      (widen)
      (goto-char (point-min))
      (re-search-forward (concat "^:ID: +" (regexp-quote id)))
      (org-back-to-heading t))
    buffer))

(defun test-companion-health--run (root op &rest args)
  (org-scribe-companion-commands-run op args root))

;;; The data

(ert-deftest test-companion-health-json-matches-the-report-data ()
  "The page's numbers are the Emacs report's numbers."
  (test-companion-health--with-project root "demo-novel"
    (let* ((answer (test-companion-health--json root))
           (health (alist-get 'health answer))
           (data (org-scribe--health-data)))
      (should (eq t (alist-get 'ok answer)))
      (should (equal (org-scribe-companion--revision-string root) (alist-get 'revision answer)))
      (should (= (plist-get data :scene-count)
                 (alist-get 'scenes (alist-get 'overview health))))
      (should (= (plist-get data :total-words)
                 (alist-get 'words (alist-get 'overview health))))
      (should (= (length (plist-get data :open)) (alist-get 'open health)))
      ;; the orphans carry their IDs, for the page to open their forms
      (should (equal '(((name . "Marisol Rivera") (id . "char-marisol-rivera-001")))
                     (alist-get 'character (alist-get 'orphans health))))
      (should (equal (mapcar #'car (plist-get (plist-get data :orphans) :character))
                     '("Marisol Rivera")))
      ;; plot points are defined, and every one is served
      (should (null (alist-get 'plotPoint (alist-get 'orphans health))))
      (should (assq 'plotPoint (alist-get 'orphans health)))
      ;; the gate travels with its measured items and notes, as plain text
      (let ((gate (alist-get 'gate health)))
        (should (= 8 (alist-get 'total gate)))
        (should (equal '(13 13) (alist-get 'points gate)))
        (dolist (note (alist-get 'notes gate))
          (should-not (string-match-p "[*=]" note))))
      (should (alist-get 'rows (alist-get 'chapters health)))
      (should (= 15 (alist-get 'total (alist-get 'storyTime health)))))))

(ert-deftest test-companion-health-short-story-has-no-chapters-nor-plot-points ()
  (test-companion-health--with-project root "demo-short-story"
    (let ((health (alist-get 'health (test-companion-health--json root))))
      (should (null (alist-get 'chapters health)))
      (should (assq 'chapters health))
      (should (null (alist-get 'plotPoint (alist-get 'orphans health)))))))

(ert-deftest test-companion-health-spanish-project-speaks-spanish ()
  (test-companion-health--with-project root "demo-novela"
    (let* ((org-scribe-message-language 'en)
           (health (alist-get 'health (test-companion-health--json root))))
      ;; the sentences composed in Emacs are in the project's language
      (should (string-prefix-p "Mín" (alist-get 'summary (alist-get 'chapters health)))))))

(ert-deftest test-companion-health-missing-field-lists-the-pending-scene ()
  (test-companion-health--with-project root "demo-novel"
    (with-current-buffer (test-companion-health--at-scene root test-companion-health--handover)
      (org-entry-delete nil "PoV"))
    (let* ((health (alist-get 'health (test-companion-health--json root)))
           (pov (cl-find "pov" (alist-get 'missing health)
                         :key (lambda (entry) (alist-get 'key entry)) :test #'equal)))
      (should (equal '("Scene 2 - The Handover")
                     (mapcar (lambda (scene) (alist-get 'title scene)) (alist-get 'scenes pov))))
      (should (equal test-companion-health--handover
                     (alist-get 'id (car (alist-get 'scenes pov))))))))

(ert-deftest test-companion-health-outside-a-project-is-not-found ()
  (let* ((dir (file-name-as-directory (make-temp-file "test-companion-health-none-" t))))
    (unwind-protect
        (let ((answer (org-scribe-companion-health-get dir)))
          (should (eq :false (plist-get answer :ok)))
          (should (equal "not-found" (plist-get (plist-get answer :error) :code))))
      (delete-directory dir t))))

;;; The Emacs report is drawn from the same data

(ert-deftest test-companion-health-report-is-drawn-from-the-data ()
  (test-companion-health--with-project root "demo-novel"
    (with-current-buffer (test-companion-health--manuscript root)
      (org-scribe-project-health))
    (let ((report (with-current-buffer "*org-scribe-health*" (buffer-string)))
          (drawn (with-temp-buffer
                   (org-mode)
                   (org-scribe--health-render (org-scribe--health-data))
                   (org-table-map-tables #'org-table-align t)
                   (buffer-string))))
      ;; the same text, but for the time stamp
      (should (equal (replace-regexp-in-string "^#\\+DATE: .*$" "" report)
                     (replace-regexp-in-string "^#\\+DATE: .*$" "" drawn))))))

;;; Text

(ert-deftest test-companion-health-plain-takes-out-org-markup ()
  (should (equal "Plot points with content: 13 of 13."
                 (org-scribe-companion-health--plain "Plot points with content: *13 of 13*.")))
  (should (equal "not set (expected en_US) — run M-x org-scribe-update-dir-locals"
                 (org-scribe-companion-health--plain
                  "not set (expected en_US) — run =M-x org-scribe-update-dir-locals=")))
  (should (equal "Title: The Draft" (org-scribe-companion-health--plain "Title :: The Draft")))
  ;; slashes and asterisks that are not markup stay
  (should (equal "a/b/c and 2*3*4" (org-scribe-companion-health--plain "a/b/c and 2*3*4")))
  (should (null (org-scribe-companion-health--plain nil))))

(ert-deftest test-companion-health-without-link-keeps-the-other-items ()
  (let ((text "[[id:a][Ash]], [[id:dead][Nell, the Elder]], Plain name"))
    (should (equal "[[id:a][Ash]], Plain name"
                   (org-scribe-companion-health--without-link text "dead")))
    (should (equal "[[id:dead][Nell, the Elder]], Plain name"
                   (org-scribe-companion-health--without-link text "a")))
    (should (equal "" (org-scribe-companion-health--without-link "[[id:dead][N]]" "dead")))
    (should (equal "[[id:a][A]]" (org-scribe-companion-health--without-link
                                  "[[id:dead]], [[id:a][A]]" "dead")))
    ;; an ID that only starts the same is another ID
    (should (equal "[[id:dead-2][M]]" (org-scribe-companion-health--without-link
                                       "[[id:dead][N]], [[id:dead-2][M]]" "dead")))))

;;; remove-link

(ert-deftest test-companion-health-remove-link-drops-the-dead-link ()
  (test-companion-health--with-project root "demo-novel"
    (let ((buffer (test-companion-health--at-scene root test-companion-health--handover)))
      (with-current-buffer buffer
        (org-entry-put nil "Characters"
                       "[[id:char-alex-rivera-001][Alex]], [[id:gone-001][Nell]], [[id:char-sam-chen-001][Sam]]")
        (undo-boundary))
      (org-scribe-companion--clear-cache)
      ;; the report sees it, with the key the page sends back
      (let* ((health (alist-get 'health (test-companion-health--json root)))
             (broken (car (alist-get 'broken health))))
        (should (= 1 (length (alist-get 'broken health))))
        (should (equal "characters" (alist-get 'key broken)))
        (should (equal "gone-001" (alist-get 'target broken)))
        (should (equal test-companion-health--handover (alist-get 'id broken))))
      (let ((answer (test-companion-health--run root "remove-link"
                                                :id test-companion-health--handover
                                                :key "characters" :target "gone-001")))
        (should (eq t (plist-get answer :ok))))
      (with-current-buffer (test-companion-health--at-scene root test-companion-health--handover)
        (should (equal "[[id:char-alex-rivera-001][Alex]], [[id:char-sam-chen-001][Sam]]"
                       (org-entry-get nil "Characters"))))
      (should (null (alist-get 'broken (alist-get 'health (test-companion-health--json root)))))
      ;; one step of the page's undo
      (should (eq t (plist-get (test-companion-health--run root "undo") :ok)))
      (with-current-buffer (test-companion-health--at-scene root test-companion-health--handover)
        (should (string-match-p "gone-001" (org-entry-get nil "Characters")))))))

(ert-deftest test-companion-health-remove-link-refusals ()
  (test-companion-health--with-project root "demo-novel"
    (let ((code (lambda (&rest args)
                  (plist-get (plist-get (apply #'test-companion-health--run root "remove-link" args)
                                        :error)
                             :code))))
      ;; no such link in that field
      (should (equal "conflict" (funcall code :id test-companion-health--handover
                                         :key "characters" :target "gone-001")))
      ;; not a scene field
      (should (equal "invalid" (funcall code :id test-companion-health--handover
                                        :key "no-such-field" :target "x")))
      (should (equal "invalid" (funcall code :id test-companion-health--handover :key "characters")))
      (should (equal "not-found" (funcall code :id "no-such-id" :key "characters" :target "x"))))))

;;; show-health

(ert-deftest test-companion-health-show-health-opens-the-report ()
  (test-companion-health--with-project root "demo-novel"
    (should (eq t (plist-get (test-companion-health--run root "show-health") :ok)))
    (should (get-buffer "*org-scribe-health*"))
    (with-current-buffer "*org-scribe-health*"
      (should (string-match-p "Project Health Report" (buffer-string))))))

(provide 'test-companion-health)

;;; test-companion-health.el ends here
