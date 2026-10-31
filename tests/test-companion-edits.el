;;; test-companion-edits.el --- Tests for the companion's edit markers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for N2: the scenes' own edit markers in the snapshot
;; (`org-scribe-companion--edit-markers'), the Edits tab's index
;; (`org-scribe-companion-edits-get', GET /api/edits) and `goto-marker'.

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
(require 'org-scribe-search)
(require 'org-scribe-companion)

(defconst test-companion-edits--repo
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the org-scribe checkout.")

(defconst test-companion-edits--alarm "a6aaa600-35b3-4193-9510-a4bb2ef0000f"
  "ID of \"Scene 1 - The Alarm\" in the demo novel (one NOTE, one EDIT).")

(defmacro test-companion-edits--with-project (root-var demo &rest body)
  "Run BODY with a copy of the example DEMO at ROOT-VAR (not serving)."
  (declare (indent 2))
  `(let* ((,root-var (file-name-as-directory (make-temp-file "test-companion-edits-" t)))
          (default-directory ,root-var)
          (org-id-locations-file (expand-file-name "ids" ,root-var))
          (make-backup-files nil)
          (create-lockfiles nil)
          (org-scribe-companion-raise-frame nil))
     (unwind-protect
         (save-window-excursion
           (copy-directory (expand-file-name (concat "examples/" ,demo) test-companion-edits--repo)
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

(defun test-companion-edits--scenes (root)
  "Every scene plist of the snapshot of ROOT."
  (let (scenes)
    (seq-doseq (act (plist-get (org-scribe-companion--snapshot root) :acts))
      (seq-doseq (chapter (plist-get act :chapters))
        (seq-doseq (scene (plist-get chapter :scenes))
          (push scene scenes))))
    (nreverse scenes)))

(defun test-companion-edits--scene (root id)
  (cl-find id (test-companion-edits--scenes root)
           :key (lambda (scene) (plist-get scene :id)) :test #'equal))

(defun test-companion-edits--line-of (file regexp)
  "The 1-based line of the first match of REGEXP in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward regexp)
    (line-number-at-pos)))

;;; The snapshot

(ert-deftest test-companion-edits-scenes-carry-their-markers ()
  (test-companion-edits--with-project root "demo-novel"
    (let* ((file (expand-file-name "novel.org" root))
           (markers (append (plist-get (test-companion-edits--scene root test-companion-edits--alarm)
                                       :edits)
                            nil)))
      (should (equal '("NOTE" "EDIT") (mapcar (lambda (m) (plist-get m :type)) markers)))
      (should (equal '(:null "prose") (mapcar (lambda (m) (plist-get m :category)) markers)))
      (should (string-prefix-p "Three paragraphs" (plist-get (nth 1 markers) :text)))
      ;; the line is the file's, as F8 6 gives it
      (should (= (test-companion-edits--line-of file "^\\*EDIT\\*: prose - Three paragraphs")
                 (plist-get (nth 1 markers) :line)))
      ;; the same markers, scene by scene, as the edit index finds in the manuscript
      (should (= (length (seq-filter (lambda (m) (file-equal-p (plist-get m :file) file))
                                     (org-scribe--edits-collect root)))
                 (apply #'+ (mapcar (lambda (scene) (length (plist-get scene :edits)))
                                    (test-companion-edits--scenes root)))))
      ;; every scene has the key, an empty vector when it has none
      (should (cl-every (lambda (scene) (vectorp (plist-get scene :edits)))
                        (test-companion-edits--scenes root))))))

(ert-deftest test-companion-edits-unsaved-marker-is-seen-and-plain-text-is-not ()
  (test-companion-edits--with-project root "demo-novel"
    (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
      (goto-char (point-min))
      (re-search-forward (concat "^:ID: +" test-companion-edits--alarm))
      (org-end-of-meta-data t)
      ;; a marker outside a comment block is prose, not a marker
      (insert "*EDIT*: plot - not in a block\n"
              "#+begin_comment\n*EDIT*: scene - unsaved one\n#+end_comment\n"))
    (let ((markers (append (plist-get (test-companion-edits--scene root test-companion-edits--alarm)
                                      :edits)
                           nil)))
      (should (= 3 (length markers)))
      (should (equal "unsaved one" (plist-get (car markers) :text)))
      (should (equal "scene" (plist-get (car markers) :category))))))

(ert-deftest test-companion-edits-categories-in-the-project-language ()
  (test-companion-edits--with-project root "demo-novela"
    (let ((categories (append (plist-get (org-scribe-companion--snapshot root) :editCategories) nil)))
      (should (equal (mapcar (lambda (c) (plist-get c :key)) categories)
                     org-scribe-edit-categories))
      (should (equal "trama" (plist-get (car categories) :name))))))

(ert-deftest test-companion-edits-categories-are-part-of-the-settings-revision ()
  (test-companion-edits--with-project root "demo-novel"
    (let ((before (org-scribe-companion--settings-revision))
          (org-scribe-edit-categories '("plot" "prose")))
      (should-not (equal before (org-scribe-companion--settings-revision))))))

;;; The index

(ert-deftest test-companion-edits-index-places-manuscript-markers ()
  (test-companion-edits--with-project root "demo-novel"
    ;; a marker in the design notebook too
    (with-current-buffer (find-file-noselect (expand-file-name "design.org" root))
      (goto-char (point-max))
      (insert "\n#+begin_comment\n*NOTE*: check the calendar\n#+end_comment\n"))
    (let* ((answer (org-scribe-companion-edits-get root))
           (markers (append (plist-get answer :markers) nil))
           (alarm (seq-filter (lambda (m) (equal test-companion-edits--alarm (plist-get m :sceneId)))
                              markers))
           (design (seq-filter (lambda (m) (equal "design.org" (plist-get m :file))) markers)))
      (should (eq t (plist-get answer :ok)))
      (should (equal (org-scribe-companion--revision-string root) (plist-get answer :revision)))
      (should (= (length (org-scribe--edits-collect root)) (length markers)))
      (should (= 2 (length alarm)))
      (should (equal "Chapter 1 - Three Alarms" (plist-get (car alarm) :chapterTitle)))
      (should (eq t (plist-get (car alarm) :manuscript)))
      (should (= 1 (length design)))
      (should (eq :false (plist-get (car design) :manuscript)))
      (should (eq :null (plist-get (car design) :sceneId)))
      ;; JSON-ready
      (should (stringp (json-serialize answer))))))

(ert-deftest test-companion-edits-short-story-names-no-chapter ()
  (test-companion-edits--with-project root "demo-short-story"
    ;; the demo has none: one in the first scene
    (with-current-buffer (find-file-noselect (expand-file-name "story.org" root))
      (goto-char (point-min))
      (re-search-forward "^\\*\\* FINISHED Opening")
      (org-end-of-meta-data t)
      (insert "#+begin_comment\n*EDIT*: prose - tighten\n#+end_comment\n"))
    (let ((placed (seq-remove (lambda (m) (eq :null (plist-get m :sceneId)))
                              (append (plist-get (org-scribe-companion-edits-get root) :markers)
                                      nil))))
      (should placed)
      (dolist (marker placed)
        (should (eq :null (plist-get marker :chapterTitle)))))))

;;; goto-marker

(ert-deftest test-companion-edits-goto-marker-lands-on-the-line ()
  (test-companion-edits--with-project root "demo-novel"
    (let* ((file (expand-file-name "novel.org" root))
           (line (test-companion-edits--line-of file "^\\*EDIT\\*: prose - Three paragraphs")))
      ;; narrowed elsewhere: it widens
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-max))
        (org-back-to-heading t)
        (org-narrow-to-subtree))
      ;; no :file is the manuscript
      (should (eq t (plist-get (org-scribe-companion-commands-run
                                "goto-marker" (list :line line) root)
                               :ok)))
      (should (equal file (buffer-file-name)))
      (should (= line (line-number-at-pos)))
      (should (looking-at-p "\\*EDIT\\*: prose")))))

(ert-deftest test-companion-edits-goto-marker-refusals ()
  (test-companion-edits--with-project root "demo-novel"
    (let ((code (lambda (&rest args)
                  (plist-get (plist-get (org-scribe-companion-commands-run "goto-marker" args root)
                                        :error)
                             :code))))
      (should (equal "not-found" (funcall code :file "../outside.org" :line 1)))
      (should (equal "not-found" (funcall code :file "README.md" :line 1)))
      (should (equal "not-found" (funcall code :file "no-such.org" :line 1)))
      (should (equal "invalid" (funcall code :file "novel.org")))
      (should (equal "invalid" (funcall code :file "novel.org" :line 0))))))

(provide 'test-companion-edits)

;;; test-companion-edits.el ends here
