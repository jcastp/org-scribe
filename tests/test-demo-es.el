;;; test-demo-es.el --- The Spanish demo projects mirror the English ones -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; `examples/demo-novela' and `examples/demo-cuento' are the Spanish
;; counterparts of `demo-novel' and `demo-short-story': the same story, the
;; same entities, the same scenes, as real projects in Spanish.  These tests
;; keep the pairs aligned and check the Spanish ones with the package's own
;; readers rather than by eye:
;;
;; - the entity getters find as many characters, settings, plot threads,
;;   objects and plot points as in the English demo, and as many scenes;
;; - every `id:' link in the Spanish demo points at an ID it defines;
;; - no ID is shared with an English demo (the org-id database is global, so
;;   two demos with the same ID would resolve to whichever loaded last);
;; - the project health report runs on a copy.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (dolist (dir '("core" "lang" "linking" "search" "counting" "capture"
                 "templates" "reporting" "planning" "companion"))
    (add-to-list 'load-path (expand-file-name dir parent-dir))))

(require 'org-scribe-config)
(require 'org-scribe-core)
(require 'org-scribe-character-links)
(require 'org-scribe-location-links)
(require 'org-scribe-plot-links)
(require 'org-scribe-object-links)
(require 'org-scribe-plot-point-links)
(require 'org-scribe-health)
(require 'org-scribe-companion)

(defconst test-demo-es--repo
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defconst test-demo-es--pairs
  '(("demo-novel" "novel.org" "design.org"
     "demo-novela" "novela.org" "diseno.org")
    ("demo-short-story" "story.org" "design.org"
     "demo-cuento" "cuento.org" "diseno.org"))
  "Each row: English dir, manuscript, notebook; Spanish dir, manuscript, notebook.")

(defun test-demo-es--file (dir name)
  (expand-file-name (concat "examples/" dir "/" name) test-demo-es--repo))

(defmacro test-demo-es--with-copy (dir var &rest body)
  "Run BODY with VAR bound to a temporary copy of examples/DIR."
  (declare (indent 2))
  `(let* ((tmp (file-name-as-directory (make-temp-file "test-demo-es-" t)))
          (,var (expand-file-name ,dir tmp))
          (org-id-locations-file (expand-file-name "ids" tmp))
          (make-backup-files nil)
          (create-lockfiles nil))
     (unwind-protect
         (progn
           (copy-directory (test-demo-es--file ,dir "") ,var nil t t)
           (org-scribe-project-type-cache-clear)
           ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (string-prefix-p tmp file)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (org-scribe-project-type-cache-clear)
       (delete-directory tmp t))))

(defun test-demo-es--counts (root manuscript)
  "Entity and scene counts of the project at ROOT, read by the package."
  (with-current-buffer (find-file-noselect (expand-file-name manuscript root))
    (let ((default-directory (file-name-as-directory root)))
      (org-scribe-project-type-cache-clear)
      (list :characters (length (org-scribe--get-all-characters))
            :settings (length (org-scribe--get-all-locations))
            :threads (length (org-scribe--get-all-plot-threads))
            :objects (length (org-scribe--get-all-objects))
            :plot-points (length (org-scribe--get-all-plot-points))
            :scenes (length (org-map-entries #'point (org-scribe-scene-match)))))))

(defun test-demo-es--ids-in (file regexp)
  (with-temp-buffer
    (insert-file-contents file)
    (let (ids)
      (while (re-search-forward regexp nil t)
        (push (match-string 1) ids))
      (nreverse ids))))

(defun test-demo-es--defined-ids (dir)
  "Every `:ID:' defined in the manuscript and notebook of demo DIR."
  (cl-loop for f in (directory-files (test-demo-es--file dir "") t "\\.org\\'")
           append (test-demo-es--ids-in f "^[ \t]*:ID:[ \t]+\\(\\S-+\\)")))

(ert-deftest test-demo-es-entity-and-scene-counts-match-english ()
  "The package reads as many entities and scenes in each Spanish demo."
  (dolist (row test-demo-es--pairs)
    (pcase-let ((`(,en ,en-ms ,_ ,es ,es-ms ,_) row))
      (let (english spanish)
        (test-demo-es--with-copy en root (setq english (test-demo-es--counts root en-ms)))
        (test-demo-es--with-copy es root (setq spanish (test-demo-es--counts root es-ms)))
        (should (> (plist-get spanish :characters) 0))
        (should (> (plist-get spanish :scenes) 0))
        (should (equal english spanish))))))

(ert-deftest test-demo-es-id-links-all-resolve ()
  "Every `id:' link in a Spanish demo points at an ID the demo defines."
  (dolist (dir '("demo-novela" "demo-cuento"))
    (let ((defined (test-demo-es--defined-ids dir)))
      (dolist (f (directory-files (test-demo-es--file dir "") t "\\.org\\'"))
        ;; The README writes `[[id:...][Nombre]]' as a syntax example.
        (dolist (id (remove "..." (test-demo-es--ids-in f "\\[\\[id:\\([^]]+\\)\\]")))
          (should (member id defined)))))))

(ert-deftest test-demo-es-ids-never-collide-with-english-demos ()
  "No ID is defined in both a Spanish and an English demo."
  (let ((english (append (test-demo-es--defined-ids "demo-novel")
                         (test-demo-es--defined-ids "demo-short-story")))
        (spanish (append (test-demo-es--defined-ids "demo-novela")
                         (test-demo-es--defined-ids "demo-cuento"))))
    (should spanish)
    (should-not (cl-intersection english spanish :test #'equal))
    (should (= (length spanish) (length (cl-remove-duplicates spanish :test #'equal))))))

(ert-deftest test-demo-es-project-files-declare-spanish ()
  "Each Spanish demo is a Spanish project, with the Spanish file names."
  (dolist (spec '(("demo-novela" "novela.org") ("demo-cuento" "cuento.org")))
    (dolist (name (list ".org-scribe-project" (cadr spec) "diseno.org" "plan.org"))
      (should (file-exists-p (test-demo-es--file (car spec) name))))
    (with-temp-buffer
      (insert-file-contents (test-demo-es--file (car spec) ".org-scribe-project"))
      (should (re-search-forward "^# Language: es$" nil t)))))

(ert-deftest test-demo-es-health-report-runs ()
  "The health report runs on each Spanish demo and names the right story."
  (require 'org-scribe-planner)
  (dolist (spec '(("demo-novela" "novela.org" "La rutina matinal")
                  ("demo-cuento" "cuento.org" "El último mensaje")))
    (test-demo-es--with-copy (car spec) root
      (let ((org-scribe-planner--current-plan nil)
            (org-scribe-planner--current-plan-file nil))
        (when (get-buffer "*org-scribe-health*")
          (kill-buffer "*org-scribe-health*"))
        (with-current-buffer (find-file-noselect (expand-file-name (nth 1 spec) root))
          (org-scribe-mode 1)
          (org-scribe-project-health))
        (with-current-buffer "*org-scribe-health*"
          (should (string-match-p (regexp-quote (nth 2 spec)) (buffer-string))))))))

(ert-deftest test-demo-es-companion-snapshot-reads-the-project ()
  "The companion's snapshot of each Spanish demo carries its own title."
  (dolist (spec '(("demo-novela" "La rutina matinal")
                  ("demo-cuento" "El último mensaje")))
    (test-demo-es--with-copy (car spec) root
      (org-scribe-companion--clear-cache)
      (let ((snapshot (org-scribe-companion--snapshot root)))
        (should (equal (plist-get (plist-get snapshot :project) :title)
                       (cadr spec)))
        (should (> (length (plist-get snapshot :acts)) 0)))
      (org-scribe-companion--clear-cache))))

(provide 'test-demo-es)
;;; test-demo-es.el ends here
