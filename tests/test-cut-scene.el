;;; test-cut-scene.el --- Tests for the cut bin -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; `org-scribe-cut-scene' moves a scene into the notebook's Cut scenes
;; section, `org-scribe-restore-scene' puts it back, `org-scribe-list-cut-scenes'
;; reports on them.  These tests pin what must survive the move (ID, drawer,
;; body, sub-headings), what must not stay behind (the TODO keyword, which the
;; notebook does not declare), where the section goes, that nothing reads the
;; cut scene as part of the manuscript, and that restore lands at scene level.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'org-scribe)

(defconst test-cut--manuscript
  (concat "#+TODO: TOWRITE | FINISHED PURGE\n"
          "* Act One\n** Chapter One\n"
          "*** TOWRITE The Heist :ignore:\n"
          ":PROPERTIES:\n:ID: heist-id\n:PoV: Ana\n"
          ":Characters: [[id:ana-id][Ana]]\n:WORDCOUNT: 99\n:END:\n"
          "Ana opened the safe and found nothing at all.\n\n"
          "**** Aside\n:PROPERTIES:\n:ID: aside-id\n:END:\nA second thought.\n"
          "*** TOWRITE Second Scene :ignore:\n"
          ":PROPERTIES:\n:ID: second-id\n:WORDCOUNT: 3\n:END:\nSecond scene prose here.\n"
          "** Chapter Two\n"
          "*** TOWRITE Third Scene :ignore:\n"
          ":PROPERTIES:\n:ID: third-id\n:WORDCOUNT: 3\n:END:\nThird scene prose here.\n"))

(defconst test-cut--design
  (concat "* Cast\n** Characters\n*** Ana\n:PROPERTIES:\n:ID: ana-id\n:Role: Protagonist\n:END:\n"
          "* Revision\n** Revision log\n* Scratchpad\nIdeas.\n"))

(defmacro test-cut--with-project (&rest body)
  "Run BODY in a temp novel project.
Binds `root', `manuscript' and `design'; point is on \"The Heist\"."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "test-cut-" t)))
          (default-directory root)
          (org-id-locations-file (expand-file-name "ids" root))
          (org-id-locations nil)
          (org-id-track-globally t)
          (manuscript (expand-file-name "novel.org" root))
          (design (expand-file-name "design.org" root)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".org-scribe-project" root)
             (insert "# Type: novel\n# Language: en\n"))
           (with-temp-file manuscript (insert test-cut--manuscript))
           (with-temp-file design (insert test-cut--design))
           (org-scribe-project-type-cache-clear)
           (with-current-buffer (find-file-noselect manuscript)
             (goto-char (point-min))
             (re-search-forward "^\\*\\*\\* TOWRITE The Heist")
             ,@body))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b) (string-prefix-p root (buffer-file-name b)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory root t))))

(defun test-cut--file (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun test-cut--cut (&optional reason)
  "Cut the scene at point with REASON, silently."
  (cl-letf (((symbol-function 'message) #'ignore))
    (org-scribe-cut-scene reason)))

(defun test-cut--restore (entry chapter)
  (cl-letf (((symbol-function 'message) #'ignore))
    (org-scribe-cut--restore entry chapter)))

;;; Cutting

(ert-deftest test-cut-moves-the-scene-whole-into-the-revision-part ()
  (test-cut--with-project
    (let ((id (test-cut--cut "too slow")))
      (should (equal id "heist-id"))
      (let ((notebook (test-cut--file design))
            (ms (test-cut--file manuscript)))
        ;; gone from the manuscript, prose and sub-heading with it
        (should-not (string-match-p "The Heist\\|safe and found\\|Aside" ms))
        (should (string-match-p "Second Scene" ms))
        ;; in the Revision part, after its log, before the Scratchpad
        (should (string-match "^\\*\\* Cut scenes$" notebook))
        (should (< (string-match "^\\* Revision" notebook)
                   (string-match "^\\*\\* Cut scenes" notebook)
                   (string-match "^\\* Scratchpad" notebook)))
        ;; ID, drawer and body intact; the sub-heading keeps its own drawer
        (should (string-match-p "^\\*\\*\\* The Heist :ignore:$" notebook))
        (should (string-match-p ":ID: heist-id" notebook))
        (should (string-match-p ":PoV: Ana" notebook))
        (should (string-match-p ":Characters: \\[\\[id:ana-id\\]\\[Ana\\]\\]" notebook))
        (should (string-match-p "safe and found nothing at all" notebook))
        (should (string-match-p "^\\*\\*\\*\\* Aside$" notebook))
        (should (string-match-p ":ID: aside-id" notebook))
        (should (= 1 (with-temp-buffer
                       (insert notebook)
                       (how-many ":ID: heist-id" (point-min) (point-max)))))))))

(ert-deftest test-cut-records-date-state-chapter-and-reason ()
  (test-cut--with-project
    (test-cut--cut "too slow")
    (with-current-buffer (find-file-noselect design)
      (let ((entry (car (org-scribe-cut--collect))))
        (should (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" (plist-get entry :date)))
        (should (equal (plist-get entry :state) "TOWRITE"))
        (should (equal (plist-get entry :chapter) "Chapter One"))
        (should (equal (plist-get entry :reason) "too slow"))
        (should (equal (plist-get entry :title) "The Heist"))))))

(ert-deftest test-cut-leaves-no-todo-keyword-in-the-title ()
  "The notebook declares no TOWRITE, so the keyword would become title text."
  (test-cut--with-project
    (test-cut--cut "")
    (should-not (string-match-p "TOWRITE The Heist" (test-cut--file design)))
    (should (string-match-p "Cut-state: TOWRITE" (test-cut--file design)))))

(ert-deftest test-cut-an-empty-reason-writes-no-property ()
  (test-cut--with-project
    (test-cut--cut "  ")
    (should-not (string-match-p "Cut-reason" (test-cut--file design)))))

(ert-deftest test-cut-a-scene-without-an-id-gets-one ()
  (test-cut--with-project
    (goto-char (point-min))
    (re-search-forward "^\\*\\*\\* TOWRITE The Heist")
    (org-delete-property "ID")
    (let ((id (test-cut--cut "")))
      (should (stringp id))
      (should (string-match-p (concat ":ID: +" (regexp-quote id)) (test-cut--file design))))))

(ert-deftest test-cut-the-id-resolves-in-the-notebook ()
  (test-cut--with-project
    (test-cut--cut "")
    (should (equal (file-truename (car (org-id-find "heist-id")))
                   (file-truename design)))))

(ert-deftest test-cut-refuses-outside-the-manuscript ()
  (test-cut--with-project
    (with-current-buffer (find-file-noselect design)
      (goto-char (point-min))
      (should-error (org-scribe-cut-scene "x") :type 'user-error))))

(ert-deftest test-cut-refuses-a-heading-that-is-not-a-scene ()
  (test-cut--with-project
    (goto-char (point-min))
    (re-search-forward "^\\*\\* Chapter One")
    (should-error (org-scribe-cut-scene "x") :type 'user-error)
    (should (string-match-p "The Heist" (buffer-string)))))

(ert-deftest test-cut-saves-both-files-when-they-were-clean ()
  (test-cut--with-project
    (test-cut--cut "")
    (should-not (buffer-modified-p))
    (should-not (string-match-p "The Heist" (test-cut--file manuscript)))))

(ert-deftest test-cut-leaves-the-manuscript-unsaved-when-it-had-edits ()
  (test-cut--with-project
    (goto-char (point-max))
    (insert "\n")
    (test-cut--cut "")
    (should (buffer-modified-p))
    (should (string-match-p "The Heist" (test-cut--file manuscript)))))

;;; What reads the manuscript does not see a cut scene

(ert-deftest test-cut-links-of-a-cut-scene-hide-no-orphan ()
  (test-cut--with-project
    (let ((before (org-scribe--health-collect-referenced-ids manuscript)))
      (should (gethash "ana-id" before)))
    (test-cut--cut "")
    (let ((after (org-scribe--health-collect-referenced-ids manuscript)))
      (should-not (gethash "ana-id" after)))))

(ert-deftest test-cut-the-manuscript-word-total-drops ()
  (test-cut--with-project
    (let ((before (org-scribe--count-words-region (point-min) (point-max))))
      (test-cut--cut "")
      (should (< (org-scribe--count-words-region (point-min) (point-max)) before)))))

(ert-deftest test-cut-a-snapshot-link-still-resolves-to-the-scene ()
  (test-cut--with-project
    (cl-letf (((symbol-function 'message) #'ignore))
      (org-scribe-snapshot-scene))
    (test-cut--cut "")
    (let ((region (org-scribe-snapshot--scene-region "heist-id")))
      (should region)
      (should (equal (file-truename (buffer-file-name (car region)))
                     (file-truename design))))))

;;; Restoring

(ert-deftest test-cut-restore-lands-at-scene-level-under-the-chapter ()
  (test-cut--with-project
    (test-cut--cut "reason")
    (test-cut--restore (car (org-scribe-cut--collect)) "Chapter Two")
    (let ((ms (test-cut--file manuscript)))
      ;; the last scene of Chapter Two, at scene level, with its keyword back
      (should (string-match "^\\*\\* Chapter Two" ms))
      (should (< (string-match "^\\*\\*\\* TOWRITE Third Scene" ms)
                 (string-match "^\\*\\*\\* TOWRITE The Heist :ignore:$" ms)))
      (should (< (string-match "^\\*\\* Chapter Two" ms)
                 (string-match "^\\*\\*\\* TOWRITE The Heist" ms)))
      (should (string-match-p "^\\*\\*\\*\\* Aside$" ms))
      (should (string-match-p ":ID: heist-id" ms))
      (should-not (string-match-p "Cut-" ms)))
    ;; gone from the notebook, section kept
    (let ((notebook (test-cut--file design)))
      (should-not (string-match-p "heist-id\\|The Heist" notebook))
      (should (string-match-p "^\\*\\* Cut scenes$" notebook)))
    (should (equal (file-truename (car (org-id-find "heist-id")))
                   (file-truename manuscript)))))

(ert-deftest test-cut-restore-refreshes-the-word-count ()
  (test-cut--with-project
    (test-cut--cut "")
    (test-cut--restore (car (org-scribe-cut--collect)) "Chapter One")
    (with-current-buffer (find-file-noselect manuscript)
      (goto-char (point-min))
      (re-search-forward "^\\*\\*\\* TOWRITE The Heist")
      (let ((words (string-to-number (org-entry-get nil "WORDCOUNT"))))
        (should-not (= words 99))
        (should (> words 5))))))

(ert-deftest test-cut-restore-under-a-chapter-that-is-gone-goes-to-the-end ()
  (test-cut--with-project
    (test-cut--cut "")
    (test-cut--restore (car (org-scribe-cut--collect)) "No such chapter")
    (let ((ms (test-cut--file manuscript)))
      (should (< (string-match "^\\*\\*\\* TOWRITE Third Scene" ms)
                 (string-match "^\\*\\*\\* TOWRITE The Heist" ms))))))

(ert-deftest test-cut-restore-without-a-chapter-goes-to-the-end ()
  (test-cut--with-project
    (test-cut--cut "")
    (test-cut--restore (car (org-scribe-cut--collect)) nil)
    (let ((ms (test-cut--file manuscript)))
      (should (< (string-match "^\\*\\*\\* TOWRITE Third Scene" ms)
                 (string-match "^\\*\\*\\* TOWRITE The Heist" ms))))))

(ert-deftest test-cut-restore-drops-a-state-the-manuscript-does-not-declare ()
  (test-cut--with-project
    (test-cut--cut "")
    (let ((entry (plist-put (copy-sequence (car (org-scribe-cut--collect)))
                            :state "NOSUCH")))
      (test-cut--restore entry "Chapter One"))
    (let ((ms (test-cut--file manuscript)))
      (should-not (string-match-p "NOSUCH" ms))
      (should (string-match-p "^\\*\\*\\* The Heist :ignore:$" ms)))))

(ert-deftest test-cut-restore-and-cut-again-gives-one-entry ()
  (test-cut--with-project
    (test-cut--cut "")
    (test-cut--restore (car (org-scribe-cut--collect)) "Chapter One")
    (goto-char (point-min))
    (re-search-forward "^\\*\\*\\* TOWRITE The Heist")
    (test-cut--cut "again")
    (should (= 1 (length (org-scribe-cut--collect))))))

(ert-deftest test-cut-restore-asks-nothing-without-a-cut-scene ()
  (test-cut--with-project
    (should-error (org-scribe-restore-scene) :type 'user-error)))

;;; The report and deleting for good

(ert-deftest test-cut-an-unlinked-cut-scene-is-marked-for-pruning ()
  (test-cut--with-project
    (test-cut--cut "")
    (should (= 0 (org-scribe-cut--reference-count "heist-id")))
    (let ((row (cadr (car (org-scribe-cut-list--entries)))))
      (should (string-match-p (regexp-quote (org-scribe-msg 'cut-list-prune))
                              (aref row 3))))))

(ert-deftest test-cut-a-linked-cut-scene-is-not-marked-for-pruning ()
  "A snapshot's :SCENE: link counts as a reference."
  (test-cut--with-project
    (cl-letf (((symbol-function 'message) #'ignore))
      (org-scribe-snapshot-scene))
    (test-cut--cut "")
    (should (= 1 (org-scribe-cut--reference-count "heist-id")))
    (let ((row (cadr (car (org-scribe-cut-list--entries)))))
      (should (equal "1" (aref row 3))))))

(ert-deftest test-cut-list-refuses-when-nothing-is-cut ()
  (test-cut--with-project
    (should-error (org-scribe-list-cut-scenes) :type 'user-error)))

(ert-deftest test-cut-list-shows-title-words-and-reason ()
  (test-cut--with-project
    (test-cut--cut "too slow")
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
      (org-scribe-list-cut-scenes))
    (with-current-buffer (format "*%s*" (org-scribe-msg 'cut-list-title))
      (should (derived-mode-p 'org-scribe-cut-list-mode))
      (should (string-match-p "The Heist" (buffer-string)))
      (should (string-match-p "too slow" (buffer-string)))
      (kill-buffer))))

(ert-deftest test-cut-delete-for-good-removes-the-entry ()
  (test-cut--with-project
    (test-cut--cut "")
    (cl-letf (((symbol-function 'message) #'ignore))
      (org-scribe-cut--delete (car (org-scribe-cut--collect))))
    (should-not (org-scribe-cut--collect))
    (should-not (string-match-p "The Heist" (test-cut--file design)))
    (should-not (string-match-p "The Heist" (test-cut--file manuscript)))))

(ert-deftest test-cut-list-delete-asks-first ()
  (test-cut--with-project
    (test-cut--cut "")
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
              ((symbol-function 'message) #'ignore))
      (org-scribe-list-cut-scenes)
      (with-current-buffer (format "*%s*" (org-scribe-msg 'cut-list-title))
        (goto-char (point-min))
        (org-scribe-cut-list-delete)
        (kill-buffer)))
    (should (= 1 (length (org-scribe-cut--collect))))))

;;; Spanish project

(ert-deftest test-cut-spanish-project-uses-the-spanish-heading ()
  (let* ((root (file-name-as-directory (make-temp-file "test-cut-es-" t)))
         (default-directory root)
         (org-id-locations-file (expand-file-name "ids" root))
         (org-id-locations nil)
         (manuscript (expand-file-name "novela.org" root))
         (design (expand-file-name "diseno.org" root)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".org-scribe-project" root)
            (insert "# Type: novel\n# Language: es\n"))
          (with-temp-file manuscript
            (insert (replace-regexp-in-string "novel" "novela" test-cut--manuscript t t)))
          (with-temp-file design
            (insert "* Reparto\n* Revisión\n** Versiones\n* Cuaderno\n"))
          (org-scribe-project-type-cache-clear)
          (with-current-buffer (find-file-noselect manuscript)
            (goto-char (point-min))
            (re-search-forward "^\\*\\*\\* TOWRITE The Heist")
            (test-cut--cut "")
            (let ((notebook (test-cut--file design)))
              (should (string-match-p
                       (format "^\\*\\* %s$" (org-scribe-lang-heading 'cut-scenes 'es))
                       notebook))
              (should (< (string-match "^\\* Revisión" notebook)
                         (string-match (org-scribe-lang-heading 'cut-scenes 'es) notebook)
                         (string-match "^\\* Cuaderno" notebook))))
            (test-cut--restore (car (org-scribe-cut--collect)) "Chapter Two")
            (should (string-match-p "TOWRITE The Heist" (test-cut--file manuscript)))))
      (dolist (b (buffer-list))
        (when (and (buffer-file-name b) (string-prefix-p root (buffer-file-name b)))
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b)))
      (delete-directory root t))))

;;; Short story: no chapters

(ert-deftest test-cut-short-story-section-is-level-one-and-restore-goes-to-the-end ()
  (let* ((root (file-name-as-directory (make-temp-file "test-cut-ss-" t)))
         (default-directory root)
         (org-id-locations-file (expand-file-name "ids" root))
         (org-id-locations nil)
         (manuscript (expand-file-name "story.org" root))
         (design (expand-file-name "design.org" root)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".org-scribe-project" root)
            (insert "# Type: short-story\n# Language: en\n"))
          (with-temp-file manuscript
            (insert (concat "#+TODO: TOWRITE | FINISHED PURGE\n* Story\n"
                            "** TOWRITE One :ignore:\n:PROPERTIES:\n:ID: one-id\n:END:\nOne.\n"
                            "** TOWRITE Two :ignore:\n:PROPERTIES:\n:ID: two-id\n:END:\nTwo.\n")))
          (with-temp-file design (insert "* Characters\n"))
          (org-scribe-project-type-cache-clear)
          (with-current-buffer (find-file-noselect manuscript)
            (goto-char (point-min))
            (re-search-forward "^\\*\\* TOWRITE One")
            (test-cut--cut "")
            (should (string-match-p "^\\* Cut scenes$" (test-cut--file design)))
            (should (string-match-p "^\\*\\* One :ignore:$" (test-cut--file design)))
            (test-cut--restore (car (org-scribe-cut--collect)) nil)
            (let ((ms (test-cut--file manuscript)))
              (should (< (string-match "^\\*\\* TOWRITE Two" ms)
                         (string-match "^\\*\\* TOWRITE One" ms))))))
      (dolist (b (buffer-list))
        (when (and (buffer-file-name b) (string-prefix-p root (buffer-file-name b)))
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b)))
      (delete-directory root t))))

(provide 'test-cut-scene)

;;; test-cut-scene.el ends here
