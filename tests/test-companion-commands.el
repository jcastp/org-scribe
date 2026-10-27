;;; test-companion-commands.el --- Tests for the companion's commands -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for `companion/org-scribe-companion-commands.el' (`goto', `save',
;; `move-node', `set-title', `set-todo', `set-field', `create-scene'),
;; follow mode (`focus' events) and the web files' string keys.

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

(defconst test-companion-commands--repo
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the org-scribe checkout.")

(defconst test-companion-commands--scene-1 "a6aaa600-35b3-4193-9510-a4bb2ef0000f"
  "ID of \"Scene 1 - The Alarm\" in the demo novel.")
(defconst test-companion-commands--scene-2 "1ba80e22-da5c-4457-ae43-3d577c5d3529"
  "ID of \"Scene 2 - Coffee Shop Encounter\".")
(defconst test-companion-commands--alex "char-alex-rivera-001"
  "ID of a character in the demo design notebook.")

(defmacro test-companion-commands--with-project (root-var &rest body)
  "Run BODY with a copy of the demo novel at ROOT-VAR (not serving)."
  (declare (indent 1))
  `(let* ((,root-var (file-name-as-directory (make-temp-file "test-companion-cmd-" t)))
          (default-directory ,root-var)
          (org-id-locations-file (expand-file-name "ids" ,root-var))
          (make-backup-files nil)
          (create-lockfiles nil)
          (org-scribe-companion-raise-frame nil))
     (unwind-protect
         (save-window-excursion
           (copy-directory (expand-file-name "examples/demo-novel" test-companion-commands--repo)
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

(defun test-companion-commands--run (root op &rest args)
  (org-scribe-companion-commands-run op args root))

;;; goto

(ert-deftest test-companion-commands-goto-shows-the-heading ()
  (test-companion-commands--with-project root
    (let ((answer (test-companion-commands--run
                   root "goto" :id test-companion-commands--scene-2)))
      (should (eq t (plist-get answer :ok)))
      (should (stringp (plist-get answer :revision)))
      (should (equal "novel.org" (file-name-nondirectory (buffer-file-name))))
      (should (org-at-heading-p))
      (should (equal "Scene 2 - Coffee Shop Encounter" (org-get-heading t t t t)))
      (should (eq (current-buffer) (window-buffer (selected-window)))))))

(ert-deftest test-companion-commands-goto-widens-a-narrowed-buffer ()
  (test-companion-commands--with-project root
    (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
      (goto-char (point-min))
      (re-search-forward "^\\*\\*\\* FINISHED Scene 1 - The Alarm")
      (org-narrow-to-subtree))
    (let ((answer (test-companion-commands--run
                   root "goto" :id test-companion-commands--scene-2)))
      (should (eq t (plist-get answer :ok)))
      (should (equal "Scene 2 - Coffee Shop Encounter" (org-get-heading t t t t))))))

(ert-deftest test-companion-commands-goto-reveals-a-folded-heading ()
  (test-companion-commands--with-project root
    (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
      (org-overview))
    (test-companion-commands--run root "goto" :id test-companion-commands--scene-1)
    (should-not (invisible-p (point)))))

(ert-deftest test-companion-commands-goto-finds-design-entities ()
  (test-companion-commands--with-project root
    (let ((answer (test-companion-commands--run
                   root "goto" :id test-companion-commands--alex)))
      (should (eq t (plist-get answer :ok)))
      (should (equal "design.org" (file-name-nondirectory (buffer-file-name))))
      (should (equal "Alex Rivera" (org-get-heading t t t t))))))

(ert-deftest test-companion-commands-goto-unknown-id-is-not-found ()
  (test-companion-commands--with-project root
    (let ((answer (test-companion-commands--run root "goto" :id "no-such-id")))
      (should (eq :false (plist-get answer :ok)))
      (should (equal "not-found" (plist-get (plist-get answer :error) :code)))
      (should (string-match-p "no-such-id" (plist-get (plist-get answer :error) :message))))))

(ert-deftest test-companion-commands-goto-ignores-ids-outside-the-project ()
  "An ID that exists in another file is not found."
  (test-companion-commands--with-project root
    (let ((other (expand-file-name "other.org" root)))
      (with-temp-file other (insert "* Elsewhere\n:PROPERTIES:\n:ID: elsewhere-id\n:END:\n"))
      (find-file-noselect other)
      (let ((answer (test-companion-commands--run root "goto" :id "elsewhere-id")))
        (should (equal "not-found" (plist-get (plist-get answer :error) :code)))))))

(ert-deftest test-companion-commands-goto-needs-an-id ()
  (test-companion-commands--with-project root
    (dolist (args '(nil (:id "") (:id 5) (:other "x")))
      (let ((answer (org-scribe-companion-commands-run "goto" args root)))
        (should (equal "invalid" (plist-get (plist-get answer :error) :code)))))))

(ert-deftest test-companion-commands-goto-does-not-modify-the-buffer ()
  (test-companion-commands--with-project root
    (test-companion-commands--run root "goto" :id test-companion-commands--scene-1)
    (should-not (buffer-modified-p))))

;;; save

(ert-deftest test-companion-commands-save-writes-modified-project-buffers ()
  (test-companion-commands--with-project root
    (let ((manuscript (find-file-noselect (expand-file-name "novel.org" root)))
          (design (find-file-noselect (expand-file-name "design.org" root))))
      (with-current-buffer manuscript (goto-char (point-max)) (insert "\nA saved line.\n"))
      (with-current-buffer design (goto-char (point-max)) (insert "\nA design line.\n"))
      (let ((answer (test-companion-commands--run root "save")))
        (should (eq t (plist-get answer :ok))))
      (should-not (buffer-modified-p manuscript))
      (should-not (buffer-modified-p design))
      (should (string-match-p "A saved line\\."
                              (with-temp-buffer
                                (insert-file-contents (expand-file-name "novel.org" root))
                                (buffer-string)))))))

(ert-deftest test-companion-commands-save-reply-carries-the-new-revision ()
  (test-companion-commands--with-project root
    (let ((buffer (find-file-noselect (expand-file-name "novel.org" root))))
      (with-current-buffer buffer (goto-char (point-max)) (insert "x"))
      (let ((answer (test-companion-commands--run root "save")))
        (should (equal (plist-get answer :revision)
                       (org-scribe-companion--revision-string root)))
        (should-not (string-match-p "\\*" (plist-get answer :revision)))))))

(ert-deftest test-companion-commands-nothing-is-saved-without-save ()
  (test-companion-commands--with-project root
    (let ((buffer (find-file-noselect (expand-file-name "novel.org" root))))
      (with-current-buffer buffer (goto-char (point-max)) (insert "unsaved"))
      (test-companion-commands--run root "goto" :id test-companion-commands--scene-1)
      (should (buffer-modified-p buffer))
      (should-not (string-match-p "unsaved"
                                  (with-temp-buffer
                                    (insert-file-contents (expand-file-name "novel.org" root))
                                    (buffer-string)))))))

;;; Editing commands (Phase 4)

(defun test-companion-commands--text (root file)
  "Return the text of FILE's buffer in ROOT (modified or not)."
  (with-current-buffer (find-file-noselect (expand-file-name file root))
    (org-with-wide-buffer (buffer-string))))

(defun test-companion-commands--disk (root file)
  "Return the text of FILE on disk in ROOT."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file root))
    (buffer-string)))

(defun test-companion-commands--id (root title)
  "Return the ID of the manuscript heading titled TITLE in ROOT."
  (cl-block nil
    (let ((snapshot (org-scribe-companion--snapshot root)))
      (seq-doseq (act (plist-get snapshot :acts))
        (when (equal (plist-get act :title) title) (cl-return (plist-get act :id)))
        (seq-doseq (chapter (plist-get act :chapters))
          (when (equal (plist-get chapter :title) title) (cl-return (plist-get chapter :id)))
          (seq-doseq (scene (plist-get chapter :scenes))
            (when (equal (plist-get scene :title) title) (cl-return (plist-get scene :id)))))))))

(defun test-companion-commands--order (root)
  "Return the manuscript as a list: (ACT (CHAPTER SCENE...)...), by title."
  (mapcar (lambda (act)
            (cons (plist-get act :title)
                  (mapcar (lambda (chapter)
                            (cons (plist-get chapter :title)
                                  (mapcar (lambda (scene) (plist-get scene :title))
                                          (plist-get chapter :scenes))))
                          (plist-get act :chapters))))
          (plist-get (org-scribe-companion--snapshot root) :acts)))

(defun test-companion-commands--chapter-scenes (root chapter-title)
  "Return the scene titles of CHAPTER-TITLE in ROOT, in order."
  (cl-some (lambda (act)
             (cdr (assoc chapter-title (cdr act))))
           (test-companion-commands--order root)))

(defun test-companion-commands--subtree (root title)
  "Return the text of the subtree whose heading text is TITLE, as in the buffer."
  (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
    (org-with-wide-buffer
     (goto-char (point-min))
     (re-search-forward (format "^\\*+ \\(?:[A-Z]+ \\)?\\(?:\\[#[A-Z]\\] \\)?%s\\b" (regexp-quote title)))
     (org-back-to-heading t)
     (buffer-substring (point) (progn (org-end-of-subtree t t) (point))))))

(defun test-companion-commands--ok-p (answer)
  (eq t (plist-get answer :ok)))

(defun test-companion-commands--code (answer)
  (plist-get (plist-get answer :error) :code))

(defun test-companion-commands--undo-once (root)
  "Undo one step in the manuscript buffer of ROOT, as C-/ would."
  (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
    ;; what `undo' does for one step; the command itself needs a live command loop
    (undo-boundary)
    (undo-start)
    (undo-more 1)
    (undo-more 1)))

;;; move-node

(ert-deftest test-companion-commands-move-within-a-chapter ()
  (test-companion-commands--with-project root
    (let ((before (test-companion-commands--subtree root "Scene 2 - Coffee Shop Encounter")))
      (should (test-companion-commands--ok-p
               (test-companion-commands--run
                root "move-node" :id test-companion-commands--scene-2
                :parent (test-companion-commands--id root "Chapter 1 - Three Alarms")
                :after nil)))
      (should (equal '("Scene 2 - Coffee Shop Encounter" "Scene 1 - The Alarm")
                     (test-companion-commands--chapter-scenes
                      root "Chapter 1 - Three Alarms")))
      (should (equal before (test-companion-commands--subtree
                             root "Scene 2 - Coffee Shop Encounter"))))))

(ert-deftest test-companion-commands-move-to-another-chapter-keeps-the-subtree-intact ()
  (test-companion-commands--with-project root
    (let ((before (test-companion-commands--subtree root "Scene 1 - The Alarm"))
          (original (test-companion-commands--text root "novel.org")))
      (should (test-companion-commands--ok-p
               (test-companion-commands--run
                root "move-node" :id test-companion-commands--scene-1
                :parent (test-companion-commands--id root "Chapter 2 - What It Costs")
                :after (test-companion-commands--id root "Scene 1 - The Decision"))))
      (should (equal '("Scene 2 - Coffee Shop Encounter")
                     (test-companion-commands--chapter-scenes
                      root "Chapter 1 - Three Alarms")))
      (should (equal '("Scene 1 - The Decision" "Scene 1 - The Alarm"
                       "Scene 2 - The Handover" "Scene 3 - Terms")
                     (test-companion-commands--chapter-scenes
                      root "Chapter 2 - What It Costs")))
      ;; drawer, ID, tags and body are exactly as they were
      (should (equal before (test-companion-commands--subtree root "Scene 1 - The Alarm")))
      (should (string-match-p ":ID:       a6aaa600" before))
      ;; nothing else changed: same multiset of lines
      (should (equal (sort (split-string original "\n") #'string<)
                     (sort (split-string (test-companion-commands--text root "novel.org") "\n")
                           #'string<))))))

(ert-deftest test-companion-commands-move-to-first-position-of-another-chapter ()
  (test-companion-commands--with-project root
    (test-companion-commands--run
     root "move-node" :id test-companion-commands--scene-1
     :parent (test-companion-commands--id root "Chapter 5 - Six O'Clock") :after nil)
    (should (equal '("Scene 1 - The Alarm" "Scene 1 - The Compliment")
                   (test-companion-commands--chapter-scenes root "Chapter 5 - Six O'Clock")))))

(ert-deftest test-companion-commands-move-into-an-empty-chapter ()
  (test-companion-commands--with-project root
    ;; empty Chapter 2 of its scenes by moving them away, then back in
    (let ((chapter (test-companion-commands--id root "Chapter 2 - What It Costs")))
      (dolist (title '("Scene 1 - The Decision" "Scene 2 - The Handover" "Scene 3 - Terms"))
        (test-companion-commands--run
         root "move-node" :id (test-companion-commands--id root title)
         :parent (test-companion-commands--id root "Chapter 3 - Day One") :after nil))
      (should-not (test-companion-commands--chapter-scenes root "Chapter 2 - What It Costs"))
      (should (test-companion-commands--ok-p
               (test-companion-commands--run
                root "move-node" :id test-companion-commands--scene-1
                :parent chapter :after nil)))
      (should (equal '("Scene 1 - The Alarm")
                     (test-companion-commands--chapter-scenes
                      root "Chapter 2 - What It Costs"))))))

(ert-deftest test-companion-commands-move-after-the-last-scene-of-the-last-chapter ()
  (test-companion-commands--with-project root
    (should (test-companion-commands--ok-p
             (test-companion-commands--run
              root "move-node" :id test-companion-commands--scene-1
              :parent (test-companion-commands--id root "Chapter 8 - The New Normal")
              :after (test-companion-commands--id root "Scene 2 - Full Circle"))))
    (should (equal '("Scene 1 - Eleven Minutes" "Scene 2 - Full Circle" "Scene 1 - The Alarm")
                   (test-companion-commands--chapter-scenes
                    root "Chapter 8 - The New Normal")))
    (should (string-suffix-p "\n" (test-companion-commands--text root "novel.org")))))

(ert-deftest test-companion-commands-move-a-chapter-to-another-act ()
  (test-companion-commands--with-project root
    (should (test-companion-commands--ok-p
             (test-companion-commands--run
              root "move-node" :id (test-companion-commands--id root "Chapter 1 - Three Alarms")
              :parent (test-companion-commands--id root "Act II")
              :after (test-companion-commands--id root "Chapter 3 - Day One"))))
    (let ((order (test-companion-commands--order root)))
      (should (equal '("Chapter 2 - What It Costs")
                     (mapcar #'car (cdr (assoc "Act I" order)))))
      (should (equal '("Chapter 3 - Day One" "Chapter 1 - Three Alarms" "Chapter 4 - Adjusting"
                       "Chapter 5 - Six O'Clock" "Chapter 6 - Thursday")
                     (mapcar #'car (cdr (assoc "Act II" order))))))))

(ert-deftest test-companion-commands-move-an-act ()
  (test-companion-commands--with-project root
    (should (test-companion-commands--ok-p
             (test-companion-commands--run
              root "move-node" :id (test-companion-commands--id root "Act III") :after nil)))
    (should (equal '("Act III" "Act I" "Act II") (mapcar #'car (test-companion-commands--order root))))
    ;; the apparatus (noexport, level 1) still comes first in the buffer
    (should (< (string-search "* Project Goals" (test-companion-commands--text root "novel.org"))
               (string-search "* Act III" (test-companion-commands--text root "novel.org"))))))

(ert-deftest test-companion-commands-invalid-moves-leave-the-buffer-unchanged ()
  (test-companion-commands--with-project root
    (let* ((original (test-companion-commands--text root "novel.org"))
           (act (test-companion-commands--id root "Act II"))
           (chapter (test-companion-commands--id root "Chapter 1 - Three Alarms")))
      (dolist (case (list
                     ;; a scene under an act
                     (list :id test-companion-commands--scene-1 :parent act :after nil)
                     ;; a scene to the top level
                     (list :id test-companion-commands--scene-1 :after nil)
                     ;; a chapter under a chapter
                     (list :id chapter :parent chapter :after nil)
                     ;; an act under an act
                     (list :id act :parent act :after nil)
                     ;; a chapter to the top level
                     (list :id chapter :after nil)
                     ;; after itself
                     (list :id test-companion-commands--scene-1 :parent chapter
                           :after test-companion-commands--scene-1)))
        (let ((answer (apply #'test-companion-commands--run root "move-node" case)))
          (should (equal "invalid" (test-companion-commands--code answer)))))
      (should (equal original (test-companion-commands--text root "novel.org")))
      (should-not (buffer-modified-p (find-file-noselect (expand-file-name "novel.org" root)))))))

(ert-deftest test-companion-commands-moves-into-or-out-of-apparatus-are-read-only ()
  (test-companion-commands--with-project root
    (let ((original (test-companion-commands--text root "novel.org"))
          (apparatus "a6dffc06-711f-431e-bc46-6aad18e9af06")   ; Writing Sessions
          (apparatus-act "9cb521fd-2eb4-47bc-bc41-3ed726d298dc")) ; Project Goals
      (dolist (case (list
                     (list :id test-companion-commands--scene-1 :parent apparatus :after nil)
                     (list :id apparatus :parent apparatus-act :after nil)
                     (list :id apparatus-act :after nil)
                     ;; into the design notebook
                     (list :id test-companion-commands--alex :after nil)))
        (let ((answer (apply #'test-companion-commands--run root "move-node" case)))
          (should (equal "read-only" (test-companion-commands--code answer)))))
      (should (equal original (test-companion-commands--text root "novel.org"))))))

(ert-deftest test-companion-commands-move-with-a-stale-after-is-a-conflict ()
  (test-companion-commands--with-project root
    (let ((original (test-companion-commands--text root "novel.org"))
          (chapter-2 (test-companion-commands--id root "Chapter 2 - What It Costs")))
      ;; the scene it should follow is in another chapter
      (should (equal "conflict"
                     (test-companion-commands--code
                      (test-companion-commands--run
                       root "move-node" :id test-companion-commands--scene-1 :parent chapter-2
                       :after test-companion-commands--scene-2))))
      ;; the scene it should follow does not exist any more
      (should (equal "conflict"
                     (test-companion-commands--code
                      (test-companion-commands--run
                       root "move-node" :id test-companion-commands--scene-1 :parent chapter-2
                       :after "deleted-in-emacs"))))
      ;; a chapter is not a scene to follow
      (should (equal "conflict"
                     (test-companion-commands--code
                      (test-companion-commands--run
                       root "move-node" :id test-companion-commands--scene-1 :parent chapter-2
                       :after chapter-2))))
      (should (equal original (test-companion-commands--text root "novel.org"))))))

(ert-deftest test-companion-commands-move-with-unknown-ids-is-not-found ()
  (test-companion-commands--with-project root
    (should (equal "not-found"
                   (test-companion-commands--code
                    (test-companion-commands--run root "move-node" :id "nope" :after nil))))
    (should (equal "not-found"
                   (test-companion-commands--code
                    (test-companion-commands--run
                     root "move-node" :id test-companion-commands--scene-1
                     :parent "nope" :after nil))))
    (should (equal "invalid"
                   (test-companion-commands--code
                    (test-companion-commands--run root "move-node" :after nil))))))

(ert-deftest test-companion-commands-one-undo-restores-a-move-exactly ()
  (test-companion-commands--with-project root
    (let ((original (test-companion-commands--text root "novel.org")))
      (test-companion-commands--run
       root "move-node" :id test-companion-commands--scene-1
       :parent (test-companion-commands--id root "Chapter 8 - The New Normal")
       :after nil)
      (should-not (equal original (test-companion-commands--text root "novel.org")))
      (test-companion-commands--undo-once root)
      (should (equal original (test-companion-commands--text root "novel.org"))))))

(ert-deftest test-companion-commands-move-works-in-a-narrowed-buffer-and-keeps-the-narrowing ()
  (test-companion-commands--with-project root
    (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
      (goto-char (point-min))
      (re-search-forward "^\\*\\*\\* FINISHED Scene 1 - The Alarm")
      (org-narrow-to-subtree)
      (let ((narrowed (cons (point-min) (point-max))))
        (should (test-companion-commands--ok-p
                 (test-companion-commands--run
                  root "move-node" :id test-companion-commands--scene-2
                  :parent (test-companion-commands--id root "Chapter 1 - Three Alarms")
                  :after nil)))
        (should (buffer-narrowed-p))
        (ignore narrowed)))
    (should (equal '("Scene 2 - Coffee Shop Encounter" "Scene 1 - The Alarm")
                   (test-companion-commands--chapter-scenes
                    root "Chapter 1 - Three Alarms")))))

(ert-deftest test-companion-commands-moving-to-where-it-already-is-changes-nothing ()
  (test-companion-commands--with-project root
    (let ((buffer (find-file-noselect (expand-file-name "novel.org" root)))
          (original (test-companion-commands--text root "novel.org")))
      (should (test-companion-commands--ok-p
               (test-companion-commands--run
                root "move-node" :id test-companion-commands--scene-1
                :parent (test-companion-commands--id root "Chapter 1 - Three Alarms")
                :after nil)))
      (should (test-companion-commands--ok-p
               (test-companion-commands--run
                root "move-node" :id test-companion-commands--scene-2
                :parent (test-companion-commands--id root "Chapter 1 - Three Alarms")
                :after test-companion-commands--scene-1)))
      (should (equal original (test-companion-commands--text root "novel.org")))
      (should-not (buffer-modified-p buffer)))))

(ert-deftest test-companion-commands-move-saves-nothing-unless-auto-save-is-on ()
  (test-companion-commands--with-project root
    (let ((disk (test-companion-commands--disk root "novel.org")))
      (test-companion-commands--run
       root "move-node" :id test-companion-commands--scene-1
       :parent (test-companion-commands--id root "Chapter 8 - The New Normal") :after nil)
      (should (equal disk (test-companion-commands--disk root "novel.org")))
      (should (buffer-modified-p (find-file-noselect (expand-file-name "novel.org" root))))
      (let ((org-scribe-companion-auto-save t))
        (test-companion-commands--run
         root "move-node" :id test-companion-commands--scene-2
         :parent (test-companion-commands--id root "Chapter 8 - The New Normal") :after nil))
      (should-not (equal disk (test-companion-commands--disk root "novel.org")))
      (should-not (buffer-modified-p (find-file-noselect (expand-file-name "novel.org" root)))))))

(ert-deftest test-companion-commands-logbook-tags-and-priority-survive-a-move ()
  (let* ((root (file-name-as-directory (make-temp-file "test-companion-logbook-" t)))
         (default-directory root)
         (org-id-locations-file (expand-file-name "ids" root))
         (make-backup-files nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".org-scribe-project" root)
            (insert "# Writing project: L\n# Type: novel\n# Language: en\n"))
          (with-temp-file (expand-file-name "novel.org" root)
            (insert "#+TODO: TODO | FINISHED\n* Act\n:PROPERTIES:\n:ID: a\n:END:\n"
                    "** Chapter A\n:PROPERTIES:\n:ID: ca\n:END:\n"
                    "*** TODO [#A] Scene with history :ignore:pov:\n"
                    ":PROPERTIES:\n:ID: s1\n:WORDCOUNT: 10\n:CUSTOM: keep me\n:END:\n"
                    ":LOGBOOK:\n- State \"TODO\" from \"\" [2026-10-01 Thu 10:00]\n"
                    "CLOCK: [2026-10-01 Thu 10:00]--[2026-10-01 Thu 10:30] =>  0:30\n:END:\n"
                    "Body line one.\n\nBody line two with *bold*.\n"
                    "**** A sub-heading\nunder the scene\n"
                    "*** TODO Other scene\n:PROPERTIES:\n:ID: s2\n:END:\ntext\n"
                    "** Chapter B\n:PROPERTIES:\n:ID: cb\n:END:\n"))
          (org-scribe-project-type-cache-clear)
          (org-scribe-companion--clear-cache)
          (let ((before (test-companion-commands--subtree root "Scene with history")))
            (should (test-companion-commands--ok-p
                     (test-companion-commands--run
                      root "move-node" :id "s1" :parent "cb" :after nil)))
            (should (equal before (test-companion-commands--subtree
                                   root "Scene with history")))
            (should (string-match-p ":LOGBOOK:" before))
            (should (string-match-p ":ignore:pov:" before))
            (should (string-match-p "\\*\\*\\*\\* A sub-heading" before))
            (should (equal '("Other scene")
                           (test-companion-commands--chapter-scenes root "Chapter A")))))
      (dolist (buffer (buffer-list))
        (when-let* ((file (buffer-file-name buffer)))
          (when (string-prefix-p root file)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (org-scribe-project-type-cache-clear)
      (delete-directory root t))))

;;; set-title

(ert-deftest test-companion-commands-set-title-changes-only-the-heading-text ()
  (test-companion-commands--with-project root
    (let ((original (test-companion-commands--text root "novel.org")))
      (should (test-companion-commands--ok-p
               (test-companion-commands--run
                root "set-title" :id test-companion-commands--scene-1 :title "  The Third Alarm ")))
      (let ((after (test-companion-commands--text root "novel.org")))
        (should (string-match-p "^\\*\\*\\* FINISHED The Third Alarm +:ignore:$" after))
        (should (equal (cl-remove-if (lambda (l) (string-match-p "Scene 1 - The Alarm\\|The Third Alarm" l))
                                     (split-string original "\n"))
                       (cl-remove-if (lambda (l) (string-match-p "Scene 1 - The Alarm\\|The Third Alarm" l))
                                     (split-string after "\n"))))))))

(ert-deftest test-companion-commands-set-title-rejects-bad-titles-and-apparatus ()
  (test-companion-commands--with-project root
    (let ((original (test-companion-commands--text root "novel.org")))
      (dolist (title '("" "   " "* looks like a heading" nil 5))
        (should (equal "invalid"
                       (test-companion-commands--code
                        (test-companion-commands--run
                         root "set-title" :id test-companion-commands--scene-1 :title title)))))
      (should (equal "read-only"
                     (test-companion-commands--code
                      (test-companion-commands--run
                       root "set-title" :id "a6dffc06-711f-431e-bc46-6aad18e9af06"
                       :title "Renamed apparatus"))))
      (should (equal "read-only"
                     (test-companion-commands--code
                      (test-companion-commands--run
                       root "set-title" :id test-companion-commands--alex :title "Renamed"))))
      (should (equal original (test-companion-commands--text root "novel.org"))))))

(ert-deftest test-companion-commands-set-title-flattens-newlines ()
  (test-companion-commands--with-project root
    (test-companion-commands--run
     root "set-title" :id test-companion-commands--scene-1 :title "Two\nlines")
    (should (string-match-p "^\\*\\*\\* FINISHED Two lines +:ignore:$"
                            (test-companion-commands--text root "novel.org")))))

(ert-deftest test-companion-commands-set-title-same-title-is-a-no-op ()
  (test-companion-commands--with-project root
    (test-companion-commands--run
     root "set-title" :id test-companion-commands--scene-1 :title "Scene 1 - The Alarm")
    (should-not (buffer-modified-p (find-file-noselect (expand-file-name "novel.org" root))))))

;;; set-todo

(ert-deftest test-companion-commands-set-todo-changes-the-state ()
  (test-companion-commands--with-project root
    (should (test-companion-commands--ok-p
             (test-companion-commands--run
              root "set-todo" :id test-companion-commands--scene-1 :state "TOREVIEW")))
    (should (string-match-p "^\\*\\*\\* TOREVIEW Scene 1 - The Alarm +:ignore:$"
                            (test-companion-commands--text root "novel.org")))
    (should (test-companion-commands--ok-p
             (test-companion-commands--run
              root "set-todo" :id test-companion-commands--scene-1 :state "")))
    (should (string-match-p "^\\*\\*\\* Scene 1 - The Alarm +:ignore:$"
                            (test-companion-commands--text root "novel.org")))))

(ert-deftest test-companion-commands-set-todo-never-opens-a-note-buffer ()
  "TOREVIEW is declared `j@/!': Org would ask for a note.  The browser must not."
  (test-companion-commands--with-project root
    (test-companion-commands--run
     root "set-todo" :id test-companion-commands--scene-1 :state "TOREVIEW")
    (should-not (get-buffer "*Org Note*"))
    (should-not (cl-some (lambda (b) (string-prefix-p "*Org Note" (buffer-name b))) (buffer-list)))))

(ert-deftest test-companion-commands-set-todo-rejects-unknown-states ()
  (test-companion-commands--with-project root
    (let ((original (test-companion-commands--text root "novel.org")))
      (dolist (state '("BOGUS" "todo" nil 3))
        (should (equal "invalid"
                       (test-companion-commands--code
                        (test-companion-commands--run
                         root "set-todo" :id test-companion-commands--scene-1 :state state)))))
      (should (equal original (test-companion-commands--text root "novel.org"))))))

(ert-deftest test-companion-commands-set-todo-is-one-undo-step ()
  (test-companion-commands--with-project root
    (let ((original (test-companion-commands--text root "novel.org")))
      (test-companion-commands--run
       root "set-todo" :id test-companion-commands--scene-2 :state "REDO")
      (test-companion-commands--undo-once root)
      (should (equal original (test-companion-commands--text root "novel.org"))))))

;;; set-field

(ert-deftest test-companion-commands-set-field-changes-only-that-line ()
  (test-companion-commands--with-project root
    (let ((original (test-companion-commands--text root "novel.org")))
      (should (test-companion-commands--ok-p
               (test-companion-commands--run
                root "set-field" :id test-companion-commands--scene-1
                :key "summary" :value "A new summary, with a comma.")))
      (let ((old-lines (split-string original "\n"))
            (new-lines (split-string (test-companion-commands--text root "novel.org") "\n")))
        (should (= (length old-lines) (length new-lines)))
        (let ((changed (cl-remove-if #'null
                                     (cl-mapcar (lambda (a b) (and (not (equal a b)) (cons a b)))
                                                old-lines new-lines))))
          (should (= 1 (length changed)))
          (should (string-prefix-p ":Summary: The alarm goes off" (car (car changed))))
          (should (string-match-p "\\`:Summary: +A new summary, with a comma\\.\\'" (cdr (car changed)))))))))

(ert-deftest test-companion-commands-set-field-can-clear-a-text-field ()
  (test-companion-commands--with-project root
    (test-companion-commands--run
     root "set-field" :id test-companion-commands--scene-1 :key "emotion" :value "")
    (let ((fields (plist-get (test-companion-commands--scene-data root "Scene 1 - The Alarm")
                             :fields)))
      (should-not (plist-member fields :emotion)))
    (should (string-match-p "^:Emotion:$" (test-companion-commands--text root "novel.org")))))

(defun test-companion-commands--scene-data (root title)
  (cl-block nil
    (seq-doseq (act (plist-get (org-scribe-companion--snapshot root) :acts))
      (seq-doseq (chapter (plist-get act :chapters))
        (seq-doseq (scene (plist-get chapter :scenes))
          (when (equal (plist-get scene :title) title) (cl-return scene)))))))

(ert-deftest test-companion-commands-set-field-flattens-newlines-and-refuses-links ()
  (test-companion-commands--with-project root
    (test-companion-commands--run
     root "set-field" :id test-companion-commands--scene-1 :key "summary" :value "one\n\ntwo")
    (should (string-match-p "^:Summary: +one two$" (test-companion-commands--text root "novel.org")))
    (let ((original (test-companion-commands--text root "novel.org")))
      ;; link fields are edited by `links', not by text (a later phase)
      (should (equal "unsupported"
                     (test-companion-commands--code
                      (test-companion-commands--run
                       root "set-field" :id test-companion-commands--scene-1
                       :key "characters" :value "Alex"))))
      (dolist (key '("nonsense" "relationships" nil))
        (should (equal "invalid"
                       (test-companion-commands--code
                        (test-companion-commands--run
                         root "set-field" :id test-companion-commands--scene-1
                         :key key :value "x")))))
      (should (equal "invalid"
                     (test-companion-commands--code
                      (test-companion-commands--run
                       root "set-field" :id test-companion-commands--scene-1 :key "summary"))))
      ;; not a scene
      (should (equal "invalid"
                     (test-companion-commands--code
                      (test-companion-commands--run
                       root "set-field" :id (test-companion-commands--id root "Chapter 1 - Three Alarms")
                       :key "summary" :value "x"))))
      (should (equal original (test-companion-commands--text root "novel.org"))))))

(defun test-companion-commands--spanish-project (root)
  "Write a small Spanish novel with two chapters into ROOT."
  (with-temp-file (expand-file-name ".org-scribe-project" root)
    (insert "# Writing project: Prueba\n# Type: novel\n# Language: es\n"))
  (with-temp-file (expand-file-name "novela.org" root)
    (insert "#+TODO: TODO TOWRITE | FINISHED\n"
            "* Acto 1\n:PROPERTIES:\n:ID: a1\n:END:\n"
            "** Capítulo 1\n:PROPERTIES:\n:ID: c1\n:END:\n"
            "*** TOWRITE Escena 1 :ignore:\n:PROPERTIES:\n:ID: e1\n:Resumen: Ana llega.\n:Trama:\n:END:\ntexto\n"
            "*** TOWRITE Escena 2 :ignore:\n:PROPERTIES:\n:ID: e2\n:END:\nmás texto\n"
            "** Capítulo 2\n:PROPERTIES:\n:ID: c2\n:END:\n")))

(defmacro test-companion-commands--with-spanish (root-var &rest body)
  (declare (indent 1))
  `(let* ((,root-var (file-name-as-directory (make-temp-file "test-companion-es-" t)))
          (default-directory ,root-var)
          (org-id-locations-file (expand-file-name "ids" ,root-var))
          (make-backup-files nil))
     (unwind-protect
         (progn
           (test-companion-commands--spanish-project ,root-var)
           (org-scribe-project-type-cache-clear)
           (org-scribe-companion--clear-cache)
           ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (string-prefix-p ,root-var file)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (org-scribe-project-type-cache-clear)
       (delete-directory ,root-var t))))

(ert-deftest test-companion-commands-set-field-uses-the-localised-property-name ()
  (test-companion-commands--with-spanish root
    ;; an existing property keeps its spelling
    (test-companion-commands--run root "set-field" :id "e1" :key "summary" :value "Ana se va.")
    ;; an absent one is written in the project's language
    (test-companion-commands--run root "set-field" :id "e2" :key "summary" :value "Nada.")
    (test-companion-commands--run root "set-field" :id "e2" :key "emotion" :value "Calma")
    (let ((text (with-current-buffer (find-file-noselect (expand-file-name "novela.org" root))
                  (buffer-string))))
      (should (string-match-p "^:Resumen: +Ana se va\\.$" text))
      (should (string-match-p "^:Resumen: +Nada\\.$" text))
      (should (string-match-p "^:Emocion: +Calma$" text))
      (should-not (string-match-p ":Summary:\\|:Emotion:" text)))))

;;; create-scene

(ert-deftest test-companion-commands-create-scene-after-a-scene ()
  (test-companion-commands--with-project root
    (let* ((answer (test-companion-commands--run
                    root "create-scene"
                    :parent (test-companion-commands--id root "Chapter 2 - What It Costs")
                    :after (test-companion-commands--id root "Scene 1 - The Decision")
                    :title "A brand new scene"))
           (id (plist-get answer :id)))
      (should (test-companion-commands--ok-p answer))
      (should (stringp id))
      (should (equal '("Scene 1 - The Decision" "A brand new scene"
                       "Scene 2 - The Handover" "Scene 3 - Terms")
                     (test-companion-commands--chapter-scenes
                      root "Chapter 2 - What It Costs")))
      (let ((text (test-companion-commands--subtree root "A brand new scene")))
        (should (string-prefix-p "*** TODO A brand new scene :ignore:\n" text))
        (should (string-match-p (format ":ID:       %s" (regexp-quote id)) text))
        (should (string-match-p ":WORD-OBJECTIVE: 500" text))
        (should (string-match-p "^:Summary:$" text))
        (should (string-match-p "{{{scene-break}}}" text)))
      ;; the new scene is in the snapshot, addressable
      (should (eq :false (plist-get (test-companion-commands--scene-data root "A brand new scene")
                                    :readOnly))))))

(ert-deftest test-companion-commands-create-scene-first-and-in-an-empty-chapter ()
  (test-companion-commands--with-project root
    (test-companion-commands--run
     root "create-scene" :parent (test-companion-commands--id root "Chapter 1 - Three Alarms")
     :after nil :title "Very first")
    (should (equal '("Very first" "Scene 1 - The Alarm" "Scene 2 - Coffee Shop Encounter")
                   (test-companion-commands--chapter-scenes root "Chapter 1 - Three Alarms")))
    ;; the chapter heading's own text stays above the new scene
    (should (< (string-search "** FINISHED Chapter 1" (test-companion-commands--text root "novel.org"))
               (string-search "*** TODO Very first" (test-companion-commands--text root "novel.org"))))))

(ert-deftest test-companion-commands-create-scene-at-the-end-of-the-last-chapter ()
  (test-companion-commands--with-project root
    (test-companion-commands--run
     root "create-scene" :parent (test-companion-commands--id root "Chapter 8 - The New Normal")
     :after (test-companion-commands--id root "Scene 2 - Full Circle") :title "Epilogue")
    (should (equal '("Scene 1 - Eleven Minutes" "Scene 2 - Full Circle" "Epilogue")
                   (test-companion-commands--chapter-scenes root "Chapter 8 - The New Normal")))))

(ert-deftest test-companion-commands-create-scene-default-title-and-one-undo ()
  (test-companion-commands--with-project root
    (let ((original (test-companion-commands--text root "novel.org")))
      (test-companion-commands--run
       root "create-scene" :parent (test-companion-commands--id root "Chapter 1 - Three Alarms")
       :after nil :title "  ")
      (should (member (org-scribe-msg 'default-scene-name)
                      (test-companion-commands--chapter-scenes
                       root "Chapter 1 - Three Alarms")))
      (test-companion-commands--undo-once root)
      (should (equal original (test-companion-commands--text root "novel.org"))))))

(ert-deftest test-companion-commands-create-scene-refuses-what-is-not-a-chapter ()
  (test-companion-commands--with-project root
    (let ((original (test-companion-commands--text root "novel.org")))
      (should (equal "invalid"
                     (test-companion-commands--code
                      (test-companion-commands--run
                       root "create-scene" :parent (test-companion-commands--id root "Act I")
                       :title "x"))))
      (should (equal "invalid"
                     (test-companion-commands--code
                      (test-companion-commands--run
                       root "create-scene" :parent test-companion-commands--scene-1 :title "x"))))
      (should (equal "read-only"
                     (test-companion-commands--code
                      (test-companion-commands--run
                       root "create-scene" :parent "a6dffc06-711f-431e-bc46-6aad18e9af06"
                       :title "x"))))
      (should (equal "conflict"
                     (test-companion-commands--code
                      (test-companion-commands--run
                       root "create-scene"
                       :parent (test-companion-commands--id root "Chapter 2 - What It Costs")
                       :after test-companion-commands--scene-1 :title "x"))))
      (should (equal original (test-companion-commands--text root "novel.org"))))))

(ert-deftest test-companion-commands-create-scene-in-a-spanish-project-uses-its-language ()
  (test-companion-commands--with-spanish root
    (let ((answer (test-companion-commands--run
                   root "create-scene" :parent "c2" :after nil :title "Escena nueva")))
      (should (test-companion-commands--ok-p answer))
      (let ((text (with-current-buffer (find-file-noselect (expand-file-name "novela.org" root))
                    (buffer-string))))
        (should (string-match-p "^\\*\\*\\* TODO Escena nueva :ignore:$" text))
        (should (string-match-p "^:Personajes:$" text))
        (should (string-match-p "^:Punto-de-trama:$" text))
        (should-not (string-match-p "^:Characters:$" text))))))

(ert-deftest test-companion-commands-a-failing-edit-is-undone-whole ()
  (test-companion-commands--with-project root
    (let ((original (test-companion-commands--text root "novel.org")))
      (cl-letf (((symbol-function 'org-id-get-create)
                 (lambda (&rest _) (error "Simulated failure after the insertion"))))
        (let ((answer (test-companion-commands--run
                       root "create-scene"
                       :parent (test-companion-commands--id root "Chapter 1 - Three Alarms")
                       :after nil :title "Doomed")))
          (should (eq :false (plist-get answer :ok)))))
      (should (equal original (test-companion-commands--text root "novel.org"))))))

(ert-deftest test-companion-commands-new-messages-in-both-languages ()
  (dolist (key '(companion-error-read-only companion-error-invalid-move
                 companion-error-conflict-after companion-error-invalid-title
                 companion-error-invalid-state companion-error-todo-refused
                 companion-error-invalid-key companion-error-not-a-scene
                 companion-error-not-a-chapter))
    (should (alist-get key org-scribe-messages-en))
    (should (alist-get key org-scribe-messages-es))))

;;; Follow mode

(defun test-companion-commands--collect-focus (root body)
  "Serve ROOT, run BODY, and return the `focus' events sent meanwhile."
  (let ((events nil))
    (cl-letf (((symbol-function 'org-scribe-companion-server-subscribed-p) (lambda () t))
              ((symbol-function 'org-scribe-companion-server-broadcast)
               (lambda (event data) (when (equal event "focus") (push (plist-get data :id) events)))))
      (funcall body))
    (nreverse events)))

(ert-deftest test-companion-commands-scene-id-at-point ()
  (test-companion-commands--with-project root
    (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
      (goto-char (point-min))
      (re-search-forward "^Alex wakes\\|alarm goes off\\|^:Summary: The alarm")
      (should (equal test-companion-commands--scene-1
                     (org-scribe-companion--scene-id-at-point)))
      ;; on a chapter heading, the chapter
      (goto-char (point-min))
      (re-search-forward "^\\*\\* FINISHED Chapter 1")
      (should (equal "f9532c04-63d5-4871-9ed2-4aeefa13fac1"
                     (org-scribe-companion--scene-id-at-point))))))

(ert-deftest test-companion-commands-follow-sends-focus-only-when-the-scene-changes ()
  (test-companion-commands--with-project root
    (let* ((org-scribe-companion-follow t)
           (org-scribe-companion--last-focus nil)
           (events
            (test-companion-commands--collect-focus
             root
             (lambda ()
               (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
                 (goto-char (point-min))
                 (re-search-forward "^\\*\\*\\* FINISHED Scene 1")
                 (org-scribe-companion--follow)
                 (forward-line 3)
                 (org-scribe-companion--follow)
                 (re-search-forward "^\\*\\*\\* FINISHED Scene 2")
                 (org-scribe-companion--follow)
                 (org-scribe-companion--follow))))))
      (should (equal (list test-companion-commands--scene-1 test-companion-commands--scene-2)
                     events)))))

(ert-deftest test-companion-commands-follow-off-or-nobody-listening-sends-nothing ()
  (test-companion-commands--with-project root
    (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
      (goto-char (point-min))
      (re-search-forward "^\\*\\*\\* FINISHED Scene 1")
      (let ((org-scribe-companion-follow nil) (org-scribe-companion--last-focus nil))
        (should-not (test-companion-commands--collect-focus
                     root #'org-scribe-companion--follow)))
      (let ((org-scribe-companion-follow t) (org-scribe-companion--last-focus nil)
            sent)
        (cl-letf (((symbol-function 'org-scribe-companion-server-subscribed-p) (lambda () nil))
                  ((symbol-function 'org-scribe-companion-server-broadcast)
                   (lambda (&rest _) (setq sent t))))
          (org-scribe-companion--follow))
        (should-not sent)))))

(ert-deftest test-companion-commands-show-scene-sends-focus-even-if-unchanged ()
  (test-companion-commands--with-project root
    (let ((org-scribe-companion--last-focus test-companion-commands--scene-1)
          (events nil))
      (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
        (goto-char (point-min))
        (re-search-forward "^\\*\\*\\* FINISHED Scene 1")
        (cl-letf (((symbol-function 'org-scribe-companion-server-running-p) (lambda () t))
                  ((symbol-function 'org-scribe-companion-server-broadcast)
                   (lambda (_event data) (push (plist-get data :id) events))))
          (org-scribe-companion-show-scene)))
      (should (equal (list test-companion-commands--scene-1) events)))))

(ert-deftest test-companion-commands-show-scene-needs-a-running-companion ()
  (should-error (org-scribe-companion-show-scene) :type 'user-error))

(ert-deftest test-companion-commands-hooks-follow-install-and-removal ()
  (test-companion-commands--with-project root
    (let ((org-scribe-companion-port 0))
      (unwind-protect
          (progn
            (org-scribe-companion--start root)
            (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
              (should (memq #'org-scribe-companion--follow post-command-hook)))
            (with-current-buffer (find-file-noselect (expand-file-name "design.org" root))
              (should-not (memq #'org-scribe-companion--follow post-command-hook))))
        (org-scribe-companion--stop))
      (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
        (should-not (memq #'org-scribe-companion--follow post-command-hook))))))

;;; The web files only use strings that exist

(defconst test-companion-commands--web
  (expand-file-name "companion/web" test-companion-commands--repo))

(defun test-companion-commands--string-keys-used ()
  "Return the `companion-*' keys the web files mention, in any form."
  (let (keys)
    (dolist (file (directory-files-recursively test-companion-commands--web "\\.\\(js\\|html\\)\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "['\"]\\(companion-[a-z0-9-]+\\)['\"]" nil t)
          (push (intern (match-string 1)) keys))))
    (delete-dups keys)))

(ert-deftest test-companion-commands-web-strings-exist-in-both-languages ()
  (let ((keys (test-companion-commands--string-keys-used)))
    (should (> (length keys) 10))
    (dolist (key keys)
      (should (alist-get key org-scribe-messages-en))
      (should (alist-get key org-scribe-messages-es)))))

(ert-deftest test-companion-commands-web-files-use-no-innerhtml ()
  (dolist (file (directory-files-recursively test-companion-commands--web "\\.js\\'"))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not (re-search-forward "innerHTML\\|outerHTML\\|insertAdjacentHTML\\|document\\.write" nil t)))))

(ert-deftest test-companion-commands-web-files-have-no-literal-ui-text ()
  "Visible text in HTML elements comes from data-i18n, not from the markup."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "index.html" test-companion-commands--web))
    (goto-char (point-min))
    ;; text between a tag's > and the next <, ignoring whitespace
    (while (re-search-forward ">\\([^<>]*[[:alpha:]][^<>]*\\)<" nil t)
      (should (member (string-trim (match-string 1)) '("org-scribe"))))))

(ert-deftest test-companion-commands-messages-in-both-languages ()
  (dolist (key '(companion-error-not-found companion-error-missing-arg
                 companion-error-no-scene-here))
    (should (alist-get key org-scribe-messages-en))
    (should (alist-get key org-scribe-messages-es))))

(provide 'test-companion-commands)
;;; test-companion-commands.el ends here
