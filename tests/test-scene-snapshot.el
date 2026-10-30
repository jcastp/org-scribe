;;; test-scene-snapshot.el --- Tests for scene snapshots -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; `org-scribe-snapshot-scene' copies a scene's prose into a Versions section
;; of the design notebook's Revision part; `org-scribe-compare-snapshot' diffs
;; one against the live scene.  These tests pin what is copied (and, as
;; important, what is not: the scene's drawer, its sub-headings' drawers), where
;; the section is created, and that nothing else reads the result.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'org-scribe)

(defconst test-snapshot--manuscript
  (concat "* Act One\n** Chapter One\n"
          "*** TODO The Heist :ignore:\n"
          ":PROPERTIES:\n:PoV: Ana\n:WORDCOUNT: 99\n:END:\n"
          "Ana opened the safe and found nothing.\n\n"
          "#+begin_comment\n*EDIT* tighten this\n#+end_comment\n"
          "**** Aside\n:PROPERTIES:\n:ID: aside-id-1\n:END:\n"
          "A second thought.\n"
          "*** Empty Scene :ignore:\n"))

(defmacro test-snapshot--with-project (notebook &rest body)
  "Run BODY in a temp novel project whose notebook is NOTEBOOK.
Binds `root' and `manuscript'; point is left on the first scene's heading."
  (declare (indent 1))
  `(let* ((root (file-name-as-directory (make-temp-file "test-snapshot-" t)))
          (default-directory root)
          (org-id-locations-file (expand-file-name "ids" root))
          (org-id-locations nil)
          (org-id-track-globally t)
          (manuscript (expand-file-name "novel.org" root)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".org-scribe-project" root)
             (insert "# Type: novel\n# Language: en\n"))
           (with-temp-file manuscript (insert test-snapshot--manuscript))
           (with-temp-file (expand-file-name "design.org" root) (insert ,notebook))
           (org-scribe-project-type-cache-clear)
           (with-current-buffer (find-file-noselect manuscript)
             (goto-char (point-min))
             (re-search-forward "^\\*\\*\\* TODO The Heist")
             ,@body))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b) (string-prefix-p root (buffer-file-name b)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory root t))))

(defun test-snapshot--notebook (root)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "design.org" root))
    (buffer-string)))

(defconst test-snapshot--design "* Cast\n** Characters\n* Revision\n** Revision log\n* Scratchpad\nIdeas.\n")

;;; Taking a snapshot

(ert-deftest test-snapshot-creates-versions-under-the-revision-part ()
  (test-snapshot--with-project test-snapshot--design
    (org-scribe-snapshot-scene)
    (let ((text (test-snapshot--notebook root)))
      (should (string-match "^\\*\\* Versions$" text))
      ;; inside the Revision part, after its log, before the Scratchpad
      (should (< (string-match "^\\* Revision" text)
                 (string-match "^\\*\\* Versions" text)
                 (string-match "^\\* Scratchpad" text)))
      (should (string-match "^\\*\\*\\* Snapshot [0-9-]+ [0-9:]+ - The Heist$" text)))))

(ert-deftest test-snapshot-leaves-the-scratchpad-untouched ()
  (test-snapshot--with-project test-snapshot--design
    (org-scribe-snapshot-scene)
    (let ((text (test-snapshot--notebook root)))
      (should (string-suffix-p "* Scratchpad\nIdeas.\n" text)))))

(ert-deftest test-snapshot-copies-prose-but-not-the-scene-drawer ()
  (test-snapshot--with-project test-snapshot--design
    (org-scribe-snapshot-scene)
    (let ((text (test-snapshot--notebook root)))
      (should (string-match-p "Ana opened the safe and found nothing\\." text))
      (should (string-match-p "\\*EDIT\\* tighten this" text))
      ;; the scene's own properties are not duplicated
      (should-not (string-match-p ":PoV:" text))
      (should-not (string-match-p ":WORDCOUNT:" text)))))

(ert-deftest test-snapshot-writes-link-words-and-state ()
  (test-snapshot--with-project test-snapshot--design
    (org-scribe-snapshot-scene)
    (let ((id (save-excursion (org-back-to-heading t) (org-entry-get nil "ID")))
          (text (test-snapshot--notebook root)))
      (should id)
      (should (string-match-p (format ":SCENE: \\[\\[id:%s\\]\\[The Heist\\]\\]" (regexp-quote id)) text))
      (should (string-match-p ":WORDS: [1-9][0-9]*" text))
      (should (string-match-p ":STATE: TODO" text)))))

(ert-deftest test-snapshot-subheading-id-is-not-duplicated-and-level-is-kept ()
  "A copied sub-heading drops its drawer, so its :ID: exists once."
  (test-snapshot--with-project test-snapshot--design
    (org-scribe-snapshot-scene)
    (let ((text (test-snapshot--notebook root)))
      (should-not (string-match-p "aside-id-1" text))
      ;; scene level 3, snapshot level 3: the aside stays at level 4
      (should (string-match-p "^\\*\\*\\*\\* Aside$" text))
      (should (string-match-p "A second thought\\." text)))))

(ert-deftest test-snapshot-versions-heading-is-created-once-and-snapshots-append ()
  (test-snapshot--with-project test-snapshot--design
    (org-scribe-snapshot-scene)
    (org-scribe-snapshot-scene)
    (let ((text (test-snapshot--notebook root)))
      (should (= 1 (with-temp-buffer
                     (insert text)
                     (how-many "^\\*\\* Versions$" (point-min) (point-max)))))
      (should (= 2 (with-temp-buffer
                     (insert text)
                     (how-many "^\\*\\*\\* Snapshot " (point-min) (point-max))))))))

(ert-deftest test-snapshot-refuses-a-scene-without-prose ()
  (test-snapshot--with-project test-snapshot--design
    (re-search-forward "^\\*\\*\\* Empty Scene")
    (should-error (org-scribe-snapshot-scene) :type 'user-error)
    (should-not (string-match-p "Versions" (test-snapshot--notebook root)))))

(ert-deftest test-snapshot-refuses-outside-a-scene ()
  (test-snapshot--with-project test-snapshot--design
    (goto-char (point-min))
    (should-error (org-scribe-snapshot-scene) :type 'user-error)))

(ert-deftest test-snapshot-works-from-a-scene-subheading ()
  (test-snapshot--with-project test-snapshot--design
    (re-search-forward "^\\*\\*\\*\\* Aside")
    (org-scribe-snapshot-scene)
    (should (string-match-p "Snapshot [0-9-]+ [0-9:]+ - The Heist"
                            (test-snapshot--notebook root)))))

(ert-deftest test-snapshot-leaves-the-writers-unsaved-notebook-unsaved ()
  (test-snapshot--with-project test-snapshot--design
    (let ((buffer (find-file-noselect (expand-file-name "design.org" root))))
      (with-current-buffer buffer (goto-char (point-max)) (insert "unsaved\n"))
      (org-scribe-snapshot-scene)
      (should (buffer-modified-p buffer))
      (should-not (string-match-p "Versions" (test-snapshot--notebook root))))))

(ert-deftest test-snapshot-heading-is-in-the-project-language ()
  (test-snapshot--with-project "* Revisión\n"
    (with-temp-file (expand-file-name ".org-scribe-project" root)
      (insert "# Type: novel\n# Language: es\n"))
    (org-scribe-project-type-cache-clear)
    (org-scribe-snapshot-scene)
    (let ((text (test-snapshot--notebook root)))
      (should (string-match-p "^\\*\\* Versiones$" text))
      (should (string-match-p "^\\*\\*\\* Instantánea " text)))))

(ert-deftest test-snapshot-short-story-gets-a-level-one-section ()
  (let* ((root (file-name-as-directory (make-temp-file "test-snapshot-" t)))
         (default-directory root)
         (org-id-locations-file (expand-file-name "ids" root))
         (org-id-locations nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".org-scribe-project" root)
            (insert "# Type: short-story\n# Language: en\n"))
          (with-temp-file (expand-file-name "story.org" root)
            (insert "* Story\n** Scene One :ignore:\nHe left.\n"))
          (with-temp-file (expand-file-name "design.org" root)
            (insert "* Characters\n"))
          (org-scribe-project-type-cache-clear)
          (with-current-buffer (find-file-noselect (expand-file-name "story.org" root))
            (goto-char (point-min))
            (re-search-forward "^\\*\\* Scene One")
            (org-scribe-snapshot-scene))
          (let ((text (test-snapshot--notebook root)))
            (should (string-match-p "^\\* Versions$" text))
            (should (string-match-p "^\\*\\* Snapshot .* - Scene One$" text))))
      (dolist (b (buffer-list))
        (when (and (buffer-file-name b) (string-prefix-p root (buffer-file-name b)))
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b)))
      (delete-directory root t))))

;;; Nothing else reads the snapshots

(ert-deftest test-snapshot-manuscript-prose-and-word-count-are-unchanged ()
  (test-snapshot--with-project test-snapshot--design
    (let ((before (buffer-string))
          (words (org-scribe--count-words-region (point-min) (point-max))))
      (org-scribe-snapshot-scene)
      ;; only the scene's new :ID: differs
      (should (equal (replace-regexp-in-string ":ID: [^\n]+\n" "" (buffer-string))
                     (replace-regexp-in-string ":ID: [^\n]+\n" "" before)))
      (should (= words (org-scribe--count-words-region (point-min) (point-max)))))))

(ert-deftest test-snapshot-children-of-versions-are-not-entities ()
  (test-snapshot--with-project test-snapshot--design
    (org-scribe-snapshot-scene)
    (with-current-buffer (find-file-noselect (expand-file-name "design.org" root))
      (goto-char (point-min))
      (re-search-forward "^\\*\\*\\* Snapshot")
      (should-not (org-scribe--heading-parent-section-p 'characters))
      (should-not (org-scribe--heading-parent-section-p 'setting)))))

;;; Comparing

(defmacro test-snapshot--capturing-ediff (var &rest body)
  "Run BODY with `ediff-regions-internal' stubbed; VAR gets its arguments."
  (declare (indent 1))
  `(let (,var)
     (require 'ediff)                   ; or the code under test loads it over the stub
     (cl-letf (((symbol-function 'ediff-regions-internal)
                (lambda (&rest got) (setq ,var got))))
       ,@body)))

(ert-deftest test-snapshot-compare-diffs-the-real-regions ()
  (test-snapshot--with-project test-snapshot--design
    (org-scribe-snapshot-scene)
    (goto-char (point-min))
    (re-search-forward "^\\*\\*\\* TODO The Heist")
    ;; rewrite the scene
    (re-search-forward "Ana opened the safe")
    (replace-match "Ana cracked the vault")
    (test-snapshot--capturing-ediff args
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p coll &rest _) (car (all-completions "" coll)))))
        (org-scribe-compare-snapshot))
      (should args)
      (let ((buf-a (nth 0 args)) (buf-b (nth 3 args)))
        (should (string-suffix-p "design.org" (buffer-file-name buf-a)))
        (should (string-suffix-p "novel.org" (buffer-file-name buf-b)))
        (should (string-match-p "opened the safe"
                                (with-current-buffer buf-a
                                  (buffer-substring (nth 1 args) (nth 2 args)))))
        (should (string-match-p "cracked the vault"
                                (with-current-buffer buf-b
                                  (buffer-substring (nth 4 args) (nth 5 args)))))))))

(ert-deftest test-snapshot-compare-reports-when-there-are-none ()
  (test-snapshot--with-project test-snapshot--design
    (should-error (org-scribe-compare-snapshot) :type 'user-error)))

(ert-deftest test-snapshot-compare-offers-only-the-scenes-own-snapshots ()
  (test-snapshot--with-project test-snapshot--design
    (org-scribe-snapshot-scene)
    ;; a snapshot of another scene, written by hand
    (with-current-buffer (find-file-noselect (expand-file-name "design.org" root))
      (goto-char (point-max))
      (insert "*** Snapshot 2026-01-01 10:00 - Other\n:PROPERTIES:\n:SCENE: [[id:nope][Other]]\n:END:\ntext\n")
      (save-buffer))
    (let (offered)
      (test-snapshot--capturing-ediff args
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p coll &rest _)
                     (setq offered (all-completions "" coll))
                     (car offered))))
          (org-scribe-compare-snapshot)))
      (should (= 1 (length offered)))
      (should (string-match-p "The Heist" (car offered))))))

(ert-deftest test-snapshot-compare-with-point-in-a-scene-without-snapshots ()
  (test-snapshot--with-project test-snapshot--design
    (org-scribe-snapshot-scene)
    (re-search-forward "^\\*\\*\\* Empty Scene")
    (should-error (org-scribe-compare-snapshot) :type 'user-error)))

(ert-deftest test-snapshot-compare-reports-a-missing-scene ()
  (test-snapshot--with-project test-snapshot--design
    (with-current-buffer (find-file-noselect (expand-file-name "design.org" root))
      (goto-char (point-max))
      (insert "** Versions\n*** Snapshot 2026-01-01 10:00 - Gone\n:PROPERTIES:\n:SCENE: [[id:gone][Gone]]\n:END:\ntext\n")
      (save-buffer))
    (goto-char (point-min))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p coll &rest _) (car (all-completions "" coll)))))
      (should-error (org-scribe-compare-snapshot) :type 'user-error))))

(ert-deftest test-snapshot-two-in-one-minute-have-distinct-labels ()
  (test-snapshot--with-project test-snapshot--design
    (org-scribe-snapshot-scene)
    (org-scribe-snapshot-scene)
    (let ((labels (with-current-buffer (find-file-noselect (expand-file-name "design.org" root))
                    (mapcar (lambda (s) (plist-get s :label))
                            (org-scribe-snapshot--collect)))))
      (should (= 2 (length labels)))
      (should (= 2 (length (delete-dups (copy-sequence labels))))))))

(provide 'test-scene-snapshot)

;;; test-scene-snapshot.el ends here
