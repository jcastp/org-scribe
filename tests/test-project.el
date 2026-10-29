;;; test-project.el --- Tests for project creation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for project creation and template processing.
;; Tests novel and short story project creation, template
;; variable substitution, title validation, and project structure.

;;; Code:

(require 'ert)
(require 'org)

;;; Add paths
(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../templates" default-directory)))

(require 'org-scribe-project)

;;; Function Availability Tests

(ert-deftest test-project-functions-defined ()
  "Test that all public project functions are defined."
  ;; Project creation
  (should (fboundp 'org-scribe-create-novel-project))
  (should (fboundp 'org-scribe-create-short-story-project))

  ;; Template insertion
  (should (fboundp 'org-scribe-insert-scene))
  (should (fboundp 'org-scribe-insert-chapter))

  ;; Navigation
  (should (fboundp 'org-scribe-open-project-file))

  ;; Utilities
  (should (fboundp 'org-scribe-edit-templates))
  (should (fboundp 'org-scribe-register-projects))

  ;; Backward compatibility
  (should (fboundp 'org-scribe-project-create-novel-project))
  (should (fboundp 'org-scribe-create-project)))

;;; Title Validation Tests

(ert-deftest test-validate-project-title-valid ()
  "Test that valid titles pass validation."
  (should (null (org-scribe--validate-project-title "My Novel")))
  (should (null (org-scribe--validate-project-title "Novel-2025")))
  (should (null (org-scribe--validate-project-title "SciFi_Project")))
  (should (null (org-scribe--validate-project-title "A"))))

(ert-deftest test-validate-project-title-empty ()
  "Test that empty titles fail validation."
  (should (stringp (org-scribe--validate-project-title "")))
  (should (stringp (org-scribe--validate-project-title "   ")))
  (should (stringp (org-scribe--validate-project-title "\t\n"))))

(ert-deftest test-validate-project-title-path-separators ()
  "Test that titles with path separators fail validation."
  (should (stringp (org-scribe--validate-project-title "My/Novel")))
  (should (stringp (org-scribe--validate-project-title "My\\Novel")))
  (should (stringp (org-scribe--validate-project-title "Novel/Chapter/1"))))

(ert-deftest test-validate-project-title-special-chars ()
  "Test that titles with special characters fail validation."
  (should (stringp (org-scribe--validate-project-title "Novel:Draft")))
  (should (stringp (org-scribe--validate-project-title "Novel*")))
  (should (stringp (org-scribe--validate-project-title "Novel?")))
  (should (stringp (org-scribe--validate-project-title "Novel<>")))
  (should (stringp (org-scribe--validate-project-title "Novel|Pipe")))
  (should (stringp (org-scribe--validate-project-title "Novel\"Quote")))
  (should (stringp (org-scribe--validate-project-title "Novel'Quote"))))

(ert-deftest test-validate-project-title-dots ()
  "Test that titles with problematic dots fail validation."
  (should (stringp (org-scribe--validate-project-title ".hidden")))
  (should (stringp (org-scribe--validate-project-title "...dots")))
  (should (stringp (org-scribe--validate-project-title "Novel..Test")))
  ;; Single dot in middle should be OK
  (should (null (org-scribe--validate-project-title "Novel.Draft"))))

;;; Template Variable Substitution Tests

(ert-deftest test-template-variable-substitution ()
  "Test that template variables are correctly substituted."
  (let* ((temp-template (make-temp-file "test-template-" nil ".template"))
         (temp-output (make-temp-file "test-output-"))
         (variables '(("TITLE" . "Test Novel")
                     ("AUTHOR" . "John Doe")
                     ("DATE" . "2025-01-15"))))

    (unwind-protect
        (progn
          ;; Write template file
          (with-temp-file temp-template
            (insert "#+TITLE: ${TITLE}\n")
            (insert "#+AUTHOR: ${AUTHOR}\n")
            (insert "#+DATE: ${DATE}\n")
            (insert "This is a test for ${TITLE} by ${AUTHOR}.\n"))

          ;; Process template
          (org-scribe--process-template temp-template temp-output variables)

          ;; Verify output
          (with-temp-buffer
            (insert-file-contents temp-output)
            (let ((content (buffer-string)))
              (should (string-match-p "Test Novel" content))
              (should (string-match-p "John Doe" content))
              (should (string-match-p "2025-01-15" content))
              (should-not (string-match-p "\\${TITLE}" content))
              (should-not (string-match-p "\\${AUTHOR}" content))
              (should-not (string-match-p "\\${DATE}" content)))))

      ;; Cleanup
      (when (file-exists-p temp-template)
        (delete-file temp-template))
      (when (file-exists-p temp-output)
        (delete-file temp-output)))))

(ert-deftest test-template-variable-multiple-occurrences ()
  "Test that variables appearing multiple times are all substituted."
  (let* ((temp-template (make-temp-file "test-template-" nil ".template"))
         (temp-output (make-temp-file "test-output-"))
         (variables '(("TITLE" . "My Novel"))))

    (unwind-protect
        (progn
          (with-temp-file temp-template
            (insert "${TITLE} - Chapter 1\n")
            (insert "This is ${TITLE}.\n")
            (insert "Working on ${TITLE} today.\n"))

          (org-scribe--process-template temp-template temp-output variables)

          (with-temp-buffer
            (insert-file-contents temp-output)
            (let ((content (buffer-string)))
              ;; All three occurrences should be replaced
              (should (= 3 (cl-count "My Novel" (split-string content "\n") :test #'string-match-p)))
              (should-not (string-match-p "\\${TITLE}" content)))))

      (when (file-exists-p temp-template)
        (delete-file temp-template))
      (when (file-exists-p temp-output)
        (delete-file temp-output)))))

;;; Scene Template Insertion Tests

(ert-deftest test-insert-scene-template ()
  "Test scene template insertion."
  (with-temp-buffer
    (org-mode)
    (org-scribe-insert-scene "Opening Scene")

    (let ((content (buffer-string)))
      ;; Check heading
      (should (string-match-p "\\*\\*\\* TODO Opening Scene :ignore:" content))

      ;; Check properties
      (should (string-match-p ":PoV:" content))
      (should (string-match-p ":Characters:" content))
      (should (string-match-p ":Plot:" content))
      (should (string-match-p ":Timeline:" content))
      (should (string-match-p ":Location:" content))
      (should (string-match-p ":Description:" content))
      (should (string-match-p ":Summary:" content))
      (should (string-match-p ":Scene-motivation:" content))
      (should (string-match-p ":Conflict-source:" content))
      (should (string-match-p ":What-is-at-stake:" content))
      (should (string-match-p ":Emotion:" content))
      (should (string-match-p ":Comment:" content))
      (should (string-match-p ":WORD-OBJECTIVE: 500" content))

      ;; Check scene break macro
      (should (string-match-p "{{{scene-break}}}" content)))))

(ert-deftest test-insert-scene-template-closes-with-the-scene-break ()
  "The scene break comes after the body's gap, as in the shipped scenes."
  (with-temp-buffer
    (org-mode)
    (org-scribe-insert-scene "Opening Scene")
    (should (string-match-p ":END:\n\n\n{{{scene-break}}}\n\\'" (buffer-string)))))

(ert-deftest test-insert-scene-template-has-no-beat-property ()
  "Inserted scenes carry no `:Beat:'/`:Ritmo:' property.
The drawer used to ship a free-text beat field beside `:Plot-point:' —
two structural classifications of the same scene, one of them read by no
module.  A scene template that re-grows it would re-create exactly the
ambiguity the glossary removes, and nothing else would fail."
  (with-temp-buffer
    (org-mode)
    (org-scribe-insert-scene "Opening Scene")
    (let ((content (buffer-string)))
      (should (string-match-p ":Plot-point:" content))
      (should-not (string-match-p ":Beat:" content))
      (should-not (string-match-p ":Ritmo:" content))))
  (should-not (memq 'beat org-scribe--scene-property-keys))
  ;; The vocabulary is gone from the alias table too, so the key no longer
  ;; resolves to a localized name; unknown keys pass through unchanged.
  (should-not (assq 'beat org-scribe--scene-property-aliases)))

(ert-deftest test-insert-scene-template-spanish-project ()
  "Inside a Spanish project, inserted scenes use localized property names."
  (let* ((temp-dir (make-temp-file "org-scribe-insert-scene-es-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".org-scribe-project" temp-dir)
            (insert "# Writing project: Test\n# Language: es\n"))
          (with-temp-buffer
            (let ((default-directory temp-dir))
              (org-mode)
              (org-scribe-insert-scene "Escena de apertura")
              (let ((content (buffer-string)))
                ;; PoV has no Spanish alias, stays as-is.
                (should (string-match-p ":PoV:" content))
                (should (string-match-p
                         (format ":%s:" (org-scribe-lang-property 'characters 'es)) content))
                (should (string-match-p
                         (format ":%s:" (org-scribe-lang-property 'plot 'es)) content))
                (should (string-match-p
                         (format ":%s:" (org-scribe-lang-property 'location 'es)) content))
                (should-not (string-match-p
                             (format ":%s:" (org-scribe-lang-property 'characters 'en)) content))
                (should-not (string-match-p
                             (format ":%s:" (org-scribe-lang-property 'plot 'en)) content))))))
      (delete-directory temp-dir t))))

(ert-deftest test-insert-scene-empty-name ()
  "Test scene template with empty name uses default."
  (with-temp-buffer
    (org-mode)
    (org-scribe-insert-scene "")

    (let ((content (buffer-string)))
      (should (string-match-p "\\*\\*\\* TODO New scene :ignore:" content)))))

(ert-deftest test-insert-scene-whitespace-name ()
  "Test scene template with whitespace-only name uses default."
  (with-temp-buffer
    (org-mode)
    (org-scribe-insert-scene "   ")

    (let ((content (buffer-string)))
      (should (string-match-p "\\*\\*\\* TODO New scene :ignore:" content)))))

(ert-deftest test-insert-scene-novel-is-level-3-tagged-ignore ()
  "Explicit regression pin for a novel project, stubbed rather than
relying on the ambient (batch-run) project type defaulting to `novel':
inserting a scene produces a level-3 heading tagged :ignore:, exactly as
before this command was made project-type aware."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'novel)))
    (with-temp-buffer
      (org-mode)
      (org-scribe-insert-scene "Opening Scene")
      (should (string-match-p "\\*\\*\\* TODO Opening Scene :ignore:"
                              (buffer-string))))))

(ert-deftest test-insert-scene-short-story-is-level-2 ()
  "In a short-story project, inserting a scene produces a level-2
heading, matching `org-scribe-scene-level' for that project type and the
shipped story.org/cuento.org templates -- not the level-3 heading a
novel gets.  Regression test: before this command consulted the current
project's own outline shape, every short-story scene it inserted was
invisible to `org-scribe-scene-match' and so uncounted by word counting,
linking and the health report."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (org-scribe-insert-scene "Opening")
      (let ((content (buffer-string)))
        (should (string-match-p "^\\*\\* TODO Opening" content))
        (should-not (string-match-p "^\\*\\*\\* " content))
        ;; Tag presence follows whatever `org-scribe-project-levels' says
        ;; for short-story's `:scene-tag' at the time this test runs --
        ;; this only pins the level, not the tag, since the tag is a
        ;; separate, independently-tested decision (see
        ;; `org-scribe-scene-match' tests in test-core-extended.el).
        (should (string-match-p ":PoV:" content))))))

(ert-deftest test-insert-scene-short-story-has-no-word-objective ()
  "A short-story scene gets no `:WORD-OBJECTIVE:', like the shipped Scene 1.
The whole target sits on the container; the 500 default is a novel's."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (org-scribe-insert-scene "Opening")
      (should-not (string-match-p "WORD-OBJECTIVE" (buffer-string)))
      ;; Point still lands after :PoV:.
      (should (looking-back ":PoV: " (line-beginning-position))))))

(ert-deftest test-insert-scene-short-story-tag-matches-project-levels ()
  "The tag on an inserted short-story scene -- present or absent -- must
follow `org-scribe-project-levels' `:scene-tag' for that project type,
not a hardcoded assumption either way."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (org-scribe-insert-scene "Opening")
      (let ((tag (plist-get (org-scribe-project-levels 'short-story) :scene-tag))
            (content (buffer-string)))
        (if tag
            (should (string-match-p (format ":%s:" (regexp-quote tag)) content))
          (should-not (string-match-p "^\\*\\* TODO Opening :" content)))))))

;;; Chapter Template Insertion Tests

;;; Where inserted scenes and chapters go

(defconst test-project--outline
  (concat "* Act I :ignore:\n"
          "** TODO Chapter 1 :ignore:\n"
          "*** TODO Scene A :ignore:\n:PROPERTIES:\n:PoV:\n:END:\n\n"
          "First paragraph of A, with some words in it.\n\nSecond paragraph.\n"
          "*** TODO Scene B :ignore:\n\nText of B.\n"
          "** TODO Chapter 2 :ignore:\n"
          "*** TODO Scene C :ignore:\n\nText of C.\n"
          "* Act II :ignore:\n")
  "A small novel outline for the insertion-point tests.")

(defun test-project--headings ()
  "Return the buffer's headings as (LEVEL . TITLE) pairs, in order."
  (org-map-entries (lambda () (cons (org-current-level)
                                    (org-get-heading t t t t)))))

(defmacro test-project--in-outline (search &rest body)
  "Run BODY in the outline with point just after the first match of SEARCH."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'novel)))
     (with-temp-buffer
       (org-mode)
       (insert test-project--outline)
       (goto-char (point-min))
       (search-forward ,search)
       ,@body)))

(ert-deftest test-insert-scene-mid-paragraph-does-not-split-prose ()
  "With point inside a paragraph the scene goes after the scene, whole.
It used to be inserted at raw point: \"First paragraph of A, with*** TODO ...\",
which is not a heading, with its property drawer inside the sentence."
  (test-project--in-outline "with some"
    (org-scribe-insert-scene "Interrupted")
    (should (string-match-p "^First paragraph of A, with some words in it\\.$"
                            (buffer-string)))
    (should (equal (test-project--headings)
                   '((1 . "Act I") (2 . "Chapter 1") (3 . "Scene A")
                     (3 . "Interrupted") (3 . "Scene B") (2 . "Chapter 2")
                     (3 . "Scene C") (1 . "Act II"))))))

(ert-deftest test-insert-scene-on-chapter-heading-goes-inside-that-chapter ()
  "A scene added with point on a chapter heading is that chapter's last scene.
It used to be inserted above the heading, making it the last scene of the
previous chapter."
  (test-project--in-outline "Chapter 2"
    (org-scribe-insert-scene "Added")
    (should (equal (test-project--headings)
                   '((1 . "Act I") (2 . "Chapter 1") (3 . "Scene A")
                     (3 . "Scene B") (2 . "Chapter 2") (3 . "Scene C")
                     (3 . "Added") (1 . "Act II"))))))

(ert-deftest test-insert-scene-on-act-goes-in-its-last-chapter ()
  "On an act that has chapters, the scene is its last chapter's last scene."
  (test-project--in-outline "Act I"
    (org-scribe-insert-scene "Added")
    (should (equal (test-project--headings)
                   '((1 . "Act I") (2 . "Chapter 1") (3 . "Scene A")
                     (3 . "Scene B") (2 . "Chapter 2") (3 . "Scene C")
                     (3 . "Added") (1 . "Act II"))))))

(ert-deftest test-insert-scene-on-empty-act-creates-its-first-chapter ()
  "On an act with no chapter, a chapter is created first, named by the
writer, and the scene is its first scene.  A scene straight under the act
compiled as part of the previous act's last chapter, and the health
report counted the act as a chapter."
  (let (prompt)
    (cl-letf (((symbol-function 'read-string)
               (lambda (p &rest _) (setq prompt p) "Chapter 3")))
      (test-project--in-outline "Act II"
        (org-scribe-insert-scene "The lamp goes out")
        (should (string-match-p "Act II" prompt))
        (should (equal (test-project--headings)
                       '((1 . "Act I") (2 . "Chapter 1") (3 . "Scene A")
                         (3 . "Scene B") (2 . "Chapter 2") (3 . "Scene C")
                         (1 . "Act II") (2 . "Chapter 3")
                         (3 . "The lamp goes out"))))
        (should (string-match-p "^:PoV:" (buffer-substring (line-beginning-position)
                                                            (line-end-position))))
        (should (equal "The lamp goes out" (org-get-heading t t t t)))))))

(ert-deftest test-insert-scene-after-the-scene-point-is-in ()
  "From inside Scene A's drawer or text, the new scene follows Scene A."
  (dolist (where '(":PoV:" "Second paragraph"))
    (test-project--in-outline where
      (org-scribe-insert-scene "Next")
      (should (equal (mapcar #'cdr (test-project--headings))
                     '("Act I" "Chapter 1" "Scene A" "Next" "Scene B"
                       "Chapter 2" "Scene C" "Act II"))))))

(ert-deftest test-insert-scene-leaves-point-on-the-new-scenes-pov-line ()
  "Point ends on the new scene's :PoV: line wherever the scene went."
  (test-project--in-outline "with some"
    (org-scribe-insert-scene "Interrupted")
    (should (string-match-p "^:PoV: $" (buffer-substring (line-beginning-position)
                                                          (line-end-position))))
    (should (equal "Interrupted" (org-get-heading t t t t)))))

(ert-deftest test-insert-scene-and-chapter-a-typed-pov-is-read ()
  "A name typed where the cursor is left becomes the PoV.
With no space after \":PoV:\" the line read \":PoV:Xoán\", which Org does
not parse as a property, so the PoV was silently empty."
  (dolist (insert-fn (list (lambda () (org-scribe-insert-scene "Next"))
                           (lambda () (org-scribe-insert-chapter "Coda"))))
    (test-project--in-outline "Text of C"
      (funcall insert-fn)
      (insert "Xoán")
      (should (equal "Xoán" (org-entry-get nil "PoV"))))))

(ert-deftest test-insert-chapter-from-inside-a-scene-goes-after-that-chapter ()
  "A chapter added from inside Scene B follows Chapter 1, not Scene B's prose."
  (test-project--in-outline "Text of B"
    (org-scribe-insert-chapter "Interlude")
    (should (equal (test-project--headings)
                   '((1 . "Act I") (2 . "Chapter 1") (3 . "Scene A")
                     (3 . "Scene B") (2 . "Interlude") (3 . "New scene")
                     (2 . "Chapter 2") (3 . "Scene C") (1 . "Act II"))))))

(ert-deftest test-insert-scene-at-end-of-file-without-trailing-newline ()
  "The last subtree may lack a final newline; the heading must start a line."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'novel)))
    (with-temp-buffer
      (org-mode)
      (insert "** TODO Chapter 1 :ignore:\n*** TODO Scene A :ignore:\n\nlast words")
      (goto-char (point-max))
      (org-scribe-insert-scene "Next")
      (should (equal (mapcar #'cdr (test-project--headings))
                     '("Chapter 1" "Scene A" "Next")))
      (should (string-match-p "^last words$" (buffer-string))))))

(ert-deftest test-insert-scene-in-short-story-goes-after-current-scene ()
  "Short-story scenes are level 2 under a level-1 container."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (insert "* Story\n** TODO One :ignore:\n\nfirst scene text\n** TODO Two :ignore:\n\ntext\n")
      (goto-char (point-min))
      (search-forward "first scene")
      (org-scribe-insert-scene "Between")
      (should (equal (test-project--headings)
                     '((1 . "Story") (2 . "One") (2 . "Between") (2 . "Two")))))))

(ert-deftest test-insert-chapter-template ()
  "Test chapter template insertion includes chapter heading and first scene."
  (with-temp-buffer
    (org-mode)
    (org-scribe-insert-chapter "Chapter 1")

    (let ((content (buffer-string)))
      ;; Check chapter heading
      (should (string-match-p "\\*\\* TODO Chapter 1 :ignore:" content))

      ;; Check chapter properties
      (should (string-match-p ":WORD-OBJECTIVE: 3000" content))
      (should (string-match-p ":WORDCOUNT: 0" content))

      ;; Check first scene is present
      (should (string-match-p "\\*\\*\\* TODO .+ :ignore:" content))
      (should (string-match-p ":PoV:" content))
      (should (string-match-p ":Characters:" content))
      (should (string-match-p ":WORD-OBJECTIVE: 500" content))
      (should (string-match-p "{{{scene-break}}}" content)))))

(ert-deftest test-insert-chapter-empty-name ()
  "Test chapter template with empty name uses default."
  (with-temp-buffer
    (org-mode)
    (org-scribe-insert-chapter "")

    (let ((content (buffer-string)))
      (should (string-match-p "\\*\\* TODO New chapter :ignore:" content)))))

(ert-deftest test-insert-chapter-refuses-in-short-story-project ()
  "A short story has no chapter level at all (`org-scribe-chapter-level'
returns nil for it), so inserting a chapter must refuse with a
`user-error' naming the reason, rather than inserting a heading that
would collide with the project's own scene level."
  (cl-letf (((symbol-function 'org-scribe-project-type) (lambda () 'short-story)))
    (with-temp-buffer
      (org-mode)
      (should-error (org-scribe-insert-chapter "Chapter 1") :type 'user-error)
      ;; Nothing was inserted.
      (should (string-empty-p (buffer-string))))))

;;; Insertion from the preamble / apparatus, title and creation hardening

(defconst test-project--act-doc
  "#+TITLE: T

* Project Goals :ignore:noexport:
Notes.
* Act I
** Chapter 1
*** TODO One :ignore:
Text.
* Act II
** Chapter 2
*** TODO Two :ignore:
More.
"
  "A minimal manuscript: apparatus first, then two acts.")

(defmacro test-project--with-novel-buffer (&rest body)
  "Run BODY in an Org buffer holding `test-project--act-doc' as a novel."
  (declare (indent 0))
  `(with-temp-buffer
     (insert test-project--act-doc)
     (org-mode)
     (cl-letf (((symbol-function 'org-scribe-project-levels)
                (lambda (&optional _) '(:chapter 2 :scene 3 :scene-tag "ignore"))))
       ,@body)))

(ert-deftest test-insert-scene-from-preamble-goes-after-last-scene ()
  (test-project--with-novel-buffer
    (goto-char (point-min))
    (org-scribe-insert-scene "Fresh")
    (let ((text (buffer-string)))
      (should (string-match-p "More\\.\n\\*\\*\\* TODO Fresh" text))
      (should-not (string-match-p "\\`\\*\\*\\* TODO Fresh" text)))
    (org-back-to-heading t)
    (should (equal "Fresh" (org-get-heading t t t t)))
    (should-not (member "noexport" (org-get-tags)))))

(ert-deftest test-insert-scene-from-noexport-apparatus-leaves-it-alone ()
  (test-project--with-novel-buffer
    (goto-char (point-min))
    (search-forward "Notes.")
    (org-scribe-insert-scene "Fresh")
    (goto-char (point-min))
    (re-search-forward "^\\*\\*\\* TODO Fresh")
    (should-not (member "noexport" (org-get-tags)))
    (should (equal "Act II" (save-excursion
                              (org-up-heading-safe) (org-up-heading-safe)
                              (org-get-heading t t t t))))))

(ert-deftest test-insert-chapter-from-preamble-goes-in-last-act ()
  (test-project--with-novel-buffer
    (goto-char (point-min))
    (org-scribe-insert-chapter "Late")
    (goto-char (point-min))
    (re-search-forward "^\\*\\* TODO Late")
    (should (equal "Act II" (save-excursion
                              (org-up-heading-safe)
                              (org-get-heading t t t t))))))

(ert-deftest test-insert-scene-from-apparatus-ignores-tag-inheritance-settings ()
  "Apparatus is recognised with tag inheritance off or narrowed."
  (dolist (setting '((org-use-tag-inheritance . nil)
                     (org-tags-exclude-from-inheritance . ("noexport"))))
    (with-temp-buffer
      (insert "* Progress :ignore:noexport:\n** Daily\nLog.\n"
              "* Act I\n** Chapter 1\n*** TODO One :ignore:\nText.\n")
      (org-mode)
      (cl-letf (((symbol-function 'org-scribe-project-levels)
                 (lambda (&optional _) '(:chapter 2 :scene 3 :scene-tag "ignore"))))
        (set (make-local-variable (car setting)) (cdr setting))
        (goto-char (point-min))
        (search-forward "Log.")
        (org-scribe-insert-scene "Fresh")
        (org-back-to-heading t)
        (should (equal "Fresh" (org-get-heading t t t t)))
        (org-up-heading-safe)
        (should (equal "Chapter 1" (org-get-heading t t t t)))))))

(ert-deftest test-insert-scene-in-apparatus-without-narrative-is-not-glued ()
  "With no narrative heading at all, the scene still starts on its own line."
  (with-temp-buffer
    (insert "#+TITLE: x\n* Goals :noexport:\nSome text here\n")
    (org-mode)
    (cl-letf (((symbol-function 'org-scribe-project-levels)
               (lambda (&optional _) '(:chapter 2 :scene 3 :scene-tag "ignore"))))
      (goto-char (point-min))
      (search-forward "Some te")
      (org-scribe-insert-scene "C")
      (should (string-match-p "^Some text here\n\\*\\*\\* TODO C" (buffer-string))))))

(ert-deftest test-validate-title-rejects-newline-and-padding ()
  (should (org-scribe--validate-project-title "Multi\nline"))
  (should (org-scribe--validate-project-title "  Padded  "))
  (should (org-scribe--validate-project-title (make-string 400 ?a)))
  (should-not (org-scribe--validate-project-title "Plain Title")))

(ert-deftest test-process-template-string-is-single-pass ()
  "A value holding another variable's token is not rewritten."
  (should (equal "Dollar ${AUTHOR} X / Ana"
                 (org-scribe--process-template-string
                  "${TITLE} / ${AUTHOR}"
                  '(("TITLE" . "Dollar ${AUTHOR} X") ("AUTHOR" . "Ana")))))
  (should (equal "${UNKNOWN}"
                 (org-scribe--process-template-string "${UNKNOWN}" nil))))

(ert-deftest test-default-author-prefers-configured-author-name ()
  (let ((org-scribe-author-name "Ana Perez") (user-full-name "jcastp"))
    (should (equal "Ana Perez" (org-scribe--default-author))))
  (let ((org-scribe-author-name nil) (user-full-name "jcastp"))
    (should (equal "jcastp" (org-scribe--default-author))))
  (let ((org-scribe-author-name "") (user-full-name ""))
    (should (equal "Author" (org-scribe--default-author)))))

(ert-deftest test-create-novel-project-percent-title-and-no-git ()
  "A `%' in the title and a missing git neither abort nor half-build."
  (test-project--with-temp-base-dir base
    (let ((exec-path nil)
          (process-environment (cons "PATH=/nonexistent" process-environment)))
      (cl-letf (((symbol-function 'project-remember-project) #'ignore)
                ((symbol-function 'find-file) #'ignore))
        (org-scribe-create-novel-project base "50% Off" 'en 'sistema 80000)))
    (let ((dir (expand-file-name "50% Off" base)))
      (should (file-exists-p (expand-file-name "novel.org" dir)))
      (should (file-exists-p (expand-file-name ".org-scribe-project" dir))))))

;;; Configuration Tests

(ert-deftest test-template-language-configured ()
  "Test that template language is configured."
  (should (boundp 'org-scribe-template-language))
  (should (memq org-scribe-template-language '(en es))))

;;; Backward Compatibility Tests

(ert-deftest test-backward-compatibility-aliases ()
  "Test that old function names are still available."
  ;; Old function names should be callable
  (should (fboundp 'org-scribe-project-create-novel-project))
  (should (fboundp 'org-scribe-create-project))
  (should (fboundp 'org-scribe-project-insert-scene))
  (should (fboundp 'org-scribe-project-insert-chapter))
  (should (fboundp 'org-scribe-project-open-novel-file))

  ;; And should be marked as aliases (indirect functions)
  (should (symbolp (symbol-function 'org-scribe-project-create-novel-project)))
  (should (symbolp (symbol-function 'org-scribe-create-project))))

(ert-deftest test-obsolete-functions-marked ()
  "Test that old functions are marked as obsolete."
  (should (get 'org-scribe-project-create-novel-project 'byte-obsolete-info))
  (should (get 'org-scribe-create-project 'byte-obsolete-info))
  (should (get 'org-scribe-project-insert-scene 'byte-obsolete-info))
  (should (get 'org-scribe-project-insert-chapter 'byte-obsolete-info))
  (should (get 'org-scribe-project-open-novel-file 'byte-obsolete-info)))

;;; Integration Tests (require actual template files)

(ert-deftest test-template-no-plan-org-in-any-template ()
  "No template directory ships a plan.org file (Phase 4: per-project planner gate).
A project only gets a plan.org once it explicitly opts into the planner
via `org-scribe-planner-new-plan' or the offer-on-create prompt — never
as a template artifact present from project creation."
  (let ((templates-root (expand-file-name
                         "../org-scribe-templates/" org-scribe-project-package-directory)))
    (dolist (dir '("novel-en" "novel-es" "short-story-en" "short-story-es"))
      (let ((plan (expand-file-name "plan.org" (expand-file-name dir templates-root))))
        (should-not (file-exists-p plan))))))

;;; Project Creation Language Selection Tests

(defmacro test-project--with-temp-base-dir (var &rest body)
  "Bind VAR to a fresh temp directory for BODY, then delete it."
  (declare (indent 1))
  `(let ((,var (make-temp-file "org-scribe-project-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,var t))))

(defun test-project--kill-file-buffer (file)
  "Kill any buffer visiting FILE, if one was opened by project creation."
  (let ((buf (get-file-buffer file)))
    (when buf (kill-buffer buf))))

(ert-deftest test-create-novel-project-spanish-templates ()
  "Creating a novel project with LANGUAGE \\='es uses the Spanish templates."
  (test-project--with-temp-base-dir base-dir
    (let ((project-dir (expand-file-name "Mi Novela" base-dir)))
      (org-scribe-create-novel-project base-dir "Mi Novela" 'es)
      (unwind-protect
          (progn
            (should (file-exists-p (expand-file-name (org-scribe-lang-file 'manuscript-novel 'es) project-dir)))
            (should-not (file-exists-p (expand-file-name (org-scribe-lang-file 'manuscript-novel 'en) project-dir)))
            (with-temp-buffer
              (insert-file-contents (expand-file-name ".org-scribe-project" project-dir))
              (should (string-match-p "# Language: es" (buffer-string)))))
        (test-project--kill-file-buffer (expand-file-name "README.org" project-dir))))))

(ert-deftest test-create-novel-project-english-default ()
  "Omitting LANGUAGE keeps the default English templates."
  (test-project--with-temp-base-dir base-dir
    (let ((project-dir (expand-file-name "My Novel" base-dir)))
      (org-scribe-create-novel-project base-dir "My Novel")
      (unwind-protect
          (should (file-exists-p (expand-file-name "novel.org" project-dir)))
        (test-project--kill-file-buffer (expand-file-name "README.org" project-dir))))))

(defun test-project--git-output (dir &rest args)
  "Return the output of git ARGS run in DIR."
  (let ((default-directory (file-name-as-directory dir)))
    (with-output-to-string
      (with-current-buffer standard-output
        (apply #'call-process "git" nil t nil args)))))

(ert-deftest test-create-novel-project-repo-clean-after-plan-offer ()
  "Whatever the plan offer writes is committed: accepting (marker lines and
plan.org), declining (the Planner: no line), or quitting midway.  The
initial commit came before the offer, so the new repository was dirty."
  (skip-unless (executable-find "git"))
  ;; Load it first: loading it later would replace the stub below.
  (require 'org-scribe-planner)
  (let ((process-environment
         (append '("GIT_AUTHOR_NAME=T" "GIT_AUTHOR_EMAIL=t@example.org"
                   "GIT_COMMITTER_NAME=T" "GIT_COMMITTER_EMAIL=t@example.org")
                 process-environment))
        (org-scribe-planner-offer-plan-on-create t))
    (dolist (answer '(accept decline quit))
      (test-project--with-temp-base-dir base-dir
        (let ((project-dir (expand-file-name "My Novel" base-dir)))
          (org-scribe-create-novel-project base-dir "My Novel" 'en 'sistema 80000)
          (unwind-protect
              (progn
                (should (string-empty-p (test-project--git-output project-dir "status" "--porcelain")))
                (cl-letf (((symbol-function 'org-scribe-planner--offer-plan-on-create)
                           (lambda (dir _title)
                             (pcase answer
                               ('accept
                                (with-temp-file (expand-file-name "plan.org" dir) (insert "* Plan\n"))
                                (org-scribe--project-marker-set dir "Planner" "yes")
                                (org-scribe--project-marker-set dir "Plan" "plan.org"))
                               ('decline
                                (org-scribe--project-marker-set dir "Planner" "no"))
                               ('quit
                                (org-scribe--project-marker-set dir "Planner" "yes")
                                (signal 'quit nil))))))
                  (condition-case nil
                      (org-scribe--offer-plan-after-create project-dir "My Novel")
                    (quit nil)))
                (should (string-empty-p (test-project--git-output project-dir "status" "--porcelain")))
                (should (= 2 (length (split-string (test-project--git-output
                                                    project-dir "log" "--oneline")
                                                   "\n" t))))
                (when (eq answer 'accept)
                  (should (string-match-p "plan.org" (test-project--git-output
                                                      project-dir "ls-files")))))
            (test-project--kill-file-buffer (expand-file-name "novel.org" project-dir))))))))

(ert-deftest test-create-project-plan-offer-commits-nothing-when-nothing-changed ()
  "No second commit when the offer changes nothing (offer turned off)."
  (skip-unless (executable-find "git"))
  ;; Load it first, so the `let' below binds its (special) variable.
  (require 'org-scribe-planner)
  (let ((process-environment
         (append '("GIT_AUTHOR_NAME=T" "GIT_AUTHOR_EMAIL=t@example.org"
                   "GIT_COMMITTER_NAME=T" "GIT_COMMITTER_EMAIL=t@example.org")
                 process-environment))
        (org-scribe-planner-offer-plan-on-create nil))
    (test-project--with-temp-base-dir base-dir
      (let ((project-dir (expand-file-name "My Novel" base-dir)))
        (org-scribe-create-novel-project base-dir "My Novel" 'en 'sistema 80000)
        (unwind-protect
            (progn
              (org-scribe--offer-plan-after-create project-dir "My Novel")
              (should (= 1 (length (split-string (test-project--git-output
                                                  project-dir "log" "--oneline")
                                                 "\n" t)))))
          (test-project--kill-file-buffer (expand-file-name "novel.org" project-dir)))))))

(ert-deftest test-open-project-file-prompt-default-and-match ()
  "F8 o prompts from the pack, defaults to the manuscript (the notebook
from the manuscript), requires a match, and opens nothing on an empty
answer.  It used a literal English prompt with no default and no match,
so RET opened the project root in Dired."
  (test-project--with-temp-base-dir base-dir
    (let ((root (file-name-as-directory base-dir))
          (org-scribe-message-language 'es)
          seen)
      (with-temp-file (expand-file-name ".org-scribe-project" root)
        (insert "# Type: novel\n# Language: en\n"))
      (dolist (file '("novel.org" "design.org"))
        (with-temp-file (expand-file-name file root) (insert "* H\n")))
      (org-scribe-project-type-cache-clear)
      (unwind-protect
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt collection _pred require-match _init _hist default)
                       (setq seen (list prompt require-match default collection))
                       "")))
            (let ((default-directory root))
              (with-temp-buffer
                (setq default-directory root)
                (should-error (call-interactively #'org-scribe-open-project-file)
                              :type 'user-error)
                (should (string-prefix-p "Abrir archivo" (nth 0 seen)))
                (should (eq t (nth 1 seen)))
                (should (equal "novel.org" (nth 2 seen)))
                (should (member "design.org" (nth 3 seen)))
                (should-not (derived-mode-p 'dired-mode)))
              (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
                (should-error (call-interactively #'org-scribe-open-project-file)
                              :type 'user-error)
                (should (equal "design.org" (nth 2 seen)))
                (kill-buffer))))
        (org-scribe-project-type-cache-clear)
        (when-let* ((dired (get-buffer (file-name-nondirectory (directory-file-name root)))))
          (kill-buffer dired))))))

(ert-deftest test-create-short-story-project-spanish-templates ()
  "Creating a short story project with LANGUAGE \\='es uses cuento.org."
  (test-project--with-temp-base-dir base-dir
    (let ((project-dir (expand-file-name "Mi Cuento" base-dir))
          (manuscript-es (org-scribe-lang-file 'manuscript-short 'es))
          (manuscript-en (org-scribe-lang-file 'manuscript-short 'en)))
      (org-scribe-create-short-story-project base-dir "Mi Cuento" 'es)
      (unwind-protect
          (progn
            (should (file-exists-p (expand-file-name manuscript-es project-dir)))
            (should-not (file-exists-p (expand-file-name manuscript-en project-dir))))
        (test-project--kill-file-buffer (expand-file-name manuscript-es project-dir))))))

(ert-deftest test-create-short-story-project-mints-entity-ids ()
  "A new short story's template characters and settings are linkable at once.
Novel creation always minted IDs; short-story creation did not, and every
entity getter skips an ID-less heading — so a fresh short story had no
characters to set as PoV, and the health report reported \"no orphaned
characters\" while three placeholders sat in the design file.  The IDs must
also be part of the initial commit, not a dirty working tree."
  (require 'org-scribe-character-links)
  (require 'org-scribe-location-links)
  (dolist (language '(en es))
    (test-project--with-temp-base-dir base-dir
      (let* ((title (format "Story %s" language))
             (project-dir (expand-file-name title base-dir))
             (notes (expand-file-name (org-scribe-lang-file 'design language)
                                      project-dir)))
        (org-scribe-create-short-story-project base-dir title language)
        (unwind-protect
            (let ((default-directory (file-name-as-directory project-dir)))
              (let ((characters (org-scribe--get-all-characters))
                    (locations (org-scribe--get-all-locations)))
                (should (= 3 (length characters)))
                (should (>= (length locations) 1))
                (dolist (entity (append characters locations))
                  (should (org-string-nw-p (cadr entity)))))
              (should (string-empty-p
                       (shell-command-to-string "git status --porcelain"))))
          (test-project--kill-file-buffer notes)
          (test-project--kill-file-buffer
           (expand-file-name (org-scribe-lang-file 'manuscript-short language)
                             project-dir)))))))

;;; Project File Navigation Tests

(ert-deftest test-project-file-candidates-scans-project ()
  "Completion candidates come from disk, not a hardcoded list."
  (let* ((manuscript (org-scribe-lang-file 'manuscript-novel 'es))
         (design (org-scribe-lang-file 'design 'es)))
    (test-project--with-temp-base-dir root
      (dolist (file (list manuscript design "extra.org" ".org-scribe-project"))
        (with-temp-file (expand-file-name file root) (insert "x")))
      (let ((candidates (org-scribe--project-file-candidates root)))
        (should (member design candidates))
        (should (member manuscript candidates))
        (should (member "extra.org" candidates))
        ;; Non-Org files and the marker file are not offered.
        (should-not (member ".org-scribe-project" candidates))
        ;; Sorted, so completion order is stable.
        (should (equal candidates (sort (copy-sequence candidates) #'string<)))))))

(ert-deftest test-project-file-candidates-fallback-without-root ()
  "With no project root the static fallback list is used, and it
includes the design file under both language names."
  (let ((candidates (org-scribe--project-file-candidates nil)))
    (should (equal candidates org-scribe--known-project-files))
    (should (member (org-scribe-lang-file 'design 'en) candidates))
    (should (member (org-scribe-lang-file 'design 'es) candidates))))

(ert-deftest test-project-known-files-match-shipped-templates ()
  "Every Org file the novel templates ship appears in the fallback list.
A template that adds a file must not silently drop out of completion
for users outside a detectable project."
  (let ((templates-root (expand-file-name
                         "../org-scribe-templates/" org-scribe-project-package-directory)))
    (dolist (set '("novel-en" "novel-es"))
      (let ((dir (expand-file-name set templates-root)))
        (dolist (file (directory-files-recursively dir "\\.org\\.template\\'"))
          (let ((relative (string-remove-suffix
                           ".template" (file-relative-name file dir))))
            (should (member relative org-scribe--known-project-files))))))))

;;; org-scribe-create-novel-project + METHOD

(ert-deftest test-create-novel-project-sistema-is-byte-identical-to-omitted-method ()
  "Explicitly passing METHOD \\='sistema produces the same design.org as
omitting METHOD altogether — the legacy-preservation guarantee (Q6):
Sistema's own design file is never touched by this feature."
  (test-project--with-temp-base-dir base-dir
    (test-project--with-temp-base-dir base-dir-2
      (let ((dir-a (expand-file-name "Novel A" base-dir))
            (dir-b (expand-file-name "Novel B" base-dir-2)))
        (org-scribe-create-novel-project base-dir "Novel A" 'en)
        (org-scribe-create-novel-project base-dir-2 "Novel B" 'en 'sistema)
        (unwind-protect
            (let ((a (with-temp-buffer
                       (insert-file-contents (expand-file-name "design.org" dir-a))
                       (buffer-string)))
                  (b (with-temp-buffer
                       (insert-file-contents (expand-file-name "design.org" dir-b))
                       (buffer-string))))
              ;; Both were substituted with their own TITLE, so compare with
              ;; that difference normalized rather than asserting equality
              ;; on the raw text.
              (cl-flet ((norm (text title)
                          (replace-regexp-in-string
                           ":ID:.*" ":ID:" (replace-regexp-in-string title "X" text))))
                (should (string= (norm a "Novel A") (norm b "Novel B")))))
          (test-project--kill-file-buffer (expand-file-name "README.org" dir-a))
          (test-project--kill-file-buffer (expand-file-name "README.org" dir-b)))))))

(ert-deftest test-create-novel-project-sistema-marker-line-written ()
  "A project created with no METHOD argument still gets an explicit
'# Method: sistema' line, matching what `org-scribe-project-method'
resolves an absent line to."
  (test-project--with-temp-base-dir base-dir
    (let ((project-dir (expand-file-name "Sistema Novel" base-dir)))
      (org-scribe-create-novel-project base-dir "Sistema Novel" 'en)
      (unwind-protect
          (should (eq 'sistema (org-scribe-project-method project-dir)))
        (test-project--kill-file-buffer (expand-file-name "README.org" project-dir))))))

(defun test-project--foundations-part (file)
  "Return the text of the level-1 Foundations part of notebook FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((case-fold-search nil))
      (re-search-forward "^\\* \\(Foundations\\|Fundamentos\\)$")
      (let ((start (point)))
        (if (re-search-forward "^\\* " nil t)
            (buffer-substring start (match-beginning 0))
          (buffer-substring start (point-max)))))))

(ert-deftest test-create-novel-project-helice-foundations-hold-the-method ()
  "METHOD \\='helice puts the Hélice's fragment in the notebook's Foundations
part, and the marker records the method."
  (test-project--with-temp-base-dir base-dir
    (let* ((project-dir (expand-file-name "Helice Novel" base-dir))
           (design (expand-file-name "design.org" project-dir)))
      (org-scribe-create-novel-project base-dir "Helice Novel" 'en 'helice)
      (should (eq 'helice (org-scribe-project-method project-dir)))
      (let ((found (test-project--foundations-part design)))
        (should (string-match-p "^\\*\\* The Helix method$" found))
        (should (string-match-p "^\\*\\* Starting Gate$" found))
        (should-not (string-match-p "^\\*\\* Core$" found))
        (should-not (string-match-p "\\${" found))))))

(ert-deftest test-create-novel-project-matriz-foundations-spanish ()
  "METHOD \\='matriz with Spanish templates writes diseno.org, not design.org,
with the Matriz's fragment under Fundamentos."
  (let ((design-es (org-scribe-lang-file 'design 'es))
        (design-en (org-scribe-lang-file 'design 'en)))
    (test-project--with-temp-base-dir base-dir
      (let ((project-dir (expand-file-name "Novela Matriz" base-dir)))
        (org-scribe-create-novel-project base-dir "Novela Matriz" 'es 'matriz)
        (should (file-exists-p (expand-file-name design-es project-dir)))
        (should-not (file-exists-p (expand-file-name design-en project-dir)))
        (should (eq 'matriz (org-scribe-project-method project-dir)))
        (should (string-match-p
                 "^\\*\\* El método de la Matriz$"
                 (test-project--foundations-part
                  (expand-file-name design-es project-dir))))))))

(ert-deftest test-create-novel-project-leaves-no-fragment-files ()
  "Fragments are template inputs: no method leaves a *.fragment in the project."
  (dolist (method '(sistema helice matriz))
    (test-project--with-temp-base-dir base-dir
      (let ((project-dir (expand-file-name "Fragments" base-dir)))
        (org-scribe-create-novel-project base-dir "Fragments" 'en method)
        (should-not (directory-files-recursively project-dir "\\.fragment\\'"))))))

(ert-deftest test-create-novel-project-foundations-fit-every-method ()
  "The Hélice and Matriz gates have 6 and 7 boxes and neither has a Core:
the Sistema's Core heading must not appear in their Foundations part."
  (dolist (lang '(en es))
    (dolist (method '(helice matriz))
      (test-project--with-temp-base-dir base-dir
        (let ((project-dir (expand-file-name "Gate Check" base-dir)))
          (org-scribe-create-novel-project base-dir "Gate Check" lang method)
          (should-not (string-match-p
                       "^\\*\\* \\(Core\\|Núcleo\\)$"
                       (test-project--foundations-part
                        (expand-file-name (org-scribe-lang-file 'design lang)
                                          project-dir)))))))))

(ert-deftest test-create-novel-project-method-changes-only-foundations ()
  "Choosing a method changes only the Foundations part of the notebook;
novel.org and every other part are the same as a Sistema project's.

Project titles are deliberately not \"Sistema\"/\"Helice\": those words
legitimately appear in shared template prose, so a title-stripping
replacement keyed on the method name would mangle it."
  (test-project--with-temp-base-dir base-dir
    (test-project--with-temp-base-dir base-dir-2
      (let ((sistema-dir (expand-file-name "Alfaproj" base-dir))
            (helice-dir (expand-file-name "Betaproj" base-dir-2)))
        (org-scribe-create-novel-project base-dir "Alfaproj" 'en)
        (org-scribe-create-novel-project base-dir-2 "Betaproj" 'en 'helice)
        (dolist (relative '("novel.org" "design.org"))
          ;; `:ID:' lines are minted fresh on every creation, so they are
          ;; normalized out with the title before comparing.
          (cl-flet ((norm (dir title)
                      (with-temp-buffer
                        (insert-file-contents (expand-file-name relative dir))
                        (let ((text (replace-regexp-in-string
                                     ":ID:.*" ":ID:"
                                     (replace-regexp-in-string title "X" (buffer-string)))))
                          ;; Drop the Foundations part: it is what differs.
                          (if (string= relative "design.org")
                              (concat (substring text 0 (string-match "^\\* Foundations$" text))
                                      (substring text (string-match "^\\* Plot$" text)))
                            text)))))
            (should (string= (norm sistema-dir "Alfaproj")
                             (norm helice-dir "Betaproj")))))))))

;;; Foundations fragment (org-scribe--foundations-fragment)

(ert-deftest test-project-foundations-fragment-sistema-reads-base-set ()
  "Sistema has no overlay: its fragment is the base set's, and nothing
under the package directory is required."
  (test-project--with-temp-base-dir base-dir
    (let ((org-scribe-project-package-directory "/nonexistent/package/dir/"))
      (with-temp-file (expand-file-name "foundations.fragment" base-dir)
        (insert "** Gate for ${TITLE}\n"))
      (should (string= "** Gate for T\n"
                       (org-scribe--foundations-fragment
                        'sistema 'en base-dir '(("TITLE" . "T"))))))))

(ert-deftest test-project-foundations-fragment-overlay-substitutes ()
  "A method with an overlay reads that directory's fragment, substituting
variables exactly as the base copy does."
  (test-project--with-temp-base-dir package-root
    (let* ((overlay-dir (expand-file-name "org-scribe-templates/methods/helice/en" package-root))
           (org-scribe-project-package-directory
            (file-name-as-directory (expand-file-name "templates" package-root))))
      (make-directory overlay-dir t)
      (with-temp-file (expand-file-name "foundations.fragment" overlay-dir)
        (insert "** Helix for ${TITLE} by ${AUTHOR}\n"))
      (should (string= "** Helix for My Novel by Jane\n"
                       (org-scribe--foundations-fragment
                        'helice 'en "/unused/"
                        '(("TITLE" . "My Novel") ("AUTHOR" . "Jane"))))))))

(ert-deftest test-project-foundations-fragment-errors-when-missing ()
  "A method that declares an overlay but has no fragment on disk errors
loudly rather than leaving the Foundations part empty."
  (let ((org-scribe-project-package-directory
         (file-name-as-directory (make-temp-file "org-scribe-empty-pkg-" t))))
    (unwind-protect
        (should-error
         (org-scribe--foundations-fragment 'helice 'en "/unused/" '(("TITLE" . "T")))
         :type 'user-error)
      (delete-directory org-scribe-project-package-directory t))))

(ert-deftest test-project-copy-templates-skips-fragments ()
  "`org-scribe--copy-templates' never copies a *.fragment file."
  (test-project--with-temp-base-dir template-dir
    (test-project--with-temp-base-dir project-dir
      (with-temp-file (expand-file-name "foundations.fragment" template-dir) (insert "x"))
      (with-temp-file (expand-file-name "a.org.template" template-dir) (insert "${FOUNDATIONS}"))
      (org-scribe--copy-templates template-dir project-dir '(("FOUNDATIONS" . "body")))
      (should-not (file-exists-p (expand-file-name "foundations.fragment" project-dir)))
      (should (string= "body" (test-project--read project-dir "a.org"))))))

;;; Run tests

(defun org-scribe-project-run-tests ()
  "Run all project creation tests."
  (interactive)
  (ert "^test-project-\\|^test-validate-\\|^test-template-\\|^test-insert-\\|^test-backward-"))

;;; Total word target at creation

(defun test-project--read (&rest parts)
  "Return the text of the file named by PARTS joined as a path."
  (with-temp-buffer
    (insert-file-contents (apply #'expand-file-name (list (mapconcat #'identity (cdr parts) "/") (car parts))))
    (buffer-string)))

(defmacro test-project--with-created (dir-var kind title language target &rest body)
  "Create a KIND project TITLE in LANGUAGE with TARGET words; run BODY.
DIR-VAR is bound to the project directory."
  (declare (indent 5))
  `(test-project--with-temp-base-dir base
     (let ((,dir-var (expand-file-name ,title base)))
       (cl-letf (((symbol-function 'message) #'ignore))
         (if (eq ,kind 'novel)
             (org-scribe-create-novel-project base ,title ,language nil ,target)
           (org-scribe-create-short-story-project base ,title ,language ,target)))
       (unwind-protect (progn ,@body)
         (dolist (buf (buffer-list))
           (when (and (buffer-file-name buf)
                      (string-prefix-p (file-name-as-directory ,dir-var)
                                       (buffer-file-name buf)))
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf)))))))

(defun test-project--objectives (text)
  "Return the WORD-OBJECTIVE values in TEXT, in order, as integers."
  (let (values (start 0))
    (while (string-match ":WORD-OBJECTIVE: +\\([0-9]+\\)" text start)
      (push (string-to-number (match-string 1 text)) values)
      (setq start (match-end 0)))
    (nreverse values)))

(ert-deftest test-project-word-target-novel-explicit ()
  (test-project--with-created dir 'novel "Novel" 'en 50000
    (should (= (org-scribe-project-word-target dir) 50000))
    (let ((novel (test-project--read dir "novel.org")))
      (should (equal (test-project--objectives novel) '(12500 3000 500 25000 12500)))
      (should (string-match-p "^- Total word target :: 50,000$" novel)))
    (dolist (file (directory-files-recursively dir "\\.org$"))
      (should-not (string-match-p "\\${" (test-project--read file))))))

(ert-deftest test-project-word-target-novel-default ()
  (test-project--with-created dir 'novel "Novel" 'en nil
    (should (= (org-scribe-project-word-target dir) 80000))
    (should (equal (test-project--objectives (test-project--read dir "novel.org"))
                   '(20000 3000 500 40000 20000)))))

(ert-deftest test-project-word-target-novel-spanish-separator ()
  (test-project--with-created dir 'novel "Novela" 'es nil
    (should (string-match-p "^- Objetivo total de palabras :: 80\\.000$"
                            (test-project--read dir (org-scribe-lang-file 'manuscript-novel 'es))))))

(ert-deftest test-project-word-target-short-story ()
  "A short story keeps the whole target on its container; the scene has none."
  (dolist (case '((5000 . (5000)) (8000 . (8000))))
    (test-project--with-created dir 'short-story "Story" 'en (car case)
      (let ((story (test-project--read dir "story.org")))
        (should (equal (test-project--objectives story) (cdr case)))
        (should (string-match-p (format "^:TARGET_WORDCOUNT: %d$" (car case)) story)))
      (should (= (org-scribe-project-word-target dir) (car case))))))

(ert-deftest test-project-word-target-bad-proportions-leave-nothing-behind ()
  (test-project--with-temp-base-dir base
    (let ((org-scribe-word-target-proportions '(1 1)))
      (should-error (org-scribe-create-novel-project base "Novel" 'en nil 50000)
                    :type 'user-error)
      (should-not (directory-files base nil "\\`[^.]")))))

(ert-deftest test-project-word-target-short-story-ignores-proportions ()
  "A short story is not split, so unusable proportions do not block it."
  (let ((org-scribe-word-target-proportions '(1 1)))
    (test-project--with-created dir 'short-story "Story" 'en 5000
      (should (equal (test-project--objectives (test-project--read dir "story.org"))
                     '(5000))))))

(ert-deftest test-project-word-target-bad-total-leaves-nothing-behind ()
  (test-project--with-temp-base-dir base
    (dolist (bad '(0 -5 2 12.5))
      (should-error (org-scribe-create-novel-project base "Novel" 'en nil bad)
                    :type 'user-error))
    (should-not (directory-files base nil "\\`[^.]"))))

(ert-deftest test-project-word-target-same-for-every-method ()
  (dolist (method '(sistema helice matriz))
    (test-project--with-temp-base-dir base
      (cl-letf (((symbol-function 'message) #'ignore))
        (org-scribe-create-novel-project base "Novel" 'en method 60000))
      (let ((dir (expand-file-name "Novel" base)))
        (unwind-protect
            (progn
              (should (= (org-scribe-project-word-target dir) 60000))
              (should (equal (test-project--objectives (test-project--read dir "novel.org"))
                             '(15000 3000 500 30000 15000))))
          (test-project--kill-file-buffer (expand-file-name "README.org" dir)))))))

(ert-deftest test-project-word-target-prompt-offers-the-default ()
  (let ((offered nil)
        (org-scribe-default-word-target '((novel . 70000) (short-story . 4000))))
    (cl-letf (((symbol-function 'read-number)
               (lambda (_prompt &optional default) (setq offered default) default)))
      (should (= (org-scribe--read-word-target 'novel) 70000))
      (should (= offered 70000))
      (org-scribe--read-word-target 'short-story)
      (should (= offered 4000)))))

(ert-deftest test-project-word-target-prompt-asks-again-on-too-small ()
  (let ((answers (list 1 2 9000)) (asked 0))
    (cl-letf (((symbol-function 'read-number)
               (lambda (&rest _) (cl-incf asked) (pop answers))))
      (should (= (org-scribe--read-word-target 'novel) 9000))
      (should (= asked 3)))))

(provide 'test-project)

;;; test-project.el ends here
