;;; test-compile.el --- Tests for manuscript compilation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;;; Commentary:

;; Tests for `org-scribe-compile' (export/org-scribe-compile.el).
;;
;; These assert on *real exported output* wherever the claim is about
;; what a reader ends up seeing, not only on the intermediate Org
;; document.  That is deliberate: the ODT scene-break bug this module
;; exists to fix was invisible in every intermediate representation and
;; only appeared in the emitted XML, so an intermediate-only suite would
;; have passed while the manuscript was corrupt.

;;; Code:

(require 'ert)
(require 'org)

(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../export" default-directory)))

(require 'org-scribe-messages)
(require 'org-scribe-core)
(require 'org-scribe-config)
(require 'org-scribe-compile)

;;; Fixtures

(defconst org-scribe-compile-test--novel "\
#+TITLE: The Drowning House
#+AUTHOR: A Writer
#+LANGUAGE: en
#+MACRO: scene-break SCENE-BREAK
#+OPTIONS: todo:nil tags:nil
#+TODO: TODO TOWRITE | FINISHED

* Project Goals :ignore:noexport:
- Total word target :: 80,000

* Progress Tracking :ignore:noexport:
#+BEGIN: org-generate-wordcount-table
| Heading | Wordcount |
#+END:

* Act I :ignore:
:PROPERTIES:
:CUSTOM_ID: act-1
:END:

** TODO Chapter 1 :ignore:
:PROPERTIES:
:WORDCOUNT: 120
:END:

*** TODO Scene 1 :ignore:
:PROPERTIES:
:PoV: [[id:c1][Alice]]
:END:

The door creaked open onto water.

#+begin_comment
*EDIT*: plot - check the tide table here
#+end_comment

Alice counted the stairs going down.

{{{scene-break}}}
*** TODO Scene 2 :ignore:
Rain arrived by evening.

{{{scene-break}}}

Later, the house had settled.

** TODO Chapter 2 :ignore:
*** TODO Scene 1 :ignore:
Morning found nothing changed.

*** TOWRITE Scene 2 :ignore:

* Act II :ignore:
** TODO Chapter 3 :ignore:
*** TODO Scene 1 :ignore:
The last chapter began here.
"
  "A novel manuscript shaped like the shipped template.")

(defconst org-scribe-compile-test--short-story "\
#+TITLE: Small Hours
#+AUTHOR: A Writer
#+LANGUAGE: en
#+MACRO: scene-break SCENE-BREAK
#+OPTIONS: todo:nil tags:nil

* Story Info :noexport:
:PROPERTIES:
:GENRE: Literary
:END:

* Story Content

** Opening
:PROPERTIES:
:WORDCOUNT: 40
:END:

She waited by the window.

** Middle
The train did not come.

** Ending
By dawn she had stopped waiting.
"
  "A short-story manuscript shaped like the shipped template.")

(defmacro org-scribe-compile-test--with-project (type filename content &rest body)
  "Run BODY in a throwaway org-scribe project of TYPE.
FILENAME holds CONTENT.  Binds `root' and `manuscript', and sets
`default-directory' so `org-scribe-project-root' resolves to the
project."
  (declare (indent 3))
  `(let* ((root (file-name-as-directory (make-temp-file "org-scribe-compile-" t)))
          (manuscript (expand-file-name ,filename root))
          (default-directory root))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".org-scribe-project" root)
             (insert (format "# Writing project: Test\n# Type: %s\n" ,type)))
           (with-temp-file manuscript (insert ,content))
           (org-scribe-project-type-cache-clear)
           ,@body)
       (org-scribe-project-type-cache-clear)
       (delete-directory root t))))

(defun org-scribe-compile-test--intermediate (root name)
  "Return the text of the intermediate Org file for NAME under ROOT."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name (concat name "-clean.org")
                       (expand-file-name org-scribe-compile-output-directory root)))
    (buffer-string)))

(defun org-scribe-compile-test--read (file)
  "Return the contents of FILE."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun org-scribe-compile-test--count (needle haystack)
  "Return how many times NEEDLE occurs in HAYSTACK."
  (let ((start 0) (n 0))
    (while (string-match (regexp-quote needle) haystack start)
      (setq n (1+ n) start (match-end 0)))
    n))

(defvar org-scribe-compile-test--odt-available
  (and (require 'ox-odt nil t) (fboundp 'org-odt-export-to-odt))
  "Non-nil when this Emacs can export ODT at all.")

;;; Definition

(ert-deftest test-compile-functions-defined ()
  "The compile entry points are defined."
  (should (fboundp 'org-scribe-compile))
  (should (fboundp 'org-scribe-compile-normalize))
  (should (boundp 'org-scribe-compile-scene-break))
  (should (boundp 'org-scribe-compile-output-directory)))

;;; Structure: chapters kept, acts and scenes silent

(ert-deftest test-compile-keeps-chapter-headings ()
  "Chapter headings survive compilation as real headings.
The regression this pins is the one that made export useless: because
:ignore: sits on chapter headings as well as scene headings, a direct
export of the working file produces a wall of prose with no chapter
division anywhere."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (org-scribe-compile 'clean 'org)
    (let ((out (org-scribe-compile-test--intermediate root "novel")))
      (should (string-match-p "^\\* Chapter 1$" out))
      (should (string-match-p "^\\* Chapter 2$" out))
      (should (string-match-p "^\\* Chapter 3$" out)))))

(ert-deftest test-compile-omits-act-headings ()
  "Acts are dropped; they are a planning structure, not a reading one."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (org-scribe-compile 'clean 'org)
    (let ((out (org-scribe-compile-test--intermediate root "novel")))
      (should-not (string-match-p "Act I" out))
      (should-not (string-match-p "Act II" out)))))

(ert-deftest test-compile-drops-scene-headings-and-todo-keywords ()
  "Scene titles and TODO keywords never reach the output."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (org-scribe-compile 'clean 'org)
    (let ((out (org-scribe-compile-test--intermediate root "novel"))
          ;; Case-sensitively: the intermediate's own "#+OPTIONS: ...
          ;; todo:nil ..." is not a leaked TODO keyword.
          (case-fold-search nil))
      (should-not (string-match-p "Scene" out))
      (should-not (string-match-p "\\bTODO\\b" out))
      (should-not (string-match-p "TOWRITE" out))
      ;; ...while the prose of those scenes is all present.
      (should (string-match-p "door creaked open" out))
      (should (string-match-p "last chapter began here" out)))))

;;; Apparatus removal

(ert-deftest test-compile-drops-noexport-apparatus ()
  "Planning sections tagged :noexport: are pruned wholesale."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (org-scribe-compile 'clean 'org)
    (let ((out (org-scribe-compile-test--intermediate root "novel")))
      (should-not (string-match-p "Project Goals" out))
      (should-not (string-match-p "Total word target" out))
      (should-not (string-match-p "Progress Tracking" out))
      (should-not (string-match-p "org-generate-wordcount-table" out)))))

(ert-deftest test-compile-drops-property-drawers ()
  "Scene metadata never reaches the manuscript."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (org-scribe-compile 'clean 'org)
    (let ((out (org-scribe-compile-test--intermediate root "novel")))
      (should-not (string-match-p ":PROPERTIES:" out))
      (should-not (string-match-p "WORDCOUNT" out))
      (should-not (string-match-p "PoV" out))
      (should-not (string-match-p "id:c1" out)))))

(ert-deftest test-compile-drops-edit-markers ()
  "Edit markers in comment blocks are removed, not merely unexported.
They are already excluded by every backend, but the intermediate is a
file the writer may hand to someone, so it must not carry them either."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (org-scribe-compile 'clean 'org)
    (let ((out (org-scribe-compile-test--intermediate root "novel")))
      (should-not (string-match-p "EDIT" out))
      (should-not (string-match-p "tide table" out))
      (should-not (string-match-p "begin_comment" out)))))

;;; Scene breaks derived from structure

(ert-deftest test-compile-inserts-break-between-sibling-scenes ()
  "Consecutive scenes of one chapter are separated by a break."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (org-scribe-compile 'clean 'org)
    (let ((out (org-scribe-compile-test--intermediate root "novel")))
      ;; Chapter 1: two written scenes, and scene 2 has an interior
      ;; macro break of its own -- two breaks in total, no more.
      (should (= 2 (org-scribe-compile-test--count
                    org-scribe-compile-scene-break out))))))

(ert-deftest test-compile-no-break-before-a-chapter ()
  "A chapter boundary is not also a scene break.
The break counter resets per chapter, so the first scene of a chapter
never opens with a separator under its own heading."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (org-scribe-compile 'clean 'org)
    (let ((out (org-scribe-compile-test--intermediate root "novel")))
      (should-not (string-match-p
                   (concat "\\* Chapter 2\n+#\\+begin_center")
                   out)))))

(ert-deftest test-compile-trailing-scene-break-macro-is-not-doubled ()
  "A {{{scene-break}}} at the end of a scene body is dropped.
The structural break already occupies that position; keeping the macro
too would render two separators where the writer meant one."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (org-scribe-compile 'clean 'org)
    (let ((out (org-scribe-compile-test--intermediate root "novel")))
      (should-not (string-match-p "scene-break" out))
      ;; No two breaks in a row anywhere.
      (should-not (string-match-p
                   (concat (regexp-quote org-scribe-compile-scene-break)
                           "\n#\\+end_center\n+#\\+begin_center")
                   out)))))

(ert-deftest test-compile-interior-scene-break-macro-becomes-a-break ()
  "A macro in the middle of a scene body is a real intra-scene break."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (org-scribe-compile 'clean 'org)
    (let ((out (org-scribe-compile-test--intermediate root "novel")))
      (should (string-match-p
               (concat "Rain arrived by evening\\.\n+#\\+begin_center\n"
                       (regexp-quote org-scribe-compile-scene-break)
                       "\n#\\+end_center\n+Later, the house had settled\\.")
               out)))))

(ert-deftest test-compile-unwritten-scene-emits-nothing ()
  "An empty scene contributes neither prose nor a break.
A fresh project is nothing but empty scenes; emitting a separator for
each would compile a manuscript made entirely of separators."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (org-scribe-compile 'clean 'org)
    (let ((out (org-scribe-compile-test--intermediate root "novel")))
      ;; Chapter 2 has one written scene and one empty one: its prose
      ;; must be followed by Chapter 3, with no break between.
      (should (string-match-p "Morning found nothing changed\\.\n+\\* Chapter 3"
                              out)))))

;;; Short fiction

(ert-deftest test-compile-short-story-has-no-chapter-headings ()
  "A short story compiles with no chapter level and no wrapper heading."
  (org-scribe-compile-test--with-project "short-story" "story.org"
      org-scribe-compile-test--short-story
    (org-scribe-compile 'clean 'org)
    (let ((out (org-scribe-compile-test--intermediate root "story")))
      (should-not (string-match-p "^\\*" out))
      (should-not (string-match-p "Story Content" out))
      (should-not (string-match-p "Story Info" out))
      (should-not (string-match-p "Opening" out))
      (should-not (string-match-p "GENRE" out)))))

(ert-deftest test-compile-short-story-separates-its-scenes ()
  "The short story's three scenes are separated by two breaks."
  (org-scribe-compile-test--with-project "short-story" "story.org"
      org-scribe-compile-test--short-story
    (org-scribe-compile 'clean 'org)
    (let ((out (org-scribe-compile-test--intermediate root "story")))
      (should (= 2 (org-scribe-compile-test--count
                    org-scribe-compile-scene-break out)))
      (should (string-match-p "waited by the window" out))
      (should (string-match-p "stopped waiting" out)))))

;;; The scene-break marker itself

(ert-deftest test-compile-rejects-unsafe-scene-break-markers ()
  "A marker Org would reparse as structure is refused, not emitted.
`* * *' inside a center block is parsed as a *headline*, splitting the
manuscript; a leading `#' is an Org comment and vanishes silently.
Both were confirmed against real exports, which is why this refuses
rather than warns."
  (dolist (bad '("* * *" "***" "# # #" "#" ""))
    (let ((org-scribe-compile-scene-break bad))
      (should-error (org-scribe--compile-scene-break) :type 'user-error)))
  ;; The shipped default is accepted.
  (should (stringp (org-scribe--compile-scene-break))))

(ert-deftest test-compile-scene-break-survives-plain-text-export ()
  "The break is visible in exported plain text."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (let ((output (org-scribe-compile 'clean 'txt)))
      (should (string-match-p (regexp-quote org-scribe-compile-scene-break)
                              (org-scribe-compile-test--read output))))))

(ert-deftest test-compile-odt-scene-break-is-a-sibling-paragraph ()
  "The ODT break is a sibling paragraph, never a nested `text:p'.
This is the regression that motivated the module.  The SCENE-BREAK macro
is expanded by a final-output *string* filter, which necessarily
substitutes inside the paragraph Org already emitted, producing
`<text:p ...><text:p>***</text:p></text:p>' -- invalid ODF that no
intermediate representation reveals.  Compiling emits the break as a
center block, so it becomes a real sibling element."
  (skip-unless org-scribe-compile-test--odt-available)
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (org-scribe-compile 'clean 'org)
    (let* ((intermediate (expand-file-name
                          "novel-clean.org"
                          (expand-file-name org-scribe-compile-output-directory root)))
           (buffer (find-file-noselect intermediate)))
      (unwind-protect
          (with-current-buffer buffer
            (let ((xml (org-export-as 'odt nil nil t)))
              (should (string-match-p "<text:p[^>]*OrgCenter[^>]*>⁂</text:p>" xml))
              (should-not (string-match-p "<text:p[^>]*><text:p" xml))
              ;; Chapters arrive as real ODT headings.
              (should (string-match-p "<text:h[^>]*>" xml))))
        (kill-buffer buffer)))))

(ert-deftest test-compile-odt-file-is-a-real-container ()
  "ODT compiles to a zip container, not a bare content.xml.

`org-export-to-file' writes the transcoded string straight to the
target, so using it for `odt' yields a file with the right extension
that is not an ODT at all -- unopenable, and with no error to say so.
Only `org-odt-export-to-odt' builds the container.  Asserting the file
exists would pass either way, so this checks the magic bytes and the
required container members, and that ox-odt left no scratch files
beside the output."
  (skip-unless org-scribe-compile-test--odt-available)
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (let* ((output (org-scribe-compile 'clean 'odt))
           (directory (file-name-directory output)))
      (should (string-suffix-p ".odt" output))
      (should (file-exists-p output))
      ;; A zip container, not XML.
      (should (string= "PK" (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally output nil 0 2)
                              (buffer-string))))
      ;; ox-odt unpacks its work into a temp directory; nothing of it
      ;; may be left in the writer's export directory.
      (should-not (file-exists-p (expand-file-name "content.xml" directory)))
      (should-not (file-exists-p (expand-file-name "styles.xml" directory)))
      (should-not (file-exists-p (expand-file-name "meta.xml" directory))))))

(ert-deftest test-compile-markdown-scene-break-has-no-html-wrapper ()
  "The Markdown break is a bare marker, not an org-center div.
`ox-md' has no centering and renders a center block by delegating to the
HTML backend, so without this the break arrives wrapped in
`<div class=\"org-center\"><p>...</p></div>' -- raw HTML in the one
format whose value is being lightweight."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (let ((out (org-scribe-compile-test--read (org-scribe-compile 'clean 'md))))
      (should-not (string-match-p "org-center" out))
      (should-not (string-match-p "<div" out))
      ;; The marker stands alone as its own Markdown block: a blank line
      ;; on each side.  Without the trailing one it would join the
      ;; paragraph below and render as a word in the next sentence.
      (should (string-match-p
               (concat "\n\n" (regexp-quote org-scribe-compile-scene-break) "\n\n")
               out)))))

(ert-deftest test-compile-markdown-filter-leaves-other-center-blocks-alone ()
  "Only the scene break is rewritten; a writer's own center block is not.
The filter matches on the block's content being exactly the marker, so
an epigraph or a few lines of verse the writer centred deliberately keep
whatever `ox-md' does with them."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      (concat "#+TITLE: T\n#+OPTIONS: todo:nil tags:nil\n\n"
              "* Act I :ignore:\n** Chapter 1 :ignore:\n*** Scene 1 :ignore:\n"
              "Prose.\n\n#+begin_center\nAn epigraph.\n#+end_center\n")
    (let ((out (org-scribe-compile-test--read (org-scribe-compile 'clean 'md))))
      (should (string-match-p "An epigraph" out))
      (should (string-match-p "org-center" out)))))

;;; Command behaviour

(ert-deftest test-compile-writes-the-intermediate-for-every-format ()
  "The inspectable intermediate is written whatever the output format."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (dolist (format '(org txt md))
      (org-scribe-compile 'clean format)
      (should (file-exists-p
               (expand-file-name "novel-clean.org"
                                 (expand-file-name
                                  org-scribe-compile-output-directory root)))))))

(ert-deftest test-compile-uses-the-configured-output-directory ()
  "Output lands in `org-scribe-compile-output-directory', not the root."
  (let ((org-scribe-compile-output-directory "built"))
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (org-scribe-compile 'clean 'txt)
      (should (file-directory-p (expand-file-name "built" root)))
      (should (file-exists-p (expand-file-name "built/novel-clean.txt" root)))
      (should-not (file-exists-p (expand-file-name "novel-clean.txt" root))))))

(ert-deftest test-compile-refuses-the-shunn-style ()
  "Shunn is refused with a message, not approximated.
It needs author and address data the project does not record; guessing
at a submission standard would be worse than declining."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (should-error (org-scribe-compile 'shunn 'txt) :type 'user-error)))

(ert-deftest test-compile-refuses-an-unknown-format ()
  "An unknown format is reported rather than silently defaulted."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (should-error (org-scribe-compile 'clean 'pdf) :type 'user-error)))

(ert-deftest test-compile-reports-a-project-with-no-manuscript ()
  "A project without a manuscript file is reported, not crashed on."
  (let* ((root (file-name-as-directory (make-temp-file "org-scribe-compile-" t)))
         (default-directory root))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".org-scribe-project" root)
            (insert "# Writing project: Test\n# Type: novel\n"))
          (org-scribe-project-type-cache-clear)
          (should-error (org-scribe-compile 'clean 'txt) :type 'user-error))
      (org-scribe-project-type-cache-clear)
      (delete-directory root t))))

;;; Messages

(ert-deftest test-compile-messages-exist-in-both-languages ()
  "Every compile message key is registered in both alists."
  (dolist (key '(compile-prompt-format compile-not-in-project
                 compile-no-manuscript compile-empty
                 compile-style-unsupported compile-format-unknown
                 compile-backend-missing compile-unsafe-scene-break
                 compile-done))
    (should (assq key org-scribe-messages-en))
    (should (assq key org-scribe-messages-es))))

;;; Run tests

(defun org-scribe-compile-run-tests ()
  "Run all manuscript compilation tests."
  (interactive)
  (ert "^test-compile-"))

(provide 'test-compile)

;;; test-compile.el ends here
