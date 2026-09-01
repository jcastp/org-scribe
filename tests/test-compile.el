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

(defun org-scribe-compile-test--intermediate (root name &optional style)
  "Return the text of the intermediate Org file for NAME under ROOT.
STYLE (default \"clean\") selects which style's intermediate to read --
`org-scribe-compile' names it after the style, so a `shunn' compile
writes a separate file rather than overwriting the `clean' one."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name (concat name "-" (or style "clean") ".org")
                       (expand-file-name org-scribe-compile-output-directory root)))
    (buffer-string)))

(defmacro org-scribe-compile-test--with-author (&rest body)
  "Run BODY with a complete, valid Shunn author identity configured."
  (declare (indent 0))
  `(let ((org-scribe-author-name "Jane Alcott Doe")
         (org-scribe-author-address '("123 Elm Street" "Springfield, IL 62704"))
         (org-scribe-author-email "jane@example.com")
         (org-scribe-author-phone nil)
         (org-scribe-author-agent nil))
     ,@body))

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

(defvar org-scribe-compile-test--pdflatex-available
  (and (require 'ox-latex nil t) (executable-find "pdflatex"))
  "Non-nil when this Emacs can compile LaTeX to PDF.")

(defvar org-scribe-compile-test--pandoc-available
  (executable-find "pandoc")
  "Non-nil when pandoc is on PATH.")

(defvar org-scribe-compile-test--epub-available
  (and (require 'ox-epub nil t) (fboundp 'org-epub-export-to-epub))
  "Non-nil when this Emacs can export EPUB (the optional `ox-epub' package).")

(defun org-scribe-compile-test--magic-bytes (file n)
  "Return the first N bytes of FILE as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 n)
    (buffer-string)))

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
              (should (string-match-p
                       (concat "<text:p[^>]*OrgCenter[^>]*>"
                               (regexp-quote org-scribe-compile-scene-break)
                               "</text:p>")
                       xml))
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

(ert-deftest test-compile-honours-a-customised-scene-break ()
  "A customised marker reaches every output format.
The marker is read at call time in all three places that touch it -- the
break builder, the validator and the Markdown filter -- so setting the
variable is enough; nothing caches the default."
  (let ((org-scribe-compile-scene-break "· · ·"))
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (dolist (format '(org txt md))
        (let ((out (org-scribe-compile-test--read (org-scribe-compile 'clean format))))
          (should (string-match-p (regexp-quote "· · ·") out))
          (should-not (string-match-p "⁂" out)))))))

(ert-deftest test-compile-pdf-file-is-a-real-pdf ()
  "PDF compiles to an actual PDF, via `org-latex-export-to-pdf'.
The exporter/backend split matters here the same way it does for ODT:
`org-export-to-file' on the `latex' backend would write a .tex file
under a .pdf name, which is not a PDF and would pass any
`file-exists-p' check. Checking the `%PDF' magic bytes is what would
have caught that."
  (skip-unless org-scribe-compile-test--pdflatex-available)
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (let ((output (org-scribe-compile 'clean 'pdf)))
      (should (string-suffix-p ".pdf" output))
      (should (file-exists-p output))
      (should (string= "%PDF"
                       (org-scribe-compile-test--magic-bytes output 4))))))

(ert-deftest test-compile-docx-file-is-a-real-docx ()
  "DOCX compiles to an actual zip container via pandoc, not the intermediate.
This pins the regression found while wiring pandoc in: `docx' has
:backend nil (there is no Org export backend for it, pandoc is
external) and an :exporter, and the dispatch in `org-scribe-compile'
originally branched on :backend alone -- so a DOCX request silently
returned the *intermediate .org file*, unnoticed because it also
satisfies `file-exists-p'."
  (skip-unless org-scribe-compile-test--pandoc-available)
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (let ((output (org-scribe-compile 'clean 'docx)))
      (should (string-suffix-p ".docx" output))
      (should (file-exists-p output))
      (should (string= "PK" (org-scribe-compile-test--magic-bytes output 2)))
      ;; Not the intermediate: a real docx round-trips as text via pandoc.
      (should (executable-find "pandoc"))
      (with-temp-buffer
        (call-process "pandoc" nil t nil "-f" "docx" "-t" "plain" output)
        (should (string-match-p "door creaked open" (buffer-string)))
        (should (string-match-p "Chapter 1" (buffer-string)))))))

;;; EPUB

(ert-deftest test-compile-epub-file-is-a-real-container ()
  "EPUB compiles to an actual EPUB container, via `org-epub-export-to-epub'.
Checks the zip magic bytes and the EPUB-specific `mimetype' member (the
first entry in every valid EPUB, required to be stored uncompressed and
to read exactly \"application/epub+zip\") rather than just
`file-exists-p', matching every other packaged-format test in this
file: existence alone would also be true of a broken or empty file."
  (skip-unless org-scribe-compile-test--epub-available)
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (let ((output (org-scribe-compile 'clean 'epub)))
      (should (string-suffix-p ".epub" output))
      (should (file-exists-p output))
      (should (string= "PK" (org-scribe-compile-test--magic-bytes output 2)))
      (with-temp-buffer
        (call-process "unzip" nil t nil "-p" output "mimetype")
        (should (string= "application/epub+zip" (string-trim (buffer-string))))))))

(ert-deftest test-compile-epub-scene-break-has-distinct-class ()
  "The compiled scene break gets its own CSS class in the EPUB body,
distinguishing it from an ordinary `.org-center' block -- the actual
point of `org-scribe--compile-filter-epub-scene-break': without it, a
scene break looks identical to any other centered paragraph, with no
visual signal that a break happened."
  (skip-unless org-scribe-compile-test--epub-available)
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (let ((output (org-scribe-compile 'clean 'epub)))
      (with-temp-buffer
        (call-process "unzip" nil t nil "-p" output "body.html")
        (let ((body (buffer-string)))
          (should (string-match-p "class=\"org-scribe-scene-break\"" body))
          (should (string-match-p "⁂" body))
          (should (string-match-p "Chapter 1" body))
          (should (string-match-p "door creaked open" body)))))))

(ert-deftest test-compile-epub-scene-break-class-is-not-corrupted ()
  "Regression test for a real bug found while building this: the CSS
class `org-scribe-scene-break' contains, case-insensitively, the exact
text the *pre-existing* {{{scene-break}}} macro's final-output filter
(`org-scribe--export-replace-scene-breaks', export/org-scribe-export.el)
searches for and replaces -- and that filter runs on the entire
rendered document for any org-scribe-context export, EPUB included, not
only where the old macro was actually used.  Before that filter was
fixed to match case-sensitively, the class attribute came out as
`org-scribe-<br><br><br>\\n\"', not `org-scribe-scene-break', silently
breaking the CSS selector.  This asserts the class name survives intact
through the *real* compile pipeline, not just the filter in isolation
(which `test-scene-break-replacement-is-case-sensitive' in
tests/test-export.el already covers)."
  (skip-unless org-scribe-compile-test--epub-available)
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (let ((output (org-scribe-compile 'clean 'epub)))
      (with-temp-buffer
        (call-process "unzip" nil t nil "-p" output "body.html")
        (should-not (string-match-p "<br>" (buffer-string)))
        (should (string-match-p "\"org-scribe-scene-break\"" (buffer-string)))))))

(ert-deftest test-compile-epub-ships-and-embeds-its-stylesheet ()
  "The scene-break stylesheet exists on disk and is embedded in the EPUB,
with its actual rule intact -- not just referenced by a path that might
not resolve."
  (skip-unless org-scribe-compile-test--epub-available)
  (should (file-exists-p org-scribe--compile-epub-css))
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (let ((output (org-scribe-compile 'clean 'epub)))
      (with-temp-buffer
        (call-process "unzip" nil t nil "-p" output "style-1.css")
        (should (string-match-p "\\.org-scribe-scene-break" (buffer-string)))))))

(ert-deftest test-compile-epub-writer-own-center-block-keeps-plain-class ()
  "A center block the writer wrote for their own reasons -- an epigraph,
a few lines of verse -- is not mistaken for the scene break and keeps
ox-epub's ordinary `.org-center' class."
  (skip-unless org-scribe-compile-test--epub-available)
  (org-scribe-compile-test--with-project "novel" "novel.org"
      (concat "#+TITLE: T\n#+OPTIONS: todo:nil tags:nil\n\n"
              "* Act I :ignore:\n** Chapter 1 :ignore:\n*** Scene 1 :ignore:\n"
              "Prose.\n\n#+begin_center\nAn epigraph.\n#+end_center\n")
    (let ((output (org-scribe-compile 'clean 'epub)))
      (with-temp-buffer
        (call-process "unzip" nil t nil "-p" output "body.html")
        (should (string-match-p "An epigraph" (buffer-string)))
        (should (string-match-p "class=\"org-center\"" (buffer-string)))
        (should-not (string-match-p "org-scribe-scene-break" (buffer-string)))))))

(ert-deftest test-compile-shunn-refuses-epub ()
  "EPUB is reflowable; Shunn is a fixed-page standard, so it is not
offered for EPUB, the same way `md' and `odt' are not."
  (org-scribe-compile-test--with-author
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (should-error (org-scribe-compile 'shunn 'epub) :type 'user-error))))

(ert-deftest test-compile-refuses-when-ox-epub-is-missing ()
  "A missing `ox-epub' is reported by name, not signalled as a crash."
  (let ((real-require (symbol-function 'require)))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (if (eq feature 'ox-epub)
                     (if noerror nil (signal 'file-missing (list "" feature)))
                   (funcall real-require feature filename noerror)))))
      (org-scribe-compile-test--with-project "novel" "novel.org"
          org-scribe-compile-test--novel
        (should-error (org-scribe-compile 'clean 'epub) :type 'user-error)))))

(ert-deftest test-compile-epub-dependency-is-registered ()
  "`ox-epub' appears in `org-scribe--dependencies' as :optional, so
`org-scribe-setup-check' reports it."
  (skip-unless (boundp 'org-scribe--dependencies))
  (should (assq 'ox-epub (alist-get :optional org-scribe--dependencies))))

(ert-deftest test-compile-refuses-when-pdflatex-is-missing ()
  "A missing pdflatex is reported by name, not signalled as a crash."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (should-error (org-scribe-compile 'clean 'pdf) :type 'user-error))))

(ert-deftest test-compile-refuses-when-pandoc-is-missing ()
  "A missing pandoc is reported by name, not signalled as a crash.
No file is left behind for a request that failed before rendering."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (should-error (org-scribe-compile 'clean 'docx) :type 'user-error)
      (should-not (file-exists-p
                   (expand-file-name
                    "export/novel-clean.docx" root))))))

(ert-deftest test-compile-pandoc-dependencies-are-registered ()
  "pdflatex and pandoc appear in `org-scribe--dependencies' as :optional,
so `org-scribe-setup-check' reports them the way every other optional
toolchain is reported.  `org-scribe--dependencies' lives in the main
org-scribe.el entry point, which this test file's own requires do not
load (only core/ and export/ are on its load-path, matching every other
compile test) -- so this only runs when something else in the session
already loaded the full package, e.g. via test-load.el."
  (skip-unless (boundp 'org-scribe--dependencies))
  (should (assoc "pdflatex" (alist-get :optional org-scribe--dependencies)))
  (should (assoc "pandoc" (alist-get :optional org-scribe--dependencies))))

;;; Shunn style -- word count and rounding

(ert-deftest test-compile-round-wordcount-nearest-hundred ()
  "Word counts round to the nearest hundred, per Shunn convention."
  (should (= 0 (org-scribe--compile-round-wordcount 37)))
  (should (= 300 (org-scribe--compile-round-wordcount 253)))
  (should (= 200 (org-scribe--compile-round-wordcount 249)))
  (should (= 400 (org-scribe--compile-round-wordcount 350))))

(ert-deftest test-compile-word-count-counts-scene-and-prose-only ()
  "The word count is computed from what actually compiles, not raw metadata."
  (should (= 5 (org-scribe--compile-word-count
               '((chapter . "Chapter One, Ignored")
                 (scene . "one two three")
                 (break)
                 (prose . "four five")))))
  (should (= 0 (org-scribe--compile-word-count nil))))

;;; Shunn style -- validation

(ert-deftest test-compile-shunn-refuses-without-author-name ()
  "Shunn is refused, by name, when no author identity is configured."
  (let ((org-scribe-author-name nil))
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (should-error (org-scribe-compile 'shunn 'txt) :type 'user-error))))

(ert-deftest test-compile-shunn-refuses-md-and-odt ()
  "Shunn has no layout for Markdown and no styles file for ODT (yet)."
  (org-scribe-compile-test--with-author
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (should-error (org-scribe-compile 'shunn 'md) :type 'user-error)
      (should-error (org-scribe-compile 'shunn 'odt) :type 'user-error))))

(ert-deftest test-compile-shunn-accepts-org-and-txt ()
  "Shunn compiles for the two formats that need no external toolchain."
  (org-scribe-compile-test--with-author
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (should (file-exists-p (org-scribe-compile 'shunn 'org)))
      (should (file-exists-p (org-scribe-compile 'shunn 'txt))))))

;;; Shunn style -- front matter content

(ert-deftest test-compile-shunn-includes-contact-block ()
  "The author's name, address and email appear in a verse block."
  (org-scribe-compile-test--with-author
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (org-scribe-compile 'clean 'org)  ; unrelated clean compile: proves no interference
      (org-scribe-compile 'shunn 'org)
      (let ((out (org-scribe-compile-test--intermediate root "novel" "shunn")))
        (should (string-match-p "#\\+begin_verse" out))
        (should (string-match-p "Jane Alcott Doe" out))
        (should (string-match-p "123 Elm Street" out))
        (should (string-match-p "jane@example.com" out))))))

(ert-deftest test-compile-shunn-agent-replaces-own-contact ()
  "An agent's contact lines replace the author's own, not append to them."
  (org-scribe-compile-test--with-author
    (let ((org-scribe-author-agent '("Pat Agent" "Agent House Literary")))
      (org-scribe-compile-test--with-project "novel" "novel.org"
          org-scribe-compile-test--novel
        (org-scribe-compile 'shunn 'org)
        (let ((out (org-scribe-compile-test--intermediate root "novel" "shunn")))
          (should (string-match-p "Pat Agent" out))
          (should (string-match-p "Agent House Literary" out))
          (should-not (string-match-p "123 Elm Street" out))
          (should-not (string-match-p "jane@example.com" out)))))))

(ert-deftest test-compile-shunn-byline-falls-back-to-author-name ()
  "With no Pen-name marker, the byline is the author's legal name."
  (org-scribe-compile-test--with-author
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (org-scribe-compile 'shunn 'org)
      (should (string-match-p "by Jane Alcott Doe"
                              (org-scribe-compile-test--intermediate root "novel" "shunn"))))))

(ert-deftest test-compile-shunn-byline-uses-pen-name-marker ()
  "A `# Pen-name:' marker overrides the byline without touching the contact block."
  (org-scribe-compile-test--with-author
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (org-scribe--project-marker-set root "Pen-name" "J.A. Doe")
      (org-scribe-compile 'shunn 'org)
      (let ((out (org-scribe-compile-test--intermediate root "novel" "shunn")))
        (should (string-match-p "by J\\.A\\. Doe" out))
        ;; The contact block still carries the legal name.
        (should (string-match-p "Jane Alcott Doe" out))))))

(ert-deftest test-compile-shunn-running-header-keyword-defaults-from-title ()
  "With no Running-header marker, the keyword is the title's first word."
  (org-scribe-compile-test--with-author
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (org-scribe-compile 'shunn 'org)
      (should (string-match-p "#\\+LATEX_HEADER:.*Doe / THE /"
                              (org-scribe-compile-test--intermediate root "novel" "shunn"))))))

(ert-deftest test-compile-shunn-running-header-keyword-from-marker ()
  "A `# Running-header:' marker overrides the derived keyword."
  (org-scribe-compile-test--with-author
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (org-scribe--project-marker-set root "Running-header" "TIDE")
      (org-scribe-compile 'shunn 'org)
      (should (string-match-p "#\\+LATEX_HEADER:.*Doe / TIDE /"
                              (org-scribe-compile-test--intermediate root "novel" "shunn"))))))

(ert-deftest test-compile-shunn-word-count-matches-computed-rounding ()
  "The displayed word count is the actual rounding of the compiled prose,
not a hardcoded or unrelated figure."
  (org-scribe-compile-test--with-author
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (let* ((blocks (car (org-scribe-compile-normalize manuscript)))
             (expected (org-scribe--compile-round-wordcount
                        (org-scribe--compile-word-count blocks))))
        (org-scribe-compile 'shunn 'org)
        (should (string-match-p
                 (format "about %d words" expected)
                 (org-scribe-compile-test--intermediate root "novel" "shunn")))))))

(ert-deftest test-compile-shunn-suppresses-org-title-block ()
  "Org's own title/author/date rendering is suppressed for Shunn.
Without title:nil/author:nil/date:nil, every backend prints its own
title block (a `\\maketitle' in LaTeX, a boilerplate author/date line in
ASCII) above the front matter this module builds -- confirmed by
exporting a minimal fixture before this suppression was added."
  (org-scribe-compile-test--with-author
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (org-scribe-compile 'shunn 'org)
      (should (string-match-p "title:nil author:nil date:nil"
                              (org-scribe-compile-test--intermediate root "novel" "shunn"))))))

(ert-deftest test-compile-shunn-clean-title-block-unaffected ()
  "The `clean' style does not suppress the title -- only `shunn' needs to."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (org-scribe-compile 'clean 'org)
    (should-not (string-match-p "title:nil"
                                (org-scribe-compile-test--intermediate root "novel" "clean")))))

(ert-deftest test-compile-shunn-ends-with-end-mark ()
  "A Shunn manuscript closes with a centered END mark; clean does not."
  (org-scribe-compile-test--with-author
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (org-scribe-compile 'clean 'org)
      (org-scribe-compile 'shunn 'org)
      ;; The exact marker block, not a bare "END" substring search: every
      ;; clean compile already contains "#+end_center" from scene
      ;; breaks, which a loose case-insensitive \\bEND\\b would match
      ;; ("+" and "_" both count as non-word characters in Emacs's
      ;; default syntax table, so "end" inside "#+end_center" sits at a
      ;; word boundary on both sides) -- confirmed by this exact
      ;; false positive during development.
      (should (string-match-p "#\\+begin_center\nEND\n#\\+end_center"
                              (org-scribe-compile-test--intermediate root "novel" "shunn")))
      (should-not (string-match-p "#\\+begin_center\nEND\n#\\+end_center"
                                  (org-scribe-compile-test--intermediate root "novel" "clean"))))))

(ert-deftest test-compile-shunn-preamble-rejects-malformed-template ()
  "A customized preamble with the wrong placeholder count is refused,
not silently rendered with SURNAME/KEYWORD missing or an opaque Elisp
`format' error."
  (org-scribe-compile-test--with-author
    (dolist (bad '("no placeholders" "only %s one" "%s %s %s three"))
      (let ((org-scribe-compile-shunn-latex-preamble bad))
        (org-scribe-compile-test--with-project "novel" "novel.org"
            org-scribe-compile-test--novel
          (should-error (org-scribe-compile 'shunn 'org) :type 'user-error))))))

;;; Shunn style -- real PDF and DOCX output

(ert-deftest test-compile-shunn-pdf-has-real-running-header ()
  "The compiled PDF's LaTeX source carries the actual Shunn running
header, and pdflatex accepts it -- a real end-to-end check, not just
that the preamble text was assembled correctly."
  (skip-unless org-scribe-compile-test--pdflatex-available)
  (org-scribe-compile-test--with-author
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (let* ((output (org-scribe-compile 'shunn 'pdf))
             (tex (expand-file-name "novel-shunn.tex" (file-name-directory output))))
        (should (string= "%PDF" (org-scribe-compile-test--magic-bytes output 4)))
        (should (file-exists-p tex))
        (with-temp-buffer
          (insert-file-contents tex)
          (should (string-match-p "\\\\fancyhead\\[R\\]{Doe / THE / \\\\thepage}"
                                  (buffer-string)))
          (should (string-match-p "\\\\thispagestyle{empty}" (buffer-string)))
          (should (string-match-p "\\\\doublespacing" (buffer-string))))))))

(ert-deftest test-compile-pdf-scene-break-is-visible-not-silently-dropped ()
  "The scene break actually appears in the rendered PDF text.
The default marker (the asterism, U+2042) is outside pdflatex's default
font encoding: it compiles with no error -- only ox-latex's own
\"unicode character(s) not supported\" warning, easy to miss in a batch
build -- and the glyph is silently absent from the PDF, with no visible
trace at all where the break should be.  A test that only checked the
build succeeded, or that some replacement string is *present* in the
.tex source, would have missed this: the bug is specifically that a
clean compile produces a PDF with content missing.  This reads the
rendered PDF back with pdftotext and checks the break is actually there,
in a `clean' compile -- the bug affects every PDF, not just Shunn's."
  (skip-unless org-scribe-compile-test--pdflatex-available)
  (skip-unless (executable-find "pdftotext"))
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (let ((output (org-scribe-compile 'clean 'pdf)))
      (with-temp-buffer
        (call-process "pdftotext" nil t nil output "-")
        ;; `org-scribe--compile-latex-scene-break' is *LaTeX source*
        ;; ("*~~*~~*", `~' being a tie); what a reader sees, and what
        ;; pdftotext extracts, is three asterisks with the ties
        ;; rendered as ordinary spaces -- so check for the rendered
        ;; result, not the source string.
        (should (string-match-p "\\*\\*\\*"
                                (replace-regexp-in-string "[ \t]+" "" (buffer-string))))))))

(ert-deftest test-compile-shunn-docx-round-trips-frontmatter ()
  "The Shunn DOCX carries the front matter content through pandoc,
verified by converting it back to plain text rather than assuming the
input text made it through unmangled."
  (skip-unless org-scribe-compile-test--pandoc-available)
  (org-scribe-compile-test--with-author
    (org-scribe-compile-test--with-project "novel" "novel.org"
        org-scribe-compile-test--novel
      (let ((output (org-scribe-compile 'shunn 'docx)))
        (should (string= "PK" (org-scribe-compile-test--magic-bytes output 2)))
        (with-temp-buffer
          (call-process "pandoc" nil t nil "-f" "docx" "-t" "plain" output)
          (should (string-match-p "Jane Alcott Doe" (buffer-string)))
          (should (string-match-p "by Jane Alcott Doe" (buffer-string)))
          (should (string-match-p "END" (buffer-string))))))))

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

;; Shunn's own validation (author identity, supported formats, a
;; malformed preamble) is covered in the "Shunn style" section above --
;; `test-compile-shunn-refuses-without-author-name' is what this test
;; used to be, back when `shunn' was refused unconditionally.

(ert-deftest test-compile-refuses-an-unknown-format ()
  "An unknown format is reported rather than silently defaulted."
  (org-scribe-compile-test--with-project "novel" "novel.org"
      org-scribe-compile-test--novel
    (should-error (org-scribe-compile 'clean 'rtf) :type 'user-error)))

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
  (dolist (key '(compile-prompt-style compile-prompt-format compile-not-in-project
                 compile-no-manuscript compile-empty
                 compile-style-unsupported compile-format-unknown
                 compile-backend-missing compile-unsafe-scene-break
                 compile-done compile-shunn-format-unsupported
                 compile-shunn-author-missing compile-shunn-preamble-malformed))
    (should (assq key org-scribe-messages-en))
    (should (assq key org-scribe-messages-es))))

;;; Run tests

(defun org-scribe-compile-run-tests ()
  "Run all manuscript compilation tests."
  (interactive)
  (ert "^test-compile-"))

(provide 'test-compile)

;;; test-compile.el ends here
