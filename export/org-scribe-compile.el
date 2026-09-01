;;; org-scribe-compile.el --- Manuscript compilation for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `org-scribe-compile' turns the working manuscript into a clean,
;; readable document.
;;
;; The working file is not a manuscript.  It is a workspace with a
;; document inside it: apparatus sections tagged :noexport:, acts and
;; chapters and scenes all tagged :ignore:, an eighteen-field property
;; drawer on every scene, dynamic blocks, and edit markers in comment
;; blocks.  Exporting it directly -- the only thing org-scribe could do
;; before this module -- produces an undifferentiated wall of prose with
;; no chapter divisions at all, because :ignore: is on the chapter and
;; act headings just as it is on the scenes.
;;
;; So compilation is a *transformation*, not a set of export filters.
;; The pipeline is:
;;
;;   manuscript.org --normalize--> <clean .org> --stock ox-*/pandoc--> txt/md/odt/pdf/docx
;;
;; Two properties of that shape are the reason for it:
;;
;; - The structural knowledge is written *once*.  Which headings are
;;   apparatus, which are containers, which are chapters and which are
;;   scenes is answered in one place (`org-scribe--compile-blocks'), and
;;   every output format inherits the answer.  Defining a derived backend
;;   per format would restate it once per backend per style instead.
;;
;; - The intermediate is a real file, left on disk beside the rendered
;;   output.  When a compile looks wrong the writer opens it and can see
;;   whether the transform or the renderer is at fault.  A silent
;;   transformation would be the worst of the options here, for the same
;;   reason a hidden timeline entity leaves a trace and a substituted
;;   lemma is disclosed.
;;
;; Scene breaks are *derived from structure*, not typed.  Scenes are
;; already first-class headings the package understands, so a break goes
;; between consecutive sibling scenes automatically and a forgotten
;; {{{scene-break}}} macro can no longer produce a missing break.  The
;; break is emitted as an Org =center= block, which is a genuine
;; block-level element: this is what makes the ODT output a sibling
;; paragraph rather than the nested (invalid) `text:p' that the
;; final-output string filter in org-scribe-export.el cannot avoid
;; producing.  A string filter runs too late to insert structure; that
;; limitation is what this module exists to get past.
;;
;; Existing {{{scene-break}}} macros are honored rather than ignored: one
;; in the *middle* of a scene body is a deliberate intra-scene break and
;; becomes a real break element, while one at the start or end of a body
;; is dropped, because the structural break already covers that position
;; and emitting both would double it.
;;
;; Styles: only `clean' (a readable draft) exists today.  `shunn'
;; (submission format) needs front-matter data the project does not yet
;; record and is refused with a clear message rather than approximated.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-element)
(require 'ox)
(require 'org-scribe-config)
(require 'org-scribe-messages)
(require 'org-scribe-export)

(declare-function org-scribe-project-root "org-scribe-core")
(declare-function org-scribe-project-type "org-scribe-core")
(declare-function org-scribe--find-existing-file "org-scribe-core" (root &rest relative-paths))
(declare-function org-scribe--project-marker-get "org-scribe-core" (root key))

;;; Manuscript Resolution

(defconst org-scribe--compile-manuscript-names
  '("novel.org" "novela.org" "story.org" "cuento.org")
  "Manuscript file names, in resolution order, for both languages.
A fixed bilingual list rather than a glob, matching how every other
file resolver in the package works: a project contains exactly one of
these, and globbing would pick up a writer's own stray .org file.")

(defun org-scribe--compile-manuscript-file (root)
  "Return ROOT's manuscript file, or nil when there is none."
  (apply #'org-scribe--find-existing-file root
         org-scribe--compile-manuscript-names))

;;; Document Structure
;;
;; Which level means what differs by project type, so the walk is
;; parameterized rather than hardcoded -- the same branch on
;; `org-scribe-project-type' every other resolver makes.
;;
;;   novel:        * Act   ** Chapter   *** Scene
;;   short story:  * Story Content   ** Scene
;;
;; `:chapter' is the level whose headings survive into the output as
;; visible headings; `:scene' is the level at and below which headings
;; go silent and become breaks.  Anything *above* :chapter (a novel's
;; acts) or above :scene when there are no chapters (a short story's
;; content wrapper) is a container: its heading is dropped and its
;; children are processed in order.  Dropping acts is deliberate --
;; acts in this method are a planning structure, not a reading one.

(defconst org-scribe--compile-levels
  '((novel       . (:chapter 2 :scene 3))
    (short-story . (:chapter nil :scene 2)))
  "Per project type, the outline levels that carry chapters and scenes.")

(defun org-scribe--compile-levels-for (type)
  "Return the chapter/scene level plist for project TYPE."
  (or (alist-get type org-scribe--compile-levels)
      (alist-get 'novel org-scribe--compile-levels)))

(defconst org-scribe--compile-skip-elements
  '(property-drawer planning drawer comment comment-block dynamic-block)
  "Element types dropped from a heading's body during normalization.
Property drawers and planning lines are scene metadata; `drawer' covers
LOGBOOK; comments and comment blocks are the writer's edit markers,
which must never reach a reader; dynamic blocks are the generated
progress tables.  None of it is prose.")

;;; Normalization

(defun org-scribe--compile-excluded-p (headline)
  "Return non-nil when HEADLINE carries an export-excluding tag."
  (seq-intersection (org-element-property :tags headline)
                    org-export-exclude-tags
                    #'string=))

(defun org-scribe--compile-child-headlines (element)
  "Return the direct child headlines of ELEMENT, in document order.
Works for both an `org-data' tree and a `headline', whose contents are
alike: an optional leading section, then any child headlines."
  (seq-filter (lambda (child) (eq (org-element-type child) 'headline))
              (org-element-contents element)))

(defun org-scribe--compile-scene-break ()
  "Return the scene break as Org markup, validating the configured marker.
Signals a `user-error' for a marker Org would reparse as something other
than text -- see `org-scribe-compile-scene-break' for why the two
rejected shapes are not hypothetical."
  (let ((marker (string-trim (or org-scribe-compile-scene-break ""))))
    (when (or (string-empty-p marker)
              (memq (aref marker 0) '(?* ?#)))
      (user-error "%s" (org-scribe-msg 'compile-unsafe-scene-break
                                       (format "%S" org-scribe-compile-scene-break))))
    (format "#+begin_center\n%s\n#+end_center" marker)))

(defun org-scribe--compile-strip-macros (body)
  "Return BODY with {{{scene-break}}} macros resolved.
A macro at the start or end of a scene body is dropped: the structural
break between sibling scenes already occupies that position, and keeping
both would render two breaks where the writer meant one.  A macro in the
middle of a body is a deliberate break *within* one scene and becomes a
real break element."
  (let ((edge "\\`\\(?:[ \t\n]*{{{scene-break}}}[ \t]*\n?\\)+\\|\\(?:\n?[ \t]*{{{scene-break}}}[ \t\n]*\\)+\\'"))
    (string-trim
     (replace-regexp-in-string
      "{{{scene-break}}}"
      (lambda (_) (org-scribe--compile-scene-break))
      (replace-regexp-in-string edge "" body)
      t t))))

(defun org-scribe--compile-heading-body (headline)
  "Return HEADLINE's own prose, excluding subheadings and apparatus.
Must be called in the buffer HEADLINE was parsed from: the text is taken
from buffer positions so the writer's prose survives verbatim, rather
than being regenerated from the parse tree."
  (let ((section (car (org-element-contents headline)))
        (parts nil))
    (when (eq (org-element-type section) 'section)
      (dolist (element (org-element-contents section))
        (unless (memq (org-element-type element) org-scribe--compile-skip-elements)
          (push (buffer-substring-no-properties
                 (org-element-property :begin element)
                 (org-element-property :end element))
                parts))))
    (org-scribe--compile-strip-macros (string-trim (apply #'concat (nreverse parts))))))

(defun org-scribe--compile-blocks (tree levels)
  "Return the ordered output blocks for parse TREE, given LEVELS.
Each block is (chapter . TITLE), (scene . TEXT), (prose . TEXT) or
\(break).  `scene' and `prose' render identically; they are kept apart so
scenes can be counted exactly, without re-deriving them from the breaks
between them (which under-counts by one per chapter, and counts an
unwritten scene that emitted nothing).  Must be called in the buffer
TREE was parsed from."
  (let ((chapter-level (plist-get levels :chapter))
        (scene-level (plist-get levels :scene))
        (blocks nil)
        (scenes-this-chapter 0))
    (cl-labels
        ((emit-prose (body)
           (when (org-string-nw-p body) (push (cons 'prose body) blocks)))
         (walk (headline)
           (unless (org-scribe--compile-excluded-p headline)
             (let ((level (org-element-property :level headline))
                   (body (org-scribe--compile-heading-body headline)))
               (cond
                ;; Scene: the heading is a working label, never shown.  A
                ;; break separates it from the previous scene of the same
                ;; chapter -- not from a chapter heading, and not across
                ;; a chapter boundary.  An *unwritten* scene emits
                ;; nothing at all, break included: a fresh project is all
                ;; empty scenes, and breaks around them would be a
                ;; manuscript made entirely of separators.
                ((and scene-level (>= level scene-level))
                 (when (org-string-nw-p body)
                   (when (> scenes-this-chapter 0)
                     (push '(break) blocks))
                   (cl-incf scenes-this-chapter)
                   (push (cons 'scene body) blocks)))
                ;; Chapter: kept, and resets the run of scenes.
                ((and chapter-level (= level chapter-level))
                 (push (cons 'chapter (org-element-property :raw-value headline)) blocks)
                 (setq scenes-this-chapter 0)
                 (emit-prose body))
                ;; Container (act, story wrapper): heading dropped, any
                ;; prose of its own kept, children processed in order.
                (t (emit-prose body)))
               (mapc #'walk (org-scribe--compile-child-headlines headline))))))
      (mapc #'walk (org-scribe--compile-child-headlines tree)))
    (nreverse blocks)))

(defun org-scribe--compile-keywords (&optional buffer)
  "Return the document keywords carried over from BUFFER into the output."
  (with-current-buffer (or buffer (current-buffer))
    (org-collect-keywords '("TITLE" "AUTHOR" "DATE" "LANGUAGE"))))

;;; Shunn Front Matter
;;
;; Shunn ("standard manuscript format") needs data the manuscript itself
;; does not carry: the writer's real name and contact information, a
;; running-header keyword, and the manuscript's own word count.  The
;; split follows the project's existing marker mechanisms rather than
;; inventing a new one:
;;
;; - Per author, stable across projects: `org-scribe-author-*'
;;   defcustoms (core/org-scribe-config.el).
;; - Per project: `.org-scribe-project' marker lines, read with the same
;;   `org-scribe--project-marker-get' that `Plan'/`Planner' already use --
;;   `Pen-name' (when the byline differs from the legal name) and
;;   `Running-header' (the keyword half of "Surname / KEYWORD / page").
;; - Computed: the word count, from the blocks that are actually going
;;   to be in the compiled document -- not the manuscript's own
;;   `WORDCOUNT' properties, which include apparatus this pass drops.
;;
;; The content itself (contact block, word count, title/byline, and the
;; closing "END" mark) is one shared block of Org markup, not one per
;; format: a `verse' block keeps its line breaks in every backend
;; (confirmed against ascii, pandoc's docx, and LaTeX/PDF), and a
;; `#+begin_export latex' block is inert everywhere except LaTeX/PDF, so
;; the running header's `\thispagestyle{empty}' can live in the shared
;; text without a fork per format.  Real per-page layout (running header
;; from page 2 on, double spacing, 1-inch margins) is only possible for
;; PDF, via `#+LATEX_HEADER' lines built from
;; `org-scribe-compile-shunn-latex-preamble' -- txt and docx get the
;; same front-matter *content*, simplified, since neither format has a
;; page model to run a header or margins against.

(defun org-scribe--compile-word-count (blocks)
  "Return the number of words across the prose/scene text in BLOCKS.
Deliberately not the manuscript's own `WORDCOUNT' properties: those
total every heading, including apparatus this pass has already dropped,
so they would overcount what the compiled document actually contains."
  (let ((total 0))
    (dolist (block blocks)
      (when (memq (car-safe block) '(prose scene))
        (setq total (+ total (length (split-string (cdr block) "[ \t\n\r]+" t))))))
    total))

(defun org-scribe--compile-round-wordcount (n)
  "Round N to the nearest hundred, per Shunn format's word-count convention."
  (* 100 (round n 100)))

(defun org-scribe--compile-shunn-contact-lines ()
  "Return the Shunn contact block, as a list of lines.
`org-scribe-author-agent', when set, replaces the writer's own contact
information entirely -- the convention for a work submitted through an
agent, where the agent's address is what a publisher writes back to."
  (or org-scribe-author-agent
      (delq nil (append (list org-scribe-author-name)
                        org-scribe-author-address
                        (list org-scribe-author-phone
                              org-scribe-author-email)))))

(defun org-scribe--compile-shunn-surname ()
  "Return the last word of `org-scribe-author-name', for the running header."
  (car (last (split-string org-scribe-author-name))))

(defun org-scribe--compile-shunn-keyword (root title)
  "Return the running-header keyword.
The `# Running-header:' marker under ROOT if the writer set one,
otherwise the first word of TITLE, uppercased -- Shunn's own suggestion
is \"one keyword from the title\", so deriving a default costs the
writer nothing while `Running-header' still lets them override it."
  (or (org-scribe--project-marker-get root "Running-header")
      (upcase (or (car (split-string (or title "") "[^[:alnum:]]+" t))
                 "TITLE"))))

(defun org-scribe--compile-shunn-byline (root)
  "Return the byline name for ROOT's project.
The `# Pen-name:' marker if the writer set one, otherwise
`org-scribe-author-name' -- the contact block always carries the legal
name (that is who a check gets written to), but the byline on the page
is the writer's choice."
  (or (org-scribe--project-marker-get root "Pen-name")
      org-scribe-author-name))

(defun org-scribe--compile-shunn-preamble-lines (surname keyword)
  "Return `#+LATEX_HEADER:' lines built from the Shunn LaTeX preamble.
SURNAME and KEYWORD fill the two `%s' placeholders in
`org-scribe-compile-shunn-latex-preamble', in that order.  The template
is validated to contain exactly two before `format' ever runs: fewer
would silently produce a preamble with SURNAME/KEYWORD missing (`format'
ignores unused arguments rather than erroring), and more would raise
Elisp's own \"not enough arguments\" instead of naming the actual
customization that is wrong."
  (let ((template org-scribe-compile-shunn-latex-preamble)
        (count 0) (pos 0))
    (while (string-match "%s" template pos)
      (setq count (1+ count) pos (match-end 0)))
    (unless (= count 2)
      (user-error "%s" (org-scribe-msg 'compile-shunn-preamble-malformed template)))
    (mapconcat (lambda (line) (concat "#+LATEX_HEADER: " line "\n"))
              (split-string (format template surname keyword) "\n")
              "")))

(defun org-scribe--compile-shunn-frontmatter (root title wordcount)
  "Return the Shunn front matter as Org markup: contact block, rounded
WORDCOUNT, and centered TITLE/byline.  Prepended to the manuscript body
by `org-scribe--compile-to-org'; ROOT supplies the project's Pen-name
marker for the byline."
  (concat
   "#+begin_verse\n"
   (mapconcat #'identity (org-scribe--compile-shunn-contact-lines) "\n")
   "\n#+end_verse\n\n"
   "#+begin_center\n"
   (format "about %d words" (org-scribe--compile-round-wordcount wordcount))
   "\n#+end_center\n\n"
   "#+begin_export latex\n\\thispagestyle{empty}\n#+end_export\n\n"
   "#+begin_center\n"
   (or title "Untitled") "\n\n"
   "by " (org-scribe--compile-shunn-byline root)
   "\n#+end_center\n\n"))

(defun org-scribe--compile-keyword-value (name keywords)
  "Return the first value of keyword NAME in KEYWORDS, or nil.
KEYWORDS is the alist `org-collect-keywords' returns: NAME to a list of
every occurrence, in document order."
  (cadr (assoc name keywords)))

(defun org-scribe--compile-to-org (blocks keywords &optional style root)
  "Return the intermediate Org document for BLOCKS with KEYWORDS.
STYLE `shunn' additionally prepends the Shunn front matter, inserts the
LaTeX running-header preamble (inert outside PDF output), and appends a
centered \"END\" mark; ROOT supplies the project's Shunn markers.  Any
other STYLE (including nil) produces exactly the `clean' document."
  (with-temp-buffer
    (dolist (keyword '("TITLE" "AUTHOR" "DATE" "LANGUAGE"))
      (when-let* ((value (org-scribe--compile-keyword-value keyword keywords))
                  ((org-string-nw-p value)))
        (insert (format "#+%s: %s\n" keyword value))))
    ;; num:nil because the writer's own heading text already says
    ;; "Chapter 1"; auto-numbering would silently disagree with a
    ;; chapter the writer renamed.  Shunn additionally suppresses Org's
    ;; own title rendering (title:nil drops \maketitle in LaTeX and the
    ;; boilerplate title line in ASCII; author:nil/date:nil are still
    ;; needed on top of it -- title:nil alone leaves "A Writer  2026-09-01"
    ;; sitting above the shared front-matter block this function builds,
    ;; confirmed by exporting a minimal fixture before writing this).
    (insert (concat "#+OPTIONS: toc:nil num:nil todo:nil tags:nil"
                    (if (eq style 'shunn) " title:nil author:nil date:nil" "")
                    "\n"))
    (when (eq style 'shunn)
      (insert (org-scribe--compile-shunn-preamble-lines
              (org-scribe--compile-shunn-surname)
              (org-scribe--compile-shunn-keyword
               root (org-scribe--compile-keyword-value "TITLE" keywords)))))
    (insert "\n")
    (when (eq style 'shunn)
      (insert (org-scribe--compile-shunn-frontmatter
              root (org-scribe--compile-keyword-value "TITLE" keywords)
              (org-scribe--compile-word-count blocks))))
    (dolist (block blocks)
      (pcase block
        (`(chapter . ,title) (insert "* " title "\n\n"))
        (`(,(or 'prose 'scene) . ,text) (insert text "\n\n"))
        (`(break) (insert (org-scribe--compile-scene-break) "\n\n"))))
    (when (eq style 'shunn)
      (insert "#+begin_center\nEND\n#+end_center\n"))
    (buffer-string)))

(defun org-scribe-compile-normalize (file &optional style root)
  "Return (BLOCKS . ORG-TEXT) compiled from the manuscript FILE.
STYLE and ROOT are forwarded to `org-scribe--compile-to-org' -- see
there for what `shunn' adds.  A buffer opened here purely to read FILE
is closed again; one the writer already had open is left alone, and its
unsaved edits are what gets compiled -- which is the useful behavior
when checking a draft."
  (let* ((existing (get-file-buffer file))
         (buffer (or existing (find-file-noselect file))))
    (unwind-protect
        (with-current-buffer buffer
          (org-with-wide-buffer
           (let* ((levels (org-scribe--compile-levels-for (org-scribe-project-type)))
                  (blocks (org-scribe--compile-blocks (org-element-parse-buffer) levels)))
             (cons blocks (org-scribe--compile-to-org
                           blocks (org-scribe--compile-keywords) style root)))))
      (unless existing (kill-buffer buffer)))))


;;; Markdown Rendering
;;
;; Markdown has no centering, so `ox-md' renders a center block by
;; delegating to the HTML backend and emitting a raw
;; `<div class="org-center">' wrapper.  In a format whose whole point is
;; being lightweight -- proofing, diffing, pasting elsewhere -- that
;; wrapper is noise around a single glyph.
;;
;; This is a *rendering* concern, not a structural one, so it is fixed
;; here rather than by varying the intermediate per format: there stays
;; exactly one intermediate document, and each backend decides how to
;; draw the break it contains.
;;
;; The rewrite is deliberately narrow.  It fires only on a center block
;; whose entire content is the scene-break marker, so a center block the
;; writer put in the manuscript for their own reasons -- an epigraph, a
;; few lines of verse -- keeps whatever `ox-md' does with it.  Widening
;; this to all center blocks would silently flatten those.

(defun org-scribe--compile-filter-md-scene-break (data backend info)
  "Render a compiled scene break as a bare line in Markdown.
DATA is the transcoded center block, BACKEND the backend name and INFO
the export communication channel.  Applies only to Markdown output for
org-scribe documents, and only when DATA is the scene break itself."
  (if (and (org-export-derived-backend-p backend 'md)
           (org-scribe--export-in-scribe-context-p info)
           (string= (string-trim (replace-regexp-in-string "<[^>]*>" "" data))
                    (string-trim (or org-scribe-compile-scene-break ""))))
      ;; A trailing blank line is required, not cosmetic: without it the
      ;; marker and the paragraph beneath it are one Markdown block, and
      ;; the break renders as a word at the start of the next sentence.
      (concat (string-trim org-scribe-compile-scene-break) "\n\n")
    data))

(add-to-list 'org-export-filter-center-block-functions
             #'org-scribe--compile-filter-md-scene-break)

;;; LaTeX/PDF Rendering
;;
;; pdflatex's default font encoding does not cover every glyph the
;; shipped scene-break presets offer.  Confirmed against all five: the
;; default asterism (U+2042) and the fleuron (U+2767) both compile with
;; ox-latex's own "unicode character(s) not supported by pdflatex"
;; warning and *vanish from the PDF with no visible trace* -- not even a
;; missing-glyph box, just blank space where the break should be, found
;; by rendering real PDFs and reading them back with pdftotext rather
;; than trusting the absence of a build error.  Three dots, an em dash
;; and a section sign all render correctly.
;;
;; There is no reliable way from Elisp to test whether a given glyph is
;; in the current LaTeX font's encoding, and a hardcoded allowlist of
;; "known safe" presets would silently rot the moment a preset list
;; changes.  So PDF output does not attempt to render the writer's
;; configured marker at all: it always uses three centered asterisks --
;; itself the traditional print-manuscript scene-break convention
;; (Shunn format's own suggestion, alongside a centered "#"), so this is
;; not an arbitrary substitute for the medium it applies to.
;;
;; `*~~*~~*' rather than `*\\,*\\,*' or `*\\quad{}*\\quad{}*': both of
;; those were tried first and both compile without error, but leaked
;; literal backslash sequences into `pdftotext' output in testing here,
;; which is close enough to F2's nested-`text:p' lesson -- a build that
;; succeeds is not proof the output is right -- to check by reading the
;; rendered PDF rather than trusting a clean compile.  `~' is LaTeX's
;; own tie character, needs no package, and round-trips through
;; `pdftotext' as plain spaces.

(defconst org-scribe--compile-latex-scene-break "*~~*~~*"
  "The scene break as rendered in LaTeX/PDF output.
Deliberately independent of `org-scribe-compile-scene-break': see the
commentary above `org-scribe--compile-filter-latex-scene-break' for why
PDF cannot safely render the writer's configured marker glyph.")

(defun org-scribe--compile-filter-latex-scene-break (data backend info)
  "Render a compiled scene break as three safe asterisks in LaTeX/PDF.
DATA is the transcoded center block (`\\\\begin{center}...\\\\end{center}'),
BACKEND the backend name and INFO the export communication channel.
Applies only to LaTeX-derived output for org-scribe documents, and only
when the block's inner content -- on its own line, between the
begin/end -- is the scene break itself; a multi-line center block (an
epigraph, a few lines of verse the writer wrote for their own reasons)
does not match the single-line pattern and is left untouched."
  (if (and (org-export-derived-backend-p backend 'latex)
           (org-scribe--export-in-scribe-context-p info)
           (string-match "\\`\\\\begin{center}\n\\(.*\\)\n\\\\end{center}\n*\\'" data)
           (string= (string-trim (match-string 1 data))
                    (string-trim (or org-scribe-compile-scene-break ""))))
      (concat "\\begin{center}\n" org-scribe--compile-latex-scene-break
             "\n\\end{center}\n\n")
    data))

(add-to-list 'org-export-filter-center-block-functions
             #'org-scribe--compile-filter-latex-scene-break)

;;; Output Formats

(defconst org-scribe--compile-formats
  '((org  . (:extension "org"  :backend nil   :library nil))
    (txt  . (:extension "txt"  :backend ascii :library ox-ascii))
    (md   . (:extension "md"   :backend md    :library ox-md))
    (odt  . (:extension "odt"  :backend odt   :library ox-odt
             :exporter org-odt-export-to-odt))
    (pdf  . (:extension "pdf"  :backend latex :library ox-latex
             :exporter org-latex-export-to-pdf :executable "pdflatex"))
    (docx . (:extension "docx" :backend nil   :library nil
             :exporter org-scribe--compile-pandoc-export :executable "pandoc")))
  "Formats `org-scribe-compile' can produce, and what each needs.
Every format needs at most an Elisp library (:library, always bundled
with Org except EPUB's, which is not offered here) and at most one
external program (:executable, checked with `executable-find' rather
than required as a library, since neither pdflatex nor pandoc is
Elisp).  A missing executable is reported by name via
`compile-backend-missing' before anything is written, and both are
listed in `org-scribe--dependencies' as :optional so
`org-scribe-setup-check' reports them too.")

(defun org-scribe--compile-pandoc-export ()
  "Convert the intermediate .org file to .docx with pandoc.
Returns the .docx file's name, relative to its directory -- the same
contract `org-odt-export-to-odt' and `org-latex-export-to-pdf' satisfy,
so pandoc slots into `org-scribe--compile-export' as just another
:exporter rather than a special case.

Called with `current-buffer' visiting the intermediate, exactly as those
two are, but unlike them pandoc is not Elisp: it is an external process
reading the file already saved to disk, so the buffer is consulted only
for its file name.  `default-directory' is the intermediate's directory
because Emacs sets it that way for any buffer visiting a file, which is
what lets the output name stay relative and the working directory stay
implicit rather than threading it through explicitly."
  (let* ((source (buffer-file-name))
         (target (concat (file-name-base source) ".docx")))
    (with-temp-buffer
      (let ((status (call-process "pandoc" nil t nil
                                  "-f" "org" "-t" "docx"
                                  source "-o" target)))
        (unless (zerop status)
          (user-error "%s" (org-scribe-msg 'compile-pandoc-failed
                                           (string-trim (buffer-string)))))))
    target))

(defconst org-scribe--compile-shunn-formats '(org txt pdf docx)
  "Formats the `shunn' style supports.
Per the style x format matrix this module was designed against: `md' has
no layout for a submission standard to apply to, and `odt' is deferred
(no shipped styles file to carry the formatting -- the same reasoning
that keeps every other style file out of the shipped templates).")

(defun org-scribe--compile-read-style ()
  "Prompt for a compile style, returning its symbol."
  (intern (completing-read (org-scribe-msg 'compile-prompt-style)
                           '("clean" "shunn") nil t nil nil "clean")))

(defun org-scribe--compile-read-format (&optional style)
  "Prompt for an output format, returning its symbol.
When STYLE is `shunn', only `org-scribe--compile-shunn-formats' is
offered -- excluding a doomed choice from completion up front, rather
than letting the writer pick `md' and then explaining why it was
refused."
  (let ((choices (if (eq style 'shunn)
                     (mapcar #'symbol-name org-scribe--compile-shunn-formats)
                   (mapcar (lambda (entry) (symbol-name (car entry)))
                           org-scribe--compile-formats))))
    (intern (completing-read (org-scribe-msg 'compile-prompt-format)
                             choices nil t nil nil
                             (if (member "txt" choices) "txt" (car choices))))))

(defun org-scribe--compile-export (spec intermediate output)
  "Export INTERMEDIATE to OUTPUT per format SPEC, returning the file written.

*ODT is a packaged format, not a text one*, and getting this wrong
produces a broken file rather than an error.  `org-export-to-file'
writes the transcoded string straight to the target, which for `odt' is
the bare content.xml: a file with an .odt extension that is not a zip
container and that no reader will open.  ODT must go through
`org-odt-export-to-odt', which builds the container, adds
styles.xml/meta.xml/mimetype/manifest and zips the result -- and which
also let-binds the `hfy-*' variables `org-odt-template' expects, so
calling it is what keeps a manuscript (a document with no source blocks
to fontify) from signalling `void-variable hfy-user-sheet-assoc'.

`org-odt-export-to-odt', `org-latex-export-to-pdf' and
`org-scribe--compile-pandoc-export' all follow the same contract -- name
their own output, beside the buffer's file and sharing its base name --
which is why the intermediate is always written with the base name every
output wants, and why any future packaged or external-tool format (EPUB)
needs only an :exporter entry in the format spec, never a branch here."
  (let ((buffer (find-file-noselect intermediate))
        (exporter (plist-get spec :exporter)))
    (unwind-protect
        (with-current-buffer buffer
          (if exporter
              ;; `org-odt-export-to-odt' returns a *relative* name, which
              ;; only resolves against the intermediate's directory --
              ;; not the caller's.  Absolutize it, or the path handed
              ;; back (and shown to the writer) points nowhere.
              (expand-file-name (funcall exporter)
                                (file-name-directory intermediate))
            (org-export-to-file (plist-get spec :backend) output)))
      (kill-buffer buffer))))

;;; Command

;;;###autoload
(defun org-scribe-compile (&optional style format)
  "Compile the project manuscript into a clean document.

STYLE is `clean' (the default): a readable draft with chapter headings
kept, scene headings dropped, breaks between scenes, and all planning
apparatus removed.  `shunn' additionally produces a submission-format
manuscript: a contact block, rounded word count, centered title and
byline, and -- for `pdf' -- 1-inch margins, double spacing, and a
running header, all built from `org-scribe-author-*' (per-author
defcustoms) and the project's `Pen-name'/`Running-header' markers.
`shunn' is refused for a project with no `org-scribe-author-name' set,
since a submission manuscript with a blank contact block would be worse
than an explicit error, and for `md'/`odt' output, which the style does
not support (see `org-scribe--compile-shunn-formats').

FORMAT is one of the keys of `org-scribe--compile-formats': `org',
`txt', `md' and `odt' need nothing beyond Emacs; `pdf' needs a LaTeX
distribution (pdflatex on PATH) and `docx' needs pandoc.  A missing
toolchain is reported by name -- see `org-scribe-setup-check' -- before
anything is written, rather than partway through.  The intermediate Org
document is always written too, whatever FORMAT is, so there is
something to inspect when the output looks wrong.

Both are written to `org-scribe-compile-output-directory' under the
project root.  That directory is a build artifact: it is worth adding to
the project's .gitignore, which org-scribe deliberately does not edit on
your behalf."
  (interactive (let ((style (org-scribe--compile-read-style)))
                (list style (org-scribe--compile-read-format style))))
  (let* ((style (or style 'clean))
         (format (or format 'txt))
         (spec (alist-get format org-scribe--compile-formats))
         (root (org-scribe-project-root)))
    (unless (memq style '(clean shunn))
      (user-error "%s" (org-scribe-msg 'compile-style-unsupported style)))
    (unless spec
      (user-error "%s" (org-scribe-msg 'compile-format-unknown format)))
    (when (eq style 'shunn)
      (unless (memq format org-scribe--compile-shunn-formats)
        (user-error "%s" (org-scribe-msg 'compile-shunn-format-unsupported format)))
      (unless (org-string-nw-p org-scribe-author-name)
        (user-error "%s" (org-scribe-msg 'compile-shunn-author-missing))))
    (unless root
      (user-error "%s" (org-scribe-msg 'compile-not-in-project)))
    (let ((manuscript (org-scribe--compile-manuscript-file root)))
      (unless manuscript
        (user-error "%s" (org-scribe-msg 'compile-no-manuscript root)))
      (let* ((library (plist-get spec :library))
             (backend (plist-get spec :backend))
             (executable (plist-get spec :executable)))
        (when (and library (not (require library nil t)))
          (user-error "%s" (org-scribe-msg 'compile-backend-missing format library)))
        (when (and executable (not (executable-find executable)))
          (user-error "%s" (org-scribe-msg 'compile-backend-missing format executable)))
        (pcase-let* ((`(,blocks . ,text) (org-scribe-compile-normalize manuscript style root))
                     (chapters (seq-count (lambda (b) (eq (car-safe b) 'chapter)) blocks))
                     (scenes (seq-count (lambda (b) (eq (car-safe b) 'scene)) blocks))
                     (directory (expand-file-name org-scribe-compile-output-directory root))
                     (base (concat (file-name-base manuscript) "-" (symbol-name style)))
                     (intermediate (expand-file-name (concat base ".org") directory)))
          (unless blocks
            (user-error "%s" (org-scribe-msg 'compile-empty)))
          (make-directory directory t)
          (with-temp-file intermediate (insert text))
          (let ((output
                 ;; `org' is the only format with neither a real backend
                 ;; nor an :exporter -- the intermediate *is* its output.
                 ;; `docx' has an :exporter but no Org export backend
                 ;; (pandoc is external), so the branch must check both,
                 ;; not just `backend'.
                 (if (or backend (plist-get spec :exporter))
                     (org-scribe--compile-export
                     spec intermediate
                     (expand-file-name
                      (concat base "." (plist-get spec :extension))
                      directory))
                   intermediate)))
            (message "%s" (org-scribe-msg 'compile-done
                                          chapters (org-scribe-plural chapters "")
                                          scenes (org-scribe-plural scenes "")
                                          (abbreviate-file-name output)))
            output))))))

(provide 'org-scribe-compile)

;;; org-scribe-compile.el ends here
