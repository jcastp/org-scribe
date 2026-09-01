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
;;   manuscript.org --normalize--> <clean .org> --stock ox-*--> txt/md/odt
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

(defun org-scribe--compile-to-org (blocks keywords)
  "Return the intermediate Org document for BLOCKS with KEYWORDS."
  (with-temp-buffer
    (dolist (keyword '("TITLE" "AUTHOR" "DATE" "LANGUAGE"))
      (when-let* ((value (car (cdr (assoc keyword keywords))))
                  ((org-string-nw-p value)))
        (insert (format "#+%s: %s\n" keyword value))))
    ;; num:nil because the writer's own heading text already says
    ;; "Chapter 1"; auto-numbering would silently disagree with a
    ;; chapter the writer renamed.
    (insert "#+OPTIONS: toc:nil num:nil todo:nil tags:nil\n\n")
    (dolist (block blocks)
      (pcase block
        (`(chapter . ,title) (insert "* " title "\n\n"))
        (`(,(or 'prose 'scene) . ,text) (insert text "\n\n"))
        (`(break) (insert (org-scribe--compile-scene-break) "\n\n"))))
    (buffer-string)))

(defun org-scribe-compile-normalize (file)
  "Return (BLOCKS . ORG-TEXT) compiled from the manuscript FILE.
A buffer opened here purely to read FILE is closed again; one the writer
already had open is left alone, and its unsaved edits are what gets
compiled -- which is the useful behavior when checking a draft."
  (let* ((existing (get-file-buffer file))
         (buffer (or existing (find-file-noselect file))))
    (unwind-protect
        (with-current-buffer buffer
          (org-with-wide-buffer
           (let* ((levels (org-scribe--compile-levels-for (org-scribe-project-type)))
                  (blocks (org-scribe--compile-blocks (org-element-parse-buffer) levels)))
             (cons blocks (org-scribe--compile-to-org
                           blocks (org-scribe--compile-keywords))))))
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

;;; Output Formats

(defconst org-scribe--compile-formats
  '((org . (:extension "org" :backend nil    :library nil))
    (txt . (:extension "txt" :backend ascii  :library ox-ascii))
    (md  . (:extension "md"  :backend md     :library ox-md))
    (odt . (:extension "odt" :backend odt    :library ox-odt
            :exporter org-odt-export-to-odt)))
  "Formats `org-scribe-compile' can produce, and what each needs.
Phase 1 deliberately ships only the formats that require no toolchain
beyond Emacs itself.  PDF (LaTeX) and DOCX (pandoc) are separate work,
and each will be reported by `org-scribe-setup-check' when it lands, so
a missing toolchain is a legible message rather than a bad export.")

(defun org-scribe--compile-read-format ()
  "Prompt for an output format, returning its symbol."
  (intern (completing-read (org-scribe-msg 'compile-prompt-format)
                           (mapcar (lambda (entry) (symbol-name (car entry)))
                                   org-scribe--compile-formats)
                           nil t nil nil "txt")))

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

`org-odt-export-to-odt' names its own output, beside the buffer's file
and sharing its base name, which is why the intermediate is written with
the base name the output wants.  Any future packaged backend (EPUB) needs
the same treatment, hence `:exporter' in the format spec rather than a
branch on `odt'."
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

STYLE is `clean' (the default and, for now, the only one): a readable
draft with chapter headings kept, scene headings dropped, breaks between
scenes, and all planning apparatus removed.  `shunn' -- submission
format -- is refused rather than approximated, because it needs author
and address data the project does not yet record.

FORMAT is one of the keys of `org-scribe--compile-formats'.  The
intermediate Org document is always written, whatever FORMAT is, so
there is something to inspect when the output looks wrong.

Both are written to `org-scribe-compile-output-directory' under the
project root.  That directory is a build artifact: it is worth adding to
the project's .gitignore, which org-scribe deliberately does not edit on
your behalf."
  (interactive (list 'clean (org-scribe--compile-read-format)))
  (let* ((style (or style 'clean))
         (format (or format 'txt))
         (spec (alist-get format org-scribe--compile-formats))
         (root (org-scribe-project-root)))
    (unless (eq style 'clean)
      (user-error "%s" (org-scribe-msg 'compile-style-unsupported style)))
    (unless spec
      (user-error "%s" (org-scribe-msg 'compile-format-unknown format)))
    (unless root
      (user-error "%s" (org-scribe-msg 'compile-not-in-project)))
    (let ((manuscript (org-scribe--compile-manuscript-file root)))
      (unless manuscript
        (user-error "%s" (org-scribe-msg 'compile-no-manuscript root)))
      (let* ((library (plist-get spec :library))
             (backend (plist-get spec :backend)))
        (when (and library (not (require library nil t)))
          (user-error "%s" (org-scribe-msg 'compile-backend-missing format library)))
        (pcase-let* ((`(,blocks . ,text) (org-scribe-compile-normalize manuscript))
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
                 (if (null backend)
                     intermediate
                   (org-scribe--compile-export
                    spec intermediate
                    (expand-file-name
                     (concat base "." (plist-get spec :extension))
                     directory)))))
            (message "%s" (org-scribe-msg 'compile-done
                                          chapters (org-scribe-plural chapters "")
                                          scenes (org-scribe-plural scenes "")
                                          (abbreviate-file-name output)))
            output))))))

(provide 'org-scribe-compile)

;;; org-scribe-compile.el ends here
