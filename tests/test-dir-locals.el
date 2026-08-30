;;; test-dir-locals.el --- Tests for the project-wide spelling dictionary -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; A writing project is monolingual by construction: `.org-scribe-project'
;; records the language and that choice picks the whole template set.  The
;; spelling dictionary is therefore a property of the *project*, and is
;; written once to `.dir-locals.el' at creation time rather than repeated in
;; a per-file `Local Variables' block in each template.
;;
;; Before this, eight of the shipped templates carried such a block and the
;; rest did not — notes, the writing journal, the README, the whole English
;; set bar the manuscript, and every file created later by capture or by the
;; planner were simply uncovered.  Worse, the two manuscripts declared the
;; dictionary through an `eval:' form, which is never a safe file-local, so
;; Emacs prompted on every open of novel.org.
;;
;; These tests pin both halves: that creation writes the file, and that it
;; writes a form which applies *without* prompting.
;;
;; The file names the language twice, once per spell checker: ispell and
;; jinx read different variables and neither falls back to the other, so a
;; file that mentions only one silently leaves the other checker — and the
;; word completion that follows it — on the user's global default.

;;; Code:

(require 'ert)
(require 'org)

(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" default-directory))
  (add-to-list 'load-path (expand-file-name "../templates" default-directory)))

(require 'org-scribe-config)
(require 'org-scribe-project)
(require 'ispell)

;;; Helpers

(defmacro test-dir-locals--with-temp-base-dir (var &rest body)
  "Bind VAR to a fresh temp directory for BODY, then delete it."
  (declare (indent 1))
  `(let ((,var (make-temp-file "org-scribe-dir-locals-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,var t))))

(defun test-dir-locals--kill-file-buffer (file)
  "Kill any buffer visiting FILE, if project creation opened one."
  (let ((buf (get-file-buffer file)))
    (when buf (kill-buffer buf))))

(defun test-dir-locals--read (project-dir)
  "Return the parsed contents of PROJECT-DIR/.dir-locals.el."
  (with-temp-buffer
    (insert-file-contents (expand-file-name ".dir-locals.el" project-dir))
    (read (current-buffer))))

(defun test-dir-locals--dictionary (project-dir)
  "Return the ispell dictionary recorded in PROJECT-DIR/.dir-locals.el."
  (alist-get 'ispell-local-dictionary
             (alist-get nil (test-dir-locals--read project-dir))))

(defun test-dir-locals--jinx-language (project-dir)
  "Return the jinx language recorded in PROJECT-DIR/.dir-locals.el."
  (alist-get 'jinx-languages
             (alist-get nil (test-dir-locals--read project-dir))))

;;; Creation

(ert-deftest test-dir-locals-novel-spanish-project-gets-spanish-dictionary ()
  "Creating a Spanish novel project writes .dir-locals.el with es_ES."
  (test-dir-locals--with-temp-base-dir base-dir
    (let ((project-dir (expand-file-name "Mi Novela" base-dir)))
      (org-scribe-create-novel-project base-dir "Mi Novela" 'es)
      (unwind-protect
          (progn
            (should (file-exists-p (expand-file-name ".dir-locals.el" project-dir)))
            (should (equal (test-dir-locals--dictionary project-dir) "es_ES")))
        (test-dir-locals--kill-file-buffer
         (expand-file-name "README.org" project-dir))))))

(ert-deftest test-dir-locals-novel-english-project-gets-english-dictionary ()
  "Creating an English novel project writes .dir-locals.el with en_US."
  (test-dir-locals--with-temp-base-dir base-dir
    (let ((project-dir (expand-file-name "My Novel" base-dir)))
      (org-scribe-create-novel-project base-dir "My Novel" 'en)
      (unwind-protect
          (should (equal (test-dir-locals--dictionary project-dir) "en_US"))
        (test-dir-locals--kill-file-buffer
         (expand-file-name "README.org" project-dir))))))

(ert-deftest test-dir-locals-short-story-project-gets-a-dictionary ()
  "Short story projects get the file too.
Neither short-story template set ever carried a `Local Variables'
block, so this is the set that gains coverage it never had."
  (test-dir-locals--with-temp-base-dir base-dir
    (let ((project-dir (expand-file-name "Mi Cuento" base-dir)))
      (org-scribe-create-short-story-project base-dir "Mi Cuento" 'es)
      (unwind-protect
          (should (equal (test-dir-locals--dictionary project-dir) "es_ES"))
        (test-dir-locals--kill-file-buffer
         (expand-file-name "cuento.org" project-dir))))))

;;; The form that is written

(ert-deftest test-dir-locals-written-form-is-well-formed ()
  "The generated file parses to the directory-local alist Emacs expects."
  (test-dir-locals--with-temp-base-dir root
    (org-scribe--write-dir-locals root 'es)
    (should (equal (test-dir-locals--read root)
                   '((nil . ((ispell-local-dictionary . "es_ES")
                             (jinx-languages . "es_ES"))))))))

(ert-deftest test-dir-locals-variable-written-is-safe ()
  "The dictionary applies without prompting.

This is the whole user-visible point of the change: the manuscript
templates used to declare the dictionary through an `eval:' form, which
Emacs never considers safe, so it asked the writer to approve the local
variables list every single time the file was opened.
`ispell-local-dictionary' carries a `safe-local-variable' property;
`ispell-dictionary' does not, and must never be used here."
  (should (safe-local-variable-p 'ispell-local-dictionary "es_ES"))
  (should-not (safe-local-variable-p 'ispell-dictionary "es_ES"))
  (test-dir-locals--with-temp-base-dir root
    (org-scribe--write-dir-locals root 'en)
    (let ((entries (alist-get nil (test-dir-locals--read root))))
      (should entries)
      (dolist (entry entries)
        (should (safe-local-variable-p (car entry) (cdr entry)))))))

(ert-deftest test-dir-locals-covers-both-spell-checkers ()
  "The file pins the language for jinx as well as for ispell.

Neither checker reads the other's variable, so writing only
`ispell-local-dictionary' left every jinx user — and, through
`cape-dict', their word completion — on the global default.  In a
Spanish project that means English spell-check and English candidates,
with nothing in the project to explain why."
  (test-dir-locals--with-temp-base-dir root
    (org-scribe--write-dir-locals root 'es)
    (should (equal (test-dir-locals--dictionary root) "es_ES"))
    (should (equal (test-dir-locals--jinx-language root) "es_ES"))))

(ert-deftest test-dir-locals-jinx-language-is-safe-without-jinx ()
  "`jinx-languages' applies without prompting even when jinx is absent.

Jinx declares the property itself, but through an autoload, so it is
only in force once jinx is installed.  Without org-scribe declaring it
too, a project opened on a machine that lacks jinx would ask the writer
to approve the local variables list on every file — reintroducing the
prompt this whole arrangement exists to remove.  This test runs in a
batch Emacs with no jinx loaded, which is exactly the case at issue."
  (should-not (featurep 'jinx))
  (should (safe-local-variable-p 'jinx-languages "es_ES"))
  (should-not (safe-local-variable-p 'jinx-languages 42)))

(ert-deftest test-dir-locals-regional-variant-reaches-both-variables ()
  "A configured regional dictionary is written for both checkers.
One option governs both, so the two can never disagree about a project."
  (test-dir-locals--with-temp-base-dir root
    (let ((org-scribe-ispell-dictionaries '((es . "es_MX"))))
      (org-scribe--write-dir-locals root 'es))
    (should (equal (test-dir-locals--dictionary root) "es_MX"))
    (should (equal (test-dir-locals--jinx-language root) "es_MX"))))

(ert-deftest test-dir-locals-names-the-authoritative-file ()
  "The generated file points a reader at .org-scribe-project.
The marker file is the single source of truth for the language; a reader
who finds the two disagreeing needs to know which one wins."
  (test-dir-locals--with-temp-base-dir root
    (org-scribe--write-dir-locals root 'es)
    (with-temp-buffer
      (insert-file-contents (expand-file-name ".dir-locals.el" root))
      (should (string-match-p "org-scribe-project" (buffer-string)))
      (should (string-match-p "org-scribe-update-dir-locals" (buffer-string))))))

;;; Configuration

(ert-deftest test-dir-locals-suppressed-by-defcustom ()
  "`org-scribe-write-dir-locals' nil suppresses the file entirely."
  (test-dir-locals--with-temp-base-dir root
    (let ((org-scribe-write-dir-locals nil))
      (should-not (org-scribe--write-dir-locals root 'es)))
    (should-not (file-exists-p (expand-file-name ".dir-locals.el" root)))))

(ert-deftest test-dir-locals-regional-variant-reaches-the-file ()
  "A configured regional dictionary is what gets written."
  (test-dir-locals--with-temp-base-dir root
    (let ((org-scribe-ispell-dictionaries '((es . "es_MX") (en . "en_GB"))))
      (org-scribe--write-dir-locals root 'es))
    (should (equal (test-dir-locals--dictionary root) "es_MX"))))

(ert-deftest test-dir-locals-language-without-dictionary-writes-nothing ()
  "A language with no configured dictionary is not guessed at.
Both a nil entry and a missing one mean \"write no dictionary\"."
  (test-dir-locals--with-temp-base-dir root
    (let ((org-scribe-ispell-dictionaries '((es . nil))))
      (should-not (org-scribe--write-dir-locals root 'es))
      (should-not (org-scribe--write-dir-locals root 'fr)))
    (should-not (file-exists-p (expand-file-name ".dir-locals.el" root)))))

;;; Not clobbering the user's own file

(ert-deftest test-dir-locals-existing-file-is-left-alone ()
  "An existing .dir-locals.el is the user's; creation must not overwrite it.
Merging into an arbitrary directory-locals alist is not something this
package should guess at, so it declines rather than guessing."
  (test-dir-locals--with-temp-base-dir root
    (let ((file (expand-file-name ".dir-locals.el" root)))
      (with-temp-file file
        (insert "((nil . ((fill-column . 72))))\n"))
      (should-not (org-scribe--write-dir-locals root 'es))
      (with-temp-buffer
        (insert-file-contents file)
        (should (string-match-p "fill-column" (buffer-string)))
        (should-not (string-match-p "ispell" (buffer-string)))))))

;;; Regeneration reads the marker file, not the global default

(ert-deftest test-dir-locals-update-reads-language-from-marker ()
  "`org-scribe-update-dir-locals' takes the language from .org-scribe-project.

The drift this guards against is real: `org-scribe-template-language' is
a global default for *new* projects, so regenerating from it would give
a Spanish project an English dictionary the moment the user's default
differs from the project at hand."
  (test-dir-locals--with-temp-base-dir root
    (with-temp-file (expand-file-name ".org-scribe-project" root)
      (insert "# Writing project: Mi Novela\n# Type: novel\n# Language: es\n"))
    (let ((org-scribe-template-language 'en)
          (default-directory (file-name-as-directory root)))
      (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () root)))
        (org-scribe-update-dir-locals)))
    (should (equal (test-dir-locals--dictionary root) "es_ES"))))

(ert-deftest test-dir-locals-update-overwrites-only-on-confirmation ()
  "Regeneration replaces an existing file only when the user agrees."
  (test-dir-locals--with-temp-base-dir root
    (with-temp-file (expand-file-name ".org-scribe-project" root)
      (insert "# Language: es\n"))
    (with-temp-file (expand-file-name ".dir-locals.el" root)
      (insert "((nil . ((fill-column . 72))))\n"))
    (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () root))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (org-scribe-update-dir-locals))
    (should-not (test-dir-locals--dictionary root))
    (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () root))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (org-scribe-update-dir-locals))
    (should (equal (test-dir-locals--dictionary root) "es_ES"))))

(ert-deftest test-dir-locals-update-outside-a-project-signals ()
  "Outside a project there is no language to read, so the command refuses."
  (cl-letf (((symbol-function 'org-scribe-project-root) (lambda () nil)))
    (should-error (org-scribe-update-dir-locals) :type 'user-error)))

(provide 'test-dir-locals)

;;; test-dir-locals.el ends here
