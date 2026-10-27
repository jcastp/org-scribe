;;; org-scribe-project.el --- Novel project structure generator -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides functions to create complete novel and short story
;; project structures from templates with variable substitution.
;;
;; Main functions:
;;   - org-scribe-create-novel-project: Create new novel project from templates
;;   - org-scribe-create-short-story-project: Create new short story project
;;   - org-scribe-insert-scene: Insert scene template
;;   - org-scribe-insert-chapter: Insert chapter template
;;   - org-scribe-open-project-file: Quick file navigation
;;
;; Template Variables:
;;   ${TITLE}  - Project title
;;   ${AUTHOR} - User's full name
;;   ${DATE}   - Current date (YYYY-MM-DD format)
;;
;; This was merged from org-scribe-template package (v2.0).

;;; Code:

(require 'project)
(require 'org-scribe-messages)
(require 'org-scribe-core)

;; Linking is loaded after this module; these are resolved at runtime when
;; `org-scribe--auto-setup-links' runs during project creation.
(defvar org-scribe-entity-registry)
(declare-function org-scribe--add-entity-ids "org-scribe-linking-core")
(declare-function org-scribe-project-structure "org-scribe-core")
(declare-function org-scribe-project-type-cache-clear "org-scribe-core")
(declare-function org-scribe-planner--offer-plan-on-create "org-scribe-planner"
                  (project-dir title))
(defvar org-scribe-planner-offer-plan-on-create)
(defvar org-scribe-compile-output-directory)

;;; Configuration

(defvar org-scribe-project-package-directory
  (file-name-directory
   (or load-file-name
       (buffer-file-name)))
  "Directory where this file is located.")

(defcustom org-scribe-template-language 'en
  "Default language for novel templates.
Can be \\='en for English or \\='es for Spanish.
It selects which template set new projects are created from."
  :type '(choice (const :tag "English" en)
                 (const :tag "Spanish" es))
  :group 'org-scribe)

;;; Project Creation

(defun org-scribe--validate-project-title (title)
  "Validate TITLE for use as a directory name.
Returns nil if valid, otherwise returns an error message.
The title is also the directory name, hence the filesystem rules; a
title the directory cannot carry (a colon, a question mark) can be set
afterwards with `org-scribe-set-title', whose rule is looser."
  (cond
   ((string-empty-p (string-trim title))
    (org-scribe-msg 'error-empty-title))
   ((string-match-p "[\n\r]" title)
    (org-scribe-msg 'error-title-newline))
   ((not (string= title (string-trim title)))
    (org-scribe-msg 'error-title-padded))
   ((> (string-bytes title) 200)
    (org-scribe-msg 'error-title-too-long))
   ((string-match-p "[/\\]" title)
    (org-scribe-msg 'error-path-separator))
   ((string-match-p ":" title)
    (org-scribe-msg 'error-title-colon))
   ((string-match-p "[*?<>|\"']" title)
    (org-scribe-msg 'error-title-special-chars))
   ((string-match-p "^\\." title)
    (org-scribe-msg 'error-title-dot))
   ((string-match-p "\\.\\." title)
    (org-scribe-msg 'error-title-double-dot))
   (t nil)))

(defun org-scribe--default-author ()
  "Return the author name for a new manuscript.
`org-scribe-author-name' when set (it is what the Shunn front matter
uses, so both outputs agree), else `user-full-name', else \"Author\"."
  (cond ((and (boundp 'org-scribe-author-name)
              (stringp org-scribe-author-name)
              (not (string-empty-p org-scribe-author-name)))
         org-scribe-author-name)
        ((and (boundp 'user-full-name) (stringp user-full-name)
              (not (string-empty-p user-full-name)))
         user-full-name)
        (t "Author")))

(defun org-scribe--git (&rest args)
  "Run git with ARGS in `default-directory'; return t on success.
A missing git executable is not an error: git is optional, and a
missing program must not abort a creation half way."
  (and (executable-find "git")
       (zerop (apply #'call-process "git" nil nil nil args))))

(defun org-scribe--auto-setup-links (project-dir)
  "Give every template entity in PROJECT-DIR a stable ID.

Linking is on by default: the user should never run a separate \"setup\"
step.  Right after a project is created, this mints an ID for every
character, location, and plot-thread heading shipped in the planning
files, so scene links stay stable even as those entities are renamed.

Works off `org-scribe-entity-registry', so any future entity type is
covered automatically.  Errors are swallowed — this is a convenience and
must never make project creation fail."
  (require 'org-scribe-linking-core)
  ;; The marker file already exists, so project detection resolves to the
  ;; new project; drop any stale cached type for this root first.
  (org-scribe-project-type-cache-clear project-dir)
  (ignore-errors
    ;; Routine bookkeeping: keep it out of the echo area and *Messages*.
    (let ((default-directory (file-name-as-directory project-dir))
          (inhibit-message t)
          (message-log-max nil))
      (dolist (entry org-scribe-entity-registry)
        (let* ((entity (cdr entry))
               (file (funcall (plist-get entity :file-fn))))
          (when (and file (file-exists-p file))
            (org-scribe--add-entity-ids entity)))))))

;;; Directory-local spelling dictionary

;; A project is monolingual by construction: `.org-scribe-project' records
;; the language and it picks the whole template set.  The spelling
;; dictionary is therefore a property of the project, not of each file, and
;; is written once to `.dir-locals.el' rather than repeated in a per-file
;; `Local Variables' block.  Templates deliberately ship no such block —
;; see `test-template-parity-templates-carry-no-local-variables'.
;;
;; Templates do still ship a `#+LANGUAGE:' keyword, which is not a
;; duplicate of this file: it is the per-file fallback for a file read
;; outside its project tree, and jinx reads it natively.  Where both are
;; present the directory-local wins, since `jinx--get-org-language' only
;; consults the keyword when `jinx-languages' is not already buffer-local.
;; That is the right precedence — the project is authoritative — and it is
;; why a writer who deletes the keyword line loses nothing.

;; Jinx declares this itself, but via an autoload, so it is only in force
;; once jinx is installed.  Declaring it here as well means the generated
;; file applies silently for every org-scribe user, jinx or no jinx —
;; without it, a project opened on a machine that lacks jinx prompts the
;; writer to approve the local variables list on every single file, which
;; is the exact failure this whole arrangement exists to prevent.  The
;; predicate is jinx's own.
(put 'jinx-languages 'safe-local-variable #'stringp)

(defun org-scribe--dir-locals-dictionary (language)
  "Return the dictionary/language name configured for LANGUAGE, or nil.
Consults `org-scribe-ispell-dictionaries' first -- a documented user
override for a regional variant (\"es_MX\", \"en_GB\") -- so an EXPLICIT
entry there, even one of nil, is respected as given (an explicit nil
still means \"write no dictionary\", never guessed at).  Only when
LANGUAGE has no entry there at all does this fall back to the LANGUAGE
pack's own `:ispell' default (see `lang/org-scribe-lang.el'), and only
when LANGUAGE is actually registered -- a language nothing knows about
gets no dictionary either, quietly, rather than signalling.  The name
is written both as an ispell dictionary and as a jinx language: the
codes coincide (\"es_ES\", \"en_US\"), since both resolve against the
installed hunspell data."
  (let ((cell (assq language org-scribe-ispell-dictionaries)))
    (if cell
        (cdr cell)
      (and (memq language (org-scribe-languages))
           (plist-get (org-scribe-lang-pack language) :ispell)))))

(defun org-scribe--dir-locals-content (language dictionary)
  "Return the text of a `.dir-locals.el' pinning DICTIONARY for LANGUAGE.

Two variables are written because the two spell checkers in common use
read different ones, and neither reads the other's:
`ispell-local-dictionary' for ispell/flyspell, and `jinx-languages' for
jinx, which also drives word completion for anyone whose `cape-dict'
word list follows the buffer language.  Writing only one leaves the
other checker on the user's global default — in a Spanish project that
means English spell-check and English completion candidates, with no
visible cause.

Both carry a `safe-local-variable' property, so the file applies without
prompting — unlike the `eval:' form the manuscript templates used to
ship.  Jinx declares its own via an autoload; org-scribe declares it too,
so the generated file is silent even when jinx is not installed.  The key is nil rather than `org-mode' because a project's
notes may hold other modes and the language is right for all of them."
  (format ";;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; Written by org-scribe.  Language: %s (see .org-scribe-project).
;;; Regenerate with M-x org-scribe-update-dir-locals.

((nil . ((ispell-local-dictionary . %S)
         (jinx-languages . %S))))
"
          language dictionary dictionary))

(defun org-scribe--write-dir-locals (project-dir language)
  "Write PROJECT-DIR/.dir-locals.el pinning the dictionary for LANGUAGE.

Does nothing when `org-scribe-write-dir-locals' is nil, when LANGUAGE has
no dictionary in `org-scribe-ispell-dictionaries', or when the file
already exists — an existing `.dir-locals.el' is the user's, and merging
into it is not something this package should guess at.  Returns the
dictionary written, or nil."
  (when org-scribe-write-dir-locals
    (let ((dictionary (org-scribe--dir-locals-dictionary language))
          (file (expand-file-name ".dir-locals.el" project-dir)))
      (when (and dictionary (not (file-exists-p file)))
        (with-temp-file file
          (insert (org-scribe--dir-locals-content language dictionary)))
        dictionary))))

;;;###autoload
(defun org-scribe-update-dir-locals ()
  "Create or refresh `.dir-locals.el' for the current project.

The language is read from the project's `.org-scribe-project' marker
file, which stays the single source of truth; the dictionary name comes
from `org-scribe-ispell-dictionaries'.  Use this on projects created
before org-scribe generated the file, or after changing the configured
dictionary.  An existing file is only replaced after confirmation."
  (interactive)
  ;; Not `org-scribe-project-root': it is never nil, so outside a project
  ;; this wrote a .dir-locals.el into whatever directory it was in.
  (let ((root (org-scribe--project-root-if-any)))
    (unless root
      (user-error "%s" (org-scribe-msg 'dir-locals-not-in-project)))
    (let* ((language (org-scribe-project-language))
           (dictionary (org-scribe--dir-locals-dictionary language))
           (file (expand-file-name ".dir-locals.el" root)))
      (unless dictionary
        (user-error "%s" (org-scribe-msg 'dir-locals-no-dictionary language)))
      (if (and (file-exists-p file)
               (not (yes-or-no-p (org-scribe-msg 'dir-locals-overwrite-confirm file))))
          (message "%s" (org-scribe-msg 'dir-locals-exists))
        (with-temp-file file
          (insert (org-scribe--dir-locals-content language dictionary)))
        (message "%s" (org-scribe-msg 'dir-locals-written dictionary))))))

(defun org-scribe--read-method (&optional language)
  "Prompt for a novel plotting method, returning its symbol.
LANGUAGE (\\='en or \\='es) selects which registered language pack's
`:method-labels' section is offered; defaults to
`org-scribe-template-language'.  The default candidate is \\='sistema,
matching what an omitted `# Method:' marker resolves to via
`org-scribe-project-method'."
  (let* ((language (or language org-scribe-template-language))
         (labels (plist-get (org-scribe-lang-pack language) :method-labels))
         (choices (mapcar (lambda (entry)
                            (cons (alist-get (car entry) labels) (car entry)))
                          org-scribe--methods))
         (default-label (car (rassq 'sistema choices))))
    (alist-get (completing-read (org-scribe-msg 'project-creation-method-prompt)
                                 (mapcar #'car choices) nil t nil nil default-label)
               choices nil nil #'string=)))

;; Number of outermost parts the shipped templates have: the three acts of
;; a novel, the three scenes of a short story.
(defconst org-scribe--word-target-parts 3)

(defun org-scribe--read-word-target (type)
  "Prompt for the total word target of a TYPE project, returning an integer.
TYPE is `novel' or `short-story'; the default offered comes from
`org-scribe-default-word-target'.  Asks again until the answer is a whole
number with at least one word for each outermost part."
  (let ((default (org-scribe-default-word-target-for type))
        (answer nil))
    (while (not (and answer (integerp answer)
                     (>= answer org-scribe--word-target-parts)))
      (setq answer (read-number (org-scribe-msg 'project-creation-word-target-prompt)
                                default)))
    answer))

(defun org-scribe--word-target-variables (target-words language)
  "Return the template variables for a project of TARGET-WORDS in LANGUAGE.
TOTAL_WORDS (plain digits), TOTAL_WORDS_FMT (with LANGUAGE's thousands
separator) and PART1_WORDS..PART3_WORDS (the exact split by
`org-scribe-word-target-proportions').  Signal a `user-error' for a bad
total or for proportions that are not exactly three weights; callers run
this before creating anything on disk."
  (unless (and (integerp target-words)
               (>= target-words org-scribe--word-target-parts))
    (user-error "%s" (org-scribe-msg 'error-word-target-invalid)))
  (unless (= (length org-scribe-word-target-proportions)
             org-scribe--word-target-parts)
    (user-error "%s" (org-scribe-msg 'error-word-target-proportions)))
  (let ((parts (org-scribe--split-word-target
                target-words org-scribe-word-target-proportions)))
    (when (memq 0 parts)
      (user-error "%s" (org-scribe-msg 'error-word-target-invalid)))
    `(("TOTAL_WORDS" . ,(number-to-string target-words))
      ("TOTAL_WORDS_FMT" . ,(org-scribe--format-word-count target-words language))
      ,@(cl-loop for part in parts
                 for i from 1
                 collect (cons (format "PART%d_WORDS" i) (number-to-string part))))))

;;;###autoload
(defun org-scribe-create-novel-project (base-dir title &optional language method target-words)
  "Create a new novel project structure from templates.
BASE-DIR is the parent directory where the project will be created.
TITLE is the name of the novel/project.
LANGUAGE selects which template set to use, either \\='en or \\='es.
When omitted (e.g. non-interactive callers), defaults to
`org-scribe-template-language'.
METHOD selects which plotting method's design file is deployed — one
of \\='sistema, \\='helice, or \\='matriz (see `org-scribe--methods').
When omitted, defaults to \\='sistema, exactly matching the legacy
behavior a project got before this argument existed.
TARGET-WORDS is the project's total word count, recorded in the marker
file and used to size the three acts; when omitted, the default for a
novel in `org-scribe-default-word-target'.

This function:
1. Validates the title
2. Creates the project directory
3. Initializes a git repository
4. Processes all template files with variable substitution
5. Fills the design notebook's Foundations part from the chosen method
6. Creates an initial git commit
7. Registers the project with project.el
8. Opens the manuscript"
  (interactive
   (let* ((base-dir (read-directory-name (org-scribe-msg 'project-creation-base-dir)
                                         org-scribe-projects-directory))
          (title (read-string (org-scribe-msg 'project-creation-novel-title)))
          (language (intern (completing-read (org-scribe-msg 'project-creation-language-prompt)
                                              (mapcar #'symbol-name (org-scribe-languages))
                                              nil t nil nil
                                              (symbol-name org-scribe-template-language)))))
     (list base-dir title language (org-scribe--read-method language)
           (org-scribe--read-word-target 'novel))))

  (let* ((language (or language org-scribe-template-language))
         (method (or method 'sistema))
         (target-words (or target-words (org-scribe-default-word-target-for 'novel)))
         (template-dir (expand-file-name
                        (concat "../org-scribe-templates/" (org-scribe-lang-template 'novel language))
                        org-scribe-project-package-directory)))

    (unless (file-directory-p template-dir)
      (user-error "%s" (org-scribe-msg 'error-template-not-found template-dir)))

    ;; Validate title
    (let ((validation-error (org-scribe--validate-project-title title)))
      (when validation-error
        (user-error "%s" validation-error)))

    (let* ((project-dir (expand-file-name (concat "./" title) base-dir))
           (variables `(("TITLE" . ,title)
                       ("AUTHOR" . ,(org-scribe--default-author))
                       ("DATE" . ,(format-time-string "%Y-%m-%d"))
                       ,@(org-scribe--word-target-variables target-words language)))
           ;; Resolved before anything is written, so a missing fragment
           ;; cannot leave a half-built directory behind.
           (foundations (org-scribe--foundations-fragment
                         method language template-dir variables)))

      ;; Check if project already exists
      (when (file-exists-p project-dir)
        (user-error "%s" (org-scribe-msg 'project-already-exists project-dir)))

      ;; Create project directory
      (make-directory project-dir t)

      ;; Create .org-scribe-project marker file for project detection
      (with-temp-file (expand-file-name ".org-scribe-project" project-dir)
        (insert (format "# Writing project: %s\n" title)
                (format "# Title: %s\n" title)
                (format "# Created: %s\n" (format-time-string "%Y-%m-%d"))
                (format "# Language: %s\n" language)
                (format "# Method: %s\n" method)
                (format "# Target-words: %d\n" target-words)))

      ;; Pin the spelling dictionary for the whole project, before the
      ;; initial commit so it is versioned like every other created file.
      (org-scribe--write-dir-locals project-dir language)

      ;; Initialize git repository
      (let ((default-directory project-dir))
        (unless (org-scribe--git "init")
          (warn "%s" (org-scribe-msg 'warn-git-init-failed))))

      ;; Process all templates; the notebook's Foundations part is the
      ;; method's own fragment (see `org-scribe--foundations-fragment').
      (org-scribe--copy-templates
       template-dir project-dir
       (cons (cons "FOUNDATIONS" foundations) variables))

      ;; Linking on by default: mint IDs for the template's entities so the
      ;; user never has to run a separate "setup" step (A7).
      (org-scribe--auto-setup-links project-dir)

      ;; Create initial git commit
      (let ((default-directory project-dir))
        (when (org-scribe--git "add" ".")
          (org-scribe--git "commit" "-m"
                           (format "Initial commit: %s" title))))

      ;; Register project with project.el
      (project-remember-project (project-current nil project-dir))

      ;; Open the manuscript
      (find-file (expand-file-name (org-scribe-lang-file 'manuscript-novel language) project-dir))
      (message "%s" (org-scribe-msg 'project-creation-success-novel title project-dir))
      (when (called-interactively-p 'any)
        (org-scribe--offer-plan-after-create project-dir title)))))

;;;###autoload
(defun org-scribe-create-short-story-project (base-dir title &optional language target-words)
  "Create a new short story project structure from templates.
BASE-DIR is the parent directory where the project will be created.
TITLE is the name of the short story/project.
LANGUAGE selects which template set to use, either \\='en or \\='es.
When omitted (e.g. non-interactive callers), defaults to
`org-scribe-template-language'.
TARGET-WORDS is the story's total word count, recorded in the marker
file and used to size its three scenes; when omitted, the default for a
short story in `org-scribe-default-word-target'.

This function:
1. Validates the title
2. Creates the project directory
3. Initializes a git repository
4. Processes all template files with variable substitution
5. Mints IDs for the template's characters and settings
6. Creates an initial git commit
7. Registers the project with project.el
8. Opens the story file, named for LANGUAGE"
  (interactive
   (list
    (read-directory-name (org-scribe-msg 'project-creation-base-dir) org-scribe-projects-directory)
    (read-string (org-scribe-msg 'project-creation-short-story-title))
    (intern (completing-read (org-scribe-msg 'project-creation-language-prompt)
                              (mapcar #'symbol-name (org-scribe-languages))
                              nil t nil nil
                              (symbol-name org-scribe-template-language)))
    (org-scribe--read-word-target 'short-story)))

  ;; Determine template directory based on language
  (let* ((language (or language org-scribe-template-language))
         (target-words (or target-words (org-scribe-default-word-target-for 'short-story)))
         (template-dir (expand-file-name
                      (concat "../org-scribe-templates/" (org-scribe-lang-template 'short-story language))
                      org-scribe-project-package-directory)))

    (unless (file-directory-p template-dir)
      (user-error "%s" (org-scribe-msg 'error-template-not-found template-dir)))

    ;; Validate title
    (let ((validation-error (org-scribe--validate-project-title title)))
      (when validation-error
        (user-error "%s" validation-error)))

    (let* ((project-dir (expand-file-name (concat "./" title) base-dir))
           (variables `(("TITLE" . ,title)
                       ("AUTHOR" . ,(org-scribe--default-author))
                       ("DATE" . ,(format-time-string "%Y-%m-%d"))
                       ,@(org-scribe--word-target-variables target-words language)))
           (story-file (org-scribe-lang-file 'manuscript-short language)))

      ;; Check if project already exists
      (when (file-exists-p project-dir)
        (user-error "%s" (org-scribe-msg 'project-already-exists project-dir)))

      ;; Create project directory
      (make-directory project-dir t)

      ;; Create .org-scribe-project marker file for project detection
      (with-temp-file (expand-file-name ".org-scribe-project" project-dir)
        (insert (format "# Writing project: %s\n" title)
                (format "# Title: %s\n" title)
                (format "# Type: short-story\n")
                (format "# Created: %s\n" (format-time-string "%Y-%m-%d"))
                (format "# Language: %s\n" language)
                (format "# Target-words: %d\n" target-words)))

      ;; Pin the spelling dictionary for the whole project, before the
      ;; initial commit so it is versioned like every other created file.
      (org-scribe--write-dir-locals project-dir language)

      ;; Initialize git repository
      (let ((default-directory project-dir))
        (unless (org-scribe--git "init")
          (warn "%s" (org-scribe-msg 'warn-git-init-failed))))

      ;; Process all templates
      (org-scribe--copy-templates template-dir project-dir variables)

      ;; Mint IDs for the template's characters and settings, as for a
      ;; novel.  Without this they carried `:Role:'/`:Type:' but no `:ID:',
      ;; and every entity getter skips an ID-less heading — so a new short
      ;; story had no characters to link and the health report called it
      ;; "no orphans" until the writer found the setup command.
      (org-scribe--auto-setup-links project-dir)

      ;; Create initial git commit
      (let ((default-directory project-dir))
        (when (org-scribe--git "add" ".")
          (org-scribe--git "commit" "-m"
                           (format "Initial commit: %s" title))))

      ;; Register project with project.el
      (project-remember-project (project-current nil project-dir))

      ;; Open the story file
      (find-file (expand-file-name story-file project-dir))
      (message "%s" (org-scribe-msg 'project-creation-success-short-story title project-dir))
      (when (called-interactively-p 'any)
        (org-scribe--offer-plan-after-create project-dir title)))))

(defun org-scribe--offer-plan-after-create (project-dir title)
  "Offer a writing plan for the project TITLE just created in PROJECT-DIR.
Called by the two creation commands when run interactively.  The offer
used to be advice the planner installed on them when it loaded, and the
planner is autoloaded, so a fresh session never offered a plan; the
advice also fired for programmatic calls.  Honours
`org-scribe-planner-offer-plan-on-create', which a writer may have set
before the planner loads (unbound means its default, t).

Whatever the answer changed — the Planner and Plan lines of the marker,
plan.org — is committed afterwards (see
`org-scribe--commit-plan-offer'), even if the offer is cut short, so a
new project's repository is clean."
  (unwind-protect
      (when (if (boundp 'org-scribe-planner-offer-plan-on-create)
                org-scribe-planner-offer-plan-on-create
              t)
        (when (require 'org-scribe-planner nil t)
          (org-scribe-planner--offer-plan-on-create project-dir title)))
    (org-scribe--commit-plan-offer project-dir title)))

(defun org-scribe--commit-plan-offer (project-dir title)
  "Commit what the plan offer changed in PROJECT-DIR, if anything.
The initial commit is made before the offer, so accepting left
plan.org untracked and the marker modified, and declining left the
marker modified.  Does nothing when PROJECT-DIR is not a git
repository, git is missing, or nothing changed."
  (let ((default-directory (file-name-as-directory project-dir)))
    (when (and (file-directory-p (expand-file-name ".git"))
               (org-scribe--git "add" "-A")
               ;; Exit status 1: something is staged.
               (not (org-scribe--git "diff" "--cached" "--quiet")))
      (org-scribe--git "commit" "-m"
                       (if (file-exists-p (expand-file-name "plan.org"))
                           (format "Add writing plan: %s" title)
                         (format "Record planner choice: %s" title))))))

(defun org-scribe--copy-templates (template-dir project-dir variables)
  "Copy and process templates from TEMPLATE-DIR to PROJECT-DIR.
VARIABLES is an alist of (NAME . VALUE) pairs for substitution.
Files ending in `.fragment' are inputs to other templates, not project
files, and are never copied."
  (dolist (file (directory-files-recursively template-dir ".*"))
    (unless (string-suffix-p ".fragment" file)
      (let* ((relative-path (file-relative-name file template-dir))
             ;; Remove .template extension if present
             (output-path (expand-file-name
                           (replace-regexp-in-string "\\.template\\'" "" relative-path)
                           project-dir)))

        ;; Create parent directory if needed
        (make-directory (file-name-directory output-path) t)

        ;; Process template or copy file
        (if (string-suffix-p ".template" file)
            (org-scribe--process-template file output-path variables)
          (copy-file file output-path))))))

(defun org-scribe--foundations-fragment (method language base-dir variables)
  "Return the processed Foundations text for METHOD in LANGUAGE.
LANGUAGE is \\='en or \\='es.  BASE-DIR is the base template set's
directory; VARIABLES is passed to `org-scribe--process-template-string'.

METHOD is a key of `org-scribe--methods'.  When its :overlay is nil
(the case for \\='sistema), the fragment is `foundations.fragment' in
BASE-DIR.  Otherwise it is the one in
`org-scribe-templates/methods/<overlay>/<language>/'.  Either way it is
the body of the notebook's Foundations part, substituted into the
notebook template through the FOUNDATIONS variable; only the Foundations
part varies between methods.

This is a separate function rather than a parameter on
`org-scribe--copy-templates' because the base copy is shared with
`org-scribe-create-short-story-project', which never has a method or
a fragment."
  (let* ((overlay (plist-get (alist-get method org-scribe--methods) :overlay))
         (dir (if overlay
                  (expand-file-name
                   (format "../org-scribe-templates/methods/%s/%s"
                           overlay (plist-get (org-scribe-lang-pack language) :code))
                   org-scribe-project-package-directory)
                base-dir))
         (file (expand-file-name "foundations.fragment" dir)))
    (unless (file-exists-p file)
      (user-error "%s" (org-scribe-msg 'error-template-not-found file)))
    (org-scribe--process-template-string
     (with-temp-buffer
       (insert-file-contents file)
       (buffer-string))
     variables)))

(defun org-scribe--process-template-string (text variables)
  "Return TEXT with each `${NAME}' replaced from the VARIABLES alist."
  ;; One pass over the text, so a value that itself contains a `${...}'
  ;; token (a title) is never rewritten, whatever the variable order.
  (replace-regexp-in-string
   "\\${\\([^}]+\\)}"
   (lambda (match)
     (let ((var (assoc (match-string 1 match) variables)))
       (if var (cdr var) match)))
   text t t))

(defun org-scribe--process-template (template-file output-file variables)
  "Process TEMPLATE-FILE replacing variables, save to OUTPUT-FILE.
VARIABLES is an alist of (NAME . VALUE) pairs for substitution."
  (let ((text (org-scribe--process-template-string
               (with-temp-buffer
                 (insert-file-contents template-file)
                 (buffer-string))
               variables)))
    (with-temp-buffer
      (insert text)
      (write-region (point-min) (point-max) output-file))))

;;; Template Insertion

(defconst org-scribe--scene-property-keys
  '(pov characters plot plot-point timeline location description summary
    scene-motivation conflict-source gap what-is-at-stake world-problem
    emotion tension-level outcome sequel-decision comment)
  "Canonical scene property keys, in the order they appear in a scene drawer.

Ordered to follow the method's scene table: whose desire drives the
scene (scene-motivation), what opposes it (conflict-source), the Gap
between what the character expected and what the world gave, the
outcome, and the sequel decision that opens the next scene.  `plot-point'
sits beside `plot' because both classify the scene structurally rather
than describing what happens in it.

There is deliberately no `beat' key.  The drawer used to carry a free-text
`:Beat:' (shipped as \"Opening Image\", a Save the Cat term) beside
`plot-point', which is the method's own structural classifier — linkable,
health-checked and jumpable.  Two overlapping structural classifications
is exactly the ambiguity the glossary exists to remove, and no module ever
read `:Beat:'.  Scenes in projects created before the removal keep the
property; it is inert.")

(defun org-scribe--scene-property-drawer-lines ()
  "Return the scene property drawer lines, localized to the current project."
  (mapconcat (lambda (key) (format ":%s:" (org-scribe-scene-property-name key)))
             org-scribe--scene-property-keys
             "\n"))

(defun org-scribe--heading-tag-suffix (tag)
  "Return \" :TAG:\" when TAG is non-nil, or the empty string when nil.
Shared by `org-scribe-insert-scene' and `org-scribe-insert-chapter' so
both read the current project type's tagging convention from
`org-scribe-project-levels' `:scene-tag' instead of assuming a novel's
own :ignore: unconditionally -- which is what let a short story's
scenes go uncounted by every scene-level function in the package until
the project type each command runs in is actually consulted (a short
story's scenes carry no tag at all, per that table)."
  (if tag (format " :%s:" tag) ""))

(defun org-scribe--goto-insertion-point (level)
  "Move point to where a new heading of LEVEL stars may be inserted.
Returns that position.

The commands used to insert at raw point, which has two failure modes a
writer meets daily: with point in the middle of a paragraph the heading
was glued onto the prose (\"Mara found the*** TODO ...\", not a heading
at all, its drawer inside the sentence), and with point on a chapter
heading the new scene went in *above* it, ending up as the last scene of
the previous chapter.

So, in a buffer with a heading at or above point, climb to the enclosing
heading of LEVEL or higher (the scene when adding a scene, the chapter
when adding a chapter, whichever is nearer) and insert at the end of its
subtree: after the scene point is in, or as the last scene of the chapter
point is on.

From the preamble (where a freshly opened manuscript has point) or from
`noexport' apparatus such as Project Goals, there is nothing to nest
under: a heading inserted there is in no act, or inherits `noexport' and
is dropped from every export and compile while still being counted.  The
new heading then goes after the last narrative heading of LEVEL: after
the last scene written so far, in whichever act holds it (in a fresh
novel that is Act I, since the later acts ship empty).  If the buffer
has none, it goes at the end of the apparatus heading point is in, or at
the start of the current line before the first heading."
  (unless (and (org-scribe--outside-narrative-p)
               (org-scribe--goto-end-of-last-narrative-heading level))
    (if (org-before-first-heading-p)
        (beginning-of-line)
      (org-back-to-heading t)
      (while (and (> (org-current-level) level)
                  (org-up-heading-safe)))
      (org-scribe--goto-after-subtree)))
  (point))

(defun org-scribe--goto-after-subtree ()
  "Move point to the line after the subtree of the heading at point."
  (org-end-of-subtree t)
  (if (eobp)
      (unless (bolp) (insert "\n"))
    (forward-line 1)))

(defun org-scribe--heading-under-noexport-p ()
  "Non-nil when the heading at point or one of its ancestors is `noexport'.
Reads each heading's own tags rather than relying on tag inheritance,
which a writer may have turned off (`org-use-tag-inheritance') or
narrowed (`org-tags-exclude-from-inheritance'); export drops a
`noexport' subtree whatever those settings say, and so must this."
  (save-excursion
    (org-back-to-heading t)
    (let ((found nil))
      (while (progn (when (member "noexport" (org-get-tags nil t))
                      (setq found t))
                    (and (not found) (org-up-heading-safe))))
      found)))

(defun org-scribe--outside-narrative-p ()
  "Non-nil when point is in the preamble or inside `noexport' apparatus."
  (or (org-before-first-heading-p)
      (org-scribe--heading-under-noexport-p)))

(defun org-scribe--goto-end-of-last-narrative-heading (level)
  "Move point past the last narrative heading of LEVEL; return non-nil if found.
Narrative means not under `noexport'.  Return nil, leaving point alone,
when the buffer has no such heading."
  (let ((found nil))
    (save-excursion
      (goto-char (point-max))
      (while (and (not found)
                  (re-search-backward (format "^\\*\\{%d\\}[ \t]" level) nil t))
        (unless (org-scribe--heading-under-noexport-p)
          (setq found (point)))))
    (when found
      (goto-char found)
      (org-scribe--goto-after-subtree)
      t)))

(defun org-scribe--chapterless-container-at-point ()
  "Return the title of the act point is on when it has no chapter yet.
That is: the nearest heading at or above point is above the project's
chapter level (an act), is not apparatus (`noexport'), and has no
chapter-level heading anywhere in its subtree.  Return nil otherwise,
including in a project type with no chapter level (a short story) and
before the first heading."
  (when-let* ((chapter-level (org-scribe-chapter-level))
              ((not (org-before-first-heading-p))))
    (save-excursion
      (org-back-to-heading t)
      (when (and (< (org-current-level) chapter-level)
                 (not (member "noexport" (org-get-tags))))
        (let ((title (org-get-heading t t t t))
              (end (save-excursion (org-end-of-subtree t t))))
          (unless (re-search-forward (format "^\\*\\{%d\\}[ \t]" chapter-level)
                                     end t)
            title))))))

;;;###autoload
(defun org-scribe-insert-scene (scene-name)
  "Insert a scene template with SCENE-NAME after the scene point is in.
Point may be anywhere: in the middle of prose, on a scene or chapter
heading, in a scene's drawer.  The scene goes at the end of the scene
point is in, or at the end of the chapter point is on (see
`org-scribe--goto-insertion-point'); before the first heading, or in
`noexport' apparatus, it goes after the last scene written so far.  With point on an act, the scene goes at
the end of the act's last chapter; an act with no chapter yet gets one
first (its name is asked for), since a scene directly under an act
compiled as part of the previous act's last chapter.
The template includes a TODO heading at the current project's scene
level (see `org-scribe-scene-level'), tagged per
`org-scribe-project-levels' `:scene-tag' when that project type tags its
scenes, and a property drawer for scene metadata (PoV, Characters, Plot,
Timeline, Location, Description, Summary, Scene-motivation,
Conflict-source, What-is-at-stake, Emotion, and Comment).
If SCENE-NAME is empty, defaults to \"New scene\"."
  (interactive (list (read-string (org-scribe-msg 'scene-name-prompt))))

  ;; Validate we're in org-mode
  (unless (derived-mode-p 'org-mode)
    (user-error "%s" (org-scribe-msg 'not-in-org-mode)))

  ;; Use default title if scene-name is empty
  (when (string-empty-p (string-trim scene-name))
    (setq scene-name (org-scribe-msg 'default-scene-name)))

  (if-let* ((act (org-scribe--chapterless-container-at-point)))
      ;; `org-scribe-insert-chapter' adds the chapter at the end of the
      ;; act with a first scene, and leaves point on that scene's :PoV:
      ;; line; the scene only needs the name the writer typed.
      (progn
        (org-scribe-insert-chapter
         (read-string (org-scribe-msg 'prompt-chapter-for-empty-act act)))
        (save-excursion
          (org-back-to-heading t)
          (org-edit-headline scene-name)))
    (org-scribe--insert-scene-template scene-name)))

(defun org-scribe--scene-template (scene-name)
  "Return the text of a new scene named SCENE-NAME: heading and property drawer.
The heading is a TODO at the project's scene level, tagged as the project
type does, and the drawer holds the empty scene properties in the
project's language.  Shared by `org-scribe-insert-scene' and the visual
companion's `create-scene', which inserts it at an exact place."
  (let* ((levels (org-scribe-project-levels))
         (stars (make-string (plist-get levels :scene) ?*))
         (tag (org-scribe--heading-tag-suffix (plist-get levels :scene-tag))))
    (format "%s TODO %s%s
:PROPERTIES:
:WORDCOUNT: 0
:WORD-OBJECTIVE: 500
%s
:END:

{{{scene-break}}}
" stars scene-name tag (org-scribe--scene-property-drawer-lines))))

(defun org-scribe--insert-scene-template (scene-name)
  "Insert the scene template named SCENE-NAME; see `org-scribe-insert-scene'."
  (let* ((levels (org-scribe-project-levels))
         (template (org-scribe--scene-template scene-name))
         (start-pos (org-scribe--goto-insertion-point
                     (plist-get levels :scene))))

    ;; Insert template
    (insert template)
    ;; A stale refile cache would not notice this new heading; see
    ;; `org-scribe--refile-invalidate-cache' -- a no-op if the writer never
    ;; turned `org-refile-use-cache' on.
    (org-scribe--refile-invalidate-cache)

    ;; Position cursor at first property value (after :PoV:).  The
    ;; heading occupies exactly one line regardless of its star count or
    ;; tag, so this offset holds for every project type.
    (goto-char start-pos)
    (forward-line 4)  ; Move to PoV line
    (end-of-line)     ; Move to end of line (after :PoV:)
    ;; Org needs whitespace between a property name and its value: a name
    ;; typed straight after ":PoV:" makes ":PoV:Xoán", which is not a
    ;; property at all, so the PoV was silently lost.
    (insert " ")
    ;; The new heading may sit inside a folded chapter or act.
    (org-fold-show-context)))

;;; Changing the project's word target

(defun org-scribe--objective-markers-values (markers)
  "Return the WORD-OBJECTIVE of each of MARKERS as an integer, in order."
  (mapcar (lambda (m)
            (string-to-number
             (org-with-point-at m (org-entry-get nil "WORD-OBJECTIVE"))))
          markers))

(defun org-scribe--release-markers (markers)
  "Detach every marker in MARKERS."
  (dolist (m markers) (set-marker m nil)))

(defun org-scribe--word-target-weights (split values)
  "Return the weights SPLIT asks for: VALUES (`keep') or the proportions (`reset').
`keep' with every value 0 falls back to equal weights."
  (pcase split
    ('reset
     (unless (= (length values) (length org-scribe-word-target-proportions))
       (user-error "%s" (org-scribe-msg 'error-word-target-reset-unavailable
                                         (length values)
                                         (length org-scribe-word-target-proportions))))
     org-scribe-word-target-proportions)
    (_ (if (zerop (apply #'+ values)) (mapcar (lambda (_) 1) values) values))))

(defun org-scribe--read-word-target-change (root markers)
  "Ask for a new total and how to split it, for ROOT's project.
MARKERS are the outermost objective headings.  Return (TOTAL SPLIT)."
  (let* ((values (org-scribe--objective-markers-values markers))
         (default (or (org-scribe-project-word-target root)
                      (and values (> (apply #'+ values) 0) (apply #'+ values))
                      (org-scribe-default-word-target-for (org-scribe-project-type))))
         (minimum (max 1 (length values)))
         (total nil))
    (while (not (and (integerp total) (>= total minimum)))
      (setq total (read-number (org-scribe-msg 'word-target-prompt) default)))
    (list total
          (when markers
            (let* ((keep (org-scribe-msg 'word-target-split-keep))
                   (reset (org-scribe-msg 'word-target-split-reset))
                   (choices (append (list keep)
                                    (when (= (length values)
                                             (length org-scribe-word-target-proportions))
                                      (list reset)))))
              (if (cdr choices)
                  (if (equal (completing-read (org-scribe-msg 'word-target-split-prompt)
                                              choices nil t nil nil keep)
                             reset)
                      'reset
                    'keep)
                'keep))))))

(defun org-scribe--update-total-goal-line (total)
  "Rewrite the Project Goals total-words line in this buffer to TOTAL.
The line is found by the project language's `goals-total-label'; only
the text after the \"::\" changes.  Return 1 when a line was rewritten, else 0."
  (let ((label (let ((org-scribe-message-language (org-scribe-project-language)))
                 (org-scribe-msg 'goals-total-label))))
    (org-with-wide-buffer
     (goto-char (point-min))
     (if (re-search-forward
          (concat "^\\([ \t]*- " (regexp-quote label) " :: \\).*$") nil t)
         (progn (replace-match (concat "\\1" (org-scribe--format-word-count total))
                               t)
                1)
       0))))

(defun org-scribe--update-total-properties (total)
  "Set TOTAL in every existing target word count property of this buffer.
Only a property that is already there is touched.  Return how many were set."
  (let ((count 0))
    (org-with-wide-buffer
     (org-map-entries
      (lambda ()
        (dolist (name '("TARGET-WORDCOUNT" "TARGET_WORDCOUNT"))
          (when (org-entry-get nil name)
            (org-entry-put nil name (number-to-string total))
            (cl-incf count))))
      nil 'file))
    count))

(defun org-scribe--save-unless-modified-before (buffer was-modified)
  "Save BUFFER if it was not WAS-MODIFIED before; else say it was left modified."
  (with-current-buffer buffer
    (if was-modified
        (message "%s" (org-scribe-msg 'word-target-unsaved
                                      (file-name-nondirectory (buffer-file-name))))
      (save-buffer))))

(defvar org-scribe--word-target-offer-planner t
  "Non-nil lets `org-scribe-set-word-target' offer the plan the new total.
The planner binds it to nil around the call it makes itself, so the two
offers cannot ask each other in turn.")

(declare-function org-scribe-planner-set-total "planning/org-scribe-planner")
(defvar org-scribe-planner--current-plan)
(defvar org-scribe-planner--current-plan-file)
(declare-function org-scribe-plan-total-words "planning/org-scribe-planner")
(declare-function org-scribe-planner--plan-file-in-current-project-p "planning/org-scribe-planner")

(defun org-scribe--offer-plan-total (total)
  "Offer to change the active plan's total to TOTAL, when it differs.
Asked only when the planner is already loaded, the project's gate is
`yes' and the active plan belongs to this project: the planner is never
loaded just to ask."
  (when (and org-scribe--word-target-offer-planner
             (featurep 'org-scribe-planner)
             (eq (org-scribe-planner-gate) 'yes)
             org-scribe-planner--current-plan
             org-scribe-planner--current-plan-file
             (org-scribe-planner--plan-file-in-current-project-p
              org-scribe-planner--current-plan-file)
             (/= (org-scribe-plan-total-words org-scribe-planner--current-plan) total)
             (y-or-n-p (org-scribe-msg
                        'word-target-offer-plan
                        (round (org-scribe-plan-total-words org-scribe-planner--current-plan))
                        total)))
    (org-scribe-planner-set-total total)))

;;;###autoload
(defun org-scribe-set-word-target (total &optional split confirm)
  "Set the project's total word target to TOTAL and rescale its parts.
Records TOTAL in the project marker file, rewrites the outermost
WORD-OBJECTIVE headings (a novel's acts, a short story's scenes), the
Project Goals total line, and any target word count property in the
manuscript.  Chapter and scene objectives, IDs and the
writing plan are never touched.

SPLIT is `keep' (the current proportions) or `reset' (the proportions
in `org-scribe-word-target-proportions', only when the part count
matches); nil means `keep'.  Unless CONFIRM is non-nil, the change is
previewed and confirmed first.  A file whose buffer had unsaved changes
is left modified rather than saved."
  (interactive
   (let* ((root (org-scribe--require-project))
          (markers (org-scribe--outermost-objective-markers
                    (plist-get (org-scribe-project-structure) :manuscript-file))))
     (unwind-protect (org-scribe--read-word-target-change root markers)
       (org-scribe--release-markers markers))))
  (let* ((root (org-scribe--require-project))
         (file (plist-get (org-scribe-project-structure) :manuscript-file))
         (markers (org-scribe--outermost-objective-markers file)))
    (unwind-protect
        (let* ((old (org-scribe--objective-markers-values markers))
               (new (when markers
                      (org-scribe--split-word-target
                       total (org-scribe--word-target-weights split old))))
               (names (mapcar (lambda (m)
                                (org-with-point-at m (org-get-heading t t t t)))
                              markers)))
          (unless (and (integerp total) (>= total (max 1 (length old))))
            (user-error "%s" (org-scribe-msg 'error-word-target-invalid)))
          (unless (or confirm
                      (y-or-n-p
                       (org-scribe-msg
                        'word-target-preview
                        (org-scribe--format-word-count total)
                        (string-join
                         (cl-mapcar (lambda (name o n)
                                      (org-scribe-msg 'word-target-preview-line name o n))
                                    names old new)
                         "\n"))))
            (user-error "%s" (org-scribe-msg 'word-target-cancelled)))
          (org-scribe--set-project-word-target root total)
          (let ((goal-lines 0) (properties 0))
            ;; objectives, goals line and properties, one save
            (when (and file (file-exists-p file))
              (let ((buffer (find-file-noselect file)))
                (with-current-buffer buffer
                  (let ((was-modified (buffer-modified-p)))
                    (cl-mapc (lambda (m n)
                               (org-with-point-at m
                                 (org-entry-put nil "WORD-OBJECTIVE" (number-to-string n))))
                             markers new)
                    (cl-incf goal-lines (org-scribe--update-total-goal-line total))
                    (cl-incf properties (org-scribe--update-total-properties total))
                    (org-scribe--save-unless-modified-before buffer was-modified)))))
            (message "%s" (org-scribe-msg 'word-target-applied
                                           (org-scribe--format-word-count total)
                                           (length markers) goal-lines properties))
            (org-scribe--offer-plan-total total)))
      (org-scribe--release-markers markers))))

;;; Changing the title

(defun org-scribe--validate-title-text (title)
  "Return an error string if TITLE cannot be a project title, else nil.
Looser than `org-scribe--validate-project-title': once the title is no
longer the directory name, only what would corrupt a one-line keyword
matters, so \"Why Not?\" and \"Part I: Arrival\" are fine."
  (cond
   ((string-empty-p (string-trim title))
    (org-scribe-msg 'error-title-empty))
   ((string-match-p "[\n\r]" title)
    (org-scribe-msg 'error-title-newline))))

(defun org-scribe--title-rewrite-value (value old new)
  "Return VALUE with the leading title OLD replaced by NEW, or nil.
VALUE is the text of a #+TITLE: line.  It matches when it is OLD
exactly, or OLD followed by \" - \" and a suffix, which is kept as is.
Anything else, including a longer title that merely starts with OLD,
returns nil."
  (cond
   ((string= value old) new)
   ((string-prefix-p (concat old " - ") value)
    (concat new (substring value (length old))))))

(defun org-scribe--title-candidate-files (root)
  "Return the absolute Org files of ROOT's project that may carry a title.
Every Org file in the project and its immediate subdirectories, except
the compile output directory, whose files are outputs and not the
writer's."
  (let ((skip (file-name-as-directory
               (expand-file-name org-scribe-compile-output-directory root))))
    (seq-remove
     (lambda (file) (string-prefix-p skip file))
     (mapcar (lambda (relative) (expand-file-name relative root))
             (org-scribe--project-file-candidates root)))))

(defun org-scribe--title-file-status (file olds new)
  "Return how FILE's #+TITLE: line stands against a change to NEW.
OLDS are the titles it may start with.  The result is `none' (no
#+TITLE: line, or a file that does not exist), `current' (already
NEW), `rewrite' or `alone' (a title that starts with none of OLDS, left
for the writer)."
  (let ((value (org-scribe--file-title-keyword file)))
    (cond
     ((null value) 'none)
     (t (let ((result (seq-some (lambda (old)
                                  (org-scribe--title-rewrite-value value old new))
                                olds)))
          (cond ((null result) 'alone)
                ((string= result value) 'current)
                (t 'rewrite)))))))

(defun org-scribe--title-set-in-buffer (value-fn)
  "Replace the first #+TITLE: value of this buffer with (VALUE-FN OLD-VALUE).
Return non-nil when a line was changed."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((case-fold-search t))
     (when (re-search-forward "^#\\+TITLE:[ \t]*\\(.*?\\)[ \t]*$" nil t)
       (let ((new-value (funcall value-fn (match-string-no-properties 1))))
         (when (and new-value (not (string= new-value (match-string-no-properties 1))))
           (replace-match (concat "#+TITLE: " new-value) t t)
           t))))))

(defun org-scribe--title-write-file (file value-fn)
  "Apply `org-scribe--title-set-in-buffer' with VALUE-FN to FILE.
The file is saved unless its buffer had unsaved edits, in which case it
is left modified and the writer is told.  Return non-nil when changed."
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (let ((was-modified (buffer-modified-p)))
        (when (org-scribe--title-set-in-buffer value-fn)
          (if was-modified
              (message "%s" (org-scribe-msg 'title-unsaved
                                             (file-name-nondirectory file)))
            (save-buffer))
          t)))))

(defun org-scribe--title-plan-heading-rewrite (plan-file old new)
  "Rename the first level-1 heading of PLAN-FILE from OLD to NEW.
Only when it reads exactly OLD.  Return non-nil when changed."
  (let ((buffer (find-file-noselect plan-file)))
    (with-current-buffer buffer
      (let ((was-modified (buffer-modified-p)))
        (org-with-wide-buffer
         (goto-char (point-min))
         (when (and (re-search-forward "^\\* \\(.*?\\)[ \t]*$" nil t)
                    (string= (match-string-no-properties 1) old))
           (replace-match (concat "* " new) t t)
           (if was-modified
               (message "%s" (org-scribe-msg 'title-unsaved
                                              (file-name-nondirectory plan-file)))
             (save-buffer))
           t))))))

(declare-function org-scribe-planner-set-title "planning/org-scribe-planner")
(declare-function org-scribe-plan-title "planning/org-scribe-planner")

(defun org-scribe--offer-plan-title (root old new)
  "Carry the title change OLD -> NEW to ROOT's writing plan.
A plan the planner has loaded for this project is offered the new title
and renamed through the planner, whose in-memory copy would otherwise
overwrite an edited file.  A plan file the planner has not loaded gets
its heading rewritten directly.  The planner is never loaded just for
this, and a project whose planner gate is `no' is left alone.  A plan
whose title is not OLD is the writer's own and is left alone too."
  (unless (eq (org-scribe-planner-gate root) 'no)
    (if (and (featurep 'org-scribe-planner)
             org-scribe-planner--current-plan
             org-scribe-planner--current-plan-file
             (file-in-directory-p org-scribe-planner--current-plan-file root))
        (when (and (string= (org-scribe-plan-title org-scribe-planner--current-plan) old)
                   (y-or-n-p (org-scribe-msg 'title-offer-plan old new)))
          (org-scribe-planner-set-title new))
      (let ((plan-file (expand-file-name "plan.org" root)))
        (when (file-exists-p plan-file)
          (org-scribe--title-plan-heading-rewrite plan-file old new))))))

;;;###autoload
(defun org-scribe-set-title (new-title &optional confirm)
  "Change the project's title to NEW-TITLE everywhere it is recorded.
Records it in the project marker file, sets the manuscript's #+TITLE:
to it, and replaces the old title at the start of the #+TITLE: line of
every other Org file of the project, keeping what follows (\"Title -
Characters\" becomes \"New - Characters\").  A file whose #+TITLE: does
not start with the old title is left alone and named in the result.
The writing plan is renamed too (offered when the planner has it
loaded).  The project directory, the git history, IDs and word counts
are never touched.

Unless CONFIRM is non-nil, the change is previewed and confirmed first.
A file whose buffer had unsaved changes is left modified rather than
saved."
  (interactive
   (let ((root (org-scribe--require-project)))
     (list (read-string (org-scribe-msg 'title-prompt) (org-scribe-project-title root)))))
  (let* ((root (org-scribe--require-project))
         (new (string-trim new-title))
         (known (org-scribe--project-title-candidates root))
         (old (org-scribe-project-title root))
         (olds (or known (list old)))
         (manuscript (org-scribe--resolve-manuscript root 'unknown))
         (files (seq-remove (lambda (f) (and manuscript (file-equal-p f manuscript)))
                            (org-scribe--title-candidate-files root)))
         (statuses (mapcar (lambda (f) (cons f (org-scribe--title-file-status f olds new)))
                           files))
         (to-rewrite (mapcar #'car (seq-filter (lambda (p) (eq (cdr p) 'rewrite)) statuses)))
         (alone (mapcar #'car (seq-filter (lambda (p) (eq (cdr p) 'alone)) statuses))))
    (when-let* ((problem (org-scribe--validate-title-text new)))
      (user-error "%s" problem))
    (when (and (equal known (list new)) (null to-rewrite))
      (user-error "%s" (org-scribe-msg 'title-unchanged)))
    (unless (or confirm
                (y-or-n-p (org-scribe-msg 'title-preview old new
                                           (1+ (length to-rewrite))
                                           (length alone))))
      (user-error "%s" (org-scribe-msg 'title-cancelled)))
    (org-scribe--project-marker-set root "Title" new)
    (let ((changed 0))
      (when (and manuscript
                 (org-scribe--title-write-file manuscript (lambda (_) new)))
        (cl-incf changed))
      (dolist (file to-rewrite)
        (when (org-scribe--title-write-file
               file
               (lambda (value)
                 (seq-some (lambda (o) (org-scribe--title-rewrite-value value o new))
                           olds)))
          (cl-incf changed)))
      (message "%s" (org-scribe-msg 'title-applied new changed))
      (when alone
        (message "%s" (org-scribe-msg
                       'title-left-alone
                       (mapconcat (lambda (f) (file-relative-name f root)) alone ", "))))
      (dolist (old-title olds)
        (unless (string= old-title new)
          (org-scribe--offer-plan-title root old-title new))))))

;;;###autoload
(defun org-scribe-insert-chapter (chapter-name)
  "Insert a chapter template with CHAPTER-NAME after the chapter point is in.
The chapter goes at the end of the chapter point is in, scene or not
\(see `org-scribe--goto-insertion-point'), never in the middle of prose.
The template includes a TODO heading at the current project's chapter
level, tagged per `org-scribe-project-levels' when that project type
tags its chapters, a property drawer with WORDCOUNT field initialized to
0, and an empty first scene.
If CHAPTER-NAME is empty, defaults to \"New chapter\".

Refuses with a `user-error' in a project type with no chapter level at
all (a short story, per `org-scribe-chapter-level' returning nil) --
there is no level to insert a chapter at, and inserting one anyway would
collide with the project's own scene level."
  (interactive (list (read-string (org-scribe-msg 'chapter-name-prompt))))

  ;; Validate we're in org-mode
  (unless (derived-mode-p 'org-mode)
    (user-error "%s" (org-scribe-msg 'not-in-org-mode)))

  (unless (org-scribe-chapter-level)
    (user-error "%s" (org-scribe-msg 'insert-chapter-no-chapters)))

  ;; Use default title if chapter-name is empty
  (when (string-empty-p (string-trim chapter-name))
    (setq chapter-name (org-scribe-msg 'default-chapter-name)))

  ;; Define and insert combined chapter + first scene template
  (let* ((levels (org-scribe-project-levels))
         (tag (org-scribe--heading-tag-suffix (plist-get levels :scene-tag)))
         (chapter-stars (make-string (plist-get levels :chapter) ?*))
         (scene-stars (make-string (plist-get levels :scene) ?*))
         (template (format "%s TODO %s%s
:PROPERTIES:
:WORDCOUNT: 0
:WORD-OBJECTIVE: 3000
:END:

%s TODO %s%s
:PROPERTIES:
:WORDCOUNT: 0
:WORD-OBJECTIVE: 500
%s
:END:

{{{scene-break}}}
" chapter-stars chapter-name tag scene-stars (org-scribe-msg 'default-scene-name) tag
  (org-scribe--scene-property-drawer-lines)))
         (start-pos (org-scribe--goto-insertion-point
                     (plist-get levels :chapter))))

    ;; Insert template
    (insert template)
    ;; A stale refile cache would not notice this new heading; see
    ;; `org-scribe--refile-invalidate-cache' -- a no-op if the writer never
    ;; turned `org-refile-use-cache' on.
    (org-scribe--refile-invalidate-cache)

    ;; Position cursor at first scene's :PoV: property (line 10 from
    ;; start).  Like the scene heading above, a heading's star count and
    ;; tag do not change how many lines it occupies, so this offset holds
    ;; regardless of project type.
    (goto-char start-pos)
    (forward-line 10)
    (end-of-line)
    (insert " ")  ; see `org-scribe--insert-scene-template'
    (org-fold-show-context)))

;;; Project Navigation

(defconst org-scribe--known-project-files
  (let (files)
    (dolist (lang (org-scribe-languages))
      (dolist (row (plist-get (org-scribe-lang-pack lang) :files))
        (unless (member (cdr row) files) (push (cdr row) files))))
    (dolist (extra '("plan.org"))
      (unless (member extra files) (push extra files)))
    (nreverse files))
  "Fallback completion candidates for `org-scribe-open-project-file'.
Used when no project root can be detected.  Derived, once at load
time, from the union of every registered language pack's `:files'
values (see `lang/org-scribe-lang.el'), plus \"plan.org\" explicitly --
already covered by the union for the two shipped packs, added directly
so it cannot silently drop out if a future pack ever omits the key.  Inside a project the candidates
are scanned from disk instead, so a file created after this constant
was computed is still offered; a language pack registered after this
file loaded is not reflected here, but real projects in that language
resolve correctly regardless, since `org-scribe-project-structure'
reads the live registry rather than this fallback list.")

(defun org-scribe--open-file-project-root ()
  "Return the project root for file navigation, or nil."
  (if (fboundp 'org-scribe-project-root)
      (org-scribe-project-root)
    (or (when-let* ((project (project-current)))
          (project-root project))
        (locate-dominating-file default-directory ".org-scribe-project"))))

(defun org-scribe--project-file-candidates (&optional root)
  "Return relative names of the Org files in ROOT for completion.
A project is flat, so only ROOT itself is scanned (hidden files and the
compile output directory are not offered).  Falls back to
`org-scribe--known-project-files' when ROOT is nil or holds no Org files."
  (let ((found
         (when (and root (file-directory-p root))
           (sort (directory-files root nil "\\`[^.].*\\.org\\'" t)
                 #'string<))))
    (or found org-scribe--known-project-files)))

(defun org-scribe--open-file-default (root candidates)
  "Return the file `org-scribe-open-project-file' offers by default.
The manuscript, or the design notebook when the current buffer already
visits the manuscript, provided it is one of CANDIDATES (relative to
ROOT); nil otherwise."
  (when root
    (let* ((structure (org-scribe-project-structure))
           (manuscript (plist-get structure :manuscript-file))
           (file (if (and manuscript buffer-file-name
                          (file-equal-p manuscript buffer-file-name))
                     (plist-get structure :design-file)
                   manuscript)))
      (when file
        (car (member (file-relative-name file root) candidates))))))

;;;###autoload
(defun org-scribe-open-project-file (filename)
  "Quickly open a file in the current writing project (novel or short story).
FILENAME should be relative to project root (e.g., \"design.org\").
Completion offers the Org files actually present in the project, and
only those; the default is the manuscript, or the design notebook from
the manuscript.  An empty answer used to open the project root in Dired."
  (interactive
   (let* ((root (org-scribe--open-file-project-root))
          (candidates (org-scribe--project-file-candidates root))
          (default (org-scribe--open-file-default root candidates)))
     (list (completing-read
            (format-prompt (org-scribe-msg 'file-open-prompt) default)
            candidates nil t nil nil default))))
  (when (string-empty-p filename)
    (user-error "%s" (org-scribe-msg 'error-no-file-chosen)))
  (let ((project-root (org-scribe--open-file-project-root)))
    (if project-root
        (let ((full-path (expand-file-name filename project-root)))
          (if (file-exists-p full-path)
              (find-file full-path)
            (when (yes-or-no-p (org-scribe-msg 'file-not-found filename))
              (find-file full-path))))
      (message "%s" (org-scribe-msg 'not-in-novel-project)))))

;;; Utility Functions

;;;###autoload
(defun org-scribe-edit-templates ()
  "Open the package's template directory for editing.
Edits apply to every project created afterwards; the directory is the
package's own copy, not a setting."
  (interactive)
  (let ((dir (expand-file-name "../org-scribe-templates"
                               org-scribe-project-package-directory)))
    (if (file-directory-p dir)
        (dired dir)
      (user-error "%s" (org-scribe-msg 'error-template-not-found dir)))))

;;;###autoload
(defun org-scribe-register-projects (directory)
  "Register all existing novel projects under DIRECTORY with project.el.
This is useful for adding novels created before project.el integration."
  (interactive "DBase directory containing novel projects: ")
  (let ((count (project-remember-projects-under directory t)))
    (message "%s" (org-scribe-msg 'msg-projects-registered count directory))))

;;; Backwards Compatibility Aliases

;; Provide old function names for backwards compatibility
;;;###autoload
(defalias 'org-scribe-project-create-novel-project #'org-scribe-create-novel-project
  "Deprecated: Use `org-scribe-create-novel-project' instead.")
(make-obsolete 'org-scribe-project-create-novel-project 'org-scribe-create-novel-project "0.2.0")

;;;###autoload
(defalias 'org-scribe-create-project #'org-scribe-create-novel-project
  "Deprecated: Use `org-scribe-create-novel-project' instead.")
(make-obsolete 'org-scribe-create-project 'org-scribe-create-novel-project "0.2.1")

;;;###autoload
(defalias 'org-scribe-project-insert-scene #'org-scribe-insert-scene
  "Deprecated: Use `org-scribe-insert-scene' instead.")
(make-obsolete 'org-scribe-project-insert-scene 'org-scribe-insert-scene "0.2.0")

;;;###autoload
(defalias 'org-scribe-project-insert-chapter #'org-scribe-insert-chapter
  "Deprecated: Use `org-scribe-insert-chapter' instead.")
(make-obsolete 'org-scribe-project-insert-chapter 'org-scribe-insert-chapter "0.2.0")

;;;###autoload
(defalias 'org-scribe-project-open-novel-file #'org-scribe-open-project-file
  "Deprecated: Use `org-scribe-open-project-file' instead.")
(make-obsolete 'org-scribe-project-open-novel-file 'org-scribe-open-project-file "0.2.0")

;;;###autoload
(defalias 'org-scribe-project-edit-novel-templates #'org-scribe-edit-templates
  "Deprecated: Use `org-scribe-edit-templates' instead.")
(make-obsolete 'org-scribe-project-edit-novel-templates 'org-scribe-edit-templates "0.2.0")

;;;###autoload
(defalias 'org-scribe-project-register-existing-projects #'org-scribe-register-projects
  "Deprecated: Use `org-scribe-register-projects' instead.")
(make-obsolete 'org-scribe-project-register-existing-projects 'org-scribe-register-projects "0.2.0")

;; Provide old feature name for old configs
(provide 'org-scribe-project)

;;; org-scribe-project.el ends here
