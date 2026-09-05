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

;;; Configuration

(defvar org-scribe-project-package-directory
  (file-name-directory
   (or load-file-name
       (buffer-file-name)))
  "Directory where this file is located.")

(defcustom org-scribe-template-directory
  (expand-file-name "../org-scribe-templates/novel-en" org-scribe-project-package-directory)
  "Directory containing novel project templates.
By default, uses the novel-en (English) templates. Change to
'novel-es' for Spanish templates, or provide a custom path."
  :type 'directory
  :group 'org-scribe)

(defcustom org-scribe-template-language 'en
  "Default language for novel templates.
Can be 'en for English or 'es for Spanish.
This is used to automatically set the template directory."
  :type '(choice (const :tag "English" en)
                 (const :tag "Spanish" es))
  :group 'org-scribe
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; Update template directory when language changes
         (setq org-scribe-template-directory
               (expand-file-name
                (format "../org-scribe-templates/novel-%s"
                        (if (eq value 'es) "es" "en"))
                org-scribe-project-package-directory))))

(defcustom org-scribe-short-story-template-directory
  (expand-file-name "../org-scribe-templates/short-story-en" org-scribe-project-package-directory)
  "Directory containing short story project templates.
By default, uses the short-story-en (English) templates."
  :type 'directory
  :group 'org-scribe)

;;; Project Creation

(defun org-scribe--validate-project-title (title)
  "Validate TITLE for use as a directory name.
Returns nil if valid, otherwise returns an error message."
  (cond
   ((string-empty-p (string-trim title))
    "Title cannot be empty or contain only whitespace")
   ((string-match-p "[/\\]" title)
    "Title cannot contain path separators (/ or \\)")
   ((string-match-p ":" title)
    "Title cannot contain colons (:)")
   ((string-match-p "[*?<>|\"']" title)
    "Title cannot contain special characters (* ? < > | \" ')")
   ((string-match-p "^\\." title)
    "Title cannot start with a dot (.)")
   ((string-match-p "\\.\\." title)
    "Title cannot contain double dots (..)")
   (t nil)))

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
    (let ((default-directory (file-name-as-directory project-dir)))
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
Reads `org-scribe-ispell-dictionaries'; a missing entry and an entry of
nil both mean \"write no dictionary\".  The name is written both as an
ispell dictionary and as a jinx language: the codes coincide (\"es_ES\",
\"en_US\"), since both resolve against the installed hunspell data."
  (alist-get language org-scribe-ispell-dictionaries))

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
  (let ((root (org-scribe-project-root)))
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

;;;###autoload
(defun org-scribe-create-novel-project (base-dir title &optional language)
  "Create a new novel project structure from templates.
BASE-DIR is the parent directory where the project will be created.
TITLE is the name of the novel/project.
LANGUAGE selects which template set to use, either \\='en or \\='es.
When omitted (e.g. non-interactive callers), defaults to
`org-scribe-template-language'.

This function:
1. Validates the title
2. Creates the project directory
3. Initializes a git repository
4. Processes all template files with variable substitution
5. Creates an initial git commit
6. Registers the project with project.el
7. Opens the README.org file"
  (interactive
   (list
    (read-directory-name (org-scribe-msg 'project-creation-base-dir) org-scribe-projects-directory)
    (read-string (org-scribe-msg 'project-creation-novel-title))
    (intern (completing-read (org-scribe-msg 'project-creation-language-prompt)
                              '("en" "es") nil t
                              (if (eq org-scribe-template-language 'es) "es" "en")))))

  (let* ((language (or language org-scribe-template-language))
         (template-dir (expand-file-name
                        (format "../org-scribe-templates/novel-%s"
                                (if (eq language 'es) "es" "en"))
                        org-scribe-project-package-directory)))

    (unless (file-directory-p template-dir)
      (user-error (org-scribe-msg 'error-template-not-found template-dir)))

    ;; Validate title
    (let ((validation-error (org-scribe--validate-project-title title)))
      (when validation-error
        (user-error "%s" validation-error)))

    (let* ((project-dir (expand-file-name title base-dir))
           (variables `(("TITLE" . ,title)
                       ("AUTHOR" . ,(if (boundp 'user-full-name) user-full-name "Author"))
                       ("DATE" . ,(format-time-string "%Y-%m-%d")))))

      ;; Check if project already exists
      (when (file-exists-p project-dir)
        (user-error (org-scribe-msg 'project-already-exists project-dir)))

      ;; Create project directory
      (make-directory project-dir t)

      ;; Create .org-scribe-project marker file for project detection
      (with-temp-file (expand-file-name ".org-scribe-project" project-dir)
        (insert (format "# Writing project: %s\n" title)
                (format "# Created: %s\n" (format-time-string "%Y-%m-%d"))
                (format "# Language: %s\n" language)))

      ;; Pin the spelling dictionary for the whole project, before the
      ;; initial commit so it is versioned like every other created file.
      (org-scribe--write-dir-locals project-dir language)

      ;; Initialize git repository
      (let ((default-directory project-dir))
        (unless (zerop (call-process "git" nil nil nil "init"))
          (warn "Failed to initialize git repository")))

      ;; Process all templates
      (org-scribe--copy-templates template-dir project-dir variables)

      ;; Linking on by default: mint IDs for the template's entities so the
      ;; user never has to run a separate "setup" step (A7).
      (org-scribe--auto-setup-links project-dir)

      ;; Create initial git commit
      (let ((default-directory project-dir))
        (when (zerop (call-process "git" nil nil nil "add" "."))
          (call-process "git" nil nil nil "commit" "-m"
                       (format "Initial commit: %s" title))))

      ;; Register project with project.el
      (project-remember-project (project-current nil project-dir))

      ;; Open README.org
      (find-file (expand-file-name "README.org" project-dir))
      (message (org-scribe-msg 'project-creation-success-novel title project-dir)))))

;;;###autoload
(defun org-scribe-create-short-story-project (base-dir title &optional language)
  "Create a new short story project structure from templates.
BASE-DIR is the parent directory where the project will be created.
TITLE is the name of the short story/project.
LANGUAGE selects which template set to use, either \\='en or \\='es.
When omitted (e.g. non-interactive callers), defaults to
`org-scribe-template-language'.

This function:
1. Validates the title
2. Creates the project directory
3. Initializes a git repository
4. Processes all template files with variable substitution
5. Creates an initial git commit
6. Registers the project with project.el
7. Opens the story file (story.org or cuento.org)"
  (interactive
   (list
    (read-directory-name (org-scribe-msg 'project-creation-base-dir) org-scribe-projects-directory)
    (read-string (org-scribe-msg 'project-creation-short-story-title))
    (intern (completing-read (org-scribe-msg 'project-creation-language-prompt)
                              '("en" "es") nil t
                              (if (eq org-scribe-template-language 'es) "es" "en")))))

  ;; Determine template directory based on language
  (let* ((language (or language org-scribe-template-language))
         (template-dir (expand-file-name
                      (format "../org-scribe-templates/short-story-%s"
                              (if (eq language 'es) "es" "en"))
                      org-scribe-project-package-directory)))

    (unless (file-directory-p template-dir)
      (user-error (org-scribe-msg 'error-template-not-found template-dir)))

    ;; Validate title
    (let ((validation-error (org-scribe--validate-project-title title)))
      (when validation-error
        (user-error "%s" validation-error)))

    (let* ((project-dir (expand-file-name title base-dir))
           (variables `(("TITLE" . ,title)
                       ("AUTHOR" . ,(if (boundp 'user-full-name) user-full-name "Author"))
                       ("DATE" . ,(format-time-string "%Y-%m-%d"))))
           (story-file (if (eq language 'es) "cuento.org" "story.org")))

      ;; Check if project already exists
      (when (file-exists-p project-dir)
        (user-error (org-scribe-msg 'project-already-exists project-dir)))

      ;; Create project directory
      (make-directory project-dir t)

      ;; Create .org-scribe-project marker file for project detection
      (with-temp-file (expand-file-name ".org-scribe-project" project-dir)
        (insert (format "# Writing project: %s\n" title)
                (format "# Type: short-story\n")
                (format "# Created: %s\n" (format-time-string "%Y-%m-%d"))
                (format "# Language: %s\n" language)))

      ;; Pin the spelling dictionary for the whole project, before the
      ;; initial commit so it is versioned like every other created file.
      (org-scribe--write-dir-locals project-dir language)

      ;; Initialize git repository
      (let ((default-directory project-dir))
        (unless (zerop (call-process "git" nil nil nil "init"))
          (warn "Failed to initialize git repository")))

      ;; Process all templates
      (org-scribe--copy-templates template-dir project-dir variables)

      ;; Create initial git commit
      (let ((default-directory project-dir))
        (when (zerop (call-process "git" nil nil nil "add" "."))
          (call-process "git" nil nil nil "commit" "-m"
                       (format "Initial commit: %s" title))))

      ;; Register project with project.el
      (project-remember-project (project-current nil project-dir))

      ;; Open the story file
      (find-file (expand-file-name story-file project-dir))
      (message (org-scribe-msg 'project-creation-success-short-story title project-dir)))))

(defun org-scribe--copy-templates (template-dir project-dir variables)
  "Copy and process templates from TEMPLATE-DIR to PROJECT-DIR.
VARIABLES is an alist of (NAME . VALUE) pairs for substitution."
  (dolist (file (directory-files-recursively template-dir ".*"))
    (let* ((relative-path (file-relative-name file template-dir))
           ;; Remove .template extension if present
           (output-path (expand-file-name
                        (replace-regexp-in-string "\\.template$" "" relative-path)
                        project-dir)))

      ;; Create parent directory if needed
      (make-directory (file-name-directory output-path) t)

      ;; Process template or copy file
      (if (string-match-p "\\.template$" file)
          (org-scribe--process-template file output-path variables)
        (copy-file file output-path)))))

(defun org-scribe--process-template (template-file output-file variables)
  "Process TEMPLATE-FILE replacing variables, save to OUTPUT-FILE.
VARIABLES is an alist of (NAME . VALUE) pairs for substitution."
  (with-temp-buffer
    (insert-file-contents template-file)

    ;; Replace all variables
    (dolist (var variables)
      (goto-char (point-min))
      (while (search-forward (format "${%s}" (car var)) nil t)
        (replace-match (cdr var) t t)))

    ;; Write processed content
    (write-region (point-min) (point-max) output-file)))

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

;;;###autoload
(defun org-scribe-insert-scene (scene-name)
  "Insert a scene template at point with SCENE-NAME.
The template includes a TODO heading with :ignore: tag and property
drawer for scene metadata (PoV, Characters, Plot, Timeline, Location,
Description, Summary, Scene-motivation, Conflict-source, What-is-at-stake,
Emotion, and Comment).
If SCENE-NAME is empty, defaults to \"New scene\"."
  (interactive (list (read-string (org-scribe-msg 'scene-name-prompt))))

  ;; Validate we're in org-mode
  (unless (derived-mode-p 'org-mode)
    (user-error (org-scribe-msg 'not-in-org-mode)))

  ;; Use default title if scene-name is empty
  (when (string-empty-p (string-trim scene-name))
    (setq scene-name (org-scribe-msg 'default-scene-name)))

  ;; Define and insert template
  (let ((template (format "*** TODO %s :ignore:
:PROPERTIES:
%s
:WORD-OBJECTIVE: 500
:END:

{{{scene-break}}}
" scene-name (org-scribe--scene-property-drawer-lines)))
        (start-pos (point)))

    ;; Insert template
    (insert template)

    ;; Position cursor at first property value (after :PoV:)
    (goto-char start-pos)
    (forward-line 2)  ; Move to PoV line
    (end-of-line)))   ; Move to end of line (after :PoV:)

;;;###autoload
(defun org-scribe-insert-chapter (chapter-name)
  "Insert a chapter template at point with CHAPTER-NAME.
The template includes a TODO heading with :ignore: tag, a property
drawer with WORDCOUNT field initialized to 0, and an empty first scene.
If CHAPTER-NAME is empty, defaults to \"New chapter\"."
  (interactive (list (read-string (org-scribe-msg 'chapter-name-prompt))))

  ;; Validate we're in org-mode
  (unless (derived-mode-p 'org-mode)
    (user-error (org-scribe-msg 'not-in-org-mode)))

  ;; Use default title if chapter-name is empty
  (when (string-empty-p (string-trim chapter-name))
    (setq chapter-name (org-scribe-msg 'default-chapter-name)))

  ;; Define and insert combined chapter + first scene template
  (let ((template (format "** TODO %s :ignore:
:PROPERTIES:
:WORD-OBJECTIVE: 5000
:WORDCOUNT: 0
:END:

*** TODO %s :ignore:
:PROPERTIES:
%s
:WORD-OBJECTIVE: 500
:END:

{{{scene-break}}}
" chapter-name (org-scribe-msg 'default-scene-name) (org-scribe--scene-property-drawer-lines)))
        (start-pos (point)))

    ;; Insert template
    (insert template)

    ;; Position cursor at first scene's :PoV: property (line 8 from start)
    (goto-char start-pos)
    (forward-line 8)
    (end-of-line)))

;;; Project Navigation

(defconst org-scribe--known-project-files
  '(;; Common
    "README.org"
    ;; Novel files
    "novel.org" "novela.org"
    "design.org" "diseno.org"
    "revision.org"
    "plan.org"
    "writing-journal.org" "diario-escritura.org"
    "scratchpad.org" "cuaderno-borradores.org"
    "objects/characters.org" "objects/personajes.org"
    "objects/locations.org" "objects/localizaciones.org"
    "objects/objects.org" "objects/objetos.org"
    "objects/plot.org" "objects/trama.org"
    "objects/timeline.org" "objects/cronologia.org"
    "objects/worldbuilding.org"
    "notes/notes.org" "notas/notas.org"
    "notes/research.org" "notas/investigacion.org"
    ;; Short story files
    "story.org" "cuento.org"
    "notes.org" "notas.org")
  "Fallback completion candidates for `org-scribe-open-project-file'.
Used when no project root can be detected.  Inside a project the
candidates are scanned from disk instead, so a file created after
this list was written is still offered.")

(defun org-scribe--open-file-project-root ()
  "Return the project root for file navigation, or nil."
  (if (fboundp 'org-scribe-project-root)
      (org-scribe-project-root)
    (or (when-let ((project (project-current)))
          (project-root project))
        (locate-dominating-file default-directory "README.org"))))

(defun org-scribe--project-file-candidates (&optional root)
  "Return relative paths of Org files under ROOT for completion.
Scans ROOT and its immediate subdirectories (skipping hidden ones),
so files added by newer templates — such as design.org / diseno.org —
are offered without this function knowing their names.  Falls back to
`org-scribe--known-project-files' when ROOT is nil or holds no Org files."
  (let ((found
         (when (and root (file-directory-p root))
           (let (files)
             (dolist (entry (directory-files root nil "\\`[^.]" t))
               (let ((full (expand-file-name entry root)))
                 (cond
                  ((file-directory-p full)
                   (dolist (sub (directory-files full nil "\\.org\\'" t))
                     (push (concat entry "/" sub) files)))
                  ((string-suffix-p ".org" entry)
                   (push entry files)))))
             (sort files #'string<)))))
    (or found org-scribe--known-project-files)))

;;;###autoload
(defun org-scribe-open-project-file (filename)
  "Quickly open a file in the current writing project (novel or short story).
FILENAME should be relative to project root (e.g., \"objects/characters.org\").
Completion offers the Org files actually present in the project."
  (interactive
   (list (completing-read
          "Open file: "
          (org-scribe--project-file-candidates
           (org-scribe--open-file-project-root)))))
  (let ((project-root (org-scribe--open-file-project-root)))
    (if project-root
        (let ((full-path (expand-file-name filename project-root)))
          (if (file-exists-p full-path)
              (find-file full-path)
            (when (yes-or-no-p (org-scribe-msg 'file-not-found filename))
              (find-file full-path))))
      (message (org-scribe-msg 'not-in-novel-project)))))

;;; Utility Functions

;;;###autoload
(defun org-scribe-edit-templates ()
  "Open the novel template directory for editing.
This allows you to customize the templates used for new projects."
  (interactive)
  (if (file-directory-p org-scribe-template-directory)
      (dired org-scribe-template-directory)
    (user-error (org-scribe-msg 'error-template-not-found org-scribe-template-directory))))

;;;###autoload
(defun org-scribe-register-projects (directory)
  "Register all existing novel projects under DIRECTORY with project.el.
This is useful for adding novels created before project.el integration."
  (interactive "DBase directory containing novel projects: ")
  (let ((count (project-remember-projects-under directory t)))
    (message (org-scribe-msg 'msg-projects-registered count directory))))

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
