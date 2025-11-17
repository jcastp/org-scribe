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
Returns nil if valid, otherwise returns a translated error message."
  (require 'org-scribe-i18n)
  (cond
   ((string-empty-p (string-trim title))
    (org-scribe-i18n validation-empty-title))
   ((string-match-p "[/\\]" title)
    (org-scribe-i18n validation-path-separator))
   ((string-match-p ":" title)
    (org-scribe-i18n validation-colon))
   ((string-match-p "[*?<>|\"']" title)
    (org-scribe-i18n validation-special-chars))
   ((string-match-p "^\\." title)
    (org-scribe-i18n validation-starts-dot))
   ((string-match-p "\\.\\." title)
    (org-scribe-i18n validation-double-dots))
   (t nil)))

;;;###autoload
(defun org-scribe-create-novel-project (base-dir title language)
  "Create a new novel project structure from templates.
BASE-DIR is the parent directory where the project will be created.
TITLE is the name of the novel/project.
LANGUAGE is the project language ('en or 'es).

This function:
1. Prompts for language selection
2. Validates the title
3. Creates the project directory
4. Initializes a git repository
5. Processes all template files with variable substitution
6. Creates an initial git commit
7. Registers the project with project.el
8. Opens the README.org file"
  (interactive
   (let* ((lang (intern (completing-read
                         (if (boundp 'org-scribe-template-language)
                             (org-scribe-i18n project-creation-language)
                           "Project language: ")
                         '("en" "es")
                         nil t nil nil
                         (if (boundp 'org-scribe-template-language)
                             (symbol-name org-scribe-template-language)
                           "en")))))
     (list
      (read-directory-name (org-scribe-i18n-string 'project-creation-base-dir lang)
                          "~/writing/")
      (read-string (org-scribe-i18n-string 'project-creation-novel-title lang))
      lang)))

  ;; Temporarily set language for this operation
  (let ((org-scribe-template-language language))
    ;; Determine template directory based on language
    (let ((template-dir (expand-file-name
                        (format "../org-scribe-templates/novel-%s"
                                (if (eq language 'es) "es" "en"))
                        org-scribe-project-package-directory)))

      (unless (file-directory-p template-dir)
        (user-error (org-scribe-i18n-string 'error-template-not-found language)
                   template-dir))

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
          (user-error (org-scribe-i18n-string 'error-project-exists language)
                     project-dir))

        ;; Create project directory
        (make-directory project-dir t)

        ;; Create .org-scribe-project marker file for project detection
        (with-temp-file (expand-file-name ".org-scribe-project" project-dir)
          (insert (format "# %s\n"
                         (format (org-scribe-i18n-string 'project-created-marker language) title))
                  (format "# %s\n"
                         (format (org-scribe-i18n-string 'project-created-date language)
                                (format-time-string "%Y-%m-%d")))
                  (format "# %s\n"
                         (format (org-scribe-i18n-string 'project-language-marker language)
                                (symbol-name language)))))

        ;; Initialize git repository
        (let ((default-directory project-dir))
          (unless (zerop (call-process "git" nil nil nil "init"))
            (warn (org-scribe-i18n-string 'msg-git-init-failed language))))

        ;; Process all templates
        (org-scribe--copy-templates template-dir project-dir variables)

        ;; Create initial git commit
        (let ((default-directory project-dir))
          (when (zerop (call-process "git" nil nil nil "add" "."))
            (call-process "git" nil nil nil "commit" "-m"
                         (format "Initial commit: %s" title))))

        ;; Register project with project.el
        (project-remember-project (project-current nil project-dir))

        ;; Open README.org
        (find-file (expand-file-name "README.org" project-dir))
        (message (org-scribe-i18n-string 'project-creation-success-novel language)
                title project-dir)))))

;;;###autoload
(defun org-scribe-create-short-story-project (base-dir title language)
  "Create a new short story project structure from templates.
BASE-DIR is the parent directory where the project will be created.
TITLE is the name of the short story/project.
LANGUAGE is the project language ('en or 'es).

This function:
1. Prompts for language selection
2. Validates the title
3. Creates the project directory
4. Initializes a git repository
5. Processes all template files with variable substitution
6. Creates an initial git commit
7. Registers the project with project.el
8. Opens the story file (story.org or cuento.org)"
  (interactive
   (let* ((lang (intern (completing-read
                         (if (boundp 'org-scribe-template-language)
                             (org-scribe-i18n project-creation-language)
                           "Project language: ")
                         '("en" "es")
                         nil t nil nil
                         (if (boundp 'org-scribe-template-language)
                             (symbol-name org-scribe-template-language)
                           "en")))))
     (list
      (read-directory-name (org-scribe-i18n-string 'project-creation-base-dir lang)
                          "~/writing/")
      (read-string (org-scribe-i18n-string 'project-creation-story-title lang))
      lang)))

  ;; Temporarily set language for this operation
  (let ((org-scribe-template-language language))
    ;; Determine template directory based on language
    (let ((template-dir (expand-file-name
                        (format "../org-scribe-templates/short-story-%s"
                                (if (eq language 'es) "es" "en"))
                        org-scribe-project-package-directory)))

      (unless (file-directory-p template-dir)
        (user-error (org-scribe-i18n-string 'error-template-not-found language)
                   template-dir))

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
          (user-error (org-scribe-i18n-string 'error-project-exists language)
                     project-dir))

        ;; Create project directory
        (make-directory project-dir t)

        ;; Create .org-scribe-project marker file for project detection
        (with-temp-file (expand-file-name ".org-scribe-project" project-dir)
          (insert (format "# %s\n"
                         (format (org-scribe-i18n-string 'project-created-marker language) title))
                  (format "# %s\n"
                         (format (org-scribe-i18n-string 'project-type-marker language) "short-story"))
                  (format "# %s\n"
                         (format (org-scribe-i18n-string 'project-created-date language)
                                (format-time-string "%Y-%m-%d")))
                  (format "# %s\n"
                         (format (org-scribe-i18n-string 'project-language-marker language)
                                (symbol-name language)))))

        ;; Initialize git repository
        (let ((default-directory project-dir))
          (unless (zerop (call-process "git" nil nil nil "init"))
            (warn (org-scribe-i18n-string 'msg-git-init-failed language))))

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
        (message (org-scribe-i18n-string 'project-creation-success-story language)
                title project-dir)))))

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

;;;###autoload
(defun org-scribe-insert-scene (scene-name)
  "Insert a scene template at point with SCENE-NAME.
The template includes a TODO heading with :ignore: tag and property
drawer for scene metadata (PoV, Characters, Plot, Timeline, Location,
Description, Summary, Scene-motivation, Conflict-source, What-is-at-stake,
Emotion, and Comment).
If SCENE-NAME is empty, defaults to a translated \"New scene\" string."
  (interactive (list (read-string (org-scribe-i18n scene-name-prompt))))

  ;; Validate we're in org-mode
  (unless (derived-mode-p 'org-mode)
    (user-error (org-scribe-i18n error-no-org-mode)))

  ;; Use default title if scene-name is empty
  (when (string-empty-p (string-trim scene-name))
    (setq scene-name (org-scribe-i18n default-scene-name)))

  ;; Define and insert template
  (let ((template (format "*** TODO %s :ignore:
:PROPERTIES:
:PoV:
:Characters:
:Plot:
:Timeline:
:Location:
:Description:
:Summary:
:Scene-motivation:
:Conflict-source:
:What-is-at-stake:
:Emotion:
:Comment:
:WORD-OBJECTIVE: 500
:END:

{{{scene-break}}}
" scene-name))
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
The template includes a TODO heading with :ignore: tag and a property
drawer with WORDCOUNT field initialized to 0.
If CHAPTER-NAME is empty, defaults to a translated \"New chapter\" string."
  (interactive (list (read-string (org-scribe-i18n chapter-name-prompt))))

  ;; Validate we're in org-mode
  (unless (derived-mode-p 'org-mode)
    (user-error (org-scribe-i18n error-no-org-mode)))

  ;; Use default title if chapter-name is empty
  (when (string-empty-p (string-trim chapter-name))
    (setq chapter-name (org-scribe-i18n default-chapter-name)))

  ;; Define and insert template
  (let ((template (format "** TODO %s :ignore:
:PROPERTIES:
:WORD-OBJECTIVE: 5000
:WORDCOUNT: 0
:END:
" chapter-name))
        (start-pos (point)))

    ;; Insert template
    (insert template)

    ;; Position cursor at the end of the heading
    (goto-char start-pos)
    (end-of-line)))

;;; Project Navigation

;;;###autoload
(defun org-scribe-open-project-file (filename)
  "Quickly open a file in the current writing project (novel or short story).
FILENAME should be relative to project root (e.g., 'plan/characters.org').
Uses completion to help select from common project files."
  (interactive
   (list (completing-read (org-scribe-i18n file-open-prompt)
                          '("README.org"
                            ;; Novel files
                            "novel.org"
                            "novela.org"
                            "revision.org"
                            "org-scribe-journal.org"
                            "diario-escritura.org"
                            "plan/characters.org"
                            "plan/personajes.org"
                            "plan/locations.org"
                            "plan/localizaciones.org"
                            "plan/objects.org"
                            "plan/objetos.org"
                            "plan/plot.org"
                            "plan/trama.org"
                            "plan/timeline.org"
                            "plan/cronologia.org"
                            "notes/notes.org"
                            "notas/notas.org"
                            "notes/research.org"
                            "notas/investigacion.org"
                            ;; Short story files
                            "story.org"
                            "cuento.org"
                            "notes.org"
                            "notas.org"))))
  ;; Use org-scribe-core's project detection if available
  (let ((project-root (if (fboundp 'org-scribe-project-root)
                          (org-scribe-project-root)
                        (or (when-let ((project (project-current)))
                              (project-root project))
                            (locate-dominating-file default-directory "README.org")))))
    (if project-root
        (let ((full-path (expand-file-name filename project-root)))
          (if (file-exists-p full-path)
              (find-file full-path)
            (when (yes-or-no-p (org-scribe-i18n file-not-found filename))
              (find-file full-path))))
      (message (org-scribe-i18n not-in-project)))))

;;; Utility Functions

;;;###autoload
(defun org-scribe-edit-templates ()
  "Open the novel template directory for editing.
This allows you to customize the templates used for new projects."
  (interactive)
  (if (file-directory-p org-scribe-template-directory)
      (dired org-scribe-template-directory)
    (user-error "Template directory not found: %s" org-scribe-template-directory)))

;;;###autoload
(defun org-scribe-register-projects (directory)
  "Register all existing novel projects under DIRECTORY with project.el.
This is useful for adding novels created before project.el integration."
  (interactive "DBase directory containing novel projects: ")
  (let ((count (project-remember-projects-under directory t)))
    (message "Scanned and registered %d project(s) under %s" count directory)))

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
(provide 'writing_project)
(provide 'org-scribe-template)
(provide 'org-scribe-project)

;;; org-scribe-project.el ends here
