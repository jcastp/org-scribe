;;; writing-project.el --- Novel project structure generator -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides functions to create complete novel and short story
;; project structures from templates with variable substitution.
;;
;; Main functions:
;;   - writing-create-project: Create new novel project from templates
;;   - writing-create-short-story-project: Create new short story project
;;   - writing-insert-scene: Insert scene template
;;   - writing-insert-chapter: Insert chapter template
;;   - writing-open-project-file: Quick file navigation
;;
;; Template Variables:
;;   ${TITLE}  - Project title
;;   ${AUTHOR} - User's full name
;;   ${DATE}   - Current date (YYYY-MM-DD format)
;;
;; This was merged from emacs-writing-template package (v2.0).

;;; Code:

(require 'project)

;;; Configuration

(defvar writing-project-package-directory
  (file-name-directory
   (or load-file-name
       (buffer-file-name)))
  "Directory where this file is located.")

(defcustom writing-template-directory
  (expand-file-name "../writing-templates/novel-en" writing-project-package-directory)
  "Directory containing novel project templates.
By default, uses the novel-en (English) templates. Change to
'novel-es' for Spanish templates, or provide a custom path."
  :type 'directory
  :group 'writing)

(defcustom writing-template-language 'en
  "Default language for novel templates.
Can be 'en for English or 'es for Spanish.
This is used to automatically set the template directory."
  :type '(choice (const :tag "English" en)
                 (const :tag "Spanish" es))
  :group 'writing
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; Update template directory when language changes
         (setq writing-template-directory
               (expand-file-name
                (format "../writing-templates/novel-%s"
                        (if (eq value 'es) "es" "en"))
                writing-project-package-directory))))

(defcustom writing-short-story-template-directory
  (expand-file-name "../writing-templates/short-story-en" writing-project-package-directory)
  "Directory containing short story project templates.
By default, uses the short-story-en (English) templates."
  :type 'directory
  :group 'writing)

;;; Project Creation

(defun writing--validate-project-title (title)
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

;;;###autoload
(defun writing-create-project (base-dir title)
  "Create a new novel project structure from templates.
BASE-DIR is the parent directory where the project will be created.
TITLE is the name of the novel/project.

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
    (read-directory-name "Base directory for project: " "~/writing/")
    (read-string "Novel title: ")))

  (unless (file-directory-p writing-template-directory)
    (user-error "Template directory not found: %s" writing-template-directory))

  ;; Validate title
  (let ((validation-error (writing--validate-project-title title)))
    (when validation-error
      (user-error "%s" validation-error)))

  (let* ((project-dir (expand-file-name title base-dir))
         (variables `(("TITLE" . ,title)
                     ("AUTHOR" . ,(if (boundp 'user-full-name) user-full-name "Author"))
                     ("DATE" . ,(format-time-string "%Y-%m-%d")))))

    ;; Check if project already exists
    (when (file-exists-p project-dir)
      (user-error "Project directory '%s' already exists!" project-dir))

    ;; Create project directory
    (make-directory project-dir t)

    ;; Create .writing-project marker file for project detection
    (with-temp-file (expand-file-name ".writing-project" project-dir)
      (insert (format "# Writing project: %s\n" title)
              (format "# Created: %s\n" (format-time-string "%Y-%m-%d"))
              (format "# Language: %s\n" writing-template-language)))

    ;; Initialize git repository
    (let ((default-directory project-dir))
      (unless (zerop (call-process "git" nil nil nil "init"))
        (warn "Failed to initialize git repository")))

    ;; Process all templates
    (writing--copy-templates writing-template-directory project-dir variables)

    ;; Create initial git commit
    (let ((default-directory project-dir))
      (when (zerop (call-process "git" nil nil nil "add" "."))
        (call-process "git" nil nil nil "commit" "-m"
                     (format "Initial commit: %s" title))))

    ;; Register project with project.el
    (project-remember-project (project-current nil project-dir))

    ;; Open README.org
    (find-file (expand-file-name "README.org" project-dir))
    (message "Novel project '%s' created successfully at %s" title project-dir)))

;;;###autoload
(defun writing-create-short-story-project (base-dir title)
  "Create a new short story project structure from templates.
BASE-DIR is the parent directory where the project will be created.
TITLE is the name of the short story/project.

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
    (read-directory-name "Base directory for short story: " "~/writing/")
    (read-string "Short story title: ")))

  ;; Determine template directory based on language
  (let ((template-dir (expand-file-name
                      (format "../writing-templates/short-story-%s"
                              (if (eq writing-template-language 'es) "es" "en"))
                      writing-project-package-directory)))

    (unless (file-directory-p template-dir)
      (user-error "Short story template directory not found: %s" template-dir))

    ;; Validate title
    (let ((validation-error (writing--validate-project-title title)))
      (when validation-error
        (user-error "%s" validation-error)))

    (let* ((project-dir (expand-file-name title base-dir))
           (variables `(("TITLE" . ,title)
                       ("AUTHOR" . ,(if (boundp 'user-full-name) user-full-name "Author"))
                       ("DATE" . ,(format-time-string "%Y-%m-%d"))))
           (story-file (if (eq writing-template-language 'es) "cuento.org" "story.org")))

      ;; Check if project already exists
      (when (file-exists-p project-dir)
        (user-error "Project directory '%s' already exists!" project-dir))

      ;; Create project directory
      (make-directory project-dir t)

      ;; Create .writing-project marker file for project detection
      (with-temp-file (expand-file-name ".writing-project" project-dir)
        (insert (format "# Writing project: %s\n" title)
                (format "# Type: short-story\n")
                (format "# Created: %s\n" (format-time-string "%Y-%m-%d"))
                (format "# Language: %s\n" writing-template-language)))

      ;; Initialize git repository
      (let ((default-directory project-dir))
        (unless (zerop (call-process "git" nil nil nil "init"))
          (warn "Failed to initialize git repository")))

      ;; Process all templates
      (writing--copy-templates template-dir project-dir variables)

      ;; Create initial git commit
      (let ((default-directory project-dir))
        (when (zerop (call-process "git" nil nil nil "add" "."))
          (call-process "git" nil nil nil "commit" "-m"
                       (format "Initial commit: %s" title))))

      ;; Register project with project.el
      (project-remember-project (project-current nil project-dir))

      ;; Open the story file
      (find-file (expand-file-name story-file project-dir))
      (message "Short story project '%s' created successfully at %s" title project-dir))))

(defun writing--copy-templates (template-dir project-dir variables)
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
          (writing--process-template file output-path variables)
        (copy-file file output-path)))))

(defun writing--process-template (template-file output-file variables)
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
(defun writing-insert-scene (scene-name)
  "Insert a scene template at point with SCENE-NAME.
The template includes a TODO heading with :ignore: tag and property
drawer for scene metadata (PoV, Characters, Plot, Timeline, Location,
Description, Summary, Scene-motivation, Conflict-source, What-is-at-stake,
Emotion, and Comment).
If SCENE-NAME is empty, defaults to \"New scene\"."
  (interactive "sScene name: ")

  ;; Validate we're in org-mode
  (unless (derived-mode-p 'org-mode)
    (user-error "This command can only be used in org-mode buffers"))

  ;; Use default title if scene-name is empty
  (when (string-empty-p (string-trim scene-name))
    (setq scene-name "New scene"))

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
:END:
" scene-name))
        (start-pos (point)))

    ;; Insert template
    (insert template)

    ;; Position cursor at first property value (after :PoV:)
    (goto-char start-pos)
    (forward-line 2)  ; Move to PoV line
    (end-of-line)))   ; Move to end of line (after :PoV:)

;;;###autoload
(defun writing-insert-chapter (chapter-name)
  "Insert a chapter template at point with CHAPTER-NAME.
The template includes a TODO heading with :ignore: tag and a property
drawer with WORDCOUNT field initialized to 0.
If CHAPTER-NAME is empty, defaults to \"New chapter\"."
  (interactive "sChapter name: ")

  ;; Validate we're in org-mode
  (unless (derived-mode-p 'org-mode)
    (user-error "This command can only be used in org-mode buffers"))

  ;; Use default title if chapter-name is empty
  (when (string-empty-p (string-trim chapter-name))
    (setq chapter-name "New chapter"))

  ;; Define and insert template
  (let ((template (format "** TODO %s :ignore:
:PROPERTIES:
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
(defun writing-open-project-file (filename)
  "Quickly open a file in the current writing project (novel or short story).
FILENAME should be relative to project root (e.g., 'plan/characters.org').
Uses completion to help select from common project files."
  (interactive
   (list (completing-read "Open file: "
                          '("README.org"
                            ;; Novel files
                            "novel.org"
                            "novela.org"
                            "revision.org"
                            "statistics.org"
                            "estadisticas.org"
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
  ;; Use writing-core's project detection if available
  (let ((project-root (if (fboundp 'writing-project-root)
                          (writing-project-root)
                        (or (when-let ((project (project-current)))
                              (project-root project))
                            (locate-dominating-file default-directory "README.org")))))
    (if project-root
        (let ((full-path (expand-file-name filename project-root)))
          (if (file-exists-p full-path)
              (find-file full-path)
            (when (yes-or-no-p (format "File %s doesn't exist. Create it? " filename))
              (find-file full-path))))
      (message "Not in a novel project directory"))))

;;; Utility Functions

;;;###autoload
(defun writing-edit-templates ()
  "Open the novel template directory for editing.
This allows you to customize the templates used for new projects."
  (interactive)
  (if (file-directory-p writing-template-directory)
      (dired writing-template-directory)
    (user-error "Template directory not found: %s" writing-template-directory)))

;;;###autoload
(defun writing-register-projects (directory)
  "Register all existing novel projects under DIRECTORY with project.el.
This is useful for adding novels created before project.el integration."
  (interactive "DBase directory containing novel projects: ")
  (let ((count (project-remember-projects-under directory t)))
    (message "Scanned and registered %d project(s) under %s" count directory)))

;;; Backwards Compatibility Aliases

;; Provide old function names for backwards compatibility
;;;###autoload
(defalias 'writing-project-create-novel-project #'writing-create-project
  "Deprecated: Use `writing-create-project' instead.")
(make-obsolete 'writing-project-create-novel-project 'writing-create-project "0.2.0")

;;;###autoload
(defalias 'writing-project-insert-scene #'writing-insert-scene
  "Deprecated: Use `writing-insert-scene' instead.")
(make-obsolete 'writing-project-insert-scene 'writing-insert-scene "0.2.0")

;;;###autoload
(defalias 'writing-project-insert-chapter #'writing-insert-chapter
  "Deprecated: Use `writing-insert-chapter' instead.")
(make-obsolete 'writing-project-insert-chapter 'writing-insert-chapter "0.2.0")

;;;###autoload
(defalias 'writing-project-open-novel-file #'writing-open-project-file
  "Deprecated: Use `writing-open-project-file' instead.")
(make-obsolete 'writing-project-open-novel-file 'writing-open-project-file "0.2.0")

;;;###autoload
(defalias 'writing-project-edit-novel-templates #'writing-edit-templates
  "Deprecated: Use `writing-edit-templates' instead.")
(make-obsolete 'writing-project-edit-novel-templates 'writing-edit-templates "0.2.0")

;;;###autoload
(defalias 'writing-project-register-existing-projects #'writing-register-projects
  "Deprecated: Use `writing-register-projects' instead.")
(make-obsolete 'writing-project-register-existing-projects 'writing-register-projects "0.2.0")

;; Provide old feature name for old configs
(provide 'writing_project)
(provide 'writing-template)
(provide 'writing-project)

;;; writing-project.el ends here
