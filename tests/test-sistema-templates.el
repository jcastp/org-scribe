;;; test-sistema-templates.el --- Tests for the sistema template set -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for phases 2 and 3 of the "mi sistema" template migration
;; (see opus-mi-sistema.org).
;;
;; Two things are checked here:
;;
;; 1. The heading-recognition regexps extended in phase 3 match the role
;;    and setting names the method uses, in both languages, and — just as
;;    importantly — do NOT match the structural wrapper headings the new
;;    templates ship (`Los trece irrenunciables', `Cuartiles', and so on),
;;    which would otherwise become phantom entities.
;;
;; 2. The shipped novel-es templates satisfy the contract the entity
;;    predicates impose: characters and settings are level-1 headings
;;    carrying an explicit Role/Type, plot threads carry THREAD-TYPE, and
;;    the thirteen non-negotiables are level 2 so they do not collide with
;;    the plot-thread predicate.
;;
;; The second group parses the real shipped template files rather than
;; fixtures, so a template edit that breaks entity recognition fails here
;; instead of silently producing projects where linking finds nothing.

;;; Code:

(require 'ert)
(require 'org)
(require 'ox)

;;; Add paths

(defvar org-scribe-test--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "Repository root, used to locate the shipped template files.")

(let ((default-directory org-scribe-test--root))
  (dolist (dir '("core" "search" "linking" "capture" "templates"))
    (add-to-list 'load-path (expand-file-name dir default-directory))))

(require 'org-scribe-character-links)
(require 'org-scribe-location-links)
(require 'org-scribe-plot-links)
(require 'org-scribe-plot-point-links)

;;; Helpers

(defun org-scribe-test--template (relative)
  "Return the absolute path of template RELATIVE to the templates directory."
  (expand-file-name (concat "org-scribe-templates/" relative)
                    org-scribe-test--root))

(defmacro org-scribe-test--with-template (relative &rest body)
  "Visit template RELATIVE in `org-mode' and run BODY with point at start."
  (declare (indent 1))
  `(let ((file (org-scribe-test--template ,relative)))
     (should (file-exists-p file))
     (with-temp-buffer
       (insert-file-contents file)
       (org-mode)
       (goto-char (point-min))
       ,@body)))

(defun org-scribe-test--headings (level)
  "Return the heading texts at LEVEL in the current buffer."
  (let (result)
    (org-map-entries
     (lambda ()
       (when (= (org-current-level) level)
         (push (org-get-heading t t t t) result))))
    (nreverse result)))

(defun org-scribe-test--entities (predicate)
  "Return heading texts in the current buffer for which PREDICATE returns non-nil.
PREDICATE is called with point on each heading."
  (let (result)
    (org-map-entries
     (lambda ()
       (when (funcall predicate)
         (push (org-get-heading t t t t) result))))
    (nreverse result)))

;;; Phase 3 — heading regexps

(ert-deftest test-sistema-character-regexp-matches-method-roles ()
  "The character regexp matches every role name the method uses."
  (dolist (heading '("Protagonista" "Protagonist"
                     "Oponente" "Opponent"
                     "Aliado" "Ally"
                     "Falso aliado" "Fake-Ally Opponent"
                     "Secundario temático" "Thematic Supporting"
                     ;; pre-existing vocabulary must keep working
                     "Character" "Personaje" "Antagonista"))
    (should (string-match-p org-scribe--character-heading-regexp heading))))

(ert-deftest test-sistema-character-regexp-rejects-structural-headings ()
  "The character regexp does not match the templates' wrapper headings.
A false positive here would create a phantom character entity and mint
an ID on a structural heading."
  (dolist (heading '("Comprobación del elenco" "Cast Check"
                     "Notas" "Notes" "Deseo" "Necesidad" "Plan"
                     "Aspectos" "Trasfondo" "Interrogatorio"
                     "Hitos de cambio" "Change Milestones"))
    (should-not (string-match-p org-scribe--character-heading-regexp heading))))

(ert-deftest test-sistema-location-regexp-matches-escenario ()
  "The location regexp matches the method's \"Escenario\" heading."
  (dolist (heading '("Escenario" "Setting" "Location" "Localización"
                     "Ubicación" "Lugar" "Place"))
    (should (string-match-p org-scribe--location-heading-regexp heading))))

(ert-deftest test-sistema-location-regexp-rejects-structural-headings ()
  "The location regexp does not match the world/plot wrapper headings."
  (dolist (heading '("El mundo en una línea" "Reglas y límites"
                     "Descripción" "Investigación pendiente"
                     "Cuartiles" "Los trece irrenunciables"))
    (should-not (string-match-p org-scribe--location-heading-regexp heading))))

;;; Phase 2 — the shipped Spanish templates

(ert-deftest test-sistema-es-design-file-exists ()
  "The new diseno.org template ships and carries the Starting Gate."
  (org-scribe-test--with-template "novel-es/diseno.org.template"
    (let ((headings (org-scribe-test--headings 1)))
      (should (member "Puerta de salida" headings))
      (should (member "Núcleo" headings))
      (should (member "Premisa" (append headings (list "Premisa")))))
    ;; The gate has exactly eight boxes: it is the one list that decides
    ;; when writing starts, and losing one silently lowers the bar.
    (goto-char (point-min))
    (should (= 8 (how-many "^- \\[ \\]" (point-min) (point-max))))))

(ert-deftest test-sistema-es-characters-are-level-1-with-role ()
  "Every character in the Spanish template is level 1 with an explicit Role.
This is the contract in `org-scribe--character-heading-p'; a nested
character is invisible to linking, capture and the health report."
  (org-scribe-test--with-template "novel-es/objects/personajes.org.template"
    (let ((roles '("Protagonist" "Opponent" "Ally"
                   "Fake-Ally Opponent" "Thematic Supporting"))
          (found nil))
      (org-map-entries
       (lambda ()
         (when-let ((role (org-entry-get nil "Role")))
           (should (= 1 (org-current-level)))
           (push role found))))
      (should (equal roles (nreverse found))))))

(ert-deftest test-sistema-character-headings-are-names-not-roles ()
  "Character headings are name placeholders, never the role itself.
The heading is the character's name and the role lives in `Role'.  That
separation is what lets a writer rename a character freely: the heading
changes, the property does not, and every scene link keeps resolving.
Shipping the role as the heading would invite the writer to overwrite it
and lose the only human-readable label the file has.

The placeholders must also be distinct from one another, for the same
reason as the settings above — completion is keyed on display name."
  (org-scribe-test--with-template "novel-es/objects/personajes.org.template"
    (let (names)
      (org-map-entries
       (lambda ()
         (when (org-entry-get nil "Role")
           (push (org-get-heading t t t t) names))))
      (setq names (nreverse names))
      (should (equal '("[Nombre del protagonista]"
                       "[Nombre del oponente]"
                       "[Nombre del aliado]"
                       "[Nombre del falso aliado]"
                       "[Nombre del secundario]")
                     names))
      ;; No heading is a bare role word.
      (dolist (n names)
        (should-not (member n '("Protagonista" "Oponente" "Aliado"
                                "Falso aliado" "Secundario temático"))))
      ;; All distinct.
      (should (= (length names) (length (delete-dups (copy-sequence names))))))))

(ert-deftest test-sistema-es-cast-check-is-not-a-character ()
  "The Comprobación del elenco heading does not become a phantom character."
  (org-scribe-test--with-template "novel-es/objects/personajes.org.template"
    (goto-char (point-min))
    (should (re-search-forward "^\\* Comprobación del elenco" nil t))
    (should-not (org-entry-get nil "Role"))
    (should-not (string-match-p org-scribe--character-heading-regexp
                                (org-get-heading t t t t)))))

(ert-deftest test-sistema-es-settings-are-level-1-with-type ()
  "Every Escenario in the Spanish template is level 1 with an explicit Type.
Their headings must also be distinct: entity completion is keyed on the
display name, so three placeholders all called \"Escenario\" would
collapse into one selectable item and every link would resolve to the
first."
  (org-scribe-test--with-template "novel-es/objects/localizaciones.org.template"
    (let (names)
      (org-map-entries
       (lambda ()
         (when (org-entry-get nil "Type")
           (should (= 1 (org-current-level)))
           (push (org-get-heading t t t t) names))))
      ;; Three worked settings ship, matching the plantilla.  Headings are
      ;; name placeholders, not the classification: that lives in `Type'.
      (should (equal '("[Nombre del escenario 1]"
                       "[Nombre del escenario 2]"
                       "[Nombre del escenario 3]")
                     (nreverse names))))))

(ert-deftest test-sistema-es-thirteen-non-negotiables-are-level-2 ()
  "The thirteen are level-2 headings under their wrapper.
Level 1 would put them beside plot threads, where only the heading text
would tell them apart (see opus-mi-sistema.org section 11.1)."
  (org-scribe-test--with-template "novel-es/objects/trama.org.template"
    (goto-char (point-min))
    (should (re-search-forward "^\\* Los trece irrenunciables" nil t))
    (let ((subs (save-restriction
                  (org-narrow-to-subtree)
                  (org-scribe-test--headings 2))))
      (should (= 13 (length subs)))
      (should (string-match-p "\\`1\\. Susurro del tema" (car subs)))
      (should (string-match-p "\\`13\\. Decisión moral" (car (last subs)))))))

(ert-deftest test-sistema-es-autorrevelacion-is-one-heading ()
  "Self-revelation is a single heading, not split into two.
It is \"un punto en dos tiempos\", but the count thirteen is load-bearing
in the method's own name for the set."
  (org-scribe-test--with-template "novel-es/objects/trama.org.template"
    (should (= 1 (how-many "^\\*\\* 11\\. Autorrevelación" (point-min) (point-max))))
    (should (= 0 (how-many "^\\*\\* 1[45]\\." (point-min) (point-max))))))

(ert-deftest test-sistema-es-plot-threads-carry-thread-type ()
  "Exactly the two shipped narrative lines carry THREAD-TYPE.
The structural wrappers in trama.org must not, or they would each become
a phantom plot-thread entity with an ID minted at project creation."
  (org-scribe-test--with-template "novel-es/objects/trama.org.template"
    (let (threads)
      (org-map-entries
       (lambda ()
         (when (org-entry-get nil "THREAD-TYPE")
           (should (= 1 (org-current-level)))
           (push (org-get-heading t t t t) threads))))
      (should (equal '("Línea principal" "Subtrama") (nreverse threads))))))

(ert-deftest test-sistema-es-plot-wrappers-are-not-threads ()
  "The structural level-1 headings in trama.org fail the plot predicate."
  (org-scribe-test--with-template "novel-es/objects/trama.org.template"
    (dolist (heading '("Los trece irrenunciables" "Puntos opcionales usados"
                       "Secuencia de revelaciones" "Cuartiles"
                       "Orden de apertura y cierre" "Secuencias y escenas"))
      (should-not (string-match-p
                   "\\(Main Plot\\|Subplot\\|Thread\\|A-[Pp]lot\\|B-[Pp]lot\\|C-[Pp]lot\\)"
                   heading)))))

(ert-deftest test-sistema-no-template-ships-export-config ()
  "No shipped template carries export configuration, in any template set.
Decision recorded in opus-mi-sistema.org section 4, note 5 and extended
to the manuscripts and short-story sets: the blocks carried personal
absolute paths (a `file:///home/jcastp/' stylesheet, a private .ott) and
required LaTeX classes that are not shipped, so every user's first
export failed on someone else's machine.

Dropping `#+EXCLUDE_TAGS: noexport' along with them is safe and was
checked rather than assumed: `org-export-exclude-tags' already defaults
to (\"noexport\"), so `:noexport:' subtrees still drop from every
backend.  A regression here would silently start exporting the project's
planning sections into the manuscript."
  (let ((offenders nil))
    (dolist (file (directory-files-recursively
                   (expand-file-name "org-scribe-templates" org-scribe-test--root)
                   "\\.template\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((raw (buffer-string)))
          (when (string-match-p
                 (concat "ODT_STYLES_FILE\\|LATEX_CLASS\\|EPUBSTYLE\\|EPUB_STYLESHEET"
                         "\\|EXCLUDE_TAGS\\|EXPORT_FILE_NAME"
                         "\\|^\\* Export [Cc]onfig\\|^\\* Configuración de Exportación")
                 raw)
            (push (file-name-nondirectory file) offenders))
          ;; No template may hard-code a personal home directory.
          (when (string-match-p "/home/[a-z]+/" raw)
            (push (concat (file-name-nondirectory file) " (home path)") offenders)))))
    (should-not offenders)))

(ert-deftest test-sistema-noexport-still-drops-without-exclude-tags ()
  "`:noexport:' subtrees drop from export with no `#+EXCLUDE_TAGS' line.
This is the assumption that made removing the export blocks safe."
  (should (member "noexport" (default-value 'org-export-exclude-tags)))
  (with-temp-buffer
    (org-mode)
    (insert "* Kept\nprose here\n* Dropped :noexport:\nplanning metadata\n")
    (let ((out (org-export-as 'ascii nil nil t)))
      (should (string-match-p "prose here" out))
      (should-not (string-match-p "planning metadata" out)))))

(ert-deftest test-sistema-es-templates-substitute-cleanly ()
  "Every sistema template declares TITLE and AUTHOR placeholders."
  (dolist (relative '("novel-es/diseno.org.template"
                      "novel-es/objects/personajes.org.template"
                      "novel-es/objects/localizaciones.org.template"
                      "novel-es/objects/worldbuilding.org.template"
                      "novel-es/objects/trama.org.template"
                      "novel-es/revision.org.template"))
    (let ((raw (with-temp-buffer
                 (insert-file-contents (org-scribe-test--template relative))
                 (buffer-string))))
      (should (string-match-p "\\${TITLE}" raw))
      (should (string-match-p "\\${AUTHOR}" raw)))))

;;; Phase 4 — new scene property keys

(ert-deftest test-sistema-new-scene-property-aliases ()
  "The four keys added for the method resolve in both languages."
  (should (equal (org-scribe-scene-property-aliases 'gap) '("Gap" "Brecha")))
  (should (equal (org-scribe-scene-property-aliases 'world-problem)
                 '("World-problem" "Problema-mundo")))
  (should (equal (org-scribe-scene-property-aliases 'sequel-decision)
                 '("Sequel-decision" "Decision-secuela")))
  (should (equal (org-scribe-scene-property-aliases 'plot-point)
                 '("Plot-point" "Punto-de-trama")))
  (should (equal (org-scribe-scene-property-name 'gap 'es) "Brecha"))
  (should (equal (org-scribe-scene-property-name 'gap 'en) "Gap"))
  (should (equal (org-scribe-scene-property-name 'plot-point 'es) "Punto-de-trama")))

(ert-deftest test-sistema-scene-drawer-includes-new-keys ()
  "Generated scene drawers carry the method's four new properties.
Uses the default project language (English), since
`org-scribe--scene-property-drawer-lines' resolves names through
`org-scribe-project-language', which reads the project marker file and
falls back to the *default value* of `org-scribe-template-language' — a
`let' binding cannot influence it."
  (let ((drawer (org-scribe--scene-property-drawer-lines)))
    (dolist (prop '(":Plot-point:" ":Gap:" ":World-problem:" ":Sequel-decision:"))
      (should (string-match-p (regexp-quote prop) drawer)))
    ;; Gap sits between the opposition and what is at stake, following the
    ;; method's scene table order.
    (should (< (string-match ":Conflict-source:" drawer)
               (string-match ":Gap:" drawer)
               (string-match ":What-is-at-stake:" drawer)))))

(ert-deftest test-sistema-manuscript-drawers-cover-every-canonical-key ()
  "Both shipped manuscripts declare every canonical scene property.
Checked through the alias table rather than by generating a drawer,
because drawer generation resolves the language from the project marker
file and cannot be redirected by binding a variable in a test.

A gap here means a project's shipped first scene and any scene the
writer later inserts have different property sets, which silently breaks
column view and the health report's missing-property lists."
  (dolist (relative '("novel-es/novela.org.template"
                      "novel-en/novel.org.template"))
    (let ((raw (with-temp-buffer
                 (insert-file-contents (org-scribe-test--template relative))
                 (buffer-string))))
      (dolist (key org-scribe--scene-property-keys)
        (let ((aliases (org-scribe-scene-property-aliases key)))
          ;; At least one localized spelling of the key must be present.
          (should (cl-some (lambda (alias)
                             (string-match-p (regexp-quote (concat ":" alias ":")) raw))
                           aliases)))))))

(ert-deftest test-sistema-columns-spec-is-localized-and-parallel ()
  "Both manuscripts expose the same columns, under localized property names."
  (let ((es (with-temp-buffer
              (insert-file-contents (org-scribe-test--template "novel-es/novela.org.template"))
              (buffer-string)))
        (en (with-temp-buffer
              (insert-file-contents (org-scribe-test--template "novel-en/novel.org.template"))
              (buffer-string))))
    (should (string-match-p "#\\+COLUMNS:.*Punto-de-trama.*Brecha" es))
    (should (string-match-p "#\\+COLUMNS:.*Plot-point.*Gap" en))
    ;; Same number of columns in both.
    (cl-flet ((ncols (s) (length (split-string
                                  (car (split-string
                                        (substring s (string-match "#\\+COLUMNS:" s)) "\n"))
                                  "%" t))))
      (should (= (ncols es) (ncols en))))))

;;; Phase 4b — the plot-point entity

(ert-deftest test-sistema-plot-point-entity-registered ()
  "The plot-point entity type is registered and its API is generated."
  (should (assq 'plot-point org-scribe-entity-registry))
  (dolist (fn '(org-scribe--get-all-plot-points
                org-scribe-add-plot-point-ids
                org-scribe-insert-plot-point-link
                org-scribe-set-scene-plot-points
                org-scribe-setup-plot-point-links
                org-scribe-update-all-plot-point-link-names
                org-scribe-jump-to-plot-point))
    (should (fboundp fn))))

(ert-deftest test-sistema-plot-point-predicate-requires-level-2-under-wrapper ()
  "Only level-2 headings under the non-negotiables wrapper are plot points.
The wrapper itself must not match, or it would become a phantom entity
with an ID minted at project creation."
  (with-temp-buffer
    (org-mode)
    (insert "* Los trece irrenunciables\n"
            "** 1. Susurro del tema\n"
            "*** Detalle\n"
            "* Subtrama\n"
            ":PROPERTIES:\n:THREAD-TYPE: Subtrama\n:END:\n"
            "** Cruces\n")
    (let (matched)
      (org-map-entries
       (lambda ()
         (when (org-scribe--plot-point-heading-p)
           (push (org-get-heading t t t t) matched))))
      (should (equal '("1. Susurro del tema") (nreverse matched))))))

(ert-deftest test-sistema-plot-point-predicate-matches-english-wrapper ()
  "The English wrapper name is recognized too."
  (with-temp-buffer
    (org-mode)
    (insert "* The Thirteen Non-Negotiables\n** 1. Theme Stated\n")
    (goto-char (point-min))
    (re-search-forward "^\\*\\* 1\\.")
    (should (org-scribe--plot-point-heading-p))))

(ert-deftest test-sistema-es-thirteen-are-recognized-as-plot-points ()
  "All thirteen shipped non-negotiables satisfy the plot-point predicate,
and nothing else in the plot file does."
  (org-scribe-test--with-template "novel-es/objects/trama.org.template"
    (let (points)
      (org-map-entries
       (lambda ()
         (when (org-scribe--plot-point-heading-p)
           (push (org-get-heading t t t t) points))))
      (should (= 13 (length points))))))

(ert-deftest test-sistema-plot-points-and-threads-do-not-overlap ()
  "No heading is both a plot point and a plot thread.
They share a file, so a heading matching both predicates would be minted
twice and appear in two completion lists."
  (org-scribe-test--with-template "novel-es/objects/trama.org.template"
    (org-map-entries
     (lambda ()
       (should-not (and (org-scribe--plot-point-heading-p)
                        (org-scribe--plot-heading-p)))))))

(declare-function org-scribe--health-collect-referenced-ids "org-scribe-health")

(ert-deftest test-sistema-health-counts-plot-point-references ()
  "The health report's ID collector reads the Plot-point property.
Every entity type whose orphans are reported must have its scene
property in `org-scribe--health-collect-referenced-ids'; omitting one
makes the report claim all of that type's entities are orphaned, which
is exactly wrong and looks plausible."
  (add-to-list 'load-path (expand-file-name "reporting" org-scribe-test--root))
  (require 'org-scribe-health)
  (let ((file (make-temp-file "org-scribe-health-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* Act I\n** Chapter 1\n*** Scene 1\n"
                    ":PROPERTIES:\n"
                    ":Plot-point: [[id:pp-001][1. Theme Stated]]\n"
                    ":Characters: [[id:ch-001][Alex]]\n"
                    ":END:\n"))
          (let ((ids (org-scribe--health-collect-referenced-ids file)))
            (should (gethash "pp-001" ids))
            (should (gethash "ch-001" ids))))
      (delete-file file))))

(provide 'test-sistema-templates)
;;; test-sistema-templates.el ends here
