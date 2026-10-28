;;; test-companion-model.el --- Tests for the companion's snapshot -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for `companion/org-scribe-companion-model.el' (Phase 1): the
;; snapshot of a project as the browser will see it.  The English checks run
;; on a temporary copy of `examples/demo-novel'; the Spanish and edge cases
;; on small hand-written projects.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (dolist (dir '("core" "lang" "linking" "search" "capture" "templates" "companion"))
    (add-to-list 'load-path (expand-file-name dir parent-dir))))

(require 'org-scribe-config)
(require 'org-scribe-core)
(require 'org-scribe-character-links)
(require 'org-scribe-location-links)
(require 'org-scribe-plot-links)
(require 'org-scribe-plot-point-links)
(require 'org-scribe-search)
(require 'org-scribe-companion)

(defconst test-companion-model--repo
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the org-scribe checkout.")

;;; Fixtures

(defmacro test-companion-model--with-project (root-var setup &rest body)
  "Run BODY with ROOT-VAR bound to a fresh project directory made by SETUP.
SETUP is a function called with the directory.  The directory is the
`default-directory' and every file buffer opened in it is killed after."
  (declare (indent 2))
  `(let* ((,root-var (file-name-as-directory (make-temp-file "test-companion-model-" t)))
          (default-directory ,root-var)
          (org-id-locations-file (expand-file-name "ids" ,root-var))
          (make-backup-files nil)
          (create-lockfiles nil))
     (unwind-protect
         (progn
           (funcall ,setup ,root-var)
           (org-scribe-project-type-cache-clear)
           (org-scribe-companion--clear-cache)
           ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (string-prefix-p ,root-var file)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (org-scribe-project-type-cache-clear)
       (org-scribe-companion--clear-cache)
       (delete-directory ,root-var t))))

(defun test-companion-model--copy-demo (root)
  "Copy examples/demo-novel into ROOT."
  (copy-directory (expand-file-name "examples/demo-novel" test-companion-model--repo)
                  root t t t))

(defun test-companion-model--write (root name content)
  "Write CONTENT to file NAME under ROOT."
  (with-temp-file (expand-file-name name root) (insert content)))

(defun test-companion-model--spanish (root)
  "Write a small Spanish novel project into ROOT."
  (test-companion-model--write
   root ".org-scribe-project"
   "# Writing project: Prueba\n# Type: novel\n# Language: es\n")
  (test-companion-model--write
   root "novela.org"
   "#+TITLE: Prueba
#+TODO: TODO TOWRITE | FINISHED
* Apéndice :noexport:
** Capítulo falso
*** Escena falsa
* Acto 1
:PROPERTIES:
:ID: a1
:END:
** Capítulo 1
:PROPERTIES:
:ID: ch1
:WORD-OBJECTIVE: 3000
:END:
*** TOWRITE Escena 1
:PROPERTIES:
:ID: s1
:WORDCOUNT: 120
:PoV: [[id:c1][Ana]]
:Personajes: [[id:c1][Ana]], [[id:c3][Smith, John]], Pedro
:Trama:
:Resumen: Ana llega.
:Relaciones: no es un campo
:END:
*** Escena 2 sin ID
:PROPERTIES:
:Resumen: otra
:END:
")
  (test-companion-model--write
   root "diseno.org"
   "#+TITLE: Diseño
#+TODO: OPEN | CHOSEN
* Reparto
** Personajes
*** Ana
:PROPERTIES:
:ID: c1
:Role: Protagonista
:Weight:
:END:
*** Pedro
:PROPERTIES:
:ID: c2
:Role: Aliado
:Weight: -1
:END:
*** Smith, John
:PROPERTIES:
:ID: c3
:Role: Opositor
:Weight: 2
:END:
*** Sin Rol
"))

(defun test-companion-model--titles (snapshot level)
  "Return the titles of SNAPSHOT's headings at LEVEL (1 act, 2 chapter, 3 scene)."
  (let (titles)
    (seq-doseq (act (plist-get snapshot :acts))
      (when (= level 1) (push (plist-get act :title) titles))
      (seq-doseq (chapter (plist-get act :chapters))
        (when (= level 2) (push (plist-get chapter :title) titles))
        (seq-doseq (scene (plist-get chapter :scenes))
          (when (= level 3) (push (plist-get scene :title) titles)))))
    (nreverse titles)))

(defun test-companion-model--scene (snapshot title)
  "Return the scene of SNAPSHOT titled TITLE."
  (cl-block nil
    (seq-doseq (act (plist-get snapshot :acts))
      (seq-doseq (chapter (plist-get act :chapters))
        (seq-doseq (scene (plist-get chapter :scenes))
          (when (equal (plist-get scene :title) title) (cl-return scene)))))))

(defun test-companion-model--entity (snapshot kind name)
  "Return the entity NAME of KIND (a keyword) in SNAPSHOT."
  (seq-find (lambda (e) (equal (plist-get e :name) name))
            (plist-get (plist-get snapshot :entities) kind)))

;;; The demo novel

(ert-deftest test-companion-model-demo-structure-and-order ()
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (let ((snapshot (org-scribe-companion--snapshot)))
      (should (equal '("Act I" "Act II" "Act III")
                     (test-companion-model--titles snapshot 1)))
      (should (equal '("Chapter 1 - Three Alarms" "Chapter 2 - What It Costs"
                       "Chapter 3 - Day One" "Chapter 4 - Adjusting"
                       "Chapter 5 - Six O'Clock" "Chapter 6 - Thursday"
                       "Chapter 7 - Winding It" "Chapter 8 - The New Normal")
                     (test-companion-model--titles snapshot 2)))
      (should (= 15 (length (test-companion-model--titles snapshot 3))))
      (should (equal "Scene 1 - The Alarm"
                     (car (test-companion-model--titles snapshot 3))))
      (should (equal "Scene 2 - Full Circle"
                     (car (last (test-companion-model--titles snapshot 3))))))))

(ert-deftest test-companion-model-demo-has-no-apparatus ()
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (let ((titles (append (test-companion-model--titles (org-scribe-companion--snapshot) 1)
                          (test-companion-model--titles (org-scribe-companion--snapshot) 2)
                          (test-companion-model--titles (org-scribe-companion--snapshot) 3))))
      (dolist (apparatus '("Project Goals" "Progress Tracking" "Quick Navigation"
                           "Writing Sessions" "Character Timeline"))
        (should-not (member apparatus titles))))))

(ert-deftest test-companion-model-demo-scene-data ()
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (let* ((snapshot (org-scribe-companion--snapshot))
           (scene (test-companion-model--scene snapshot "Scene 1 - The Alarm")))
      (should (equal "FINISHED" (plist-get scene :todo)))
      (should (= 167 (plist-get scene :words)))
      (should (= 500 (plist-get scene :objective)))
      (should (eq :false (plist-get scene :readOnly)))
      (should (equal "a6aaa600-35b3-4193-9510-a4bb2ef0000f" (plist-get scene :id)))
      (let ((pov (plist-get (plist-get scene :fields) :pov)))
        (should (equal "char-alex-rivera-001"
                       (plist-get (aref (plist-get pov :links) 0) :id)))
        (should (equal "Alex" (plist-get (aref (plist-get pov :links) 0) :name))))
      (should (equal "Day 1, 06:30"
                     (plist-get (plist-get (plist-get scene :fields) :timeline) :raw))))))

(ert-deftest test-companion-model-demo-todo-keywords-come-from-the-file ()
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (let ((todo (plist-get (org-scribe-companion--snapshot) :todo)))
      (should (equal ["TODO" "TOWRITE" "TOREVIEW" "REDO" "RESTRUCTURE" "FINISHED" "PURGE"]
                     (plist-get todo :keywords)))
      (should (equal ["FINISHED" "PURGE"] (plist-get todo :done)))
      (should (plist-get todo :colors)))))

(ert-deftest test-companion-model-demo-entities-weights-and-hidden ()
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (let* ((snapshot (org-scribe-companion--snapshot))
           (alex (test-companion-model--entity snapshot :character "Alex Rivera"))
           (marisol (test-companion-model--entity snapshot :character "Marisol Rivera"))
           (late (test-companion-model--entity snapshot :plot "Running late"))
           (flat (test-companion-model--entity snapshot :location "Alex's Flat, 4A")))
      (should (= 1 (plist-get alex :weight)))
      (should (eq :false (plist-get alex :hidden)))
      (should (equal "Protagonist" (plist-get alex :role)))
      ;; hidden entities stay in the list, flagged
      (should (eq t (plist-get marisol :hidden)))
      (should (eq t (plist-get late :hidden)))
      (should (= 999.0 (plist-get flat :weight)))
      (should (eq :false (plist-get flat :hidden)))
      (should (= 13 (length (plist-get (plist-get snapshot :entities) :plot-point))))
      (should (cl-every (lambda (p) (eq t (plist-get p :hasContent)))
                        (plist-get (plist-get snapshot :entities) :plot-point))))))

(ert-deftest test-companion-model-entity-pass-agrees-with-the-linking-getters ()
  "The fast entity walk finds what `org-scribe--get-all-entities' finds."
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (let ((snapshot (org-scribe-companion--snapshot)))
      (dolist (entry org-scribe-entity-registry)
        (let ((expected (sort (mapcar #'car (org-scribe--get-all-entities (cdr entry)))
                              #'string<))
              (found (sort (mapcar (lambda (e) (plist-get e :name))
                                   (plist-get (plist-get snapshot :entities)
                                              (intern (concat ":" (symbol-name (car entry))))))
                           #'string<)))
          (should (equal expected found)))))))

(ert-deftest test-companion-model-demo-serializes-to-json ()
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (let* ((json (org-scribe-companion--snapshot-json))
           (parsed (json-parse-string json :object-type 'alist)))
      (should (equal "en" (alist-get 'language parsed)))
      (should (equal "The Morning Routine"
                     (alist-get 'title (alist-get 'project parsed))))
      (should (= 3 (length (alist-get 'acts parsed))))
      (should (equal "novel.org"
                     (alist-get 'file (alist-get 'manuscript (alist-get 'buffers parsed))))))))

;;; Spanish project and edge cases

(ert-deftest test-companion-model-spanish-fields-and-links ()
  (test-companion-model--with-project root #'test-companion-model--spanish
    (let* ((snapshot (org-scribe-companion--snapshot))
           (scene (test-companion-model--scene snapshot "Escena 1"))
           (fields (plist-get scene :fields)))
      (should (equal "es" (plist-get snapshot :language)))
      (should (equal "TOWRITE" (plist-get scene :todo)))
      (should (= 120 (plist-get scene :words)))
      ;; a comma inside a link's text does not split it
      (let ((chars (plist-get fields :characters)))
        (should (equal '("Ana" "Smith, John")
                       (mapcar (lambda (l) (plist-get l :name))
                               (plist-get chars :links))))
        ;; plain names are kept, not dropped
        (should (equal ["Pedro"] (plist-get chars :unlinked))))
      (should (equal "Ana llega." (plist-get (plist-get fields :summary) :raw)))
      ;; a blank property is absent
      (should-not (plist-member fields :plot))
      ;; relationships data is not a scene field
      (should-not (plist-member fields :relationships))
      (should-not (seq-find (lambda (f) (equal "relationships" (plist-get f :key)))
                            (plist-get snapshot :sceneFields))))))

(ert-deftest test-companion-model-scene-field-names-follow-the-project-language ()
  (test-companion-model--with-project root #'test-companion-model--spanish
    (let ((field (seq-find (lambda (f) (equal "characters" (plist-get f :key)))
                           (plist-get (org-scribe-companion--snapshot) :sceneFields))))
      (should (equal "Personajes" (plist-get field :name)))
      (should (equal "link-many" (plist-get field :kind)))
      (should (equal "character" (plist-get field :entity))))))

(ert-deftest test-companion-model-noexport-excluded-and-missing-ids-read-only ()
  (test-companion-model--with-project root #'test-companion-model--spanish
    (let ((snapshot (org-scribe-companion--snapshot)))
      (should (equal '("Acto 1") (test-companion-model--titles snapshot 1)))
      (should (equal '("Escena 1" "Escena 2 sin ID")
                     (test-companion-model--titles snapshot 3)))
      (let ((no-id (test-companion-model--scene snapshot "Escena 2 sin ID")))
        (should (eq :null (plist-get no-id :id)))
        (should (eq t (plist-get no-id :readOnly))))
      (should (= 1 (org-scribe-companion--headings-without-id snapshot))))))

(ert-deftest test-companion-model-weights-blank-is-not-hidden-negative-is ()
  (test-companion-model--with-project root #'test-companion-model--spanish
    (let* ((snapshot (org-scribe-companion--snapshot))
           (ana (test-companion-model--entity snapshot :character "Ana"))
           (pedro (test-companion-model--entity snapshot :character "Pedro"))
           (john (test-companion-model--entity snapshot :character "Smith, John")))
      (should (= 999.0 (plist-get ana :weight)))
      (should (eq :false (plist-get ana :hidden)))
      (should (eq t (plist-get pedro :hidden)))
      (should (= 2 (plist-get john :weight)))
      ;; a heading with no Role under Personajes is not a character
      (should-not (test-companion-model--entity snapshot :character "Sin Rol")))))

(ert-deftest test-companion-model-chapter-outside-any-act-gets-an-implicit-parent ()
  (test-companion-model--with-project root
      (lambda (root)
        (test-companion-model--write
         root ".org-scribe-project" "# Writing project: P\n# Type: novel\n# Language: en\n")
        (test-companion-model--write
         root "novel.org"
         "** Orphan chapter\n:PROPERTIES:\n:ID: c\n:END:\n*** Orphan scene\n:PROPERTIES:\n:ID: s\n:END:\n"))
    (let* ((snapshot (org-scribe-companion--snapshot))
           (act (aref (plist-get snapshot :acts) 0)))
      (should (eq :null (plist-get act :id)))
      (should (eq t (plist-get act :implicit)))
      (should (equal '("Orphan scene") (test-companion-model--titles snapshot 3)))
      ;; implicit parents are not headings that could be given an ID
      (should (= 0 (org-scribe-companion--headings-without-id snapshot))))))

;;; The kind table

(ert-deftest test-companion-model-kind-table-covers-every-registry-property ()
  (dolist (entry org-scribe-entity-registry)
    (dolist (property (plist-get (cdr entry) :properties))
      (should (assq property org-scribe-companion--field-kinds))
      (should (eq (car entry)
                  (plist-get (alist-get property org-scribe-companion--field-kinds)
                             :entity))))))

(ert-deftest test-companion-model-every-entity-kind-has-a-section ()
  (dolist (entry org-scribe-entity-registry)
    (should (assq (car entry) org-scribe-companion--entity-sections))))

;;; Revision and cache

(ert-deftest test-companion-model-unchanged-project-is-served-from-the-cache ()
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (let ((first (org-scribe-companion--snapshot))
          (second (org-scribe-companion--snapshot)))
      (should (equal (plist-get first :revision) (plist-get second :revision)))
      ;; the TODO colours are built on every call (they follow the theme), the
      ;; keywords come from the cached manuscript half
      (should (equal (plist-get first :todo) (plist-get second :todo)))
      (should (eq (plist-get (plist-get first :todo) :keywords)
                  (plist-get (plist-get second :todo) :keywords)))
      (should (eq (plist-get first :storyline) (plist-get second :storyline)))
      (should (eq (plist-get first :entities) (plist-get second :entities))))))

(ert-deftest test-companion-model-edit-changes-revision-and-only-its-half ()
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (let ((before (org-scribe-companion--snapshot)))
      (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
        (goto-char (point-min))
        (re-search-forward "^\\*\\*\\* FINISHED Scene 1 - The Alarm")
        (replace-match "*** TODO Scene 1 - The Alarm"))
      (let ((after (org-scribe-companion--snapshot)))
        (should-not (equal (plist-get before :revision) (plist-get after :revision)))
        (should (equal "TODO" (plist-get (test-companion-model--scene
                                          after "Scene 1 - The Alarm")
                                         :todo)))
        (should (eq :true-modified
                    (if (eq t (plist-get (plist-get (plist-get after :buffers) :manuscript)
                                         :modified))
                        :true-modified :no)))
        ;; the design half was not walked again
        (should (eq (plist-get before :entities) (plist-get after :entities)))))))

;;; Missing IDs

(ert-deftest test-companion-model-add-missing-ids-edits-the-buffer-only ()
  (test-companion-model--with-project root #'test-companion-model--spanish
    (should (= 1 (org-scribe-companion--add-missing-ids root)))
    (should (= 0 (org-scribe-companion--headings-without-id
                  (org-scribe-companion--snapshot))))
    ;; an ordinary buffer edit: not saved
    (should (buffer-modified-p (find-file-noselect (expand-file-name "novela.org" root))))
    (should-not (string-match-p
                 "Escena 2 sin ID\n:PROPERTIES:\n:ID:"
                 (with-temp-buffer
                   (insert-file-contents (expand-file-name "novela.org" root))
                   (buffer-string))))))

(ert-deftest test-companion-model-ensure-ids-asks-once-and-respects-no ()
  (test-companion-model--with-project root #'test-companion-model--spanish
    (let ((asked 0))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) (cl-incf asked) nil)))
        (should (= 1 (org-scribe-companion--ensure-ids root)))
        (should (= 1 asked)))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) (cl-incf asked) t)))
        (should (= 0 (org-scribe-companion--ensure-ids root)))
        (should (= 2 asked))
        (should (= 0 (org-scribe-companion--ensure-ids root)))
        (should (= 2 asked))))))

(ert-deftest test-companion-model-debug-snapshot-shows-json ()
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (save-window-excursion
      (org-scribe-companion-debug-snapshot)
      (should (string-match-p "\"The Morning Routine\"" (buffer-string)))
      (kill-buffer (current-buffer)))))

(ert-deftest test-companion-model-messages-in-both-languages ()
  (dolist (key '(companion-error-no-manuscript companion-ask-add-ids
                 companion-ids-added companion-ids-missing-note
                 companion-debug-buffer-name))
    (should (alist-get key org-scribe-messages-en))
    (should (alist-get key org-scribe-messages-es))))

;;; Storyline data (Phase 6)

(ert-deftest test-companion-model-storyline-gaps-match-the-plot-thread-report ()
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (let* ((snapshot (org-scribe-companion--snapshot root))
           (gaps (plist-get (plist-get snapshot :storyline) :plotGaps))
           (checked 0))
      (should (= org-scribe-plot-thread-gap-threshold
                 (plist-get (plist-get snapshot :storyline) :gapThreshold)))
      (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
        (let ((scenes (org-scribe--get-all-scenes-with-plots)))
          (dolist (thread (org-scribe--get-all-plot-threads))
            (let ((appearances (org-scribe--find-thread-in-scenes (car thread) scenes)))
              (when appearances
                (cl-incf checked)
                (should (equal (org-scribe--calculate-thread-gap appearances scenes)
                               (plist-get gaps (org-scribe-companion--keyword (cadr thread))))))))))
      (should (> checked 0)))))

(ert-deftest test-companion-model-storyline-orphans-match-the-health-report ()
  (require 'org-scribe-health)
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (let* ((snapshot (org-scribe-companion--snapshot root))
           (orphans (plist-get (plist-get snapshot :storyline) :orphans))
           (structure (org-scribe-project-structure))
           (refs (org-scribe--health-collect-referenced-ids
                  (plist-get structure :manuscript-file))))
      (cl-loop for (kind getter) in '((:character org-scribe--get-all-characters)
                                      (:location org-scribe--get-all-locations)
                                      (:plot org-scribe--get-all-plot-threads)
                                      (:plot-point org-scribe--get-all-plot-points))
               do (let* ((expected (sort (copy-sequence (org-scribe--health-find-orphans
                                                        (funcall getter) refs))
                                         #'string<))
                         (names (sort (mapcar
                                       (lambda (id)
                                         (plist-get
                                          (cl-find id (plist-get (plist-get snapshot :entities) kind)
                                                   :key (lambda (e) (plist-get e :id))
                                                   :test #'equal)
                                          :name))
                                       (append (plist-get orphans kind) nil))
                                      #'string<)))
                    (should (equal expected names)))))))

(ert-deftest test-companion-model-storyline-follows-edits ()
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (let ((before (plist-get (org-scribe-companion--snapshot root) :storyline)))
      (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward "^:Plot: .*$" nil t) (replace-match ":Plot:"))))
      (let ((after (plist-get (org-scribe-companion--snapshot root) :storyline)))
        (should-not (equal before after))
        (should-not (plist-get after :plotGaps))))))

;;; Relationships

(ert-deftest test-companion-model-characters-carry-their-relationships ()
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (let* ((snapshot (org-scribe-companion--snapshot root))
           (characters (append (plist-get (plist-get snapshot :entities) :character) nil))
           (alex (cl-find "char-alex-rivera-001" characters
                          :key (lambda (e) (plist-get e :id)) :test #'equal))
           (relationships (append (plist-get alex :relationships) nil)))
      (should (= 4 (length relationships)))
      (should (equal '(:target "char-sam-chen-001" :name "Sam Chen" :type "neighbour")
                     (car relationships)))
      ;; every character has the key, even with none: the page never checks for it
      (should (cl-every (lambda (c) (vectorp (plist-get c :relationships))) characters))
      ;; and the suggestions reach the page
      (should (member "mentor" (append (plist-get snapshot :relationshipTypes) nil))))))

(ert-deftest test-companion-model-relationships-read-the-older-shape ()
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (with-current-buffer (find-file-noselect (expand-file-name "design.org" root))
      (org-with-wide-buffer
       (goto-char (org-find-property "ID" "char-sam-chen-001"))
       (org-entry-put nil "RelationshipsData"
                      "[[id:char-alex-rivera-001][Alex Rivera]](neighbour,7,positive)")))
    (org-scribe-companion--clear-cache)
    (let* ((characters (append (plist-get (plist-get (org-scribe-companion--snapshot root) :entities)
                                          :character)
                               nil))
           (sam (cl-find "char-sam-chen-001" characters
                         :key (lambda (e) (plist-get e :id)) :test #'equal)))
      (should (equal "neighbour"
                     (plist-get (aref (plist-get sam :relationships) 0) :type))))))

(ert-deftest test-companion-model-objects-are-a-linked-field-and-entity-kind ()
  (should (equal 'link-many
                 (plist-get (alist-get 'objects org-scribe-companion--field-kinds) :kind)))
  (should (equal 'object
                 (plist-get (alist-get 'objects org-scribe-companion--field-kinds) :entity)))
  (should (eq 'objects (alist-get 'object org-scribe-companion--entity-sections)))
  (should (assq 'object org-scribe-entity-registry))
  (should (assoc "object" org-scribe-companion-entity--capture-commands)))

;;; Story time

(ert-deftest test-companion-model-snapshot-carries-story-time ()
  "Each scene gets a key from the one Lisp parser; events come from the notebook."
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (let* ((story (plist-get (org-scribe-companion--snapshot root) :storyTime))
           (scenes (append (plist-get story :scenes) nil))
           (first (car scenes)))
      (should (> (length scenes) 3))
      (should (equal "Day 1, 06:30" (plist-get first :text)))
      (should (= (+ 390) (plist-get first :key)))
      ;; Same JSON round trip the browser gets.
      (should (json-serialize (org-scribe-companion--snapshot root)))
      (should (cl-some (lambda (e) (equal "Day 25, 09:00" (plist-get e :text)))
                       (append (plist-get story :events) nil))))))

(ert-deftest test-companion-model-short-story-scenes-pass-through-the-act ()
  "A project with no chapter level hands the page the act as scene parent."
  (test-companion-model--with-project root
      (lambda (root)
        (copy-directory (expand-file-name "examples/demo-short-story"
                                          test-companion-model--repo)
                        root t t t))
    (let* ((snapshot (org-scribe-companion--snapshot root))
           (act (aref (plist-get snapshot :acts) 0))
           (chapter (aref (plist-get act :chapters) 0)))
      (should (equal "short-story" (plist-get (plist-get snapshot :project) :type)))
      (should (eq t (plist-get chapter :passthrough)))
      (should (equal (plist-get act :id) (plist-get chapter :containerId)))
      (should (= 3 (length (plist-get chapter :scenes)))))))

(ert-deftest test-companion-model-novel-chapters-do-not-pass-through ()
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (let ((act (aref (plist-get (org-scribe-companion--snapshot root) :acts) 0)))
      (seq-doseq (chapter (plist-get act :chapters))
        (should-not (plist-member chapter :passthrough))))))

(provide 'test-companion-model)
;;; test-companion-model.el ends here

(ert-deftest test-companion-model-json-is-kept-per-revision ()
  (test-companion-model--with-project root #'test-companion-model--copy-demo
    (let ((first (org-scribe-companion--snapshot-json root))
          (count 0))
      (cl-letf (((symbol-function 'json-serialize)
                 (lambda (&rest _) (cl-incf count) "{}")))
        (should (equal first (org-scribe-companion--snapshot-json root)))
        (should (= 0 count))
        (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
          (goto-char (point-max))
          (insert "\n"))
        (org-scribe-companion--snapshot-json root)
        (should (= 1 count))))))
