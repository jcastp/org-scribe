;;; org-scribe-linking-core.el --- Generic entity linking framework -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Generic framework for ID-based entity linking in writing projects.
;; Provides parameterized functions and a macro for generating
;; entity-specific linking APIs (characters, locations, plot threads).
;;
;; Each entity type is described by a plist containing:
;;   :file-fn            - Function returning the entity database file path
;;   :heading-predicate  - Function (no args) returning t if point is on an entity heading
;;   :properties         - List of scene property names this entity uses
;;   :msg-added-ids      - Message key for "Added N IDs"
;;   :msg-ids-updated    - Message key for "IDs updated in file"
;;   :error-no-file      - Message key for "No entity file found"
;;   :error-none-found   - Message key for "No entities found"
;;   :prompt-select      - Message key for single-select prompt
;;   :prompt-select-multi - Message key for multi-select prompt
;;   :error-no-id        - Message key for "No ID found for entity"
;;   :msg-inserted-links - Message key for "Inserted N links"
;;   :msg-no-selected    - Message key for "No entities selected"
;;   :msg-set            - Message key for "Set property to entities"
;;   :msg-updated-single - Message key for "Updated property" (single property)
;;   :msg-no-updates     - Message key for "No updates needed"
;;   :msg-updated-links  - Message key for "Updated links in N scenes"
;;   :msg-setting-up     - Message key for "Setting up..."
;;   :question-link-existing - Message key for "Link existing scenes?"
;;   :msg-setup-complete - Message key for "Setup complete"
;;   :msg-updated-link-names - Message key for "Updated link names"
;;   :msg-no-link-updates-type - String (e.g. "character") for no-link-updates msg
;;   :msg-updated-all-type - String (e.g. "character") for updated-all-link-names msg

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-scribe-core)
(require 'org-scribe-messages)

;;; Shared Helper Functions

(defun org-scribe--entity-name-at-point ()
  "Get entity name from current heading or NAME property."
  (or (org-entry-get nil "NAME")
      (org-get-heading t t t t)))

(defun org-scribe--create-entity-link (name id-alist)
  "Create an ID link for NAME using ID-ALIST.
ID-ALIST should be in format ((NAME . (ID . HEADING)) ...).
Returns the link string or plain text if no ID found."
  (if-let* ((entry (assoc name id-alist))
            (id (cadr entry)))
      (format "[[id:%s][%s]]" id name)
    name))

;;; Generic Entity Functions

(defun org-scribe--get-all-entities (entity)
  "Return alist of (NAME . (ID . HEADING)) from ENTITY database file.
ENTITY is an entity descriptor plist."
  (let ((file (funcall (plist-get entity :file-fn)))
        (pred (plist-get entity :heading-predicate))
        result)
    (when (and file (file-exists-p file))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (let ((id (org-id-get))
                  (name (org-scribe--entity-name-at-point)))
              (when (and id name (funcall pred))
                (push (cons name (cons id (org-get-heading t t t t))) result))))
          nil 'file))))
    (nreverse result)))

(defun org-scribe--get-entity-weight (entity name)
  "Get the Weight property for entity NAME from its database file.
ENTITY is an entity descriptor plist.
Returns the weight as a float, or 999.0 if not found."
  (let ((file (funcall (plist-get entity :file-fn)))
        (pred (plist-get entity :heading-predicate))
        (weight 999.0))
    (when (and file (file-exists-p file))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (catch 'found
           (org-map-entries
            (lambda ()
              (when (and (funcall pred)
                         (string= (org-scribe--entity-name-at-point) name))
                (when-let ((weight-str (org-entry-get nil "Weight")))
                  (setq weight (string-to-number weight-str)))
                (throw 'found t)))
            nil 'file)))))
    weight))

(defun org-scribe--add-ids-to-all-entities (entity)
  "Add IDs to all entity headings in current buffer.
ENTITY is an entity descriptor plist."
  (save-excursion
    (goto-char (point-min))
    (let ((count 0)
          (pred (plist-get entity :heading-predicate)))
      (org-map-entries
       (lambda ()
         (when (funcall pred)
           (unless (org-id-get)
             (org-id-get-create)
             (setq count (1+ count)))))
       nil 'file)
      (message (org-scribe-msg (plist-get entity :msg-added-ids)
                               count (org-scribe-plural count ""))))))

(defun org-scribe--add-entity-ids (entity)
  "Add unique IDs to all entities in the database file.
ENTITY is an entity descriptor plist."
  (let ((file (funcall (plist-get entity :file-fn))))
    (if (not (file-exists-p file))
        (message (org-scribe-msg (plist-get entity :error-no-file)))
      (with-current-buffer (find-file-noselect file)
        (org-scribe--add-ids-to-all-entities entity)
        (save-buffer)
        (message (org-scribe-msg (plist-get entity :msg-ids-updated) file))))))

(defun org-scribe--insert-entity-link (entity)
  "Insert an entity link at point with completion.
ENTITY is an entity descriptor plist."
  (let* ((items (org-scribe--get-all-entities entity))
         (names (mapcar #'car items)))
    (if (null names)
        (message (org-scribe-msg (plist-get entity :error-none-found)))
      (let* ((selected (completing-read
                        (org-scribe-msg (plist-get entity :prompt-select))
                        names nil t))
             (entry (assoc selected items))
             (id (cadr entry)))
        (if id
            (progn
              (insert (format "[[id:%s][%s]]" id selected))
              (message (org-scribe-msg 'msg-inserted-link selected)))
          (message (org-scribe-msg (plist-get entity :error-no-id) selected)))))))

(defun org-scribe--insert-multiple-entity-links (entity)
  "Insert multiple entity links separated by commas.
ENTITY is an entity descriptor plist."
  (let* ((items (org-scribe--get-all-entities entity))
         (names (mapcar #'car items))
         selected-items
         links)
    (if (null names)
        (message (org-scribe-msg (plist-get entity :error-none-found)))
      (while (let ((choice (completing-read
                            (org-scribe-msg (plist-get entity :prompt-select-multi))
                            names nil nil)))
               (when (and choice (not (string-empty-p choice)))
                 (push choice selected-items)
                 t)))
      (setq selected-items (nreverse selected-items))
      (dolist (name selected-items)
        (if-let* ((entry (assoc name items))
                  (id (cadr entry)))
            (push (format "[[id:%s][%s]]" id name) links)
          (push name links)))
      (setq links (nreverse links))
      (if links
          (progn
            (insert (string-join links ", "))
            (message (org-scribe-msg (plist-get entity :msg-inserted-links)
                                     (length links)
                                     (org-scribe-plural (length links) ""))))
        (message (org-scribe-msg (plist-get entity :msg-no-selected)))))))

(defun org-scribe--set-scene-entity (entity property)
  "Set PROPERTY to multiple entity ID links via completion.
ENTITY is an entity descriptor plist.
PROPERTY is the org property name to set (e.g. \"Characters\")."
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (let* ((items (org-scribe--get-all-entities entity))
         (names (mapcar #'car items))
         selected-items
         links)
    (if (null names)
        (message (org-scribe-msg (plist-get entity :error-none-found)))
      (while (let ((choice (completing-read
                            (org-scribe-msg (plist-get entity :prompt-select-multi))
                            names nil nil)))
               (when (and choice (not (string-empty-p choice)))
                 (push choice selected-items)
                 t)))
      (setq selected-items (nreverse selected-items))
      (dolist (name selected-items)
        (if-let* ((entry (assoc name items))
                  (id (cadr entry)))
            (push (format "[[id:%s][%s]]" id name) links)
          (push name links)))
      (setq links (nreverse links))
      (if links
          (progn
            (org-set-property property (string-join links ", "))
            (message (org-scribe-msg (plist-get entity :msg-set)
                                     (string-join selected-items ", "))))
        (message (org-scribe-msg (plist-get entity :msg-no-selected)))))))

(defun org-scribe--link-entity-in-property (entity property)
  "Convert entity names to ID links in PROPERTY of current heading.
ENTITY is an entity descriptor plist.
Handles both single entities and comma-separated lists."
  (when-let ((prop-value (org-entry-get nil property)))
    (let* ((id-alist (org-scribe--get-all-entities entity))
           (name-list (mapcar #'string-trim
                              (split-string prop-value "," t)))
           (linked (mapcar (lambda (name)
                             (org-scribe--create-entity-link name id-alist))
                           name-list))
           (linked-string (string-join linked ", ")))
      (unless (string= prop-value linked-string)
        (org-set-property property linked-string)
        t))))

(defun org-scribe--link-scene-entity (entity)
  "Convert entity names to ID links in current scene.
ENTITY is an entity descriptor plist.
Uses the first property in the entity's :properties list."
  (save-excursion
    (org-back-to-heading)
    (let* ((property (car (plist-get entity :properties)))
           (updated (org-scribe--link-entity-in-property entity property)))
      (cond
       (updated
        (message (org-scribe-msg (plist-get entity :msg-updated-single))))
       (t
        (message (org-scribe-msg (plist-get entity :msg-no-updates))))))))

(defun org-scribe--link-all-scene-entities (entity)
  "Convert entity names to ID links in all scenes in current buffer.
ENTITY is an entity descriptor plist."
  (save-excursion
    (goto-char (point-min))
    (let ((count 0)
          (properties (plist-get entity :properties)))
      (org-map-entries
       (lambda ()
         (when (cl-some (lambda (prop) (org-entry-get nil prop)) properties)
           (let ((any-updated nil))
             (dolist (prop properties)
               (when (org-scribe--link-entity-in-property entity prop)
                 (setq any-updated t)))
             (when any-updated
               (setq count (1+ count))))))
       nil 'file)
      (message (org-scribe-msg (plist-get entity :msg-updated-links)
                               count (org-scribe-plural count ""))))))

(defun org-scribe--setup-entity-links (entity add-ids-fn link-all-fn)
  "Set up entity linking system for current project.
ENTITY is an entity descriptor plist.
ADD-IDS-FN is the function to call to add IDs.
LINK-ALL-FN is the function to call to link all scenes."
  (message (org-scribe-msg (plist-get entity :msg-setting-up)))
  (funcall add-ids-fn)
  (when (y-or-n-p (org-scribe-msg (plist-get entity :question-link-existing)))
    (let ((novel-file (plist-get (org-scribe-project-structure) :novel-file)))
      (when (and novel-file (file-exists-p novel-file))
        (with-current-buffer (find-file-noselect novel-file)
          (funcall link-all-fn)
          (save-buffer)))))
  (message (org-scribe-msg (plist-get entity :msg-setup-complete))))

(defun org-scribe--update-entity-link-names (entity)
  "Update entity link display names in current scene.
ENTITY is an entity descriptor plist.
Returns t if any updates were made."
  (require 'org-scribe-link-update)
  (save-excursion
    (org-back-to-heading)
    (let* ((items-alist (org-scribe--get-all-entities entity))
           (id-map (org-scribe--build-id-to-name-map items-alist))
           (properties (plist-get entity :properties))
           (any-updated nil))
      (dolist (prop properties)
        (when (org-scribe--update-links-in-property prop id-map)
          (setq any-updated t)))
      (if any-updated
          (message (org-scribe-msg (plist-get entity :msg-updated-link-names)))
        (message (org-scribe-msg 'msg-no-link-updates
                                 (plist-get entity :msg-no-link-updates-type))))
      any-updated)))

(defun org-scribe--update-all-entity-link-names (entity)
  "Update entity link display names in all scenes.
ENTITY is an entity descriptor plist.
Returns the number of scenes updated."
  (require 'org-scribe-link-update)
  (save-excursion
    (goto-char (point-min))
    (let* ((items-alist (org-scribe--get-all-entities entity))
           (id-map (org-scribe--build-id-to-name-map items-alist))
           (properties (plist-get entity :properties))
           (count 0))
      (org-map-entries
       (lambda ()
         (when (cl-some (lambda (prop) (org-entry-get nil prop)) properties)
           (let ((any-updated nil))
             (dolist (prop properties)
               (when (org-scribe--update-links-in-property prop id-map)
                 (setq any-updated t)))
             (when any-updated
               (setq count (1+ count))))))
       nil 'file)
      (message (org-scribe-msg 'msg-updated-all-link-names
                               (plist-get entity :msg-updated-all-type)
                               count (org-scribe-plural count "")))
      count)))

;;; Timeline Utilities

(defun org-scribe--get-all-scenes (data-fn)
  "Return list of scene data from the novel file.
DATA-FN is called at each level-3 heading.  It should return scene data
\(a list starting with heading and chapter) or nil to skip the heading."
  (let ((novel-file (plist-get (org-scribe-project-structure) :novel-file))
        scenes)
    (when (and novel-file (file-exists-p novel-file))
      (with-current-buffer (find-file-noselect novel-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (when (= (org-current-level) 3)
              (when-let ((data (funcall data-fn)))
                (push data scenes))))
          nil 'file))))
    (nreverse scenes)))

(defun org-scribe--sort-entities-by-weight (items weight-fn)
  "Return ITEMS sorted by weight, then alphabetically.
WEIGHT-FN takes a name string and returns its weight (float; lower = earlier)."
  (let ((weighted (mapcar (lambda (name) (cons name (funcall weight-fn name)))
                          items)))
    (mapcar #'car
            (sort weighted
                  (lambda (a b)
                    (if (= (cdr a) (cdr b))
                        (string< (car a) (car b))
                      (< (cdr a) (cdr b))))))))

(defun org-scribe--render-presence-table (entities scenes cell-fn)
  "Insert an org table showing ENTITIES across SCENES.
ENTITIES is the list of column header strings.
SCENES is a list where each entry is (heading chapter ...).
CELL-FN is called with (entity-name scene) and should return the cell string."
  (insert "| Scene | Chapter |")
  (dolist (entity entities)
    (insert (format " %s |" entity)))
  (insert "\n|-------+---------+")
  (dolist (_ entities)
    (insert "--------+"))
  (insert "\n")
  (dolist (scene scenes)
    (insert (format "| %s | %s |" (nth 0 scene) (nth 1 scene)))
    (dolist (entity entities)
      (insert (format " %s |" (funcall cell-fn entity scene))))
    (insert "\n"))
  (org-table-align))

;;; Entity Registry

(defvar org-scribe-entity-registry '()
  "Alist of (entity-symbol . config-plist) for all registered entity types.
Populated automatically by `org-scribe-define-entity'.
Keys are bare symbols (e.g. \\='character, \\='location, \\='plot).
Values are the entity config plists (without function-name directives).")

;;; Entity Definition Macro

(defmacro org-scribe-define-entity (entity-symbol &rest keys)
  "Register an entity type and generate its named API functions.

ENTITY-SYMBOL is an unquoted bare symbol (e.g. character, location, plot).
A defconst named org-scribe--SYMBOL-entity is created from the config keys
and ENTITY-SYMBOL is registered in `org-scribe-entity-registry'.

KEYS are keyword/value pairs in two groups:

Config keys — stored in the entity descriptor constant and registry:
  :file-fn, :heading-predicate, :properties,
  :msg-added-ids, :msg-ids-updated, :error-no-file, :error-none-found,
  :prompt-select, :prompt-select-multi, :error-no-id, :msg-inserted-links,
  :msg-no-selected, :msg-set, :msg-updated-single, :msg-no-updates,
  :msg-updated-links, :msg-setting-up, :question-link-existing,
  :msg-setup-complete, :msg-updated-link-names, :msg-no-link-updates-type,
  :msg-updated-all-type

Function-name keys — consumed at expansion time; each generates one function:
  :get-file-name, :get-all-name, :create-link-name, :add-ids-to-all-name,
  :add-ids-name, :insert-link-name, :insert-multi-name, :set-scene-name,
  :set-scene-property, :link-in-prop-name, :link-scene-name, :link-all-name,
  :setup-name, :setup-add-ids-fn, :setup-link-all-fn, :update-names-name,
  :update-all-name

Omit a function-name key to skip generating that function."
  (let* ((entity-var (intern (format "org-scribe--%s-entity"
                                     (symbol-name entity-symbol))))
         ;; Auxiliary keys consumed at expansion time that are not themselves
         ;; function-name directives (referenced by generated bodies/guards).
         (set-scene-property (plist-get keys :set-scene-property))
         (setup-add-ids-fn   (plist-get keys :setup-add-ids-fn))
         (setup-link-all-fn  (plist-get keys :setup-link-all-fn))
         ;; Generation table: one row per function the macro can emit.  Each row
         ;; is (:key KEYWORD :args ARGLIST [:command t] :doc STRING :body FORM
         ;; [:when GUARD]).  A function is emitted only when its :key is supplied
         ;; in KEYS (and any :when guard is non-nil).  Bodies forward to the
         ;; generic engine functions above, passing the entity descriptor.
         (specs
          (list
           (list :key :get-file-name :args '()
                 :doc "Get the path to the entity file."
                 :body `(funcall (plist-get ,entity-var :file-fn)))
           (list :key :get-all-name :args '()
                 :doc "Return alist of (NAME . (ID . HEADING)) from entity file."
                 :body `(org-scribe--get-all-entities ,entity-var))
           (list :key :create-link-name :args '(name id-alist)
                 :doc "Create an ID link for NAME using ID-ALIST."
                 :body `(org-scribe--create-entity-link name id-alist))
           (list :key :add-ids-to-all-name :args '() :command t
                 :doc "Add IDs to all entity headings in current buffer."
                 :body `(org-scribe--add-ids-to-all-entities ,entity-var))
           (list :key :add-ids-name :args '() :command t
                 :doc "Add unique IDs to all entities in the database file."
                 :body `(org-scribe--add-entity-ids ,entity-var))
           (list :key :insert-link-name :args '() :command t
                 :doc "Insert an entity link at point with completion."
                 :body `(org-scribe--insert-entity-link ,entity-var))
           (list :key :insert-multi-name :args '() :command t
                 :doc "Insert multiple entity links separated by commas."
                 :body `(org-scribe--insert-multiple-entity-links ,entity-var))
           (list :key :set-scene-name :args '() :command t
                 :when set-scene-property
                 :doc "Set scene property to multiple entity ID links."
                 :body `(org-scribe--set-scene-entity ,entity-var ,set-scene-property))
           (list :key :link-in-prop-name :args '(property-name)
                 :doc "Convert entity names to ID links in PROPERTY-NAME."
                 :body `(org-scribe--link-entity-in-property ,entity-var property-name))
           (list :key :link-scene-name :args '() :command t
                 :doc "Convert entity names to ID links in current scene."
                 :body `(org-scribe--link-scene-entity ,entity-var))
           (list :key :link-all-name :args '() :command t
                 :doc "Convert entity names to ID links in all scenes."
                 :body `(org-scribe--link-all-scene-entities ,entity-var))
           (list :key :setup-name :args '() :command t
                 :when (and setup-add-ids-fn setup-link-all-fn)
                 :doc "Set up entity linking system for current project."
                 :body `(org-scribe--setup-entity-links
                         ,entity-var #',setup-add-ids-fn #',setup-link-all-fn))
           (list :key :update-names-name :args '() :command t
                 :doc "Update entity link display names in current scene."
                 :body `(org-scribe--update-entity-link-names ,entity-var))
           (list :key :update-all-name :args '() :command t
                 :doc "Update entity link display names in all scenes."
                 :body `(org-scribe--update-all-entity-link-names ,entity-var))))
         ;; Keys consumed only at expansion time — derived from SPECS (plus the
         ;; auxiliary keys) so the filter can never drift from what is emitted.
         (fn-key-list (append (mapcar (lambda (s) (plist-get s :key)) specs)
                              '(:set-scene-property :setup-add-ids-fn
                                :setup-link-all-fn)))
         ;; Build config-only plist by filtering out expansion-time keys.
         (config-plist
          (let ((result nil) (rest keys))
            (while rest
              (let ((k (car rest)) (v (cadr rest)))
                (unless (memq k fn-key-list)
                  (setq result (append result (list k v))))
                (setq rest (cddr rest))))
            result))
         (forms nil))

    ;; 1. Define entity config as a named constant (config keys only).
    (push `(defconst ,entity-var
             ',config-plist
             ,(format "Entity descriptor for %s. Auto-generated by `org-scribe-define-entity'."
                      entity-symbol))
          forms)

    ;; 2. Register entity in the global registry (replacing any prior entry).
    (push `(setq org-scribe-entity-registry
                 (cons (cons ',entity-symbol ,entity-var)
                       (assq-delete-all ',entity-symbol org-scribe-entity-registry)))
          forms)

    ;; 3. Emit each requested API function from the generation table.  A row is
    ;;    materialised into a defun only when its :key was supplied in KEYS and
    ;;    its optional :when guard holds.
    (dolist (spec specs)
      (let ((name  (plist-get keys (plist-get spec :key)))
            (guard (if (plist-member spec :when) (plist-get spec :when) t)))
        (when (and name guard)
          (push `(defun ,name ,(plist-get spec :args)
                   ,(format "%s\nAuto-generated from %s."
                            (plist-get spec :doc) entity-var)
                   ,@(when (plist-get spec :command) '((interactive)))
                   ,(plist-get spec :body))
                forms))))

    `(progn ,@(nreverse forms))))

(provide 'org-scribe-linking-core)

;;; org-scribe-linking-core.el ends here
