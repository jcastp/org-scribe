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

;;; Entity Definition Macro

(defmacro org-scribe-define-entity (entity-var &rest keys)
  "Generate linking functions for an entity type.

ENTITY-VAR is the symbol of the entity descriptor defconst.

KEYS are keyword-value pairs specifying function names to generate:
  :get-file-name        - Private function returning the entity file path
  :get-all-name         - Private function returning entity alist
  :create-link-name     - Private function creating a link string
  :add-ids-to-all-name  - Private/interactive function adding IDs in buffer
  :add-ids-name         - Public interactive function adding IDs to file
  :insert-link-name     - Public interactive function inserting a link
  :insert-multi-name    - Public interactive function inserting multiple links
  :set-scene-name       - Public interactive function setting scene property
  :set-scene-property   - Property name string for set-scene (e.g. \"Characters\")
  :link-in-prop-name    - Private function converting names to links in property
  :link-scene-name      - Public interactive function linking current scene
  :link-all-name        - Public interactive function linking all scenes
  :setup-name           - Public interactive function for initial setup
  :setup-add-ids-fn     - Symbol of add-ids function to call from setup
  :setup-link-all-fn    - Symbol of link-all function to call from setup
  :update-names-name    - Public interactive function updating link names
  :update-all-name      - Public interactive function updating all link names

Omit a keyword to skip generating that function."
  (let ((get-file-name (plist-get keys :get-file-name))
        (get-all-name (plist-get keys :get-all-name))
        (create-link-name (plist-get keys :create-link-name))
        (add-ids-to-all-name (plist-get keys :add-ids-to-all-name))
        (add-ids-name (plist-get keys :add-ids-name))
        (insert-link-name (plist-get keys :insert-link-name))
        (insert-multi-name (plist-get keys :insert-multi-name))
        (set-scene-name (plist-get keys :set-scene-name))
        (set-scene-property (plist-get keys :set-scene-property))
        (link-in-prop-name (plist-get keys :link-in-prop-name))
        (link-scene-name (plist-get keys :link-scene-name))
        (link-all-name (plist-get keys :link-all-name))
        (setup-name (plist-get keys :setup-name))
        (setup-add-ids-fn (plist-get keys :setup-add-ids-fn))
        (setup-link-all-fn (plist-get keys :setup-link-all-fn))
        (update-names-name (plist-get keys :update-names-name))
        (update-all-name (plist-get keys :update-all-name))
        (forms nil))

    (when get-file-name
      (push `(defun ,get-file-name ()
               ,(format "Get the path to the entity file.\nAuto-generated wrapper for %s." entity-var)
               (funcall (plist-get ,entity-var :file-fn)))
            forms))

    (when get-all-name
      (push `(defun ,get-all-name ()
               ,(format "Return alist of (NAME . (ID . HEADING)) from entity file.\nAuto-generated from %s." entity-var)
               (org-scribe--get-all-entities ,entity-var))
            forms))

    (when create-link-name
      (push `(defun ,create-link-name (name id-alist)
               ,(format "Create an ID link for NAME using ID-ALIST.\nAuto-generated from %s." entity-var)
               (org-scribe--create-entity-link name id-alist))
            forms))

    (when add-ids-to-all-name
      (push `(defun ,add-ids-to-all-name ()
               ,(format "Add IDs to all entity headings in current buffer.\nAuto-generated from %s." entity-var)
               (interactive)
               (org-scribe--add-ids-to-all-entities ,entity-var))
            forms))

    (when add-ids-name
      (push `(progn
               ;;;###autoload
               (defun ,add-ids-name ()
                 ,(format "Add unique IDs to all entities in the database file.\nAuto-generated from %s." entity-var)
                 (interactive)
                 (org-scribe--add-entity-ids ,entity-var)))
            forms))

    (when insert-link-name
      (push `(progn
               ;;;###autoload
               (defun ,insert-link-name ()
                 ,(format "Insert an entity link at point with completion.\nAuto-generated from %s." entity-var)
                 (interactive)
                 (org-scribe--insert-entity-link ,entity-var)))
            forms))

    (when insert-multi-name
      (push `(progn
               ;;;###autoload
               (defun ,insert-multi-name ()
                 ,(format "Insert multiple entity links separated by commas.\nAuto-generated from %s." entity-var)
                 (interactive)
                 (org-scribe--insert-multiple-entity-links ,entity-var)))
            forms))

    (when (and set-scene-name set-scene-property)
      (push `(progn
               ;;;###autoload
               (defun ,set-scene-name ()
                 ,(format "Set scene property to multiple entity ID links.\nAuto-generated from %s." entity-var)
                 (interactive)
                 (org-scribe--set-scene-entity ,entity-var ,set-scene-property)))
            forms))

    (when link-in-prop-name
      (push `(defun ,link-in-prop-name (property-name)
               ,(format "Convert entity names to ID links in PROPERTY-NAME.\nAuto-generated from %s." entity-var)
               (org-scribe--link-entity-in-property ,entity-var property-name))
            forms))

    (when link-scene-name
      (push `(progn
               ;;;###autoload
               (defun ,link-scene-name ()
                 ,(format "Convert entity names to ID links in current scene.\nAuto-generated from %s." entity-var)
                 (interactive)
                 (org-scribe--link-scene-entity ,entity-var)))
            forms))

    (when link-all-name
      (push `(progn
               ;;;###autoload
               (defun ,link-all-name ()
                 ,(format "Convert entity names to ID links in all scenes.\nAuto-generated from %s." entity-var)
                 (interactive)
                 (org-scribe--link-all-scene-entities ,entity-var)))
            forms))

    (when (and setup-name setup-add-ids-fn setup-link-all-fn)
      (push `(progn
               ;;;###autoload
               (defun ,setup-name ()
                 ,(format "Set up entity linking system for current project.\nAuto-generated from %s." entity-var)
                 (interactive)
                 (org-scribe--setup-entity-links ,entity-var
                                                 #',setup-add-ids-fn
                                                 #',setup-link-all-fn)))
            forms))

    (when update-names-name
      (push `(progn
               ;;;###autoload
               (defun ,update-names-name ()
                 ,(format "Update entity link display names in current scene.\nAuto-generated from %s." entity-var)
                 (interactive)
                 (org-scribe--update-entity-link-names ,entity-var)))
            forms))

    (when update-all-name
      (push `(progn
               ;;;###autoload
               (defun ,update-all-name ()
                 ,(format "Update entity link display names in all scenes.\nAuto-generated from %s." entity-var)
                 (interactive)
                 (org-scribe--update-all-entity-link-names ,entity-var)))
            forms))

    `(progn ,@(nreverse forms))))

(provide 'org-scribe-linking-core)

;;; org-scribe-linking-core.el ends here
