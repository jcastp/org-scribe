;;; org-scribe-companion-entity.el --- Entity forms for the companion -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; The design notebook side of the companion: `GET /api/entity' (one
;; entity, on demand) and the commands that edit it.
;;
;; An entity is a heading of the design notebook with an ID.  A form has
;; two kinds of fields:
;;
;;   property fields   the entity heading's property drawer
;;   section fields    the heading's own body (index 0) and the body of each
;;                     sub-heading under it, up to the next heading
;;
;; Commands (see `org-scribe-companion-commands--operations'):
;;
;;   set-entity-property   id, key, value
;;   set-entity-section    id, index, title, hash, body: replaces the body
;;                         only if its hash still matches (else `conflict',
;;                         with the current body in the error)
;;   set-relationship      source, target, type, old: adds, changes or removes
;;                         one entry of a character's RelationshipsData
;;   capture-entity        kind: starts the existing capture in Emacs
;;
;; Heading lines, deeper sub-headings and the ID are never touched.  Plot
;; points are the method's prose, not form data: they are read-only here.

;;; Code:

(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'subr-x)
(require 'org-scribe-messages)
(require 'org-scribe-core)
(require 'org-scribe-companion-model)
(require 'org-scribe-companion-commands)

(defvar org-scribe-companion-raise-frame)
(declare-function org-scribe--get-character-relationships "org-scribe-character-relationships")
(declare-function org-scribe--relationships-to-string "org-scribe-character-relationships")
(declare-function org-scribe--relationships-property-at-point "org-scribe-character-relationships")
(declare-function org-scribe--relationships-property-name "org-scribe-character-relationships")
(declare-function org-scribe-capture-character "org-scribe-capture")
(declare-function org-scribe-capture-location "org-scribe-capture")
(declare-function org-scribe-capture-object "org-scribe-capture")
(declare-function org-scribe-capture-plot-thread "org-scribe-capture")

(defconst org-scribe-companion-entity--capture-commands
  '(("character" . org-scribe-capture-character)
    ("location" . org-scribe-capture-location)
    ("object" . org-scribe-capture-object)
    ("plot" . org-scribe-capture-plot-thread))
  "Entity kind -> the capture command that creates one.")

(defconst org-scribe-companion-entity--protected-properties
  '("ID" "RELATIONSHIPSDATA"
    ;; Org's special properties: writing one through `org-entry-put' does
    ;; more than set a drawer line (SCHEDULED=FINISHED became a timestamp)
    "TODO" "TAGS" "PRIORITY" "CATEGORY" "ARCHIVE" "SCHEDULED" "DEADLINE"
    "CLOSED" "TIMESTAMP" "TIMESTAMP_IA" "CLOCKSUM" "BLOCKED" "ITEM" "FILE" "ALLTAGS")
  "Properties a form never writes (upcased).")

;;; Lookup

(defun org-scribe-companion-entity--find (root id)
  "Return (KIND . ENTITY) for the entity ID in ROOT's snapshot, or nil.
KIND is a string such as \"character\"."
  (let ((entities (plist-get (org-scribe-companion--snapshot root) :entities)))
    (cl-loop for (kind list) on entities by #'cddr
             for entity = (cl-find id (append list nil)
                                   :key (lambda (e) (plist-get e :id)) :test #'equal)
             when entity return (cons (substring (symbol-name kind) 1) entity))))

(defun org-scribe-companion-entity--marker (root id)
  "Return a marker at entity ID in the design notebook of ROOT, or nil."
  (let* ((default-directory root)
         (file (plist-get (org-scribe-project-structure) :design-file)))
    (when (and file (file-exists-p file))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (when-let* ((pos (org-find-property "ID" id)))
           (copy-marker pos)))))))

(defun org-scribe-companion-entity--require (root id)
  "Return (KIND . MARKER) for the editable entity ID, or refuse the command."
  (let ((found (org-scribe-companion-entity--find root id)))
    (unless found
      (org-scribe-companion-commands--fail "not-found" 'companion-error-not-found
                                           (or id "")))
    (when (equal (car found) "plot-point")
      (org-scribe-companion-commands--fail "read-only" 'companion-error-read-only id))
    (let ((marker (org-scribe-companion-entity--marker root id)))
      (unless marker
        (org-scribe-companion-commands--fail "not-found" 'companion-error-not-found id))
      (cons (car found) marker))))

;;; Reading an entity

(defun org-scribe-companion-entity--properties ()
  "Return the property drawer of the heading at point as a vector of plists."
  (let ((range (org-get-property-block))
        props)
    (when range
      (save-excursion
        (goto-char (car range))
        (while (re-search-forward "^[ \t]*:\\([^: \t\n]+\\):[ \t]*\\(.*?\\)[ \t]*$"
                                  (cdr range) t)
          (push (list :name (match-string-no-properties 1)
                      :value (match-string-no-properties 2))
                props))))
    (vconcat (nreverse props))))

(defun org-scribe-companion-entity--body-bounds ()
  "Return (BEG . END) of the body of the heading at point.
The body starts after the heading line and its property drawer and runs
to the next heading of any level."
  (save-excursion
    (org-back-to-heading t)
    (let ((range (org-get-property-block)))
      (if range
          (progn (goto-char (cdr range)) (forward-line 1))
        (end-of-line)
        (forward-line 1)))
    (let ((beg (min (point) (point-max))))
      (goto-char beg)
      (cons beg (progn (or (outline-next-heading) (goto-char (point-max)))
                       (max beg (point)))))))

(defun org-scribe-companion-entity--hash (body)
  "Return a short hash of the string BODY."
  (substring (secure-hash 'sha1 body) 0 16))

(defun org-scribe-companion-entity--section-at-point (index)
  "Return the section plist of the heading at point, numbered INDEX."
  (let* ((bounds (org-scribe-companion-entity--body-bounds))
         (body (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (list :index index
          :level (org-current-level)
          :title (org-get-heading t t t t)
          :body body
          :hash (org-scribe-companion-entity--hash body))))

(defun org-scribe-companion-entity--sections ()
  "Return the sections of the entity at point as a vector.
Index 0 is the entity's own body, then each sub-heading in document order."
  (let ((end (save-excursion (org-end-of-subtree t t) (point)))
        (index 0)
        sections)
    (save-excursion
      (org-back-to-heading t)
      (push (org-scribe-companion-entity--section-at-point index) sections)
      (while (and (outline-next-heading) (< (point) end))
        (cl-incf index)
        (push (org-scribe-companion-entity--section-at-point index) sections)))
    (vconcat (nreverse sections))))

(defun org-scribe-companion-entity--relationships (raw)
  "Return the links in the RelationshipsData value RAW as a vector.
Each is (:id :name :detail): DETAIL is whatever follows the link, as written."
  (let ((pos 0) found)
    (while (string-match "\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]*\\)\\]\\]" raw pos)
      (let* ((id (match-string 1 raw))
             (name (match-string 2 raw))
             (after (match-end 0))
             (next (or (string-match "\\[\\[id:" raw after) (length raw))))
        (push (list :id id :name name
                    :detail (string-trim (substring raw after next) nil "[ \t;]+"))
              found)
        (setq pos next)))
    (vconcat (nreverse found))))

(defun org-scribe-companion-entity--choices (kind)
  "Return a plist of property name to the vector of values its template offers.
Read from the capture prompts (\"Label|first|second\"), in the project's
language, so the form offers what the templates do."
  (let ((org-scribe-message-language (org-scribe-project-language)))
  (cl-flet ((options (key)
              (let ((parts (split-string (org-scribe-msg key) "|")))
                (vconcat (cdr parts)))))
    (pcase kind
      ("character" (list :Role (options (if (eq (org-scribe-project-type) 'short-story)
                                            'capture-ss-char-role-prompt
                                          'capture-char-role-prompt))))
      ("plot" (append (list :THREAD-TYPE (options 'capture-pt-type-prompt))
                      (when (org-scribe-msg 'capture-pt-status-prompt)
                        (list :STATUS (options 'capture-pt-status-prompt)))))
      (_ nil)))))

(defun org-scribe-companion-entity--appears-in (root id)
  "Return the scenes of ROOT that link to the entity ID, as a vector."
  (let (found)
    (dolist (scene (org-scribe-companion--scene-list
                    (plist-get (org-scribe-companion--snapshot root) :acts)))
      (when (cl-some (lambda (key) (member id (org-scribe-companion--field-ids scene key)))
                     '(:pov :characters :location :objects :plot :plot-point))
        (push (list :id (plist-get scene :id) :title (plist-get scene :title)) found)))
    (vconcat (nreverse found))))

(defun org-scribe-companion-entity-get (root id)
  "Return the form data of entity ID in ROOT, or a failure answer."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (default-directory root)
         ;; messages and choices follow the project, not the user's UI language
         (org-scribe-message-language (org-scribe-project-language))
         (found (and (stringp id) (org-scribe-companion-entity--find root id)))
         (marker (and found (org-scribe-companion-entity--marker root id))))
    (if (not marker)
        (org-scribe-companion-commands-error
         "not-found" (org-scribe-msg 'companion-error-not-found (format "%s" id)))
      (unwind-protect
          (with-current-buffer (marker-buffer marker)
            (org-with-wide-buffer
             (goto-char marker)
             (let ((properties (org-scribe-companion-entity--properties)))
               (list :ok t
                     :revision (org-scribe-companion--revision-string root)
                     :id id
                     :kind (car found)
                     :name (plist-get (cdr found) :name)
                     :readOnly (org-scribe-companion--bool (equal (car found) "plot-point"))
                     :properties properties
                     :choices (org-scribe-companion-entity--choices (car found))
                     :sections (org-scribe-companion-entity--sections)
                     :relationships
                     (org-scribe-companion-entity--relationships
                      (or (cl-some (lambda (p)
                                     (and (string-equal-ignore-case
                                           (plist-get p :name) "RelationshipsData")
                                          (plist-get p :value)))
                                   properties)
                          ""))
                     :appearsIn (org-scribe-companion-entity--appears-in root id)))))
        (set-marker marker nil)))))

;;; Commands

(defun org-scribe-companion-entity--editing (root id function)
  "Call FUNCTION with the entity ID's heading marker, as one undo step.
The design notebook of ROOT is edited widened; a failure undoes it whole.
Returns the success answer."
  (let ((marker (cdr (org-scribe-companion-entity--require root id))))
    (unwind-protect
        (progn
          (org-scribe-companion-commands--with-edit (marker-buffer marker)
            (goto-char marker)
            (funcall function marker))
          (org-scribe-companion-commands--finish root))
      (set-marker marker nil))))

;;; set-relationship

(defun org-scribe-companion-entity--character-name (root id)
  "Return the current name of the character ID in ROOT, or refuse the command."
  (let ((found (org-scribe-companion-entity--find root id)))
    (unless found
      (org-scribe-companion-commands--fail "not-found" 'companion-error-not-found (or id "")))
    (unless (equal (car found) "character")
      (org-scribe-companion-commands--fail "invalid" 'companion-error-not-a-character))
    (plist-get (cdr found) :name)))

(defun org-scribe-companion-entity--set-relationship (root args)
  "Add, change or remove one relationship of a character.
ARGS: `:source' (the character it is written on), `:target', `:type' and
`:old'.  Without `:old' the entry (TARGET, TYPE) is added; with `:old' the
entry (TARGET, OLD) becomes TYPE, or is removed when TYPE is empty.  An
entry that is no longer there is a `conflict': the page was out of date.
The link text is the target's current name."
  (let* ((source (org-scribe-companion-commands--require-id args :source))
         (target (org-scribe-companion-commands--require-id args :target))
         (type (or (org-scribe-companion-commands--clean-line
                    (org-scribe-companion-commands--arg args :type))
                   ""))
         (old (org-scribe-companion-commands--clean-line
               (org-scribe-companion-commands--arg args :old))))
    (when (equal source target)
      (org-scribe-companion-commands--fail "invalid" 'companion-error-same-character))
    (when (string-match-p "|\\|\\[\\[" type)
      (org-scribe-companion-commands--fail "invalid" 'companion-error-invalid-relationship-type))
    (when (and (null old) (string-empty-p type))
      (org-scribe-companion-commands--fail "invalid" 'companion-error-missing-arg "type"))
    (org-scribe-companion-entity--character-name root source)
    (let ((name (org-scribe-companion-entity--character-name root target))
          (default-directory root))
      (org-scribe-companion-entity--editing
       root source
       (lambda (_marker)
         (let* ((entries (org-scribe--get-character-relationships))
                (same (lambda (entry kind) (and (equal (nth 0 entry) target)
                                                (equal (nth 2 entry) kind))))
                (new (list target name type))
                result)
           (if (null old)
               (setq result (if (cl-find-if (lambda (e) (funcall same e type)) entries)
                                entries
                              (append entries (list new))))
             (unless (cl-find-if (lambda (e) (funcall same e old)) entries)
               (org-scribe-companion-commands--fail "conflict" 'companion-error-relationship-gone))
             (dolist (entry entries)
               (cond
                ((not (funcall same entry old)) (push entry result))
                ;; changed: unless the new type already exists, which makes it a removal
                ((and (not (string-empty-p type))
                      (not (cl-find-if (lambda (e) (and (not (eq e entry)) (funcall same e type)))
                                       entries)))
                 (push new result))))
             (setq result (nreverse result)))
           (let ((property (or (org-scribe--relationships-property-at-point)
                               (org-scribe--relationships-property-name))))
             (if result
                 (org-entry-put nil property (org-scribe--relationships-to-string result))
               (org-entry-delete nil property)))))))))

(defun org-scribe-companion-entity--set-property (root args)
  "Set a property of an entity.  ARGS: `:id', `:key', `:value' (one line)."
  (let* ((id (org-scribe-companion-commands--require-id args :id))
         (key (org-scribe-companion-commands--arg-string args :key))
         (value (org-scribe-companion-commands--clean-line
                 (org-scribe-companion-commands--arg args :value))))
    (unless (and key (string-match-p "\\`[A-Za-z][A-Za-z0-9_-]*\\'" key))
      (org-scribe-companion-commands--fail "invalid" 'companion-error-invalid-key (or key "")))
    (when (member (upcase key) org-scribe-companion-entity--protected-properties)
      (org-scribe-companion-commands--fail "read-only" 'companion-error-read-only key))
    (unless value
      (org-scribe-companion-commands--fail "invalid" 'companion-error-missing-arg "value"))
    (when (and (string-equal-ignore-case key "Weight")
               (not (string-empty-p value))
               (null (org-scribe--parse-weight value)))
      (org-scribe-companion-commands--fail "invalid" 'companion-error-invalid-weight value))
    (let ((default-directory root))
      (org-scribe-companion-entity--editing
       root id
       (lambda (_marker)
         (let ((current (org-entry-get nil key)))
           (unless (equal value (or current ""))
             (org-entry-put nil key value))))))))

(defun org-scribe-companion-entity--set-section (root args)
  "Replace the body of one section of an entity.
ARGS: `:id', `:index', `:title' (the heading the section has), `:hash' (of
the body the form was showing) and `:body'.  A body that changed meanwhile
is a `conflict' and the error carries the current body."
  (let* ((id (org-scribe-companion-commands--require-id args :id))
         (index (plist-get args :index))
         (title (plist-get args :title))
         (hash (org-scribe-companion-commands--arg-string args :hash))
         (body (plist-get args :body)))
    (unless (and (integerp index) (>= index 0))
      (org-scribe-companion-commands--fail "invalid" 'companion-error-missing-arg "index"))
    (unless (and (stringp body) hash)
      (org-scribe-companion-commands--fail "invalid" 'companion-error-missing-arg "body"))
    (setq body (replace-regexp-in-string "\r\n?" "\n" body))
    (when (string-match-p "^\\*+ " body)
      (org-scribe-companion-commands--fail "invalid" 'companion-error-body-heading))
    (let ((default-directory root)
          (new (if (or (string-empty-p body) (string-suffix-p "\n" body))
                   body
                 (concat body "\n"))))
      ;; The answer carries the body and hash now stored, so the form need not
      ;; wait for a refresh before its next save.
      (append
       (org-scribe-companion-entity--editing
       root id
       (lambda (marker)
         (let* ((sections (org-scribe-companion-entity--sections))
                (section (and (< index (length sections)) (aref sections index))))
           (unless (and section (or (null title) (equal title (plist-get section :title))))
             (org-scribe-companion-commands--fail-with
              "conflict" (list :current :null) 'companion-error-section-gone))
           (unless (equal hash (plist-get section :hash))
             (org-scribe-companion-commands--fail-with
              "conflict" (list :current (plist-get section :body)
                               :hash (plist-get section :hash))
              'companion-error-section-conflict))
           (unless (equal new (plist-get section :body))
             (goto-char marker)
             (dotimes (_ index) (outline-next-heading))
             (let ((bounds (org-scribe-companion-entity--body-bounds)))
               (delete-region (car bounds) (cdr bounds))
               (goto-char (car bounds))
               (insert new))))))
       (list :body new :hash (org-scribe-companion-entity--hash new))))))

(defun org-scribe-companion-entity--capture (root args)
  "Start the capture for a new entity of the kind in ARGS (`:kind') in Emacs.
The capture runs from a timer, so the browser's request is answered at
once and any prompt appears in Emacs, in the selected frame."
  (let* ((kind (org-scribe-companion-commands--arg-string args :kind))
         (command (cdr (assoc kind org-scribe-companion-entity--capture-commands))))
    (unless command
      (org-scribe-companion-commands--fail "invalid" 'companion-error-invalid-key (or kind "")))
    (require 'org-scribe-capture)
    (let ((default-directory root))
      (run-at-time 0 nil (lambda ()
                           (let ((default-directory root))
                             (when org-scribe-companion-raise-frame
                               (select-frame-set-input-focus (selected-frame)))
                             (call-interactively command)))))
    (org-scribe-companion-commands--ok root)))

(provide 'org-scribe-companion-entity)

;;; org-scribe-companion-entity.el ends here
