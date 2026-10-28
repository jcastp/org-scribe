;;; org-scribe-companion-commands.el --- Visual companion: write operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; The operations the browser can ask for through POST /api/command:
;;   goto, save                         show a heading / save the buffers
;;   move-node, set-title, set-todo,
;;   set-field, create-scene,
;;   create-chapter                     edit the manuscript
;;   set-color                          the page's theme and pinned colours
;;                                      (written to .org-scribe-project)
;; Any other name is `unsupported'.  (Entity edits arrive in a later phase.)
;;
;; Headings are found by ID, only in the project's manuscript and design
;; files, never by position.  Every editing command
;;   - validates before it touches the buffer;
;;   - runs in the wide buffer (a narrowing in the user's window does not
;;     hide the target) and as one undo step, undone whole if it fails;
;;   - refuses `noexport' apparatus and anything outside the manuscript;
;;   - saves nothing, unless `org-scribe-companion-auto-save' is on.
;;
;; A command's answer is a plist: (:ok t :revision REV ...) or
;; (:ok :false :error (:code CODE :message TEXT)), ready for `json-serialize'.
;; Error codes: not-found, invalid, conflict, read-only, unsupported.

;;; Code:

(require 'org)
(require 'org-scribe-messages)
(require 'org-scribe-core)
(require 'org-scribe-companion-model)
(require 'org-scribe-companion-colors)

(defvar org-scribe-companion-raise-frame)
(defvar org-scribe-companion-auto-save)
(defvar org-todo-log-states)
(declare-function org-scribe--scene-template "org-scribe-project")
(declare-function org-scribe--chapter-template "org-scribe-project")
(declare-function org-scribe--refile-invalidate-cache "org-scribe-core")
(declare-function org-scribe-companion--announce-change "org-scribe-companion")

(defconst org-scribe-companion-commands--operations
  '(("goto" . org-scribe-companion-commands--goto)
    ("save" . org-scribe-companion-commands--save)
    ("move-node" . org-scribe-companion-commands--move-node)
    ("set-title" . org-scribe-companion-commands--set-title)
    ("set-todo" . org-scribe-companion-commands--set-todo)
    ("set-field" . org-scribe-companion-commands--set-field)
    ("create-scene" . org-scribe-companion-commands--create-scene)
    ("create-chapter" . org-scribe-companion-commands--create-chapter)
    ("set-color" . org-scribe-companion-commands--set-color)
    ("set-entity-property" . org-scribe-companion-entity--set-property)
    ("set-entity-section" . org-scribe-companion-entity--set-section)
    ("set-relationship" . org-scribe-companion-entity--set-relationship)
    ("capture-entity" . org-scribe-companion-entity--capture))
  "Alist of (OP-NAME . FUNCTION).  FUNCTION takes ROOT and the args plist.")

(define-error 'org-scribe-companion-command-error
  "Companion command refused" 'user-error)

(defun org-scribe-companion-commands--fail (code key &rest args)
  "Refuse the command with error CODE and the message KEY formatted with ARGS."
  (signal 'org-scribe-companion-command-error
          (list code (apply #'org-scribe-msg key args))))

(defun org-scribe-companion-commands--fail-with (code extra key &rest args)
  "Like `--fail', and the error answer also carries the plist EXTRA."
  (signal 'org-scribe-companion-command-error
          (list code (apply #'org-scribe-msg key args) extra)))

(defun org-scribe-companion-commands-error (code message &optional extra)
  "Return the failure answer with error CODE (a string) and MESSAGE.
EXTRA, a plist, is added to the error."
  (list :ok :false :error (append (list :code code :message message) extra)))

(defun org-scribe-companion-commands--project-buffers (root)
  "Return the live buffers visiting ROOT's manuscript and design files."
  (let* ((default-directory (file-name-as-directory root))
         (structure (org-scribe-project-structure)))
    (delq nil (mapcar (lambda (file) (and file (find-buffer-visiting file)))
                      (list (plist-get structure :manuscript-file)
                            (plist-get structure :design-file))))))

(defun org-scribe-companion-commands--find (root id)
  "Return a marker at the heading with ID in ROOT's project files, or nil.
Searches the manuscript and design buffers only (opening them if
needed), ignoring any narrowing."
  (let* ((default-directory (file-name-as-directory root))
         (structure (org-scribe-project-structure)))
    (cl-some
     (lambda (file)
       (when (and file (file-exists-p file))
         (with-current-buffer (find-file-noselect file)
           (org-with-wide-buffer
            (when-let* ((pos (org-find-property "ID" id)))
              (copy-marker pos))))))
     (list (plist-get structure :manuscript-file)
           (plist-get structure :design-file)))))

(defun org-scribe-companion-commands--ok (root)
  "Return the success answer for the project at ROOT."
  (list :ok t :revision (org-scribe-companion--revision-string root)))

(defun org-scribe-companion-commands--arg-string (args key)
  "Return the string argument KEY of the plist ARGS, or nil."
  (let ((value (and (listp args) (plist-get args key))))
    (and (stringp value) (not (string-empty-p value)) value)))

(defun org-scribe-companion-commands--goto (root args)
  "Show the heading ARGS names in Emacs, in the project at ROOT.
ARGS has `:id'.  Widens the buffer if the heading is outside a narrowing,
reveals it, and raises the frame when `org-scribe-companion-raise-frame'."
  (if-let* ((id (org-scribe-companion-commands--arg-string args :id)))
      (if-let* ((marker (org-scribe-companion-commands--find root id)))
          (progn
            (pop-to-buffer (marker-buffer marker))
            (when (or (< marker (point-min)) (> marker (point-max))) (widen))
            (goto-char marker)
            (set-marker marker nil)
            (org-fold-show-context 'link-search)
            (org-fold-show-entry)
            (recenter)
            (when org-scribe-companion-raise-frame
              (select-frame-set-input-focus (selected-frame)))
            (org-scribe-companion-commands--ok root))
        (org-scribe-companion-commands-error
         "not-found" (org-scribe-msg 'companion-error-not-found id)))
    (org-scribe-companion-commands-error
     "invalid" (org-scribe-msg 'companion-error-missing-arg "id"))))

(defun org-scribe-companion-commands--save (root _args)
  "Save the manuscript and design buffers of the project at ROOT, if modified."
  (dolist (buffer (org-scribe-companion-commands--project-buffers root))
    (with-current-buffer buffer
      (when (buffer-modified-p) (save-buffer))))
  (org-scribe-companion-commands--ok root))

;;; Editing machinery

(defmacro org-scribe-companion-commands--in-wide (buffer &rest body)
  "Run BODY in BUFFER, ignoring any narrowing."
  (declare (indent 1))
  `(with-current-buffer ,buffer (org-with-wide-buffer ,@body)))

(defmacro org-scribe-companion-commands--at (marker &rest body)
  "Run BODY with point at MARKER, in its buffer, ignoring any narrowing."
  (declare (indent 1))
  `(org-scribe-companion-commands--in-wide (marker-buffer ,marker)
     (goto-char ,marker)
     ,@body))

(defun org-scribe-companion-commands--manuscript-buffer (root)
  "Return the buffer visiting the manuscript of the project at ROOT."
  (let* ((default-directory root)
         (file (plist-get (org-scribe-project-structure) :manuscript-file)))
    (if (and file (file-exists-p file))
        (find-file-noselect file)
      (org-scribe-companion-commands--fail "not-found" 'companion-error-no-manuscript))))

(defmacro org-scribe-companion-commands--with-edit (buffer &rest body)
  "Run BODY in BUFFER, widened, as one undo step that is undone if BODY fails."
  (declare (indent 1))
  `(let ((handle (prepare-change-group ,buffer))
         (undo-outer-limit nil)
         (undo-limit most-positive-fixnum)
         (undo-strong-limit most-positive-fixnum)
         (success nil))
     (unwind-protect
         (with-current-buffer ,buffer
           (activate-change-group handle)
           (prog1 (org-with-wide-buffer ,@body)
             (setq success t)))
       (if success
           (progn (accept-change-group handle)
                  (undo-amalgamate-change-group handle))
         (cancel-change-group handle)))))

(defun org-scribe-companion-commands--arg (args key)
  "Return the value of KEY in the plist ARGS, or nil."
  (and (listp args) (plist-get args key)))

(defun org-scribe-companion-commands--require-id (args key)
  "Return the ID string under KEY in ARGS, or refuse as invalid."
  (or (org-scribe-companion-commands--arg-string args key)
      (org-scribe-companion-commands--fail
       "invalid" 'companion-error-missing-arg (substring (symbol-name key) 1))))

(defun org-scribe-companion-commands--heading (root buffer id)
  "Return a marker at the editable heading ID in BUFFER, the manuscript of ROOT.
Refuses with `not-found' when the project has no such heading, and with
`read-only' when it is in the design notebook or is `noexport' apparatus."
  (let ((marker (org-scribe-companion-commands--in-wide buffer
                  (when-let* ((pos (org-find-property "ID" id)))
                    (copy-marker pos)))))
    (cond
     (marker
      (when (org-scribe-companion-commands--at marker (member "noexport" (org-get-tags)))
        (set-marker marker nil)
        (org-scribe-companion-commands--fail "read-only" 'companion-error-read-only id))
      marker)
     ((org-scribe-companion-commands--find root id)
      (org-scribe-companion-commands--fail "read-only" 'companion-error-read-only id))
     (t (org-scribe-companion-commands--fail "not-found" 'companion-error-not-found id)))))

(defun org-scribe-companion-commands--level (marker)
  "Return the outline level of the heading at MARKER."
  (org-scribe-companion-commands--at marker (org-current-level)))

(defun org-scribe-companion-commands--finish (root)
  "Return the success answer for ROOT, saving first when `auto-save' is on."
  (when org-scribe-companion-auto-save
    (org-scribe-companion-commands--save root nil))
  (org-scribe-companion-commands--ok root))

(defun org-scribe-companion-commands--clean-line (value)
  "Return VALUE, a string, as one trimmed line, or nil when it is not a string.
Runs of whitespace, newlines included, become single spaces: a property
or a heading holds one line."
  (and (stringp value)
       (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " value))))

;;; Where a heading goes

(defun org-scribe-companion-commands--first-act-position ()
  "Return the position before the first act (non-`noexport' level-1 heading).
The end of the buffer when there is none.  Point moves."
  (goto-char (point-min))
  (catch 'found
    (while (re-search-forward "^\\* " nil t)
      (unless (member "noexport" (org-get-tags))
        (throw 'found (line-beginning-position))))
    (point-max)))

(defun org-scribe-companion-commands--insertion-point (parent after)
  "Return the position (a line start) where a new child of PARENT goes.
PARENT and AFTER are markers or nil.  After AFTER's subtree when given;
else first among PARENT's children (before the first child, or at the
end of PARENT's subtree when it has none); with no PARENT either, before
the first act."
  (save-excursion
    (cond
     (after (goto-char after) (org-scribe--end-of-subtree-position))
     (parent (goto-char parent)
            (let ((start (point)))
              (outline-next-heading)
              (org-scribe--clamp-before-local-variables (point) start)))
     (t (org-scribe-companion-commands--first-act-position)))))

(defun org-scribe-companion-commands--check-after (parent after level)
  "Refuse with `conflict' unless AFTER is a child of PARENT at LEVEL.
PARENT is a marker, or nil for the acts.  The usual cause is a view that
went out of date while the browser was open."
  (unless (org-scribe-companion-commands--at after
            (and (= (org-current-level) level)
                 (if parent
                     (save-excursion (and (org-up-heading-safe)
                                          (= (point) (marker-position parent))))
                   t)))
    (org-scribe-companion-commands--fail "conflict" 'companion-error-conflict-after)))

(defun org-scribe-companion-commands--insert-at (position text)
  "Insert TEXT, whole lines, at POSITION (a line start), keeping lines apart."
  (goto-char position)
  (when (and (eobp) (not (bolp))) (insert "\n"))
  (insert text)
  (unless (bolp) (insert "\n")))

;;; move-node

(defun org-scribe-companion-commands--clock-offsets (beg end)
  "Return the running clock's markers inside BEG..END as (VARIABLE . OFFSET).
Deleting the text would collapse them; `--restore-clock' puts them back."
  (let (found)
    (dolist (variable '(org-clock-marker org-clock-hd-marker))
      (let ((marker (and (boundp variable) (symbol-value variable))))
        (when (and (markerp marker) (eq (marker-buffer marker) (current-buffer))
                   (>= (marker-position marker) beg) (< (marker-position marker) end))
          (push (cons variable (- (marker-position marker) beg)) found))))
    found))

(defun org-scribe-companion-commands--restore-clock (offsets start)
  "Point the clock markers in OFFSETS back into the text now beginning at START."
  (pcase-dolist (`(,variable . ,offset) offsets)
    (set-marker (symbol-value variable) (+ start offset) (current-buffer))))

(defun org-scribe-companion-commands--move-node (root args)
  "Move a heading and its subtree.  ARGS: `:id', `:parent', `:after'.
A scene goes under a chapter, a chapter under an act, an act to the top
level (no `:parent').  `:after' is the ID of the sibling it follows, or
nil for first.  The subtree moves as it is: drawer, ID, tags, body."
  (let* ((buffer (org-scribe-companion-commands--manuscript-buffer root))
         (id (org-scribe-companion-commands--require-id args :id))
         (parent-id (org-scribe-companion-commands--arg-string args :parent))
         (after-id (org-scribe-companion-commands--arg-string args :after))
         (node (org-scribe-companion-commands--heading root buffer id))
         (parent (and parent-id (org-scribe-companion-commands--heading root buffer parent-id)))
         (after (and after-id (org-scribe-companion-commands--arg-heading root buffer after-id))))
    (unwind-protect
        (let ((level (org-scribe-companion-commands--level node)))
          (unless (memq level (list 1 (org-scribe-chapter-level) (org-scribe-scene-level)))
            (org-scribe-companion-commands--fail "invalid" 'companion-error-invalid-move
                                                 (org-scribe-companion-commands--at node (org-get-heading t t t t))))
          (unless (if parent
                      (= (org-scribe-companion-commands--level parent) (1- level))
                    (= level 1))
            (org-scribe-companion-commands--fail "invalid" 'companion-error-invalid-move
                                                 (org-scribe-companion-commands--at node (org-get-heading t t t t))))
          (when (equal after-id id)
            (org-scribe-companion-commands--fail "invalid" 'companion-error-invalid-move
                                                 (org-scribe-companion-commands--at node (org-get-heading t t t t))))
          (when after
            (org-scribe-companion-commands--check-after parent after level))
          (org-scribe-companion-commands--with-edit buffer
            (let* ((beg (org-scribe-companion-commands--at node (org-back-to-heading t) (point)))
                   (end (org-scribe-companion-commands--at node (org-scribe--end-of-subtree-position)))
                   (target (org-scribe-companion-commands--insertion-point parent after)))
              (unless (or (= target beg) (= target end))
                (let ((text (buffer-substring beg end))
                      (spot (copy-marker target))
                      (clock (org-scribe-companion-commands--clock-offsets beg end)))
                  (delete-region beg end)
                  (org-scribe-companion-commands--insert-at spot text)
                  (org-scribe-companion-commands--restore-clock clock (marker-position spot))
                  (set-marker spot nil)
                  ;; a raw delete and insert leave Org's element cache out of
                  ;; step, and the next lookup by ID would then miss a heading
                  (org-element-cache-reset)))))
          (org-scribe-companion-commands--finish root))
      (dolist (marker (list node parent after)) (when marker (set-marker marker nil))))))

(defun org-scribe-companion-commands--arg-heading (root buffer id)
  "Like `--heading', but an AFTER heading that is gone is a `conflict'."
  (condition-case err
      (org-scribe-companion-commands--heading root buffer id)
    (org-scribe-companion-command-error
     (if (equal (nth 1 err) "not-found")
         (org-scribe-companion-commands--fail "conflict" 'companion-error-conflict-after)
       (signal (car err) (cdr err))))))

;;; set-title, set-todo, set-field

;; Org reads a heading's text: these prefixes would not stay part of the title.
(defun org-scribe-companion-commands--title-parsed-p (title)
  "Non-nil when Org would read TITLE as more than text: a leading `*', the
COMMENT keyword (the scene is dropped from export) or a priority cookie."
  (string-match-p "\\`\\(?:\\*\\|COMMENT\\(?: \\|\\'\\)\\|\\[#[A-Z0-9]+\\]\\)" title))

(defun org-scribe-companion-commands--set-title (root args)
  "Replace a heading's text.  ARGS: `:id', `:title'.
TODO state, priority and tags stay."
  (let* ((buffer (org-scribe-companion-commands--manuscript-buffer root))
         (id (org-scribe-companion-commands--require-id args :id))
         (raw (org-scribe-companion-commands--arg args :title))
         (title (org-scribe-companion-commands--clean-line raw))
         (node (org-scribe-companion-commands--heading root buffer id)))
    (unwind-protect
        (progn
          (when (or (null title) (string-empty-p title)
                    (org-scribe-companion-commands--title-parsed-p title))
            (org-scribe-companion-commands--fail "invalid" 'companion-error-invalid-title))
          (org-scribe-companion-commands--with-edit buffer
            (org-scribe-companion-commands--at node
              (unless (equal title (org-get-heading t t t t))
                (org-edit-headline title))))
          (org-scribe-companion-commands--finish root))
      (set-marker node nil))))

(defun org-scribe-companion-commands--quiet-log-states (states)
  "Return STATES, `org-todo-log-states', with note prompts made plain timestamps.
A browser change must not open a note buffer in the writer's Emacs; the
timestamp of a `!' state is kept."
  (mapcar (lambda (entry)
            (list (car entry)
                  (if (eq (nth 1 entry) 'note) 'time (nth 1 entry))
                  (if (eq (nth 2 entry) 'note) 'time (nth 2 entry))))
          states))

(defun org-scribe-companion-commands--flush-log-note ()
  "Write the log line `org-todo' left pending, now.
`org-todo' defers it to `org-add-log-note' on `post-command-hook', which would
run at the writer's next keystroke, in whatever buffer they are in, and keep
only the last of several pending notes.  The states have been made quiet, so
the note is a plain line and needs no window."
  (when (memq #'org-add-log-note (default-value 'post-command-hook))
    (remove-hook 'post-command-hook #'org-add-log-note)
    (setq org-log-setup nil
          org-log-note-window-configuration (current-window-configuration))
    (move-marker org-log-note-return-to (point))
    (let ((inhibit-message t)
          (org-note-abort nil))
      (with-temp-buffer (org-store-log-note)))))

(defun org-scribe-companion-commands--set-todo (root args)
  "Set a heading's TODO state.  ARGS: `:id', `:state' (a keyword, or \"\" for none).
Goes through `org-todo', so CLOSED stamps and the file's logging settings
apply, except that a state asking for a note logs a timestamp instead."
  (let* ((buffer (org-scribe-companion-commands--manuscript-buffer root))
         (id (org-scribe-companion-commands--require-id args :id))
         (state (org-scribe-companion-commands--arg args :state))
         (node (org-scribe-companion-commands--heading root buffer id)))
    (unwind-protect
        (progn
          (unless (stringp state)
            (org-scribe-companion-commands--fail "invalid" 'companion-error-missing-arg "state"))
          (unless (or (string-empty-p state)
                      (with-current-buffer buffer (member state org-todo-keywords-1)))
            (org-scribe-companion-commands--fail "invalid" 'companion-error-invalid-state state))
          (org-scribe-companion-commands--with-edit buffer
            (org-scribe-companion-commands--at node
              (unless (equal (or (org-get-todo-state) "") state)
                (condition-case err
                    (let ((org-todo-log-states
                           (org-scribe-companion-commands--quiet-log-states org-todo-log-states)))
                      (org-todo (if (string-empty-p state) 'none state))
                      (org-scribe-companion-commands--flush-log-note))
                  (user-error
                   (org-scribe-companion-commands--fail
                    "invalid" 'companion-error-todo-refused (error-message-string err)))))))
          (org-scribe-companion-commands--finish root))
      (set-marker node nil))))

(defun org-scribe-companion-commands--existing-link (id current)
  "Return the link to ID written in the property text CURRENT, or nil."
  (when (stringp current)
    (let ((start 0) found)
      (while (and (not found)
                  (string-match org-link-bracket-re current start))
        (setq start (match-end 0))
        (when (equal (match-string 1 current) (concat "id:" id))
          (setq found (match-string 0 current))))
      found)))

(defun org-scribe-companion-commands--link-value (root spec args &optional current)
  "Return the property text for the link field SPEC from ARGS, or refuse.
CURRENT is the property's present text: a link in it keeps its text as
written (renames are `org-scribe-update-all-link-names''s job), and one to an
ID that names no entity (deleted since) is kept too, so it cannot block every
edit of the field.
ARGS has `:links', a vector of entity IDs, and optionally `:unlinked', a
vector of plain names kept after them.  Each ID must name an entity of the
field's kind; the link text is that entity's current name."
  (let* ((links (plist-get args :links))
         (unlinked (plist-get args :unlinked))
         (entities (plist-get (org-scribe-companion--snapshot root) :entities))
         (kind (org-scribe-companion--keyword (symbol-name (nth 2 spec))))
         (known (append (plist-get entities kind) nil))
         (seen nil)
         (items nil))
    (unless (vectorp links)
      (org-scribe-companion-commands--fail "unsupported" 'companion-error-invalid-key
                                           (symbol-name (car spec))))
    (seq-doseq (id links)
      (let* ((entity (and (stringp id)
                          (cl-find id known :key (lambda (e) (plist-get e :id)) :test #'equal)))
             (existing (and (stringp id)
                            (org-scribe-companion-commands--existing-link id current)))
             (dangling (and (not entity) existing)))
        (unless (or entity dangling)
          (org-scribe-companion-commands--fail
           "invalid" 'companion-error-unknown-entity (format "%s" id)))
        (unless (member id seen)
          (push id seen)
          ;; a link already in the field keeps the text the writer gave it
          (push (or existing
                    (org-link-make-string (concat "id:" id) (plist-get entity :name)))
                items))))
    (when (and (eq (nth 1 spec) 'link-one) (> (length items) 1))
      (org-scribe-companion-commands--fail "invalid" 'companion-error-invalid-key
                                           (symbol-name (car spec))))
    (when (vectorp unlinked)
      (seq-doseq (name unlinked)
        (when-let* ((clean (and (stringp name)
                                (org-string-nw-p (org-scribe-companion-commands--clean-line name)))))
          (push clean items))))
    (mapconcat #'identity (nreverse items) ", ")))

(defun org-scribe-companion-commands--set-field (root args)
  "Set a scene property.  ARGS: `:id' (a scene), `:key' (canonical), and
`:value' (text fields, flattened to one line) or `:links' (link fields, IDs).
The property name written is the one already on the heading, else the
project language's."
  (let* ((buffer (org-scribe-companion-commands--manuscript-buffer root))
         (id (org-scribe-companion-commands--require-id args :id))
         (key-name (org-scribe-companion-commands--arg-string args :key))
         (node (org-scribe-companion-commands--heading root buffer id)))
    (unwind-protect
        (let* ((default-directory root)
               (spec (and key-name
                          (assq (intern-soft key-name)
                                (org-scribe-companion--field-specs))))
               value)
          (unless spec
            (org-scribe-companion-commands--fail "invalid" 'companion-error-invalid-key
                                                 (or key-name "")))
          (unless (= (org-scribe-companion-commands--level node) (org-scribe-scene-level))
            (org-scribe-companion-commands--fail "invalid" 'companion-error-not-a-scene id))
          (if (eq (nth 1 spec) 'text)
              (progn
                (setq value (org-scribe-companion-commands--clean-line
                             (org-scribe-companion-commands--arg args :value)))
                (unless value
                  (org-scribe-companion-commands--fail
                   "invalid" 'companion-error-missing-arg "value")))
            (setq value (org-scribe-companion-commands--link-value
                         root spec args
                         (org-scribe-companion-commands--at node
                           (org-scribe-scene-property-get (car spec))))))
          (org-scribe-companion-commands--with-edit buffer
            (org-scribe-companion-commands--at node
              (unless (equal value (or (org-scribe-scene-property-get (car spec)) ""))
                (org-scribe-scene-property-set (car spec) value))))
          (org-scribe-companion-commands--finish root))
      (set-marker node nil))))

;;; create-scene

(defun org-scribe-companion-commands--create-scene (root args)
  "Add a scene to a chapter.  ARGS: `:parent' (chapter ID), `:after', `:title'.
The text is the one `org-scribe-insert-scene' inserts.  The answer carries
the new scene's `:id'."
  (require 'org-scribe-project)
  (let* ((buffer (org-scribe-companion-commands--manuscript-buffer root))
         (parent-id (org-scribe-companion-commands--require-id args :parent))
         (after-id (org-scribe-companion-commands--arg-string args :after))
         (raw-title (org-scribe-companion-commands--arg args :title))
         (title (if (stringp raw-title)
                    (org-scribe-companion-commands--clean-line raw-title)
                  ""))
         (parent (org-scribe-companion-commands--heading root buffer parent-id))
         (after (and after-id (org-scribe-companion-commands--arg-heading root buffer after-id)))
         new-id)
    (unwind-protect
        (progn
          (unless (eql (org-scribe-companion-commands--level parent)
                       (org-scribe-chapter-level))
            (org-scribe-companion-commands--fail "invalid" 'companion-error-not-a-chapter))
          (when after
            (org-scribe-companion-commands--check-after
             parent after (org-scribe-scene-level)))
          (when (org-scribe-companion-commands--title-parsed-p title)
            (org-scribe-companion-commands--fail "invalid" 'companion-error-invalid-title))
          (when (string-empty-p title) (setq title (let ((default-directory root))
                                           (org-scribe--content-msg 'default-scene-name))))
          (org-scribe-companion-commands--with-edit buffer
            (let* ((default-directory root)
                   (position (org-scribe-companion-commands--insertion-point parent after))
                   (spot (copy-marker position)))
              (org-scribe-companion-commands--insert-at
               spot (org-scribe--scene-template title))
              (goto-char spot)
              (setq new-id (org-id-get-create))
              (set-marker spot nil)))
          (org-scribe--refile-invalidate-cache)
          (append (org-scribe-companion-commands--finish root) (list :id new-id)))
      (dolist (marker (list parent after)) (when marker (set-marker marker nil))))))

;;; create-chapter

(defun org-scribe-companion-commands--create-chapter (root args)
  "Add an empty chapter to an act.  ARGS: `:parent' (act ID), `:after', `:title'.
The text is `org-scribe--chapter-template' without a first scene.  The
answer carries the new chapter's `:id'."
  (require 'org-scribe-project)
  (let* ((buffer (org-scribe-companion-commands--manuscript-buffer root))
         (parent-id (org-scribe-companion-commands--require-id args :parent))
         (after-id (org-scribe-companion-commands--arg-string args :after))
         (raw-title (org-scribe-companion-commands--arg args :title))
         (title (if (stringp raw-title)
                    (org-scribe-companion-commands--clean-line raw-title)
                  ""))
         (parent (org-scribe-companion-commands--heading root buffer parent-id))
         (after (and after-id (org-scribe-companion-commands--arg-heading root buffer after-id)))
         new-id)
    (unwind-protect
        (progn
          (unless (org-scribe-chapter-level)
            (org-scribe-companion-commands--fail "invalid" 'companion-error-no-chapters))
          (unless (eql (org-scribe-companion-commands--level parent) 1)
            (org-scribe-companion-commands--fail "invalid" 'companion-error-not-an-act))
          (when after
            (org-scribe-companion-commands--check-after
             parent after (org-scribe-chapter-level)))
          (when (org-scribe-companion-commands--title-parsed-p title)
            (org-scribe-companion-commands--fail "invalid" 'companion-error-invalid-title))
          (when (string-empty-p title) (setq title (let ((default-directory root))
                                           (org-scribe--content-msg 'default-chapter-name))))
          (org-scribe-companion-commands--with-edit buffer
            (let* ((default-directory root)
                   (position (org-scribe-companion-commands--insertion-point parent after))
                   (spot (copy-marker position)))
              (org-scribe-companion-commands--insert-at
               spot (org-scribe--chapter-template title))
              (goto-char spot)
              (setq new-id (org-id-get-create))
              (set-marker spot nil)))
          (org-scribe--refile-invalidate-cache)
          (append (org-scribe-companion-commands--finish root) (list :id new-id)))
      (dolist (marker (list parent after)) (when marker (set-marker marker nil))))))

(defun org-scribe-companion-commands--set-color (root args)
  "Change the page's colours for the project at ROOT.  ARGS: `:kind', `:value', `:color'.
KIND is `theme' (COLOR names the theme), `warn' or `focus', `role',
`reltype' or `pov' (VALUE names the item; a PoV by character ID), or `all'
\(remove every pinned colour).  COLOR is #rrggbb, or empty to remove the pin.
The choice is written to the project's marker file, so it is not one of
the buffers' undo steps: the page offers a reset instead."
  (let* ((kind (org-scribe-companion-commands--arg-string args :kind))
         (value (org-scribe-companion-commands--clean-line
                 (org-scribe-companion-commands--arg args :value)))
         (color (org-scribe-companion-commands--arg-string args :color))
         (symbol (and kind (intern-soft kind))))
    (unless (file-exists-p (expand-file-name ".org-scribe-project" root))
      (org-scribe-companion-commands--fail "invalid" 'companion-error-no-marker))
    (cond
     ((equal kind "theme")
      (let ((theme (and color (intern-soft color))))
        (unless (and theme (memq theme (org-scribe-companion-colors-theme-names)))
          (org-scribe-companion-commands--fail
           "invalid" 'companion-error-invalid-theme (or color "")))
        (org-scribe-companion-colors-set-theme root theme)))
     ((equal kind "all")
      (org-scribe-companion-colors-clear-pins root))
     ((memq symbol '(warn focus role reltype pov))
      (when (and color (not (org-scribe-companion-colors-color-p color)))
        (org-scribe-companion-commands--fail "invalid" 'companion-error-invalid-color color))
      (unless (memq symbol '(warn focus))
        (when (or (null value) (string-empty-p value))
          (org-scribe-companion-commands--fail "invalid" 'companion-error-missing-arg "value"))
        (when (and (eq symbol 'pov)
                   (not (cl-some (lambda (character) (equal (plist-get character :id) value))
                                 (plist-get (plist-get (org-scribe-companion--snapshot root) :entities)
                                            :character))))
          (org-scribe-companion-commands--fail "not-found" 'companion-error-not-a-character)))
      (org-scribe-companion-colors-set-pin root symbol value color))
     (t (org-scribe-companion-commands--fail
         "invalid" 'companion-error-invalid-color-kind (or kind ""))))
    (org-scribe-companion--clear-cache)
    (when (fboundp 'org-scribe-companion--announce-change)
      (org-scribe-companion--announce-change))
    (let ((answer (org-scribe-companion-commands--ok root))
          (ratio (and color (org-scribe-companion-colors-hex-p color)
                      (org-scribe-companion-colors-contrast color))))
      ;; a pin is the writer's choice: accepted, with a warning when it will
      ;; be hard to read on the page
      (if (and ratio (< ratio org-scribe-companion-colors-min-contrast))
          (append answer (list :warning (org-scribe-msg 'companion-warning-low-contrast
                                                        (downcase color) ratio)))
        answer))))

(defun org-scribe-companion-commands-run (op args root)
  "Run the operation OP (a string) with the plist ARGS for the project at ROOT.
Returns the answer plist described in the Commentary."
  (let* ((function (cdr (assoc op org-scribe-companion-commands--operations)))
         (root (file-name-as-directory (expand-file-name root)))
         ;; error notices are in the project's language, as the page is
         (org-scribe-message-language (let ((default-directory root))
                                        (org-scribe-project-language))))
    (if (null function)
        (org-scribe-companion-commands-error
         "unsupported" (org-scribe-msg 'companion-error-unsupported-op (format "%s" op)))
      (condition-case err
          (funcall function root args)
        (org-scribe-companion-command-error
         (org-scribe-companion-commands-error (nth 1 err) (nth 2 err) (nth 3 err)))
        (error (org-scribe-companion-commands-error
                "invalid" (error-message-string err)))))))

(provide 'org-scribe-companion-commands)

;; The entity commands need this file's machinery, so load them after it.
(require 'org-scribe-companion-entity)

;;; org-scribe-companion-commands.el ends here
