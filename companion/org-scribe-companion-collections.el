;;; org-scribe-companion-collections.el --- Visual companion: saved filters -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; A collection is a named set of the page's scene filters, kept in the
;; project so it travels with it: one line per collection in
;; .org-scribe-project,
;;
;;   # Companion-collection: Ana's open scenes | pov=ID | todo=!FINISHED
;;
;; meant to survive hand editing.  The name comes first; every other part is
;; KEY=VALUE, with KEY one of `org-scribe-companion-collections--keys'.  A
;; link filter holds an entity ID (never a name, so a rename keeps it), or
;; "-" for "no value"; the state filter holds a TODO keyword, "-" for "no
;; state", or "!KEYWORD" for "any state but KEYWORD".  A collection is a
;; rule: a scene that matches later shows up in it by itself.
;;
;; A part naming something that no longer exists is kept as written: the page
;; marks the collection and leaves the part out when it applies it, but
;; never empties a saved definition.  Parts this file does not understand
;; are kept too, unless the writer saves new filters over the collection.
;;
;; Like the colour pins, the lines are in no buffer: their hash ends the
;; revision string (`-k'), so the page refreshes when one changes.  Saving
;; is the `set-collection' command (`org-scribe-companion-collections-set');
;; applying a collection is the page's own business.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org-scribe-core)
(require 'org-scribe-messages)

(declare-function org-scribe-companion--snapshot "org-scribe-companion-model")
(declare-function org-scribe-companion-commands--fail "org-scribe-companion-commands")
(declare-function org-scribe-companion-commands--arg "org-scribe-companion-commands")
(declare-function org-scribe-companion-commands--arg-string "org-scribe-companion-commands")
(declare-function org-scribe-companion-commands--clean-line "org-scribe-companion-commands")
(declare-function org-scribe-companion-commands--ok "org-scribe-companion-commands")
(declare-function org-scribe-companion--announce-change "org-scribe-companion")

(defconst org-scribe-companion-collections--marker "Companion-collection"
  "The key of the collection lines in .org-scribe-project.")

(defconst org-scribe-companion-collections--keys
  '(("pov" . :character)
    ("character" . :character)
    ("plot" . :plot)
    ("location" . :location)
    ("object" . :object)
    ("todo"))
  "The filter keys a collection may hold, in the order they are written.
Each is (KEY . ENTITY-KIND): the snapshot's entity list a link filter's
ID must belong to; nil for the state filter.  The same keys as the
page's filters (companion/web/filters.js).")

(defconst org-scribe-companion-collections--max-name 80
  "The longest collection name accepted.")

;;; Reading

(defun org-scribe-companion-collections--lines (root)
  "Return the raw collection lines of ROOT's marker file, in file order."
  (org-scribe--project-marker-get-all root org-scribe-companion-collections--marker))

(defun org-scribe-companion-collections--name-of (line)
  "Return the name part of the collection LINE (trimmed; may be empty)."
  (string-trim (car (split-string line "|"))))

(defun org-scribe-companion-collections--parse (line)
  "Return (NAME . FILTERS) for the collection LINE, or nil when it has no name.
FILTERS is an alist of (KEY . VALUE) strings, known keys with a non-empty
value only, in file order."
  (let* ((parts (split-string line "|"))
         (name (string-trim (car parts)))
         filters)
    (unless (string-empty-p name)
      (dolist (part (cdr parts))
        (when (string-match "\\`[ \t]*\\([^=]+?\\)[ \t]*=[ \t]*\\(.*?\\)[ \t]*\\'" part)
          (let ((key (downcase (match-string 1 part)))
                (value (match-string 2 part)))
            (when (and (assoc key org-scribe-companion-collections--keys)
                       (not (string-empty-p value))
                       (not (assoc key filters)))
              (push (cons key value) filters)))))
      (cons name (nreverse filters)))))

(defun org-scribe-companion-collections-read (root)
  "Return ROOT's collections as a list of (NAME . FILTERS), in file order.
See `org-scribe-companion-collections--parse'.  Lines without a name are
skipped."
  (delq nil (mapcar #'org-scribe-companion-collections--parse
                    (org-scribe-companion-collections--lines root))))

(defun org-scribe-companion-collections-json (root)
  "Return the snapshot's `collections' entry for ROOT.
A vector of (:name NAME :filters PLIST), PLIST keyed by filter key."
  (vconcat
   (mapcar (lambda (collection)
             (list :name (car collection)
                   :filters (cl-loop for (key . value) in (cdr collection)
                                     append (list (intern (concat ":" key)) value))))
           (org-scribe-companion-collections-read root))))

(defun org-scribe-companion-collections-revision (root)
  "Return a short string that changes when ROOT's collection lines do."
  (substring (secure-hash 'md5 (format "%S" (org-scribe-companion-collections--lines root)))
             0 8))

;;; Writing

(defun org-scribe-companion-collections--format (name filters)
  "Return the collection line for NAME and the FILTERS alist, keys in order."
  (mapconcat #'identity
             (cons name
                   (cl-loop for (key . _) in org-scribe-companion-collections--keys
                            for value = (cdr (assoc key filters))
                            when value collect (format "%s=%s" key value)))
             " | "))

(defun org-scribe-companion-collections--index (lines name)
  "Return the index in LINES of the collection called NAME, or nil.
Names are compared ignoring case, so two collections cannot differ only
by it."
  (cl-position-if (lambda (line)
                    (string-equal-ignore-case
                     (org-scribe-companion-collections--name-of line) name))
                  lines))

(defun org-scribe-companion-collections--known-id-p (root kind id)
  "Non-nil when ID belongs to an entity of KIND (a snapshot key) at ROOT.
Hidden entities count."
  (cl-some (lambda (entity) (equal (plist-get entity :id) id))
           (append (plist-get (plist-get (org-scribe-companion--snapshot root) :entities)
                              kind)
                   nil)))

(defun org-scribe-companion-collections--valid-value-p (root key value)
  "Non-nil when VALUE is a valid value of the filter KEY at ROOT."
  (let ((kind (cdr (assoc key org-scribe-companion-collections--keys))))
    (cond
     ((string-match-p "[|\n]" value) nil)
     ((equal value "-") t)
     (kind (org-scribe-companion-collections--known-id-p root kind value))
     (t (let ((keywords (append (plist-get (plist-get (org-scribe-companion--snapshot root) :todo)
                                           :keywords)
                                nil)))
          (member (string-remove-prefix "!" value) keywords))))))

(defun org-scribe-companion-collections--filters (root filters)
  "Return the FILTERS plist from the page as a checked (KEY . VALUE) alist.
Refuses an unknown key or a value that names nothing; skips empty values."
  (unless (and (listp filters) (cl-evenp (length filters)))
    (org-scribe-companion-commands--fail "invalid" 'companion-error-missing-arg "filters"))
  (let (result)
    (cl-loop for (keyword value) on filters by #'cddr
             for key = (string-remove-prefix ":" (format "%s" keyword))
             do (unless (assoc key org-scribe-companion-collections--keys)
                  (org-scribe-companion-commands--fail
                   "invalid" 'companion-error-collection-key key))
             (when (and value (not (stringp value)))
               (org-scribe-companion-commands--fail
                "invalid" 'companion-error-collection-value (format "%s" value) key))
             (when (and value (not (string-empty-p value)))
               (unless (org-scribe-companion-collections--valid-value-p root key value)
                 (org-scribe-companion-commands--fail
                  "invalid" 'companion-error-collection-value value key))
               (push (cons key value) result)))
    (unless result
      (org-scribe-companion-commands--fail "invalid" 'companion-error-collection-empty))
    (nreverse result)))

(defun org-scribe-companion-collections--check-name (name)
  "Refuse NAME unless it can be a collection name."
  (when (or (null name) (string-empty-p name)
            (string-match-p "|" name)
            (> (length name) org-scribe-companion-collections--max-name))
    (org-scribe-companion-commands--fail
     "invalid" 'companion-error-collection-name org-scribe-companion-collections--max-name)))

(defun org-scribe-companion-collections-set (root args)
  "Create, change, rename or delete a collection of the project at ROOT.
ARGS:
  `:name' and `:filters'           a new collection;
  `:old', `:name' and `:filters'   replace OLD's filters (and rename it);
  `:old' and `:name'               rename OLD, keeping its line as written;
  `:old' and `:delete' true        delete OLD.
`:filters' is an object of filter keys, as the page keeps them.  Errors:
`invalid' (bad or duplicate name, unknown key, a value that names
nothing, no filter at all), `conflict' when OLD is no longer there.  The
line goes to the marker file, so it is not an undo step of any buffer."
  (unless (file-exists-p (expand-file-name ".org-scribe-project" root))
    (org-scribe-companion-commands--fail "invalid" 'companion-error-no-marker))
  (let* ((lines (org-scribe-companion-collections--lines root))
         (old (org-scribe-companion-commands--clean-line
               (org-scribe-companion-commands--arg args :old)))
         (old (and old (not (string-empty-p old)) old))
         (old-index (and old (org-scribe-companion-collections--index lines old)))
         (name (org-scribe-companion-commands--clean-line
                (org-scribe-companion-commands--arg args :name)))
         (has-filters (plist-member args :filters)))
    (when (and old (null old-index))
      (org-scribe-companion-commands--fail "conflict" 'companion-error-collection-missing old))
    (cond
     ((org-scribe-companion-commands--arg args :delete)
      (unless old
        (org-scribe-companion-commands--fail "invalid" 'companion-error-missing-arg "old"))
      (setq lines (append (seq-take lines old-index) (nthcdr (1+ old-index) lines))))
     (t
      (org-scribe-companion-collections--check-name name)
      (let ((same (org-scribe-companion-collections--index lines name)))
        (when (and same (not (eql same old-index)))
          (org-scribe-companion-commands--fail
           "invalid" 'companion-error-collection-exists name)))
      (unless (or has-filters old)
        (org-scribe-companion-commands--fail "invalid" 'companion-error-missing-arg "filters"))
      (let ((line (if has-filters
                      (org-scribe-companion-collections--format
                       name (org-scribe-companion-collections--filters
                             root (plist-get args :filters)))
                    ;; a rename: everything after the name stays as written
                    (let ((raw (nth old-index lines)))
                      (concat name (if (string-match "[ \t]*|" raw)
                                       (substring raw (match-beginning 0))
                                     ""))))))
        (if old-index
            (setf (nth old-index lines) line)
          (setq lines (append lines (list line)))))))
    (org-scribe--project-marker-set-all root org-scribe-companion-collections--marker lines)
    (when (fboundp 'org-scribe-companion--announce-change)
      (org-scribe-companion--announce-change))
    (append (org-scribe-companion-commands--ok root)
            (when name (list :name name)))))

(provide 'org-scribe-companion-collections)

;;; org-scribe-companion-collections.el ends here
