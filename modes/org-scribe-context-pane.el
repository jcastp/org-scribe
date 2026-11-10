;;; org-scribe-context-pane.el --- Scene context pane for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A read-only side window that follows point in the manuscript and shows
;; the facts the current scene is built from: the PoV's Goal, Motivation,
;; Need and Lie, the scene's Description and Summary, the whole cast (one
;; line each, the characters in this scene first), the location's Echo and
;; Hazard, the plot threads' premises, the plot point's definition, and the
;; scene's own dramatic properties.  A header line gives the position
;; (outline path) and the scene's word count.
;;
;; It is a fifth workspace layout, `context', registered when this module
;; loads.  The module is optional and is never loaded while
;; `org-scribe-context-pane-enable' is nil: the entry points
;; (`org-scribe-context-pane-open' and friends) live in
;; `core/org-scribe-core.el' and check the switches before requiring it.
;;
;; Nothing new is stored.  Every part is read from the design notebook with
;; `org-id-find' and the headings' own text; the pane is recomputed from an
;; idle timer, and only when the scene, the manuscript's modification tick or
;; the notebook's tick changed.

;;; Code:

(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'subr-x)
(require 'org-scribe-core)
(require 'org-scribe-config)
(require 'org-scribe-messages)
(require 'org-scribe-modes)
(require 'org-scribe-wordcount)
(require 'org-scribe-linking-core)
(require 'org-scribe-character-links)
(require 'org-scribe-location-links)
(require 'org-scribe-plot-links)
(require 'org-scribe-plot-point-links)

(defvar org-scribe--character-entity)
(defvar org-scribe--location-entity)
(defvar org-scribe--plot-entity)
(defvar org-scribe--plot-point-entity)

;;; Faces

(defface org-scribe-context-pane-heading
  '((t :inherit bold))
  "Face of the pane's section headings."
  :group 'org-scribe)

(defface org-scribe-context-pane-label
  '((t :inherit font-lock-keyword-face))
  "Face of the labels (Goal, Lie, Echo, ...) in the pane."
  :group 'org-scribe)

(defface org-scribe-context-pane-dim
  '((t :inherit shadow))
  "Face of the pane's secondary lines (empty states, hidden counts)."
  :group 'org-scribe)

;;; State

(defvar org-scribe-context-pane--source nil
  "The manuscript buffer the pane follows.")

(defvar org-scribe-context-pane--timer nil
  "Idle timer that refreshes the pane.")

(defvar org-scribe-context-pane--last-key nil
  "Key of what the pane shows now; the pane is redrawn only when it changes.")

(defvar org-scribe-context-pane--drawn-id nil
  "ID of the scene drawn now, so a redraw of the same scene keeps its scroll.")

(defvar org-scribe-context-pane--saved-config nil
  "Window configuration saved before the pane layout was set up.")

(defvar org-scribe-context-pane--cast-expanded nil
  "Non-nil while the collapsed part of the cast is shown in full.")

;; What the pane reads from the notebook is remembered for as long as the
;; notebook's modification tick stays the same, so an edit to the manuscript
;; (which changes the key of every redraw) recounts the scene's words but
;; does not read the whole cast and every entity again.
(defvar org-scribe-context-pane--memo (make-hash-table :test 'equal)
  "Entity data read from the notebook, keyed by kind and ID.")

(defvar org-scribe-context-pane--memo-tick nil
  "The notebook tick `org-scribe-context-pane--memo' was filled at.")

(defvar org-scribe-context-pane--tick-now nil
  "The state of the notebook, (FILE . TICK), during the collection in progress.
The file is part of it: two projects must never share remembered data just
because their notebooks happen to have the same tick.")

(defvar org-scribe-context-pane--rescanned-tick nil
  "The notebook tick at which the ID locations were last rescanned.")

(defconst org-scribe-context-pane--buffer-name "*org-scribe-context*"
  "Name of the pane buffer; the same in every language, so that a change of
`org-scribe-message-language' never orphans an open pane.")

(defconst org-scribe-context-pane--line-max 600
  "Longest one-liner, in characters, before it is cut with an ellipsis.
Generous: the pane wraps lines, and a premise cut at 160 hid the part a writer
most wanted to re-read.")

(defconst org-scribe-context-pane--emphasis-re
  "\\(\\`\\|[ \t(\"'—-]\\)\\([*/=~]\\)\\([^ \t\n*/=~]\\(?:[^\n]*?[^ \t\n]\\)?\\)\\2\\(\\'\\|[] \t.,;:!?)\"'—-]\\)"
  "An Org emphasis span: bold, italic, verbatim or code, at word boundaries.")

(defun org-scribe-context-pane--strip-emphasis (text)
  "Return TEXT without Org emphasis markers, so `*word*' reads as plain text."
  (let ((prev nil))
    (while (not (equal prev text))
      (setq prev text
            text (replace-regexp-in-string
                  org-scribe-context-pane--emphasis-re "\\1\\3\\4" text)))
    text))

;;; Reading text out of the notebook

(defun org-scribe-context-pane--clean (text)
  "Return TEXT as one tidy line: links displayed, spaces collapsed, cut short."
  (when text
    (let ((s (string-trim
              (replace-regexp-in-string
               "[ \t\n]+" " "
               (org-scribe-context-pane--strip-emphasis
                (org-link-display-format text))))))
      (cond ((string-empty-p s) nil)
            ((> (length s) org-scribe-context-pane--line-max)
             (concat (string-trim-right
                      (substring s 0 (1- org-scribe-context-pane--line-max)))
                     "…"))
            (t s)))))

(defun org-scribe-context-pane--item (label from to)
  "Return the text of the descriptive-list item LABEL between FROM and TO.
LABEL is as the template writes it, for example \"Lie ::\".  The item's
continuation lines are joined.  Returns nil when there is no such item or
it is blank."
  (let* ((bare (string-trim (string-remove-suffix "::" (string-trim label))))
         (re (format "^[ \t]*[-+][ \t]+%s[ \t]*::[ \t]*" (regexp-quote bare))))
    (save-excursion
      (goto-char from)
      (when (re-search-forward re to t)
        (let ((parts (list (buffer-substring-no-properties
                            (point) (line-end-position)))))
          (forward-line 1)
          (while (and (< (point) to)
                      (looking-at "[ \t]+\\([^ \t\n].*\\)$")
                      (not (string-match-p "\\`[-+][ \t]" (match-string 1))))
            (push (match-string-no-properties 1) parts)
            (forward-line 1))
          (org-scribe-context-pane--clean
           (string-join (nreverse parts) " ")))))))

(defun org-scribe-context-pane--subheading (title from to)
  "Return (BEG . END) of the sub-heading TITLE between FROM and TO, or nil.
END is the end of its subtree."
  (save-excursion
    (goto-char from)
    (when (re-search-forward
           (format "^\\*+[ \t]+%s[ \t]*\\(?::[^ \t\n]*:\\)?[ \t]*$"
                   (regexp-quote title))
           to t)
      (let ((beg (line-beginning-position)))
        (goto-char beg)
        (org-end-of-subtree t t)
        (cons beg (point))))))

(defun org-scribe-context-pane--first-paragraph ()
  "Return the first paragraph of the entry at point's body, as one line.
Skips the property drawer, `#' comment lines and comment blocks; stops at a
blank line or the next heading.  Returns nil when the body has no prose."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (outline-next-heading) (point))))
      (forward-line 1)
      (when (looking-at-p "[ \t]*:PROPERTIES:")
        (when (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
          (forward-line 1)))
      (let (parts done)
        (while (and (not done) (< (point) end))
          (cond
           ((looking-at-p "[ \t]*#\\+begin_")
            (if (re-search-forward "^[ \t]*#\\+end_" end t)
                (forward-line 1)
              (goto-char end)))
           ((looking-at-p "[ \t]*#")
            (forward-line 1))
           ((looking-at-p "[ \t]*$")
            (if parts (setq done t) (forward-line 1)))
           (t
            (push (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))
                  parts)
            (forward-line 1))))
        (let ((paragraph (string-join (nreverse parts) " ")))
          ;; A list is structure, not a premise or a definition.
          (unless (string-match-p "\\`[ \t]*[-+][ \t]" paragraph)
            (org-scribe-context-pane--clean paragraph)))))))

(defun org-scribe-context-pane--find-id (id)
  "Return a marker at the entry with ID, or nil when no such entry exists.
`org-id-find' rescans every known file when it misses, which is the cost of
a dead link; so a miss is remembered for as long as the notebook is
unchanged, and the notebook itself is rescanned at most once in that time
\(an ID minted a moment ago, or written by hand, is not yet in
`org-id-locations')."
  (when id
    (unless (org-scribe-context-pane--memo (list 'dead id) (lambda () nil))
      (or (ignore-errors (org-id-find id t))
          (when-let* ((_ (not (equal org-scribe-context-pane--rescanned-tick
                                     org-scribe-context-pane--tick-now)))
                      (file (plist-get (org-scribe-project-structure) :design-file)))
            (setq org-scribe-context-pane--rescanned-tick
                  org-scribe-context-pane--tick-now)
            (ignore-errors
              (let ((inhibit-message t))
                (org-id-update-id-locations (list file) t)))
            (ignore-errors (org-id-find id t)))
          (progn (puthash (list 'dead id) t org-scribe-context-pane--memo)
                 nil)))))

(defun org-scribe-context-pane--memo (key thunk)
  "Return the value for KEY, calling THUNK once per state of the notebook."
  (unless (equal org-scribe-context-pane--memo-tick org-scribe-context-pane--tick-now)
    (clrhash org-scribe-context-pane--memo)
    (setq org-scribe-context-pane--memo-tick org-scribe-context-pane--tick-now))
  (let ((hit (gethash key org-scribe-context-pane--memo 'none)))
    (if (eq hit 'none)
        (puthash key (funcall thunk) org-scribe-context-pane--memo)
      hit)))

(defun org-scribe-context-pane--entity-label (key)
  "Return the template label KEY (\"Lie ::\", \"Desire\", ...) as the notebook spells it.
The notebook was written in the project's language, which is not
necessarily the language of the interface messages."
  (let ((org-scribe-message-language
         (or (ignore-errors (org-scribe-project-language))
             org-scribe-message-language)))
    (org-scribe-msg key)))

(defmacro org-scribe-context-pane--with-entity (id &rest body)
  "Run BODY at the heading of the entity ID, widened; nil when it is gone."
  (declare (indent 1))
  `(when-let* ((marker (org-scribe-context-pane--find-id ,id)))
     (with-current-buffer (marker-buffer marker)
       (org-with-wide-buffer
        (goto-char marker)
        ,@body))))

(defun org-scribe-context-pane--parse-links (value)
  "Return the items of the link property VALUE as a list of (ID . NAME).
A plain name (no ID link) comes back with a nil ID."
  (when (stringp value)
    (let ((rest value) (items nil) (start 0))
      (while (string-match "\\[\\[id:\\([^]]+\\)\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]"
                           value start)
        (push (cons (match-string 1 value)
                    (or (match-string 2 value) (match-string 1 value)))
              items)
        (setq start (match-end 0)))
      (setq rest (replace-regexp-in-string
                  "\\[\\[id:[^]]+\\]\\(?:\\[[^]]*\\]\\)?\\]" "," value))
      (dolist (name (split-string rest "[,;]" t "[ \t]+"))
        (push (cons nil name) items))
      (nreverse items))))

;;; Collecting the facts

(defun org-scribe-context-pane--cached-entity (kind id thunk)
  "Return the data THUNK reads at entity ID's heading, memoized; nil if gone."
  (let ((data (org-scribe-context-pane--memo
               (list kind id)
               (lambda ()
                 (or (org-scribe-context-pane--with-entity id (funcall thunk))
                     'missing)))))
    (unless (eq data 'missing) data)))

(defun org-scribe-context-pane--entity-block (item kind thunk)
  "Return the plist for ITEM, an (ID . NAME) pair, with THUNK's data appended."
  (let* ((id (car item))
         (data (and id (org-scribe-context-pane--cached-entity kind id thunk))))
    (append (list :id id :name (cdr item) :missing (and id (null data))) data)))

(defun org-scribe-context-pane--pov-data (item)
  "Return the PoV block for ITEM, an (ID . NAME) pair."
  (org-scribe-context-pane--entity-block
   item 'pov
   (lambda ()
     (let* ((to (save-excursion (org-end-of-subtree t t) (point)))
            (from (point))
            (desire (org-scribe-context-pane--subheading
                     (org-scribe-context-pane--entity-label 'capture-char-desire)
                     from to))
            (need (org-scribe-context-pane--subheading
                   (org-scribe-context-pane--entity-label 'capture-char-need)
                   from to))
            (goal-label (org-scribe-context-pane--entity-label 'capture-char-goal)))
       (list :goal (or (org-scribe-context-pane--clean (org-entry-get nil "Goal"))
                       (and desire
                            (org-scribe-context-pane--item
                             goal-label (car desire) (cdr desire))))
             :motivation (and desire
                              (org-scribe-context-pane--item
                               (org-scribe-context-pane--entity-label
                                'capture-char-motivation)
                               (car desire) (cdr desire)))
             :need (and need
                        (org-scribe-context-pane--item
                         goal-label (car need) (cdr need)))
             :lie (org-scribe-context-pane--item
                   (org-scribe-context-pane--entity-label 'capture-char-lie)
                   from to))))))

(defun org-scribe-context-pane--location-data (item)
  "Return the location block for ITEM, an (ID . NAME) pair."
  (org-scribe-context-pane--entity-block
   item 'location
   (lambda ()
     (let ((to (save-excursion (org-end-of-subtree t t) (point)))
           (from (point)))
       (list :echo (org-scribe-context-pane--item
                    (org-scribe-context-pane--entity-label 'capture-loc-echo)
                    from to)
             :hazard (org-scribe-context-pane--item
                      (org-scribe-context-pane--entity-label 'capture-loc-hazard)
                      from to))))))

(defun org-scribe-context-pane--text-data (item)
  "Return the (name, first paragraph) block for ITEM, an (ID . NAME) pair."
  (org-scribe-context-pane--entity-block
   item 'text
   (lambda () (list :text (org-scribe-context-pane--first-paragraph)))))

(defun org-scribe-context-pane--read-cast ()
  "Return every character of the notebook as a plist, in document order.
Read-only: unlike `org-scribe--get-all-entities' this never gives a heading
an ID, so moving the cursor cannot make Emacs write to the notebook.  A
character with no ID yet is listed all the same (without a link to follow)."
  (let* ((entity org-scribe--character-entity)
         (file (funcall (plist-get entity :file-fn)))
         (pred (plist-get entity :heading-predicate))
         result)
    (when (and file (file-exists-p file))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (when (funcall pred)
              (push (list :id (org-id-get)
                          :name (org-scribe--entity-name-at-point)
                          :role (org-scribe-context-pane--clean
                                 (org-entry-get nil "Role"))
                          :goal (org-scribe-context-pane--clean
                                 (org-entry-get nil "Goal"))
                          :hidden (org-scribe--entity-hidden-weight-p
                                   (org-scribe--parse-weight
                                    (org-entry-get nil "Weight"))))
                    result)))
          nil 'file))))
    (nreverse result)))

(defun org-scribe-context-pane--cast (pov-items char-items)
  "Return (CAST . UNKNOWN) for the scene's PoV-ITEMS and CHAR-ITEMS.
CAST is every character, each flagged `:in-scene' when the scene links its
ID or names it in plain text.  UNKNOWN lists what the scene's characters
property holds that matches no character: a link to a deleted entity
\(`:missing') or a plain name that is not in the cast."
  (let* ((raw (org-scribe-context-pane--memo
               '(cast) #'org-scribe-context-pane--read-cast))
         (ids (delq nil (mapcar #'car (append pov-items char-items))))
         (names (delq nil (mapcar (lambda (i) (and (null (car i)) (cdr i)))
                                  (append pov-items char-items))))
         (known-ids (delq nil (mapcar (lambda (c) (plist-get c :id)) raw)))
         (cast (mapcar
                (lambda (c)
                  (append (list :in-scene
                                (and (or (member (plist-get c :id) ids)
                                         (seq-some (lambda (n)
                                                     (string-equal-ignore-case
                                                      n (plist-get c :name)))
                                                   names))
                                     t))
                          c))
                raw))
         unknown)
    (dolist (item char-items)
      (cond
       ((and (car item) (not (member (car item) known-ids)))
        (push (list :name (cdr item) :missing t) unknown))
       ((and (null (car item))
             (not (seq-some (lambda (c)
                              (string-equal-ignore-case (cdr item) (plist-get c :name)))
                            raw)))
        (push (list :name (cdr item) :missing nil) unknown))))
    (cons cast (nreverse unknown))))

(defun org-scribe-context-pane--scene-properties ()
  "Return the (CANONICAL-KEY . VALUE) pairs of the scene block, blanks skipped."
  (let (result)
    (dolist (key org-scribe-context-pane-scene-properties)
      ;; A key the language packs do not know is skipped, not an error.
      (when-let* ((_ (and (symbolp key) (org-scribe-lang-all :properties key)))
                  (value (org-scribe-context-pane--clean
                          (org-scribe-scene-property-get key))))
        (push (cons key value) result)))
    (nreverse result)))

(defun org-scribe-context-pane--scene-words ()
  "Return the word count of the scene at point, sub-headings included.
The scene's text is counted in a scratch buffer: counting it in place costs
time in proportion to the whole manuscript (0.17 s for 400 words in a
640 KB buffer, against 5 ms for the same words alone), and this runs after
every pause in typing."
  (let ((text (save-excursion
                (org-back-to-heading t)
                (buffer-substring-no-properties
                 (point) (save-excursion (org-end-of-subtree t t) (point))))))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert text)
      (org-scribe--count-words-region (point-min) (point-max)))))

(defun org-scribe-context-pane--position-data ()
  "Return a plist for point outside any scene: where it is, and nothing more.
Used on a chapter or act heading, so that the pane says where the writer is
instead of going blank."
  (save-excursion
    (let ((path (and (ignore-errors (org-back-to-heading t) t)
                     (org-get-outline-path t))))
      (list :no-scene t
            :id (list 'no-scene (point))
            :path path))))

(defun org-scribe-context-pane--scene-data ()
  "Return a plist describing the scene at point, or nil when point is in none.
Must be called in the manuscript, widened.  Point is preserved."
  (save-excursion
    (when (ignore-errors (org-scribe--goto-scene-heading) t)
      (let* ((pov-items (org-scribe-context-pane--parse-links
                         (org-scribe-scene-property-get 'pov)))
             (char-items (org-scribe-context-pane--parse-links
                          (org-scribe-scene-property-get 'characters)))
             (org-scribe-context-pane--tick-now
              (org-scribe-context-pane--notebook-state t))
             (cast (org-scribe-context-pane--cast pov-items char-items)))
        (list :id (or (org-entry-get nil "ID") (point))
              :title (org-scribe--heading-text)
              :path (org-get-outline-path t)
              :words (org-scribe-context-pane--scene-words)
              :pov (and pov-items (org-scribe-context-pane--pov-data (car pov-items)))
              :description (org-scribe-context-pane--clean
                            (org-scribe-scene-property-get 'description))
              :summary (org-scribe-context-pane--clean
                        (org-scribe-scene-property-get 'summary))
              :cast (car cast)
              :unknown-cast (cdr cast)
              :locations (mapcar #'org-scribe-context-pane--location-data
                                 (org-scribe-context-pane--parse-links
                                  (org-scribe-scene-property-get 'location)))
              :threads (mapcar #'org-scribe-context-pane--text-data
                               (org-scribe-context-pane--parse-links
                                (org-scribe-scene-property-get 'plot)))
              :plot-points (mapcar #'org-scribe-context-pane--text-data
                                   (org-scribe-context-pane--parse-links
                                    (org-scribe-scene-property-get 'plot-point)))
              :properties (org-scribe-context-pane--scene-properties))))))

;;; Drawing

(defun org-scribe-context-pane--visit-window ()
  "Return the window an entity opened from the pane should be shown in.
Never the pane itself and never the manuscript's window, which the pane
follows: a window showing something else when there is one, otherwise the
manuscript's window split below."
  (let* ((pane (get-buffer org-scribe-context-pane--buffer-name))
         (source-window (and (buffer-live-p org-scribe-context-pane--source)
                             (get-buffer-window org-scribe-context-pane--source))))
    (or (seq-find (lambda (w)
                    (not (memq (window-buffer w)
                               (list pane org-scribe-context-pane--source))))
                  (window-list nil 'no-minibuf))
        (and source-window
             (ignore-errors (split-window source-window nil 'below)))
        source-window)))

(defun org-scribe-context-pane--visit (button)
  "Visit the entity heading BUTTON stands for, beside the manuscript."
  (let ((id (button-get button 'org-scribe-id))
        (org-scribe-context-pane--tick-now
         (org-scribe-context-pane--notebook-state)))
    (if-let* ((marker (org-scribe-context-pane--find-id id)))
        (progn (when-let* ((window (org-scribe-context-pane--visit-window)))
                 (select-window window))
               (switch-to-buffer (marker-buffer marker))
               (goto-char marker)
               (org-reveal))
      (message "%s" (org-scribe-msg 'error-entity-link-broken
                                    (button-label button))))))

(defun org-scribe-context-pane--toggle-cast (_button)
  "Expand or collapse the off-scene part of the cast."
  (setq org-scribe-context-pane--cast-expanded
        (not org-scribe-context-pane--cast-expanded))
  (org-scribe-context-pane--refresh t))

(defun org-scribe-context-pane--insert-name (name id &optional missing)
  "Insert NAME, as a button to entity ID when there is one and it exists."
  (cond (missing
         (insert (propertize (org-scribe-msg 'context-pane-missing name)
                             'face 'org-scribe-context-pane-dim)))
        (id (insert-text-button name 'action #'org-scribe-context-pane--visit
                                'org-scribe-id id 'follow-link t
                                'face 'org-scribe-context-pane-heading))
        (t (insert (propertize name 'face 'org-scribe-context-pane-heading)))))

(defun org-scribe-context-pane--insert-field (label value)
  "Insert a \"LABEL: VALUE\" line; nothing when VALUE is nil."
  (when value
    (insert (propertize (concat label ": ") 'face 'org-scribe-context-pane-label)
            value "\n")))

(defun org-scribe-context-pane--insert-heading (title)
  "Insert the section heading TITLE after a blank line."
  (insert "\n" (propertize title 'face 'org-scribe-context-pane-heading) "\n"))

(defun org-scribe-context-pane--insert-dim (text)
  "Insert TEXT on a line of its own, dimmed."
  (insert (propertize text 'face 'org-scribe-context-pane-dim) "\n"))

(defun org-scribe-context-pane--unknown-line (entry)
  "Insert the line for ENTRY, a character the scene names but the cast lacks."
  (insert "● "
          (propertize (org-scribe-msg (if (plist-get entry :missing)
                                          'context-pane-missing
                                        'context-pane-unknown)
                                      (plist-get entry :name))
                      'face 'org-scribe-context-pane-dim)
          "\n"))

(defun org-scribe-context-pane--cast-line (character)
  "Insert the line for CHARACTER: dot, name, role, goal."
  (insert (if (plist-get character :in-scene) "● " "○ "))
  (org-scribe-context-pane--insert-name
   (plist-get character :name) (plist-get character :id))
  (dolist (part (list (plist-get character :role) (plist-get character :goal)))
    (when part (insert (propertize " · " 'face 'org-scribe-context-pane-dim) part)))
  (insert "\n"))

(defun org-scribe-context-pane--insert-cast (cast &optional unknown)
  "Insert the CAST section: in-scene characters first, then the rest.
UNKNOWN lists names the scene gives that are not in the cast."
  (org-scribe-context-pane--insert-heading (org-scribe-msg 'context-pane-cast))
  (let* ((in (seq-filter (lambda (c) (plist-get c :in-scene)) cast))
         (rest (seq-remove (lambda (c) (plist-get c :in-scene)) cast))
         (hidden (seq-filter (lambda (c) (plist-get c :hidden)) rest))
         (shown (seq-remove (lambda (c) (plist-get c :hidden)) rest)))
    (cond
     ((and (null cast) (null unknown))
      (org-scribe-context-pane--insert-dim (org-scribe-msg 'context-pane-no-characters)))
     (t
      (mapc #'org-scribe-context-pane--cast-line in)
      (mapc #'org-scribe-context-pane--unknown-line unknown)
      (if (and (not org-scribe-context-pane--cast-expanded)
               (> (length shown) org-scribe-context-pane-cast-collapse-threshold))
          (let ((names (mapcar (lambda (c) (plist-get c :name)) shown)))
            (insert-text-button
             (org-scribe-msg 'context-pane-more (length shown)
                             (concat (string-join (seq-take names 3) ", ")
                                     (when (> (length names) 3) ", …")))
             'action #'org-scribe-context-pane--toggle-cast
             'follow-link t 'face 'org-scribe-context-pane-dim)
            (insert "\n"))
        (mapc #'org-scribe-context-pane--cast-line shown))
      (when hidden
        (org-scribe-context-pane--insert-dim
         (org-scribe-msg 'context-pane-hidden (length hidden))))))))

(defun org-scribe-context-pane--property-label (key)
  "Return the pane's label for the scene property KEY."
  (org-scribe-msg (intern (format "context-pane-prop-%s" key))))

(defun org-scribe-context-pane--insert-scene (data)
  "Insert the whole pane body for DATA, a plist from `--scene-data'."
  (let ((pov (plist-get data :pov))
        (locations (plist-get data :locations))
        (threads (plist-get data :threads))
        (points (plist-get data :plot-points))
        (props (plist-get data :properties)))
    ;; PoV
    (insert (propertize (org-scribe-msg 'context-pane-pov)
                        'face 'org-scribe-context-pane-heading)
            "  ")
    (cond
     ((null pov)
      (org-scribe-context-pane--insert-dim (org-scribe-msg 'context-pane-no-pov)))
     (t
      (org-scribe-context-pane--insert-name
       (plist-get pov :name) (plist-get pov :id) (plist-get pov :missing))
      (insert "\n")
      (unless (plist-get pov :missing)
        (dolist (field '((:goal . context-pane-goal)
                         (:motivation . context-pane-motivation)
                         (:need . context-pane-need)
                         (:lie . context-pane-lie)))
          (org-scribe-context-pane--insert-field
           (org-scribe-msg (cdr field)) (plist-get pov (car field)))))))
    ;; Scene text
    (when (or (plist-get data :description) (plist-get data :summary))
      (insert "\n")
      (org-scribe-context-pane--insert-field
       (org-scribe-msg 'context-pane-description) (plist-get data :description))
      (org-scribe-context-pane--insert-field
       (org-scribe-msg 'context-pane-summary) (plist-get data :summary)))
    ;; Cast
    (org-scribe-context-pane--insert-cast (plist-get data :cast)
                                          (plist-get data :unknown-cast))
    ;; Location
    (org-scribe-context-pane--insert-heading (org-scribe-msg 'context-pane-location))
    (if (null locations)
        (org-scribe-context-pane--insert-dim (org-scribe-msg 'context-pane-none))
      (dolist (location locations)
        (org-scribe-context-pane--insert-name
         (plist-get location :name) (plist-get location :id) (plist-get location :missing))
        (insert "\n")
        (unless (plist-get location :missing)
          (org-scribe-context-pane--insert-field
           (org-scribe-msg 'context-pane-echo) (plist-get location :echo))
          (org-scribe-context-pane--insert-field
           (org-scribe-msg 'context-pane-hazard) (plist-get location :hazard)))))
    ;; Plot threads
    (org-scribe-context-pane--insert-heading (org-scribe-msg 'context-pane-threads))
    (if (null threads)
        (org-scribe-context-pane--insert-dim (org-scribe-msg 'context-pane-none))
      (dolist (thread threads)
        (org-scribe-context-pane--insert-name
         (plist-get thread :name) (plist-get thread :id) (plist-get thread :missing))
        (insert "\n")
        (when (plist-get thread :text)
          (insert "  " (plist-get thread :text) "\n"))))
    ;; Plot point
    (org-scribe-context-pane--insert-heading (org-scribe-msg 'context-pane-plot-point))
    (if (null points)
        (org-scribe-context-pane--insert-dim (org-scribe-msg 'context-pane-none))
      (dolist (point-data points)
        (org-scribe-context-pane--insert-name
         (plist-get point-data :name) (plist-get point-data :id)
         (plist-get point-data :missing))
        (insert "\n")
        (when (plist-get point-data :text)
          (insert "  " (plist-get point-data :text) "\n"))))
    ;; The scene's own dramatic properties
    (when props
      (org-scribe-context-pane--insert-heading (org-scribe-msg 'context-pane-scene))
      (dolist (prop props)
        (org-scribe-context-pane--insert-field
         (org-scribe-context-pane--property-label (car prop)) (cdr prop))))))

(defun org-scribe-context-pane--header (data)
  "Return the header-line string for DATA: outline path and word count."
  (let ((path (string-join (plist-get data :path) " › ")))
    (replace-regexp-in-string
     "%" "%%"
     (concat " " path "  ·  "
             (org-scribe-msg 'context-pane-words (plist-get data :words))))))

(defvar org-scribe-context-pane-mode)

(define-derived-mode org-scribe-context-pane-view-mode special-mode "Scene-context"
  "Major mode of the scene context pane buffer.  Read-only; q closes the pane."
  (setq-local truncate-lines nil)
  (visual-line-mode 1))

(declare-function org-scribe-context-pane-close "org-scribe-core")
(define-key org-scribe-context-pane-view-mode-map (kbd "q")
            #'org-scribe-context-pane-close)

(defun org-scribe-context-pane--buffer ()
  "Return the pane's buffer, creating it."
  (let ((buffer (get-buffer-create org-scribe-context-pane--buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-scribe-context-pane-view-mode)
        (org-scribe-context-pane-view-mode)))
    buffer))

(defun org-scribe-context-pane--draw (data)
  "Draw DATA (a scene plist, or nil for no scene) in the pane buffer."
  (let* ((buffer (org-scribe-context-pane--buffer))
         (window (get-buffer-window buffer))
         (old-point (with-current-buffer buffer (point)))
         (old-start (and window (window-start window)))
         (same-scene (and data (not (plist-get data :error))
                          (equal org-scribe-context-pane--drawn-id
                                      (plist-get data :id)))))
    (setq org-scribe-context-pane--drawn-id (and data (plist-get data :id)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (cond
         ((plist-get data :error)
          (setq header-line-format nil)
          (org-scribe-context-pane--insert-dim
           (org-scribe-msg 'context-pane-error (plist-get data :error))))
         ((plist-get data :no-scene)
          (setq header-line-format
                (and (plist-get data :path)
                     (replace-regexp-in-string
                      "%" "%%" (concat " " (string-join (plist-get data :path) " › ")))))
          (org-scribe-context-pane--insert-dim
           (org-scribe-msg 'context-pane-no-scene)))
         (data
          (setq header-line-format (org-scribe-context-pane--header data))
          (org-scribe-context-pane--insert-scene data))
         (t
          (setq header-line-format nil)
          (org-scribe-context-pane--insert-dim
           (org-scribe-msg 'context-pane-no-scene))))
        (goto-char (if same-scene (min old-point (point-max)) (point-min)))
        (when (and window same-scene)
          (set-window-start window (min old-start (point-max))))))))

;;; Refreshing

(defun org-scribe-context-pane--design-tick (&optional open)
  "Return the modification tick of the design notebook's buffer, or 0.
With OPEN, visit the notebook first, so that the tick taken is the one it will
have while it is read."
  (let* ((file (plist-get (org-scribe-project-structure) :design-file))
         (buffer (and file (or (find-buffer-visiting file)
                               (and open (file-exists-p file)
                                    (find-file-noselect file))))))
    (if buffer (buffer-chars-modified-tick buffer) 0)))

(defun org-scribe-context-pane--notebook-state (&optional open)
  "Return (FILE . TICK) for the design notebook; see `--tick-now'.  OPEN visits it."
  (cons (plist-get (org-scribe-project-structure) :design-file)
        (org-scribe-context-pane--design-tick open)))

(defun org-scribe-context-pane--refresh (&optional force)
  "Redraw the pane if what it shows changed, or always when FORCE."
  (let ((source org-scribe-context-pane--source))
    (when (and (buffer-live-p source)
               (get-buffer-window (org-scribe-context-pane--buffer)))
      (let* ((window (get-buffer-window source))
             (position (if window (window-point window)
                         (with-current-buffer source (point))))
             key data)
        (with-current-buffer source
          (org-with-wide-buffer
           (save-excursion
             (goto-char position)
             (let ((scene-id (save-excursion
                               (and (ignore-errors (org-scribe--goto-scene-heading) t)
                                    (or (org-entry-get nil "ID") (point))))))
               (setq key (list scene-id
                               (buffer-chars-modified-tick)
                               (org-scribe-context-pane--design-tick)
                               org-scribe-context-pane--cast-expanded))
               (when (or force (not (equal key org-scribe-context-pane--last-key)))
                 ;; Set the key first: a failing scene is not retried on
                 ;; every idle tick, only when something changes.
                 (setq org-scribe-context-pane--last-key key)
                 (condition-case err
                     (setq data (list (or (org-scribe-context-pane--scene-data)
                                          (org-scribe-context-pane--position-data))))
                   (error
                    (message "org-scribe context pane: %s"
                             (error-message-string err))
                    ;; Say what failed, not "not in a scene".
                    (setq data (list (list :error (error-message-string err))))))
                 ;; Collecting may have opened or touched the notebook, so
                 ;; take its tick again: otherwise the very next tick would
                 ;; see a change that was our own.
                 (setf (nth 2 key) (org-scribe-context-pane--design-tick))
                 (setq org-scribe-context-pane--last-key key))))))
        (when data
          (org-scribe-context-pane--draw (car data)))))))

(defun org-scribe-context-pane--tick ()
  "Idle-timer entry point; never signals."
  (condition-case nil
      (org-scribe-context-pane--refresh)
    (error nil)))

;;; The layout

(defun org-scribe-context-pane--setup ()
  "Create the two-window layout and start following point."
  (let* ((file (or (buffer-file-name)
                   (user-error "%s" (org-scribe-msg 'error-no-org-file))))
         (manuscript (plist-get (org-scribe-project-structure) :manuscript-file)))
    (unless (and manuscript (file-equal-p file manuscript))
      (user-error "%s" (org-scribe-msg 'context-pane-error-not-manuscript)))
    ;; A second enable must not leak the first one's timer.
    (when (timerp org-scribe-context-pane--timer)
      (cancel-timer org-scribe-context-pane--timer))
    (setq org-scribe-context-pane--saved-config (current-window-configuration)
          org-scribe-context-pane--source (current-buffer)
          org-scribe-context-pane--last-key nil
          org-scribe-context-pane--cast-expanded nil)
    (delete-other-windows)
    (let* ((width (org-scribe-window-perc
                   (min 0.6 (max 0.15 org-scribe-context-pane-width-percent))))
           (window (split-window-right (- (window-total-width) width))))
      (set-window-buffer window (org-scribe-context-pane--buffer))
      (with-current-buffer (org-scribe-context-pane--buffer)
        (setq-local org-scribe-workspace-redirect-buffer
                    org-scribe-context-pane--source))
      (set-window-dedicated-p window t))
    (add-hook 'kill-buffer-hook #'org-scribe-context-pane--source-killed nil t)
    (setq org-scribe-context-pane--timer
          (run-with-idle-timer org-scribe-context-pane-idle-delay t
                               #'org-scribe-context-pane--tick))
    (org-scribe-context-pane--refresh t)))

(defun org-scribe-context-pane--teardown ()
  "Stop following point, restore the windows and drop the pane buffer."
  (when (timerp org-scribe-context-pane--timer)
    (cancel-timer org-scribe-context-pane--timer))
  (remove-hook 'kill-buffer-hook #'org-scribe-context-pane--source-killed t)
  (setq org-scribe-context-pane--timer nil
        org-scribe-context-pane--source nil
        org-scribe-context-pane--last-key nil
        org-scribe-context-pane--drawn-id nil)
  (when-let* ((buffer (get-buffer org-scribe-context-pane--buffer-name)))
    (when-let* ((window (get-buffer-window buffer)))
      (set-window-dedicated-p window nil))
    (kill-buffer buffer))
  (when (window-configuration-p org-scribe-context-pane--saved-config)
    (set-window-configuration org-scribe-context-pane--saved-config))
  (setq org-scribe-context-pane--saved-config nil))

(defun org-scribe-context-pane--source-killed ()
  "Close the pane when the manuscript buffer it follows is killed."
  (when (eq (current-buffer) org-scribe-context-pane--source)
    (org-scribe-context-pane--teardown)))

;;;###autoload
(define-minor-mode org-scribe-context-pane-mode
  "Toggle the scene context pane beside the manuscript.

A read-only window that follows point and shows the facts the current scene
is built from; see the commentary of org-scribe-context-pane.el.  It is one
of the workspace layouts, so it excludes the others.  Disabling the mode
restores the previous window configuration."
  :init-value nil
  :lighter " Ctx"
  :global nil
  (if org-scribe-context-pane-mode
      (progn
        (org-scribe--deactivate-other-modes 'org-scribe-context-pane-mode)
        (condition-case err
            (org-scribe-context-pane--setup)
          (error
           ;; Put back whatever setup had already changed.
           (ignore-errors (org-scribe-context-pane--teardown))
           (setq org-scribe-context-pane-mode nil)
           (signal (car err) (cdr err)))))
    (org-scribe-context-pane--teardown)))

;; A revert (also an auto-revert after a sync) re-runs the major mode, which
;; would reset the flag and leave the pane and its timer without an owner.
(put 'org-scribe-context-pane-mode 'permanent-local t)
(put 'org-scribe-context-pane--source-killed 'permanent-local-hook t)

;; Register the layout.  This module loads only when the pane is enabled, so
;; the layout exists only then; it joins the mutually exclusive set too.
(unless (assq 'context org-scribe-workspace-layouts)
  (setq org-scribe-workspace-layouts
        (append org-scribe-workspace-layouts
                '((context . org-scribe-context-pane-mode)))))
(add-to-list 'org-scribe-exclusive-modes 'org-scribe-context-pane-mode)

(provide 'org-scribe-context-pane)

;;; org-scribe-context-pane.el ends here
