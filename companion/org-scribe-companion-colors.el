;;; org-scribe-companion-colors.el --- Visual companion: colour themes and pins -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; What colour the browser draws things in.  Pure data and functions, no
;; networking: the snapshot carries the result (`org-scribe-companion-colors-resolve')
;; and the page only looks colours up.
;;
;; Three layers, from the loosest to the most specific:
;;
;;   theme   a palette (the same length in a light and a dark variant) and
;;           colours for the TODO states, named by `# Companion-theme:' in
;;           .org-scribe-project.  The default theme has no TODO colours of
;;           its own: `org-scribe-companion-todo-colors' stays the source.
;;   assign  a value with no colour of its own (a role, a relationship
;;           type, a PoV character) takes the palette's slots in order, so
;;           no two share a colour until the palette runs out.
;;   pin     `# Companion-color: KIND #rrggbb VALUE' lines fix one item's
;;           colour.  KIND is `role', `reltype' or `pov' (VALUE last, so a
;;           role with spaces needs no quoting; a PoV is keyed by the
;;           character's ID so a rename keeps it; `plot', `location',
;;           `plotpoint' and `act' likewise by ID), or `warn'/`focus' (no
;;           VALUE: the colour of the page's warnings and focus ring).
;;
;; A pin is a free colour (#rrggbb, one colour for light and dark) or a
;; palette slot (slot:N, the theme's Nth colour in whichever mode the page is
;; in).  The picker's swatches pin slots, so a chosen swatch stays readable on
;; both pages; only the system colour dialog pins a hex value.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org-scribe-core)

(defvar org-scribe-companion-todo-colors)

;;; Themes

(defconst org-scribe-companion-colors--todo-slots
  '(("TODO" . 1) ("TOWRITE" . 0) ("TOREVIEW" . 5) ("REDO" . 3)
    ("RESTRUCTURE" . 6) ("FINISHED" . 4) ("PURGE" . 4)
    ("OPEN" . 0) ("EXPLORING" . 3) ("CHOSEN" . 4) ("REJECTED" . 4))
  "Which palette slot each shipped TODO keyword takes in a non-default theme.
Slot 4 is the neutral one in every palette, so a done state reads as
finished; the others are kept apart in every theme that has them.")

(defconst org-scribe-companion-colors--themes
  '((default
     :light ("#2b5f9e" "#b45309" "#2f7d4f" "#8e3b8e" "#6b7280" "#0f766e" "#7c3aed" "#a16207")
     :dark  ("#7fb0ec" "#e0883a" "#5cc082" "#c77bc7" "#a0a8b5" "#4fc3b5" "#a98bf5" "#d4a63a"))
    (okabe-ito
     ;; Okabe & Ito (2008), each colour moved in lightness until it keeps 3:1
     ;; against the page in its own mode and all eight stay apart when
     ;; simulated for deuteranopia (the test checks both).
     :light ("#0072b2" "#c38700" "#009e73" "#cc79a7" "#4b4b4b" "#4d9cc9" "#847d24" "#2f6380")
     :dark  ("#6fbfec" "#e69f00" "#008662" "#d55e00" "#c3c3c3" "#0072b2" "#847d24" "#eaae80"))
    (high-contrast
     :light ("#0030c0" "#b84a00" "#006b2e" "#a0005f" "#333333" "#006b7a" "#5a2ca0" "#6b4a00")
     :dark  ("#7aa2ff" "#ffa24d" "#5fe08a" "#ff7ac0" "#e6e6e6" "#4fd8e8" "#c4a0ff" "#e8c46a"))
    (muted
     :light ("#4f7399" "#a8794a" "#5f8a6a" "#9a6a88" "#6f6f6f" "#5a8a96" "#7d6b9a" "#8a7f4a")
     :dark  ("#8fb0d0" "#d3a97c" "#8fb89a" "#c79bb8" "#b0b0b0" "#8cbcc6" "#b3a0cf" "#c2b87c")))
  "Alist of (THEME :light PALETTE :dark PALETTE), PALETTEs lists of #rrggbb.
The two palettes of a theme have the same length, so one slot index serves
both modes.")

(defconst org-scribe-companion-colors--ui-defaults
  '((warn  :light "#b85c00" :dark "#e8913a")
    (focus :light "#b07d00" :dark "#f2c14e"))
  "The warning and focus colours when the project pins none, per mode.")

(defun org-scribe-companion-colors-theme-names ()
  "Return the theme symbols, default first."
  (mapcar #'car org-scribe-companion-colors--themes))

(defun org-scribe-companion-colors--theme-palette (theme mode)
  "Return THEME's palette for MODE (`:light' or `:dark'), a list."
  (plist-get (alist-get theme org-scribe-companion-colors--themes) mode))

;;; Reading the marker

(defconst org-scribe-companion-colors--id-kinds '(pov plot location plotpoint act)
  "The kinds whose VALUE is an entity or act ID, compared as written.
They colour scenes: by PoV character, plot thread, location, plot point or
act.  A rename keeps the colour because the pin holds the ID.")

(defconst org-scribe-companion-colors--kinds
  (append '(warn focus role reltype) org-scribe-companion-colors--id-kinds)
  "The kinds a `Companion-color' line can have.")

(defun org-scribe-companion-colors-hex-p (string)
  "Non-nil when STRING is a colour written #rrggbb."
  (and (stringp string) (string-match-p "\\`#[0-9a-fA-F]\\{6\\}\\'" string)))

(defun org-scribe-companion-colors-slot-p (string)
  "Non-nil when STRING is a palette slot written slot:N."
  (and (stringp string) (string-match-p "\\`slot:[0-9]+\\'" string)))

(defun org-scribe-companion-colors--slot-number (string)
  "Return N of the palette slot STRING, \"slot:N\"."
  (string-to-number (substring string 5)))

(defun org-scribe-companion-colors-color-p (string)
  "Non-nil when STRING is a colour a pin can hold: #rrggbb or slot:N."
  (or (org-scribe-companion-colors-hex-p string)
      (org-scribe-companion-colors-slot-p string)))

(defun org-scribe-companion-colors--normalize (kind value)
  "Return the key VALUE has among the pins of KIND.
A role or relationship type compares without case or edge spaces; the
kinds of `org-scribe-companion-colors--id-kinds' hold an ID, compared as
written."
  (let ((text (string-trim value)))
    (if (memq kind org-scribe-companion-colors--id-kinds) text (downcase text))))

(defun org-scribe-companion-colors--parse-pin (line)
  "Return (KIND KEY COLOR TEXT) for the Companion-color LINE, or nil if unreadable.
TEXT is the value as written (nil for `warn' and `focus'), KEY its
normalized form."
  (setq line (string-trim line))
  (when (string-match "\\`\\([a-z]+\\)[ \t]+\\(#[0-9a-fA-F]\\{6\\}\\|slot:[0-9]+\\)\\(?:[ \t]+\\(.*\\)\\)?\\'" line)
    (let* ((kind (intern-soft (downcase (match-string 1 line))))
           (color (downcase (match-string 2 line)))
           (text (and (match-string 3 line) (string-trim (match-string 3 line)))))
      (when (memq kind org-scribe-companion-colors--kinds)
        (cond
         ((memq kind '(warn focus)) (list kind nil color nil))
         ((and text (not (string-empty-p text)))
          (list kind (org-scribe-companion-colors--normalize kind text) color text)))))))

(defun org-scribe-companion-colors-theme (root)
  "Return the theme symbol ROOT's marker names, `default' when none or unknown."
  (let ((name (org-scribe--project-marker-get root "Companion-theme")))
    (or (and name (let ((theme (intern-soft name)))
                    (and theme (assq theme org-scribe-companion-colors--themes) theme)))
        'default)))

(defun org-scribe-companion-colors-pins (root)
  "Return the readable pins of ROOT as a list of (KIND KEY COLOR TEXT).
Lines that cannot be read are skipped, like every marker reader here."
  (delq nil (mapcar #'org-scribe-companion-colors--parse-pin
                    (org-scribe--project-marker-get-all root "Companion-color"))))

(defun org-scribe-companion-colors-revision (root)
  "Return a short string that changes when ROOT's colour settings do.
Colours live in the marker file, not in a buffer, so the buffer ticks
cannot tell the page that they changed."
  (substring (secure-hash 'md5 (format "%S"
                                       (list (org-scribe--project-marker-get root "Companion-theme")
                                             (org-scribe--project-marker-get-all root "Companion-color"))))
             0 8))

;;; Contrast of a pinned colour

(defconst org-scribe-companion-colors-surfaces '("#ffffff" "#222427")
  "The page's `--surface' in light and dark mode (companion/web/style.css).")

(defconst org-scribe-companion-colors-min-contrast 3.0
  "The contrast a colour keeps against the page (WCAG 1.4.11).")

(defun org-scribe-companion-colors--luminance (hex)
  "Return the relative luminance of HEX (#rrggbb)."
  (let ((linear (mapcar (lambda (start)
                          (let ((c (/ (string-to-number (substring hex start (+ start 2)) 16)
                                      255.0)))
                            (if (<= c 0.03928) (/ c 12.92) (expt (/ (+ c 0.055) 1.055) 2.4))))
                        '(1 3 5))))
    (+ (* 0.2126 (nth 0 linear)) (* 0.7152 (nth 1 linear)) (* 0.0722 (nth 2 linear)))))

(defun org-scribe-companion-colors-contrast (hex)
  "Return the lower of HEX's contrast ratios against the page in both modes."
  (let ((l (org-scribe-companion-colors--luminance hex)))
    (apply #'min
           (mapcar (lambda (surface)
                     (let ((ls (org-scribe-companion-colors--luminance surface)))
                       (/ (+ (max l ls) 0.05) (+ (min l ls) 0.05))))
                   org-scribe-companion-colors-surfaces))))

;;; Writing the marker

(defun org-scribe-companion-colors-set-pin (root kind value color)
  "Pin COLOR (#rrggbb, or nil to remove) for VALUE of KIND in ROOT's marker.
KIND is `warn', `focus', `role', `reltype' or `pov'; VALUE is ignored for
the first two.  Any earlier pin for the same item goes, and its spelling of VALUE is kept."
  (let* ((key (and (not (memq kind '(warn focus)))
                   (org-scribe-companion-colors--normalize kind value)))
         (spelling (and key (string-trim value)))
         (lines (cl-loop for line in (org-scribe--project-marker-get-all root "Companion-color")
                         for pin = (org-scribe-companion-colors--parse-pin line)
                         ;; unreadable lines are the writer's: keep them
                         if (and pin (eq (nth 0 pin) kind) (equal (nth 1 pin) key))
                         do (setq spelling (nth 3 pin))   ; keep how it was first written
                         else collect line)))
    (when color
      (setq lines (append lines
                          (list (if key
                                    (format "%s %s %s" kind (downcase color) spelling)
                                  (format "%s %s" kind (downcase color)))))))
    (org-scribe--project-marker-set-all root "Companion-color" lines)))

(defun org-scribe-companion-colors-clear-pins (root)
  "Remove every pin from ROOT's marker file; the theme stays."
  (org-scribe--project-marker-set-all root "Companion-color" nil))

(defun org-scribe-companion-colors-set-theme (root theme)
  "Record THEME, a symbol, as ROOT's theme."
  (org-scribe--project-marker-set root "Companion-theme" (symbol-name theme)))

;;; TODO colours

(defun org-scribe-companion-colors-todo (root)
  "Return (LIGHT . DARK), two alists of (KEYWORD . COLOR), for ROOT's theme.
The default theme gives `org-scribe-companion-todo-colors' in both modes.
Another theme colours the keywords of `org-scribe-companion-colors--todo-slots';
any other keyword keeps the customized colour."
  (let* ((theme (org-scribe-companion-colors-theme root))
         (custom (copy-alist org-scribe-companion-todo-colors))
         (light (org-scribe-companion-colors--theme-palette theme :light))
         (dark (org-scribe-companion-colors--theme-palette theme :dark)))
    (if (eq theme 'default)
        (cons custom custom)
      (let ((fixed-light (mapcar (lambda (slot) (cons (car slot) (nth (cdr slot) light)))
                                 org-scribe-companion-colors--todo-slots))
            (fixed-dark (mapcar (lambda (slot) (cons (car slot) (nth (cdr slot) dark)))
                                org-scribe-companion-colors--todo-slots)))
        (cons (append fixed-light
                      (cl-remove-if (lambda (entry) (assoc (car entry) fixed-light)) custom))
              (append fixed-dark
                      (cl-remove-if (lambda (entry) (assoc (car entry) fixed-dark)) custom)))))))

;;; Assigning palette slots

(defun org-scribe-companion-colors--hash (key)
  "Return a number from KEY that is the same in every session and project."
  (string-to-number (substring (secure-hash 'md5 key) 0 6) 16))

(defun org-scribe-companion-colors-assign (keys pinned palette)
  "Return an alist (KEY . ENTRY) giving every one of KEYS a colour.
KEYS is the list of normalized values, PINNED an alist (KEY . COLOR) of the
ones with a colour of their own, COLOR being #rrggbb (ENTRY (:color COLOR
:rank N)) or slot:N (ENTRY (:index N :rank N :pinned t)).
The rest take one of PALETTE's slots (a list of colours), skipping a slot
a pin already uses; their ENTRY is (:index N :rank N), plus (:reused t)
once every slot is taken and a colour repeats.

A value's slot starts from a hash of the value itself, not from its place
in the list, so adding a value does not recolour the others.  Only a value
whose slot is already taken moves on to the next free one, and values are
settled in hash order, so the result does not depend on the order of KEYS.
:rank is the same hash, for the cues that do not depend on colour (line
pattern, node shape).  The result follows the order of KEYS."
  (let* ((taken (mapcar (lambda (pin) (downcase (cdr pin))) pinned))
         (slot-pins (cl-loop for pin in pinned
                             when (org-scribe-companion-colors-slot-p (cdr pin))
                             collect (org-scribe-companion-colors--slot-number (cdr pin))))
         (slots (cl-loop for color in palette
                         for index from 0
                         unless (or (member (downcase color) taken) (memq index slot-pins))
                         collect index))
         (slots (or slots (number-sequence 0 (1- (length palette)))))
         (count (length slots))
         (used nil)
         (chosen nil))
    (dolist (item (sort (cl-loop for key in keys
                                 unless (assoc key pinned)
                                 collect (cons (org-scribe-companion-colors--hash key) key))
                        (lambda (a b) (or (< (car a) (car b))
                                          (and (= (car a) (car b)) (string< (cdr a) (cdr b)))))))
      (let* ((hash (car item))
             (start (mod hash count))
             (slot (cl-loop for step below count
                            for candidate = (nth (mod (+ start step) count) slots)
                            unless (memq candidate used) return candidate))
             (reused (null slot)))
        (setq slot (or slot (nth start slots)))
        (unless reused (push slot used))
        (push (cons (cdr item) (append (list :index slot :rank hash)
                                       (and reused (list :reused t))))
              chosen)))
    (mapcar (lambda (key)
              (if-let* ((pin (assoc key pinned)))
                  (cons key
                        (if (org-scribe-companion-colors-slot-p (cdr pin))
                            (list :index (mod (org-scribe-companion-colors--slot-number (cdr pin))
                                              (max 1 (length palette)))
                                  :rank (org-scribe-companion-colors--hash key)
                                  :pinned t)
                          (list :color (cdr pin)
                                :rank (org-scribe-companion-colors--hash key))))
                (cons key (cdr (assoc key chosen)))))
            keys)))

;;; The snapshot's `colors'

(defun org-scribe-companion-colors--ids (entities kind)
  "Return the IDs of the visible ENTITIES of KIND, in notebook order.
KIND is a keyword of the snapshot's entities plist.  An entity hidden by a
negative weight gets no colour, so it never takes a palette slot."
  (cl-loop for entity across (or (plist-get entities kind) [])
           for id = (plist-get entity :id)
           when (and (stringp id) (not (eq (plist-get entity :hidden) t)))
           collect id))

(defun org-scribe-companion-colors--values (entities &optional acts)
  "Return (ROLES RELTYPES POVS PLOTS LOCATIONS PLOTPOINTS ACTS) for ENTITIES.
ENTITIES is the snapshot's entities plist, ACTS its list of acts.  Each is
the list of normalized values to colour, in the order slots are handed out:
roles and types alphabetical, the rest in notebook or manuscript order."
  (let* ((characters (append (plist-get entities :character) nil))
         (roles (cl-loop for character in characters
                         for role = (plist-get character :role)
                         when (and (stringp role) (not (string-empty-p (string-trim role))))
                         collect (org-scribe-companion-colors--normalize 'role role)))
         ;; Only the types in use: the suggestion list has twenty, which would
         ;; run the palette round before the writer had drawn one line.
         (types (cl-loop for character in characters
                         append (cl-loop for rel across (or (plist-get character :relationships) [])
                                         for type = (plist-get rel :type)
                                         when (and (stringp type) (not (string-empty-p type)))
                                         collect (org-scribe-companion-colors--normalize 'reltype type))))
         (povs (cl-loop for character in characters
                        for id = (plist-get character :id)
                        when (stringp id) collect id)))
    (list (sort (delete-dups roles) #'string<)
          (sort (delete-dups types) #'string<)
          (delete-dups povs)
          (org-scribe-companion-colors--ids entities :plot)
          (org-scribe-companion-colors--ids entities :location)
          (org-scribe-companion-colors--ids entities :plot-point)
          (cl-loop for act in (append acts nil)
                   for id = (plist-get act :id)
                   when (stringp id) collect id))))

(defun org-scribe-companion-colors--entries-plist (entries)
  "Return ENTRIES, an alist (KEY . PLIST), as a plist keyed by keywords."
  (cl-loop for (key . entry) in entries
           append (list (intern (concat ":" key)) entry)))

(defun org-scribe-companion-colors-resolve (root entities &optional acts)
  "Return the snapshot's `colors' plist for ROOT, whose entities are ENTITIES.
ACTS is the snapshot's list of acts.  Covers every role, relationship type,
PoV character, plot thread, location, plot point and act of the project, not
only those on screen, so a filter never moves a colour."
  (let* ((theme (org-scribe-companion-colors-theme root))
         (pins (org-scribe-companion-colors-pins root))
         (light (org-scribe-companion-colors--theme-palette theme :light))
         (values (org-scribe-companion-colors--values entities acts))
         (pinned-of (lambda (kind)
                      (cl-loop for pin in pins
                               when (eq (nth 0 pin) kind) collect (cons (nth 1 pin) (nth 2 pin)))))
         (dark (org-scribe-companion-colors--theme-palette theme :dark))
         (ui (lambda (kind)
               (let* ((defaults (alist-get kind org-scribe-companion-colors--ui-defaults))
                      (pin (cl-find kind pins :key #'car))
                      (chosen (and pin (nth 2 pin)))
                      ;; a slot is the theme's colour of that number in each mode
                      (slot (and chosen (org-scribe-companion-colors-slot-p chosen)
                                 (mod (org-scribe-companion-colors--slot-number chosen)
                                      (length light)))))
                 (list :light (cond (slot (nth slot light))
                                    (chosen chosen)
                                    (t (plist-get defaults :light)))
                       :dark (cond (slot (nth slot dark))
                                   (chosen chosen)
                                   (t (plist-get defaults :dark)))
                       :pinned (if chosen t :false))))))
    (list :theme (symbol-name theme)
          :themes (vconcat (mapcar #'symbol-name (org-scribe-companion-colors-theme-names)))
          :palettes (cl-loop for (name . _) in org-scribe-companion-colors--themes
                             append (list (intern (concat ":" (symbol-name name)))
                                          (list :light (vconcat (org-scribe-companion-colors--theme-palette name :light))
                                                :dark (vconcat (org-scribe-companion-colors--theme-palette name :dark)))))
          :palette (list :light (vconcat light)
                         :dark (vconcat (org-scribe-companion-colors--theme-palette theme :dark)))
          :warn (funcall ui 'warn)
          :focus (funcall ui 'focus)
          :role (org-scribe-companion-colors--entries-plist
                 (org-scribe-companion-colors-assign (nth 0 values) (funcall pinned-of 'role) light))
          :reltype (org-scribe-companion-colors--entries-plist
                    (org-scribe-companion-colors-assign (nth 1 values) (funcall pinned-of 'reltype) light))
          :pov (org-scribe-companion-colors--entries-plist
                (org-scribe-companion-colors-assign (nth 2 values) (funcall pinned-of 'pov) light))
          :plot (org-scribe-companion-colors--entries-plist
                 (org-scribe-companion-colors-assign (nth 3 values) (funcall pinned-of 'plot) light))
          :location (org-scribe-companion-colors--entries-plist
                     (org-scribe-companion-colors-assign (nth 4 values) (funcall pinned-of 'location) light))
          :plotpoint (org-scribe-companion-colors--entries-plist
                      (org-scribe-companion-colors-assign (nth 5 values) (funcall pinned-of 'plotpoint) light))
          :act (org-scribe-companion-colors--entries-plist
                (org-scribe-companion-colors-assign (nth 6 values) (funcall pinned-of 'act) light))
          :pins (vconcat (cl-loop for pin in pins
                                  collect (list :kind (symbol-name (nth 0 pin))
                                                :value (or (nth 3 pin) :null)
                                                :color (nth 2 pin)))))))

(provide 'org-scribe-companion-colors)

;;; org-scribe-companion-colors.el ends here
