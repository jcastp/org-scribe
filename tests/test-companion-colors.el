;;; test-companion-colors.el --- Tests for the companion's colour selector -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for `companion/org-scribe-companion-colors.el', the marker accessors
;; it rests on (`org-scribe--project-marker-get-all' / `-set-all'), the
;; snapshot's `colors' entry and the `set-color' command.
;;
;; The theme tests are what makes "colour-blind safe" a checked claim: every
;; palette keeps 3:1 contrast against the page in its own mode (WCAG 1.4.11),
;; and the Okabe-Ito one is simulated for deuteranopia (Machado et al. 2009,
;; severity 1.0) and must keep every colour apart in CIELAB.

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
(require 'org-scribe-companion)

(defconst test-companion-colors--repo
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the org-scribe checkout.")

(defmacro test-companion-colors--with-project (root-var &rest body)
  "Run BODY with a copy of the demo novel at ROOT-VAR (not serving)."
  (declare (indent 1))
  `(let* ((,root-var (file-name-as-directory (make-temp-file "test-companion-colors-" t)))
          (default-directory ,root-var)
          (org-id-locations-file (expand-file-name "ids" ,root-var))
          (make-backup-files nil)
          (create-lockfiles nil)
          (org-scribe-companion-raise-frame nil))
     (unwind-protect
         (save-window-excursion
           (copy-directory (expand-file-name "examples/demo-novel" test-companion-colors--repo)
                           ,root-var t t t)
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

(defun test-companion-colors--marker (root)
  "Return the text of ROOT's marker file."
  (with-temp-buffer
    (insert-file-contents (expand-file-name ".org-scribe-project" root))
    (buffer-string)))

(defun test-companion-colors--run (root op &rest args)
  (org-scribe-companion-commands-run op args root))

;;; Colour arithmetic for the theme checks

(defun test-companion-colors--linear (channel)
  (if (<= channel 0.04045) (/ channel 12.92) (expt (/ (+ channel 0.055) 1.055) 2.4)))

(defun test-companion-colors--rgb (hex)
  "Return HEX (#rrggbb) as a list of linear-light r g b in 0..1."
  (mapcar (lambda (start)
            (test-companion-colors--linear
             (/ (string-to-number (substring hex start (+ start 2)) 16) 255.0)))
          '(1 3 5)))

(defun test-companion-colors--luminance (hex)
  (cl-destructuring-bind (r g b) (test-companion-colors--rgb hex)
    (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))

(defun test-companion-colors--contrast (a b)
  (let ((la (test-companion-colors--luminance a))
        (lb (test-companion-colors--luminance b)))
    (/ (+ (max la lb) 0.05) (+ (min la lb) 0.05))))

(defun test-companion-colors--deuteranopia (hex)
  "Return linear r g b of HEX as a deuteranope sees it (Machado 2009, 1.0)."
  (let ((c (test-companion-colors--rgb hex))
        (m '((0.367322 0.860646 -0.227968)
             (0.280085 0.672501 0.047413)
             (-0.011820 0.042940 0.968881))))
    (mapcar (lambda (row) (max 0.0 (min 1.0 (cl-reduce #'+ (cl-mapcar #'* row c))))) m)))

(defun test-companion-colors--lab (linear)
  "Return CIELAB of LINEAR, a list of linear-light r g b."
  (cl-destructuring-bind (r g b) linear
    (let* ((x (/ (+ (* 0.4124 r) (* 0.3576 g) (* 0.1805 b)) 0.95047))
           (y (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)))
           (z (/ (+ (* 0.0193 r) (* 0.1192 g) (* 0.9505 b)) 1.08883))
           (f (lambda (tt) (if (> tt 0.008856) (expt tt (/ 1.0 3)) (+ (* 7.787 tt) (/ 16.0 116)))))
           (fx (funcall f x)) (fy (funcall f y)) (fz (funcall f z)))
      (list (- (* 116 fy) 16) (* 500 (- fx fy)) (* 200 (- fy fz))))))

(defun test-companion-colors--distance (a b)
  "Return the CIELAB distance between A and B as a deuteranope sees them."
  (sqrt (cl-reduce #'+ (cl-mapcar (lambda (p q) (expt (- p q) 2))
                                  (test-companion-colors--lab (test-companion-colors--deuteranopia a))
                                  (test-companion-colors--lab (test-companion-colors--deuteranopia b))))))

(defun test-companion-colors--min-distance (colors)
  (let ((least 1000.0))
    (cl-loop for (a . rest) on colors
             do (dolist (b rest) (setq least (min least (test-companion-colors--distance a b)))))
    least))

(defconst test-companion-colors--surface '(:light "#ffffff" :dark "#222427")
  "The page's --surface in each mode (companion/web/style.css).")

;;; Marker accessors

(ert-deftest test-companion-colors-marker-get-all-returns-every-line-in-order ()
  (test-companion-colors--with-project root
    (with-temp-file (expand-file-name ".org-scribe-project" root)
      (insert "# Type: novel\n# Tag: one\n# Other: x\n# tag: two  \n"))
    (should (equal (org-scribe--project-marker-get-all root "Tag") '("one" "two")))
    (should-not (org-scribe--project-marker-get-all root "Missing"))
    (should-not (org-scribe--project-marker-get-all (expand-file-name "nowhere/" root) "Tag"))))

(ert-deftest test-companion-colors-marker-set-all-replaces-in-place-and-keeps-the-rest ()
  (test-companion-colors--with-project root
    (with-temp-file (expand-file-name ".org-scribe-project" root)
      (insert "# Type: novel\n# Tag: one\n# Other: x\n# Tag: two\n# Last: y\n"))
    (org-scribe--project-marker-set-all root "Tag" '("a b" "c"))
    (should (equal (test-companion-colors--marker root)
                   "# Type: novel\n# Tag: a b\n# Tag: c\n# Other: x\n# Last: y\n"))
    (org-scribe--project-marker-set-all root "Tag" nil)
    (should (equal (test-companion-colors--marker root)
                   "# Type: novel\n# Other: x\n# Last: y\n"))
    (org-scribe--project-marker-set-all root "New" '("v"))
    (should (string-suffix-p "# Last: y\n# New: v\n" (test-companion-colors--marker root)))))

(ert-deftest test-companion-colors-marker-set-all-is-a-no-op-without-a-marker-file ()
  (test-companion-colors--with-project root
    (let ((nowhere (expand-file-name "nowhere/" root)))
      (make-directory nowhere)
      (org-scribe--project-marker-set-all nowhere "Tag" '("a"))
      (should-not (file-exists-p (expand-file-name ".org-scribe-project" nowhere))))))

;;; Themes

(ert-deftest test-companion-colors-themes-have-equal-light-and-dark-palettes ()
  (should (eq (car (org-scribe-companion-colors-theme-names)) 'default))
  (dolist (theme (org-scribe-companion-colors-theme-names))
    (let ((light (org-scribe-companion-colors--theme-palette theme :light))
          (dark (org-scribe-companion-colors--theme-palette theme :dark)))
      (should (= (length light) (length dark)))
      (should (>= (length light) 7))     ; the TODO slots reach index 6
      (dolist (color (append light dark))
        (should (org-scribe-companion-colors-hex-p color))))))

(ert-deftest test-companion-colors-every-palette-colour-reads-against-the-page ()
  (dolist (theme (org-scribe-companion-colors-theme-names))
    (dolist (mode '(:light :dark))
      (dolist (color (org-scribe-companion-colors--theme-palette theme mode))
        (should (>= (test-companion-colors--contrast color (plist-get test-companion-colors--surface mode))
                    3.0))))))

(ert-deftest test-companion-colors-warning-and-focus-defaults-read-against-the-page ()
  (dolist (kind '(warn focus))
    (dolist (mode '(:light :dark))
      (let ((color (plist-get (alist-get kind org-scribe-companion-colors--ui-defaults) mode)))
        (should (>= (test-companion-colors--contrast color (plist-get test-companion-colors--surface mode))
                    3.0))))))

(ert-deftest test-companion-colors-okabe-ito-palette-survives-deuteranopia ()
  (dolist (mode '(:light :dark))
    (let ((palette (org-scribe-companion-colors--theme-palette 'okabe-ito mode)))
      (should (>= (test-companion-colors--min-distance palette) 15.0)))))

(ert-deftest test-companion-colors-okabe-ito-work-states-survive-deuteranopia ()
  "The states a writer tells apart at a glance stay apart; done states may match."
  (let ((work '("TODO" "TOWRITE" "TOREVIEW" "REDO" "RESTRUCTURE" "FINISHED")))
    (dolist (mode '(:light :dark))
      (let ((palette (org-scribe-companion-colors--theme-palette 'okabe-ito mode)))
        (should (>= (test-companion-colors--min-distance
                     (mapcar (lambda (keyword)
                               (nth (cdr (assoc keyword org-scribe-companion-colors--todo-slots)) palette))
                             work))
                    15.0))))))

(ert-deftest test-companion-colors-todo-slots-cover-the-shipped-keywords ()
  (dolist (entry org-scribe-companion-todo-colors)
    (should (assoc (car entry) org-scribe-companion-colors--todo-slots))))

(ert-deftest test-companion-colors-default-theme-todo-colours-are-the-defcustom ()
  (test-companion-colors--with-project root
    (let ((colors (org-scribe-companion-colors-todo root)))
      (should (equal (car colors) org-scribe-companion-todo-colors))
      (should (equal (cdr colors) org-scribe-companion-todo-colors)))))

(ert-deftest test-companion-colors-other-theme-recolours-todo-states-and-keeps-custom-ones ()
  (test-companion-colors--with-project root
    (let ((org-scribe-companion-todo-colors
           (append org-scribe-companion-todo-colors '(("MINE" . "#123456")))))
      (org-scribe-companion-colors-set-theme root 'okabe-ito)
      (let ((colors (org-scribe-companion-colors-todo root)))
        (should (equal (cdr (assoc "TOWRITE" (car colors)))
                       (nth 0 (org-scribe-companion-colors--theme-palette 'okabe-ito :light))))
        (should (equal (cdr (assoc "TOWRITE" (cdr colors)))
                       (nth 0 (org-scribe-companion-colors--theme-palette 'okabe-ito :dark))))
        (should (equal (cdr (assoc "MINE" (car colors))) "#123456"))
        (should (equal (cdr (assoc "MINE" (cdr colors))) "#123456"))))))

;;; Reading

(ert-deftest test-companion-colors-unknown-or-missing-theme-reads-as-default ()
  (test-companion-colors--with-project root
    (should (eq (org-scribe-companion-colors-theme root) 'default))
    (org-scribe--project-marker-set root "Companion-theme" "nonsense")
    (should (eq (org-scribe-companion-colors-theme root) 'default))
    (org-scribe--project-marker-set root "Companion-theme" "muted")
    (should (eq (org-scribe-companion-colors-theme root) 'muted))))

(ert-deftest test-companion-colors-malformed-pin-lines-are-ignored ()
  (test-companion-colors--with-project root
    (org-scribe--project-marker-set-all
     root "Companion-color"
     '("role #zzzzzz Mentor" "wizard #112233 Merlin" "role #112233" "role red Mentor"
       "reltype #AABBCC rival" "warn #445566" "role #112233 Love interest"))
    (should (equal (org-scribe-companion-colors-pins root)
                   '((reltype "rival" "#aabbcc" "rival")
                     (warn nil "#445566" nil)
                     (role "love interest" "#112233" "Love interest"))))))

;;; Writing pins

(ert-deftest test-companion-colors-set-pin-replaces-the-same-item-ignoring-case ()
  (test-companion-colors--with-project root
    (org-scribe-companion-colors-set-pin root 'role "Mentor" "#AA0000")
    (org-scribe-companion-colors-set-pin root 'role "Ally" "#00AA00")
    (org-scribe-companion-colors-set-pin root 'role "mentor" "#0000AA")
    (should (equal (org-scribe--project-marker-get-all root "Companion-color")
                   '("role #00aa00 Ally" "role #0000aa Mentor")))
    (org-scribe-companion-colors-set-pin root 'role "MENTOR" nil)
    (should (equal (org-scribe--project-marker-get-all root "Companion-color")
                   '("role #00aa00 Ally")))))

(ert-deftest test-companion-colors-set-pin-keeps-lines-it-cannot-read ()
  (test-companion-colors--with-project root
    (org-scribe--project-marker-set-all root "Companion-color" '("role oops Mentor"))
    (org-scribe-companion-colors-set-pin root 'warn nil "#112233")
    (should (equal (org-scribe--project-marker-get-all root "Companion-color")
                   '("role oops Mentor" "warn #112233")))))

(ert-deftest test-companion-colors-clear-pins-keeps-the-theme ()
  (test-companion-colors--with-project root
    (org-scribe-companion-colors-set-theme root 'muted)
    (org-scribe-companion-colors-set-pin root 'focus nil "#112233")
    (org-scribe-companion-colors-clear-pins root)
    (should-not (org-scribe-companion-colors-pins root))
    (should (eq (org-scribe-companion-colors-theme root) 'muted))))

;;; Assignment

(defun test-companion-colors--index (result key)
  (plist-get (cdr (assoc key result)) :index))

(ert-deftest test-companion-colors-assign-gives-distinct-slots-and-keeps-the-order ()
  (let* ((palette '("#1" "#2" "#3" "#4" "#5" "#6" "#7" "#8"))
         (result (org-scribe-companion-colors-assign '("a" "b" "c" "d") nil palette)))
    (should (equal (mapcar #'car result) '("a" "b" "c" "d")))
    (should (= 4 (length (delete-dups (mapcar (lambda (e) (plist-get (cdr e) :index)) result)))))
    (should-not (cl-some (lambda (e) (plist-get (cdr e) :reused)) result))))

(ert-deftest test-companion-colors-assign-skips-slots-a-pin-uses ()
  (let* ((palette '("#1" "#2" "#3" "#4"))
         (result (org-scribe-companion-colors-assign '("a" "b" "c") '(("b" . "#2")) palette)))
    (should (equal (plist-get (cdr (assoc "b" result)) :color) "#2"))
    (should-not (eql 1 (test-companion-colors--index result "a")))
    (should-not (eql 1 (test-companion-colors--index result "c")))))

(ert-deftest test-companion-colors-assign-marks-repeats-when-the-palette-runs-out ()
  (let ((result (org-scribe-companion-colors-assign '("a" "b" "c") nil '("#1" "#2"))))
    (should (= 1 (cl-count-if (lambda (e) (plist-get (cdr e) :reused)) result)))
    (should (= 2 (length (delete-dups (mapcar (lambda (e) (plist-get (cdr e) :index)) result)))))))

(ert-deftest test-companion-colors-assign-does-not-depend-on-the-order-of-the-keys ()
  (let ((palette '("#1" "#2" "#3" "#4" "#5" "#6" "#7" "#8")))
    (should (equal (sort (copy-sequence (org-scribe-companion-colors-assign '("a" "b" "c" "d" "e") nil palette))
                         (lambda (x y) (string< (car x) (car y))))
                   (sort (copy-sequence (org-scribe-companion-colors-assign '("e" "c" "a" "d" "b") nil palette))
                         (lambda (x y) (string< (car x) (car y))))))))

(ert-deftest test-companion-colors-assign-adding-a-value-leaves-most-others-alone ()
  "A new value that sorts first used to shift every later one."
  (let* ((palette '("#1" "#2" "#3" "#4" "#5" "#6" "#7" "#8"))
         (before (org-scribe-companion-colors-assign
                  '("ally" "friend" "opponent" "partner" "rival") nil palette))
         (after (org-scribe-companion-colors-assign
                 '("ally" "amigo" "friend" "opponent" "partner" "rival") nil palette))
         (same (cl-count-if (lambda (e) (equal (cdr e) (cdr (assoc (car e) after)))) before)))
    ;; only a value whose slot the new one takes may move
    (should (>= same 4))))

(ert-deftest test-companion-colors-assign-rank-belongs-to-the-value ()
  (let ((palette '("#1" "#2" "#3" "#4")))
    (should (= (plist-get (cdr (assoc "rival" (org-scribe-companion-colors-assign '("rival") nil palette))) :rank)
               (plist-get (cdr (assoc "rival" (org-scribe-companion-colors-assign
                                               '("amigo" "rival" "ally") '(("ally" . "#9") ) palette))) :rank)))))

(ert-deftest test-companion-colors-assign-survives-a-palette-that-is-all-pinned ()
  (let ((result (org-scribe-companion-colors-assign
                 '("a" "b") '(("x" . "#1") ("y" . "#2")) '("#1" "#2"))))
    (should (equal (mapcar #'car result) '("a" "b")))
    (should (memq (plist-get (cdr (assoc "a" result)) :index) '(0 1)))))

;;; Snapshot

(ert-deftest test-companion-colors-snapshot-carries-the-colours ()
  (test-companion-colors--with-project root
    (let* ((snapshot (org-scribe-companion--snapshot root))
           (colors (plist-get snapshot :colors)))
      (should (equal (plist-get colors :theme) "default"))
      (should (= (length (plist-get colors :themes)) 4))
      (should (= (length (plist-get (plist-get colors :palette) :light))
                 (length (plist-get (plist-get colors :palette) :dark))))
      (should (eq (plist-get (plist-get colors :warn) :pinned) :false))
      ;; the demo's characters have roles; each gets its own colour
      (let ((roles (cl-loop for (_ entry) on (plist-get colors :role) by #'cddr collect entry)))
        (should roles)
        (should (cl-every (lambda (entry) (plist-get entry :index)) roles))
        (should (= (length roles) (length (cl-remove-duplicates (mapcar (lambda (e) (plist-get e :index)) roles)))))))))

(ert-deftest test-companion-colors-snapshot-covers-every-pov-character-whatever-the-filters ()
  (test-companion-colors--with-project root
    (let* ((snapshot (org-scribe-companion--snapshot root))
           (characters (append (plist-get (plist-get snapshot :entities) :character) nil))
           (pov (plist-get (plist-get snapshot :colors) :pov)))
      (should characters)
      (should (= (/ (length pov) 2) (length (cl-remove-if-not (lambda (c) (stringp (plist-get c :id))) characters)))))))

(ert-deftest test-companion-colors-snapshot-serializes-to-json ()
  (test-companion-colors--with-project root
    (org-scribe-companion-colors-set-pin root 'role "Protagonist" "#aa0000")
    (let* ((json (org-scribe-companion--snapshot-json root))
           (data (json-parse-string json :object-type 'alist)))
      (should (equal (alist-get 'theme (alist-get 'colors data)) "default"))
      (should (equal (alist-get 'color (alist-get 'protagonist (alist-get 'role (alist-get 'colors data))))
                     "#aa0000"))
      (should (alist-get 'colorsDark (alist-get 'todo data))))))

(ert-deftest test-companion-colors-colour-change-changes-the-revision ()
  (test-companion-colors--with-project root
    (let ((before (org-scribe-companion--revision-string root)))
      (should (string-match-p "-c[0-9a-f]\\{8\\}\\'" before))
      (org-scribe-companion-colors-set-theme root 'muted)
      (should-not (equal before (org-scribe-companion--revision-string root)))
      (should (equal (plist-get (org-scribe-companion--snapshot root) :revision)
                     (org-scribe-companion--revision-string root))))))

(ert-deftest test-companion-colors-snapshot-follows-a-hand-edit-of-the-marker ()
  (test-companion-colors--with-project root
    (should (equal (plist-get (plist-get (org-scribe-companion--snapshot root) :colors) :theme) "default"))
    (org-scribe--project-marker-set root "Companion-theme" "high-contrast")
    (let ((snapshot (org-scribe-companion--snapshot root)))
      (should (equal (plist-get (plist-get snapshot :colors) :theme) "high-contrast"))
      (should (equal (plist-get (plist-get snapshot :todo) :colors)
                     (cl-loop for (keyword . color) in (car (org-scribe-companion-colors-todo root))
                              append (list (org-scribe-companion--keyword keyword) color)))))))

;;; set-color

(ert-deftest test-companion-colors-set-color-theme-and-pins ()
  (test-companion-colors--with-project root
    (let ((answer (test-companion-colors--run root "set-color" :kind "theme" :color "okabe-ito")))
      (should (eq (plist-get answer :ok) t))
      (should (stringp (plist-get answer :revision))))
    (should (eq (org-scribe-companion-colors-theme root) 'okabe-ito))
    (should (eq (plist-get (test-companion-colors--run root "set-color" :kind "warn" :color "#AB12CD") :ok) t))
    (should (eq (plist-get (test-companion-colors--run root "set-color" :kind "reltype"
                                                       :value "  Rival " :color "#112233")
                           :ok)
                t))
    (let ((colors (plist-get (org-scribe-companion--snapshot root) :colors)))
      (should (equal (plist-get (plist-get colors :warn) :light) "#ab12cd"))
      (should (eq (plist-get (plist-get colors :warn) :pinned) t)))
    (should (member "reltype #112233 Rival" (org-scribe--project-marker-get-all root "Companion-color")))
    ;; an empty colour removes the pin
    (test-companion-colors--run root "set-color" :kind "warn" :color "")
    (should-not (cl-find 'warn (org-scribe-companion-colors-pins root) :key #'car))
    ;; reset all
    (test-companion-colors--run root "set-color" :kind "all")
    (should-not (org-scribe-companion-colors-pins root))
    (should (eq (org-scribe-companion-colors-theme root) 'okabe-ito))))

(ert-deftest test-companion-colors-set-color-warns-about-a-low-contrast-pin ()
  (test-companion-colors--with-project root
    (let ((answer (test-companion-colors--run root "set-color" :kind "role"
                                              :value "Protagonist" :color "#00ff00")))
      (should (eq (plist-get answer :ok) t))
      (should (string-match-p "#00ff00" (plist-get answer :warning)))
      (should (member "role #00ff00 Protagonist"
                      (org-scribe--project-marker-get-all root "Companion-color"))))
    (let ((answer (test-companion-colors--run root "set-color" :kind "role"
                                              :value "Protagonist" :color "#808080")))
      (should (eq (plist-get answer :ok) t))
      (should-not (plist-get answer :warning)))))

(ert-deftest test-companion-colors-a-swatch-pins-a-slot-that-follows-the-mode ()
  (test-companion-colors--with-project root
    (let ((answer (test-companion-colors--run root "set-color" :kind "role"
                                              :value "Protagonist" :color "slot:3")))
      (should (eq (plist-get answer :ok) t))
      (should-not (plist-get answer :warning)))
    (should (member "role slot:3 Protagonist"
                    (org-scribe--project-marker-get-all root "Companion-color")))
    (should (equal '((role "protagonist" "slot:3" "Protagonist"))
                   (org-scribe-companion-colors-pins root)))
    (let ((entry (cl-getf (plist-get (plist-get (org-scribe-companion--snapshot root) :colors) :role)
                          :protagonist)))
      (should (eq 3 (plist-get entry :index)))
      (should-not (plist-get entry :color)))
    ;; the other roles keep off that slot
    (let ((assigned (org-scribe-companion-colors-assign
                     '("a" "b" "c" "d" "e" "f" "g") '(("x" . "slot:3"))
                     '("1" "2" "3" "4" "5" "6" "7" "8"))))
      (should-not (cl-some (lambda (e) (eql 3 (plist-get (cdr e) :index))) assigned)))
    ;; a warning colour as a slot is the theme's colour in each mode
    (test-companion-colors--run root "set-color" :kind "warn" :color "slot:1")
    (let ((warn (plist-get (plist-get (org-scribe-companion--snapshot root) :colors) :warn)))
      (should (equal (plist-get warn :light) "#b45309"))
      (should (equal (plist-get warn :dark) "#e0883a")))
    (should (equal "invalid"
                   (plist-get (plist-get (test-companion-colors--run
                                          root "set-color" :kind "warn" :color "slot:x")
                                         :error)
                              :code)))))

(ert-deftest test-companion-colors-repinning-keeps-the-first-spelling ()
  (test-companion-colors--with-project root
    (test-companion-colors--run root "set-color" :kind "role" :value "Protagonist" :color "#0b5394")
    (test-companion-colors--run root "set-color" :kind "role" :value "protagonist " :color "#7a1f1f")
    (should (equal '("role #7a1f1f Protagonist")
                   (org-scribe--project-marker-get-all root "Companion-color")))))

(ert-deftest test-companion-colors-set-color-pins-a-pov-by-character-id ()
  (test-companion-colors--with-project root
    (should (eq (plist-get (test-companion-colors--run root "set-color" :kind "pov"
                                                       :value "char-alex-rivera-001" :color "#334455")
                           :ok)
                t))
    (let* ((colors (plist-get (org-scribe-companion--snapshot root) :colors))
           (entry (plist-get (plist-get colors :pov) :char-alex-rivera-001)))
      (should (equal (plist-get entry :color) "#334455")))))

(ert-deftest test-companion-colors-set-color-refusals ()
  (test-companion-colors--with-project root
    (cl-flet ((refused (code &rest args)
                (let ((answer (apply #'test-companion-colors--run root "set-color" args)))
                  (should (eq (plist-get answer :ok) :false))
                  (should (equal (plist-get (plist-get answer :error) :code) code)))))
      (refused "invalid" :kind "theme" :color "neon")
      (refused "invalid" :kind "theme")
      (refused "invalid" :kind "warn" :color "red")
      (refused "invalid" :kind "warn" :color "url(javascript:1)")
      (refused "invalid" :kind "role" :color "#112233")
      (refused "invalid" :kind "role" :value "x" :color "#12345")
      (refused "invalid" :kind "sparkle" :color "#112233")
      (refused "invalid")
      (refused "not-found" :kind "pov" :value "no-such-character" :color "#112233"))
    (should-not (org-scribe-companion-colors-pins root))))

(ert-deftest test-companion-colors-set-color-needs-a-marker-file ()
  (test-companion-colors--with-project root
    (delete-file (expand-file-name ".org-scribe-project" root))
    (let ((answer (test-companion-colors--run root "set-color" :kind "theme" :color "muted")))
      (should (eq (plist-get answer :ok) :false))
      (should (equal (plist-get (plist-get answer :error) :code) "invalid")))))

(ert-deftest test-companion-colors-web-colours-come-from-emacs ()
  "The page no longer hashes a hue for a role, a type or a PoV."
  (dolist (file '("views/relations.js" "views/table.js"))
    (with-temp-buffer
      (insert-file-contents (expand-file-name file (expand-file-name "companion/web" test-companion-colors--repo)))
      (goto-char (point-min))
      (should-not (re-search-forward "hsl(\\|function hash\\|povHue" nil t)))))

(provide 'test-companion-colors)

;;; test-companion-colors.el ends here
