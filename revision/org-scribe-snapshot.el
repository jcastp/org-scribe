;;; org-scribe-snapshot.el --- Scene snapshots before rewriting -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Rewriting a scene by feel loses the old version.  `org-scribe-snapshot-scene'
;; copies the prose of the scene at point, dated, into a Versions section of the
;; design notebook, with an ID link back to the scene, so the old text survives
;; the rewrite.  `org-scribe-compare-snapshot' runs ediff between a snapshot and
;; the live scene.
;;
;; The section belongs to the notebook's Revision part (see
;; `org-scribe--section-part'), not to the Scratchpad: the Scratchpad asks
;; nothing of the writer and has one filing command, while a snapshot is prose
;; the writer chose to keep.  A short story's notebook has no parts, so there
;; the section is a level-1 heading at the end of the file.  Like the Calendar
;; heading it is not shipped in the templates; the first snapshot creates it.
;;
;; Snapshots are kept forever and nothing else reads them: the word count and
;; compilation read the manuscript only, and `Versions' is not an entity
;; section, so its children are never taken for entities.
;;
;; What a snapshot copies is the scene's body: never the scene's own property
;; drawer (an :ID: copied twice would make `org-id' resolve to the copy, and the
;; scene's properties would be counted twice), and never the drawers of its
;; sub-headings either.  Sub-headings are re-levelled to sit below the snapshot
;; heading.

;;; Code:

(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'subr-x)
(require 'org-scribe-core)
(require 'org-scribe-config)
(require 'org-scribe-messages)
(require 'org-scribe-capture)
(require 'org-scribe-linking-core)
(require 'org-scribe-wordcount)

(declare-function ediff-regions-internal "ediff")

;;; Reading a body

(defun org-scribe-snapshot--body-bounds ()
  "Return (START . END) of the prose of the heading at point.
START is after the heading's planning line and property drawer, END is the
end of its subtree, so sub-headings are part of the body."
  (save-excursion
    (org-back-to-heading t)
    (cons (progn (org-end-of-meta-data t) (point))
          (progn (org-end-of-subtree t t) (point)))))

(defun org-scribe-snapshot--prepare-body (text shift)
  "Return TEXT with its headings moved SHIFT levels and their drawers dropped.
A heading never goes above level 1.  Only the :PROPERTIES: drawer straight
under a heading is removed; the rest of the body is copied as it is."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\)\\([ \t]\\|$\\)" nil t)
      (let ((stars (make-string (max 1 (+ (length (match-string 1)) shift)) ?*))
            (after (match-string 2)))
        (replace-match (concat stars after) t t)
        (forward-line 1)
        (when (looking-at "[ \t]*:PROPERTIES:[ \t]*$")
          (let ((start (point)))
            (if (re-search-forward "^[ \t]*:END:[ \t]*\n?" nil t)
                (delete-region start (point))
              (goto-char start))))))
    (string-trim-right (replace-regexp-in-string "\\`\n+" "" (buffer-string)))))

;;; Taking a snapshot

(defun org-scribe-snapshot--link-text (title)
  "Return TITLE made safe to sit inside the description of an Org link."
  (replace-regexp-in-string "[][]" "" title))

;;;###autoload
(defun org-scribe-snapshot-scene ()
  "Copy the prose of the scene at point into the notebook's Versions section.
The copy is dated and carries an ID link to the scene, its word count and
its state at the time.  Taking several snapshots of one scene is fine; the
time tells them apart.  The scene itself is not changed, apart from getting
an ID if it had none."
  (interactive)
  (org-scribe--require-project)
  (let ((design (or (org-scribe--design-file)
                    (user-error "%s" (org-scribe-msg 'calendar-no-notebook))))
        (language (org-scribe-project-language))
        id title state words body scene-level)
    (org-with-wide-buffer
     (save-excursion
       (org-scribe--goto-scene-heading)
       (setq scene-level (org-current-level)
             title (org-scribe--heading-text)
             state (org-get-todo-state)
             id (org-id-get-create))
       (let ((bounds (org-scribe-snapshot--body-bounds)))
         (setq body (buffer-substring-no-properties (car bounds) (cdr bounds))
               words (org-scribe--count-words-region (car bounds) (cdr bounds))))))
    (when (string-blank-p body)
      (user-error "%s" (org-scribe-msg 'snapshot-scene-empty title)))
    (let ((label (let ((org-scribe-message-language language))
                   (org-scribe-msg 'snapshot-label)))
          (stamp (format-time-string "%Y-%m-%d %H:%M"))
          was-modified)
      (with-current-buffer (find-file-noselect design)
        (setq was-modified (buffer-modified-p))
        (org-with-wide-buffer
         (org-scribe--capture-goto-section 'versions)
         (let* ((level (1+ (org-current-level)))
                (text (org-scribe-snapshot--prepare-body body (- level scene-level))))
           (org-end-of-subtree t t)
           (unless (bolp) (insert "\n"))
           (insert (format "%s %s %s - %s\n:PROPERTIES:\n:SCENE: [[id:%s][%s]]\n:WORDS: %d\n%s:END:\n%s\n"
                           (make-string level ?*) label stamp title
                           id (org-scribe-snapshot--link-text title) words
                           (if state (format ":STATE: %s\n" state) "")
                           text))))
        ;; save only a notebook that had nothing unsaved of the writer's own
        (unless was-modified (save-buffer)))
      (message "%s" (org-scribe-msg 'snapshot-saved title words)))))

;;; Finding snapshots

(defun org-scribe-snapshot--collect ()
  "Return the snapshots in the current buffer, oldest first.
Each is a plist with :label (unique, for completion), :pos and :end (the
snapshot's subtree) and :scene-id (from its :SCENE: link, or nil)."
  (org-with-wide-buffer
   (when-let* ((pos (org-scribe--find-section-heading 'versions)))
     (goto-char pos)
     (let ((level (1+ (org-current-level)))
           (limit (save-excursion (org-end-of-subtree t t) (point)))
           seen result)
       (forward-line 1)
       (while (re-search-forward org-heading-regexp limit t)
         (goto-char (match-beginning 0))
         (when (= (org-current-level) level)
           (when-let* ((scene (org-entry-get nil "SCENE")))
             (let* ((base (org-get-heading t t t t))
                    (count (1+ (or (cdr (assoc base seen)) 0))))
               (setf (alist-get base seen nil nil #'equal) count)
               (push (list :label (if (> count 1) (format "%s (%d)" base count) base)
                           :pos (point)
                           :end (save-excursion (org-end-of-subtree t t) (point))
                           :scene-id (and (string-match "\\[\\[id:\\([^]]+\\)\\]" scene)
                                          (match-string 1 scene)))
                     result))))
         (end-of-line))
       (nreverse result)))))

(defun org-scribe-snapshot--current-scene ()
  "Return (ID) when point is in a scene, nil when it is in none.
ID is nil for a scene that has none yet, which therefore has no snapshots."
  (when (and (derived-mode-p 'org-mode) (buffer-file-name))
    (ignore-errors
      (org-with-wide-buffer
       (save-excursion
         (org-scribe--goto-scene-heading)
         (list (org-entry-get nil "ID")))))))

(defun org-scribe-snapshot--scene-region (id)
  "Return (BUFFER START END) for the body of the scene with ID, or nil."
  (when-let* ((marker (ignore-errors (org-id-find id t))))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (let ((bounds (org-scribe-snapshot--body-bounds)))
         (list (current-buffer) (car bounds) (cdr bounds)))))))

;;; Comparing

;;;###autoload
(defun org-scribe-compare-snapshot ()
  "Compare a snapshot of a scene with the scene as it is now, using ediff.
From inside a scene, the choice is among that scene's snapshots; from inside
a snapshot in the design notebook, that snapshot is used; anywhere else, all
snapshots are offered.  Both sides are the real text, so a passage worth
keeping can be copied across."
  (interactive)
  (org-scribe--require-project)
  (let* ((design (or (org-scribe--design-file)
                     (user-error "%s" (org-scribe-msg 'calendar-no-notebook))))
         (design-buffer (find-file-noselect design))
         (scene (org-scribe-snapshot--current-scene))
         (all (with-current-buffer design-buffer (org-scribe-snapshot--collect)))
         (here (and (eq (current-buffer) design-buffer)
                    (cl-find-if (lambda (s) (and (>= (point) (plist-get s :pos))
                                                 (< (point) (plist-get s :end))))
                                all)))
         (candidates (if scene
                         (cl-remove-if-not
                          (lambda (s) (and (car scene)
                                           (equal (plist-get s :scene-id) (car scene))))
                          all)
                       all))
         snapshot)
    (cond (here (setq snapshot here))
          ((null all) (user-error "%s" (org-scribe-msg 'snapshot-none)))
          ((null candidates) (user-error "%s" (org-scribe-msg 'snapshot-none-for-scene)))
          (t (let* ((labels (mapcar (lambda (s) (plist-get s :label)) candidates))
                    (choice (completing-read
                             (org-scribe-msg 'snapshot-prompt)
                             (lambda (string pred action)
                               (if (eq action 'metadata)
                                   '(metadata (display-sort-function . identity))
                                 (complete-with-action action labels string pred)))
                             nil t nil nil (car (last labels)))))
               (setq snapshot (cl-find choice candidates
                                       :key (lambda (s) (plist-get s :label))
                                       :test #'equal)))))
    (let ((target (and (plist-get snapshot :scene-id)
                       (org-scribe-snapshot--scene-region (plist-get snapshot :scene-id))))
          (old (with-current-buffer design-buffer
                 (org-with-wide-buffer
                  (goto-char (plist-get snapshot :pos))
                  (let ((bounds (org-scribe-snapshot--body-bounds)))
                    (list design-buffer (car bounds) (cdr bounds)))))))
      (unless target
        (user-error "%s" (org-scribe-msg 'snapshot-scene-not-found)))
      (require 'ediff)
      (ediff-regions-internal (nth 0 old) (nth 1 old) (nth 2 old)
                              (nth 0 target) (nth 1 target) (nth 2 target)
                              nil 'ediff-regions-wordwise 'word-mode nil))))

(provide 'org-scribe-snapshot)

;;; org-scribe-snapshot.el ends here
