;;; org-scribe-cut-scene.el --- A cut bin for scenes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Snapshots cover rewriting; this covers deletion.  `org-scribe-cut-scene'
;; moves the scene at point, whole (ID, drawer, prose, sub-headings), out of
;; the manuscript into a "Cut scenes" section of the design notebook's
;; Revision part (a level-1 heading at the end of a short story's notebook,
;; created by the first cut, like Versions).  `org-scribe-restore-scene' puts
;; one back under a chapter and `org-scribe-list-cut-scenes' reports on them.
;;
;; Things that are easy to get wrong, and are why this file is not just a
;; refile:
;;
;; - The manuscript declares its own #+TODO: set (TOWRITE ...); the notebook
;;   declares none, so a TOWRITE left on the heading would turn into title
;;   text.  The keyword is stored in :Cut-state: and put back on restore (only
;;   if the manuscript still declares it).
;; - Two buffers have two undo lists, so there is no single undo.  The scene
;;   is written into the notebook first, its ID is found there, and only then
;;   deleted from the manuscript: a failure never loses it.  Restore runs in
;;   the opposite direction under the same rule.
;; - The ID moves with the scene (`org-id-add-location'), so every link to it
;;   keeps working, including the :SCENE: link of its snapshots.
;; - Health, the word count, compile and search read the manuscript, so a cut
;;   scene is in none of them.  The planner sees the same negative delta as a
;;   deletion.  A scene counts as referenced, for the sake of the prune mark in
;;   the list, when any project file links to its ID.
;;
;; Cut properties are English in every language, as for entities.

;;; Code:

(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'org-scribe-core)
(require 'org-scribe-config)
(require 'org-scribe-messages)
(require 'org-scribe-capture)
(require 'org-scribe-linking-core)
(require 'org-scribe-wordcount)

(declare-function org-scribe--goto-insertion-point "org-scribe-project")
(declare-function org-scribe--goto-end-of-last-narrative-heading "org-scribe-project")
(declare-function org-scribe--heading-under-noexport-p "org-scribe-project")

;;; Pieces of text

(defconst org-scribe-cut--properties
  '("Cut-date" "Cut-state" "Cut-reason" "Cut-chapter")
  "Properties a cut scene carries beyond its own drawer.")

(defun org-scribe-cut--relevel (text shift)
  "Return TEXT with every heading moved SHIFT levels, never above level 1."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\)\\([ \t]\\|$\\)" nil t)
      (let ((stars (make-string (max 1 (+ (length (match-string 1)) shift)) ?*))
            (after (match-string 2)))
        (replace-match (concat stars after) t t)))
    (buffer-string)))

(defun org-scribe-cut--strip-state (text state)
  "Return TEXT without STATE as the keyword of its first heading."
  (if (and state
           (string-match (concat "\\`\\(\\*+ \\)" (regexp-quote state) "\\(?: +\\|$\\)")
                         text))
      (replace-match "\\1" t nil text)
    text))

(defun org-scribe-cut--add-state (text state)
  "Return TEXT with STATE as the keyword of its first heading."
  (if (and state (string-match "\\`\\*+ " text))
      (concat (match-string 0 text) state " " (substring text (match-end 0)))
    text))

(defun org-scribe-cut--edit-properties (text set remove)
  "Return TEXT with the properties of its first heading changed.
SET is an alist of (NAME . VALUE) to write, REMOVE a list of names to delete."
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (org-mode))
    (goto-char (point-min))
    (dolist (name remove) (org-delete-property name))
    (dolist (pair set) (org-set-property (car pair) (cdr pair)))
    (buffer-string)))

(defun org-scribe-cut--one-line (string)
  "Return STRING trimmed to a single line."
  (string-trim (replace-regexp-in-string "[\n\r]+" " " (or string ""))))

;;; Cutting

(defun org-scribe-cut--manuscript-buffer-p ()
  "Non-nil when the current buffer visits the project's manuscript."
  (let ((manuscript (plist-get (org-scribe-project-structure) :manuscript-file)))
    (and manuscript buffer-file-name (file-equal-p manuscript buffer-file-name))))

(defun org-scribe-cut--scene-title ()
  "Check that point is in a scene of the manuscript and return its title."
  (org-scribe--require-project)
  (unless (and (derived-mode-p 'org-mode) (org-scribe-cut--manuscript-buffer-p))
    (user-error "%s" (org-scribe-msg 'cut-not-in-manuscript)))
  (org-with-wide-buffer
   (save-excursion
     (org-scribe--goto-scene-heading)
     (org-scribe--heading-text))))

(defun org-scribe-cut--save-if-clean (buffer was-modified)
  "Save BUFFER unless it had unsaved changes (WAS-MODIFIED) of the writer's."
  (unless was-modified
    (with-current-buffer buffer (save-buffer))))

;;;###autoload
(defun org-scribe-cut-scene (&optional reason)
  "Move the scene at point into the notebook's Cut scenes section.
The whole subtree goes: ID, drawer, prose and sub-headings.  It gains
`:Cut-date:', `:Cut-state:' (its TODO keyword), `:Cut-chapter:' and, when
REASON is not empty, `:Cut-reason:'.  Nothing is lost if the move fails
half way: the copy is written and found in the notebook before the scene
leaves the manuscript.  Restore it with `org-scribe-restore-scene'."
  (interactive (list (read-string
                      (org-scribe-msg 'cut-reason-prompt (org-scribe-cut--scene-title)))))
  (org-scribe-cut--scene-title)
  (let* ((design (or (org-scribe--design-file)
                     (user-error "%s" (org-scribe-msg 'calendar-no-notebook))))
         (manuscript (current-buffer))
         (manuscript-modified (buffer-modified-p))
         (chapter-level (org-scribe-chapter-level))
         id title state words text scene-level chapter start end design-modified)
    (org-with-wide-buffer
     (save-excursion
       (org-scribe--goto-scene-heading)
       (setq scene-level (org-current-level)
             title (org-scribe--heading-text)
             state (org-get-todo-state)
             id (org-id-get-create)
             start (progn (org-back-to-heading t) (point))
             end (save-excursion (org-end-of-subtree t t) (point))
             text (buffer-substring-no-properties start end)
             words (org-scribe--count-words-region start end))
       (when chapter-level
         (while (and (> (org-current-level) chapter-level) (org-up-heading-safe)))
         (when (= (org-current-level) chapter-level)
           (setq chapter (org-scribe--heading-text))))))
    (setq text (org-scribe-cut--strip-state text state))
    (with-current-buffer (find-file-noselect design)
      (setq design-modified (buffer-modified-p))
      (org-with-wide-buffer
       (org-scribe--capture-goto-section 'cut-scenes)
       (let ((level (1+ (org-current-level))))
         (org-end-of-subtree t t)
         (unless (bolp) (insert "\n"))
         (insert (string-trim-right
                  (org-scribe-cut--edit-properties
                   (org-scribe-cut--relevel text (- level scene-level))
                   (append `(("Cut-date" . ,(format-time-string "%Y-%m-%d")))
                           (and state `(("Cut-state" . ,state)))
                           (and chapter `(("Cut-chapter" . ,chapter)))
                           (let ((one (org-scribe-cut--one-line reason)))
                             (unless (string-empty-p one)
                               `(("Cut-reason" . ,one)))))
                   nil)
                  "\n")
                 "\n")))
      (org-id-add-location id design)
      (unless (org-id-find-id-in-file id design)
        (error "Could not find the cut scene in the design notebook; the manuscript is untouched"))
      (org-scribe-cut--save-if-clean (current-buffer) design-modified))
    (with-current-buffer manuscript
      (org-with-wide-buffer (delete-region start end)))
    (org-scribe--refile-invalidate-cache)
    (org-scribe-cut--save-if-clean manuscript manuscript-modified)
    (message "%s" (org-scribe-msg 'cut-done title words))
    id))

;;; Reading the cut scenes

(defun org-scribe-cut--collect (&optional design-buffer)
  "Return the cut scenes of DESIGN-BUFFER as plists, oldest first.
Keys: :label (unique), :id, :title, :date, :state, :chapter, :reason, :words,
:pos and :end (the subtree)."
  (when-let* ((buffer (or design-buffer
                          (when-let* ((file (org-scribe--design-file)))
                            (find-file-noselect file)))))
    (with-current-buffer buffer
      (org-with-wide-buffer
       (when-let* ((pos (org-scribe--find-section-heading 'cut-scenes)))
         (goto-char pos)
         (let ((level (1+ (org-current-level)))
               (limit (save-excursion (org-end-of-subtree t t) (point)))
               seen result)
           (forward-line 1)
           (while (re-search-forward org-heading-regexp limit t)
             (goto-char (match-beginning 0))
             (when (and (= (org-current-level) level) (org-entry-get nil "ID"))
               (let* ((title (org-scribe--heading-text))
                      (date (org-entry-get nil "Cut-date"))
                      (base (if date (format "%s (%s)" title date) title))
                      (count (1+ (or (cdr (assoc base seen)) 0))))
                 (setf (alist-get base seen nil nil #'equal) count)
                 (push (list :label (if (> count 1) (format "%s (%d)" base count) base)
                             :id (org-entry-get nil "ID")
                             :title title
                             :date date
                             :state (org-entry-get nil "Cut-state")
                             :chapter (org-entry-get nil "Cut-chapter")
                             :reason (org-entry-get nil "Cut-reason")
                             :words (string-to-number (or (org-entry-get nil "WORDCOUNT") "0"))
                             :pos (point)
                             :end (save-excursion (org-end-of-subtree t t) (point)))
                       result)))
             (end-of-line))
           (nreverse result)))))))

(defun org-scribe-cut--find-entry (id)
  "Return the cut scene with ID, or signal a `user-error'."
  (or (cl-find id (org-scribe-cut--collect)
               :key (lambda (e) (plist-get e :id)) :test #'equal)
      (user-error "%s" (org-scribe-msg 'cut-entry-missing))))

(defun org-scribe-cut--file-text (file)
  "Return the text of FILE, as it is in its buffer when it is being visited."
  (if-let* ((buffer (find-buffer-visiting file)))
      (with-current-buffer buffer (org-with-wide-buffer (buffer-string)))
    (with-temp-buffer (insert-file-contents file) (buffer-string))))

(defun org-scribe-cut--reference-count (id)
  "Return how many links to the scene ID the project's Org files hold."
  (let ((regexp (format "\\[\\[id:%s\\]" (regexp-quote id)))
        (count 0))
    (dolist (file (directory-files (org-scribe-project-root) t "\\.org\\'"))
      (with-temp-buffer
        (insert (org-scribe-cut--file-text file))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t) (cl-incf count))))
    count))

;;; Restoring

(defun org-scribe-cut--chapters (manuscript-buffer)
  "Return the titles of the chapters of MANUSCRIPT-BUFFER, in order."
  (when-let* ((level (org-scribe-chapter-level)))
    (with-current-buffer manuscript-buffer
      (org-with-wide-buffer
       (goto-char (point-min))
       (let (titles)
         (while (re-search-forward (format "^\\*\\{%d\\}[ \t]" level) nil t)
           (save-excursion
             (goto-char (match-beginning 0))
             (unless (org-scribe--heading-under-noexport-p)
               (push (org-scribe--heading-text) titles))))
         (delete-dups (nreverse titles)))))))

(defun org-scribe-cut--goto-chapter (title)
  "Move point to the chapter headed TITLE; return non-nil when found."
  (when-let* ((level (org-scribe-chapter-level)))
    (goto-char (point-min))
    (let (found)
      (while (and (not found)
                  (re-search-forward (format "^\\*\\{%d\\}[ \t]" level) nil t))
        (goto-char (match-beginning 0))
        (if (and (not (org-scribe--heading-under-noexport-p))
                 (equal (org-scribe--heading-text) title))
            (setq found t)
          (end-of-line)))
      found)))

(defun org-scribe-cut--restore (entry &optional chapter)
  "Put the cut scene ENTRY back in the manuscript, under CHAPTER (a title).
Without CHAPTER, or when no chapter has that title, it goes after the last
scene of the manuscript.  Return the scene's ID."
  (let* ((structure (org-scribe-project-structure))
         (design (plist-get structure :design-file))
         (manuscript (find-file-noselect (plist-get structure :manuscript-file)))
         (design-buffer (find-file-noselect design))
         (id (plist-get entry :id))
         (scene-level (org-scribe-scene-level))
         (declared (with-current-buffer manuscript
                     (and (member (plist-get entry :state)
                                  (bound-and-true-p org-todo-keywords-1))
                          (plist-get entry :state))))
         text title words design-modified manuscript-modified)
    (with-current-buffer design-buffer
      (setq design-modified (buffer-modified-p))
      (org-with-wide-buffer
       (goto-char (or (org-find-entry-with-id id)
                      (user-error "%s" (org-scribe-msg 'cut-entry-missing))))
       (let ((level (org-current-level)))
         (setq title (org-scribe--heading-text))
         (org-back-to-heading t)
         (setq text (org-scribe-cut--add-state
                     (org-scribe-cut--edit-properties
                      (org-scribe-cut--relevel
                       (buffer-substring-no-properties
                        (point) (save-excursion (org-end-of-subtree t t) (point)))
                       (- scene-level level))
                      nil org-scribe-cut--properties)
                     declared)))))
    (with-current-buffer manuscript
      (setq manuscript-modified (buffer-modified-p))
      (org-with-wide-buffer
       (if (and chapter (org-scribe-cut--goto-chapter chapter))
           (org-scribe--goto-insertion-point scene-level)
         (unless (org-scribe--goto-end-of-last-narrative-heading scene-level)
           (goto-char (point-max))))
       (unless (bolp) (insert "\n"))
       (let ((start (point)))
         (insert (string-trim-right text "\n") "\n")
         (goto-char start)
         (setq words (org-scribe--count-words-region
                      start (save-excursion (org-end-of-subtree t) (point))))
         (org-set-property "WORDCOUNT" (number-to-string words))))
      (org-id-add-location id (buffer-file-name))
      (unless (org-id-find-id-in-file id (buffer-file-name))
        (error "Could not find the restored scene in the manuscript; the notebook is untouched"))
      (org-scribe-cut--save-if-clean (current-buffer) manuscript-modified))
    (org-scribe-cut--remove-from-notebook id design-buffer design-modified)
    (org-scribe--refile-invalidate-cache)
    (message "%s" (org-scribe-msg 'cut-restored title words))
    id))

(defun org-scribe-cut--remove-from-notebook (id design-buffer was-modified)
  "Delete the cut scene ID from DESIGN-BUFFER; save it unless WAS-MODIFIED."
  (with-current-buffer design-buffer
    (org-with-wide-buffer
     (when-let* ((pos (org-find-entry-with-id id)))
       (goto-char pos)
       (delete-region (point) (save-excursion (org-end-of-subtree t t) (point)))))
    (org-scribe-cut--save-if-clean (current-buffer) was-modified)))

(defun org-scribe-cut--read-entry ()
  "Return the cut scene to act on: the one point is in, else a chosen one."
  (let* ((design (or (org-scribe--design-file)
                     (user-error "%s" (org-scribe-msg 'calendar-no-notebook))))
         (all (org-scribe-cut--collect)))
    (or (and (equal (buffer-file-name) design)
             (cl-find-if (lambda (e) (and (>= (point) (plist-get e :pos))
                                          (< (point) (plist-get e :end))))
                         all))
        (cond
         ((null all) (user-error "%s" (org-scribe-msg 'cut-none)))
         (t (let ((choice (completing-read
                           (org-scribe-msg 'cut-restore-prompt)
                           (lambda (string pred action)
                             (if (eq action 'metadata)
                                 '(metadata (display-sort-function . identity))
                               (complete-with-action
                                action (mapcar (lambda (e) (plist-get e :label)) all)
                                string pred)))
                           nil t nil nil
                           (plist-get (car (last all)) :label))))
              (cl-find choice all :key (lambda (e) (plist-get e :label)) :test #'equal)))))))

(defun org-scribe-cut--read-chapter (entry)
  "Ask for the chapter ENTRY goes back to; nil means the end of the manuscript."
  (when (org-scribe-chapter-level)
    (let* ((manuscript (find-file-noselect
                        (plist-get (org-scribe-project-structure) :manuscript-file)))
           (titles (org-scribe-cut--chapters manuscript))
           (end (org-scribe-msg 'cut-restore-end))
           (default (if (member (plist-get entry :chapter) titles)
                        (plist-get entry :chapter)
                      end))
           (choice (completing-read (org-scribe-msg 'cut-restore-chapter-prompt)
                                    (append titles (list end)) nil t nil nil default)))
      (unless (equal choice end) choice))))

;;;###autoload
(defun org-scribe-restore-scene ()
  "Put a cut scene back in the manuscript.
From inside a cut scene in the design notebook that one is used; elsewhere
the cut scenes are offered.  The scene goes under a chapter you pick (the one
it came from is the default) or at the end of the manuscript, gets its TODO
state back when the manuscript still declares it, and has its word count
refreshed."
  (interactive)
  (org-scribe--require-project)
  (let ((entry (org-scribe-cut--read-entry)))
    (org-scribe-cut--restore entry (org-scribe-cut--read-chapter entry))))

;;; Deleting for good

(defun org-scribe-cut--delete (entry)
  "Delete the cut scene ENTRY from the notebook for good."
  (let* ((buffer (find-file-noselect (org-scribe--design-file)))
         (id (plist-get entry :id)))
    (org-scribe-cut--remove-from-notebook id buffer (buffer-modified-p buffer))
    (when (hash-table-p org-id-locations) (remhash id org-id-locations))
    (message "%s" (org-scribe-msg 'cut-deleted (plist-get entry :title)))))

;;; The report

(defvar-local org-scribe-cut-list--root nil
  "Project root the report buffer belongs to.")

(defun org-scribe-cut-list--entries ()
  "Return the `tabulated-list-entries' for the current project."
  (mapcar
   (lambda (e)
     (let ((links (org-scribe-cut--reference-count (plist-get e :id))))
       (list (plist-get e :id)
             (vector (or (plist-get e :date) "")
                     (plist-get e :title)
                     (number-to-string (plist-get e :words))
                     (if (zerop links)
                         (concat "0 " (org-scribe-msg 'cut-list-prune))
                       (number-to-string links))
                     (or (plist-get e :reason) "")))))
   (org-scribe-cut--collect)))

(defun org-scribe-cut-list--refresh ()
  "Fill the report from the notebook."
  (let ((default-directory org-scribe-cut-list--root))
    (setq tabulated-list-format
          (vector (list (org-scribe-msg 'cut-list-column-date) 12 t)
                  (list (org-scribe-msg 'cut-list-column-title) 34 t)
                  (list (org-scribe-msg 'cut-list-column-words) 8 nil :right-align t)
                  (list (org-scribe-msg 'cut-list-column-links) 12 nil)
                  (list (org-scribe-msg 'cut-list-column-reason) 0 nil))
          tabulated-list-entries (org-scribe-cut-list--entries))
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(defun org-scribe-cut-list--entry-at-point ()
  "Return the cut scene on the current line."
  (let ((default-directory org-scribe-cut-list--root))
    (org-scribe-cut--find-entry
     (or (tabulated-list-get-id) (user-error "%s" (org-scribe-msg 'cut-none))))))

(defun org-scribe-cut-list-restore ()
  "Restore the cut scene on this line."
  (interactive)
  (let* ((default-directory org-scribe-cut-list--root)
         (entry (org-scribe-cut-list--entry-at-point)))
    (org-scribe-cut--restore entry (org-scribe-cut--read-chapter entry))
    (org-scribe-cut-list--refresh)))

(defun org-scribe-cut-list-delete ()
  "Delete the cut scene on this line for good, after confirming."
  (interactive)
  (let* ((default-directory org-scribe-cut-list--root)
         (entry (org-scribe-cut-list--entry-at-point)))
    (when (yes-or-no-p (org-scribe-msg 'cut-delete-confirm
                                       (plist-get entry :title) (plist-get entry :words)))
      (org-scribe-cut--delete entry)
      (org-scribe-cut-list--refresh))))

(defun org-scribe-cut-list-visit ()
  "Visit the cut scene on this line in the design notebook."
  (interactive)
  (let* ((default-directory org-scribe-cut-list--root)
         (entry (org-scribe-cut-list--entry-at-point)))
    (pop-to-buffer (find-file-noselect (org-scribe--design-file)))
    (widen)
    (goto-char (plist-get entry :pos))
    (org-fold-show-entry)))

(defvar org-scribe-cut-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "r") #'org-scribe-cut-list-restore)
    (define-key map (kbd "d") #'org-scribe-cut-list-delete)
    (define-key map (kbd "RET") #'org-scribe-cut-list-visit)
    map)
  "Keymap of `org-scribe-cut-list-mode'.")

(define-derived-mode org-scribe-cut-list-mode tabulated-list-mode "Cut-scenes"
  "Report of the scenes cut from the manuscript.
\\{org-scribe-cut-list-mode-map}"
  (setq tabulated-list-padding 1)
  (add-hook 'tabulated-list-revert-hook #'org-scribe-cut-list--refresh nil t))

;;;###autoload
(defun org-scribe-list-cut-scenes ()
  "Report on the cut scenes of this project.
Each line shows when the scene was cut, its title, its words, how many links
in the project point at it (none is marked for pruning) and the reason given.
From the report: r restores, d deletes for good (after confirming), RET visits
the scene in the design notebook."
  (interactive)
  (let ((root (org-scribe--require-project)))
    (unless (org-scribe-cut--collect)
      (user-error "%s" (org-scribe-msg 'cut-none)))
    (with-current-buffer (get-buffer-create
                          (format "*%s*" (org-scribe-msg 'cut-list-title)))
      (org-scribe-cut-list-mode)
      (setq default-directory root
            org-scribe-cut-list--root root
            header-line-format (org-scribe-msg 'cut-list-help))
      (org-scribe-cut-list--refresh)
      (pop-to-buffer (current-buffer)))))

(provide 'org-scribe-cut-scene)

;;; org-scribe-cut-scene.el ends here
