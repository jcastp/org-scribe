;;; org-scribe-chronology.el --- Story-order view of the scenes -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Reads every scene's `:Timeline:' through `org-scribe-story-time' and lists
;; the scenes (and the design notebook's Timeline events) in story order,
;; beside their position in reading order.  A scene that happens earlier than
;; the one read just before it is shown as a jump back, never as an error.
;;
;; `org-scribe-chronology-collect' is the one reader; the dynamic block
;; below, the health report and the companion's snapshot all go through it,
;; so the parser and the sorting exist once.
;;
;; Use it as a dynamic block in any Org file of the project:
;;
;;   #+BEGIN: story-chronology
;;   #+END:
;;
;; Parameters: `:events nil' leaves the Timeline events out.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'org-scribe-core)
(require 'org-scribe-messages)
(require 'org-scribe-story-time)
(require 'org-scribe-linking-core)

(defun org-scribe-chronology--event-text (date time)
  "Join an event's DATE and TIME into one story-time text."
  (cond ((not (org-string-nw-p date)) nil)
        ((and (org-string-nw-p time)
              (string-match-p "\\`[ \t]*[0-9]\\{1,2\\}:[0-9][0-9]" time)
              (not (string-match-p "[0-9]:[0-9][0-9]" date)))
         (concat (string-trim date) ", " (string-trim time)))
        (t (string-trim date))))

(defun org-scribe-chronology--collect-events (context)
  "Return the notebook's Timeline events as story-time entries.
Every heading below the Timeline section with a `:Date:' counts.  CONTEXT
is the story-time context."
  (when-let* ((file (plist-get (org-scribe-project-structure) :design-file))
              ((file-exists-p file)))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (when-let* ((pos (org-scribe--find-section-heading 'timeline)))
         (goto-char pos)
         (let ((end (save-excursion (org-end-of-subtree t t) (point)))
               events)
           (while (and (outline-next-heading) (< (point) end))
             (when-let* ((text (org-scribe-chronology--event-text
                                (org-entry-get nil "Date")
                                (org-entry-get nil "Time"))))
               (push (org-scribe-chronology--entry
                      'event (org-scribe--heading-text) nil text context nil)
                     events)))
           (nreverse events)))))))

(defun org-scribe-chronology--entry (kind heading chapter text context index
                                          &optional id)
  "Return an entry plist of KIND (`scene' or `event') parsed from TEXT.
ID is the scene's `:ID:', when it has one."
  (let ((parsed (and text (org-scribe-story-time-key text context))))
    (list :kind kind :heading heading :chapter chapter :text text
          :key (plist-get parsed :key)
          :problem (unless parsed (org-scribe-story-time-problem text context))
          :index index :id id)))

(defun org-scribe-chronology-collect (&optional with-events)
  "Return the project's chronology as a plist.
Keys: `:scenes' (reading order), `:events' (only when WITH-EVENTS),
`:ordered' (the dated entries, scenes and events, in story order),
`:undated' (the rest, in reading order, events last) and `:jumps', an alist
of (INDEX . MINUTES-BACK) for each scene earlier than the dated scene read
before it.  Entries carry `:kind', `:heading', `:chapter', `:text',
`:key', `:problem' (`blank', `unreadable', `no-anchor' or `calendar-incomplete'), `:index' and
`:id' (a scene's, when it has one)."
  (let* ((context (org-scribe-story-time-context))
         (index 0)
         (scenes
          (org-scribe--get-all-scenes
           (lambda ()
             (setq index (1+ index))
             (org-scribe-chronology--entry
              'scene (org-scribe--heading-text)
              (save-excursion (and (org-up-heading-safe) (org-scribe--heading-text)))
              (org-scribe-scene-property-get 'timeline) context index
              (org-entry-get nil "ID")))))
         (events (when with-events (org-scribe-chronology--collect-events context)))
         (all (append scenes events))
         (dated (seq-filter (lambda (e) (plist-get e :key)) all))
         ;; `sort' is stable: ties keep reading order, scenes before events.
         (ordered (sort (copy-sequence dated)
                        (lambda (a b) (< (plist-get a :key) (plist-get b :key)))))
         (undated (seq-remove (lambda (e) (plist-get e :key)) all))
         (previous nil)
         jumps)
    (dolist (scene scenes)
      (when-let* ((key (plist-get scene :key)))
        (when (and previous (< key previous))
          (push (cons (plist-get scene :index) (- previous key)) jumps))
        (setq previous key)))
    (list :scenes scenes :events events :ordered ordered :undated undated
          :jumps (nreverse jumps) :total (length scenes) :context context)))

;;; The dynamic block

(defun org-scribe-chronology--distance (minutes)
  "Describe MINUTES back in time, as a localized \"↶ 20 days\"."
  (let ((days (/ minutes org-scribe-story-time--minutes-per-day)))
    (cond ((>= days (* 2 org-scribe-story-time--days-per-year))
           (let ((n (/ days org-scribe-story-time--days-per-year)))
             (org-scribe-msg 'chrono-jump-years n (org-scribe-plural n ""))))
          ((>= days 1)
           (org-scribe-msg 'chrono-jump-days days (org-scribe-plural days "")))
          (t (let ((h (max 1 (/ minutes 60))))
               (org-scribe-msg 'chrono-jump-hours h (org-scribe-plural h "")))))))

(defun org-scribe-chronology--reason (problem)
  "Say in words why an entry with PROBLEM (`blank', `unreadable', `no-anchor', `calendar-incomplete') is undated."
  (if (memq problem '(blank unreadable no-anchor calendar-incomplete))
      (org-scribe-msg (intern (format "chrono-reason-%s" problem)))
    ""))

(defun org-scribe-chronology--undated-order (entries)
  "Return undated ENTRIES with the ones the writer tried to date first.
Unreadable and anchorless values come before blank ones, each group in its
reading order, so a typo is not buried under dozens of scenes with no time."
  (let ((attempted (seq-remove (lambda (e) (eq (plist-get e :problem) 'blank)) entries))
        (blank (seq-filter (lambda (e) (eq (plist-get e :problem) 'blank)) entries)))
    (append attempted blank)))

(defun org-scribe-chronology--row (entry total jumps)
  "Insert the table row of ENTRY (TOTAL scenes; JUMPS from the collector)."
  (let* ((scene-p (eq (plist-get entry :kind) 'scene))
         (note (cond ((null (plist-get entry :key))
                      (org-scribe-chronology--reason (plist-get entry :problem)))
                     ((not scene-p)
                      (if (< (plist-get entry :key) 0)
                          (org-scribe-msg 'chrono-backstory)
                        ""))
                     ((assq (plist-get entry :index) jumps)
                      (org-scribe-chronology--distance
                       (cdr (assq (plist-get entry :index) jumps))))
                     (t ""))))
    (insert (format "| %s | %s | %s | %s | %s |\n"
                    (org-scribe--escape-table-cell (or (plist-get entry :text) ""))
                    (org-scribe--escape-table-cell
                     (if (plist-get entry :id)
                         (org-link-make-string (concat "id:" (plist-get entry :id))
                                               (plist-get entry :heading))
                       (plist-get entry :heading)))
                    (org-scribe--escape-table-cell (or (plist-get entry :chapter) ""))
                    (if scene-p
                        (format "%d/%d" (plist-get entry :index) total)
                      "")
                    (org-scribe--escape-table-cell note)))))

(defun org-scribe-chronology--insert-table (entries total jumps &optional undated)
  "Insert the header and a row per entry of ENTRIES, then align the table.
With UNDATED the last column says why an entry has no story time."
  (insert (org-scribe-msg (if undated 'chrono-table-header-undated 'chrono-table-header))
          "\n|-\n")
  (dolist (entry entries)
    (org-scribe-chronology--row entry total jumps))
  (org-table-align))

;;;###autoload
(defun org-dblock-write:story-chronology (params)
  "Write the scenes and Timeline events in story order.
PARAMS may contain `:events nil' to leave the notebook's events out."
  (let* ((with-events (if (plist-member params :events)
                          (plist-get params :events)
                        t))
         (data (org-scribe-chronology-collect with-events))
         (total (plist-get data :total))
         (jumps (plist-get data :jumps))
         (ordered (plist-get data :ordered))
         (undated (plist-get data :undated)))
    (if (and (null ordered) (null undated))
        (insert (org-scribe-msg 'chrono-no-scenes) "\n")
      (when ordered
        (org-scribe-chronology--insert-table ordered total jumps))
      (when undated
        (when ordered (goto-char (org-table-end)))
        (insert "\n" (org-scribe-msg 'chrono-undated (length undated)) "\n")
        (org-scribe-chronology--insert-table
         (org-scribe-chronology--undated-order undated) total jumps t)))))

;; Offered by `org-insert-dblock' (C-c C-x x) too.
(with-eval-after-load 'org
  (when (fboundp 'org-dynamic-block-define)
    (org-dynamic-block-define "story-chronology" #'org-scribe-chronology-insert-block)))

(defun org-scribe-chronology-insert-block ()
  "Insert an empty story-chronology dynamic block at point and fill it."
  (interactive)
  (insert "#+BEGIN: story-chronology\n#+END:\n")
  (forward-line -2)
  (org-update-dblock))

(provide 'org-scribe-chronology)

;;; org-scribe-chronology.el ends here
