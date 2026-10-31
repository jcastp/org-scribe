;;; org-scribe-companion-undo.el --- Visual companion: the page's undo stack -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; The page can undo its own changes, newest first, without the writer
;; switching to Emacs.  Every editing command already runs as one undo step
;; (`org-scribe-companion-commands--with-edit'); this file remembers, per
;; project, which buffer each step changed and where the step sits in that
;; buffer's `buffer-undo-list':
;;
;;   (:buffer BUFFER :head HEAD :stop STOP)
;;
;; HEAD is the undo list right after the step, STOP the list right before
;; it; the step's changes are the entries from HEAD up to STOP.  Emacs keeps
;; those cons cells (`undo-amalgamate-change-group' only drops boundaries
;; between them), so the entries can be replayed with `primitive-undo'.
;;
;; The page only ever undoes a step that is still on top of the buffer's
;; history: `org-scribe-companion-undo--tops' holds, per buffer, the undo
;; list as the page last left it.  Typing, an `undo' or any other change in
;; Emacs moves the buffer's list away from it, and the page's undo is then
;; refused (`conflict'): undoing an older step under newer text would apply
;; it at stale positions.  The writer undoes in Emacs instead.
;;
;; The stack lives in this Emacs session only and is dropped when the
;; server starts or stops.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defconst org-scribe-companion-undo-depth 50
  "How many of the page's steps it can undo, per project.")

(defvar org-scribe-companion-undo--stacks nil
  "Alist of (ROOT . ENTRIES), newest entry first.  See the Commentary.")

(defvar org-scribe-companion-undo--tops nil
  "Alist of (BUFFER . LIST): BUFFER's undo list as the page last left it.
LIST has no leading boundaries (see `org-scribe-companion-undo--strip').")

(defvar org-scribe-companion-undo--root nil
  "The project root whose page is running a command, or nil.
Bound by `org-scribe-companion-commands-run'.  While nil (a command run
from anywhere else, or the undo itself) no step is recorded.")

(defun org-scribe-companion-undo--strip (list)
  "Return LIST without its leading undo boundaries (nil entries).
Emacs adds boundaries on its own (after a command, on a timer); they are
not changes, so they must not count as the writer editing the buffer."
  (while (and (consp list) (null (car list)))
    (setq list (cdr list)))
  list)

(defun org-scribe-companion-undo-clear (&optional root)
  "Forget the page's steps for ROOT, or for every project when ROOT is nil."
  (if root
      (setq org-scribe-companion-undo--stacks
            (cl-remove root org-scribe-companion-undo--stacks :key #'car :test #'equal))
    (setq org-scribe-companion-undo--stacks nil
          org-scribe-companion-undo--tops nil)))

(defun org-scribe-companion-undo--forget-buffer (buffer)
  "Drop every step that changed BUFFER, in every project."
  (setq org-scribe-companion-undo--stacks
        (mapcar (lambda (stack)
                  (cons (car stack)
                        (cl-remove buffer (cdr stack)
                                   :key (lambda (entry) (plist-get entry :buffer)))))
                org-scribe-companion-undo--stacks))
  (setf (alist-get buffer org-scribe-companion-undo--tops nil 'remove) nil))

(defun org-scribe-companion-undo-record (buffer start)
  "Remember the step just made in BUFFER, whose undo list was START before it.
Does nothing outside a page command, or when the step changed nothing.
Ends the step with an undo boundary, so a change typed next in Emacs does
not merge into it."
  (when org-scribe-companion-undo--root
    (with-current-buffer buffer
      (let ((head buffer-undo-list)
            (root org-scribe-companion-undo--root))
        (when (and (consp head) (not (eq head start)))
          ;; The writer changed this buffer since the page's last step in it:
          ;; the older steps would now undo at the wrong places.
          (unless (eq (org-scribe-companion-undo--strip start)
                      (alist-get buffer org-scribe-companion-undo--tops))
            (org-scribe-companion-undo--forget-buffer buffer))
          (let ((stack (alist-get root org-scribe-companion-undo--stacks nil nil #'equal)))
            (push (list :buffer buffer :head head :stop start) stack)
            (when (> (length stack) org-scribe-companion-undo-depth)
              (setq stack (seq-take stack org-scribe-companion-undo-depth)))
            (setf (alist-get root org-scribe-companion-undo--stacks nil nil #'equal) stack))
          (setf (alist-get buffer org-scribe-companion-undo--tops) head)
          (undo-boundary))))))

(defun org-scribe-companion-undo--valid-p (entry)
  "Return non-nil when ENTRY is still the newest change in its buffer."
  (let ((buffer (plist-get entry :buffer)))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (and (consp buffer-undo-list)
                (eq (org-scribe-companion-undo--strip buffer-undo-list)
                    (alist-get buffer org-scribe-companion-undo--tops)))))))

(defun org-scribe-companion-undo-top (root)
  "Return the newest step of ROOT's page, or nil."
  (car (alist-get root org-scribe-companion-undo--stacks nil nil #'equal)))

(defun org-scribe-companion-undo-available-p (root)
  "Return non-nil when the page of ROOT has a step it can undo now."
  (when-let* ((entry (org-scribe-companion-undo-top root)))
    (org-scribe-companion-undo--valid-p entry)))

(defun org-scribe-companion-undo--changes (entry)
  "Return ENTRY's undo entries, newest first, or nil if Emacs dropped them.
Emacs truncates long undo histories; a step that lost its STOP cannot be
undone whole."
  (let ((list (plist-get entry :head))
        (stop (plist-get entry :stop))
        changes)
    (while (and (consp list) (not (eq list stop)))
      (when (car list) (push (car list) changes))
      (setq list (cdr list)))
    (when (eq list stop) (nreverse changes))))

(defun org-scribe-companion-undo-pop (root)
  "Remove the newest step of ROOT's page and return it."
  (let ((stack (alist-get root org-scribe-companion-undo--stacks nil nil #'equal)))
    (setf (alist-get root org-scribe-companion-undo--stacks nil nil #'equal) (cdr stack))
    (car stack)))

(defun org-scribe-companion-undo-left-at (buffer)
  "Note that the page has just left BUFFER's undo list as it is now.
Called after the page undoes a step, so the step below it stays undoable."
  (with-current-buffer buffer
    (setf (alist-get buffer org-scribe-companion-undo--tops)
          (org-scribe-companion-undo--strip buffer-undo-list))
    (undo-boundary)))

(provide 'org-scribe-companion-undo)

;;; org-scribe-companion-undo.el ends here
