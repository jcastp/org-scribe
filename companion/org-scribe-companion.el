;;; org-scribe-companion.el --- Visual companion for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Entry point of the optional visual companion: a browser front end for a
;; novel project, with Emacs as the only program that reads and writes the
;; project files.  Off until `org-scribe-companion-enable' is set.
;;
;; The user-facing checks (switch, project type, per-project gate) live in
;; `org-scribe-companion-open' in core/org-scribe-core.el, so that with the
;; switch off this file is never loaded.  This file starts and stops the
;; server, keeps the browser told about changes (buffer hooks, debounced),
;; and opens the browser.

;;; Code:

(require 'org)
(require 'org-id)
(require 'json)
(require 'org-scribe-messages)
(require 'org-scribe-core)
(require 'org-scribe-companion-model)
(require 'org-scribe-companion-commands)
(require 'org-scribe-companion-server)

(defvar org-scribe-companion-follow)
(defvar org-scribe-companion-browser-function)
(defvar org-scribe-companion-app-mode-browsers)

;;; Headings without an ID

(defun org-scribe-companion--headings-without-id (snapshot)
  "Return how many acts, chapters and scenes of SNAPSHOT have no ID.
Implicit parents (made for headings outside any act or chapter) are not
counted: there is no heading to give an ID to."
  (let ((count 0))
    (cl-flet ((check (heading)
                (when (and (eq (plist-get heading :id) :null)
                           (not (plist-get heading :implicit)))
                  (cl-incf count))))
      (seq-doseq (act (plist-get snapshot :acts))
        (check act)
        (seq-doseq (chapter (plist-get act :chapters))
          (check chapter)
          (seq-doseq (scene (plist-get chapter :scenes))
            (check scene)))))
    count))

(defun org-scribe-companion--add-missing-ids (root)
  "Give an ID to every act, chapter and scene without one at ROOT.
An ordinary edit of the manuscript buffer, left unsaved.  Returns how many
IDs were added."
  (let* ((default-directory (file-name-as-directory root))
         (manuscript (plist-get (org-scribe-project-structure) :manuscript-file))
         (chapter-level (org-scribe-chapter-level))
         (scene-level (org-scribe-scene-level))
         (count 0))
    (with-current-buffer (find-file-noselect manuscript)
      (org-with-wide-buffer
       (goto-char (point-min))
       (org-map-entries
        (lambda ()
          (when (and (memq (org-current-level) (list 1 chapter-level scene-level))
                     (not (member "noexport" (org-get-tags)))
                     (not (org-id-get)))
            (org-id-get-create)
            (cl-incf count)))
        nil 'file)))
    count))

(defun org-scribe-companion--ensure-ids (root)
  "Offer once to add the IDs the project at ROOT lacks.
Returns the number of headings that will stay without an ID."
  (let ((missing (org-scribe-companion--headings-without-id
                  (org-scribe-companion--snapshot root))))
    (when (> missing 0)
      (if (yes-or-no-p (org-scribe-msg 'companion-ask-add-ids missing))
          (message "%s" (org-scribe-msg 'companion-ids-added
                                        (org-scribe-companion--add-missing-ids root)))
        (message "%s" (org-scribe-msg 'companion-ids-missing-note missing))))
    (org-scribe-companion--headings-without-id
     (org-scribe-companion--snapshot root))))

;;; Debugging

(defun org-scribe-companion-debug-snapshot ()
  "Show the current project's snapshot as JSON in a buffer.
Meant for development.  Available once the companion module is loaded
(open the companion first, or `require' it)."
  (interactive)
  (let ((json (org-scribe-companion--snapshot-json))
        (buffer (get-buffer-create (org-scribe-msg 'companion-debug-buffer-name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert json)
        (when (require 'json-ts-mode nil t)
          (ignore-errors (json-ts-mode)))
        (when (and (not (derived-mode-p 'json-ts-mode)) (require 'js nil t))
          (js-json-mode))
        (ignore-errors (json-pretty-print-buffer))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

;;; Telling the browser about changes

(defvar org-scribe-companion--debounce-seconds 0.7
  "Seconds without a change before the browser is told.")

(defvar org-scribe-companion--hooked-buffers nil
  "The buffers that carry the change hooks.")

(defvar org-scribe-companion--debounce-timer nil)
(defvar org-scribe-companion--last-revision nil
  "The revision last sent in a `changed' event.")

(defun org-scribe-companion--announce-change ()
  "Send `changed' with the current revision, unless it was already sent."
  (setq org-scribe-companion--debounce-timer nil)
  (when-let* ((root (org-scribe-companion-server-root)))
    (with-demoted-errors "org-scribe-companion: %S"
      (let ((revision (org-scribe-companion--revision-string root)))
        (unless (equal revision org-scribe-companion--last-revision)
          (setq org-scribe-companion--last-revision revision)
          (org-scribe-companion-server-broadcast "changed" (list :revision revision)))))))

(defun org-scribe-companion--on-change (&rest _)
  "Schedule a `changed' event.  Does no work itself: it only restarts a timer."
  (when (timerp org-scribe-companion--debounce-timer)
    (cancel-timer org-scribe-companion--debounce-timer))
  (setq org-scribe-companion--debounce-timer
        (run-at-time org-scribe-companion--debounce-seconds nil
                     #'org-scribe-companion--announce-change)))

(defun org-scribe-companion--install-hooks (root)
  "Hook the manuscript and design buffers of the project at ROOT."
  (org-scribe-companion--remove-hooks)
  (let* ((default-directory (file-name-as-directory root))
         (structure (org-scribe-project-structure))
         (manuscript (plist-get structure :manuscript-file)))
    (dolist (file (list manuscript (plist-get structure :design-file)))
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (add-hook 'after-change-functions #'org-scribe-companion--on-change nil t)
          (add-hook 'after-save-hook #'org-scribe-companion--on-change nil t)
          (when (equal file manuscript)
            (add-hook 'post-command-hook #'org-scribe-companion--follow nil t))
          (push (current-buffer) org-scribe-companion--hooked-buffers))))))

(defun org-scribe-companion--remove-hooks ()
  "Unhook every buffer hooked by `org-scribe-companion--install-hooks'."
  (dolist (buffer org-scribe-companion--hooked-buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (remove-hook 'after-change-functions #'org-scribe-companion--on-change t)
        (remove-hook 'after-save-hook #'org-scribe-companion--on-change t)
        (remove-hook 'post-command-hook #'org-scribe-companion--follow t))))
  (setq org-scribe-companion--hooked-buffers nil)
  (when (timerp org-scribe-companion--debounce-timer)
    (cancel-timer org-scribe-companion--debounce-timer))
  (setq org-scribe-companion--debounce-timer nil))

;;; Follow mode

(defvar org-scribe-companion--last-focus nil
  "The ID last sent in a `focus' event.")

(defun org-scribe-companion--scene-id-at-point ()
  "Return the ID of the scene (or chapter, or act) point is in, or nil.
The nearest heading at or above the scene level, going up from point."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading t)
        (let ((limit (org-scribe-scene-level)))
          (while (and (> (org-current-level) limit) (org-up-heading-safe)))
          (and (<= (org-current-level) limit) (org-id-get)))))))

(defun org-scribe-companion--follow ()
  "Tell the browser which scene point moved to, when it changed.
Runs on `post-command-hook' in the manuscript buffer; does nothing unless
a browser is listening and `org-scribe-companion-follow' is on."
  (when (and org-scribe-companion-follow
             (org-scribe-companion-server-subscribed-p))
    (let ((id (org-scribe-companion--scene-id-at-point)))
      (when (and id (not (equal id org-scribe-companion--last-focus)))
        (setq org-scribe-companion--last-focus id)
        (org-scribe-companion-server-broadcast "focus" (list :id id))))))

(defun org-scribe-companion--show-scene ()
  "Tell the browser to show the card of the scene at point.
Works even when follow mode is off or the scene was already shown."
  (interactive)
  (let ((id (org-scribe-companion--scene-id-at-point)))
    (cond ((not (org-scribe-companion-server-running-p))
           (user-error "%s" (org-scribe-msg 'companion-not-running)))
          ((null id)
           (user-error "%s" (org-scribe-msg 'companion-error-no-scene-here)))
          (t (setq org-scribe-companion--last-focus id)
             (org-scribe-companion-server-broadcast "focus" (list :id id))))))

;;; Start, stop, open

(defun org-scribe-companion--start (root)
  "Serve the project at ROOT, starting the server if needed; return the URL.
One server serves one project at a time: starting for another project
switches to it and tells the browser."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (cond
     ((not (org-scribe-companion-server-running-p))
      (org-scribe-companion-server-start root)
      (add-hook 'kill-emacs-hook #'org-scribe-companion--stop))
     ((not (equal root (org-scribe-companion-server-root)))
      (org-scribe-companion-server-set-root root)))
    (setq org-scribe-companion--last-revision nil
          org-scribe-companion--last-focus nil)
    (org-scribe-companion--install-hooks root)
    (org-scribe-companion--announce-change)
    (org-scribe-companion-server-url)))

(defun org-scribe-companion--stop ()
  "Stop the server and unhook the buffers.  Returns non-nil if it was running."
  (let ((was-running (org-scribe-companion-server-running-p)))
    (org-scribe-companion--remove-hooks)
    (org-scribe-companion-server-stop)
    (remove-hook 'kill-emacs-hook #'org-scribe-companion--stop)
    was-running))

(defun org-scribe-companion-browse-app-mode (url)
  "Show URL in a browser app window, without browser chrome.
Tries each executable of `org-scribe-companion-app-mode-browsers' with
--app=URL; falls back to `browse-url'."
  (if-let* ((browser (cl-some #'executable-find org-scribe-companion-app-mode-browsers)))
      (start-process "org-scribe-companion-browser" nil browser (concat "--app=" url))
    (browse-url url)))

(defun org-scribe-companion--open (root)
  "Offer to add missing IDs, serve the project at ROOT, and open the browser."
  (org-scribe-companion--ensure-ids root)
  (let ((url (org-scribe-companion--start root)))
    (funcall org-scribe-companion-browser-function url)
    (message "%s" (org-scribe-msg 'companion-opened url))))

(provide 'org-scribe-companion)

;;; org-scribe-companion.el ends here
