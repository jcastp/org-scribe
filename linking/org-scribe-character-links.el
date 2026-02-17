;;; org-scribe-character-links.el --- Character linking system for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides ID-based linking system for characters in writing projects.
;; Uses the generic linking framework from org-scribe-linking-core.el
;; and adds character-specific functions (PoV handling, timeline).
;;
;; Features:
;; - Auto-create IDs when capturing new characters
;; - Insert character links with completion
;; - Link multiple characters at once
;; - Set PoV character for scenes
;; - Jump to character definition from properties
;; - Update all character links in document
;; - Character timeline visualization

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-scribe-core)
(require 'org-scribe-capture)
(require 'org-scribe-messages)
(require 'org-scribe-linking-core)

;;; Character Heading Predicate

(defun org-scribe--character-heading-p ()
  "Return non-nil if the heading at point is a character heading."
  (and (>= (org-current-level) 1)
       (or (org-entry-get nil "Role")
           (org-entry-get nil "TYPE")
           (save-excursion
             (ignore-errors
               (org-up-heading-safe)
               (string-match-p "Character\\|Personaje\\|Protagonist\\|Antagonist\\|Secondary"
                              (org-get-heading t t t t)))))))

;;; Entity Descriptor

(defconst org-scribe--character-entity
  '(:file-fn org-scribe/capture-character-file
    :heading-predicate org-scribe--character-heading-p
    :properties ("PoV" "Characters")
    :msg-added-ids msg-added-ids
    :msg-ids-updated msg-character-ids-updated
    :error-no-file error-no-character-file
    :error-none-found error-no-characters-found
    :prompt-select prompt-select-character
    :prompt-select-multi prompt-select-characters-multi
    :error-no-id error-no-id-for-character
    :msg-inserted-links msg-inserted-links
    :msg-no-selected msg-no-characters-selected
    :msg-set msg-set-characters
    :msg-updated-single msg-updated-characters
    :msg-no-updates msg-no-updates-needed
    :msg-updated-links msg-updated-links
    :msg-setting-up msg-setting-up-links
    :question-link-existing question-link-existing-scenes
    :msg-setup-complete msg-setup-complete
    :msg-updated-link-names msg-updated-pov-and-chars-link-names
    :msg-no-link-updates-type "character"
    :msg-updated-all-type "character")
  "Entity descriptor for characters.")

;;; Generated API Functions

(org-scribe-define-entity org-scribe--character-entity
  :get-file-name         org-scribe--get-character-file
  :get-all-name          org-scribe--get-all-characters
  :create-link-name      org-scribe--create-character-link
  :add-ids-to-all-name   org-scribe--add-id-to-all-characters
  :add-ids-name          org-scribe/add-character-ids
  :insert-link-name      org-scribe/insert-character-link
  :insert-multi-name     org-scribe/insert-multiple-character-links
  :set-scene-name        org-scribe/set-scene-characters
  :set-scene-property    "Characters"
  :link-in-prop-name     org-scribe--link-characters-in-property
  ;; link-scene, link-all, update-names, update-all are custom (handle PoV + Characters)
  :setup-name            org-scribe/setup-character-links
  :setup-add-ids-fn      org-scribe/add-character-ids
  :setup-link-all-fn     org-scribe/link-all-scene-characters)

;;; PoV-Specific Functions (no location/plot equivalent)

;;;###autoload
(defun org-scribe/set-pov-character ()
  "Set the PoV property to a character with ID link.
Specifically designed for the :PoV: property in scene headings."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (let* ((chars (org-scribe--get-all-characters))
         (char-names (mapcar #'car chars)))
    (if (null char-names)
        (message (org-scribe-msg 'error-no-characters-found))
      (let* ((selected (completing-read (org-scribe-msg 'prompt-select-pov) char-names nil t))
             (entry (assoc selected chars))
             (id (cadr entry)))
        (if id
            (progn
              (org-set-property "PoV" (format "[[id:%s][%s]]" id selected))
              (message (org-scribe-msg 'msg-set-pov selected)))
          (message (org-scribe-msg 'error-no-id-for-character selected)))))))

;;;###autoload
(defun org-scribe/jump-to-pov-character ()
  "Jump to the character definition for the PoV of current scene.
Follows the ID link in the :PoV: property."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (if-let ((pov (org-entry-get nil "PoV")))
      (progn
        (if (string-match "\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" pov)
            (let ((id (match-string 1 pov)))
              (org-id-goto id)
              (message (org-scribe-msg 'msg-jump-to-pov)))
          (message (org-scribe-msg 'error-pov-not-link))))
    (message (org-scribe-msg 'error-no-pov-property))))

;;; Custom Link/Update Functions (handle both PoV + Characters)

;;;###autoload
(defun org-scribe/link-scene-characters ()
  "Convert character names to ID links in current scene.
Updates both :PoV: and :Characters: properties."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((updated-pov (org-scribe--link-characters-in-property "PoV"))
          (updated-chars (org-scribe--link-characters-in-property "Characters")))
      (cond
       ((and updated-pov updated-chars)
        (message (org-scribe-msg 'msg-updated-pov-and-chars)))
       (updated-pov
        (message (org-scribe-msg 'msg-updated-pov)))
       (updated-chars
        (message (org-scribe-msg 'msg-updated-characters)))
       (t
        (message (org-scribe-msg 'msg-no-updates-needed)))))))

;;;###autoload
(defun org-scribe/link-all-scene-characters ()
  "Convert character names to ID links in all scenes in current buffer.
Processes all headings with :PoV: or :Characters: properties."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (org-map-entries
       (lambda ()
         (when (or (org-entry-get nil "PoV")
                   (org-entry-get nil "Characters"))
           (let ((updated-pov (org-scribe--link-characters-in-property "PoV"))
                 (updated-chars (org-scribe--link-characters-in-property "Characters")))
             (when (or updated-pov updated-chars)
               (setq count (1+ count))))))
       nil 'file)
      (message (org-scribe-msg 'msg-updated-links count (org-scribe-plural count ""))))))

;;; Update Link Display Names (custom: handles PoV + Characters with 4-state messaging)

(require 'org-scribe-link-update)

;;;###autoload
(defun org-scribe/update-character-link-names ()
  "Update character link display names in current scene.
Refreshes both :PoV: and :Characters: properties.
Returns t if any updates were made, nil otherwise."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let* ((chars-alist (org-scribe--get-all-characters))
           (id-map (org-scribe--build-id-to-name-map chars-alist))
           (updated-pov (org-scribe--update-links-in-property "PoV" id-map))
           (updated-chars (org-scribe--update-links-in-property "Characters" id-map)))
      (cond
       ((and updated-pov updated-chars)
        (message (org-scribe-msg 'msg-updated-pov-and-chars-link-names)))
       (updated-pov
        (message (org-scribe-msg 'msg-updated-pov-link-names)))
       (updated-chars
        (message (org-scribe-msg 'msg-updated-characters-link-names)))
       (t
        (message (org-scribe-msg 'msg-no-link-updates "character"))))
      (or updated-pov updated-chars))))

;;;###autoload
(defun org-scribe/update-all-character-link-names ()
  "Update character link display names in all scenes.
Returns the number of scenes updated."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((chars-alist (org-scribe--get-all-characters))
           (id-map (org-scribe--build-id-to-name-map chars-alist))
           (count 0))
      (org-map-entries
       (lambda ()
         (when (or (org-entry-get nil "PoV")
                   (org-entry-get nil "Characters"))
           (let ((updated-pov (org-scribe--update-links-in-property "PoV" id-map))
                 (updated-chars (org-scribe--update-links-in-property "Characters" id-map)))
             (when (or updated-pov updated-chars)
               (setq count (1+ count))))))
       nil 'file)
      (message (org-scribe-msg 'msg-updated-all-link-names "character"
                              count (org-scribe-plural count "")))
      count)))

;;; Character Timeline

(defun org-scribe--get-all-scenes-with-characters ()
  "Return list of all scenes with PoV or Characters properties.
Each entry is (SCENE-HEADING CHAPTER-HEADING POV-NAME CHARACTERS-LIST)."
  (let ((novel-file (plist-get (org-scribe-project-structure) :novel-file))
        scenes)
    (when (and novel-file (file-exists-p novel-file))
      (with-current-buffer (find-file-noselect novel-file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (org-map-entries
          (lambda ()
            (when (= (org-current-level) 3)
              (let* ((heading (org-get-heading t t t t))
                     (chapter (save-excursion
                               (outline-up-heading 1)
                               (org-get-heading t t t t)))
                     (pov-prop (org-entry-get nil "PoV"))
                     (chars-prop (org-entry-get nil "Characters")))
                (when (or pov-prop chars-prop)
                  (require 'org-scribe-search)
                  (let ((pov-name (when pov-prop
                                   (org-scribe--extract-link-text pov-prop)))
                        (chars-list (when chars-prop
                                     (org-scribe--property-to-list chars-prop))))
                    (push (list heading chapter pov-name chars-list) scenes))))))
          nil 'file))))
    (nreverse scenes)))

(defun org-scribe--get-character-weight (char-name)
  "Get the Weight property for CHAR-NAME from characters file.
Returns the weight as a float, or 999.0 if not found."
  (org-scribe--get-entity-weight org-scribe--character-entity char-name))

(defun org-scribe--collect-unique-characters (scenes)
  "Extract unique character names from SCENES list.
Returns list of unique character names, sorted by Weight property (ascending)."
  (let ((characters (make-hash-table :test 'equal)))
    (dolist (scene scenes)
      (let ((pov (nth 2 scene))
            (chars-list (nth 3 scene)))
        (when (and pov (not (string-empty-p pov)))
          (puthash pov t characters))
        (when chars-list
          (dolist (char chars-list)
            (when (not (string-empty-p char))
              (puthash char t characters))))))
    (let ((char-weights nil))
      (dolist (char-name (hash-table-keys characters))
        (let ((weight (org-scribe--get-character-weight char-name)))
          (push (cons char-name weight) char-weights)))
      (setq char-weights
            (sort char-weights
                  (lambda (a b)
                    (let ((weight-a (cdr a))
                          (weight-b (cdr b)))
                      (if (= weight-a weight-b)
                          (string< (car a) (car b))
                        (< weight-a weight-b))))))
      (mapcar #'car char-weights))))

(defun org-scribe--character-symbol (char-name pov-name chars-list)
  "Return symbol for CHAR-NAME in a scene.
POV-NAME is the scene's PoV character (or nil).
CHARS-LIST is list of other characters in scene."
  (cond
   ((and pov-name (string= char-name pov-name))
    "PoV")
   ((and chars-list (member char-name chars-list))
    "●")
   (t "")))

;;;###autoload
(defun org-dblock-write:character-timeline (params)
  "Generate timeline showing character appearances across scenes.
PARAMS are ignored (reserved for future filtering options)."
  (let* ((scenes (org-scribe--get-all-scenes-with-characters))
         (characters (org-scribe--collect-unique-characters scenes)))
    (if (null scenes)
        (insert "No scenes with character properties found.\n")
      (insert "| Scene | Chapter |")
      (dolist (char characters)
        (insert (format " %s |" char)))
      (insert "\n")
      (insert "|-------+---------+")
      (dolist (_ characters)
        (insert "--------+"))
      (insert "\n")
      (dolist (scene scenes)
        (let ((scene-name (nth 0 scene))
              (chapter (nth 1 scene))
              (pov-name (nth 2 scene))
              (chars-list (nth 3 scene)))
          (insert (format "| %s | %s |" scene-name chapter))
          (dolist (char characters)
            (let ((symbol (org-scribe--character-symbol char pov-name chars-list)))
              (insert (format " %s |" symbol))))
          (insert "\n")))
      (org-table-align))))

;;; Legacy Aliases

(defun org-scribe--ensure-character-has-id ()
  "Ensure the current character heading has a unique ID."
  (org-id-get-create))

(defalias 'org-scribe--get-character-name-at-point 'org-scribe--entity-name-at-point)

(provide 'org-scribe-character-links)

;;; org-scribe-character-links.el ends here
