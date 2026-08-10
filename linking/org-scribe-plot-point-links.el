;;; org-scribe-plot-point-links.el --- Plot point linking for org-scribe -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ID-based linking for *plot points* — the thirteen non-negotiables of the
;; method (Theme Stated, Inciting Incident, Opponent, … Moral Decision).
;;
;; Plot points differ from every other entity type in org-scribe in one
;; respect worth knowing before reading further: *they are a fixed set*.
;; Characters, locations and plot threads are invented per story; the
;; thirteen are the same thirteen in every project, shipped by the template,
;; and the writer fills in their bodies rather than adding or removing rows.
;; Two consequences follow:
;;
;;   - the `setup' and `batch-link' flows are less interesting here than for
;;     the other entities, though they are generated for consistency; and
;;   - "orphan" means something genuinely useful.  For a character, an orphan
;;     is a cast member no scene uses.  For a plot point it is one of the
;;     thirteen that no scene serves — which is exactly the method's own
;;     check ("las que no sirven a ninguno son sospechosas") and the reason
;;     this entity type exists rather than the points being a plain list.
;;
;; Structurally the thirteen are *level-2* headings under a level-1 wrapper
;; ("Los trece irrenunciables" / "The Thirteen Non-Negotiables") in the same
;; file as the plot threads.  Level 2 is deliberate: plot threads are level-1
;; headings in that file, and putting the thirteen alongside them would leave
;; only the heading text to tell the two kinds apart.

;;; Code:

(require 'org)
(require 'org-scribe-core)
(require 'org-scribe-messages)
(require 'org-scribe-linking-core)

(declare-function org-scribe--get-plot-thread-file "org-scribe-plot-links")

;;; Plot Point Heading Predicate

(defun org-scribe--plot-point-heading-p ()
  "Return non-nil if the heading at point is one of the thirteen plot points.
Plot points are level-2 headings under the level-1 wrapper named by the
`plot-points' entry of `org-scribe--section-heading-aliases'.

Unlike the other entity predicates this one needs no project-type branch:
the wrapper heading identifies the section in novel and short-story
layouts alike, and requiring level 2 keeps the wrapper itself from
matching and becoming a phantom entity."
  (and (= (org-current-level) 2)
       (org-scribe--heading-parent-section-p 'plot-points)))

;;; Plot Point File Resolver

(defun org-scribe--get-plot-point-file ()
  "Get the path to the file holding the thirteen plot points.
The same file as the plot threads: they are two kinds of heading in one
plot file, not two files."
  (require 'org-scribe-plot-links)
  (org-scribe--get-plot-thread-file))

;;; Entity Definition

(org-scribe-define-entity plot-point
  ;; ── Config (entity descriptor) ──────────────────────────────────────
  :file-fn                  org-scribe--get-plot-point-file
  :heading-predicate        org-scribe--plot-point-heading-p
  :properties               (plot-point)
  :msg-added-ids            msg-added-plot-point-ids
  :msg-ids-updated          msg-plot-point-ids-updated
  :error-no-file            error-no-plot-point-file
  :error-none-found         error-no-plot-points-found
  :prompt-select            prompt-select-plot-point
  :prompt-select-multi      prompt-select-plot-points-multi
  :error-no-id              error-no-id-for-plot-point
  :msg-inserted-links       msg-inserted-plot-point-links
  :msg-no-selected          msg-no-plot-points-selected
  :msg-set                  msg-set-plot-points
  :msg-updated-single       msg-updated-plot-point
  :msg-no-updates           msg-no-plot-point-updates-needed
  :msg-updated-links        msg-updated-plot-point-links
  :msg-setting-up           msg-setting-up-plot-point-links
  :question-link-existing   question-link-existing-plot-points
  :msg-setup-complete       msg-plot-point-setup-complete
  :msg-updated-link-names   msg-updated-plot-point-link-names
  :msg-no-link-updates-type "plot point"
  :msg-updated-all-type     "plot point"
  ;; ── Generated function names ─────────────────────────────────────
  :get-all-name             org-scribe--get-all-plot-points
  :create-link-name         org-scribe--create-plot-point-link
  :add-ids-to-all-name      org-scribe--add-id-to-all-plot-points
  :add-ids-name             org-scribe-add-plot-point-ids
  :insert-link-name         org-scribe-insert-plot-point-link
  :insert-multi-name        org-scribe-insert-multiple-plot-point-links
  :set-scene-name           org-scribe-set-scene-plot-points
  :set-scene-property       plot-point
  :link-in-prop-name        org-scribe--link-plot-points-in-property
  :link-scene-name          org-scribe-link-scene-plot-points
  :link-all-name            org-scribe-link-all-scene-plot-points
  :setup-name               org-scribe-setup-plot-point-links
  :setup-add-ids-fn         org-scribe-add-plot-point-ids
  :setup-link-all-fn        org-scribe-link-all-scene-plot-points
  :update-names-name        org-scribe-update-plot-point-link-names
  :update-all-name          org-scribe-update-all-plot-point-link-names)

;;; Plot-Point-Specific Functions

;;;###autoload
(defun org-scribe-jump-to-plot-point ()
  "Jump to the plot point this scene serves, from the scene heading."
  (interactive)
  (let* ((prop (org-scribe-scene-property-get 'plot-point))
         (points (when prop (org-scribe--property-to-list prop))))
    (cond
     ((null prop)
      (message (org-scribe-msg 'msg-no-plot-point-property)))
     ((null points)
      (message (org-scribe-msg 'msg-no-plot-point-property)))
     (t
      (let* ((all (org-scribe--get-all-plot-points))
             (name (if (= 1 (length points))
                       (car points)
                     (completing-read (org-scribe-msg 'prompt-select-plot-point)
                                      points nil t)))
             (entry (alist-get name all nil nil #'string=)))
        (if (and entry (car entry))
            (org-id-goto (car entry))
          (user-error (org-scribe-msg 'error-no-id-for-plot-point) name)))))))

(provide 'org-scribe-plot-point-links)
;;; org-scribe-plot-point-links.el ends here
