;;; org-scribe-planner-integration.el --- Integration between org-scribe and org-scribe-planner -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Javier Castilla
;; Keywords: org, writing, planning

;;; Commentary:

;; Optional integration module connecting org-scribe (writing project
;; management) with org-scribe-planner (writing goal planning).
;;
;; This file is NOT loaded automatically by org-scribe.  To enable
;; the integration, add the following to your init.el after both
;; packages are loaded:
;;
;;   (with-eval-after-load 'org-scribe-planner
;;     (require 'org-scribe-planner-integration))
;;
;; What this module provides:
;;
;;   1. Auto-load the plan associated with the active org-scribe project
;;      when any file in that project is opened.
;;
;;   2. Push live word counts from org-scribe (WORDCOUNT properties) to
;;      the planner automatically after running word count commands.
;;
;;   3. Wire `org-scribe-planner-wordcount-function' so that
;;      `org-scribe-planner-update-progress' reads word counts directly
;;      from the manuscript instead of prompting the user.
;;
;;   4. Wire `org-scribe-planner-project-root-function' so that
;;      `org-scribe-planner-load-plan' starts its file picker at the
;;      current org-scribe project root.
;;
;;   5. Offer plan creation when a new org-scribe project is created.
;;
;;   6. Add a planner sub-hydra accessible via W in the main org-scribe
;;      hydra.
;;
;;   7. Show today's writing target/actual in the mode line when a plan
;;      is active.
;;
;; Each feature is independent.  To disable a specific feature:
;;
;;   ;; Stop auto-loading plans on project open:
;;   (remove-hook 'org-scribe-mode-hook
;;                #'org-scribe-planner-integration--auto-load-plan)
;;
;;   ;; Disable automatic progress update after word counting:
;;   (advice-remove 'org-scribe/ews-org-count-words
;;                  #'org-scribe-planner-integration--after-wordcount)
;;
;;   ;; Revert to manual prompt in update-progress:
;;   (setq org-scribe-planner-wordcount-function nil)
;;
;;   ;; Revert to default directory in load-plan file picker:
;;   (setq org-scribe-planner-project-root-function nil)

;;; Code:

(require 'org-scribe-planner)

(unless (featurep 'org-scribe)
  (user-error "org-scribe-planner-integration requires org-scribe to be loaded first"))

;; Silence byte-compiler for org-scribe symbols used here
(declare-function org-scribe-project-root "core/org-scribe-core")
(declare-function org-scribe-project-structure "core/org-scribe-core")
(declare-function org-scribe/ews-org-count-words "counting/org-scribe-wordcount")
(declare-function org-scribe-create-novel-project "templates/org-scribe-project")
(declare-function org-scribe-create-short-story-project "templates/org-scribe-project")
(declare-function hydra-org-scribe/body "ui/org-scribe-hydra")
(declare-function hydra-org-scribe-planner/body "integration/org-scribe-planner-integration")
(declare-function org-scribe-planner-show-current-plan "org-scribe-planner")
(declare-function org-scribe-planner-new-plan "org-scribe-planner")
(declare-function org-scribe-planner-load-plan "org-scribe-planner")
(declare-function org-scribe-planner-update-progress "org-scribe-planner")
(declare-function org-scribe-planner-update-daily-word-count "org-scribe-planner")
(declare-function org-scribe-planner-recalculate "org-scribe-planner")
(declare-function org-scribe-planner-adjust-remaining-plan "org-scribe-planner")
(declare-function org-scribe-planner-show-milestones "org-scribe-planner")
(declare-function org-scribe-planner-dashboards-menu "org-scribe-planner-dashboards")

;;; Feature 1: Auto-load plan when an org-scribe project is opened

(defun org-scribe-planner-integration--find-plan-file (root)
  "Search for a plan .org file in ROOT using a priority order.
Returns the path to the first candidate found, or nil if none exists.

Search order:
  1. <root>/plan.org
  2. <root>/<title>-plan.org  (title derived from .org-scribe-project)
  3. Any single .org file in ROOT that has a TOTAL_WORDS property."
  (let ((candidate-1 (expand-file-name "plan.org" root)))
    (cond
     ;; 1. plain plan.org
     ((file-exists-p candidate-1) candidate-1)

     ;; 2. <title>-plan.org derived from the marker file
     ((let* ((marker (expand-file-name ".org-scribe-project" root))
             (title (when (file-exists-p marker)
                      (with-temp-buffer
                        (insert-file-contents marker)
                        (goto-char (point-min))
                        (when (re-search-forward "^# Writing project: \\(.*\\)$" nil t)
                          (match-string 1)))))
             (slug (when title
                     (downcase (replace-regexp-in-string "[^[:alnum:]]" "-" title))))
             (candidate-2 (when slug
                            (expand-file-name (concat slug "-plan.org") root))))
        (when (and candidate-2 (file-exists-p candidate-2))
          candidate-2)))

     ;; 3. Any single .org file in ROOT with a TOTAL_WORDS property
     ((let* ((org-files (directory-files root t "\\.org$" t))
             (plan-file (cl-find-if
                         (lambda (f)
                           (with-temp-buffer
                             (insert-file-contents f)
                             (goto-char (point-min))
                             (re-search-forward "^:TOTAL_WORDS:" nil t)))
                         org-files)))
        plan-file)))))

(defun org-scribe-planner-integration--auto-load-plan ()
  "Try to auto-load the plan associated with the current org-scribe project.
Looks for a .org plan file in the project root.  Silently does
nothing if no candidate is found or if a plan is already active."
  ;; Only act when org-scribe-mode is enabled in a real project
  (when (and (featurep 'org-scribe)
             ;; Don't reload if a plan is already active
             (not org-scribe-planner--current-plan))
    (when-let* ((root (org-scribe-project-root))
                ;; Confirm this is a genuine org-scribe project
                (_ (file-exists-p (expand-file-name ".org-scribe-project" root)))
                (plan-file (org-scribe-planner-integration--find-plan-file root))
                (plan (ignore-errors (org-scribe-planner--load-plan plan-file))))
      (setq org-scribe-planner--current-plan plan)
      (setq org-scribe-planner--current-plan-file plan-file)
      (run-hook-with-args 'org-scribe-planner-after-plan-load-hook plan plan-file)
      (message "org-scribe-planner: loaded plan \"%s\"."
               (org-scribe-plan-title plan)))))

(add-hook 'org-scribe-mode-hook #'org-scribe-planner-integration--auto-load-plan)

;;; Feature 2 & 3: Word count bridge

(defun org-scribe-planner-integration--sum-wordcounts (novel-file)
  "Sum all WORDCOUNT properties in NOVEL-FILE and return the total.
Returns nil if NOVEL-FILE is nil or unreadable."
  (when (and novel-file (file-readable-p novel-file))
    (let ((total 0))
      (with-temp-buffer
        (insert-file-contents novel-file)
        (org-mode)
        (org-map-entries
         (lambda ()
           (let ((wc (org-entry-get nil "WORDCOUNT")))
             (when wc
               (setq total (+ total (string-to-number wc))))))))
      total)))

(defun org-scribe-planner-integration--wordcount ()
  "Return total word count for the current org-scribe project.
Sums all WORDCOUNT properties across the project's manuscript file.
Returns nil if no count is available (falls back to manual prompt)."
  (when (featurep 'org-scribe)
    (when-let* ((struct (ignore-errors (org-scribe-project-structure)))
                (novel-file (plist-get struct :novel-file)))
      (org-scribe-planner-integration--sum-wordcounts novel-file))))

;; Wire the wordcount function into the planner
(setq org-scribe-planner-wordcount-function
      #'org-scribe-planner-integration--wordcount)

(defun org-scribe-planner-integration--after-wordcount (&rest _)
  "Update the active plan after org-scribe word counting finishes.
Reads the new project total and saves it to the plan file silently."
  (when (and org-scribe-planner--current-plan
             org-scribe-planner--current-plan-file
             org-scribe-planner-wordcount-function)
    (let ((total (funcall org-scribe-planner-wordcount-function)))
      (when total
        (setf (org-scribe-plan-current-words org-scribe-planner--current-plan) total)
        (org-scribe-planner--save-plan org-scribe-planner--current-plan
                                       org-scribe-planner--current-plan-file)
        (run-hook-with-args 'org-scribe-planner-after-progress-update-hook
                            org-scribe-planner--current-plan total nil)
        (message "org-scribe-planner: progress updated to %d words." total)))))

(advice-add 'org-scribe/ews-org-count-words :after
            #'org-scribe-planner-integration--after-wordcount)

;;; Feature 4: Project root → plan file picker

(setq org-scribe-planner-project-root-function #'org-scribe-project-root)

;;; Feature 5: Offer plan creation after project creation

(defun org-scribe-planner-integration--offer-plan (project-dir title &rest _)
  "Offer to create an org-scribe-planner plan for a new project.
PROJECT-DIR is the new project directory, TITLE is the project title."
  (when (yes-or-no-p (format "Create a writing plan for \"%s\" with org-scribe-planner? "
                             title))
    (let ((default-directory project-dir))
      (org-scribe-planner-new-plan))))

(advice-add 'org-scribe-create-novel-project :after
            #'org-scribe-planner-integration--offer-plan)
(advice-add 'org-scribe-create-short-story-project :after
            #'org-scribe-planner-integration--offer-plan)

;;; Feature 6: Planner sub-hydra in the org-scribe Hydra menu

;; Require hydra for the defhydra macro
(require 'hydra)

;;;###autoload (autoload 'hydra-org-scribe-planner/body "integration/org-scribe-planner-integration" nil t)
(defhydra hydra-org-scribe-planner (:color blue :hint nil)
  "
  org-scribe-planner
  ──────────────────────────────────────────────────
  _p_: Show current plan calendar
  _n_: New plan
  _l_: Load plan
  _u_: Update progress (from word count)
  _d_: Update daily word count
  _r_: Recalculate plan
  _a_: Adjust remaining days
  _m_: Milestones
  _D_: Dashboards menu
  _q_: Back to main menu
  "
  ("p" org-scribe-planner-show-current-plan "show calendar")
  ("n" org-scribe-planner-new-plan "new plan")
  ("l" org-scribe-planner-load-plan "load plan")
  ("u" org-scribe-planner-update-progress "update progress")
  ("d" org-scribe-planner-update-daily-word-count "update daily")
  ("r" org-scribe-planner-recalculate "recalculate")
  ("a" org-scribe-planner-adjust-remaining-plan "adjust remaining")
  ("m" org-scribe-planner-show-milestones "milestones")
  ("D" org-scribe-planner-dashboards-menu "dashboards")
  ("q" hydra-org-scribe/body "back"))

;; Add W (Writing plan) to the main org-scribe hydra keymap.
;; This key is chosen to avoid conflict with P (plot threads).
;; The binding is not shown in the main hydra hint; it is documented
;; in the integration module commentary above.
(with-eval-after-load 'org-scribe-hydra
  (when (boundp 'hydra-org-scribe/keymap)
    (define-key hydra-org-scribe/keymap (kbd "W")
      #'hydra-org-scribe-planner/body)))

;;; Feature 7: Mode-line status

(defun org-scribe-planner-integration--mode-line ()
  "Return a mode-line string showing today's writing target/actual.
Returns nil when no plan is loaded or no entry exists for today."
  (when-let* ((plan org-scribe-planner--current-plan)
              (target (org-scribe-plan-daily-words plan))
              (today (org-scribe-planner--get-today-date))
              (entry (assoc today (org-scribe-plan-daily-word-counts plan)))
              (actual (org-scribe-planner--get-entry-words entry)))
    (format " [W:%d/%d]" actual target)))

(add-to-list 'mode-line-misc-info
             '(:eval (org-scribe-planner-integration--mode-line)))

;;; Provide

(provide 'org-scribe-planner-integration)

;;; org-scribe-planner-integration.el ends here
