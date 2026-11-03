;;; org-scribe-planner.el --- Writing planning tool for Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Javier Castilla
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: org, writing, planning, productivity
;; URL: https://codeberg.org/jcastp/org-scribe

;;; Commentary:

;; A comprehensive writing planning tool inspired by pacemaker.press that helps
;; writers calculate, visualize, and manage their writing goals through an
;; interactive interface integrated with Org-mode.
;;
;; Features:
;; - Calculate writing schedules given 2 of 3 variables (total words, daily words, days)
;; - Beautiful calendar visualizations with day-by-day breakdown
;; - Flexible spare days management (holidays, weekends, breaks)
;; - Plan modification and recalculation
;; - Save/load functionality using Org-mode properties
;; - Weekly summaries and milestone tracking
;; - Integration with org-agenda
;;
;; This file is the planner's entry point: it requires the parts below in
;; order, wires the planner into org-scribe, and is the feature every
;; autoload and `featurep' check names.  Each part requires only the ones
;; above it.
;;
;;   org-scribe-planner-engine  plan struct, options, schedule engine,
;;                              status and progress analytics
;;   org-scribe-planner-io      plan files: save, load, discovery
;;   org-scribe-planner-views   calendar, weeks, milestones, agenda
;;   org-scribe-planner-create  the new-plan wizard and onboarding
;;   org-scribe-planner-edit    progress, "What changed?", recalculations
;;
;; The dashboards (org-scribe-planner-dashboards) load on demand.

;;; Code:

(require 'org)
(require 'org-element)
(require 'calendar)
(require 'cl-lib)
(require 'org-scribe-planner-engine)
(require 'org-scribe-planner-io)
(require 'org-scribe-planner-views)
(require 'org-scribe-planner-create)
(require 'org-scribe-planner-edit)

(declare-function hydra-org-scribe/body "ui/org-scribe-hydra")
(declare-function org-scribe--project-marker-set "core/org-scribe-core")
(declare-function org-scribe-planner-dashboards-menu "org-scribe-planner-dashboards")
(declare-function org-scribe-planner-gate "core/org-scribe-core")
(declare-function org-scribe-project-root "core/org-scribe-core")
(declare-function org-scribe-project-structure "core/org-scribe-core")

;;; Loading a plan

;;;###autoload
(defun org-scribe-planner-load-plan (&optional file)
  "Load an existing writing plan and set it as the active plan.
FILE, when supplied, is loaded directly with no file-picker prompt —
the interactive spec supplies it automatically when invoked (via `M-x'
or a keybinding) inside an org-scribe project that already has a valid
saved plan file."
  (interactive
   (let* ((root (ignore-errors (org-scribe-project-root)))
          (in-project (and root
                           (file-exists-p (expand-file-name ".org-scribe-project" root))))
          (plan-file (and in-project (org-scribe-planner--find-plan-file root))))
     (list (and plan-file (org-scribe-planner--plan-file-valid-p plan-file) plan-file))))
  (let* ((start-dir (when org-scribe-planner-project-root-function
                      (funcall org-scribe-planner-project-root-function)))
         (file (or file (org-scribe-planner--select-plan-file "Select plan file: " start-dir))))
    (when file
      (let ((plan (org-scribe-planner--load-plan file)))
        (org-scribe-planner--lock-past-goals plan)
        ;; Set as current active plan
        (setq org-scribe-planner--current-plan plan)
        (setq org-scribe-planner--current-plan-file file)
        ;; Run hook
        (run-hook-with-args 'org-scribe-planner-after-plan-load-hook plan file)
        (org-scribe-planner-show-calendar plan file)))))

;;; Active Plan Management

;;;###autoload
(defun org-scribe-planner-show-current-plan ()
  "Display the calendar for the currently active plan.
If no plan is active, prompt to load one."
  (interactive)
  (org-scribe-planner--with-current-plan (plan file)
    (org-scribe-planner-show-calendar plan file)))

;;;###autoload
(defun org-scribe-planner-current-plan-info ()
  "Show information about the currently active plan."
  (interactive)
  (if (and org-scribe-planner--current-plan
           org-scribe-planner--current-plan-file)
      (message "Active plan: %s (%s)"
               (org-scribe-plan-title org-scribe-planner--current-plan)
               org-scribe-planner--current-plan-file)
    (message "No active plan. Use `org-scribe-planner-load-plan' to load one.")))

;;; org-scribe Integration

;; These features activate automatically when org-scribe is present.
;; Set any defcustom below to nil before loading the planner to disable
;; the corresponding feature.

(defcustom org-scribe-planner-auto-load-plan t
  "When non-nil, find and load the project plan on entering an org-scribe project.
The plan file is located via `org-scribe-planner--find-plan-file'."
  :type 'boolean
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-auto-push-wordcount t
  "When non-nil, push the manuscript word count to the active plan automatically.
Activated after each call to `org-scribe-ews-org-count-words'."
  :type 'boolean
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-offer-plan-on-create t
  "When non-nil, offer to create a writing plan after a new org-scribe project is created."
  :type 'boolean
  :group 'org-scribe-planner)

(defcustom org-scribe-planner-show-mode-line t
  "When non-nil, show today's word count status in the mode line when a plan is active."
  :type 'boolean
  :group 'org-scribe-planner)

;;; Word-count wiring

(defun org-scribe-planner--parent-has-wordcount-p ()
  "Return non-nil if the immediate parent heading of point has a WORDCOUNT property.
Used to avoid double-counting when WORDCOUNT is set on both parent and child headings
\(e.g. act → chapter → scene, where parent totals aggregate their children)."
  (save-excursion
    (when (org-up-heading-safe)
      (org-entry-get nil "WORDCOUNT"))))

(defun org-scribe-planner--sum-wordcounts (novel-file)
  "Sum WORDCOUNT properties in NOVEL-FILE, counting only the highest level in each subtree.
A heading is included only when its immediate parent heading does not already carry a
WORDCOUNT property.  This prevents double-counting in hierarchical manuscripts where
WORDCOUNT on a chapter or act is the aggregate of its scene children.
Prefers a live buffer so unsaved changes are included.  When summing a
live buffer, widens first (`org-with-wide-buffer') so an active
narrowing — e.g. org-scribe's focus mode, which narrows to a single
subtree — does not collapse the \"manuscript total\" down to whatever
subtree happens to be visible."
  (when (and novel-file (file-readable-p novel-file))
    (let ((total 0)
          (live-buffer (find-buffer-visiting novel-file)))
      (if live-buffer
          (with-current-buffer live-buffer
            (org-with-wide-buffer
             (org-map-entries
              (lambda ()
                (let ((wc (org-entry-get nil "WORDCOUNT")))
                  (when (and wc (not (org-scribe-planner--parent-has-wordcount-p)))
                    (setq total (+ total (string-to-number wc)))))))))
        (with-temp-buffer
          (insert-file-contents novel-file)
          (org-mode)
          (org-map-entries
           (lambda ()
             (let ((wc (org-entry-get nil "WORDCOUNT")))
               (when (and wc (not (org-scribe-planner--parent-has-wordcount-p)))
                 (setq total (+ total (string-to-number wc)))))))))
      total)))

(defun org-scribe-planner--wordcount-from-manuscript ()
  "Return the total word count for the current org-scribe project manuscript.
Returns nil when no count is available, causing the planner to prompt instead."
  (when (featurep 'org-scribe)
    (when-let* ((struct (ignore-errors (org-scribe-project-structure)))
                (manuscript-file (plist-get struct :manuscript-file)))
      (org-scribe-planner--sum-wordcounts manuscript-file))))

(defun org-scribe-planner--plan-file-in-current-project-p (plan-file)
  "Return non-nil if PLAN-FILE lives under the current project's root.
Used to guard against pushing word counts from the manuscript of one
project into the plan of another (M9): `org-scribe-planner--wordcount-from-manuscript'
always reads the *current* project's manuscript (via `default-directory'),
but `org-scribe-planner--current-plan-file' can still point at whatever
project's plan was active last — the auto-load hook only runs when no plan
is active at all, so it never swaps plans when switching between two
projects that both already have one loaded."
  (when-let* ((root (ignore-errors (org-scribe-project-root))))
    (file-in-directory-p plan-file root)))

(defun org-scribe-planner--ensure-plan-for-current-project ()
  "Drop the active plan if it does not belong to the current project.
When `org-scribe-planner--current-plan-file' is set but is not under the
current project root, clears it and tries to auto-load the plan that
actually belongs to this project instead.  No-op when there is no active
plan, project detection is unavailable (`org-scribe-project-root' not
loaded — the planner is usable standalone, without org-scribe), or project
detection is inconclusive."
  (when (and org-scribe-planner--current-plan-file
             (fboundp 'org-scribe-project-root)
             (ignore-errors (org-scribe-project-root))
             (not (org-scribe-planner--plan-file-in-current-project-p
                   org-scribe-planner--current-plan-file)))
    (setq org-scribe-planner--current-plan nil)
    (setq org-scribe-planner--current-plan-file nil)
    (org-scribe-planner--auto-load-plan)))

;;;###autoload
(defun org-scribe-planner-record-total (total)
  "Record TOTAL as today's cumulative manuscript word count in the active plan.
Upserts today's DAILY_WORD_COUNTS entry with TOTAL (preserving any
existing note), saves the plan, and fires
`org-scribe-planner-after-progress-update-hook' with today's derived net
delta.  Idempotent: calling repeatedly with the same TOTAL on the same
day reproduces the same entry and delta — there is no separate baseline
to roll forward, unlike the old SYNC_DATE/SYNC_WORDS mechanism.

No-op when `org-scribe-planner-auto-push-wordcount' or
`org-scribe-planner-auto-track-daily' is nil, when there is no active
plan for the current project, when the active plan belongs to a
different project than the current manuscript and no matching plan can
be found for this project (M9 — prevents cross-project data corruption
instead of silently recording into the wrong plan), when TOTAL is not
positive, or when the planner is disabled for the current project (gate
`no', see `org-scribe-planner-gate'): disabling it used to stop the mode
line and the health section but not the ledger, which kept rewriting
plan.org on every count."
  (when (and org-scribe-planner-auto-push-wordcount
             org-scribe-planner-auto-track-daily
             (not (eq (org-scribe-planner-gate) 'no)))
    (unless (and org-scribe-planner--current-plan
                 org-scribe-planner--current-plan-file)
      (org-scribe-planner--auto-load-plan))
    (org-scribe-planner--ensure-plan-for-current-project)
    (when (and org-scribe-planner--current-plan
               org-scribe-planner--current-plan-file
               (> total 0))
      (let* ((plan org-scribe-planner--current-plan)
             (file org-scribe-planner--current-plan-file)
             (today (org-scribe-planner--get-today-date))
             (existing (assoc today (org-scribe-plan-daily-word-counts plan)))
             (existing-note (when existing
                              (org-scribe-planner--get-entry-note existing)))
             ;; The manuscript's count plus the words outside it
             (written (org-scribe-planner--written-total
                       plan total (org-scribe-planner--outside-base file)))
             (entry-data (list :words written :note (or existing-note ""))))
        (org-scribe-planner--lock-past-goals plan)
        ;; The first count of a new day: yesterday is closed, so a stale
        ;; recalculation can be offered before today's words are recorded
        ;; An offer that is declined with C-g, or fails, must not cost
        ;; the day its first entry
        (unless existing
          (condition-case err
              (org-scribe-planner--offer-refresh plan file)
            (quit nil)
            (error (message "Plan recalculation skipped: %s"
                            (error-message-string err)))))
        ;; Upsert today's entry, preserving any hand-written note
        (if existing
            (setcdr existing entry-data)
          (setf (org-scribe-plan-daily-word-counts plan)
                (cons (cons today entry-data)
                      (org-scribe-plan-daily-word-counts plan))))
        (setf (org-scribe-plan-current-words plan) written)
        (org-scribe-planner--save-plan plan file)
        (setq org-scribe-planner--current-plan plan)
        (org-scribe-planner--warn-if-below-start plan)
        (run-hook-with-args 'org-scribe-planner-after-progress-update-hook
                            plan
                            (org-scribe-planner--entry-delta
                             (org-scribe-plan-daily-word-counts plan) today
                             (org-scribe-plan-preexisting-words plan))
                            today)))))

;;; Mode-line status

(defun org-scribe-planner--mode-line ()
  "Return a mode-line string showing today's target/actual, or nil.
In practice a plan is only ever loaded when the project's gate is
\\='yes (see `org-scribe-planner-gate'), but the gate is checked
explicitly here too so the mode-line contribution stays off even if
some other path ever loads a plan into a gated-off project.
The target is today's own goal from the schedule: on a spare day the
string reads \"[W:actual rest]\", and outside the plan just \"[W:actual]\"."
  (when-let* ((plan org-scribe-planner--current-plan)
              (today (org-scribe-planner--get-today-date))
              (actual (org-scribe-planner--entry-delta
                       (org-scribe-plan-daily-word-counts plan) today
                       (org-scribe-plan-preexisting-words plan)))
              ;; The gate and the plan's project are checked last: both look
              ;; the project up on disk, and the mode line is evaluated on
              ;; every redisplay.  A plan left over from another project
              ;; shows nothing here.
              (_ (eq (ignore-errors (org-scribe-planner-gate)) 'yes))
              (_ (or (null org-scribe-planner--current-plan-file)
                     (org-scribe-planner--plan-file-in-current-project-p
                      org-scribe-planner--current-plan-file))))
    (let ((day (cl-find today
                        (ignore-errors (org-scribe-planner--generate-day-schedule plan))
                        :key (lambda (d) (plist-get d :date))
                        :test #'string=)))
      (cond
       ((null day) (format " [W:%d]" actual))
       ((plist-get day :is-spare-day) (format " [W:%d rest]" actual))
       (t (format " [W:%d/%d]" actual (plist-get day :words)))))))

;;; Offer plan creation on project creation

(defun org-scribe-planner--offer-plan-on-create (project-dir title)
  "Offer to create a writing plan for the project just created in PROJECT-DIR.
TITLE is the project's title.  Both creation commands call it, through
`org-scribe--offer-plan-after-create', when run interactively, passing
the directory they created rather than letting this function recompute
it from the parent directory and the title: the two computations
disagreed for a title such as \"~user\", which the creation commands
keep under the parent directory and `expand-file-name' alone turns into
that user's home.  `org-scribe-planner-new-plan' is called with TITLE
and a SAVE-PATH of PROJECT-DIR/plan.org.

Also records the answer in the project's marker file as \"# Planner:
yes\" or \"# Planner: no\" (see `org-scribe-planner-gate'), so a decline
is remembered — the project is never asked again, and every other
gate-aware feature (auto-load, mode line, health report) stays off
until the author explicitly enables it later."
  (if (yes-or-no-p (format "Create a writing plan for \"%s\" with org-scribe-planner? "
                           title))
      (progn
        (org-scribe--project-marker-set project-dir "Planner" "yes")
        (org-scribe-planner-new-plan title (expand-file-name "plan.org" project-dir)))
    (org-scribe--project-marker-set project-dir "Planner" "no")))

;;; Wire up all features (only when org-scribe is present)

(with-eval-after-load 'org-scribe
  ;; Plan-path persistence — always active.  `org-scribe-planner-new-plan'
  ;; and `org-scribe-planner-load-plan' both run this hook themselves after
  ;; activating a plan, so a single hook covers every path (manual new/load,
  ;; the org-scribe-plan dispatcher, and auto-load) with no extra advice.
  (add-hook 'org-scribe-planner-after-plan-load-hook #'org-scribe-planner--save-plan-path)

  ;; Word-count and project-root wiring — always active.
  (setq org-scribe-planner-wordcount-function #'org-scribe-planner--wordcount-from-manuscript)
  (setq org-scribe-planner-project-root-function #'org-scribe-project-root)

  ;; Auto-load plan when opening a project file.
  (when org-scribe-planner-auto-load-plan
    (add-hook 'org-scribe-mode-hook #'org-scribe-planner--auto-load-plan))

  ;; Note: pushing the word count to the active plan after each count is no
  ;; longer advice-based — counting/org-scribe-wordcount.el calls
  ;; `org-scribe-planner-record-total' directly at the end of
  ;; `org-scribe-ews-org-count-words' (guarded on that function existing),
  ;; and `org-scribe-planner-auto-push-wordcount' is checked inside
  ;; `org-scribe-planner-record-total' itself.

  ;; Offering a plan when a project is created is not wired here: the
  ;; creation commands call `org-scribe--offer-plan-after-create'
  ;; themselves, so the offer also happens before the planner has loaded.

  ;; Mode-line status indicator.
  (when org-scribe-planner-show-mode-line
    (add-to-list 'mode-line-misc-info '(:eval (org-scribe-planner--mode-line)))))

;;; Unified project plan entry point

;;;###autoload
(defun org-scribe-plan ()
  "Open the writing plan for the current org-scribe project.
If a plan is already active, show its calendar.  If the project has
declined the planner (see `org-scribe-planner-gate'), offer to enable
it before doing anything else.  If a plan file exists for the current
project but is not yet loaded, load it silently first.  If no plan
file exists, offer to create one.  Falls back to
`org-scribe-planner-show-current-plan' when not inside an org-scribe project."
  (interactive)
  (let* ((root (ignore-errors (org-scribe-project-root)))
         (in-project (and root
                          (file-exists-p
                           (expand-file-name ".org-scribe-project" root)))))
    (cond
     ;; In a project that has declined the planner — offer to enable first.
     ;; This must come before the active-plan branch:
     ;; `org-scribe-planner-disable-for-project' leaves the plan in memory,
     ;; so checking for an active plan first showed a disabled project's
     ;; calendar without ever asking.
     ((and in-project (eq (org-scribe-planner-gate root) 'no))
      (if (yes-or-no-p "The planner is disabled for this project.  Enable it? ")
          (progn
            (org-scribe--project-marker-set root "Planner" "yes")
            (org-scribe-plan))
        (message "org-scribe-planner: staying disabled for this project.")))
     ;; Plan already active — show it
     (org-scribe-planner--current-plan
      (org-scribe-planner--lock-past-goals org-scribe-planner--current-plan)
      (org-scribe-planner-show-calendar org-scribe-planner--current-plan
                                        org-scribe-planner--current-plan-file))
     ;; In project with a valid saved plan file — load silently, then show
     ((and in-project
           (org-scribe-planner--plan-file-valid-p
            (org-scribe-planner--find-plan-file root)))
      (let* ((plan-file (org-scribe-planner--find-plan-file root))
             (plan (org-scribe-planner--load-plan plan-file)))
        (org-scribe-planner--lock-past-goals plan)
        (setq org-scribe-planner--current-plan plan)
        (setq org-scribe-planner--current-plan-file plan-file)
        (run-hook-with-args 'org-scribe-planner-after-plan-load-hook plan plan-file)
        (org-scribe-planner-show-calendar plan plan-file)))
     ;; In project but no valid plan — offer to create one
     (in-project
      (when (yes-or-no-p "No writing plan found for this project.  Create one? ")
        (org-scribe--project-marker-set root "Planner" "yes")
        (org-scribe-planner-new-plan (org-scribe-planner--project-title root)
                                     (expand-file-name "plan.org" root))))
     ;; Not in an org-scribe project — standard show-or-load flow
     (t
      (org-scribe-planner-show-current-plan)))))

;;; Per-project enable/disable

;;;###autoload
(defun org-scribe-planner-enable-for-project ()
  "Enable the writing planner for the current org-scribe project.
Sets \"# Planner: yes\" in .org-scribe-project.  With the gate open,
auto-load, the mode line, and the health report's Writing Plan section
all resume for this project.  Errors if not inside an org-scribe project."
  (interactive)
  (let ((root (org-scribe-project-root)))
    (unless (file-exists-p (expand-file-name ".org-scribe-project" root))
      (user-error "Not inside an org-scribe project"))
    (org-scribe--project-marker-set root "Planner" "yes")
    (message "org-scribe-planner: enabled for this project.")))

;;;###autoload
(defun org-scribe-planner-disable-for-project ()
  "Disable the writing planner for the current org-scribe project.
Sets \"# Planner: no\" in .org-scribe-project.  Does not unload an
already-active plan from memory, but auto-load, the mode line, and the
health report's Writing Plan section all stop for this project.
Errors if not inside an org-scribe project."
  (interactive)
  (let ((root (org-scribe-project-root)))
    (unless (file-exists-p (expand-file-name ".org-scribe-project" root))
      (user-error "Not inside an org-scribe project"))
    (org-scribe--project-marker-set root "Planner" "no")
    (message "org-scribe-planner: disabled for this project.")))

;;; Planner hydra submenu

(when (require 'hydra nil t)
  ;;;###autoload (autoload 'hydra-org-scribe-planner/body "planning/org-scribe-planner" nil t)
  (defhydra hydra-org-scribe-planner (:color blue :hint nil)
    "
  org-scribe-planner
  ──────────────────────────────────────────────────
  _p_: Open project plan (load or create)
  _n_: New plan (fresh start)
  _o_: Onboard existing project
  _l_: Load plan
  _u_: Sync word count (from manuscript)
  _d_: Add session note
  _r_: Recalculate plan
  _a_: Adjust remaining days (keep end date)
  _e_: Adjust end date (keep daily words)
  _c_: Check progress (adjust if ahead/behind)
  _m_: Milestones
  _w_: Weekly summary
  _M_: Month grid
  _D_: Dashboards menu
  _E_: Enable for project
  _X_: Disable for project
  _q_: Back to main menu
  "
    ("p" org-scribe-plan "open project plan")
    ("n" org-scribe-planner-new-plan "new plan")
    ("o" org-scribe-planner-onboard-existing-project "onboard existing")
    ("l" org-scribe-planner-load-plan "load plan")
    ("u" org-scribe-planner-update-progress "sync word count")
    ("d" org-scribe-planner-add-session-note "session note")
    ("r" org-scribe-planner-recalculate "recalculate")
    ("a" org-scribe-planner-adjust-remaining-plan "adjust remaining")
    ("e" org-scribe-planner-adjust-end-date "adjust end date")
    ("c" org-scribe-planner-check-progress "check progress")
    ("m" org-scribe-planner-show-milestones "milestones")
    ("w" org-scribe-planner-show-weekly-summary "weekly summary")
    ("M" org-scribe-planner-show-month-calendar "month grid")
    ("D" org-scribe-planner-dashboards-menu "dashboards")
    ("E" org-scribe-planner-enable-for-project "enable for project")
    ("X" org-scribe-planner-disable-for-project "disable for project")
    ("q" hydra-org-scribe/body "back")))

;;; Provide

(provide 'org-scribe-planner)

;;; org-scribe-planner.el ends here
