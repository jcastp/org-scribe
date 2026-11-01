;;; org-scribe-companion-progress.el --- Visual companion: progress -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; The data behind the page's Progress tab and its header chip ("Today 350
;; / 800"), served on GET /api/progress:
;;   - the manuscript's words against its objective (the outermost
;;     objectives, as the health report adds them up) and the project's
;;     word target.  The acts' and chapters' own figures are already in the
;;     snapshot;
;;   - the writing plan, when there is one: today's goal and words, where
;;     the plan stands, the streak, the pace, and the day-by-day series the
;;     page draws its burndown from.
;;
;; The planner is bundled but not loaded at startup, and the companion must
;; never load it on its own.  It is required here only when the project has
;; opted in (`# Planner: yes') and a plan file exists; otherwise the answer
;; says why there is no plan (`off', `none') and nothing is loaded.  The
;; figures come from the planner's own functions (the ones its dashboards
;; use), so the page and F8 W cannot disagree.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org-scribe-messages)
(require 'org-scribe-core)
(require 'org-scribe-companion-model)
(require 'org-scribe-companion-commands)

(declare-function org-scribe--health-word-totals "org-scribe-health")
(declare-function org-scribe-planner--find-plan-file "org-scribe-planner")
(declare-function org-scribe-planner--ensure-plan-for-current-project "org-scribe-planner")
(declare-function org-scribe-planner--auto-load-plan "org-scribe-planner")
(declare-function org-scribe-planner--lock-past-goals "org-scribe-planner")
(declare-function org-scribe-planner--get-today-date "org-scribe-planner")
(declare-function org-scribe-planner--generate-day-schedule "org-scribe-planner")
(declare-function org-scribe-planner--status "org-scribe-planner")
(declare-function org-scribe-planner--entry-delta "org-scribe-planner")
(declare-function org-scribe-planner--get-entry-words "org-scribe-planner")
(declare-function org-scribe-planner--planned-total "org-scribe-planner")
(declare-function org-scribe-planner--rebase-step "org-scribe-planner")
(declare-function org-scribe-planner--get-today-target "org-scribe-planner-dashboards")
(declare-function org-scribe-planner--get-today-actual "org-scribe-planner-dashboards")
(declare-function org-scribe-planner--calculate-current-streak "org-scribe-planner-dashboards")
(declare-function org-scribe-planner--calculate-velocity "org-scribe-planner-dashboards")
(declare-function org-scribe-plan-title "org-scribe-planner")
(declare-function org-scribe-plan-total-words "org-scribe-planner")
(declare-function org-scribe-plan-current-words "org-scribe-planner")
(declare-function org-scribe-plan-preexisting-words "org-scribe-planner")
(declare-function org-scribe-plan-daily-words "org-scribe-planner")
(declare-function org-scribe-plan-start-date "org-scribe-planner")
(declare-function org-scribe-plan-end-date "org-scribe-planner")
(declare-function org-scribe-plan-daily-word-counts "org-scribe-planner")
(defvar org-scribe-planner--current-plan)

(defun org-scribe-companion-progress--or-null (value)
  "VALUE, or :null when it is nil."
  (if (null value) :null value))

;;; Finding the plan without loading the planner

(defun org-scribe-companion-progress--plan-file (root)
  "Return the plan file of the project at ROOT, or nil, without loading anything.
With the planner already loaded its own search decides.  Otherwise only
the cheap places are looked at: the `# Plan:' line of the marker file and
plan.org.  A plan elsewhere is found once the planner is loaded (any
planner command, or `org-scribe-mode' in a project that opted in)."
  (if (featurep 'org-scribe-planner)
      (org-scribe-planner--find-plan-file root)
    (seq-find #'file-exists-p
              (delq nil (list (when-let* ((recorded (org-scribe--project-marker-get root "Plan")))
                                (expand-file-name recorded root))
                              (expand-file-name "plan.org" root))))))

(defun org-scribe-companion-progress--active-plan (root)
  "Return the plan of the project at ROOT, loading it when needed, or a reason.
Returns (plan . PLAN), or (STATE) with STATE `off' (the project declined
the planner) or `none' (undecided, or no plan file).  Requires the planner
only when the project opted in and a plan file exists."
  (let ((gate (org-scribe-planner-gate root)))
    (cond
     ((eq gate 'no) (list 'off))
     ((not (eq gate 'yes)) (list 'none))
     ((not (org-scribe-companion-progress--plan-file root)) (list 'none))
     (t
      (require 'org-scribe-planner)
      (require 'org-scribe-planner-dashboards)
      ;; the active plan is a session global: one of another project goes
      (org-scribe-planner--ensure-plan-for-current-project)
      (unless org-scribe-planner--current-plan
        (org-scribe-planner--auto-load-plan))
      (if org-scribe-planner--current-plan
          (cons 'plan org-scribe-planner--current-plan)
        (list 'none))))))

;;; The plan as JSON

(defun org-scribe-companion-progress--days (plan)
  "Return PLAN's days as a vector for the burndown.
Each day: `date', `goal' (that day's words, 0 on a rest day), `rest',
`planned' (where the manuscript should stand at its end, as planned: a
day before a recalculation from progress as it stood, see
`org-scribe-planner--planned-total') and `actual' (where it stood, the
last recorded total carried forward; null after today)."
  (let* ((today (org-scribe-planner--get-today-date))
         (counts (org-scribe-plan-daily-word-counts plan))
         (actual (or (org-scribe-plan-preexisting-words plan) 0))
         days)
    (dolist (day (org-scribe-planner--generate-day-schedule plan))
      (let* ((date (plist-get day :date))
             (entry (assoc date counts))
             (words (and entry (org-scribe-planner--get-entry-words entry))))
        (when (numberp words) (setq actual words))
        (push (list :date date
                    :goal (or (plist-get day :words) 0)
                    :rest (if (plist-get day :is-spare-day) t :false)
                    :planned (org-scribe-planner--planned-total plan day)
                    :actual (if (string< today date) :null actual))
              days)))
    (vconcat (nreverse days))))

(defun org-scribe-companion-progress--plan-json (plan)
  "Return PLAN as the `plan' object of /api/progress."
  (let* ((today (org-scribe-planner--get-today-date))
         (start (org-scribe-plan-start-date plan))
         (end (org-scribe-plan-end-date plan))
         (status (org-scribe-planner--status plan))
         (streak (org-scribe-planner--calculate-current-streak plan))
         (velocity (org-scribe-planner--calculate-velocity plan))
         (phase (cond ((and start (string< today start)) "before")
                      ((and end (string< end today)) "after")
                      (t "during"))))
    (list :state "active"
          :title (or (org-scribe-plan-title plan) "")
          :total (org-scribe-plan-total-words plan)
          :current (org-scribe-plan-current-words plan)
          :preexisting (or (org-scribe-plan-preexisting-words plan) 0)
          :daily (org-scribe-companion-progress--or-null (org-scribe-plan-daily-words plan))
          :start (org-scribe-companion-progress--or-null start)
          :end (org-scribe-companion-progress--or-null end)
          :today today
          :phase phase
          ;; nil outside the plan; 0 on a rest day
          :todayTarget (org-scribe-companion-progress--or-null
                        (org-scribe-planner--get-today-target plan))
          :todayActual (or (org-scribe-planner--get-today-actual plan) 0)
          :status (if status (symbol-name (plist-get status :status)) :null)
          :difference (if status (plist-get status :difference) 0)
          :daysAhead (if status (or (plist-get status :days) 0) 0)
          :streak (list :current (plist-get streak :current)
                        :longest (plist-get streak :longest))
          :velocity (list :average (plist-get velocity :average)
                          :recent (plist-get velocity :recent)
                          :trend (plist-get velocity :trend)
                          :eta (org-scribe-companion-progress--or-null
                                (plist-get velocity :eta))
                          :projected (org-scribe-companion-progress--or-null
                                      (plist-get velocity :projected-date)))
          :days (org-scribe-companion-progress--days plan)
          ;; the last recalculation from progress: the burndown draws its
          ;; step (null when there is none)
          :rebase (if-let* ((step (org-scribe-planner--rebase-step plan)))
                      (list :date (plist-get step :date)
                            :from (plist-get step :from)
                            :to (plist-get step :to))
                    :null))))

;;; The answer

(defun org-scribe-companion-progress-get (root)
  "Return the answer to GET /api/progress for the project at ROOT.
\(:ok t :revision REV :manuscript (:words W :objective O :wordTarget T)
:plan PLAN), where PLAN is `org-scribe-companion-progress--plan-json''s
object or {state: \"off\"|\"none\"}."
  (require 'org-scribe-health)
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (default-directory root))
    (condition-case err
        (let* ((manuscript (plist-get (org-scribe-project-structure) :manuscript-file))
               (totals (if manuscript
                           (org-scribe--health-word-totals manuscript)
                         (user-error "%s" (org-scribe-msg 'companion-error-no-manuscript))))
               (found (org-scribe-companion-progress--active-plan root))
               (plan (if (eq (car found) 'plan)
                         (org-scribe-companion-progress--plan-json (cdr found))
                       (list :state (symbol-name (car found))))))
          (list :ok t
                ;; the plan may have visited files: read the revision last
                :revision (org-scribe-companion--revision-string root)
                :manuscript (list :words (car totals)
                                  :objective (cdr totals)
                                  :wordTarget (org-scribe-companion-progress--or-null
                                               (org-scribe-project-word-target)))
                :plan plan))
      (user-error (org-scribe-companion-commands-error
                   "not-found" (error-message-string err)))
      ;; any other failure is still an answer: closing the socket left the
      ;; page with a bare "failed" and the cause only in *Messages*
      (error (org-scribe-companion-commands-error
              "internal" (error-message-string err))))))

(provide 'org-scribe-companion-progress)

;;; org-scribe-companion-progress.el ends here
