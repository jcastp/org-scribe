;;; org-scribe-planner-create.el --- New plans for org-scribe-planner -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Javier Castilla
;; URL: https://codeberg.org/jcastp/org-scribe

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The new-plan wizard, its presets, onboarding an existing project, and
;; spare-day configuration.
;;
;; Part of the writing planner; load it through `org-scribe-planner',
;; which requires every part in order.

;;; Code:

(require 'org)
(require 'org-element)
(require 'calendar)
(require 'cl-lib)
(require 'org-scribe-planner-engine)
(require 'org-scribe-planner-io)
(require 'org-scribe-planner-views)

(declare-function org-scribe--project-marker-set "core/org-scribe-core")
(declare-function org-scribe-project-root "core/org-scribe-core")
(declare-function org-scribe-project-type "core/org-scribe-core")

;;; New plans: presets and defaults

(defun org-scribe-planner--describe-preset (name preset)
  "Describe PRESET, called NAME, in one line for the preset question."
  (format "%s — %s (%d words %s%s)"
          name (plist-get preset :description) (plist-get preset :total)
          (if (plist-get preset :days)
              (format "in %d days" (plist-get preset :days))
            (format "at %d a day" (plist-get preset :daily)))
          (let ((pattern (plist-get preset :pattern)))
            (if (memq pattern '(nil flat)) "" (format ", %s" pattern)))))

(defun org-scribe-planner--read-preset ()
  "Ask whether to start a new plan from a preset; return (NAME . PLIST) or nil.
Offers `org-scribe-planner-presets' after \"From scratch\".  Inside an
org-scribe project, the first preset made for its type (novel or short
story) is the default answer."
  (when org-scribe-planner-presets
    (let* ((type (ignore-errors (org-scribe-project-type)))
           (labels (mapcar (lambda (preset)
                             (cons (org-scribe-planner--describe-preset (car preset) (cdr preset))
                                   preset))
                           org-scribe-planner-presets))
           ;; The first type a preset lists is its main one: prefer a preset
           ;; made mainly for this type, then any that also suits it
           (suggested (or (cl-find-if (lambda (label)
                                        (eq type (car (plist-get (cddr label) :project-types))))
                                      labels)
                          (cl-find-if (lambda (label)
                                        (memq type (plist-get (cddr label) :project-types)))
                                      labels)))
           (answer (completing-read "Start from: " (cons "From scratch" (mapcar #'car labels))
                                    nil t nil nil
                                    (if suggested (car suggested) "From scratch"))))
      (cdr (assoc answer labels)))))

(defun org-scribe-planner--pattern-config-for (pattern intensity)
  "Return the settings of PATTERN at INTENSITY; random gets a seed of its own."
  (let ((config (copy-sequence
                 (cdr (assq intensity (cdr (assq pattern org-scribe-planner--pattern-intensities)))))))
    (if (eq pattern 'random)
        (append config (list :seed (1+ (random 999999))))
      config)))

(defun org-scribe-planner--offer-pattern (plan pattern intensity why)
  "Ask which pattern PLAN uses, PATTERN being the default; apply it at INTENSITY.
WHY says where the suggestion comes from.  Asks nothing for `flat'.
The question is a choice with PATTERN as the default, not a yes-or-no:
a preset names its pattern, and a stray \"n\" dropped it.  Returns
non-nil when the plan gets a pattern other than `flat'."
  (when (and pattern (not (eq pattern 'flat)))
    (let* ((default (cdr (assq pattern org-scribe-planner--patterns)))
           (answer (completing-read (format "Writing pattern (%s: %s): " why pattern)
                                    (mapcar #'cdr org-scribe-planner--patterns)
                                    nil t nil nil default))
           (chosen (or (car (rassoc answer org-scribe-planner--patterns)) pattern)))
      (setf (org-scribe-plan-pattern plan) chosen)
      (setf (org-scribe-plan-pattern-config plan)
            (org-scribe-planner--pattern-config-for chosen intensity))
      (not (eq chosen 'flat)))))

(defun org-scribe-planner--offer-weekends (plan mode why)
  "Ask whether PLAN's weekends follow MODE; apply it on yes.
WHY says where the suggestion comes from.  A goal of their own is asked
for, offering `org-scribe-planner-default-weekend-daily' or 1.5 times the
daily target.  Does nothing for `same'."
  (when (and mode (not (eq mode 'same))
             (y-or-n-p (format "%s weekends: %s? " why
                               (if (eq mode 'rest) "no writing" "a goal of their own"))))
    (setf (org-scribe-plan-weekend-mode plan) mode)
    (setf (org-scribe-plan-weekend-days plan) (copy-sequence org-scribe-planner-default-weekend-days))
    (when (eq mode 'different)
      (setf (org-scribe-plan-weekend-daily plan)
            (org-scribe-planner--read-non-negative-number
             "Weekend goal (words/day): "
             (or org-scribe-planner-default-weekend-daily
                 (round (* 1.5 (org-scribe-plan-daily-words plan)))))))
    t))

(defun org-scribe-planner--wizard-rebalance (plan calculated daily end-date)
  "Recalculate the value the new-plan wizard worked out, keeping the other two.
CALCULATED is what `org-scribe-planner--calculate-missing-variable'
returned: `days' (keep the total and DAILY, the entered daily target),
`total-words' (keep the days, ending on END-DATE, and DAILY) or anything
else (keep the total and END-DATE)."
  (pcase calculated
    ('days (org-scribe-planner--calculate-time plan daily))
    ('total-words
     (org-scribe-planner--keep-end-date plan end-date)
     (org-scribe-planner--set-total-keeping-daily plan daily))
    (_ (org-scribe-planner--keep-end-date plan end-date))))

;;; Interactive Commands

;;;###autoload (autoload 'org-scribe-planner-new-plan "org-scribe-planner" nil t)
(defun org-scribe-planner-new-plan (&optional title save-path)
  "Create a new writing plan interactively.
TITLE and SAVE-PATH, when supplied, are used directly instead of
prompting for them — the interactive spec supplies both automatically
when invoked (via `M-x' or a keybinding) inside an org-scribe project
that has not yet been asked, so no prompt appears in that case either."
  (interactive
   (let* ((root (ignore-errors (org-scribe-project-root)))
          (in-project (and root
                           (file-exists-p (expand-file-name ".org-scribe-project" root)))))
     (list (and in-project (org-scribe-planner--project-title root))
           (and in-project (expand-file-name "plan.org" root)))))
  (let ((plan (make-org-scribe-plan :words-basis 'manuscript))
        (project-target (org-scribe-planner--project-word-target))
        (preset nil))

    ;; Get plan title
    (setf (org-scribe-plan-title plan)
          (or title (read-string "Project title: ")))

    ;; A preset, or from scratch
    (setq preset (org-scribe-planner--read-preset))
    (if preset
        (let ((settings (cdr preset)))
          (setf (org-scribe-plan-total-words plan)
                (org-scribe-planner--read-positive-number
                 "Total words to write: " (or project-target (plist-get settings :total))))
          (if (plist-get settings :days)
              (let ((days-result (org-scribe-planner--read-days
                                  "Days available: " (plist-get settings :days))))
                (if (listp days-result)
                    (setf (org-scribe-plan-days plan) (plist-get days-result :days)
                          (org-scribe-plan-start-date plan) (plist-get days-result :start-date)
                          (org-scribe-plan-end-date plan) (plist-get days-result :end-date))
                  (setf (org-scribe-plan-days plan) days-result)))
            (setf (org-scribe-plan-daily-words plan)
                  (org-scribe-planner--read-positive-number
                   "Words per day you can write: " (plist-get settings :daily)))))
    ;; Ask which variables to set and read them directly into the plan
    (let ((var-choice (org-scribe-planner--choose
                       "What do you know? "
                       '("Total words + Days → Calculate daily words"
                         "Total words + Daily words → Calculate days needed"
                         "Daily words + Days → Calculate total words")
                       "Total words + Days → Calculate daily words")))

      (cond
       ;; Total words + Days → Calculate daily words
       ((string-match-p "Calculate daily words" var-choice)
        (setf (org-scribe-plan-total-words plan)
              (org-scribe-planner--read-positive-number "Total words to write: " project-target))
        (let ((days-result (org-scribe-planner--read-days "Days available: ")))
          (if (listp days-result)
              ;; User entered dates - extract days and set start/end dates
              (progn
                (setf (org-scribe-plan-days plan) (plist-get days-result :days))
                (setf (org-scribe-plan-start-date plan) (plist-get days-result :start-date))
                (setf (org-scribe-plan-end-date plan) (plist-get days-result :end-date)))
            ;; User entered a number - just set days
            (setf (org-scribe-plan-days plan) days-result))))

       ;; Total words + Daily words → Calculate days needed
       ((string-match-p "Calculate days needed" var-choice)
        (setf (org-scribe-plan-total-words plan)
              (org-scribe-planner--read-positive-number "Total words to write: " project-target))
        (setf (org-scribe-plan-daily-words plan)
              (org-scribe-planner--read-positive-number "Words per day you can write: ")))

       ;; Daily words + Days → Calculate total words
       ((string-match-p "Calculate total words" var-choice)
        (setf (org-scribe-plan-daily-words plan)
              (org-scribe-planner--read-positive-number "Words per day you can write: "))
        (let ((days-result (org-scribe-planner--read-days "Days available: ")))
          (if (listp days-result)
              ;; User entered dates - extract days and set start/end dates
              (progn
                (setf (org-scribe-plan-days plan) (plist-get days-result :days))
                (setf (org-scribe-plan-start-date plan) (plist-get days-result :start-date))
                (setf (org-scribe-plan-end-date plan) (plist-get days-result :end-date)))
            ;; User entered a number - just set days
            (setf (org-scribe-plan-days plan) days-result))))

       ;; Fallback if nothing matched (shouldn't happen, but just in case)
       (t
        (error "Invalid choice: %s" var-choice)))))

    ;; Words already written: the plan's pre-existing words.  Inside a
    ;; project they are the manuscript's count — what every sync reads —
    ;; plus any words written outside it; elsewhere, a number.
    (let* ((counted (and org-scribe-planner-wordcount-function
                         (ignore-errors (funcall org-scribe-planner-wordcount-function))))
           (base (org-scribe-planner--outside-base save-path))
           (preexisting
            (if (numberp counted)
                (progn
                  (message "The manuscript has %d words" counted)
                  (org-scribe-planner--written-total plan counted base))
              (org-scribe-planner--read-non-negative-number
               "Words already written (before this plan): " 0)))
           (total (org-scribe-plan-total-words plan)))
      (when (and (numberp total) (>= preexisting total))
        (user-error "%d words are already written, which reaches the %d-word goal"
                    preexisting total))
      (setf (org-scribe-plan-preexisting-words plan) preexisting)
      (setf (org-scribe-plan-current-words plan) preexisting))

    ;; Ask about start date only if not already set (from date entry method)
    (unless (org-scribe-plan-start-date plan)
      (setf (org-scribe-plan-start-date plan)
            (org-scribe-planner--read-date "Start date (YYYY-MM-DD)"
                                          (org-scribe-planner--get-today-date))))

    ;; Calculate missing variable and dates FIRST (before asking about spare days)
    (condition-case err
        (progn
          ;; Initial calculation without spare days
          ;; Remember which variable we calculated so we can recalculate it if spare days are added
          (let* ((typed-daily (org-scribe-plan-daily-words plan))
                 (calculated-var (org-scribe-planner--calculate-missing-variable plan))
                 (initial-value (when calculated-var
                                  (cl-getf (list 'total-words (org-scribe-plan-total-words plan)
                                                'daily-words (org-scribe-plan-daily-words plan)
                                                'days (org-scribe-plan-days plan))
                                          calculated-var))))
            (when calculated-var
              (message "Calculated %s: %s" calculated-var initial-value))

            ;; Calculate dates (now we have start-date and all variables)
            (org-scribe-planner--calculate-dates plan)

            ;; The pattern and weekends: the preset's suggestions, or the
            ;; defaults for plans made from scratch
            ;; Rebalancing keeps the daily target as typed, when one was
            (let ((entered-daily (or typed-daily (org-scribe-plan-daily-words plan)))
                  (end-date (org-scribe-plan-end-date plan))
                  (settings (cdr preset)))
              (org-scribe-planner--offer-pattern
               plan
               (if preset (plist-get settings :pattern) org-scribe-planner-default-pattern)
               (if preset 'moderate org-scribe-planner-default-pattern-intensity)
               (if preset "suggested" "default"))
              (when (org-scribe-planner--offer-weekends
                     plan
                     (if preset (plist-get settings :weekend-mode)
                       org-scribe-planner-default-weekend-mode)
                     (if preset "Suggested" "Default"))
                (org-scribe-planner--wizard-rebalance plan calculated-var entered-daily end-date))
              (org-scribe-planner--sync-daily plan)

              ;; Spare days: the same rebalance keeps the two values given
              (when (y-or-n-p "Do you want to set spare/break days? ")
                (setq end-date (org-scribe-plan-end-date plan))
                (org-scribe-planner--configure-spare-days plan)
                (when (org-scribe-plan-spare-days plan)
                  (message "Added %d spare day(s), recalculating..."
                           (length (org-scribe-plan-spare-days plan)))
                  (org-scribe-planner--wizard-rebalance plan calculated-var entered-daily end-date)
                  (message "Recalculated: %d writing days ending %s, %d words/day, %d words"
                           (org-scribe-plan-writing-days plan) (org-scribe-plan-end-date plan)
                           (org-scribe-plan-daily-words plan) (org-scribe-plan-total-words plan))))))

          ;; A plan started in the past: its past days keep these goals
          (org-scribe-planner--lock-past-goals plan)

          ;; Ask user for save location
          (let* ((default-filename (concat (downcase (replace-regexp-in-string
                                                     "[^[:alnum:]]" "-"
                                                     (org-scribe-plan-title plan)))
                                          ".org"))
                 (default-filepath (expand-file-name default-filename org-scribe-planner-directory))
                 (save-location (or save-path
                                    (read-file-name "Save plan to: "
                                                    org-scribe-planner-directory
                                                    default-filepath
                                                    nil
                                                    default-filename)))
                 (save-dir (file-name-directory save-location)))

            ;; Outside files were stored relative to where the plan was
            ;; expected; re-anchor them to where it is saved
            (org-scribe-planner--rebase-outside-paths
             plan (org-scribe-planner--outside-base save-path) save-dir)

            ;; Ensure the directory exists
            (if (file-exists-p save-dir)
                ;; Directory exists, save the plan
                (org-scribe-planner--save-plan plan save-location)
              ;; Directory doesn't exist, ask to create it
              (if (y-or-n-p (format "Directory %s does not exist. Create it? " save-dir))
                  (progn
                    (make-directory save-dir t)
                    (org-scribe-planner--save-plan plan save-location))
                ;; User declined to create directory
                (error "Cannot save plan: directory does not exist")))

            ;; Set as current active plan
            (setq org-scribe-planner--current-plan plan)
            (setq org-scribe-planner--current-plan-file save-location)

            ;; Run hook
            (run-hook-with-args 'org-scribe-planner-after-plan-load-hook plan save-location)

            ;; Display the plan
            (org-scribe-planner-show-calendar plan save-location)))
      (error
       (message "Error creating plan: %s" (error-message-string err))))))

;;;###autoload (autoload 'org-scribe-planner-onboard-existing-project "org-scribe-planner" nil t)
(defun org-scribe-planner-onboard-existing-project ()
  "Create a writing plan for a novel already in progress.
Reads the current manuscript word count, asks for your target and
timeline, then builds a plan that starts today (or the date you give)
with the existing words recorded as its pre-existing words
\(PREEXISTING_WORDS): they count toward the goal and completion, but
not as any one day's writing, so auto-tracking records only net-new
writing going forward.

Skips the normal new-plan wizard; the daily target is calculated from
the *remaining* words over the *remaining* working days, not the total."
  (interactive)
  (let* ((today (org-scribe-planner--get-today-date))
         ;; 1. The manuscript's word count: counted when it can be (an
         ;; empty manuscript counts 0), asked otherwise
         (manuscript-words
          (let ((auto (and org-scribe-planner-wordcount-function
                           (ignore-errors
                             (funcall org-scribe-planner-wordcount-function)))))
            (if (numberp auto)
                (progn (message "The manuscript has %d words" auto) auto)
              (org-scribe-planner--read-non-negative-number
               "Manuscript word count (0 if it's empty): "))))
         (plan (make-org-scribe-plan :words-basis 'manuscript))
         ;; Outside files are stored relative to the plan's directory: the
         ;; project root, where the plan is saved by default
         (base (file-name-as-directory
                (expand-file-name (or (and (featurep 'org-scribe)
                                           (ignore-errors (org-scribe-project-root)))
                                      default-directory))))
         (current-words (org-scribe-planner--written-total plan manuscript-words base)))

    ;; 2. Core parameters
    (setf (org-scribe-plan-title plan)
          (read-string "Project title: "))
    (setf (org-scribe-plan-total-words plan)
          (org-scribe-planner--read-positive-number
           (format "Total word target (you have %d so far): " current-words)
           (org-scribe-planner--project-word-target)))

    (when (<= (org-scribe-plan-total-words plan) current-words)
      (error "Target (%d) must be greater than words already written (%d)"
             (org-scribe-plan-total-words plan) current-words))

    ;; The plan starts today by default: the words already written are
    ;; its pre-existing words, not part of its schedule
    (setf (org-scribe-plan-start-date plan)
          (org-scribe-planner--read-date "Plan start date (YYYY-MM-DD)" today))
    (setf (org-scribe-plan-preexisting-words plan) current-words)

    ;; 3. Pace: end date or daily words
    (let ((how (completing-read
                "Set the pace by: "
                '("End date → calculate daily words"
                  "Daily words → calculate end date")
                nil t nil nil "End date → calculate daily words")))
      ;; Compare the start of the choice, case-sensitively: the other
      ;; choice also contains "end date", which `string-match-p' matches
      ;; under the default `case-fold-search'
      (if (string-prefix-p "End date" how)
          (setf (org-scribe-plan-end-date plan)
                (org-scribe-planner--read-date
                 "Target end date (YYYY-MM-DD)"
                 (org-scribe-planner--add-days today 90)))
        (setf (org-scribe-plan-daily-words plan)
              (org-scribe-planner--read-positive-number
               "Words per day (for the remaining period): "))))

    ;; 4. Optional spare days
    (when (y-or-n-p "Set spare/break days (weekends, holidays)? ")
      (org-scribe-planner--configure-spare-days plan))

    ;; 5. Calculate the plan with the schedule engine.  The existing
    ;;    words are its pre-existing words, so the daily target covers only
    ;;    what is left.  If the plan starts before today, the days before
    ;;    today are locked with no goal: the words left are written from
    ;;    today on.
    (condition-case err
        (let* ((remaining-words (- (org-scribe-plan-total-words plan) current-words))
               (start-date (org-scribe-plan-start-date plan)))
          (setf (org-scribe-plan-current-words plan) current-words)
          (when (string< start-date today)
            (setf (org-scribe-plan-locked-goals plan)
                  (mapcar (lambda (date) (cons date 0))
                          (org-scribe-planner--generate-date-range
                           start-date (org-scribe-planner--add-days today -1)))))
          (if (org-scribe-plan-end-date plan)
              ;; End date given: the daily target follows
              (org-scribe-planner--set-writing-days
               plan (org-scribe-planner--writing-days-through
                     plan (org-scribe-plan-end-date plan)))
            ;; Daily words given: the plan gets the days it needs
            (org-scribe-planner--calculate-time plan (org-scribe-plan-daily-words plan)))

          (let ((future-working-days (- (org-scribe-planner--writing-days plan)
                                        (org-scribe-planner--min-writing-days plan))))
            (when (<= future-working-days 0)
              (error "No future working days in the plan — adjust your end date"))

          ;; 6. The existing words are the plan's pre-existing words (set
          ;; above), not a ledger entry: the first sync's delta is measured
          ;; against them, so only net-new writing counts as a day's words.

          ;; 7. Save
          (let* ((default-filename
                  (concat (downcase (replace-regexp-in-string
                                     "[^[:alnum:]]" "-"
                                     (org-scribe-plan-title plan)))
                          ".org"))
                 (default-dir (or (and (featurep 'org-scribe)
                                       (ignore-errors (org-scribe-project-root)))
                                  org-scribe-planner-directory))
                 (default-filepath (expand-file-name default-filename default-dir))
                 (save-location (read-file-name "Save plan to: "
                                               (file-name-directory default-filepath)
                                               default-filepath nil
                                               default-filename)))
            (make-directory (file-name-directory save-location) t)
            (org-scribe-planner--rebase-outside-paths
             plan base (file-name-directory (expand-file-name save-location)))
            (org-scribe-planner--save-plan plan save-location)
            (setq org-scribe-planner--current-plan plan)
            (setq org-scribe-planner--current-plan-file save-location)
            ;; Open the project's gate, as `org-scribe-plan' and the offer
            ;; on project creation do.  Left undecided, the planner never
            ;; loaded in a fresh session, and word counts silently missed
            ;; the ledger.
            (when (and (featurep 'org-scribe)
                       (not (eq (org-scribe-project-type) 'unknown)))
              (org-scribe--project-marker-set (org-scribe-project-root) "Planner" "yes"))
            (run-hook-with-args 'org-scribe-planner-after-plan-load-hook
                                plan save-location)
            (message "Plan ready — %d written, %d to go at %d words/day over %d days."
                     current-words remaining-words
                     (org-scribe-plan-daily-words plan)
                     future-working-days)
            (org-scribe-planner-show-calendar plan save-location))))
      (error
       (message "Error creating plan: %s" (error-message-string err))))))

(defun org-scribe-planner--configure-spare-days (plan)
  "Interactively configure spare days for PLAN."
  (let ((spare-days (or (org-scribe-plan-spare-days plan) nil))
        (continue t))

    (while continue
      (let ((method (org-scribe-planner--choose
                     "Configure spare days: "
                     '("Add: Specific date"
                       "Add: Date range"
                       "Add: All weekends"
                       "Add: All Saturdays"
                       "Add: All Sundays"
                       "Remove: Specific date"
                       "Remove: All spare days"
                       "List current spare days"
                       "Done")
                     "Done")))
        (cond
         ((equal method "Add: Specific date")
          (let ((date (org-scribe-planner--read-date "Enter date (YYYY-MM-DD)" nil t)))
            (when date
              (push date spare-days)
              (let ((note (read-string (format "Note for %s (leave empty for default): " date))))
                (when (and note (not (string-empty-p (string-trim note))))
                  ;; Add entry to daily-word-counts with note
                  (org-scribe-planner--add-spare-day-note plan date note)))
              (message "Added %s as spare day" date))))

         ((equal method "Add: Date range")
          (let* ((start (org-scribe-planner--read-date "Start date (YYYY-MM-DD)" nil t))
                 (end (when start
                        (org-scribe-planner--read-date "End date (YYYY-MM-DD)" nil t))))
            (when (and start end)
              (let ((date-range (org-scribe-planner--generate-date-range start end)))
                (setq spare-days (append spare-days date-range))
                ;; Ask if user wants to add a note for all dates in range
                (let ((note (read-string (format "Note for all dates in range (leave empty for default): "))))
                  (when (and note (not (string-empty-p (string-trim note))))
                    (dolist (date date-range)
                      (org-scribe-planner--add-spare-day-note plan date note))))
                (message "Added dates from %s to %s" start end)))))

         ((equal method "Add: All weekends")
          (let ((weekend-dates (org-scribe-planner--get-weekends plan)))
            (setq spare-days (append spare-days weekend-dates))
            (let ((note (read-string "Note for all weekends (leave empty for default): ")))
              (when (and note (not (string-empty-p (string-trim note))))
                (dolist (date weekend-dates)
                  (org-scribe-planner--add-spare-day-note plan date note))))
            (message "Added all weekends")))

         ((equal method "Add: All Saturdays")
          (let ((saturday-dates (org-scribe-planner--get-day-of-week plan 6)))
            (setq spare-days (append spare-days saturday-dates))
            (let ((note (read-string "Note for all Saturdays (leave empty for default): ")))
              (when (and note (not (string-empty-p (string-trim note))))
                (dolist (date saturday-dates)
                  (org-scribe-planner--add-spare-day-note plan date note))))
            (message "Added all Saturdays")))

         ((equal method "Add: All Sundays")
          (let ((sunday-dates (org-scribe-planner--get-day-of-week plan 0)))
            (setq spare-days (append spare-days sunday-dates))
            (let ((note (read-string "Note for all Sundays (leave empty for default): ")))
              (when (and note (not (string-empty-p (string-trim note))))
                (dolist (date sunday-dates)
                  (org-scribe-planner--add-spare-day-note plan date note))))
            (message "Added all Sundays")))

         ((equal method "Remove: Specific date")
          (if (null spare-days)
              (message "No spare days to remove")
            (let ((date (org-scribe-planner--choose "Select date to remove: "
                                                    (sort (copy-sequence spare-days) #'string<))))
              (when date
                (setq spare-days (delete date spare-days))
                (org-scribe-planner--remove-spare-day-note plan date)
                (message "Removed %s from spare days" date)))))

         ((equal method "Remove: All spare days")
          (if (null spare-days)
              (message "No spare days to remove")
            (when (y-or-n-p (format "Remove all %d spare days? " (length spare-days)))
              (dolist (date spare-days)
                (org-scribe-planner--remove-spare-day-note plan date))
              (setq spare-days nil)
              (message "Removed all spare days"))))

         ((equal method "List current spare days")
          (if (null spare-days)
              (message "No spare days configured")
            (let ((sorted-days (sort (copy-sequence spare-days) #'string<)))
              (message "Current spare days (%d): %s"
                      (length sorted-days)
                      (mapconcat #'identity sorted-days ", ")))))

         ((equal method "Done")
          (setq continue nil)))))

    (setf (org-scribe-plan-spare-days plan) (delete-dups spare-days))))

(provide 'org-scribe-planner-create)

;;; org-scribe-planner-create.el ends here
