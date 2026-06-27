;;; org-scribe-planner-integration.el --- Compatibility shim (deprecated) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;; Author: Javier Castilla
;; Keywords: org, writing, planning

;;; Commentary:

;; This file is DEPRECATED.
;;
;; The integration features it once provided (auto-load plan, word-count
;; wiring, hydra sub-menu, mode-line status, plan-path persistence, and
;; project-root wiring) are now built directly into the planner module:
;;
;;   planning/org-scribe-planner.el
;;
;; You no longer need to load this file.  Remove the following snippet
;; from your init.el if you have it:
;;
;;   (with-eval-after-load 'org-scribe-planner
;;     (require 'org-scribe-planner-integration))
;;
;; To opt out of individual integration features, set these variables
;; BEFORE org-scribe-planner is loaded (e.g. in early-init.el or before
;; the first planner command runs):
;;
;;   (setq org-scribe-planner-auto-load-plan nil)
;;   (setq org-scribe-planner-auto-push-wordcount nil)
;;   (setq org-scribe-planner-offer-plan-on-create nil)
;;   (setq org-scribe-planner-show-mode-line nil)
;;
;; This shim will be removed in the next major release.

;;; Code:

(when (featurep 'org-scribe-planner)
  (message (concat "org-scribe: org-scribe-planner-integration is deprecated "
                   "and can be removed from your init.el.  "
                   "Integration features are now built into "
                   "planning/org-scribe-planner.el.")))

(provide 'org-scribe-planner-integration)

;;; org-scribe-planner-integration.el ends here
