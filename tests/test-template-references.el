;;; test-template-references.el --- Templates must cite real keys/commands -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; The shipped templates are the writer's first and most-read
;; documentation, and they used to be wrong about the UI: `design.org' told
;; the writer the edit index was `F8 F8 e' (it is `F8 F8 6'), `plot.org'
;; told them to add narrative lines with `F8 F8 p' (that sets PoV; capture
;; is `F8 F8 k g'), and `novel.org' still referenced the removed
;; `org-scribe/plot-thread-report' slash name.  A new writer who follows
;; these gets the wrong command or an error and concludes the package is
;; broken.
;;
;; This file greps every shipped `.template' for `F8 F8 <key>' sequences
;; and `org-scribe[-/]...' command references, and checks each one against
;; the real hydra heads (recursing into submenus such as
;; `hydra-org-scribe-capture/body') and `fboundp'.  A stale reference fails
;; loudly here instead of silently in front of a writer.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar org-scribe-refs--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "Repository root, used to locate the shipped template files.")

(require 'org-scribe-hydra)

(defconst org-scribe-refs--template-sets
  '("novel-en" "novel-es" "short-story-en" "short-story-es")
  "Template directories to scan, relative to `org-scribe-templates/'.")

(defun org-scribe-refs--templates ()
  "Return the absolute paths of every shipped `.template' file."
  (cl-mapcan
   (lambda (set)
     (let ((dir (expand-file-name (concat "org-scribe-templates/" set)
                                  org-scribe-refs--root)))
       (when (file-directory-p dir)
         (directory-files-recursively dir "\\.template\\'"))))
   org-scribe-refs--template-sets))

(defun org-scribe-refs--submenu-heads (head)
  "Return the heads of the submenu HEAD's binding opens, or nil.
HEAD is a hydra head entry `(KEY CMD HINT ...)'.  CMD opens a submenu when
it is a call to a `.../body' hydra entry point; that entry point's
`.../heads' variable holds its own heads."
  (let ((cmd (cadr head)))
    (when (and (symbolp cmd)
               (string-suffix-p "/body" (symbol-name cmd)))
      (let ((heads-var (intern (replace-regexp-in-string
                                "/body\\'" "/heads" (symbol-name cmd)))))
        (when (boundp heads-var)
          (symbol-value heads-var))))))

(defun org-scribe-refs--valid-key-sequence-p (keys)
  "Return non-nil if KEYS (a list of single-character strings) is a real
path through the main hydra, `hydra-org-scribe/heads', descending into
submenus as needed."
  (let ((heads hydra-org-scribe/heads))
    (catch 'invalid
      (dolist (key keys)
        (let ((head (cl-find key heads :key #'car :test #'string=)))
          (unless head (throw 'invalid nil))
          (setq heads (org-scribe-refs--submenu-heads head))))
      t)))

(defun org-scribe-refs--find-key-sequences (text)
  "Return the list of `F8 F8 ...' key sequences (each a list of keys) in TEXT."
  (let (matches (start 0))
    (while (string-match
            "F8 F8 \\([A-Za-z0-9]\\)\\_>\\(?:[ \t]+\\([A-Za-z0-9]\\)\\_>\\)?"
            text start)
      (push (delq nil (list (match-string 1 text) (match-string 2 text)))
            matches)
      (setq start (match-end 0)))
    (nreverse matches)))

(defun org-scribe-refs--find-command-names (text)
  "Return the list of `org-scribe...' command names referenced in TEXT.
Matches both live names (`org-scribe-foo') and the removed slash
convention (`org-scribe/foo'), so a slash reference is reported as an
offender rather than silently skipped."
  (let (matches (start 0))
    (while (string-match "org-scribe[-/][a-zA-Z0-9-]+" text start)
      (push (match-string 0 text) matches)
      (setq start (match-end 0)))
    (nreverse matches)))

(ert-deftest test-template-references-key-sequences-are-real ()
  "Every `F8 F8 ...' sequence in a shipped template resolves against the
live hydra, including two-key sequences through a submenu such as
`F8 F8 k g'."
  (let (offenders)
    (dolist (file (org-scribe-refs--templates))
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (keys (org-scribe-refs--find-key-sequences (buffer-string)))
          (unless (org-scribe-refs--valid-key-sequence-p keys)
            (push (cons (file-relative-name file org-scribe-refs--root)
                       (mapconcat #'identity keys " "))
                  offenders)))))
    (should-not offenders)))

(ert-deftest test-template-references-commands-exist ()
  "Every `org-scribe...' command referenced in a shipped template is a real,
bound command, and none use the removed `org-scribe/' slash convention."
  (let (offenders)
    (dolist (file (org-scribe-refs--templates))
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (name (org-scribe-refs--find-command-names (buffer-string)))
          (when (or (string-match-p "/" name)
                    (not (fboundp (intern name))))
            (push (cons (file-relative-name file org-scribe-refs--root) name)
                  offenders)))))
    (should-not offenders)))

(ert-deftest test-template-references-no-unshipped-tracker ()
  "No shipped template references `org-tracktable'.
The novel and short-story templates used to ship a `#+NAME: tracktable'
table whose `#+TBLFM:' calls `org-tracktable-stamp' and
`org-tracktable-current-count'.  That package is not a dependency, not in
the README's optional list and not checked by `org-scribe-setup-check', so
`C-c C-c' on the shipped table errored — and it was a second daily word
tracker beside the planner's ledger.  Daily totals belong to the plan."
  (let (offenders)
    (dolist (file (org-scribe-refs--templates))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward "tracktable" nil t)
          (push (file-relative-name file org-scribe-refs--root) offenders))))
    (should-not offenders)))

(provide 'test-template-references)
;;; test-template-references.el ends here
