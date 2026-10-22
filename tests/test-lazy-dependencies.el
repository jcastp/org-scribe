;;; test-lazy-dependencies.el --- Required packages work before they are loaded -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; org-ql and writeroom-mode are installed dependencies whose commands are
;; autoloaded: in a writer's session they are present but not yet loaded
;; when org-scribe first needs them.  Guards that asked whether they were
;; *loaded* (`featurep' / `fboundp') rather than loading them broke both
;; on first use — every search refused with "org-ql is required", and the
;; write/focus workspaces signalled `void-variable writeroom-width' after
;; switching theme and font.
;;
;; The test runner has long since loaded both, so an in-process test cannot
;; reproduce that state.  Each test here runs in a fresh `emacs -Q'
;; subprocess with org-scribe loaded and the dependency only installed, and
;; first asserts that the dependency really is unloaded — otherwise the test
;; would pass for the wrong reason.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst test-lazy-deps--repo
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the org-scribe checkout, for the subprocess's load path.")

(defun test-lazy-deps--run (body)
  "Run BODY, a form given as a string, in a fresh Emacs with org-scribe loaded.
BODY runs with BASE bound to a temporary directory and must print its
result as \"RESULT:<sexp>\".  Returns the sexp read back, or the whole
output as a string when no result was printed (so a failure shows why)."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (base (make-temp-file "test-lazy-deps-" t))
         (form (format "(let ((repo %S) (base %S))
  (setq org-id-locations-file (expand-file-name \"ids\" base)
        make-backup-files nil create-lockfiles nil)
  (load (expand-file-name \"test-load.el\" repo) nil t)
  %s)" test-lazy-deps--repo base body)))
    (unwind-protect
        (let ((output (with-temp-buffer
                        (call-process emacs nil t nil "-Q" "--batch" "--eval" form)
                        (buffer-string))))
          (if (string-match "RESULT:\\(.*\\)\\'" output)
              (car (read-from-string (match-string 1 output)))
            output))
      (delete-directory base t))))

(defconst test-lazy-deps--manuscript
  "(let ((root (expand-file-name \"novel\" base)))
    (make-directory (expand-file-name \"objects\" root) t)
    (with-temp-file (expand-file-name \".org-scribe-project\" root)
      (insert \"# Writing project: Lazy\\n# Type: novel\\n# Language: en\\n\"))
    (with-temp-file (expand-file-name \"novel.org\" root)
      (insert \"#+TODO: TODO | FINISHED\\n* Act I :ignore:\\n** TODO Chapter 1 :ignore:\\n\"
              \"*** TODO The letter :ignore:\\n:PROPERTIES:\\n:PoV: Mara Quell\\n:END:\\n\\n\"
              \"Mara found the letter.\\n\"))
    (expand-file-name \"novel.org\" root))"
  "Form creating a one-scene novel project under BASE; returns the manuscript.")

(ert-deftest test-lazy-deps-searches-work-before-org-ql-loads ()
  "PoV and TODO searches run in a session where org-ql is only installed."
  (skip-unless (locate-library "org-ql"))
  (let ((result (test-lazy-deps--run
                 (format "(let ((ms %s) out)
  (push (cons 'loaded-before (featurep 'org-ql)) out)
  (with-current-buffer (find-file-noselect ms)
    (push (cons 'pov (condition-case err
                         (progn (org-scribe-org-find-pov \"Mara Quell\") 'ok)
                       (error (error-message-string err))))
          out)
    (push (cons 'todos (condition-case err
                           (progn (org-scribe-search-todos-recursive) 'ok)
                         (error (error-message-string err))))
          out))
  (princ (format \"RESULT:%%S\" out)))" test-lazy-deps--manuscript))))
    (should (consp result))
    (should-not (alist-get 'loaded-before result))
    (should (eq 'ok (alist-get 'pov result)))
    (should (eq 'ok (alist-get 'todos result)))))

(ert-deftest test-lazy-deps-write-workspace-works-before-writeroom-loads ()
  "The write and focus workspaces open in a session where writeroom is only installed."
  (skip-unless (locate-library "writeroom-mode"))
  (let ((result (test-lazy-deps--run
                 (format "(let ((ms %s) out)
  (push (cons 'loaded-before (featurep 'writeroom-mode)) out)
  (with-current-buffer (find-file-noselect ms)
    (switch-to-buffer (current-buffer))
    ;; Focus narrows to the heading at point; start where a writer would.
    (re-search-forward \"The letter\")
    (dolist (layout '(write focus))
      (push (cons layout (condition-case err
                             (progn (org-scribe-workspace layout)
                                    (prog1 (org-scribe-workspace-current)
                                      (org-scribe-workspace 'normal)))
                           (error (error-message-string err))))
            out)))
  (princ (format \"RESULT:%%S\" out)))" test-lazy-deps--manuscript))))
    (should (consp result))
    (should-not (alist-get 'loaded-before result))
    (should (eq 'write (alist-get 'write result)))
    (should (eq 'focus (alist-get 'focus result)))))

(provide 'test-lazy-dependencies)
;;; test-lazy-dependencies.el ends here
