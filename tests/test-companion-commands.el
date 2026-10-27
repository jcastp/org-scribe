;;; test-companion-commands.el --- Tests for the companion's commands -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; Tests for `companion/org-scribe-companion-commands.el' (`goto', `save'),
;; follow mode (`focus' events) and the web files' string keys (Phase 3).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory (or load-file-name buffer-file-name))))))
  (dolist (dir '("core" "lang" "linking" "search" "capture" "templates" "companion"))
    (add-to-list 'load-path (expand-file-name dir parent-dir))))

(require 'org-scribe-config)
(require 'org-scribe-core)
(require 'org-scribe-character-links)
(require 'org-scribe-location-links)
(require 'org-scribe-plot-links)
(require 'org-scribe-plot-point-links)
(require 'org-scribe-companion)

(defconst test-companion-commands--repo
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root of the org-scribe checkout.")

(defconst test-companion-commands--scene-1 "a6aaa600-35b3-4193-9510-a4bb2ef0000f"
  "ID of \"Scene 1 - The Alarm\" in the demo novel.")
(defconst test-companion-commands--scene-2 "1ba80e22-da5c-4457-ae43-3d577c5d3529"
  "ID of \"Scene 2 - Coffee Shop Encounter\".")
(defconst test-companion-commands--alex "char-alex-rivera-001"
  "ID of a character in the demo design notebook.")

(defmacro test-companion-commands--with-project (root-var &rest body)
  "Run BODY with a copy of the demo novel at ROOT-VAR (not serving)."
  (declare (indent 1))
  `(let* ((,root-var (file-name-as-directory (make-temp-file "test-companion-cmd-" t)))
          (default-directory ,root-var)
          (org-id-locations-file (expand-file-name "ids" ,root-var))
          (make-backup-files nil)
          (create-lockfiles nil)
          (org-scribe-companion-raise-frame nil))
     (unwind-protect
         (save-window-excursion
           (copy-directory (expand-file-name "examples/demo-novel" test-companion-commands--repo)
                           ,root-var t t t)
           (org-scribe-project-type-cache-clear)
           (org-scribe-companion--clear-cache)
           ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (string-prefix-p ,root-var file)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (org-scribe-project-type-cache-clear)
       (org-scribe-companion--clear-cache)
       (delete-directory ,root-var t))))

(defun test-companion-commands--run (root op &rest args)
  (org-scribe-companion-commands-run op args root))

;;; goto

(ert-deftest test-companion-commands-goto-shows-the-heading ()
  (test-companion-commands--with-project root
    (let ((answer (test-companion-commands--run
                   root "goto" :id test-companion-commands--scene-2)))
      (should (eq t (plist-get answer :ok)))
      (should (stringp (plist-get answer :revision)))
      (should (equal "novel.org" (file-name-nondirectory (buffer-file-name))))
      (should (org-at-heading-p))
      (should (equal "Scene 2 - Coffee Shop Encounter" (org-get-heading t t t t)))
      (should (eq (current-buffer) (window-buffer (selected-window)))))))

(ert-deftest test-companion-commands-goto-widens-a-narrowed-buffer ()
  (test-companion-commands--with-project root
    (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
      (goto-char (point-min))
      (re-search-forward "^\\*\\*\\* FINISHED Scene 1 - The Alarm")
      (org-narrow-to-subtree))
    (let ((answer (test-companion-commands--run
                   root "goto" :id test-companion-commands--scene-2)))
      (should (eq t (plist-get answer :ok)))
      (should (equal "Scene 2 - Coffee Shop Encounter" (org-get-heading t t t t))))))

(ert-deftest test-companion-commands-goto-reveals-a-folded-heading ()
  (test-companion-commands--with-project root
    (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
      (org-overview))
    (test-companion-commands--run root "goto" :id test-companion-commands--scene-1)
    (should-not (invisible-p (point)))))

(ert-deftest test-companion-commands-goto-finds-design-entities ()
  (test-companion-commands--with-project root
    (let ((answer (test-companion-commands--run
                   root "goto" :id test-companion-commands--alex)))
      (should (eq t (plist-get answer :ok)))
      (should (equal "design.org" (file-name-nondirectory (buffer-file-name))))
      (should (equal "Alex Rivera" (org-get-heading t t t t))))))

(ert-deftest test-companion-commands-goto-unknown-id-is-not-found ()
  (test-companion-commands--with-project root
    (let ((answer (test-companion-commands--run root "goto" :id "no-such-id")))
      (should (eq :false (plist-get answer :ok)))
      (should (equal "not-found" (plist-get (plist-get answer :error) :code)))
      (should (string-match-p "no-such-id" (plist-get (plist-get answer :error) :message))))))

(ert-deftest test-companion-commands-goto-ignores-ids-outside-the-project ()
  "An ID that exists in another file is not found."
  (test-companion-commands--with-project root
    (let ((other (expand-file-name "other.org" root)))
      (with-temp-file other (insert "* Elsewhere\n:PROPERTIES:\n:ID: elsewhere-id\n:END:\n"))
      (find-file-noselect other)
      (let ((answer (test-companion-commands--run root "goto" :id "elsewhere-id")))
        (should (equal "not-found" (plist-get (plist-get answer :error) :code)))))))

(ert-deftest test-companion-commands-goto-needs-an-id ()
  (test-companion-commands--with-project root
    (dolist (args '(nil (:id "") (:id 5) (:other "x")))
      (let ((answer (org-scribe-companion-commands-run "goto" args root)))
        (should (equal "invalid" (plist-get (plist-get answer :error) :code)))))))

(ert-deftest test-companion-commands-goto-does-not-modify-the-buffer ()
  (test-companion-commands--with-project root
    (test-companion-commands--run root "goto" :id test-companion-commands--scene-1)
    (should-not (buffer-modified-p))))

;;; save

(ert-deftest test-companion-commands-save-writes-modified-project-buffers ()
  (test-companion-commands--with-project root
    (let ((manuscript (find-file-noselect (expand-file-name "novel.org" root)))
          (design (find-file-noselect (expand-file-name "design.org" root))))
      (with-current-buffer manuscript (goto-char (point-max)) (insert "\nA saved line.\n"))
      (with-current-buffer design (goto-char (point-max)) (insert "\nA design line.\n"))
      (let ((answer (test-companion-commands--run root "save")))
        (should (eq t (plist-get answer :ok))))
      (should-not (buffer-modified-p manuscript))
      (should-not (buffer-modified-p design))
      (should (string-match-p "A saved line\\."
                              (with-temp-buffer
                                (insert-file-contents (expand-file-name "novel.org" root))
                                (buffer-string)))))))

(ert-deftest test-companion-commands-save-reply-carries-the-new-revision ()
  (test-companion-commands--with-project root
    (let ((buffer (find-file-noselect (expand-file-name "novel.org" root))))
      (with-current-buffer buffer (goto-char (point-max)) (insert "x"))
      (let ((answer (test-companion-commands--run root "save")))
        (should (equal (plist-get answer :revision)
                       (org-scribe-companion--revision-string root)))
        (should-not (string-match-p "\\*" (plist-get answer :revision)))))))

(ert-deftest test-companion-commands-nothing-is-saved-without-save ()
  (test-companion-commands--with-project root
    (let ((buffer (find-file-noselect (expand-file-name "novel.org" root))))
      (with-current-buffer buffer (goto-char (point-max)) (insert "unsaved"))
      (test-companion-commands--run root "goto" :id test-companion-commands--scene-1)
      (should (buffer-modified-p buffer))
      (should-not (string-match-p "unsaved"
                                  (with-temp-buffer
                                    (insert-file-contents (expand-file-name "novel.org" root))
                                    (buffer-string)))))))

;;; Follow mode

(defun test-companion-commands--collect-focus (root body)
  "Serve ROOT, run BODY, and return the `focus' events sent meanwhile."
  (let ((events nil))
    (cl-letf (((symbol-function 'org-scribe-companion-server-subscribed-p) (lambda () t))
              ((symbol-function 'org-scribe-companion-server-broadcast)
               (lambda (event data) (when (equal event "focus") (push (plist-get data :id) events)))))
      (funcall body))
    (nreverse events)))

(ert-deftest test-companion-commands-scene-id-at-point ()
  (test-companion-commands--with-project root
    (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
      (goto-char (point-min))
      (re-search-forward "^Alex wakes\\|alarm goes off\\|^:Summary: The alarm")
      (should (equal test-companion-commands--scene-1
                     (org-scribe-companion--scene-id-at-point)))
      ;; on a chapter heading, the chapter
      (goto-char (point-min))
      (re-search-forward "^\\*\\* FINISHED Chapter 1")
      (should (equal "f9532c04-63d5-4871-9ed2-4aeefa13fac1"
                     (org-scribe-companion--scene-id-at-point))))))

(ert-deftest test-companion-commands-follow-sends-focus-only-when-the-scene-changes ()
  (test-companion-commands--with-project root
    (let* ((org-scribe-companion-follow t)
           (org-scribe-companion--last-focus nil)
           (events
            (test-companion-commands--collect-focus
             root
             (lambda ()
               (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
                 (goto-char (point-min))
                 (re-search-forward "^\\*\\*\\* FINISHED Scene 1")
                 (org-scribe-companion--follow)
                 (forward-line 3)
                 (org-scribe-companion--follow)
                 (re-search-forward "^\\*\\*\\* FINISHED Scene 2")
                 (org-scribe-companion--follow)
                 (org-scribe-companion--follow))))))
      (should (equal (list test-companion-commands--scene-1 test-companion-commands--scene-2)
                     events)))))

(ert-deftest test-companion-commands-follow-off-or-nobody-listening-sends-nothing ()
  (test-companion-commands--with-project root
    (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
      (goto-char (point-min))
      (re-search-forward "^\\*\\*\\* FINISHED Scene 1")
      (let ((org-scribe-companion-follow nil) (org-scribe-companion--last-focus nil))
        (should-not (test-companion-commands--collect-focus
                     root #'org-scribe-companion--follow)))
      (let ((org-scribe-companion-follow t) (org-scribe-companion--last-focus nil)
            sent)
        (cl-letf (((symbol-function 'org-scribe-companion-server-subscribed-p) (lambda () nil))
                  ((symbol-function 'org-scribe-companion-server-broadcast)
                   (lambda (&rest _) (setq sent t))))
          (org-scribe-companion--follow))
        (should-not sent)))))

(ert-deftest test-companion-commands-show-scene-sends-focus-even-if-unchanged ()
  (test-companion-commands--with-project root
    (let ((org-scribe-companion--last-focus test-companion-commands--scene-1)
          (events nil))
      (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
        (goto-char (point-min))
        (re-search-forward "^\\*\\*\\* FINISHED Scene 1")
        (cl-letf (((symbol-function 'org-scribe-companion-server-running-p) (lambda () t))
                  ((symbol-function 'org-scribe-companion-server-broadcast)
                   (lambda (_event data) (push (plist-get data :id) events))))
          (org-scribe-companion-show-scene)))
      (should (equal (list test-companion-commands--scene-1) events)))))

(ert-deftest test-companion-commands-show-scene-needs-a-running-companion ()
  (should-error (org-scribe-companion-show-scene) :type 'user-error))

(ert-deftest test-companion-commands-hooks-follow-install-and-removal ()
  (test-companion-commands--with-project root
    (let ((org-scribe-companion-port 0))
      (unwind-protect
          (progn
            (org-scribe-companion--start root)
            (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
              (should (memq #'org-scribe-companion--follow post-command-hook)))
            (with-current-buffer (find-file-noselect (expand-file-name "design.org" root))
              (should-not (memq #'org-scribe-companion--follow post-command-hook))))
        (org-scribe-companion--stop))
      (with-current-buffer (find-file-noselect (expand-file-name "novel.org" root))
        (should-not (memq #'org-scribe-companion--follow post-command-hook))))))

;;; The web files only use strings that exist

(defconst test-companion-commands--web
  (expand-file-name "companion/web" test-companion-commands--repo))

(defun test-companion-commands--string-keys-used ()
  "Return the `companion-*' keys the web files mention, in any form."
  (let (keys)
    (dolist (file (directory-files-recursively test-companion-commands--web "\\.\\(js\\|html\\)\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "['\"]\\(companion-[a-z0-9-]+\\)['\"]" nil t)
          (push (intern (match-string 1)) keys))))
    (delete-dups keys)))

(ert-deftest test-companion-commands-web-strings-exist-in-both-languages ()
  (let ((keys (test-companion-commands--string-keys-used)))
    (should (> (length keys) 10))
    (dolist (key keys)
      (should (alist-get key org-scribe-messages-en))
      (should (alist-get key org-scribe-messages-es)))))

(ert-deftest test-companion-commands-web-files-use-no-innerhtml ()
  (dolist (file (directory-files-recursively test-companion-commands--web "\\.js\\'"))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not (re-search-forward "innerHTML\\|outerHTML\\|insertAdjacentHTML\\|document\\.write" nil t)))))

(ert-deftest test-companion-commands-web-files-have-no-literal-ui-text ()
  "Visible text in HTML elements comes from data-i18n, not from the markup."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "index.html" test-companion-commands--web))
    (goto-char (point-min))
    ;; text between a tag's > and the next <, ignoring whitespace
    (while (re-search-forward ">\\([^<>]*[[:alpha:]][^<>]*\\)<" nil t)
      (should (member (string-trim (match-string 1)) '("org-scribe"))))))

(ert-deftest test-companion-commands-messages-in-both-languages ()
  (dolist (key '(companion-error-not-found companion-error-missing-arg
                 companion-error-no-scene-here))
    (should (alist-get key org-scribe-messages-en))
    (should (alist-get key org-scribe-messages-es))))

(provide 'test-companion-commands)
;;; test-companion-commands.el ends here
