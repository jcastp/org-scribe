;;; test-manual-es.el --- Spanish manual stays paired with the English one -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Javier Castilla

;;; Commentary:

;; `manual-es/' mirrors `manual/' file for file.  The pairing lives in one
;; place, the table in `manual-es/README.org' (English file, Spanish file,
;; status: pendiente / traducido / revisado), and these tests read it.  The
;; table is Org, not Elisp, because a Spanish literal in a shipped `.el' is
;; what `test-lang-no-hardcoded-localized-names-in-code' exists to catch.
;;
;; Structural tests fail on a violation.  Staleness (the English file moved
;; on after the translation) is deliberately a report, not a test: an English
;; typo fix must not turn the suite red.  Run it with
;;
;;   emacs -batch -l tests/test-manual-es.el -f org-scribe-manual-es-sync-report
;;
;; Rows still `pendiente' are exempt from every check but the table's own
;; shape, so the suite stays green while the translation is in progress.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defvar org-scribe-manual-es--root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "Repository root.")

(defconst org-scribe-manual-es--statuses '("pendiente" "traducido" "revisado")
  "Values the Status column of the file map may hold.")

(defun org-scribe-manual-es--file (dir name)
  "Absolute path of NAME in the manual directory DIR (`manual' or `manual-es')."
  (expand-file-name (concat dir "/" name) org-scribe-manual-es--root))

(defun org-scribe-manual-es--read (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun org-scribe-manual-es--map ()
  "Return the file map as a list of (ENGLISH SPANISH STATUS) rows."
  (let (rows)
    (with-temp-buffer
      (insert-file-contents (org-scribe-manual-es--file "manual-es" "README.org"))
      (while (re-search-forward
              (concat "^| *=\\([^=| ]+\\.org\\)= *| *=\\([^=| ]+\\.org\\)= *"
                      "| *\\([[:alpha:]]+\\) *|")
              nil t)
        (push (list (match-string 1) (match-string 2) (match-string 3)) rows)))
    (nreverse rows)))

(defun org-scribe-manual-es--done-rows ()
  "Rows of the file map whose status is not `pendiente'."
  (cl-remove "pendiente" (org-scribe-manual-es--map)
             :key #'cl-third :test #'string=))

(defun org-scribe-manual-es--row-for-spanish (name)
  "The map row whose Spanish file is NAME, or nil."
  (cl-find name (org-scribe-manual-es--map) :key #'cl-second :test #'string=))

(defun org-scribe-manual-es--lines (file)
  "Return the lines of FILE that are outside any `#+begin_' block."
  (let (in-block lines)
    (dolist (line (split-string (org-scribe-manual-es--read file) "\n"))
      (cond ((string-match-p "\\`[ \t]*#\\+begin_" line) (setq in-block t))
            ((string-match-p "\\`[ \t]*#\\+end_" line) (setq in-block nil))
            ((not in-block) (push line lines))))
    (nreverse lines)))

(defun org-scribe-manual-es--headings (file)
  "Return the headings of FILE outside blocks as a list of (LEVEL . TEXT)."
  (let (out)
    (dolist (line (org-scribe-manual-es--lines file))
      (when (string-match "\\`\\(\\*+\\) +\\(.*?\\)[ \t]*\\(:[[:alnum:]_@#%:]+:\\)?[ \t]*\\'"
                          line)
        (push (cons (length (match-string 1 line)) (match-string 2 line)) out)))
    (nreverse out)))

(defun org-scribe-manual-es--keyword (file key)
  "Value of the `#+KEY:' keyword in FILE, or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((case-fold-search t))
      (when (re-search-forward
             (format "^#\\+%s:[ \t]*\\(.*?\\)[ \t]*$" (regexp-quote key)) nil t)
        (match-string 1)))))

(defun org-scribe-manual-es--links (file)
  "Return the `file:' links of FILE outside blocks and verbatim spans.
Each is (PATH . SEARCH), SEARCH nil when the link has none."
  (let (out)
    (dolist (line (org-scribe-manual-es--lines file))
      (let ((line (replace-regexp-in-string
                   "=[^= \n][^=\n]*=" ""
                   (replace-regexp-in-string "~[^~\n]+~" "" line)))
            (start 0))
        (while (string-match "\\[\\[file:\\([^]:]+\\)\\(?:::\\([^]]*\\)\\)?\\]"
                             line start)
          (push (cons (match-string 1 line) (match-string 2 line)) out)
          (setq start (match-end 0)))))
    (nreverse out)))

(defun org-scribe-manual-es--all-files (dir)
  "Names of the `.org' files in DIR (`manual' or `manual-es'), README excluded."
  (cl-remove "README.org"
             (directory-files (expand-file-name dir org-scribe-manual-es--root)
                              nil "\\.org\\'")
             :test #'string=))

;;; The map itself

(ert-deftest test-manual-es-map-has-valid-shape ()
  "Every row has a known status; Spanish names are ASCII, unique, and distinct."
  (let ((rows (org-scribe-manual-es--map)))
    (should rows)
    (dolist (row rows)
      (should (member (cl-third row) org-scribe-manual-es--statuses))
      (should (string-match-p "\\`[a-z0-9.-]+\\'" (cl-second row))))
    (should (= (length rows) (length (delete-dups (mapcar #'cl-first rows)))))
    (should (= (length rows) (length (delete-dups (mapcar #'cl-second rows)))))))

(ert-deftest test-manual-es-map-covers-every-english-file ()
  "Each file in manual/ has a row, and each row's English file exists."
  (let ((rows (org-scribe-manual-es--map))
        (english (org-scribe-manual-es--all-files "manual")))
    (dolist (file english)
      (should (cl-find file rows :key #'cl-first :test #'string=)))
    (dolist (row rows)
      (should (member (cl-first row) english)))))

(ert-deftest test-manual-es-spanish-files-match-their-status ()
  "A Spanish file exists exactly when its row is not `pendiente', and none is unlisted."
  (dolist (row (org-scribe-manual-es--map))
    (should (eq (not (string= (cl-third row) "pendiente"))
                (file-exists-p (org-scribe-manual-es--file "manual-es" (cl-second row))))))
  (dolist (file (org-scribe-manual-es--all-files "manual-es"))
    (should (org-scribe-manual-es--row-for-spanish file))))

;;; Translated files

(ert-deftest test-manual-es-headers-are-complete ()
  "Translated files declare their language, source file and source commit."
  (dolist (row (org-scribe-manual-es--done-rows))
    (let ((file (org-scribe-manual-es--file "manual-es" (cl-second row))))
      (should (equal (org-scribe-manual-es--keyword file "LANGUAGE") "es"))
      (should (equal (org-scribe-manual-es--keyword file "SOURCE")
                     (concat "../manual/" (cl-first row))))
      (should (string-match-p "\\`[0-9a-f]\\{7,40\\}\\'"
                              (or (org-scribe-manual-es--keyword file "SOURCE_COMMIT")
                                  ""))))))

(ert-deftest test-manual-es-english-files-declare-english ()
  "Every English manual file carries `#+LANGUAGE: en'."
  (dolist (file (org-scribe-manual-es--all-files "manual"))
    (should (equal (org-scribe-manual-es--keyword
                    (org-scribe-manual-es--file "manual" file) "LANGUAGE")
                   "en"))))

(ert-deftest test-manual-es-heading-levels-match ()
  "A translated file has the same sequence of heading levels as its source."
  (dolist (row (org-scribe-manual-es--done-rows))
    (let ((en (mapcar #'car (org-scribe-manual-es--headings
                             (org-scribe-manual-es--file "manual" (cl-first row)))))
          (es (mapcar #'car (org-scribe-manual-es--headings
                             (org-scribe-manual-es--file "manual-es" (cl-second row))))))
      (should (equal en es)))))

(ert-deftest test-manual-es-pairs-link-to-each-other ()
  "Each side of a translated pair links to the other (the language switch)."
  (dolist (row (org-scribe-manual-es--done-rows))
    (let ((en-links (org-scribe-manual-es--links
                     (org-scribe-manual-es--file "manual" (cl-first row))))
          (es-links (org-scribe-manual-es--links
                     (org-scribe-manual-es--file "manual-es" (cl-second row)))))
      (should (cl-find (concat "../manual-es/" (cl-second row)) en-links
                       :key #'car :test #'string=))
      (should (cl-find (concat "../manual/" (cl-first row)) es-links
                       :key #'car :test #'string=)))))

;;; Links, in both manuals

(defun org-scribe-manual-es--checked-files ()
  "Absolute paths of the manual files whose links are checked."
  (append
   (mapcar (lambda (f) (org-scribe-manual-es--file "manual" f))
           (org-scribe-manual-es--all-files "manual"))
   (mapcar (lambda (f) (org-scribe-manual-es--file "manual-es" f))
           (org-scribe-manual-es--all-files "manual-es"))))

(defun org-scribe-manual-es--pending-target-p (path)
  "Non-nil when absolute PATH is a Spanish manual file still `pendiente'."
  (and (string= (file-name-directory path)
                (file-name-as-directory
                 (org-scribe-manual-es--file "manual-es" "")))
       (let ((row (org-scribe-manual-es--row-for-spanish
                   (file-name-nondirectory path))))
         (and row (string= (cl-third row) "pendiente")))))

(ert-deftest test-manual-es-relative-links-resolve ()
  "Every `file:' link outside examples resolves, and so does its `::*Heading'."
  (dolist (file (org-scribe-manual-es--checked-files))
    (pcase-dolist (`(,path . ,search) (org-scribe-manual-es--links file))
      (unless (or (file-name-absolute-p path) (string-match-p "\\`https?:" path))
        (let ((target (expand-file-name path (file-name-directory file))))
          (unless (org-scribe-manual-es--pending-target-p target)
            (should-not (and (not (file-exists-p target))
                             (list :missing path :in file)))
            (when (and search (string-prefix-p "*" search) (file-exists-p target))
              (should (cl-find (substring search 1)
                               (org-scribe-manual-es--headings target)
                               :key #'cdr :test #'string=)))))))))

(ert-deftest test-manual-es-no-line-number-links ()
  "No link outside an example block points at a line number.
They break on any edit; use a `::*Heading' search.  Example blocks may keep
them, because the edit index writes them and the block shows its output."
  (dolist (file (org-scribe-manual-es--checked-files))
    (pcase-dolist (`(,path . ,search) (org-scribe-manual-es--links file))
      (should-not (and search (string-match-p "\\`[0-9]+\\'" search)
                       (list :line-number-link path search :in file))))))

;;; Org IDs

(ert-deftest test-manual-es-no-copied-id-drawers ()
  "No Spanish manual file carries an `:ID:' property.
Org's ID database is global, so an ID copied from the English file would
make links to either heading resolve to whichever file was indexed last."
  (dolist (file (org-scribe-manual-es--all-files "manual-es"))
    (should-not
     (cl-find-if (lambda (line) (string-match-p "\\`[ \t]*:ID:" line))
                 (org-scribe-manual-es--lines
                  (org-scribe-manual-es--file "manual-es" file))))))

;;; Sync report

(defun org-scribe-manual-es--stale-commits (english-file commit &optional root)
  "Commits that touched manual/ENGLISH-FILE after COMMIT, newest first.
ROOT defaults to the repository root.  Returns `error' when git fails."
  (let ((default-directory (or root org-scribe-manual-es--root)))
    (with-temp-buffer
      (if (and (executable-find "git")
               (zerop (call-process "git" nil t nil "log" "--format=%h %s"
                                    (concat commit "..HEAD") "--"
                                    (concat "manual/" english-file))))
          (split-string (buffer-string) "\n" t)
        'error))))

(defun org-scribe-manual-es--report-lines (&optional root)
  "Lines of the sync report for the repository at ROOT."
  (let (lines)
    (dolist (row (org-scribe-manual-es--done-rows))
      (let* ((file (org-scribe-manual-es--file "manual-es" (cl-second row)))
             (commit (org-scribe-manual-es--keyword file "SOURCE_COMMIT"))
             (stale (and commit (org-scribe-manual-es--stale-commits
                                 (cl-first row) commit root))))
        (cond ((null commit)
               (push (format "%s: no #+SOURCE_COMMIT:" (cl-second row)) lines))
              ((eq stale 'error)
               (push (format "%s: git could not compare with %s"
                             (cl-second row) commit)
                     lines))
              (stale
               (push (format "%s: %d change%s in %s since %s"
                             (cl-second row) (length stale)
                             (if (= (length stale) 1) "" "s")
                             (cl-first row) commit)
                     lines)
               (dolist (c stale) (push (concat "    " c) lines))))))
    (nreverse lines)))

;;;###autoload
(defun org-scribe-manual-es-sync-report ()
  "List the translated manual files whose English source has changed.
Not a test: an English typo fix must not turn the suite red."
  (interactive)
  (let ((lines (or (org-scribe-manual-es--report-lines)
                   (list "manual-es is up to date with manual."))))
    (if noninteractive
        (princ (concat (string-join lines "\n") "\n"))
      (with-current-buffer (get-buffer-create "*manual-es sync*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (string-join lines "\n") "\n"))
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

(ert-deftest test-manual-es-stale-commits-lists-only-newer-changes ()
  "The staleness check names the commits after the recorded one, per file."
  (skip-unless (executable-find "git"))
  (let ((root (make-temp-file "manual-es-git" t)))
    (unwind-protect
        (let ((default-directory root)
              (process-environment
               (append '("GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@t"
                         "GIT_COMMITTER_NAME=t" "GIT_COMMITTER_EMAIL=t@t")
                       process-environment)))
          (make-directory "manual")
          (cl-flet ((git (&rest args)
                      (should (zerop (apply #'call-process "git" nil nil nil args))))
                    (touch (name text)
                      (with-temp-file (concat "manual/" name) (insert text))))
            (git "init" "-q")
            (touch "a.org" "one") (touch "b.org" "one")
            (git "add" ".") (git "commit" "-q" "-m" "first")
            (let ((base (string-trim
                         (with-temp-buffer
                           (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                           (buffer-string)))))
              (touch "a.org" "two")
              (git "commit" "-q" "-a" "-m" "change a")
              (let ((a (org-scribe-manual-es--stale-commits "a.org" base root))
                    (b (org-scribe-manual-es--stale-commits "b.org" base root)))
                (should (= (length a) 1))
                (should (string-match-p "change a" (car a)))
                (should (null b)))
              (should (eq 'error
                          (org-scribe-manual-es--stale-commits
                           "a.org" "no-such-commit" root))))))
      (delete-directory root t))))

(provide 'test-manual-es)
;;; test-manual-es.el ends here
