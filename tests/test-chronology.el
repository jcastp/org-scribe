;;; test-chronology.el --- Tests for the story-chronology view -*- lexical-binding: t; -*-

;;; Commentary:

;; Builds a small flat project on disk (marker, manuscript, notebook) and
;; checks the collector, the dynamic block and the health lines.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'org-scribe)
(require 'org-scribe-companion-model)

(defun test-chrono--scene (title timeline)
  (format "*** TODO %s :ignore:\n:PROPERTIES:\n:Timeline: %s\n:END:\nText.\n"
          title timeline))

(defmacro test-chrono--with-project (marker-extra scenes notebook &rest body)
  "Run BODY inside a temp project.  SCENES is a list of (TITLE TIMELINE)."
  (declare (indent 3))
  `(let* ((root (file-name-as-directory (make-temp-file "test-chrono-" t)))
          (default-directory root))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".org-scribe-project" root)
             (insert "# Type: novel\n# Language: en\n" ,marker-extra))
           (with-temp-file (expand-file-name "novel.org" root)
             (insert "* Act One\n** TODO Chapter One :ignore:\n")
             (dolist (s ,scenes)
               (insert (test-chrono--scene (car s) (cadr s)))))
           (with-temp-file (expand-file-name "design.org" root)
             (insert ,notebook))
           (org-scribe-project-type-cache-clear)
           ,@body)
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p root (buffer-file-name b)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory root t))))

(defun test-chrono--titles (entries)
  (mapcar (lambda (e) (plist-get e :heading)) entries))

(ert-deftest test-chronology-orders-scenes-in-story-order ()
  (test-chrono--with-project "" '(("A" "Day 3") ("B" "Day 1") ("C" "Night 1")) ""
    (let ((data (org-scribe-chronology-collect)))
      (should (equal '("B" "C" "A") (test-chrono--titles (plist-get data :ordered))))
      (should (null (plist-get data :undated))))))

(ert-deftest test-chronology-ties-keep-reading-order ()
  (test-chrono--with-project "" '(("A" "Day 2") ("B" "Day 2") ("C" "Day 1")) ""
    (should (equal '("C" "A" "B")
                   (test-chrono--titles
                    (plist-get (org-scribe-chronology-collect) :ordered))))))

(ert-deftest test-chronology-jumps-are-measured-against-the-scene-read-before ()
  (test-chrono--with-project ""
      '(("A" "Day 25") ("B" "Day 5") ("C" "Day 6") ("D" "Year -9")) ""
    (let ((jumps (plist-get (org-scribe-chronology-collect) :jumps)))
      (should (equal '(2 4) (mapcar #'car jumps)))
      (should (= (* 20 1440) (cdr (assq 2 jumps)))))))

(ert-deftest test-chronology-undated-scenes-are-kept-and-explained ()
  (test-chrono--with-project ""
      '(("A" "Day 1") ("B" "") ("C" "Dya 3") ("D" "<2026-03-14>")) ""
    (let* ((data (org-scribe-chronology-collect))
           (undated (plist-get data :undated)))
      (should (equal '("B" "C" "D") (test-chrono--titles undated)))
      (should (equal '(blank unreadable no-anchor)
                     (mapcar (lambda (e) (plist-get e :problem)) undated))))))

(ert-deftest test-chronology-dates-project-reads-timestamps ()
  (test-chrono--with-project "# Story-time: dates\n# Story-day-one: 2026-03-14\n"
      '(("A" "<2026-03-16 Mon>") ("B" "Day 1") ("C" "<2026-03-13 Fri>")) ""
    (should (equal '("C" "B" "A")
                   (test-chrono--titles
                    (plist-get (org-scribe-chronology-collect) :ordered))))))

(ert-deftest test-chronology-calendar-project ()
  (test-chrono--with-project "# Story-time: calendar\n"
      '(("A" "14 Frostmoon 312") ("B" "Day 1") ("C" "4 Thawmoon 312"))
      "* Calendar\n:PROPERTIES:\n:ERA: AE\n:DAY-ONE: 3 Thawmoon 312\n:END:\n| Month | Days |\n|-------+------|\n| Frostmoon | 30 |\n| Thawmoon | 31 |\n"
    (should (equal '("A" "B" "C")
                   (test-chrono--titles
                    (plist-get (org-scribe-chronology-collect) :ordered))))))

(ert-deftest test-chronology-events-join-date-and-time ()
  (test-chrono--with-project "" '(("A" "Day 1"))
      "* Timeline\n** Event one\n:PROPERTIES:\n:Date: Day 25\n:Time: 09:00\n:END:\n** Old\n:PROPERTIES:\n:Date: Year -9\n:END:\n** Recurring\n:PROPERTIES:\n:Date: Every Thursday\n:END:\n"
    (let ((data (org-scribe-chronology-collect t)))
      (should (equal '("Old" "A" "Event one")
                     (test-chrono--titles (plist-get data :ordered))))
      (should (equal "Day 25, 09:00"
                     (plist-get (nth 2 (plist-get data :ordered)) :text)))
      (should (equal '("Recurring") (test-chrono--titles (plist-get data :undated))))
      ;; Events are optional.
      (should (null (plist-get (org-scribe-chronology-collect) :events))))))

(ert-deftest test-chronology-dblock-renders-table-jump-and-undated ()
  (test-chrono--with-project "" '(("A" "Day 25") ("B" "Day 5") ("C" "later"))
      "* Timeline\n** Old\n:PROPERTIES:\n:Date: Year -9\n:END:\n"
    (with-temp-buffer
      (org-mode)
      (insert "#+BEGIN: story-chronology\n#+END:\n")
      (goto-char (point-min))
      (org-update-dblock)
      (let ((text (buffer-string)))
        (should (string-match-p "Year -9 .*Old .*backstory" text))
        (should (string-match-p "Day 5 .*B .*2/3 .*↶ 20 days" text))
        (should (string-match-p "Undated (1)" text))
        (should (string-match-p "later .*C" text))))))

(ert-deftest test-chronology-dblock-says-why-an-entry-is-undated ()
  (test-chrono--with-project ""
      '(("A" "Day 1") ("Blank" "") ("Typo" "Dya 3") ("Stamp" "<2026-03-14>")) ""
    (with-temp-buffer
      (org-mode)
      (insert "#+BEGIN: story-chronology\n#+END:\n")
      (goto-char (point-min))
      (org-update-dblock)
      (let ((text (buffer-string)))
        (should (string-match-p "Why undated" text))
        (should (string-match-p "Typo .*cannot be read" text))
        (should (string-match-p "Stamp .*needs Day 1" text))
        (should (string-match-p "Blank .*no story time" text))
        ;; the ones the writer tried to date come before the blank ones
        (should (< (string-match "Typo" text) (string-match "Blank" text)))
        (should (< (string-match "Stamp" text) (string-match "Blank" text)))
        ;; the dated table keeps its own header
        (should (string-match-p "| Jump |" text))))))

(ert-deftest test-chronology-dblock-can-leave-events-out ()
  (test-chrono--with-project "" '(("A" "Day 1"))
      "* Timeline\n** Old\n:PROPERTIES:\n:Date: Year -9\n:END:\n"
    (with-temp-buffer
      (org-mode)
      (insert "#+BEGIN: story-chronology :events nil\n#+END:\n")
      (goto-char (point-min))
      (org-update-dblock)
      (should-not (string-match-p "Old" (buffer-string))))))

(ert-deftest test-chronology-health-section-names-the-problems ()
  (test-chrono--with-project ""
      '(("A" "Day 25") ("B" "Day 5") ("C" "") ("D" "Dya 3")) ""
    (with-temp-buffer
      (org-scribe--health-insert-story-time)
      (let ((text (buffer-string)))
        (should (string-match-p "Scenes with a story time :: 2 of 4" text))
        (should (string-match-p "no story time (1) :: C" text))
        (should (string-match-p "D (Dya 3)" text))
        (should (string-match-p "Jumps back in time (1) :: B ↶ 20 days" text))))))

(ert-deftest test-chronology-health-section-is-one-line-before-story-time-starts ()
  (test-chrono--with-project "" '(("A" "") ("B" "") ("C" "")) ""
    (with-temp-buffer
      (org-scribe--health-insert-story-time)
      (let ((text (buffer-string)))
        (should (string-match-p "Scenes with a story time :: 0 of 3" text))
        (should (string-match-p "No scene has a story time yet" text))
        (should-not (string-match-p "no story time (3)" text))))))

(ert-deftest test-chronology-health-section-links-the-scenes ()
  (test-chrono--with-project "" '(("A" "Day 1") ("B" "") ("C" "Dya 3")) ""
    (let ((file (expand-file-name "novel.org" root))
          (n 0))
      (with-temp-buffer
        (insert-file-contents file)
        (while (search-forward ":PROPERTIES:\n" nil t)
          (insert (format ":ID: sc-%d\n" (cl-incf n))))
        (write-region nil nil file)))
    (let ((entries (plist-get (org-scribe-chronology-collect) :scenes)))
      (should (cl-every (lambda (e) (plist-get e :id)) entries)))
    (with-temp-buffer
      (org-scribe--health-insert-story-time)
      (should (string-match-p "\\[\\[id:[^]]+\\]\\[B\\]\\]" (buffer-string))))))

(ert-deftest test-chronology-health-section-absent-without-scenes ()
  (test-chrono--with-project "" '() ""
    (with-temp-buffer
      (org-scribe--health-insert-story-time)
      (should (equal "" (buffer-string))))))

(ert-deftest test-chronology-calendar-create-writes-heading-and-marker ()
  (test-chrono--with-project "" '(("A" "3 Thawmoon 312"))
      "* World\n** Setting\n"
    (org-scribe-calendar-create "AE" "3 Thawmoon 312")
    (should (equal "calendar" (org-scribe--project-marker-get root "Story-time")))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "design.org" root))
      (should (string-match-p "^\\*\\* Calendar$" (buffer-string)))
      (should (string-match-p ":ERA: AE" (buffer-string)))
      (should (string-match-p ":DAY-ONE: 3 Thawmoon 312" (buffer-string))))
    ;; A second call refuses instead of duplicating.
    (should-error (org-scribe-calendar-create "AE" "3 Thawmoon 312")
                  :type 'user-error)))

(ert-deftest test-chronology-calendar-table-is-in-the-project-language-and-set-apart ()
  (test-chrono--with-project "" '(("A" "Day 1"))
      "* Mundo\n** Ambientación\n* Investigación\n"
    (with-temp-file (expand-file-name ".org-scribe-project" root)
      (insert "# Type: novel\n# Language: es\n"))
    (org-scribe-project-type-cache-clear)
    (org-scribe-calendar-create "EC" "3 Deshielo 312")
    (with-temp-buffer
      (insert-file-contents (expand-file-name "design.org" root))
      (let ((text (buffer-string)))
        (should (string-match-p "| Mes +| Días +|" text))
        (should-not (string-match-p "Month" text))
        ;; a blank line between the table and the next part
        (should (string-match-p "|\n\n\\* Investigación" text))))))

;;; org-scribe-set-story-time

(ert-deftest test-chronology-set-story-time-dates-writes-both-marker-lines ()
  (test-chrono--with-project "" '(("A" "<2026-03-14 Sat 18:00>")) ""
    (org-scribe-set-story-time 'dates "2026-03-10")
    (should (equal "dates" (org-scribe--project-marker-get root "Story-time")))
    (should (equal "2026-03-10" (org-scribe--project-marker-get root "Story-day-one")))
    (should (eq 'dates (org-scribe-story-time-mode root)))
    ;; the scene is now placed: it had no Day 1 before
    (should (plist-get (car (plist-get (org-scribe-chronology-collect) :ordered)) :key))))

(ert-deftest test-chronology-set-story-time-refuses-a-bad-day-one-and-writes-nothing ()
  (test-chrono--with-project "" '(("A" "Day 1")) ""
    (should-error (org-scribe-set-story-time 'dates "next tuesday") :type 'user-error)
    (should-error (org-scribe-set-story-time 'dates nil) :type 'user-error)
    (should-not (org-scribe--project-marker-get root "Story-time"))
    (should-not (org-scribe--project-marker-get root "Story-day-one"))))

(ert-deftest test-chronology-set-story-time-relative-and-existing-calendar ()
  (test-chrono--with-project "# Story-time: dates\n" '(("A" "Day 1"))
      "* World\n** Calendar\n:PROPERTIES:\n:ERA: EC\n:DAY-ONE: 1 Thaw 1\n:END:\n| M | D |\n|---+---|\n| Thaw | 30 |\n"
    (org-scribe-set-story-time 'relative)
    (should (eq 'relative (org-scribe-story-time-mode root)))
    (org-scribe-set-story-time 'calendar)
    (should (eq 'calendar (org-scribe-story-time-mode root)))))

(ert-deftest test-chronology-story-time-revision-follows-the-marker-lines ()
  (test-chrono--with-project "" '(("A" "Day 1")) ""
    (let ((before (org-scribe-story-time-revision root)))
      (org-scribe--project-marker-set root "Story-day-one" "2026-03-10")
      (should-not (equal before (org-scribe-story-time-revision root)))
      (should (string-match-p "-t[0-9a-f]\\{8\\}-c[0-9a-f]\\{8\\}\\'"
                              (org-scribe-companion--revision-string root))))))

(provide 'test-chronology)
;;; test-chronology.el ends here
